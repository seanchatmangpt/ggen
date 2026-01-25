%%%-------------------------------------------------------------------
%% @doc Kanban RabbitMQ Fallback - Persistence and backup layer
%%
%% When NATS is unavailable, fallback to RabbitMQ for message persistence.
%% Same API as kanban_queue but uses RabbitMQ under the hood.
%%
%% Architecture:
%% - RabbitMQ topics: kanban.{priority}.{domain} (fanout exchanges)
%% - Durable queues: one per worker
%% - Dead letter exchange: for retries
%% - Message TTL: configurable per domain
%%
%% @end
%%%-------------------------------------------------------------------
-module(kanban_rabbitmq_fallback).
-behaviour(gen_server).

-export([
    start_link/0,
    publish/1,
    subscribe/2,
    ack/2,
    nack/3,
    queue_depth/1,
    get_metrics/0,
    shutdown/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(RABBITMQ_TIMEOUT, 5000).
-define(DEAD_LETTER_QUEUE, "kanban.dead_letter").

-record(state, {
    connection :: pid() | undefined,
    channel :: pid() | undefined,
    subscribers :: #{atom() => list()},
    metrics :: #{
        published => integer(),
        acked => integer(),
        nacked => integer(),
        queue_depth => integer()
    }
}).

%%%-------------------------------------------------------------------
%% @doc Start fallback queue server
%% @end
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc Publish work item to RabbitMQ
-spec publish({atom(), atom(), term(), integer()}) -> {ok, string()} | {error, term()}.
publish({Priority, Domain, Payload, Deadline}) ->
    gen_server:call(?MODULE, {publish, Priority, Domain, Payload, Deadline}, ?RABBITMQ_TIMEOUT).

%% @doc Subscribe to work queue
-spec subscribe(atom(), atom()) -> ok | {error, term()}.
subscribe(Priority, Domain) ->
    gen_server:call(?MODULE, {subscribe, Priority, Domain}, ?RABBITMQ_TIMEOUT).

%% @doc Acknowledge successful work
-spec ack(string(), atom()) -> ok | {error, term()}.
ack(WorkId, Domain) ->
    gen_server:call(?MODULE, {ack, WorkId, Domain}, ?RABBITMQ_TIMEOUT).

%% @doc Negative acknowledge (retry)
-spec nack(string(), atom(), term()) -> ok | {error, term()}.
nack(WorkId, Domain, Reason) ->
    gen_server:call(?MODULE, {nack, WorkId, Domain, Reason}, ?RABBITMQ_TIMEOUT).

%% @doc Get queue depth
-spec queue_depth(atom()) -> integer() | {error, term()}.
queue_depth(Domain) ->
    gen_server:call(?MODULE, {queue_depth, Domain}, ?RABBITMQ_TIMEOUT).

%% @doc Get metrics
-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics, ?RABBITMQ_TIMEOUT).

%% @doc Graceful shutdown
-spec shutdown() -> ok.
shutdown() ->
    gen_server:call(?MODULE, shutdown, 10000).

%%%-------------------------------------------------------------------
%% Callbacks
%%%-------------------------------------------------------------------

-spec init([]) -> {ok, #state{}}.
init([]) ->
    lager:info("Initializing RabbitMQ Fallback Queue"),
    {ok, Host} = application:get_env(tps_kanban, rabbitmq_host),
    {ok, Port} = application:get_env(tps_kanban, rabbitmq_port),
    {ok, User} = application:get_env(tps_kanban, rabbitmq_user),
    {ok, Pass} = application:get_env(tps_kanban, rabbitmq_pass),

    AmqpParams = #amqp_params_network{
        host = Host,
        port = Port,
        username = list_to_binary(User),
        password = list_to_binary(Pass),
        virtual_host = <<"/">>
    },

    case amqp_connection:start(AmqpParams) of
        {ok, Connection} ->
            lager:info("Connected to RabbitMQ at ~s:~B", [Host, Port]),

            case amqp_connection:open_channel(Connection) of
                {ok, Channel} ->
                    %% Declare dead letter exchange
                    amqp_channel:call(Channel, #'exchange.declare'{
                        exchange = <<"kanban.dead_letter">>,
                        type = <<"direct">>,
                        durable = true
                    }),

                    State = #state{
                        connection = Connection,
                        channel = Channel,
                        subscribers = #{},
                        metrics = #{
                            published => 0,
                            acked => 0,
                            nacked => 0,
                            queue_depth => 0
                        }
                    },

                    %% Initialize Prometheus metrics
                    prometheus_gauge:new([
                        {name, kanban_rabbitmq_queue_depth},
                        {help, "RabbitMQ queue depth"}
                    ]),
                    prometheus_counter:new([
                        {name, kanban_rabbitmq_published_total},
                        {help, "Total RabbitMQ messages published"}
                    ]),

                    {ok, State};
                {error, Reason} ->
                    lager:error("Failed to open RabbitMQ channel: ~p", [Reason]),
                    amqp_connection:close(Connection),
                    {stop, Reason}
            end;
        {error, Reason} ->
            lager:error("Failed to connect to RabbitMQ: ~p", [Reason]),
            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

handle_call({publish, Priority, Domain, Payload, Deadline}, _From, State = #state{channel = Channel}) ->
    WorkId = generate_work_id(),
    ExchangeName = exchange_name(Priority, Domain),

    Item = #{
        <<"id">> => list_to_binary(WorkId),
        <<"priority">> => atom_to_binary(Priority),
        <<"domain">> => atom_to_binary(Domain),
        <<"payload">> => Payload,
        <<"deadline">> => Deadline,
        <<"created_at">> => now_ms(),
        <<"retry_count">> => 0
    },

    Msg = jiffy:encode(Item),

    try
        %% Declare exchange if not exists
        amqp_channel:call(Channel, #'exchange.declare'{
            exchange = list_to_binary(ExchangeName),
            type = <<"fanout">>,
            durable = true
        }),

        %% Publish message
        amqp_channel:call(Channel, #'basic.publish'{
            exchange = list_to_binary(ExchangeName),
            routing_key = <<"">>,
            mandatory = false,
            immediate = false
        }, #amqp_msg{
            payload = Msg,
            props = #'P_basic'{
                delivery_mode = 2,  %% Persistent
                content_type = <<"application/json">>,
                expiration = integer_to_list(Deadline - now_ms())
            }
        }),

        lager:debug("Published work ~s to RabbitMQ (~s)", [WorkId, ExchangeName]),
        prometheus_counter:inc(kanban_rabbitmq_published_total),

        Metrics = State#state.metrics,
        NewMetrics = Metrics#{published => maps:get(published, Metrics, 0) + 1},
        {reply, {ok, WorkId}, State#state{metrics = NewMetrics}}
    catch
        _:Error ->
            lager:error("Failed to publish to RabbitMQ: ~p", [Error]),
            {reply, {error, Error}, State}
    end;

handle_call({subscribe, Priority, Domain}, _From, State = #state{channel = Channel}) ->
    ExchangeName = exchange_name(Priority, Domain),
    QueueName = queue_name(Priority, Domain),

    try
        %% Declare queue
        amqp_channel:call(Channel, #'queue.declare'{
            queue = list_to_binary(QueueName),
            durable = true,
            auto_delete = false,
            arguments = [
                {<<"x-dead-letter-exchange">>, longstr, <<"kanban.dead_letter">>}
            ]
        }),

        %% Bind queue to exchange
        amqp_channel:call(Channel, #'queue.bind'{
            queue = list_to_binary(QueueName),
            exchange = list_to_binary(ExchangeName),
            routing_key = <<"">>
        }),

        %% Start consuming
        amqp_channel:call(Channel, #'basic.qos'{
            prefetch_size = 0,
            prefetch_count = 10,
            global = false
        }),

        amqp_channel:subscribe(Channel, #'basic.consume'{
            queue = list_to_binary(QueueName),
            no_local = false,
            no_ack = false,
            exclusive = false
        }, self()),

        lager:info("Subscribed to RabbitMQ ~s (~s)", [ExchangeName, QueueName]),
        {reply, ok, State}
    catch
        _:Error ->
            lager:error("Failed to subscribe: ~p", [Error]),
            {reply, {error, Error}, State}
    end;

handle_call({ack, WorkId, Domain}, _From, State) ->
    lager:debug("ACK work ~s (domain: ~s)", [WorkId, Domain]),
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{acked => maps:get(acked, Metrics, 0) + 1},
    {reply, ok, State#state{metrics = NewMetrics}};

handle_call({nack, WorkId, Domain, Reason}, _From, State) ->
    lager:warning("NACK work ~s (domain: ~s): ~p", [WorkId, Domain, Reason]),
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{nacked => maps:get(nacked, Metrics, 0) + 1},
    {reply, ok, State#state{metrics = NewMetrics}};

handle_call({queue_depth, Domain}, _From, State = #state{channel = Channel}) ->
    %% Estimate from metrics (placeholder)
    QueueDepth = case Channel of
        undefined -> 0;
        _ -> 0  %% Real implementation would query queue
    end,
    {reply, QueueDepth, State};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(shutdown, _From, State) ->
    {stop, normal, ok, State}.

%% @doc Handle asynchronous messages
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages (RabbitMQ messages)
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info({#'basic.deliver'{delivery_tag = DeliveryTag}, Content}, State = #state{channel = Channel}) ->
    %% Received message from RabbitMQ subscription
    #amqp_msg{payload = Body} = Content,
    try
        Item = jiffy:decode(Body, [return_maps]),
        WorkId = maps:get(<<"id">>, Item),
        lager:debug("Received work item from RabbitMQ: ~s", [WorkId]),

        %% Auto-acknowledge for now
        amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag})
    catch
        _:Error ->
            lager:error("Failed to decode work item: ~p", [Error]),
            amqp_channel:call(Channel, #'basic.nack'{delivery_tag = DeliveryTag, requeue = true})
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{connection = Conn, channel = Channel}) ->
    lager:info("Closing RabbitMQ connections"),
    case Channel of
        undefined -> ok;
        _ -> catch amqp_channel:close(Channel)
    end,
    case Conn of
        undefined -> ok;
        _ -> catch amqp_connection:close(Conn)
    end,
    ok.

%% @doc Code change
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%---

%% @doc RabbitMQ exchange name from priority and domain
-spec exchange_name(atom(), atom()) -> string().
exchange_name(Priority, Domain) ->
    lists:flatten(io_lib:format("kanban.~s.~s", [Priority, Domain])).

%% @doc RabbitMQ queue name
-spec queue_name(atom(), atom()) -> string().
queue_name(Priority, Domain) ->
    lists:flatten(io_lib:format("kanban_queue_~s_~s", [Priority, Domain])).

%% @doc Generate unique work ID
-spec generate_work_id() -> string().
generate_work_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Random = rand:uniform(100000000),
    lists:flatten(io_lib:format("~10B~6B~6B~8B", [MegaSecs, Secs, MicroSecs, Random])).

%% @doc Get current time in milliseconds
-spec now_ms() -> integer().
now_ms() ->
    erlang:system_time(millisecond).

%%%-------------------------------------------------------------------
%% Tests (Chicago TDD - Arrange/Act/Assert)
%%%---

-ifdef(TEST).

% Test 1: Verify RabbitMQ fallback initialization
init_test() ->
    % Arrange
    % (RabbitMQ must be running)

    % Act
    {ok, _Pid} = start_link(),

    % Assert
    Metrics = get_metrics(),
    ?assertNotEqual(undefined, Metrics),

    % Cleanup
    shutdown(),
    ok.

-endif.
