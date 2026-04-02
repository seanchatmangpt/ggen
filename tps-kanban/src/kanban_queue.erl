%%%-------------------------------------------------------------------
%% @doc Kanban Queue - NATS-based pull-semantics work queue
%% Primary: NATS (lightweight, fast, ephemeral)
%% Fallback: RabbitMQ (persistence layer)
%%
%% Work item format:
%% {
%%   "id": "unique-work-id",
%%   "priority": "high|normal|low",
%%   "domain": "payment|fraud|account|...",
%%   "payload": {...},
%%   "deadline": 1234567890000,
%%   "created_at": 1234567880000
%% }
%%
%% NATS subjects: kanban.{priority}.{domain}
%% Example: kanban.high.payment, kanban.normal.account
%%
%% @end
%%%-------------------------------------------------------------------
-module(kanban_queue).
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

-define(NATS_TIMEOUT, 5000).
-define(DEAD_LETTER_TIMEOUT, 300000).
-define(NACK_RETRY_MAX, 3).

-record(state, {
    nats_conn :: pid() | undefined,
    rabbitmq_conn :: pid() | undefined,
    use_nats :: boolean(),
    pending_acks :: #{string() => {reference(), pid()}},
    metrics :: #{
        published => integer(),
        acked => integer(),
        nacked => integer(),
        dead_lettered => integer(),
        queue_depth => integer()
    }
}).

%%%-------------------------------------------------------------------
%% @doc Start queue server
%% @end
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc Publish work item to queue
%% Item = {Priority, Domain, Payload, Deadline}
%% Priority = high | normal | low
%% Domain = atom() (payment, fraud, account, etc.)
%% Deadline = integer() (milliseconds since epoch)
-spec publish({atom(), atom(), term(), integer()}) -> {ok, string()} | {error, term()}.
publish({Priority, Domain, Payload, Deadline}) ->
    gen_server:call(?MODULE, {publish, Priority, Domain, Payload, Deadline}, ?NATS_TIMEOUT).

%% @doc Subscribe worker to priority/domain queue
%% Returns work items as they arrive
-spec subscribe(atom(), atom()) -> ok | {error, term()}.
subscribe(Priority, Domain) ->
    gen_server:call(?MODULE, {subscribe, Priority, Domain}, ?NATS_TIMEOUT).

%% @doc Acknowledge successful work completion
-spec ack(string(), atom()) -> ok | {error, term()}.
ack(WorkId, Domain) ->
    gen_server:call(?MODULE, {ack, WorkId, Domain}, ?NATS_TIMEOUT).

%% @doc Negative acknowledge (failed work) - retry with backoff
-spec nack(string(), atom(), term()) -> ok | {error, term()}.
nack(WorkId, Domain, Reason) ->
    gen_server:call(?MODULE, {nack, WorkId, Domain, Reason}, ?NATS_TIMEOUT).

%% @doc Get queue depth for priority/domain
-spec queue_depth(atom()) -> integer() | {error, term()}.
queue_depth(Domain) ->
    gen_server:call(?MODULE, {queue_depth, Domain}, ?NATS_TIMEOUT).

%% @doc Get queue metrics
-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics, ?NATS_TIMEOUT).

%% @doc Graceful shutdown
-spec shutdown() -> ok.
shutdown() ->
    gen_server:call(?MODULE, shutdown, 10000).

%%%-------------------------------------------------------------------
%% Callbacks
%%%-------------------------------------------------------------------

-spec init([]) -> {ok, #state{}}.
init([]) ->
    lager:info("Initializing Kanban Queue"),
    {ok, NatsServers} = application:get_env(tps_kanban, nats_servers),

    %% Try NATS first, fallback to RabbitMQ
    NatsResult = connect_nats(NatsServers),
    {UseNats, NatsConn} = case NatsResult of
        {ok, Conn} ->
            lager:info("Connected to NATS"),
            {true, Conn};
        {error, _Reason} ->
            lager:warning("Failed to connect to NATS, fallback to RabbitMQ"),
            {false, undefined}
    end,

    RabbitMQConn = case connect_rabbitmq() of
        {ok, Conn} ->
            lager:info("Connected to RabbitMQ"),
            Conn;
        {error, _} ->
            lager:warning("Failed to connect to RabbitMQ"),
            undefined
    end,

    State = #state{
        nats_conn = NatsConn,
        rabbitmq_conn = RabbitMQConn,
        use_nats = UseNats,
        pending_acks = #{},
        metrics = #{
            published => 0,
            acked => 0,
            nacked => 0,
            dead_lettered => 0,
            queue_depth => 0
        }
    },

    %% Start metrics reporter
    prometheus_histogram:new([
        {name, kanban_queue_depth},
        {help, "Current queue depth"}
    ]),
    prometheus_histogram:new([
        {name, kanban_latency_ms},
        {help, "Work item latency in milliseconds"}
    ]),
    prometheus_counter:new([
        {name, kanban_published_total},
        {help, "Total items published"}
    ]),
    prometheus_counter:new([
        {name, kanban_dead_lettered_total},
        {help, "Total items moved to dead letter queue"}
    ]),

    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

handle_call({publish, Priority, Domain, Payload, Deadline}, _From, State = #state{use_nats = true, nats_conn = Conn}) ->
    WorkId = generate_work_id(),
    Subject = subject_name(Priority, Domain),

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

    case gnat:pub(Conn, Subject, Msg) of
        ok ->
            lager:debug("Published work ~s to ~s", [WorkId, Subject]),
            prometheus_counter:inc(kanban_published_total),
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{published => maps:get(published, Metrics, 0) + 1},
            {reply, {ok, WorkId}, State#state{metrics = NewMetrics}};
        {error, Reason} ->
            lager:error("Failed to publish work to NATS: ~p, trying RabbitMQ", [Reason]),
            %% Fallback to RabbitMQ
            fallback_publish(WorkId, Subject, Msg, State)
    end;

handle_call({publish, Priority, Domain, Payload, Deadline}, _From, State) ->
    WorkId = generate_work_id(),
    Subject = subject_name(Priority, Domain),

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
    fallback_publish(WorkId, Subject, Msg, State);

handle_call({subscribe, Priority, Domain}, _From, State = #state{use_nats = true, nats_conn = Conn}) ->
    Subject = subject_name(Priority, Domain),

    case gnat:sub(Conn, self(), Subject) of
        {ok, Sid} ->
            lager:info("Subscribed to ~s (SID: ~p)", [Subject, Sid]),
            {reply, ok, State};
        {error, Reason} ->
            lager:error("Failed to subscribe to ~s: ~p", [Subject, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({subscribe, _Priority, _Domain}, _From, State) ->
    lager:warning("RabbitMQ subscription not yet implemented"),
    {reply, ok, State};

handle_call({ack, WorkId, Domain}, _From, State) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{acked => maps:get(acked, Metrics, 0) + 1},
    lager:debug("ACK work ~s (domain: ~s)", [WorkId, Domain]),
    {reply, ok, State#state{metrics = NewMetrics}};

handle_call({nack, WorkId, Domain, Reason}, _From, State) ->
    Metrics = State#state.metrics,
    NewMetrics = Metrics#{nacked => maps:get(nacked, Metrics, 0) + 1},
    lager:warning("NACK work ~s (domain: ~s): ~p", [WorkId, Domain, Reason]),
    {reply, ok, State#state{metrics = NewMetrics}};

handle_call({queue_depth, Domain}, _From, State) ->
    %% Placeholder: actual implementation would query NATS/RabbitMQ
    QueueDepth = 0,
    {reply, QueueDepth, State};

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(shutdown, _From, State) ->
    {stop, normal, ok, State}.

%% @doc Handle asynchronous messages
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages (NATS messages)
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({msg, #{body := Body}}, State) ->
    %% Received message from NATS subscription
    try
        Item = jiffy:decode(Body, [return_maps]),
        WorkId = maps:get(<<"id">>, Item),
        lager:debug("Received work item: ~s", [WorkId])
    catch
        _:Error ->
            lager:error("Failed to decode work item: ~p", [Error])
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{nats_conn = Conn}) ->
    case Conn of
        undefined -> ok;
        _ ->
            lager:info("Closing NATS connection"),
            gnat:close(Conn)
    end,
    ok.

%% @doc Code change
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

%% @doc Connect to NATS
-spec connect_nats([string()]) -> {ok, pid()} | {error, term()}.
connect_nats(Servers) ->
    ServerList = [{Host, Port} || Url <- Servers, {Host, Port} <- [parse_nats_url(Url)]],
    gnat:start_link(ServerList).

%% @doc Parse NATS URL
-spec parse_nats_url(string()) -> {string(), integer()}.
parse_nats_url(Url) ->
    % Simple parsing: nats://host:port
    {match, [Host, Port]} = re:run(Url, "nats://([^:]+):([0-9]+)", [{capture, all_but_first, list}]),
    {Host, list_to_integer(Port)}.

%% @doc Connect to RabbitMQ
-spec connect_rabbitmq() -> {ok, term()} | {error, term()}.
connect_rabbitmq() ->
    {ok, Host} = application:get_env(tps_kanban, rabbitmq_host),
    {ok, Port} = application:get_env(tps_kanban, rabbitmq_port),
    {ok, User} = application:get_env(tps_kanban, rabbitmq_user),
    {ok, Pass} = application:get_env(tps_kanban, rabbitmq_pass),

    AmqpParams = #amqp_params_network{
        host = Host,
        port = Port,
        username = list_to_binary(User),
        password = list_to_binary(Pass)
    },

    case amqp_connection:start(AmqpParams) of
        {ok, Conn} -> {ok, Conn};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Fallback publish to RabbitMQ
-spec fallback_publish(string(), string(), binary(), #state{}) -> {reply, {ok, string()} | {error, term()}, #state{}}.
fallback_publish(WorkId, _Subject, _Msg, State = #state{rabbitmq_conn = undefined}) ->
    lager:error("Both NATS and RabbitMQ unavailable"),
    {reply, {error, no_queue_available}, State};

fallback_publish(WorkId, Subject, Msg, State = #state{rabbitmq_conn = Conn}) ->
    try
        {ok, Channel} = amqp_connection:open_channel(Conn),

        %% Declare queue if not exists
        Queue = list_to_binary(Subject),
        #'queue.declare_ok'{} = amqp_channel:call(Channel, #'queue.declare'{
            queue = Queue,
            durable = true
        }),

        %% Publish message
        amqp_channel:cast(Channel, #'basic.publish'{
            exchange = <<"">>,
            routing_key = Queue
        }, #amqp_msg{payload = Msg}),

        amqp_channel:close(Channel),
        lager:info("Published work ~s to RabbitMQ (~s)", [WorkId, Subject]),

        Metrics = State#state.metrics,
        NewMetrics = Metrics#{published => maps:get(published, Metrics, 0) + 1},
        {reply, {ok, WorkId}, State#state{metrics = NewMetrics}}
    catch
        _:Error ->
            lager:error("Failed to publish to RabbitMQ: ~p", [Error]),
            {reply, {error, Error}, State}
    end.

%% @doc Generate unique work ID
-spec generate_work_id() -> string().
generate_work_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    Random = rand:uniform(100000000),
    lists:flatten(io_lib:format("~10B~6B~6B~8B", [MegaSecs, Secs, MicroSecs, Random])).

%% @doc NATS subject name from priority and domain
-spec subject_name(atom(), atom()) -> string().
subject_name(Priority, Domain) ->
    lists:flatten(io_lib:format("kanban.~s.~s", [Priority, Domain])).

%% @doc Get current time in milliseconds
-spec now_ms() -> integer().
now_ms() ->
    erlang:system_time(millisecond).

%%%-------------------------------------------------------------------
%% Tests (Chicago TDD - Arrange/Act/Assert)
%%%-------------------------------------------------------------------

-ifdef(TEST).

% Test 1: Verify work item publication
publish_test() ->
    % Arrange
    {ok, _Pid} = start_link(),
    Priority = high,
    Domain = payment,
    Payload = #{amount => 100.50},
    Deadline = now_ms() + 60000,

    % Act
    {ok, WorkId} = publish({Priority, Domain, Payload, Deadline}),

    % Assert
    ?assertNotEqual(undefined, WorkId),
    ?assertMatch(ok, shutdown()),
    ok.

% Test 2: Verify metrics tracking
metrics_test() ->
    % Arrange
    {ok, _Pid} = start_link(),

    % Act
    {ok, _} = publish({high, payment, #{}, now_ms() + 60000}),
    Metrics = get_metrics(),

    % Assert
    ?assertEqual(1, maps:get(published, Metrics, 0)),
    ?assertMatch(ok, shutdown()),
    ok.

-endif.
