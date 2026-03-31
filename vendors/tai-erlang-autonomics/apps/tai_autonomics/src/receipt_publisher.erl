%%%-------------------------------------------------------------------
%% @doc receipt_publisher: Publishes receipts to Google Pub/Sub
%%
%% Manages receipt publication:
%% - Batches receipts for publication
%% - Publishes to Google Cloud Pub/Sub topic
%% - Handles retries for failed publishes
%% - Maintains delivery guarantees
%%
%% @end
%%%-------------------------------------------------------------------
-module(receipt_publisher).
-behaviour(gen_server).

%% API
-export([start_link/0, publish_receipt/1, get_publish_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BATCH_SIZE, 100).
-define(PUBLISH_INTERVAL, 5000).  % Publish every 5 seconds

-record(state, {
    batch_queue :: queue:queue(),
    batch_size :: pos_integer(),
    publish_interval :: pos_integer(),
    timer_ref :: reference() | undefined,
    published_count :: non_neg_integer(),
    failed_count :: non_neg_integer(),
    pubsub_topic :: string()
}).

-type receipt() :: map().

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the receipt publisher
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Publish a receipt to Pub/Sub
-spec publish_receipt(Receipt) -> ok | {error, term()}
  when Receipt :: receipt().
publish_receipt(Receipt) ->
    gen_server:cast(?SERVER, {publish_receipt, Receipt}).

%% @doc Get publication statistics
-spec get_publish_stats() -> {ok, Stats} | {error, term()}
  when Stats :: #{published := non_neg_integer(), failed := non_neg_integer()}.
get_publish_stats() ->
    gen_server:call(?SERVER, get_publish_stats).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Initialize batch queue
    Queue = queue:new(),

    %% Schedule periodic publish
    TimerRef = erlang:send_after(?PUBLISH_INTERVAL, self(), publish_batch),

    %% Get Pub/Sub topic from environment or config
    PubSubTopic = get_pubsub_topic(),

    {ok, #state{
        batch_queue = Queue,
        batch_size = ?BATCH_SIZE,
        publish_interval = ?PUBLISH_INTERVAL,
        timer_ref = TimerRef,
        published_count = 0,
        failed_count = 0,
        pubsub_topic = PubSubTopic
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: #state{}.
handle_call(get_publish_stats, _From, State) ->
    Stats = #{
        published => State#state.published_count,
        failed => State#state.failed_count
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, #state{}}
  when Request :: term(),
       State :: #state{}.
handle_cast({publish_receipt, Receipt}, State) ->
    %% Add receipt to batch queue
    NewQueue = queue:in(Receipt, State#state.batch_queue),
    NewState = State#state{batch_queue = NewQueue},

    %% Check if batch is full
    case queue:len(NewQueue) >= State#state.batch_size of
        true ->
            {noreply, do_publish_batch(NewState)};
        false ->
            {noreply, NewState}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, #state{}}
  when Info :: term(),
       State :: #state{}.
handle_info(publish_batch, State) ->
    NewState = do_publish_batch(State),
    %% Reschedule
    TimerRef = erlang:send_after(State#state.publish_interval, self(), publish_batch),
    {noreply, NewState#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: #state{}.
terminate(_Reason, State) ->
    %% Publish any remaining receipts before shutdown
    case queue:len(State#state.batch_queue) > 0 of
        true ->
            _ = do_publish_batch(State);
        false ->
            ok
    end,
    ok.

%% @private
-spec code_change(OldVsn, State, Extra) -> {ok, #state{}}
  when OldVsn :: term() | {down, term()},
       State :: #state{},
       Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% @private
-spec do_publish_batch(State) -> #state{}
  when State :: #state{}.
do_publish_batch(State) ->
    case queue:len(State#state.batch_queue) of
        0 ->
            State;
        _Size ->
            %% Convert queue to list
            Receipts = queue:to_list(State#state.batch_queue),

            %% Publish batch to Pub/Sub
            case publish_to_pubsub(Receipts, State#state.pubsub_topic) of
                ok ->
                    PublishedCount = State#state.published_count + length(Receipts),
                    State#state{
                        batch_queue = queue:new(),
                        published_count = PublishedCount
                    };
                {error, _Reason} ->
                    FailedCount = State#state.failed_count + length(Receipts),
                    State#state{failed_count = FailedCount}
            end
    end.

%% @private
-spec publish_to_pubsub(Receipts, Topic) -> ok | {error, term()}
  when Receipts :: [receipt()],
       Topic :: string().
publish_to_pubsub(Receipts, Topic) ->
    %% Convert receipts to JSON for Pub/Sub
    try
        %% Convert receipts to binary format expected by gcp_pubsub:publish_batch/2
        %% gcp_pubsub expects [binary() | map()], so convert JSON strings to binary
        JsonBatch = lists:map(fun(Receipt) ->
            JsonBin = jsx:encode(Receipt),
            JsonBin  % Return binary, not string
        end, Receipts),

        %% Publish to Google Cloud Pub/Sub using GCP Pub/Sub client
        case gcp_pubsub:publish_batch(Topic, JsonBatch) of
            {ok, _PublishIds} ->
                ok;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error ->
            {error, Error}
    end.

%% @private
-spec receipt_to_json(Receipt) -> binary()
  when Receipt :: receipt().
receipt_to_json(Receipt) ->
    %% Convert map to JSON binary format expected by gcp_pubsub
    jsx:encode(Receipt).

%% @private
%% @deprecated Use receipt_to_json/1 which returns binary directly
-spec receipt_to_json_str(Receipt) -> string()
  when Receipt :: receipt().
receipt_to_json_str(Receipt) ->
    %% Serialize receipt map to JSON using jsx
    JsonBin = jsx:encode(Receipt),
    binary_to_list(JsonBin).

%% @private
-spec get_pubsub_topic() -> string().
get_pubsub_topic() ->
    case application:get_env(tai_autonomics, pubsub_topic) of
        {ok, Topic} -> Topic;
        undefined -> "projects/my-project/topics/ggen-receipts"
    end.

%%%===================================================================
%% End of module
%%%===================================================================
