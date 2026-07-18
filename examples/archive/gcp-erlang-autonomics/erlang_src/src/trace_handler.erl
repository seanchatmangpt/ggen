%%%-------------------------------------------------------------------
%% @doc Trace Handler - Captures FSM events and streams to Cloud Logging
%%      Uses Erlang dbg module to trace governor state transitions
%%      Filters by governor type and captures FSM state changes
%% @end
%%%-------------------------------------------------------------------

-module(trace_handler).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([start_trace/1, stop_trace/0, set_filter/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Trace callback
-export([trace_handler/2]).

-define(TRACE_BUFFER_SIZE, 104857600).  %% 100MB
-define(CLOUD_LOGGING_ENDPOINT, "https://logging.googleapis.com/v2/projects").

-record(state, {
    tracing = false :: boolean(),
    trace_filter = all :: all | atom(),
    buffer = [] :: list(),
    buffer_size = 0 :: integer(),
    max_buffer_size = ?TRACE_BUFFER_SIZE :: integer(),
    handlers = [] :: list()
}).

-record(trace_event, {
    timestamp :: integer(),
    node :: atom(),
    pid :: pid(),
    type :: atom(),
    governor :: atom(),
    old_state :: term(),
    new_state :: term(),
    message :: term(),
    metadata :: map()
}).

%%%-------------------------------------------------------------------
%% API Functions
%%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop, 30000).

-spec start_trace(atom()) -> ok | {error, term()}.
start_trace(GovernorType) when is_atom(GovernorType) ->
    gen_server:call(?MODULE, {start_trace, GovernorType}, 30000).

-spec stop_trace() -> ok.
stop_trace() ->
    gen_server:call(?MODULE, stop_trace, 30000).

-spec set_filter(atom()) -> ok.
set_filter(GovernorType) when is_atom(GovernorType) ->
    gen_server:call(?MODULE, {set_filter, GovernorType}).

%%%-------------------------------------------------------------------
%% gen_server Callbacks
%%%-------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{
        tracing = false,
        trace_filter = all,
        buffer = [],
        buffer_size = 0
    }}.

handle_call({start_trace, GovernorType}, _From, State) ->
    Result = start_tracing(GovernorType),
    NewState = State#state{
        tracing = true,
        trace_filter = GovernorType
    },
    {reply, Result, NewState};

handle_call(stop_trace, _From, State) ->
    dbg:stop_clear(),
    {reply, ok, State#state{tracing = false}};

handle_call({set_filter, GovernorType}, _From, State) ->
    {reply, ok, State#state{trace_filter = GovernorType}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_info({trace, Pid, Type, Data}, State) ->
    Event = build_trace_event(Pid, Type, Data),
    handle_trace_event(Event, State);

handle_info({trace, Pid, Type, Data1, Data2}, State) ->
    Event = build_trace_event(Pid, Type, {Data1, Data2}),
    handle_trace_event(Event, State);

handle_info(flush_buffer, State) ->
    NewState = flush_trace_buffer(State),
    %% Schedule next flush
    erlang:send_after(30000, self(), flush_buffer),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dbg:stop_clear(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

-spec start_tracing(atom()) -> ok | {error, term()}.
start_tracing(GovernorType) ->
    try
        %% Start dbg tracer
        {ok, _} = dbg:tracer(process, {fun trace_handler:trace_handler/2, []}),

        %% Set trace flags based on governor type
        case GovernorType of
            billing ->
                trace_billing_governor();
            entitlement ->
                trace_entitlement_governor();
            deployment ->
                trace_deployment_governor();
            all ->
                trace_all_governors();
            _ ->
                {error, unknown_governor_type}
        end,

        %% Schedule periodic buffer flushes
        erlang:send_after(30000, self(), flush_buffer),
        ok
    catch
        _:Error -> {error, Error}
    end.

-spec trace_billing_governor() -> ok.
trace_billing_governor() ->
    %% Trace state transitions in billing_governor
    dbg:tpl(billing_governor, '_', []),
    %% Trace gen_statem state transitions
    dbg:tpl(gen_statem, handle_msg, []),
    ok.

-spec trace_entitlement_governor() -> ok.
trace_entitlement_governor() ->
    %% Trace state transitions in entitlement_governor
    dbg:tpl(entitlement_governor, '_', []),
    dbg:tpl(gen_statem, handle_msg, []),
    ok.

-spec trace_deployment_governor() -> ok.
trace_deployment_governor() ->
    %% Trace deployment governor
    dbg:tpl(deployment_governor, '_', []),
    dbg:tpl(gen_statem, handle_msg, []),
    ok.

-spec trace_all_governors() -> ok.
trace_all_governors() ->
    dbg:tpl(billing_governor, '_', []),
    dbg:tpl(entitlement_governor, '_', []),
    dbg:tpl(deployment_governor, '_', []),
    dbg:tpl(gen_statem, handle_msg, []),
    ok.

-spec trace_handler(term(), term()) -> ok.
trace_handler({trace, Pid, Type, Data}, _State) ->
    %% Dispatch to trace event handler
    gen_server:cast(?MODULE, {trace_event, Pid, Type, Data}),
    ok;
trace_handler({trace, Pid, Type, Data1, Data2}, _State) ->
    gen_server:cast(?MODULE, {trace_event, Pid, Type, {Data1, Data2}}),
    ok;
trace_handler(_, _State) ->
    ok.

-spec build_trace_event(pid(), atom(), term()) -> #trace_event{}.
build_trace_event(Pid, Type, Data) ->
    Timestamp = erlang:system_time(millisecond),
    Node = node(),

    {Governor, OldState, NewState, Message} = extract_state_info(Type, Data),

    #trace_event{
        timestamp = Timestamp,
        node = Node,
        pid = Pid,
        type = Type,
        governor = Governor,
        old_state = OldState,
        new_state = NewState,
        message = Message,
        metadata = #{
            process_info => process_info(Pid),
            trace_timestamp => Timestamp
        }
    }.

-spec extract_state_info(atom(), term()) -> {atom(), term(), term(), term()}.
extract_state_info(call, {StateName, Msg}) ->
    {unknown, StateName, StateName, Msg};
extract_state_info(return_from, {FuncName, _}) ->
    {unknown, FuncName, FuncName, undefined};
extract_state_info(_, _) ->
    {unknown, undefined, undefined, undefined}.

-spec handle_trace_event(#trace_event{}, #state{}) -> {noreply, #state{}}.
handle_trace_event(Event, State) ->
    case should_buffer_event(Event, State) of
        true ->
            Buffer = [Event | State#state.buffer],
            EventSize = erlang:term_to_binary(Event),
            NewBufferSize = State#state.buffer_size + byte_size(EventSize),

            NewState = State#state{
                buffer = Buffer,
                buffer_size = NewBufferSize
            },

            case NewBufferSize > State#state.max_buffer_size of
                true -> flush_trace_buffer(NewState);
                false -> NewState
            end;
        false ->
            State
    end.

-spec should_buffer_event(#trace_event{}, #state{}) -> boolean().
should_buffer_event(_Event, #state{tracing = false}) ->
    false;
should_buffer_event(Event, #state{trace_filter = all}) ->
    true;
should_buffer_event(Event, #state{trace_filter = Filter}) ->
    Event#trace_event.governor =:= Filter.

-spec flush_trace_buffer(#state{}) -> #state{}.
flush_trace_buffer(State) ->
    case State#state.buffer of
        [] -> State;
        Buffer ->
            %% Convert events to JSON for Cloud Logging
            JsonEvents = lists:map(fun event_to_json/1, lists:reverse(Buffer)),

            %% Publish to Cloud Logging
            publish_to_cloud_logging(JsonEvents),

            %% Clear buffer
            State#state{buffer = [], buffer_size = 0}
    end.

-spec event_to_json(#trace_event{}) -> map().
event_to_json(Event) ->
    #{
        timestamp => iso8601_format(Event#trace_event.timestamp),
        node => atom_to_list(Event#trace_event.node),
        pid => pid_to_list(Event#trace_event.pid),
        type => atom_to_list(Event#trace_event.type),
        governor => atom_to_list(Event#trace_event.governor),
        old_state => term_to_string(Event#trace_event.old_state),
        new_state => term_to_string(Event#trace_event.new_state),
        message => term_to_string(Event#trace_event.message),
        metadata => Event#trace_event.metadata
    }.

-spec iso8601_format(integer()) -> string().
iso8601_format(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(Seconds + 62167219200),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Minute, Second]).

-spec pid_to_list(pid()) -> string().
pid_to_list(Pid) ->
    pid_to_list(Pid).

-spec term_to_string(term()) -> string().
term_to_string(Term) ->
    try
        io_lib:format("~w", [Term])
    catch _:_ ->
        "<unprintable>"
    end.

-spec process_info(pid()) -> map().
process_info(Pid) ->
    case catch erlang:process_info(Pid) of
        PI when is_list(PI) ->
            #{
                registered_name => proplists:get_value(registered_name, PI, ''),
                initial_call => proplists:get_value(initial_call, PI, undefined),
                message_queue_len => proplists:get_value(message_queue_len, PI, 0)
            };
        _ -> #{}
    end.

-spec publish_to_cloud_logging(list()) -> ok.
publish_to_cloud_logging(JsonEvents) ->
    %% In production, this would call GCP Cloud Logging API
    %% For now, just log it
    lists:foreach(fun(Event) ->
        io:format("Trace Event: ~p~n", [Event])
    end, JsonEvents),
    ok.
