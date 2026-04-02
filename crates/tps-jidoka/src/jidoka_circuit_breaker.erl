%%%-------------------------------------------------------------------
%%% @doc Jidoka Circuit Breaker - State Machine for Failure Detection
%%%
%%% Implements the circuit breaker pattern using Erlang gen_statem.
%%% States: closed (normal operation), open (failing), half_open (recovery).
%%%
%%% Jidoka principle: When errors accumulate, STOP calling the failing
%%% service immediately (open circuit = fail-fast). Don't retry, don't queue.
%%%
%%% State Transitions:
%%% closed -----(threshold exceeded)--> open
%%% open  -----(timeout elapsed)------> half_open
%%% half_open -(success on test)-------> closed
%%% half_open -(failure on test)-------> open
%%%
%%% Configuration:
%%% - threshold: Number of failures to trigger open (default: 5)
%%% - window_ms: Time window to count failures (default: 10000)
%%% - recovery_ms: Time to wait before trying half_open (default: 30000)
%%% @end
%%%-------------------------------------------------------------------

-module(jidoka_circuit_breaker).
-behaviour(gen_statem).

%% API
-export([start_link/1]).
-export([call/1, record_failure/0, record_success/0]).
-export([get_state/1, reset/1]).
-export([status/0]).

%% gen_statem callbacks
-export([init/1, handle_event/4, terminate/3, code_change/4, callback_mode/0]).

%% States
-export([closed/3, open/3, half_open/3]).

%% Internal state record
-record(data, {
    threshold :: integer(),      % Failures to trigger open
    window_ms :: integer(),       % Time window for counting failures
    recovery_ms :: integer(),     % Time before half_open attempt
    failures :: list(),           % List of failure timestamps {Time, Count}
    recovery_timer :: reference() % Timer reference for recovery attempt
}).

-define(DEFAULT_THRESHOLD, 5).
-define(DEFAULT_WINDOW_MS, 10000).
-define(DEFAULT_RECOVERY_MS, 30000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the circuit breaker state machine.
%% Config:
%% - threshold: Failures to trigger open (default: 5)
%% - window_ms: Time window (default: 10000ms)
%% - recovery_ms: Time to half_open (default: 30000ms)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Call through the circuit breaker.
%% Returns: ok | {error, circuit_open}
%% - If circuit is closed: forwards the call
%% - If circuit is open: returns {error, circuit_open} immediately (fail-fast)
%% - If circuit is half_open: allows one test call through
-spec call(fun()) -> any() | {error, circuit_open}.
call(Fun) ->
    gen_statem:call(?MODULE, {call, Fun}).

%% @doc Record a failure in the circuit breaker.
%% Used to track failure rate for threshold calculation.
-spec record_failure() -> ok.
record_failure() ->
    gen_statem:cast(?MODULE, record_failure).

%% @doc Record a successful call.
%% Used to reset failure counters on recovery.
-spec record_success() -> ok.
record_success() ->
    gen_statem:cast(?MODULE, record_success).

%% @doc Get current circuit breaker state.
%% Returns: closed | open | half_open
-spec get_state(pid()) -> atom().
get_state(Pid) ->
    gen_statem:call(Pid, get_state).

%% @doc Reset the circuit breaker to closed state.
%% Operational action to recover after issue is fixed.
-spec reset(pid()) -> ok | {error, term()}.
reset(Pid) ->
    gen_statem:call(Pid, reset).

%% @doc Get status information.
%% Returns: {State, FailureCount, NextRecoveryTime}
-spec status() -> {atom(), integer(), integer() | never}.
status() ->
    gen_statem:call(?MODULE, status).

%%%===================================================================
%%% gen_statem Callbacks
%%%===================================================================

%% @private
callback_mode() -> state_functions.

%% @private Initialize the circuit breaker.
init(Config) ->
    Threshold = maps:get(threshold, Config, ?DEFAULT_THRESHOLD),
    WindowMs = maps:get(window_ms, Config, ?DEFAULT_WINDOW_MS),
    RecoveryMs = maps:get(recovery_ms, Config, ?DEFAULT_RECOVERY_MS),

    logger:info(
        "Circuit breaker initialized: threshold=~p, window=~pms, recovery=~pms",
        [Threshold, WindowMs, RecoveryMs]
    ),

    Data = #data{
        threshold = Threshold,
        window_ms = WindowMs,
        recovery_ms = RecoveryMs,
        failures = [],
        recovery_timer = undefined
    },

    {ok, closed, Data}.

%% ===================================================================
%% CLOSED State: Normal operation
%% ===================================================================

%% @private Closed state: accept calls normally.
closed({call, {call, Fun}}, From, Data) ->
    try
        Result = Fun(),
        record_success(),
        {keep_state_and_data, [{reply, From, Result}]}
    catch
        Error:Reason ->
            logger:warning("Circuit breaker call failed: ~p:~p", [Error, Reason]),
            record_failure(),
            NewData = add_failure(Data),
            case should_open(NewData) of
                true ->
                    logger:error("Circuit breaker opening: threshold exceeded"),
                    {next_state, open, start_recovery_timer(NewData), [{reply, From, {error, Error}}]};
                false ->
                    {keep_state, NewData, [{reply, From, {error, Error}}]}
            end
    end;

closed({call, get_state}, From, Data) ->
    {keep_state_and_data, [{reply, From, closed}]};

closed({call, reset}, From, Data) ->
    {keep_state, Data#data{failures = []}, [{reply, From, ok}]};

closed({call, status}, From, Data) ->
    FailureCount = count_failures(Data),
    {keep_state_and_data, [{reply, From, {closed, FailureCount, never}}]};

closed(cast, record_failure, Data) ->
    NewData = add_failure(Data),
    case should_open(NewData) of
        true ->
            logger:error("Circuit breaker opening: failure threshold exceeded"),
            {next_state, open, start_recovery_timer(NewData)};
        false ->
            {keep_state, NewData}
    end;

closed(cast, record_success, Data) ->
    %% Reset failure counter on success
    {keep_state, Data#data{failures = []}};

closed(info, {timeout, _Ref, recovery_attempt}, Data) ->
    %% Stale recovery timer, ignore
    {keep_state, Data};

closed(_, _, _) ->
    keep_state_and_data.

%% ===================================================================
%% OPEN State: Rejecting calls (fail-fast)
%% ===================================================================

%% @private Open state: reject all calls immediately (Jidoka principle).
open({call, {call, _Fun}}, From, Data) ->
    logger:warning("Circuit breaker rejecting call: circuit is open"),
    {keep_state_and_data, [{reply, From, {error, circuit_open}}]};

open({call, get_state}, From, Data) ->
    {keep_state_and_data, [{reply, From, open}]};

open({call, reset}, From, Data) ->
    logger:warning("Circuit breaker reset to closed by operator"),
    {next_state, closed, Data#data{failures = []}},
    [{reply, From, ok}];

open({call, status}, From, Data) ->
    FailureCount = count_failures(Data),
    {keep_state_and_data, [{reply, From, {open, FailureCount, undefined}}]};

open(cast, record_failure, Data) ->
    %% Update failure count while open
    {keep_state, add_failure(Data)};

open(cast, record_success, Data) ->
    %% Ignore success while open (no calls make it through)
    {keep_state, Data};

open(info, {timeout, Ref, recovery_attempt}, Data) ->
    case Data#data.recovery_timer of
        Ref ->
            logger:info("Circuit breaker entering half_open state"),
            {next_state, half_open, Data#data{recovery_timer = undefined}};
        _ ->
            %% Stale timer, ignore
            {keep_state, Data}
    end;

open(_, _, _) ->
    keep_state_and_data.

%% ===================================================================
%% HALF_OPEN State: Testing recovery
%% ===================================================================

%% @private Half_open state: allow one test call through.
half_open({call, {call, Fun}}, From, Data) ->
    try
        Result = Fun(),
        logger:info("Circuit breaker test call succeeded, closing circuit"),
        {next_state, closed, Data#data{failures = []}, [{reply, From, Result}]}
    catch
        Error:Reason ->
            logger:warning("Circuit breaker test call failed, reopening circuit"),
            NewData = Data#data{failures = [erlang:system_time(millisecond)]},
            {next_state, open, start_recovery_timer(NewData), [{reply, From, {error, Error}}]}
    end;

half_open({call, get_state}, From, Data) ->
    {keep_state_and_data, [{reply, From, half_open}]};

half_open({call, reset}, From, Data) ->
    logger:warning("Circuit breaker reset to closed by operator"),
    {next_state, closed, Data#data{failures = []}},
    [{reply, From, ok}];

half_open({call, status}, From, Data) ->
    FailureCount = count_failures(Data),
    {keep_state_and_data, [{reply, From, {half_open, FailureCount, undefined}}]};

half_open(cast, record_failure, Data) ->
    %% Failure during half_open test, reopen circuit
    logger:warning("Failure during half_open test, reopening circuit"),
    NewData = add_failure(Data),
    {next_state, open, start_recovery_timer(NewData)};

half_open(cast, record_success, Data) ->
    %% Success during half_open, close circuit
    logger:info("Success during half_open test, closing circuit"),
    {next_state, closed, Data#data{failures = []}};

half_open(info, {timeout, Ref, recovery_attempt}, Data) ->
    %% Stale timer, ignore
    case Data#data.recovery_timer of
        Ref -> {keep_state, Data#data{recovery_timer = undefined}};
        _ -> {keep_state, Data}
    end;

half_open(_, _, _) ->
    keep_state_and_data.

%% @private Handle unexpected events.
handle_event(EventType, EventContent, State, Data) ->
    logger:warning(
        "Circuit breaker unexpected event: state=~p, type=~p, content=~p",
        [State, EventType, EventContent]
    ),
    {keep_state, Data}.

%% @private Cleanup on termination.
terminate(Reason, State, Data) ->
    case Data#data.recovery_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    logger:info("Circuit breaker terminating: state=~p, reason=~p", [State, Reason]),
    ok.

%% @private Code change not implemented (no state format change needed).
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Private Functions
%%%===================================================================

%% @private Add a failure record with current timestamp.
add_failure(#data{failures = Failures} = Data) ->
    Now = erlang:system_time(millisecond),
    NewFailures = [Now | Failures],
    Data#data{failures = NewFailures}.

%% @private Count failures within the time window.
count_failures(#data{failures = Failures, window_ms = WindowMs}) ->
    Now = erlang:system_time(millisecond),
    RecentFailures = [F || F <- Failures, Now - F =< WindowMs],
    length(RecentFailures).

%% @private Check if threshold is exceeded.
should_open(#data{threshold = Threshold} = Data) ->
    count_failures(Data) >= Threshold.

%% @private Start recovery timer for transition to half_open.
start_recovery_timer(#data{recovery_ms = RecoveryMs, recovery_timer = OldRef} = Data) ->
    case OldRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(OldRef)
    end,
    Ref = erlang:send_after(RecoveryMs, self(), {timeout, Ref, recovery_attempt}),
    Data#data{recovery_timer = Ref}.
