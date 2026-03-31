%%%-------------------------------------------------------------------
%% @doc gcp_failure_injector: GCP Production Failure Simulation Framework
%%
%% Provides controlled failure injection for testing autonomics recovery.
%% Simulates GCP production failures:
%%   - Firestore failures (unavailable, timeout, quota exceeded)
%%   - Pub/Sub failures (network partition, subscription failures)
%%   - GCP metadata server failures (token refresh failures)
%%   - Action execution failures (timeouts, permission denied, resource not found)
%%   - Network partitions
%%   - Cascading failures
%%
%% Usage:
%%   gcp_failure_injector:inject_failure(firestore, unavailable, 30000),
%%   gcp_failure_injector:inject_failure(pubsub, timeout, 10000),
%%   gcp_failure_injector:clear_failures().
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_failure_injector).
-behaviour(gen_server).

%% API
-export([start_link/0, inject_failure/3, inject_failure/4, clear_failures/0, clear_failure/2]).
-export([check_failure/2, list_failures/0, set_failure_rate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(failure, {
    service :: atom(),
    type :: atom(),
    duration_ms :: integer() | infinity,
    start_time :: integer(),
    failure_rate :: float()  % 0.0 to 1.0, probability of failure
}).

-record(state, {
    failures :: [{atom(), #failure{}}],
    failure_rates :: #{atom() => float()}
}).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the failure injector
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Inject a failure for a specific service
-spec inject_failure(Service, Type, DurationMs) -> ok
    when Service :: atom(),
         Type :: atom(),
         DurationMs :: integer() | infinity.
inject_failure(Service, Type, DurationMs) ->
    inject_failure(Service, Type, DurationMs, 1.0).

%% @doc Inject a failure with a failure rate (probability)
-spec inject_failure(Service, Type, DurationMs, FailureRate) -> ok
    when Service :: atom(),
         Type :: atom(),
         DurationMs :: integer() | infinity,
         FailureRate :: float().
inject_failure(Service, Type, DurationMs, FailureRate) ->
    gen_server:call(?SERVER, {inject_failure, Service, Type, DurationMs, FailureRate}).

%% @doc Clear all failures
-spec clear_failures() -> ok.
clear_failures() ->
    gen_server:call(?SERVER, clear_failures).

%% @doc Clear a specific failure
-spec clear_failure(Service, Type) -> ok
    when Service :: atom(),
         Type :: atom().
clear_failure(Service, Type) ->
    gen_server:call(?SERVER, {clear_failure, Service, Type}).

%% @doc Check if a failure should occur for a service/operation
-spec check_failure(Service, Operation) -> {should_fail, Type, Reason} | should_succeed
    when Service :: atom(),
         Operation :: atom(),
         Type :: atom(),
         Reason :: term().
check_failure(Service, Operation) ->
    gen_server:call(?SERVER, {check_failure, Service, Operation}).

%% @doc List all active failures
-spec list_failures() -> [{Service, Type, RemainingMs, FailureRate}]
    when Service :: atom(),
         Type :: atom(),
         RemainingMs :: integer() | infinity,
         FailureRate :: float().
list_failures() ->
    gen_server:call(?SERVER, list_failures).

%% @doc Set failure rate for a service (for probabilistic failures)
-spec set_failure_rate(Service, Rate) -> ok
    when Service :: atom(),
         Rate :: float().
set_failure_rate(Service, Rate) when Rate >= 0.0, Rate =< 1.0 ->
    gen_server:call(?SERVER, {set_failure_rate, Service, Rate}).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

init([]) ->
    State = #state{
        failures = [],
        failure_rates = #{}
    },
    {ok, State}.

handle_call({inject_failure, Service, Type, DurationMs, FailureRate}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Failure = #failure{
        service = Service,
        type = Type,
        duration_ms = DurationMs,
        start_time = Now,
        failure_rate = FailureRate
    },
    Key = {Service, Type},
    NewFailures = lists:keystore(Key, 1, State#state.failures, {Key, Failure}),
    NewState = State#state{failures = NewFailures},
    {reply, ok, NewState};

handle_call(clear_failures, _From, State) ->
    NewState = State#state{failures = []},
    {reply, ok, NewState};

handle_call({clear_failure, Service, Type}, _From, State) ->
    Key = {Service, Type},
    NewFailures = lists:keydelete(Key, 1, State#state.failures),
    NewState = State#state{failures = NewFailures},
    {reply, ok, NewState};

handle_call({check_failure, Service, Operation}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Result = check_failure_internal(Service, Operation, State#state.failures, Now),
    {reply, Result, State};

handle_call(list_failures, _From, State) ->
    Now = erlang:system_time(millisecond),
    FailuresList = lists:map(fun({{Service, Type}, Failure}) ->
        RemainingMs = case Failure#failure.duration_ms of
            infinity -> infinity;
            DurationMs ->
                Elapsed = Now - Failure#failure.start_time,
                max(0, DurationMs - Elapsed)
        end,
        {Service, Type, RemainingMs, Failure#failure.failure_rate}
    end, State#state.failures),
    {reply, FailuresList, State};

handle_call({set_failure_rate, Service, Rate}, _From, State) ->
    NewRates = maps:put(Service, Rate, State#state.failure_rates),
    NewState = State#state{failure_rates = NewRates},
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec check_failure_internal(Service, Operation, Failures, Now) -> Result
    when Service :: atom(),
         Operation :: atom(),
         Failures :: [{term(), #failure{}}],
         Now :: integer(),
         Result :: {should_fail, atom(), term()} | should_succeed.
check_failure_internal(Service, Operation, Failures, Now) ->
    %% Check for exact service+type match
    Key = {Service, Operation},
    case lists:keyfind(Key, 1, Failures) of
        {Key, Failure} ->
            check_failure_active(Failure, Now);
        false ->
            %% Check for service-only match
            case find_service_failure(Service, Failures) of
                {found, Failure} ->
                    check_failure_active(Failure, Now);
                not_found ->
                    should_succeed
            end
    end.

-spec find_service_failure(Service, Failures) -> {found, #failure{}} | not_found
    when Service :: atom(),
         Failures :: [{term(), #failure{}}].
find_service_failure(Service, Failures) ->
    case lists:search(fun({{S, _}, _}) -> S =:= Service end, Failures) of
        {value, {_, Failure}} -> {found, Failure};
        false -> not_found
    end.

-spec check_failure_active(Failure, Now) -> {should_fail, atom(), term()} | should_succeed
    when Failure :: #failure{},
         Now :: integer().
check_failure_active(Failure, Now) ->
    %% Check if failure has expired
    case Failure#failure.duration_ms of
        infinity ->
            check_failure_probability(Failure);
        DurationMs ->
            Elapsed = Now - Failure#failure.start_time,
            case Elapsed < DurationMs of
                true ->
                    check_failure_probability(Failure);
                false ->
                    should_succeed
            end
    end.

-spec check_failure_probability(Failure) -> {should_fail, atom(), term()} | should_succeed
    when Failure :: #failure{}.
check_failure_probability(Failure) ->
    %% Apply failure rate (probabilistic failure)
    case Failure#failure.failure_rate >= 1.0 of
        true ->
            {should_fail, Failure#failure.type, failure_reason(Failure#failure.type)};
        false ->
            Random = rand:uniform(),
            case Random =< Failure#failure.failure_rate of
                true ->
                    {should_fail, Failure#failure.type, failure_reason(Failure#failure.type)};
                false ->
                    should_succeed
            end
    end.

-spec failure_reason(Type) -> term()
    when Type :: atom().
failure_reason(unavailable) ->
    {http_error, 503};
failure_reason(timeout) ->
    timeout;
failure_reason(quota_exceeded) ->
    {http_error, 429};
failure_reason(permission_denied) ->
    {http_error, 403};
failure_reason(resource_not_found) ->
    {http_error, 404};
failure_reason(network_partition) ->
    {error, nxdomain};
failure_reason(token_refresh_failed) ->
    {error, token_refresh_failed};
failure_reason(connection_refused) ->
    {error, econnrefused};
failure_reason(Type) ->
    {error, Type}.
