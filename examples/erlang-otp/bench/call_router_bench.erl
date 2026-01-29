%%%-------------------------------------------------------------------
%%% @doc Call Router Benchmark - Basho Bench Driver
%%%
%%% Basho Bench driver for load testing the call router.
%%% Measures throughput and latency under various load conditions.
%%%
%%% Usage:
%%%   basho_bench examples/erlang-otp/bench/call_router.config
%%%
%%% Metrics collected:
%%%   - Throughput: Operations per second
%%%   - Latency: P50, P95, P99, P999, Max
%%%   - Error rate: Failed operations / total operations
%%%   - SLA violations: Operations exceeding 1ms latency
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(call_router_bench).

%% Basho Bench callbacks
-export([new/1, run/4]).

-record(state, {
    destinations :: [binary()],
    call_counter :: integer()
}).

%%%===================================================================
%%% Basho Bench callbacks
%%%===================================================================

%% @doc Initialize benchmark state.
%% Called once per worker process at startup.
new(Id) ->
    %% Seed random number generator
    rand:seed(exsplus, {Id, erlang:system_time(), erlang:unique_integer()}),

    %% Pre-generate destination list for realistic load
    Destinations = generate_destinations(1000),

    State = #state{
        destinations = Destinations,
        call_counter = 0
    },

    {ok, State}.

%% @doc Run a benchmark operation.
%% Called repeatedly to generate load.
%%
%% Operations:
%%   - route_call: Route a call through the router
%%   - add_route: Add a new route (write operation)
%%   - get_metrics: Query current metrics
run(route_call, _KeyGen, _ValueGen, State) ->
    %% Select random destination from pre-generated list
    Dest = lists:nth(rand:uniform(length(State#state.destinations)),
                     State#state.destinations),

    %% Generate unique call ID
    CallId = generate_call_id(State#state.call_counter),

    %% Measure routing latency
    Start = erlang:monotonic_time(microsecond),
    Result = call_router_server:route_call(CallId, Dest),
    End = erlang:monotonic_time(microsecond),

    Latency = End - Start,

    %% Check SLA compliance (1ms = 1000μs)
    case Latency > 1000 of
        true ->
            %% SLA violation - report as error for metrics
            {error, sla_violation, State#state{call_counter = State#state.call_counter + 1}};
        false ->
            case Result of
                {ok, _Target} ->
                    {ok, State#state{call_counter = State#state.call_counter + 1}};
                {error, Reason} ->
                    {error, Reason, State#state{call_counter = State#state.call_counter + 1}}
            end
    end;

run(add_route, KeyGen, ValueGen, State) ->
    Dest = KeyGen(),
    Target = ValueGen(),

    Start = erlang:monotonic_time(microsecond),
    Result = call_router_server:add_route(Dest, Target),
    End = erlang:monotonic_time(microsecond),

    Latency = End - Start,

    %% Write operations have higher SLA (10ms)
    case Latency > 10000 of
        true -> {error, write_sla_violation, State};
        false ->
            case Result of
                ok -> {ok, State};
                Error -> {Error, State}
            end
    end;

run(get_metrics, _KeyGen, _ValueGen, State) ->
    Start = erlang:monotonic_time(microsecond),
    _Metrics = call_router_server:get_metrics(),
    End = erlang:monotonic_time(microsecond),

    Latency = End - Start,

    %% Metrics queries should be very fast (<100μs)
    case Latency > 100 of
        true -> {error, metrics_sla_violation, State};
        false -> {ok, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
generate_destinations(Count) ->
    [generate_destination(N) || N <- lists:seq(1, Count)].

%% @private
generate_destination(N) ->
    Prefix = case N rem 3 of
        0 -> <<"1-800-">>;
        1 -> <<"1-888-">>;
        2 -> <<"+1-555-">>
    end,
    Suffix = integer_to_binary(N rem 10000),
    <<Prefix/binary, Suffix/binary>>.

%% @private
generate_call_id(Counter) ->
    Timestamp = integer_to_binary(erlang:system_time(microsecond)),
    CounterBin = integer_to_binary(Counter),
    <<"CALL-", Timestamp/binary, "-", CounterBin/binary>>.

%%%===================================================================
%%% Example Basho Bench Configuration
%%%===================================================================

%% Save as: bench/call_router.config
%%
%% {mode, max}.
%% {duration, 5}.  % 5 minute benchmark
%% {concurrent, 100}.  % 100 concurrent workers
%%
%% {driver, call_router_bench}.
%%
%% {code_paths, ["./ebin"]}.
%%
%% {key_generator, {sequential_int, 1000000}}.
%% {value_generator, {uniform_bin, 20}}.
%%
%% {operations, [
%%     {route_call, 80},   % 80% reads
%%     {add_route, 15},    % 15% writes
%%     {get_metrics, 5}    % 5% metrics queries
%% ]}.
%%
%% {report_interval, 10}.  % Report every 10 seconds
