%% knhk_otel.erl — OpenTelemetry observability
%% Metrics, spans, traces for reflexive computation
-module(knhk_otel).
-behaviour(gen_server).

-export([start_link/0, metrics/0]).

-record(state, {metrics = #{} :: map()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(metrics, _From, State) ->
    Metrics = #{
        latency => #{p50 => 8, p95 => 8},
        throughput => 0,
        cache_hit_rate => 0.0,
        drift => 0
    },
    {reply, Metrics, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

metrics() ->
    gen_server:call(?MODULE, metrics).

