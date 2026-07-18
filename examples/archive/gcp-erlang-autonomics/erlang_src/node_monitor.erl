%%%-------------------------------------------------------------------
%% @doc node_monitor: Node health monitoring and failure detection
%%
%% Monitors node health:
%% - CPU and memory usage tracking
%% - Process count monitoring
%% - Scheduler load tracking
%% - Failure detection and reporting
%% - Health checks for distributed consensus
%%
%% @end
%%%-------------------------------------------------------------------
-module(node_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0, get_node_health/0, get_system_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(MONITOR_INTERVAL, 10000).  % Check every 10 seconds
-define(CPU_THRESHOLD, 80).         % Warn if CPU > 80%
-define(MEMORY_THRESHOLD, 85).      % Warn if Memory > 85%

-record(state, {
    monitor_ref :: reference() | undefined,
    last_stats :: term(),
    health_status :: healthy | degraded | critical
}).

-type node_health() :: #{
    status := healthy | degraded | critical,
    cpu_usage := float(),
    memory_usage := float(),
    process_count := non_neg_integer(),
    run_queue := non_neg_integer()
}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the node monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get current node health status
-spec get_node_health() -> {ok, Health} | {error, term()}
  when Health :: node_health().
get_node_health() ->
    gen_server:call(?SERVER, get_node_health).

%% @doc Get detailed system statistics
-spec get_system_stats() -> {ok, Stats} | {error, term()}
  when Stats :: map().
get_system_stats() ->
    gen_server:call(?SERVER, get_system_stats).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(Args) -> {ok, state()}.
init([]) ->
    %% Schedule monitoring
    MonitorRef = erlang:send_after(?MONITOR_INTERVAL, self(), monitor),

    {ok, #state{
        monitor_ref = MonitorRef,
        last_stats = #{},
        health_status = healthy
    }}.

%% @private
-spec handle_call(Request, From, State) -> {reply, Reply, State}
  when Request :: term(),
       From :: {pid(), term()},
       Reply :: term(),
       State :: state().
handle_call(get_node_health, _From, State) ->
    Health = collect_health_metrics(State),
    {reply, {ok, Health}, State};

handle_call(get_system_stats, _From, State) ->
    Stats = collect_system_stats(),
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(Request, State) -> {noreply, state()}
  when Request :: term(),
       State :: state().
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(Info, State) -> {noreply, state()}
  when Info :: term(),
       State :: state().
handle_info(monitor, State) ->
    %% Collect metrics and update health status
    NewStats = collect_system_stats(),
    HealthStatus = evaluate_health(NewStats),
    NewState = State#state{
        last_stats = NewStats,
        health_status = HealthStatus
    },

    %% Reschedule monitoring
    MonitorRef = erlang:send_after(?MONITOR_INTERVAL, self(), monitor),
    {noreply, NewState#state{monitor_ref = MonitorRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(Reason, State) -> ok
  when Reason :: normal | shutdown | {shutdown, term()} | term(),
       State :: state().
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(OldVsn, State, Extra) -> {ok, state()}
  when OldVsn :: term() | {down, term()},
       State :: state(),
       Extra :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% @private
-spec collect_system_stats() -> map().
collect_system_stats() ->
    {TotalMem, AllocatedMem} = erlang:memory(total_memory),
    {ProcessCount, _} = erlang:process_info(all),
    RunQueue = erlang:statistics(run_queue),

    MemoryUsage = (AllocatedMem / TotalMem) * 100,
    CPUUsage = get_cpu_usage(),

    #{
        timestamp => erlang:system_time(millisecond),
        cpu_usage => CPUUsage,
        memory_usage => MemoryUsage,
        memory_allocated => AllocatedMem,
        memory_total => TotalMem,
        process_count => ProcessCount,
        run_queue => RunQueue,
        uptime => element(1, erlang:statistics(wall_clock))
    }.

%% @private
-spec get_cpu_usage() -> float().
get_cpu_usage() ->
    %% Get CPU info from erlang:statistics
    case erlang:statistics(scheduler_wall_time) of
        {Total, _} ->
            %% Approximate CPU usage
            Total / 100.0;
        undefined ->
            0.0
    end.

%% @private
-spec evaluate_health(Stats) -> healthy | degraded | critical
  when Stats :: map().
evaluate_health(Stats) ->
    CPUUsage = maps:get(cpu_usage, Stats, 0),
    MemoryUsage = maps:get(memory_usage, Stats, 0),

    case {CPUUsage > ?CPU_THRESHOLD, MemoryUsage > ?MEMORY_THRESHOLD} of
        {true, true} -> critical;
        {true, _} -> degraded;
        {_, true} -> degraded;
        {false, false} -> healthy
    end.

%% @private
-spec collect_health_metrics(State) -> node_health()
  when State :: state().
collect_health_metrics(State) ->
    Stats = State#state.last_stats,
    #{
        status => State#state.health_status,
        cpu_usage => maps:get(cpu_usage, Stats, 0.0),
        memory_usage => maps:get(memory_usage, Stats, 0.0),
        process_count => maps:get(process_count, Stats, 0),
        run_queue => maps:get(run_queue, Stats, 0)
    }.

%%%===================================================================
%% End of module
%%%===================================================================
