%%%-------------------------------------------------------------------
%% @doc Kanban Scheduler - Cloud Scheduler integration for Kanban
%%
%% Responsibilities:
%% - Schedule periodic work items at specified intervals
%% - Publish high-priority test items for canary deployments
%% - Respect worker capacity (backpressure awareness)
%% - Manage SLA-driven scheduling
%%
%% @end
%%%-------------------------------------------------------------------
-module(kanban_scheduler).
-behaviour(gen_server).

-export([
    start_link/0,
    schedule_periodic/4,
    schedule_canary/2,
    cancel_schedule/1,
    get_scheduled_jobs/0,
    adjust_rate/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(SCHEDULE_CHECK_INTERVAL, 1000).

-record(state, {
    scheduled_jobs :: #{string() => #scheduled_job{}},
    queue_server :: atom(),
    rate_limiter :: #{domain => integer()}
}).

-record(scheduled_job, {
    id :: string(),
    priority :: atom(),
    domain :: atom(),
    interval_ms :: integer(),
    last_scheduled :: integer(),
    next_scheduled :: integer(),
    payload :: map(),
    deadline_ms :: integer(),
    active :: boolean()
}).

%%%-------------------------------------------------------------------
%% @doc Start scheduler
%% @end
%%%-------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc Schedule periodic work
%% IntervalMs = interval in milliseconds (e.g., 60000 for every minute)
%% DeadlineMs = deadline for each item (e.g., 55000 to leave 5s buffer)
-spec schedule_periodic(atom(), atom(), integer(), integer()) -> {ok, string()} | {error, term()}.
schedule_periodic(Priority, Domain, IntervalMs, DeadlineMs) ->
    gen_server:call(?MODULE, {schedule_periodic, Priority, Domain, IntervalMs, DeadlineMs}, 5000).

%% @doc Schedule canary test item
%% Used for safe deployments: test with 1 high-priority item
-spec schedule_canary(atom(), map()) -> ok | {error, term()}.
schedule_canary(Domain, Payload) ->
    gen_server:call(?MODULE, {schedule_canary, Domain, Payload}, 5000).

%% @doc Cancel scheduled job
-spec cancel_schedule(string()) -> ok | {error, term()}.
cancel_schedule(JobId) ->
    gen_server:call(?MODULE, {cancel_schedule, JobId}, 5000).

%% @doc Get all scheduled jobs
-spec get_scheduled_jobs() -> [map()].
get_scheduled_jobs() ->
    gen_server:call(?MODULE, get_scheduled_jobs, 5000).

%% @doc Adjust rate limiter for domain (backpressure handling)
-spec adjust_rate(atom()) -> ok.
adjust_rate(Domain) ->
    gen_server:call(?MODULE, {adjust_rate, Domain}, 5000).

%%%-------------------------------------------------------------------
%% Callbacks
%%%-------------------------------------------------------------------

-spec init([]) -> {ok, #state{}}.
init([]) ->
    lager:info("Initializing Kanban Scheduler"),

    State = #state{
        scheduled_jobs = #{},
        queue_server = kanban_queue,
        rate_limiter = #{}
    },

    %% Start schedule check loop
    self() ! check_schedules,

    %% Initialize Prometheus metrics
    prometheus_gauge:new([
        {name, kanban_scheduled_jobs},
        {help, "Number of active scheduled jobs"}
    ]),
    prometheus_counter:new([
        {name, kanban_scheduled_items_published_total},
        {help, "Total scheduled items published"}
    ]),

    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.

handle_call({schedule_periodic, Priority, Domain, IntervalMs, DeadlineMs}, _From, State) ->
    JobId = generate_job_id(),
    Now = now_ms(),
    Job = #scheduled_job{
        id = JobId,
        priority = Priority,
        domain = Domain,
        interval_ms = IntervalMs,
        last_scheduled = 0,
        next_scheduled = Now + IntervalMs,
        payload = #{scheduled => true},
        deadline_ms = DeadlineMs,
        active = true
    },

    Jobs = State#state.scheduled_jobs,
    NewJobs = Jobs#{JobId => Job},

    lager:info("Scheduled periodic job ~s: ~s.~s every ~Bms", [JobId, Priority, Domain, IntervalMs]),
    prometheus_gauge:set(kanban_scheduled_jobs, map_size(NewJobs)),

    {reply, {ok, JobId}, State#state{scheduled_jobs = NewJobs}};

handle_call({schedule_canary, Domain, Payload}, _From, State) ->
    lager:info("Scheduling canary test for domain ~s", [Domain]),

    %% Publish single high-priority test item
    Deadline = now_ms() + 10000,
    case kanban_queue:publish({high, Domain, Payload#{canary => true}, Deadline}) of
        {ok, WorkId} ->
            lager:info("Canary item published: ~s", [WorkId]),
            prometheus_counter:inc(kanban_scheduled_items_published_total),
            {reply, ok, State};
        {error, Reason} ->
            lager:error("Failed to publish canary item: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({cancel_schedule, JobId}, _From, State) ->
    Jobs = State#state.scheduled_jobs,
    case maps:find(JobId, Jobs) of
        {ok, Job} ->
            NewJobs = Jobs#{JobId => Job#scheduled_job{active = false}},
            lager:info("Cancelled scheduled job ~s", [JobId]),
            prometheus_gauge:set(kanban_scheduled_jobs, length([J || J <- maps:values(NewJobs), J#scheduled_job.active])),
            {reply, ok, State#state{scheduled_jobs = NewJobs}};
        error ->
            {reply, {error, job_not_found}, State}
    end;

handle_call(get_scheduled_jobs, _From, State) ->
    Jobs = State#state.scheduled_jobs,
    JobsList = [job_to_map(Job) || Job <- maps:values(Jobs), Job#scheduled_job.active],
    {reply, JobsList, State};

handle_call({adjust_rate, Domain}, _From, State) ->
    %% Get current queue depth to determine backpressure
    QueueDepth = kanban_queue:queue_depth(Domain),

    RateLimiter = State#state.rate_limiter,
    case QueueDepth of
        Depth when Depth > 100 ->
            %% Queue backing up, reduce rate
            lager:warning("Queue depth high (~B), reducing scheduling rate for ~s", [Depth, Domain]),
            prometheus_gauge:set(kanban_queue_backpressure, 1),
            NewRate = max(5000, maps:get(Domain, RateLimiter, 1000) * 2),
            NewRateLimiter = RateLimiter#{Domain => NewRate};
        Depth when Depth < 10 ->
            %% Queue draining, increase rate
            lager:info("Queue depth low (~B), increasing scheduling rate for ~s", [Depth, Domain]),
            prometheus_gauge:set(kanban_queue_backpressure, 0),
            NewRate = max(100, maps:get(Domain, RateLimiter, 1000) div 2),
            NewRateLimiter = RateLimiter#{Domain => NewRate};
        _ ->
            NewRateLimiter = RateLimiter
    end,

    {reply, ok, State#state{rate_limiter = NewRateLimiter}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handle asynchronous messages
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages (schedule check loop)
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(check_schedules, State) ->
    %% Check which jobs need to be scheduled
    NewState = check_and_schedule_jobs(State),

    %% Check backpressure and adjust rates
    check_backpressure(NewState),

    %% Schedule next check
    erlang:send_after(?SCHEDULE_CHECK_INTERVAL, self(), check_schedules),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    lager:info("Terminating Kanban Scheduler"),
    ok.

%% @doc Code change
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%------

%% @doc Check and schedule jobs that are due
-spec check_and_schedule_jobs(#state{}) -> #state{}.
check_and_schedule_jobs(State) ->
    Now = now_ms(),
    Jobs = State#state.scheduled_jobs,

    NewJobs = maps:map(fun(JobId, Job) ->
        case should_schedule(Job, Now) of
            true ->
                schedule_job(Job),
                Job#scheduled_job{
                    last_scheduled = Now,
                    next_scheduled = Now + Job#scheduled_job.interval_ms
                };
            false ->
                Job
        end
    end, Jobs),

    State#state{scheduled_jobs = NewJobs}.

%% @doc Check if job should be scheduled
-spec should_schedule(#scheduled_job{}, integer()) -> boolean().
should_schedule(Job, Now) ->
    Job#scheduled_job.active andalso Now >= Job#scheduled_job.next_scheduled.

%% @doc Schedule job by publishing work item
-spec schedule_job(#scheduled_job{}) -> ok | {error, term()}.
schedule_job(Job) ->
    #scheduled_job{
        id = JobId,
        priority = Priority,
        domain = Domain,
        payload = Payload,
        deadline_ms = DeadlineMs
    } = Job,

    Deadline = now_ms() + DeadlineMs,

    case kanban_queue:publish({Priority, Domain, Payload, Deadline}) of
        {ok, WorkId} ->
            lager:debug("Scheduled job ~s published as work item ~s", [JobId, WorkId]),
            prometheus_counter:inc(kanban_scheduled_items_published_total),
            ok;
        {error, Reason} ->
            lager:error("Failed to schedule job ~s: ~p", [JobId, Reason]),
            {error, Reason}
    end.

%% @doc Check backpressure for all domains and adjust rates
-spec check_backpressure(#state{}) -> ok.
check_backpressure(State) ->
    Jobs = State#state.scheduled_jobs,
    Domains = lists:usort([D || D <- [J#scheduled_job.domain || J <- maps:values(Jobs)], D =/= undefined]),

    lists:foreach(fun(Domain) ->
        adjust_rate(Domain)
    end, Domains),

    ok.

%% @doc Convert job record to map
-spec job_to_map(#scheduled_job{}) -> map().
job_to_map(Job) ->
    #{
        id => Job#scheduled_job.id,
        priority => Job#scheduled_job.priority,
        domain => Job#scheduled_job.domain,
        interval_ms => Job#scheduled_job.interval_ms,
        next_scheduled => Job#scheduled_job.next_scheduled,
        active => Job#scheduled_job.active
    }.

%% @doc Generate job ID
-spec generate_job_id() -> string().
generate_job_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    lists:flatten(io_lib:format("job_~10B~6B~6B", [MegaSecs, Secs, MicroSecs])).

%% @doc Get current time in milliseconds
-spec now_ms() -> integer().
now_ms() ->
    erlang:system_time(millisecond).

%%%-------------------------------------------------------------------
%% Tests (Chicago TDD - Arrange/Act/Assert)
%%%------

-ifdef(TEST).

% Test 1: Schedule periodic job
schedule_periodic_test() ->
    % Arrange
    {ok, _Pid} = start_link(),
    Priority = high,
    Domain = payment,
    IntervalMs = 60000,
    DeadlineMs = 55000,

    % Act
    {ok, JobId} = schedule_periodic(Priority, Domain, IntervalMs, DeadlineMs),

    % Assert
    ?assertNotEqual(undefined, JobId),
    Jobs = get_scheduled_jobs(),
    ?assertEqual(1, length(Jobs)),
    ok.

% Test 2: Cancel schedule
cancel_schedule_test() ->
    % Arrange
    {ok, _Pid} = start_link(),
    {ok, JobId} = schedule_periodic(normal, payment, 60000, 55000),

    % Act
    ok = cancel_schedule(JobId),
    Jobs = get_scheduled_jobs(),

    % Assert
    ?assertEqual(0, length(Jobs)),
    ok.

-endif.
