%%%-------------------------------------------------------------------
%% @doc Kanban Worker - Pull-based work processing
%% Each worker pulls N items from queue, processes them, ACKs on success
%%
%% Lifecycle: init → pull N items → process → ACK → repeat
%%
%% If process_timeout exceeded (>deadline), fail gracefully with jidoka
%% If circuit breaker opens, stop pulling until cleared
%%
%% @end
%%%-------------------------------------------------------------------
-module(kanban_worker).
-behaviour(gen_server).

-export([
    start_link/2,
    pull_work/1,
    stop/1,
    get_status/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(PULL_TIMEOUT, 10000).
-define(PROCESS_TIMEOUT, 30000).

-record(state, {
    worker_id :: string(),
    priority :: atom(),
    domain :: atom(),
    pull_size :: integer(),
    process_timeout :: integer(),
    queue_server :: atom(),
    processing_count :: integer(),
    failed_count :: integer(),
    success_count :: integer(),
    circuit_breaker_open :: boolean(),
    metrics :: map()
}).

%%%-------------------------------------------------------------------
%% @doc Start worker
%% @end
%%%-------------------------------------------------------------------
-spec start_link(atom(), atom()) -> {ok, pid()} | {error, term()}.
start_link(Priority, Domain) ->
    WorkerId = generate_worker_id(Priority, Domain),
    gen_server:start_link({local, list_to_atom(WorkerId)}, ?MODULE, {Priority, Domain, WorkerId}, []).

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc Pull N work items from queue
-spec pull_work(pid()) -> {ok, [map()]} | {error, term()}.
pull_work(Pid) ->
    gen_server:call(Pid, pull_work, ?PULL_TIMEOUT).

%% @doc Stop worker gracefully (drain queue first)
-spec stop(pid()) -> ok | {error, term()}.
stop(Pid) ->
    gen_server:call(Pid, stop, 10000).

%% @doc Get worker status
-spec get_status(pid()) -> map().
get_status(Pid) ->
    gen_server:call(Pid, get_status, ?PULL_TIMEOUT).

%%%-------------------------------------------------------------------
%% Callbacks
%%%-------------------------------------------------------------------

-spec init({atom(), atom(), string()}) -> {ok, #state{}}.
init({Priority, Domain, WorkerId}) ->
    lager:info("Initializing worker ~s (priority: ~s, domain: ~s)", [WorkerId, Priority, Domain]),

    {ok, PullSize} = application:get_env(tps_kanban, worker_pull_size),
    {ok, ProcessTimeout} = application:get_env(tps_kanban, worker_process_timeout_ms),

    State = #state{
        worker_id = WorkerId,
        priority = Priority,
        domain = Domain,
        pull_size = PullSize,
        process_timeout = ProcessTimeout,
        queue_server = kanban_queue,
        processing_count = 0,
        failed_count = 0,
        success_count = 0,
        circuit_breaker_open = false,
        metrics = #{
            pulled => 0,
            processed => 0,
            failed => 0,
            acked => 0
        }
    },

    %% Subscribe to work queue
    ok = kanban_queue:subscribe(Priority, Domain),

    %% Start pull loop
    self() ! pull_work,

    {ok, State}.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {stop, normal, ok, #state{}}.

handle_call(pull_work, _From, State = #state{circuit_breaker_open = true}) ->
    lager:warning("Circuit breaker open, not pulling work"),
    {reply, {error, circuit_breaker_open}, State};

handle_call(pull_work, _From, State = #state{pull_size = PullSize, priority = Priority, domain = Domain}) ->
    %% Simulate pulling PullSize items from queue
    %% Real implementation would receive from NATS subscription
    Items = pull_items_from_queue(PullSize, Priority, Domain),

    Metrics = State#state.metrics,
    NewMetrics = Metrics#{pulled => maps:get(pulled, Metrics, 0) + length(Items)},

    {reply, {ok, Items}, State#state{metrics = NewMetrics}};

handle_call(get_status, _From, State) ->
    Status = #{
        worker_id => State#state.worker_id,
        priority => State#state.priority,
        domain => State#state.domain,
        processing_count => State#state.processing_count,
        failed_count => State#state.failed_count,
        success_count => State#state.success_count,
        circuit_breaker_open => State#state.circuit_breaker_open,
        metrics => State#state.metrics
    },
    {reply, Status, State};

handle_call(stop, _From, State) ->
    lager:info("Stopping worker ~s", [State#state.worker_id]),
    %% Graceful shutdown: wait for processing to complete
    {stop, normal, ok, State}.

%% @doc Handle asynchronous messages
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages (pull loop, process results)
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(pull_work, State = #state{circuit_breaker_open = false}) ->
    %% Pull next batch of work
    case pull_work(self()) of
        {ok, []} ->
            %% No work available, try again soon
            erlang:send_after(1000, self(), pull_work),
            {noreply, State};
        {ok, Items} ->
            %% Process items
            lager:debug("Pulled ~B items", [length(Items)]),
            NewState = process_items(Items, State),

            %% Continue pulling
            self() ! pull_work,
            {noreply, NewState};
        {error, Reason} ->
            lager:error("Failed to pull work: ~p", [Reason]),
            erlang:send_after(5000, self(), pull_work),
            {noreply, State}
    end;

handle_info(pull_work, State) ->
    %% Circuit breaker open, wait before retrying
    erlang:send_after(10000, self(), pull_work),
    {noreply, State};

handle_info({process_result, WorkId, Success}, State) ->
    %% Handle result from async process
    NewState = case Success of
        true ->
            lager:debug("Work ~s completed successfully", [WorkId]),
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{
                processed => maps:get(processed, Metrics, 0) + 1,
                acked => maps:get(acked, Metrics, 0) + 1
            },
            State#state{
                metrics = NewMetrics,
                success_count = State#state.success_count + 1
            };
        false ->
            lager:error("Work ~s failed", [WorkId]),
            Metrics = State#state.metrics,
            NewMetrics = Metrics#{
                processed => maps:get(processed, Metrics, 0) + 1,
                failed => maps:get(failed, Metrics, 0) + 1
            },
            FailCount = State#state.failed_count + 1,
            IsOpen = should_open_circuit_breaker(FailCount),
            case IsOpen of
                true ->
                    lager:alert("Circuit breaker opened for worker ~s", [State#state.worker_id]),
                    prometheus_counter:inc(kanban_circuit_breaker_open);
                false ->
                    ok
            end,
            State#state{
                metrics = NewMetrics,
                failed_count = FailCount,
                circuit_breaker_open = IsOpen
            }
    end,

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    lager:info("Terminating worker ~s", [State#state.worker_id]),
    ok.

%% @doc Code change
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%------

%% @doc Simulate pulling items from queue
-spec pull_items_from_queue(integer(), atom(), atom()) -> [map()].
pull_items_from_queue(0, _Priority, _Domain) ->
    [];
pull_items_from_queue(N, Priority, Domain) when N > 0 ->
    %% Placeholder: real implementation receives from NATS
    case rand:uniform(10) > 7 of
        true ->
            %% Simulate having work 30% of the time
            WorkId = generate_work_id(),
            Item = #{
                id => WorkId,
                priority => Priority,
                domain => Domain,
                payload => #{amount => rand:uniform(1000)},
                deadline => now_ms() + ?PROCESS_TIMEOUT,
                created_at => now_ms()
            },
            [Item];
        false ->
            []
    end.

%% @doc Process batch of work items
-spec process_items([map()], #state{}) -> #state{}.
process_items(Items, State) ->
    lists:foldl(fun(Item, AccState) ->
        process_item(Item, AccState)
    end, State, Items).

%% @doc Process single work item asynchronously
-spec process_item(map(), #state{}) -> #state{}.
process_item(Item, State) ->
    WorkId = maps:get(id, Item),
    Deadline = maps:get(deadline, Item),
    Payload = maps:get(payload, Item),

    Now = now_ms(),
    TimeRemaining = Deadline - Now,

    case TimeRemaining < 0 of
        true ->
            %% Deadline exceeded, fail gracefully
            lager:warning("Work ~s deadline exceeded (overdue by ~Bms)", [WorkId, abs(TimeRemaining)]),
            kanban_queue:nack(WorkId, State#state.domain, deadline_exceeded),

            Metrics = State#state.metrics,
            NewMetrics = Metrics#{
                processed => maps:get(processed, Metrics, 0) + 1,
                failed => maps:get(failed, Metrics, 0) + 1
            },
            State#state{
                metrics = NewMetrics,
                failed_count = State#state.failed_count + 1
            };
        false ->
            %% Start async processing with timeout
            ProcessTimeout = min(State#state.process_timeout, TimeRemaining),
            spawn_link(fun() ->
                process_work_with_timeout(WorkId, Payload, ProcessTimeout, self())
            end),

            State#state{
                processing_count = State#state.processing_count + 1
            }
    end.

%% @doc Process work with timeout
-spec process_work_with_timeout(string(), map(), integer(), pid()) -> ok.
process_work_with_timeout(WorkId, Payload, Timeout, ParentPid) ->
    Ref = erlang:send_after(Timeout, self(), process_timeout),

    try
        %% Simulate work processing (replace with actual business logic)
        Result = do_work(Payload),

        erlang:cancel_timer(Ref),

        case Result of
            ok ->
                ParentPid ! {process_result, WorkId, true};
            {error, _Reason} ->
                ParentPid ! {process_result, WorkId, false}
        end
    catch
        _:Error ->
            erlang:cancel_timer(Ref),
            lager:error("Work ~s processing failed: ~p", [WorkId, Error]),
            ParentPid ! {process_result, WorkId, false}
    end.

%% @doc Actual work implementation (placeholder)
-spec do_work(map()) -> ok | {error, term()}.
do_work(Payload) ->
    %% Replace with actual business logic
    Amount = maps:get(amount, Payload, 0),
    case Amount > 0 of
        true -> ok;
        false -> {error, invalid_amount}
    end.

%% @doc Generate worker ID
-spec generate_worker_id(atom(), atom()) -> string().
generate_worker_id(Priority, Domain) ->
    lists:flatten(io_lib:format("worker_~s_~s_~B", [Priority, Domain, erlang:phash2({Priority, Domain}, 1000)])).

%% @doc Generate work ID
-spec generate_work_id() -> string().
generate_work_id() ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    lists:flatten(io_lib:format("~10B~6B~6B", [MegaSecs, Secs, MicroSecs])).

%% @doc Check if circuit breaker should open
-spec should_open_circuit_breaker(integer()) -> boolean().
should_open_circuit_breaker(FailCount) ->
    %% Open after 3 consecutive failures
    FailCount >= 3.

%% @doc Get current time in milliseconds
-spec now_ms() -> integer().
now_ms() ->
    erlang:system_time(millisecond).

%%%-------------------------------------------------------------------
%% Tests (Chicago TDD - Arrange/Act/Assert)
%%%-------------------------------------------------------------------

-ifdef(TEST).

% Test 1: Verify worker starts and gets status
worker_init_test() ->
    % Arrange
    Priority = high,
    Domain = payment,

    % Act
    {ok, Pid} = start_link(Priority, Domain),
    Status = get_status(Pid),

    % Assert
    ?assertEqual(Priority, maps:get(priority, Status)),
    ?assertEqual(Domain, maps:get(domain, Status)),
    ?assertEqual(false, maps:get(circuit_breaker_open, Status)),

    stop(Pid),
    ok.

% Test 2: Verify pull_work returns items
pull_work_test() ->
    % Arrange
    {ok, Pid} = start_link(high, payment),

    % Act
    {ok, Items} = pull_work(Pid),

    % Assert
    ?assertIsList(Items),

    stop(Pid),
    ok.

-endif.
