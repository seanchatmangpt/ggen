# gen_yawl User Guide

Complete guide for using `gen_yawl` to define and execute workflow specifications.

## Table of Contents

1. [Concepts](#concepts)
2. [Defining Workflows](#defining-workflows)
3. [Pattern Usage](#pattern-usage)
4. [API Reference](#api-reference)
5. [Event Handling](#event-handling)
6. [Error Handling](#error-handling)
7. [Testing](#testing)
8. [Best Practices](#best-practices)

---

## Concepts

### YAWL Terminology

| YAWL Concept | Petri Net Concept | Description |
|--------------|-------------------|-------------|
| **Case** | Net Instance | A running workflow instance |
| **Task** | Transition | A unit of work that executes |
| **Condition** | Place | Holds tokens between tasks |
| **Work Item** | Token + Context | A task instance in a case |
| **Specification** | Net Structure | The workflow template |

### Token Lifecycle

```
None -> Enabled -> Started -> Completed
          |           |           |
          v           v           v
        Cancelled   Cancelled   (done)
```

---

## Defining Workflows

### Basic Workflow Structure

Every workflow implements the `gen_yawl` behavior:

```erlang
-module(my_workflow).
-behaviour(gen_yawl).

%% Required callbacks
-export([
    init_workflow/1,
    handle_work_item_enabled/2,
    execute_task/2,
    handle_work_item_completed/3
]).

%% Optional callbacks
-export([
    handle_cancellation/3,
    handle_task_timeout/3,
    workflow_completed/1
]).

-include_lib("gen_yawl/include/gen_yawl.hrl").
```

### Callback Specifications

#### init_workflow/1

Initialize the workflow specification.

```erlang
-spec init_workflow(Args :: term()) ->
    {ok, #yawl_spec{}} | {error, Reason :: term()}.

init_workflow(_Args) ->
    Spec = #yawl_spec{
        id = <<"workflow-id">>,
        name = <<"Workflow Name">>,
        version = {1, 0},
        tasks = #{},
        flows = [],
        input_condition = input,
        output_condition = output
    },
    {ok, Spec}.
```

#### handle_work_item_enabled/2

Called when a work item becomes enabled. Use this for notifications or external triggers.

```erlang
-spec handle_work_item_enabled(
    WorkItem :: #work_item{},
    State :: #yawl_state{}
) -> {ok, #yawl_state{}} | {error, Reason :: term()}.

handle_work_item_enabled(#work_item{task_id = TaskId}, State) ->
    logger:info("Task ~p enabled for case ~p", [TaskId, State#yawl_state.case_id]),
    {ok, State}.
```

#### execute_task/2

Execute the task logic. This is where your workflow behavior lives.

```erlang
-spec execute_task(
    WorkItem :: #work_item{},
    State :: #yawl_state{}
) ->
    {ok, Result :: map(), #yawl_state{}} | {error, Reason :: term()}.

execute_task(#work_item{task_id = send_email, data = Data}, State) ->
    To = maps:get(<<"to">>, Data),
    Subject = maps:get(<<"subject">>, Data),
    case send_email(To, Subject) of
        ok ->
            Result = #{<<"sent">> => true, <<"to">> => To},
            {ok, Result, State};
        {error, Reason} ->
            {error, Reason}
    end.
```

#### handle_work_item_completed/3

Called after task execution completes. Return the next tasks to enable.

```erlang
-spec handle_work_item_completed(
    WorkItem :: #work_item{},
    Result :: map(),
    State :: #yawl_state{}
) ->
    {ok, EnabledTasks :: [atom()], #yawl_state{}} | {error, Reason :: term()}.

handle_work_item_completed(#work_item{task_id = TaskId}, Result, State) ->
    %% Return empty list for automatic flow following
    %% Or specify next tasks explicitly
    {ok, [], State}.
```

---

## Pattern Usage

### Sequence (WP1)

Tasks execute one after another.

```erlang
init_workflow(_Args) ->
    Tasks = #{
        step1 => #task_def{id = step1, name = <<"Step 1">>, kind = atomic},
        step2 => #task_def{id = step2, name = <<"Step 2">>, kind = atomic},
        step3 => #task_def{id = step3, name = <<"Step 3">>, kind = atomic}
    },
    Flows = [
        #flow{from = input, to = step1},
        #flow{from = step1, to = step2},
        #flow{from = step2, to = step3},
        #flow{from = step3, to = output}
    ],
    {ok, #yawl_spec{id = <<"sequence">>, tasks = Tasks, flows = Flows, ...}}.
```

### Parallel Split and Synchronization (WP2, WP3)

Execute tasks in parallel and wait for all to complete.

```erlang
init_workflow(_Args) ->
    Tasks = #{
        start => #task_def{id = start, split_type = and},
        task_a => #task_def{id = task_a, join_type = and},
        task_b => #task_def{id = task_b, join_type = and},
        task_c => #task_def{id = task_c, join_type = and},
        finish => #task_def{id = finish, join_type = and}
    },
    Flows = [
        #flow{from = input, to = start},
        %% AND-split: all branches enabled
        #flow{from = start, to = task_a, split_type = and},
        #flow{from = start, to = task_b, split_type = and},
        #flow{from = start, to = task_c, split_type = and},
        %% AND-join: wait for all branches
        #flow{from = task_a, to = finish, join_type = and},
        #flow{from = task_b, to = finish, join_type = and},
        #flow{from = task_c, to = finish, join_type = and},
        #flow{from = finish, to = output}
    ],
    {ok, #yawl_spec{id = <<"parallel">>, tasks = Tasks, flows = Flows, ...}}.
```

### Exclusive Choice and Simple Merge (WP4, WP5)

Choose one path based on conditions.

```erlang
init_workflow(_Args) ->
    Tasks = #{
        decide => #task_def{id = decide, split_type = xor},
        path_a => #task_def{id = path_a, kind = atomic},
        path_b => #task_def{id = path_b, kind = atomic},
        merge => #task_def{id = merge, join_type = xor}
    },
    %% Define conditions
    IsHighValue = fun(Data) -> maps:get(<<"amount">>, Data, 0) > 1000 end,
    IsLowValue = fun(Data) -> maps:get(<<"amount">>, Data, 0) =< 1000 end,
    Flows = [
        #flow{from = input, to = decide},
        %% XOR-split: one branch based on conditions
        #flow{from = decide, to = path_a, condition = IsHighValue, priority = 1},
        #flow{from = decide, to = path_b, condition = IsLowValue, priority = 2},
        %% XOR-join: first to arrive wins
        #flow{from = path_a, to = merge, join_type = xor},
        #flow{from = path_b, to = merge, join_type = xor},
        #flow{from = merge, to = output}
    ],
    {ok, #yawl_spec{id = <<"exclusive">>, tasks = Tasks, flows = Flows, ...}}.
```

### Arbitrary Cycle (WP10)

Loop back to a previous task.

```erlang
init_workflow(_Args) ->
    Tasks = #{
        process => #task_def{id = process, kind = atomic},
        check => #task_def{id = check, split_type = xor},
        retry => #task_def{id = retry, kind = atomic},
        complete => #task_def{id = complete, kind = atomic}
    },
    NeedsRetry = fun(Data) -> maps:get(<<"attempts">>, Data, 0) < 3 end,
    Flows = [
        #flow{from = input, to = process},
        #flow{from = process, to = check},
        %% Cycle back to process via retry
        #flow{from = check, to = retry, condition = NeedsRetry, is_cycle = true},
        #flow{from = check, to = complete},
        #flow{from = retry, to = process, is_cycle = true},
        #flow{from = complete, to = output}
    ],
    {ok, #yawl_spec{id = <<"cycle">>, tasks = Tasks, flows = Flows, ...}}.
```

### Cancellation (WP19, WP20)

Define cancellation regions for tasks.

```erlang
init_workflow(_Args) ->
    Tasks = #{
        start => #task_def{id = start, kind = atomic},
        review => #task_def{id = review, kind = atomic, cancellation_region = review_region},
        approve => #task_def{id = approve, kind = atomic, cancellation_region = review_region},
        reject => #task_def{id = reject, kind = atomic},
        done => #task_def{id = done, kind = atomic}
    },
    Flows = [
        #flow{from = input, to = start},
        #flow{from = start, to = review},
        #flow{from = review, to = approve},
        #flow{from = review, to = reject},
        #flow{from = approve, to = done},
        #flow{from = reject, to = done},
        #flow{from = done, to = output}
    ],
    Regions = [
        #cancellation_region{
            id = review_region,
            tasks = [review, approve],
            cancel_trigger = reject
        }
    ],
    {ok, #yawl_spec{
        id = <<"cancellation">>,
        tasks = Tasks,
        flows = Flows,
        cancellation_regions = Regions,
        ...
    }}.
```

---

## API Reference

### Starting Cases

```erlang
%% Start with initial data
{ok, CaseId, Pid} = gen_yawl_api:start_case(my_workflow, #{
    case_data => #{<<"user">> => <<"alice">>, <<"amount">> => 100},
    metadata => #{<<"priority">> => high}
}).
```

### Managing Work Items

```erlang
%% List enabled work items
{ok, WorkItems} = gen_yawl_api:list_enabled_work_items(CaseId).

%% Get specific work item
{ok, WorkItem} = gen_yawl_api:get_work_item(CaseId, WorkItemId).

%% Complete with result
{ok, Receipt, NextTasks} = gen_yawl_api:complete_work_item(
    CaseId,
    WorkItemId,
    #{<<"result">> => <<"approved">>}
).

%% Start a work item (manual execution mode)
{ok, Receipt} = gen_yawl_api:start_work_item(CaseId, WorkItemId, my_resource).

%% Cancel a work item
ok = gen_yawl_api:cancel_work_item(CaseId, WorkItemId).
```

### Querying State

```erlang
%% Get full case state
{ok, State} = gen_yawl_api:get_case_state(CaseId).

%% Get active work items
{ok, ActiveItems} = gen_yawl_api:list_active_work_items(CaseId).

%% Get execution receipts
{ok, Receipts} = gen_yawl_api:get_case_receipts(CaseId).

%% Get case history
{ok, History} = gen_yawl_api:get_case_history(CaseId).
```

### Case Lifecycle

```erlang
%% Suspend a case
ok = gen_yawl_api:suspend_case(CaseId).

%% Resume a suspended case
ok = gen_yawl_api:resume_case(CaseId).

%% Stop a case
ok = gen_yawl_api:stop_case(CaseId).

%% Cancel a case (WP20)
ok = gen_yawl_api:cancel_case(CaseId).
```

---

## Event Handling

### Subscribing to Events

```erlang
%% Subscribe to all case events
{ok, SubId} = gen_yawl_api:subscribe_to_case(CaseId, self()).

%% Subscribe to specific work item
{ok, SubId} = gen_yawl_api:subscribe_to_work_item(WorkItemId, self()).

%% Handle events in your process
handle_info({yawl_event, Event}, State) ->
    case Event of
        #work_item_enabled{work_item_id = Id, task_id = Task} ->
            io:format("Task ~p enabled~n", [Task]);
        #work_item_completed{work_item_id = Id, result = Result} ->
            io:format("Work item completed: ~p~n", [Result]);
        #case_completed{case_id = CaseId} ->
            io:format("Case ~p completed~n", [CaseId])
    end,
    {noreply, State}.

%% Unsubscribe
ok = gen_yawl_api:unsubscribe(SubId).
```

### Event Types

```erlang
-record(work_item_enabled, {
    case_id     :: binary(),
    work_item_id :: binary(),
    task_id     :: atom(),
    enabled_at  :: integer()
}).

-record(work_item_started, {
    case_id     :: binary(),
    work_item_id :: binary(),
    task_id     :: atom(),
    resource    :: atom(),
    started_at  :: integer()
}).

-record(work_item_completed, {
    case_id     :: binary(),
    work_item_id :: binary(),
    task_id     :: atom(),
    result      :: map(),
    completed_at :: integer()
}).

-record(case_completed, {
    case_id     :: binary(),
    completed_at :: integer(),
    receipts    :: [#receipt{}]
}).

-record(case_cancelled, {
    case_id     :: binary(),
    reason      :: term(),
    cancelled_at :: integer()
}).
```

---

## Error Handling

### Task Execution Errors

```erlang
execute_task(#work_item{task_id = risky_task, data = Data}, State) ->
    try
        Result = do_risky_operation(Data),
        {ok, Result, State}
    catch
        throw:Reason ->
            logger:error("Task failed: ~p", [Reason]),
            {error, {task_failed, risky_task, Reason}};
        error:Reason ->
            logger:error("Task crashed: ~p", [Reason]),
            {error, {task_crashed, risky_task, Reason}};
        exit:Reason ->
            {error, {task_exited, risky_task, Reason}}
    end.
```

### Handling Failed Cases

```erlang
%% Check case status
{ok, State} = gen_yawl_api:get_case_state(CaseId),
case State#yawl_state.status of
    failed ->
        logger:error("Case ~p failed", [CaseId]),
        handle_failure(State);
    completed ->
        logger:info("Case ~p completed successfully", [CaseId]);
    cancelled ->
        logger:warning("Case ~p was cancelled", [CaseId])
end.
```

### Retry Logic

```erlang
handle_work_item_completed(#work_item{task_id = TaskId}, Result, State) ->
    Attempts = maps:get(<<"attempts">>, Result, 0),
    case maps:get(<<"success">>, Result, false) of
        true ->
            {ok, [next_task], State};
        false when Attempts < 3 ->
            %% Retry the same task
            {ok, [TaskId], State};
        false ->
            %% Give up, move to error handling
            {ok, [error_handler], State}
    end.
```

---

## Testing

### Unit Testing with Proper

```erlang
-module(my_workflow_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Property: sequence completes in order
prop_sequence_completes_in_order() ->
    ?FORALL(Data, vector(3, integer()),
        begin
            {ok, CaseId, _Pid} = gen_yawl_api:start_case(my_workflow, #{
                case_data => #{<<"values">> => Data}
            }),
            {ok, State} = await_completion(CaseId, 5000),
            State#yawl_state.status =:= completed
        end).

%% Test case
parallel_split_test() ->
    {ok, CaseId, _Pid} = gen_yawl_api:start_case(parallel_workflow, #{}),
    {ok, WorkItems} = gen_yawl_api:list_enabled_work_items(CaseId),
    ?assertEqual(3, length(WorkItems)),  %% 3 parallel branches
    ok.
```

### Integration Testing

```erlang
workflow_integration_test() ->
    %% Start workflow
    {ok, CaseId, Pid} = gen_yawl_api:start_case(integration_workflow, #{
        case_data => test_data()
    }),

    %% Subscribe to events
    {ok, _SubId} = gen_yawl_api:subscribe_to_case(CaseId, self()),

    %% Complete tasks manually
    receive
        {yawl_event, #work_item_enabled{work_item_id = W1}} ->
            ok = gen_yawl_api:complete_work_item(CaseId, W1, #{})
    end,

    %% Verify completion
    {ok, State} = await_completion(CaseId, 10000),
    ?assertEqual(completed, State#yawl_state.status),
    ?assertEqual(5, length(State#yawl_state.receipts)),
    ok.
```

---

## Best Practices

### 1. Keep Tasks Idempotent

```erlang
%% Good: idempotent
execute_task(#work_item{id = Id, data = Data}, State) ->
    case already_processed(Id) of
        true -> {ok, #{<<"cached">> => true}, State};
        false -> do_process(Id, Data, State)
    end.

%% Avoid: non-idempotent side effects
execute_task(#work_item{data = Data}, State) ->
    send_email(Data),  %% Might send duplicate emails
    {ok, #{}, State}.
```

### 2. Use Timeouts

```erlang
#task_def{
    id = external_task,
    name = <<"External Call">>,
    kind = atomic,
    timeout = 30000  %% 30 second timeout
}
```

### 3. Validate Input Early

```erlang
validate_case_data(CaseData, _Spec) ->
    Required = [<<"user">>, <<"amount">>],
    case lists:all(fun(K) -> maps:is_key(K, CaseData) end, Required) of
        true -> ok;
        false -> {error, {missing_fields, Required -- maps:keys(CaseData)}}
    end.
```

### 4. Log Important Events

```erlang
handle_work_item_completed(#work_item{task_id = TaskId}, Result, State) ->
    logger:info("Task ~p completed for case ~p with result: ~p",
        [TaskId, State#yawl_state.case_id, Result]),
    {ok, [], State}.
```

### 5. Use Receipts for Audit

```erlang
%% Export receipts for compliance
{ok, Receipts} = gen_yawl_api:get_case_receipts(CaseId),
gen_yawl_api:export_receipts(CaseId, "/tmp/audit/" ++ CaseId ++ ".ttl").
```

### 6. Design for Composability

```erlang
%% Reusable sub-workflow
sub_workflow_spec() ->
    #yawl_spec{
        id = <<"common-approval">>,
        tasks = #{
            notify => #task_def{id = notify, ...},
            record => #task_def{id = record, ...}
        },
        flows = [...]
    }.

%% Compose into larger workflow
init_workflow(_Args) ->
    Spec = #yawl_spec{
        id = <<"main-workflow">>,
        tasks = maps:merge(
            main_tasks(),
            sub_workflow_tasks()  %% Reuse sub-workflow
        ),
        flows = main_flows() ++ sub_workflow_flows()
    },
    {ok, Spec}.
```

---

## Advanced Topics

### RDF Workflow Definitions

Define workflows in Turtle RDF:

```turtle
@prefix yawl: <http://unrdf.org/yawl#> .

<workflow/my-approval> a yawl:WorkflowSpec ;
    yawl:taskId "my-approval" ;
    yawl:taskName "My Approval Workflow" ;
    yawl:hasTask <task/submit>, <task/approve> .

<task/submit> a yawl:AtomicTask ;
    yawl:taskId "submit" ;
    yawl:taskName "Submit Request" ;
    yawl:splitBehavior yawl:XOR_Split .

<task/approve> a yawl:AtomicTask ;
    yawl:taskId "approve" ;
    yawl:taskName "Approve Request" ;
    yawl:joinBehavior yawl:XOR_Join .
```

Load from RDF:

```erlang
{ok, Spec} = gen_yawl_rdf:load_workflow_from_rdf(<<"workflow/my-approval">>).
```

### Distributed Execution

```erlang
%% Enable clustering
{gen_yawl, [
    {cluster_mode, true},
    {case_replication, quorum},
    {sync_strategy, gossip}
]}.

%% Cases automatically replicate across nodes
{ok, CaseId, _Pid} = gen_yawl_api:start_case(my_workflow, #{}).
%% Case is now available on all cluster nodes
```

---

For complete pattern reference, see [PATTERNS.md](PATTERNS.md).
