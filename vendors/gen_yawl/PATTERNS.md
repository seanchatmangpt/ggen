# Van der Aalst Workflow Patterns Reference

Complete reference for all 20 Van der Aalst workflow control-flow patterns as implemented in `gen_yawl`.

## Table of Contents

1. [Control-Flow Patterns](#control-flow-patterns)
2. [Pattern Reference](#pattern-reference)
3. [Implementation Examples](#implementation-examples)
4. [Performance Characteristics](#performance-characteristics)

---

## Control-Flow Patterns

The 20 patterns are organized into categories:

| Category | Patterns |
|----------|----------|
| **Basic Control Flow** | WP1-WP5: Sequence, Parallel Split, Synchronization, Exclusive Choice, Simple Merge |
| **Advanced Branching and Sync** | WP6-WP9: Multi-Choice, Structured Sync Merge, Multi-Merge, Discriminator |
| **Structural Patterns** | WP10-WP11: Arbitrary Cycle, Implicit Termination |
| **Multiple Instances** | WP12-WP14: MI w/o Synchronization, MI w/ Design-time Knowledge, N-out-of-M |
| **State-Based Patterns** | WP15-WP16: State-based Choice, Deferred Choice |
| **Cancellation Patterns** | WP17-WP20: Interleaved, Milestone, Cancel Task, Cancel Case |

---

## Pattern Reference

### WP1: Sequence

**Description**: Tasks execute one after another in order.

**Visual**:
```
Input --> [Task A] --> [Task B] --> [Task C] --> Output
```

**When to Use**:
- Simple linear processes
- Step-by-step approvals
- Sequential data processing

**Implementation**:
```erlang
#flow{from = input, to = task_a},
#flow{from = task_a, to = task_b},
#flow{from = task_b, to = task_c},
#flow{from = task_c, to = output}
```

**Performance**: O(n) - linear with task count

---

### WP2: Parallel Split

**Description**: Split into multiple concurrent branches.

**Visual**:
```
                    --> [Branch A] -->
Input --> [Split] --|--> [Branch B] -->--> [Join] --> Output
                    --> [Branch C] -->
```

**When to Use**:
- Independent concurrent tasks
- Multi-party approvals
- Parallel data processing

**Implementation**:
```erlang
%% Define split task
split => #task_def{id = split, split_type = and},

%% Branch flows
#flow{from = split, to = branch_a, split_type = and},
#flow{from = split, to = branch_b, split_type = and},
#flow{from = split, to = branch_c, split_type = and},
```

**Performance**: O(1) - constant time, concurrent execution

---

### WP3: Synchronization

**Description**: Wait for all parallel branches to complete.

**Visual**:
```
[Branch A] --|
             |--> [Join] --> Output
[Branch C] --|
```

**When to Use**:
- Collecting results from parallel tasks
- Ensuring all approvals received
- Merging parallel data streams

**Implementation**:
```erlang
%% Define join task
join => #task_def{id = join, join_type = and},

%% Branch flows into join
#flow{from = branch_a, to = join, join_type = and},
#flow{from = branch_b, to = join, join_type = and},
#flow{from = branch_c, to = join, join_type = and},
```

**Performance**: O(n) - waits for slowest branch

---

### WP4: Exclusive Choice

**Description**: Choose exactly one branch based on conditions.

**Visual**:
```
            --> [Branch A] (condition A)
Input --|--> [Branch B] (condition B)
            --> [Branch C] (default)
```

**When to Use**:
- Conditional routing
- Data-based decisions
- Risk-based processing

**Implementation**:
```erlang
%% Define choice task
choice => #task_def{id = choice, split_type = xor},

%% Conditional flows
#flow{
    from = choice,
    to = branch_a,
    condition = fun(Data) -> maps:get(<<"risk">>, Data) =:= low end,
    priority = 1
},
#flow{
    from = choice,
    to = branch_b,
    condition = fun(Data) -> maps:get(<<"risk">>, Data) =:= medium end,
    priority = 2
},
#flow{
    from = choice,
    to = branch_c,
    is_default = true
},
```

**Performance**: O(1) - single branch executed

---

### WP5: Simple Merge

**Description**: Merge multiple branches without synchronization.

**Visual**:
```
[Branch A] --|
             |--> [Next Task]
[Branch C] --|
```

**When to Use**:
- Re-converging exclusive paths
- Alternative completion paths
- Non-synchronized merging

**Implementation**:
```erlang
%% Define merge task
merge => #task_def{id = merge, join_type = xor},

#flow{from = branch_a, to = merge, join_type = xor},
#flow{from = branch_b, to = merge, join_type = xor},
#flow{from = branch_c, to = merge, join_type = xor},
```

**Performance**: O(1) - first arriving branch proceeds

---

### WP6: Multi-Choice

**Description**: Enable one or more branches based on conditions.

**Visual**:
```
            --> [Branch A] (condition A) --|
Input --|--> [Branch B] (condition B)---->|--> [Sync]
            --> [Branch C] (condition C) --|
```

**When to Use**:
- Dynamic parallel execution
- Conditional multi-party notifications
- Flexible approval chains

**Implementation**:
```erlang
multi_choice => #task_def{id = multi_choice, split_type = or},

#flow{
    from = multi_choice,
    to = branch_a,
    condition = fun(Data) -> needs_a(Data) end,
    split_type = or
},
#flow{
    from = multi_choice,
    to = branch_b,
    condition = fun(Data) -> needs_b(Data) end,
    split_type = or
},
```

**Performance**: O(k) where k = number of enabled branches

---

### WP7: Structured Synchronization Merge

**Description**: Wait for all ACTIVATED branches (not all possible branches).

**Visual**:
```
[Branch A] --|
             |--> [Sync Merge] --> Output
[Branch C] --|
(Only wait for branches that were actually activated)
```

**When to Use**:
- Sync after multi-choice
- Dynamic parallel completion
- Partial synchronization

**Implementation**:
```erlang
sync_merge => #task_def{id = sync_merge, join_type = or},

#flow{from = branch_a, to = sync_merge, join_type = or},
#flow{from = branch_b, to = sync_merge, join_type = or},
```

**Performance**: O(k) where k = number of activated branches

---

### WP8: Multi-Merge

**Description**: Merge without tracking which branches were activated.

**Visual**:
```
[Branch A] --|
             |--> [Next Task]
[Branch C] --|
(Any branch can proceed, no state tracking)
```

**When to Use**:
- Simple converging paths
- Independent completion
- Fire-and-forget merging

**Implementation**:
```erlang
multi_merge => #task_def{id = multi_merge, join_type = xor},

#flow{from = branch_a, to = multi_merge},
#flow{from = branch_b, to = multi_merge},
```

**Performance**: O(1) - no state tracking overhead

---

### WP9: Discriminator

**Description**: Wait for first branch to complete, then cancel others.

**Visual**:
```
[Branch A] --|
             |--> [Discriminator] --> Output
[Branch C] --|
(First to arrive wins, others are cancelled)
```

**When to Use**:
- Racing alternatives
- Competitive parallel execution
- First-response processing

**Implementation**:
```erlang
discriminator => #task_def{
    id = discriminator,
    join_type = xor,
    cancellation_set = [branch_a, branch_b, branch_c]
},

#flow{from = branch_a, to = discriminator},
#flow{from = branch_b, to = discriminator},
#flow{from = branch_c, to = discriminator},
```

**Performance**: O(1) - cancels remaining branches

---

### WP10: Arbitrary Cycle

**Description**: Loop back to a previous task with a condition.

**Visual**:
```
Input --> [Process] --> [Check] --> Output
                            |
                            v
                         [Retry]
                            |
                            +----> (back to Process)
```

**When to Use**:
- Retry loops
- Iterative processing
- Recomputation with updated data

**Implementation**:
```erlang
#flow{
    from = check,
    to = retry,
    condition = fun(Data) -> maps:get(<<"attempts">>, Data, 0) < 3 end,
    is_cycle = true
},
#flow{
    from = retry,
    to = process,
    is_cycle = true
},
#flow{
    from = check,
    to = output,
    condition = fun(Data) -> maps:get(<<"attempts">>, Data, 0) >= 3 end
},
```

**Performance**: O(n) where n = max iterations

---

### WP11: Implicit Termination

**Description**: Workflow ends when no active tokens remain.

**Visual**:
```
[Task] --> [Task] --> [Task]
(Ends when no more tasks can execute)
```

**When to Use**:
- Dynamic completion detection
- Unknown completion point
- Self-terminating processes

**Implementation**:
Automatic - no explicit configuration needed. The workflow terminates when:

```erlang
%% All conditions are met:
%% 1. No enabled work items
%% 2. No active tokens in transit
%% 3. Not in a wait state
```

**Performance**: O(1) detection per state change

---

### WP12: Multiple Instances (No Synchronization)

**Description**: Create multiple parallel instances of a task.

**Visual**:
```
Input --> [Task Instance 1] --> Output
       --> [Task Instance 2] -----^
       --> [Task Instance N] -----^
```

**When to Use**:
- Batch processing
- Parallel independent operations
- Multi-item processing

**Implementation** (Planned):
```erlang
process_batch => #task_def{
    id = process_batch,
    kind = multiple_instance,
    split_type = and,
    instance_count = fun(Data) -> length(maps:get(<<"items">>, Data)) end
},
```

**Performance**: O(n) parallel where n = instance count

---

### WP13: Multiple Instances (Sequential)

**Description**: Execute instances one at a time.

**Visual**:
```
Input --> [Instance 1] --> [Instance 2] --> ... --> [Instance N] --> Output
```

**When to Use**:
- Resource-constrained processing
- Ordered batch processing
- Sequential state updates

**Implementation** (Planned):
```erlang
process_sequential => #task_def{
    id = process_sequential,
    kind = multiple_instance,
    execution_mode = sequential,
    instance_count = 5
},
```

**Performance**: O(n) sequential where n = instance count

---

### WP14: N-out-of-M

**Description**: Wait for N of M parallel instances to complete.

**Visual**:
```
[Inst 1] --|
           |
[Inst 2] --|--> [N/M Join] --> Output (when N complete)
           |
[Inst M] --|
```

**When to Use**:
- Quorum-based decisions
- Partial completion acceptance
- Fault-tolerant parallel execution

**Implementation** (Planned):
```erlang
quorum_join => #task_def{
    id = quorum_join,
    join_type = n_of_m,
    required_count = 3,
    total_count = 5
},
```

**Performance**: O(N) where N = required count

---

### WP15: State-Based Choice

**Description**: Route based on workflow state, not just data.

**Visual**:
```
         [State A?] --> [Branch A]
Input --|
         [State B?] --> [Branch B]
```

**When to Use**:
- State-machine workflows
- Context-aware routing
- History-dependent decisions

**Implementation** (Planned):
```erlang
#flow{
    from = check_state,
    to = branch_a,
    condition = fun(_Data, State) ->
        sets:is_element(prev_task_a, State#yawl_state.completed_tasks)
    end
},
```

**Performance**: O(1) - state lookup

---

### WP16: Deferred Choice

**Description**: Enable multiple branches, but external events decide which executes.

**Visual**:
```
                    (wait for event)
Input --> [Enable] --|--> [Branch A] (event A)
                   --|--> [Branch B] (event B)
```

**When to Use**:
- External event triggers
- User choice workflows
- Timeout-based selection

**Implementation**:
```erlang
deferred => #task_def{id = deferred, split_type = deferred},

%% Create candidate work items that wait for external trigger
handle_work_item_enabled(#work_item{task_id = deferred}, State) ->
    Candidates = [option_a, option_b, option_c],
    lists:foreach(fun(C) ->
        gen_yawl_api:create_candidate_work_item(
            State#yawl_state.case_id, C, Candidates)
    end, Candidates),
    {ok, State};

%% External trigger selects the winner
select_option(CaseId, Option) ->
    gen_yawl_api:select_deferred_choice(CaseId, Option).
```

**Performance**: O(1) - first event wins

---

### WP17: Interleaved Parallel Routing

**Description**: Execute tasks in interleaved fashion (not true parallel).

**Visual**:
```
[Task A] --> [Mutex] --> [Task A continues]
[Task B] --> [Mutex] --> [Task B continues]
```

**When to Use**:
- Shared resource access
- Non-atomic parallel processing
- Interleaved execution requirements

**Implementation** (Planned):
```erlang
mutex_region => #task_def{
    id = mutex_region,
    kind = mutex,
    tasks = [task_a, task_b],
    mutex_strategy = round_robin
},
```

**Performance**: O(n) with mutex overhead

---

### WP18: Milestone

**Description**: Task only executes if workflow is in a specific state.

**Visual**:
```
[Milestone A reached] --> [Enabled Task]
(Only enabled if milestone passed)
```

**When to Use**:
- Guarded task execution
- State-dependent authorization
- Progressive workflows

**Implementation** (Planned):
```erlang
final_approval => #task_def{
    id = final_approval,
    kind = atomic,
    milestone = tech_review_completed,
    milestone_check = fun(State) ->
        sets:is_element(tech_review, State#yawl_state.completed_tasks)
    end
},
```

**Performance**: O(1) - state check before enablement

---

### WP19: Cancel Task

**Description**: Cancel a specific task and its region.

**Visual**:
```
[Trigger] --> (cancel) [Region: A, B, C]
```

**When to Use**:
- Early termination scenarios
- Exception handling
- Alternative path cancellation

**Implementation**:
```erlang
%% Define cancellation region
Region = #cancellation_region{
    id = review_region,
    tasks = [review_task, approve_task, notify_task],
    cancel_trigger = reject_task
},

%% Add to spec
#yawl_spec{
    cancellation_regions = [Region],
    ...
}.
```

**Performance**: O(k) where k = tasks in region

---

### WP20: Cancel Case

**Description**: Cancel entire workflow case.

**Visual**:
```
[Trigger] --> (cancel entire case)
```

**When to Use**:
- Emergency stop
- Global exception handling
- Workflow termination

**Implementation**:
```erlang
%% Programmatically cancel
gen_yawl_api:cancel_case(CaseId).

%% Or via cancellation region with all tasks
Region = #cancellation_region{
    id = entire_case,
    tasks = all,  %% Magic value for all tasks
    cancel_trigger = abort_task
},
```

**Performance**: O(n) where n = total tasks in case

---

## Implementation Examples

### Complete Example: Document Approval

Combines WP1 (Sequence), WP2+WP3 (Parallel Split/Sync), WP4+WP5 (Exclusive Choice/Merge), and WP10 (Cycle).

```erlang
-module(document_approval).
-behaviour(gen_yawl).

-export([
    init_workflow/1,
    execute_task/2,
    handle_work_item_completed/3
]).

-include_lib("gen_yawl/include/gen_yawl.hrl").

init_workflow(_Args) ->
    Spec = #yawl_spec{
        id = <<"doc-approval">>,
        name = <<"Document Approval Process">>,
        version = {1, 0},
        tasks = #{
            %% WP1: Sequence start
            submit => #task_def{
                id = submit,
                name = <<"Submit Document">>,
                kind = atomic,
                split_type = xor  %% WP4: Exclusive choice follows
            },

            %% WP4: Branch based on document type
            standard_review => #task_def{
                id = standard_review,
                name = <<"Standard Review">>,
                kind = atomic
            },

            urgent_review => #task_def{
                id = urgent_review,
                name = <<"Urgent Review">>,
                kind = atomic,
                split_type = and  %% WP2: Parallel split follows
            },

            %% WP2: Parallel reviews
            legal_review => #task_def{
                id = legal_review,
                name = <<"Legal Review">>,
                kind = atomic,
                join_type = and
            },

            tech_review => #task_def{
                id = tech_review,
                name = <<"Technical Review">>,
                kind = atomic,
                join_type = and
            },

            %% WP3: Synchronization
            finalize => #task_def{
                id = finalize,
                name = <<"Finalize Approval">>,
                kind = atomic,
                split_type = xor  %% WP4: Choice follows
            },

            %% WP5: Simple merge
            complete => #task_def{
                id = complete,
                name = <<"Complete">>,
                kind = atomic,
                join_type = xor
            },

            %% WP10: Cycle back
            revise => #task_def{
                id = revise,
                name = <<"Revise Document">>,
                kind = atomic
            }
        },
        flows = [
            %% Start
            #flow{from = input, to = submit},

            %% WP4: Exclusive choice based on urgency
            #flow{
                from = submit,
                to = standard_review,
                condition = fun(D) -> not maps:get(<<"urgent">>, D, false) end,
                priority = 1
            },
            #flow{
                from = submit,
                to = urgent_review,
                condition = fun(D) -> maps:get(<<"urgent">>, D, false) end,
                priority = 2
            },

            %% Standard path
            #flow{from = standard_review, to = finalize},

            %% Urgent path: WP2 parallel split
            #flow{from = urgent_review, to = legal_review, split_type = and},
            #flow{from = urgent_review, to = tech_review, split_type = and},

            %% WP3: Synchronization
            #flow{from = legal_review, to = finalize, join_type = and},
            #flow{from = tech_review, to = finalize, join_type = and},

            %% WP4: After finalize, approve or revise
            #flow{
                from = finalize,
                to = complete,
                condition = fun(D) -> maps:get(<<"approved">>, D, false) end
            },
            #flow{
                from = finalize,
                to = revise,
                condition = fun(D) -> not maps:get(<<"approved">>, D, true) end
            },

            %% WP10: Cycle back
            #flow{from = revise, to = submit, is_cycle = true},

            %% End
            #flow{from = complete, to = output}
        ],
        input_condition = input,
        output_condition = output
    },
    {ok, Spec}.

execute_task(#work_item{task_id = TaskId, data = Data}, State) ->
    Result = case TaskId of
        submit ->
            #{<<"submitted_at">> => os:system_time(millisecond)};
        standard_review ->
            %% Auto-approve standard docs
            #{<<"approved">> => true};
        urgent_review ->
            #{?>"review_type">> => <<"urgent">>};
        legal_review ->
            #{<<"legal_approved">> => true};
        tech_review ->
            #{<<"tech_approved">> => true};
        finalize ->
            %% Check if all reviews passed
            Approved = maps:get(<<"tech_approved">>, Data, false) andalso
                      maps:get(<<"legal_approved">>, Data, false),
            #{<<"approved">> => Approved};
        revise ->
            #{<<"revision_count">> => maps:get(<<"revision_count">>, Data, 0) + 1};
        complete ->
            #{<<"completed_at">> => os:system_time(millisecond)}
    end,
    {ok, Result, State}.

handle_work_item_completed(_WorkItem, _Result, State) ->
    {ok, [], State}.  %% Automatic flow following
```

---

## Performance Characteristics

| Pattern | Time Complexity | Space Complexity | Notes |
|---------|----------------|------------------|-------|
| WP1: Sequence | O(n) | O(1) | Linear execution |
| WP2: Parallel Split | O(1) | O(k) | k = branches |
| WP3: Synchronization | O(k) | O(1) | Wait for all branches |
| WP4: Exclusive Choice | O(1) | O(1) | Single branch |
| WP5: Simple Merge | O(1) | O(1) | No synchronization |
| WP6: Multi-Choice | O(k) | O(k) | k = enabled branches |
| WP7: Structured Sync | O(k) | O(k) | k = activated branches |
| WP8: Multi-Merge | O(1) | O(1) | No tracking |
| WP9: Discriminator | O(1) | O(k) | Cancel remaining |
| WP10: Arbitrary Cycle | O(n*i) | O(1) | i = iterations |
| WP11: Implicit Term | O(1) | O(1) | Detection overhead |
| WP12: MI No Sync | O(n) | O(n) | n = instances |
| WP13: MI Sequential | O(n) | O(1) | Sequential processing |
| WP14: N-of-M | O(N) | O(M) | N = required, M = total |
| WP15: State Choice | O(1) | O(1) | State lookup |
| WP16: Deferred Choice | O(1) | O(k) | Event-driven |
| WP17: Interleaved | O(n) | O(1) | Mutex overhead |
| WP18: Milestone | O(1) | O(1) | State check |
| WP19: Cancel Task | O(k) | O(1) | k = region size |
| WP20: Cancel Case | O(n) | O(1) | n = total tasks |

---

## Pattern Selection Guide

| Requirement | Recommended Pattern(s) |
|-------------|------------------------|
| Simple step-by-step | WP1: Sequence |
| Parallel independent tasks | WP2+WP3: Parallel Split/Sync |
| Conditional routing | WP4+WP5: Exclusive Choice/Merge |
| Dynamic parallel execution | WP6+WP7: Multi-Choice/Sync Merge |
| Retry/loops | WP10: Arbitrary Cycle |
| Batch processing | WP12: Multiple Instances |
| Partial completion OK | WP14: N-out-of-M |
| External triggers | WP16: Deferred Choice |
| Exception handling | WP19+WP20: Cancellation |

---

For usage tutorials, see [GUIDE.md](GUIDE.md).
