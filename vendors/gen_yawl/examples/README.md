# gen_yawl Examples

Collection of workflow examples demonstrating various Van der Aalst patterns and usage patterns.

## Table of Contents

1. [Quick Start](#quick-start)
2. [Example List](#example-list)
3. [Running Examples](#running-examples)
4. [Expected Outputs](#expected-outputs)

---

## Quick Start

### Start the Shell

```bash
cd /path/to/gen_yawl
rebar3 shell
```

### Run an Example

```erlang
%% Start gen_yawl
application:ensure_all_started(gen_yawl).

%% Run an example
{ok, CaseId} = simple_sequence:run().

%% Check the result
{ok, State} = gen_yawl_api:get_case_state(CaseId).
```

---

## Example List

### 1. Simple Sequence

**File**: `simple_sequence.erl`

**Patterns**: WP1 (Sequence)

**Description**: A minimal workflow with three sequential tasks. Each task completes before the next begins.

**Visual**:
```
Input --> [Submit] --> [Review] --> [Approve] --> Output
```

**Key Concepts**:
- Basic workflow definition
- Task execution order
- Automatic flow following

**Run**:
```erlang
{ok, CaseId} = simple_sequence:run().
```

**Expected Output**:
```
Case started: <<"simple-seq-...">>
Task submit enabled
Task submit completed
Task review enabled
Task review completed
Task approve enabled
Task approve completed
Case completed
```

---

### 2. Parallel Approval

**File**: `parallel_approval.erl`

**Patterns**: WP2 (Parallel Split), WP3 (Synchronization)

**Description**: A document approval workflow that routes to multiple reviewers in parallel and waits for all to complete before finalizing.

**Visual**:
```
                    --> [Legal Review] --|
Input --> [Submit] --|--> [Tech Review] -->--> [Finalize] --> Output
                    --> [Finance Review]-|
```

**Key Concepts**:
- AND-split behavior
- AND-join synchronization
- Waiting for all parallel branches

**Run**:
```erlang
{ok, CaseId} = parallel_approval:run().
```

**Expected Output**:
```
Case started: <<"parallel-apr-...">>
Task submit enabled
Task submit completed
3 parallel branches enabled:
  - legal_review
  - tech_review
  - finance_review
[Awaiting all branches...]
All branches completed, finalize enabled
Task finalize completed
Case completed
```

---

### 3. Conditional Routing

**File**: `conditional_routing.erl`

**Patterns**: WP4 (Exclusive Choice), WP5 (Simple Merge)

**Description**: Routes documents based on amount to different approval paths. Low amounts auto-approve, medium amounts need manual review, high amounts are rejected.

**Visual**:
```
                --> [Auto Approve] --|
Input -->[Check]                      |--> [Done]
                --> [Manual Review]---|
                --> [Reject]---------|
```

**Key Concepts**:
- XOR-split with conditions
- Conditional flow evaluation
- Priority-based branch selection

**Run**:
```erlang
%% Low amount - auto approve
{ok, CaseId1} = conditional_routing:run(#{<<"amount">> => 500}).

%% Medium amount - manual review
{ok, CaseId2} = conditional_routing:run(#{<<"amount">> => 5000}).

%% High amount - reject
{ok, CaseId3} = conditional_routing:run(#{<<"amount">> => 50000}).
```

**Expected Output** (for low amount):
```
Case started with amount: 500
Condition check: amount < 1000
Branch selected: auto_approve
Auto approval completed
Case completed
```

---

### 4. Multi-Choice Approval

**File**: `multi_choice_approval.erl`

**Patterns**: WP6 (Multi-Choice), WP7 (Structured Synchronization)

**Description**: Notifies multiple stakeholders based on document type. Only relevant departments are notified, and all notified departments must respond before proceeding.

**Visual**:
```
                    --> [Legal] --|
Input -->[Analyze] --|--> [Tech] --->-- [Sync] --> Output
                    --> [Finance]-|
(Only relevant branches activated)
```

**Key Concepts**:
- OR-split with dynamic branches
- Active branch tracking
- Structured synchronization merge

**Run**:
```erlang
%% Legal only
{ok, CaseId1} = multi_choice_approval:run(#{<<"type">> => legal}).

%% Legal + Tech
{ok, CaseId2} = multi_choice_approval:run(#{<<"type">> => product}).

%% All departments
{ok, CaseId3} = multi_choice_approval:run(#{<<"type">> => acquisition}).
```

**Expected Output** (for Legal + Tech):
```
Document type: product
Activated branches: [legal, tech]
Waiting for 2 responses...
Legal review completed
Tech review completed
All active branches completed
Case completed
```

---

### 5. Retry with Cycle

**File**: `retry_cycle.erl`

**Patterns**: WP10 (Arbitrary Cycle)

**Description**: Demonstrates a retry loop. Tasks can fail and trigger a retry flow with a maximum attempt limit.

**Visual**:
```
Input --> [Process] --> [Check Result] --> Output
                            |
                            v
                         [Retry]
                            |
                            +----> (back to Process)
```

**Key Concepts**:
- Backward flows (cycles)
- Loop condition evaluation
- Attempt counting and limits

**Run**:
```erlang
%% Succeeds on first try
{ok, CaseId1} = retry_cycle:run(success_first).

%% Succeeds after retries
{ok, CaseId2} = retry_cycle:run(success_after_3).

%% Fails after max retries
{ok, CaseId3} = retry_cycle:run(fail_always).
```

**Expected Output** (for success after retries):
```
Processing attempt 1...
Attempt 1 failed
Processing attempt 2...
Attempt 2 failed
Processing attempt 3...
Attempt 3 succeeded
Case completed
```

---

### 6. Cancellation

**File**: `cancellation.erl`

**Patterns**: WP19 (Cancel Task), WP20 (Cancel Case)

**Description**: Demonstrates cancellation regions. Rejecting a document cancels all pending review tasks.

**Visual**:
```
Input --> [Submit] --> [Review] --|
                |                 |--> [Cancel Region]
                v                 |
             [Reject] ------------|
```

**Key Concepts**:
- Cancellation region definition
- Trigger-based cancellation
- Task withdrawal

**Run**:
```erlang
%% Normal flow - complete
{ok, CaseId1} = cancellation:run(complete).

%% Trigger cancellation
{ok, CaseId2} = cancellation:run(cancel).
```

**Expected Output** (for cancel):
```
Review task enabled
Reject triggered
Cancellation region activated: review_region
Review task cancelled
Case cancelled
```

---

### 7. Deferred Choice

**File**: `deferred_choice.erl`

**Patterns**: WP16 (Deferred Choice)

**Description**: Creates multiple candidate work items and waits for external selection (e.g., user input, timeout, event).

**Visual**:
```
                    (wait for external event)
Input --> [Enable] --|--> [Option A]
                   --|--> [Option B]
```

**Key Concepts**:
- Candidate work items
- Event-driven selection
- Race condition handling

**Run**:
```erlang
{ok, CaseId} = deferred_choice:run().

%% Query candidates
{ok, Candidates} = deferred_choice:list_candidates(CaseId).

%% Select one
ok = deferred_choice:select(CaseId, option_a).
```

**Expected Output**:
```
Enabled 2 candidate work items:
  - option_a
  - option_b
Waiting for external selection...
Selected: option_a
Other candidates cancelled
Case completed
```

---

### 8. Discriminator

**File**: `discriminator.erl`

**Patterns**: WP9 (Discriminator)

**Description**: Race between multiple options. First to complete wins; others are cancelled.

**Visual**:
```
[Option A] --|
             |--> [Discriminator] --> Output
[Option C] --|
(First wins, rest cancelled)
```

**Key Concepts**:
- Racing branches
- First-completion wins
- Automatic cancellation of losers

**Run**:
```erlang
{ok, CaseId} = discriminator:run().
```

**Expected Output**:
```
Started 3 racing options
Option b completed first!
Discriminator activated
Other options cancelled:
  - option_a (cancelled)
  - option_c (cancelled)
Case completed
```

---

### 9. Multiple Instances

**File**: `multiple_instances.erl`

**Patterns**: WP12 (Multiple Instances)

**Description**: Process multiple items in parallel. Each item gets its own task instance.

**Visual**:
```
Input --> [Item 1] --> Output
       --> [Item 2] ---^
       --> [Item 3] ---^
       --> [Item N] ---^
```

**Key Concepts**:
- Dynamic instance creation
- Parallel processing
- Instance tracking

**Run**:
```erlang
%% Process 5 items
{ok, CaseId} = multiple_instances:run([item1, item2, item3, item4, item5]).
```

**Expected Output**:
```
Creating 5 instances:
  - instance_1 for item1
  - instance_2 for item2
  - instance_3 for item3
  - instance_4 for item4
  - instance_5 for item5
All instances processing in parallel...
Instance 2 completed
Instance 5 completed
Instance 1 completed
Instance 3 completed
Instance 4 completed
All instances completed
Case completed
```

---

### 10. State-Based Routing

**File**: `state_based_routing.erl`

**Patterns**: WP15 (State-based Choice)

**Description**: Routes tasks based on workflow state (completed tasks, history) rather than just input data.

**Visual**:
```
                    [State A completed?]
Input --> [Check] --|--> [Branch A]
                    |
                    [State B completed?]
                    --> [Branch B]
```

**Key Concepts**:
- State-dependent routing
- History-aware decisions
- Completion tracking

**Run**:
```erlang
%% First time - goes to branch_a
{ok, CaseId} = state_based_routing:run(#{}).

%% After branch_a completed - goes to branch_b
state_based_routing:continue(CaseId, branch_a).
```

**Expected Output**:
```
Checking state...
No previous tasks completed
Routing to: branch_a
Branch A completed
Checking state...
Branch A in history
Routing to: branch_b
Branch B completed
Case completed
```

---

## Running Examples

### Individual Example

```erlang
%% Compile
c(simple_sequence).

%% Run
{ok, CaseId} = simple_sequence:run().

%% Query
{ok, State} = gen_yawl_api:get_case_state(CaseId).
```

### All Examples

```erlang
%% Run all examples sequentially
lists:foreach(fun(Example) ->
    io:format("~n=== Running ~p ===~n", [Example]),
    {ok, CaseId} = Example:run(),
    {ok, State} = gen_yawl_api:get_case_state(CaseId),
    io:format("Status: ~p~n", [State#yawl_state.status])
end, [
    simple_sequence,
    parallel_approval,
    conditional_routing,
    multi_choice_approval,
    retry_cycle,
    cancellation,
    deferred_choice,
    discriminator,
    multiple_instances,
    state_based_routing
]).
```

### With Observability

```erlang
%% Subscribe to events before running
{ok, CaseId, Pid} = simple_sequence:run(),
{ok, SubId} = gen_yawl_api:subscribe_to_case(CaseId, self()),

%% Collect all events
collect_events(CaseId, 10000).
```

---

## Expected Outputs

### Completion Status

All examples should end with one of these statuses:

| Status | Description |
|--------|-------------|
| `completed` | Workflow finished successfully |
| `cancelled` | Workflow was cancelled (expected for cancellation examples) |
| `failed` | Workflow failed (check logs for errors) |

### Receipt Chain

Every completed case produces a chain of cryptographic receipts:

```erlang
{ok, State} = gen_yawl_api:get_case_state(CaseId),
Receipts = State#yawl_state.receipts,

%% Verify chain
{ok, true, []} = gen_yawl_receipts:verify_receipts(Receipts).
```

### Event Timeline

```erlang
%% Get case history
{ok, History} = gen_yawl_api:get_case_history(CaseId),

%% Print timeline
lists:foreach(fun(Event) ->
    Timestamp = maps:get(<<"timestamp">>, Event),
    Type = maps:get(<<"event_type">>, Event),
    io:format("~s: ~p~n", [format_ts(Timestamp), Type])
end, History).
```

---

## Troubleshooting

### Example Fails to Start

```erlang
%% Check if gen_yawl is running
application:which_applications().
%% Should include {gen_yawl,...}

%% Start if needed
application:ensure_all_started(gen_yawl).
```

### Case Stuck

```erlang
%% Check enabled work items
{ok, WorkItems} = gen_yawl_api:list_enabled_work_items(CaseId).

%% Manually complete if needed
[#work_item{id = WorkItemId}] = WorkItems,
gen_yawl_api:complete_work_item(CaseId, WorkItemId, #{}).
```

### View Full State

```erlang
{ok, State} = gen_yawl_api:get_case_state(CaseId),
io:format("Status: ~p~n", [State#yawl_state.status]),
io:format("Completed tasks: ~p~n", [sets:to_list(State#yawl_state.completed_tasks)]),
io:format("Active tokens: ~p~n", [State#yawl_state.active_tokens]).
```

---

For pattern details, see [PATTERNS.md](../PATTERNS.md).
For user guide, see [GUIDE.md](../GUIDE.md).
