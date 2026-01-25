# gen_statem Design Patterns and Best Practices

## Table of Contents
1. [When to Use gen_statem vs gen_server](#when-to-use-genstatem-vs-genserver)
2. [State Machine Design Principles](#state-machine-design-principles)
3. [Avoiding Circular Waits and Deadlocks](#avoiding-circular-waits-and-deadlocks)
4. [Timeout Strategies and Escalation](#timeout-strategies-and-escalation)
5. [Event Handling: cast vs call](#event-handling-cast-vs-call)
6. [Real Governor Examples](#real-governor-examples)
7. [Advanced Patterns](#advanced-patterns)
8. [Anti-Patterns to Avoid](#anti-patterns-to-avoid)

---

## When to Use gen_statem vs gen_server

### Use gen_statem When:
- **Explicit state machine**: System has distinct states with specific allowed transitions
- **Deterministic behavior**: Each state + event combination has predictable outcome
- **State-dependent handling**: Same event means different things in different states
- **Audit trail needed**: Must record state transitions (receipts)
- **Compliance required**: Financial/legal system needing immutable history
- **Complex sequences**: Multi-step workflows with rollback capability

### Use gen_server When:
- **Simple request/response**: Stateless or minimal state
- **Flexible state changes**: State transitions don't follow clear patterns
- **RPC-like behavior**: Call-and-return model with no complex sequencing
- **Counter/accumulator**: Simple state modifications (increment, append)

### Comparison Table

| Feature | gen_statem | gen_server |
|---------|-----------|-----------|
| **State transitions** | Explicit, validated | Implicit, in handle_call/cast |
| **Invalid transitions** | Prevented by code | Programmer's responsibility |
| **Timeout handling** | Built-in state timeouts | Manual timer management |
| **Code clarity** | Very clear (states explicit) | Less clear (scattered logic) |
| **Performance** | Minimal overhead | Slightly faster for trivial cases |
| **Learning curve** | Steeper (FSM concepts) | Easier (simple message passing) |
| **Audit trail** | Natural fit (receipts) | Requires wrapper logic |

### Decision Tree
```
Is behavior state-dependent?
├─ YES → Is there a clear FSM?
│        ├─ YES → USE gen_statem ✓
│        └─ NO  → Use gen_server (rethink design)
└─ NO  → USE gen_server
```

---

## State Machine Design Principles

### 1. Explicit State Encoding

**GOOD - Clear states:**
```erlang
-type order_state() :: pending | processing | completed | cancelled | refunded.

handle_event({call, From}, {cancel}, processing, Data) ->
    {next_state, cancelled, Data, [{reply, From, {ok, cancelled}}]}.
```

**BAD - Implicit state (using booleans):**
```erlang
% Don't do this!
Data = #{is_active => true, is_processing => false, is_cancelled => false}.
% What does {true, true, true} mean?
```

### 2. State Transition Table

Create a clear diagram of all valid transitions:

```
Entitlement FSM:

unentitled
    │
    ├─ grant_entitlement ──→ pending_review
    │
    └─ (no other transitions)

pending_review
    │
    ├─ suspend ──→ suspended
    ├─ approve ──→ entitled
    └─ (timeout) ──→ expired

entitled
    │
    ├─ revoke ──→ revoked
    ├─ suspend ──→ suspended
    └─ (timeout) ──→ expired

suspended
    │
    ├─ escalate ──→ escalated
    ├─ reinstate ──→ entitled
    └─ (timeout) ──→ escalated

escalated
    │
    └─ archive ──→ archived

revoked
    │
    └─ archive ──→ archived

archived
    │
    └─ (timeout/manual) ──→ expired

expired
    └─ (terminal state - no transitions)
```

### 3. Data Structure Design

Keep data immutable where possible:

```erlang
% Per-FSM state data record
-record(entitlement_data, {
    % Identity (immutable)
    tenant_id :: binary(),
    entitlement_id :: binary(),
    customer_id :: binary() | undefined,

    % Timestamps (immutable once set)
    created_at :: non_neg_integer(),
    expires_at :: non_neg_integer() | infinity,
    last_transition :: non_neg_integer(),

    % Audit trail (append-only)
    receipt_table :: atom(),

    % Mutable state (only what's necessary)
    metadata :: map()
}).
```

**Benefits:**
- Easier to reason about
- Simpler to parallelize
- Clearer in audit logs

### 4. Validation in State Transitions

Always validate before transitioning:

```erlang
handle_event({call, From}, {revoke_entitlement, Reason, Metadata}, entitled, Data) ->
    % Validate inputs before state change
    case validate_revocation(Data, Reason, Metadata) of
        {ok, ValidMetadata} ->
            emit_receipt(Data, entitled, revoked, Reason, ValidMetadata),
            {next_state, revoked, Data, [{reply, From, {ok, revoked}}]};
        {error, Error} ->
            {keep_state_and_data, [{reply, From, {error, Error}}]}
    end.

validate_revocation(Data, Reason, Metadata) ->
    case {is_binary(Reason), Reason =/= <<>>} of
        {true, true} ->
            % Reason validation passed
            {ok, Metadata#{validated_at => timestamp()}};
        _ ->
            {error, invalid_reason}
    end.
```

### 5. Guard Clauses for Extra Safety

Prevent invalid patterns from matching:

```erlang
% GOOD - Guard prevents invalid state combinations
handle_event({call, From}, {grant, CustomerId, Metadata}, State, Data)
    when State =:= unentitled orelse State =:= expired ->
    % Can grant from both unentitled and expired states
    {ok, pending_review} = grant_entitlement(Data, CustomerId, Metadata),
    ...;

% BAD - No guards, harder to read
handle_event({call, From}, {grant, CustomerId, Metadata}, State, Data) ->
    case State of
        unentitled -> ...;
        expired -> ...;
        _ -> {error, invalid_state}
    end.
```

---

## Avoiding Circular Waits and Deadlocks

### The Problem

In distributed systems with multiple FSMs, deadlocks can occur:

```
FSM A (entitled state)
    ├─ Waiting for FSM B to reply
    └─ Holding lock on resource X

FSM B (pending state)
    ├─ Waiting for FSM A to reply
    └─ Circular wait detected!
```

### Solution 1: One-Way Messages (cast)

Use `gen_statem:cast/2` instead of `gen_statem:call/2` for non-critical updates:

```erlang
% SAFE: A doesn't wait for B
handle_event({call, From}, {update_downstream}, State, Data) ->
    % Notify downstream FSM but don't wait for response
    gen_statem:cast(downstream_fsm_pid, {update, self(), Data}),
    {keep_state_and_data, [{reply, From, {ok, queued}}]}.

% Handle async update from upstream
handle_event(cast, {update, UpstreamPid, UpstreamData}, State, Data) ->
    % Process update from upstream without blocking
    NewData = merge_upstream_state(Data, UpstreamData),
    {keep_state, NewData}.
```

### Solution 2: Timeout-Based Retry

Don't wait indefinitely for blocking calls:

```erlang
% SAFE: Call with timeout, retry if needed
make_rpc_call(Node, Request) ->
    try
        gen_statem:call({fsm_name, Node}, Request, 5000)  % 5 second timeout
    catch
        exit:{timeout, _} ->
            {error, timeout}  % Don't retry inside FSM, let supervisor handle it
    end.
```

### Solution 3: Event-Driven Architecture

Break circular dependencies through intermediate events:

```
Instead of:
A → B (call) → A (call)  [DEADLOCK]

Use:
A → B (cast) → event_queue
Event queue → A (cast) [NO DEADLOCK]
```

**Implementation:**

```erlang
% In FSM A:
handle_event({call, From}, {request_from_b}, State, Data) ->
    % Don't call B directly, send async notification
    notify_fsm(b_pid, {notification, self(), Data}),
    {keep_state_and_data, [{reply, From, {ok, queued}}]}.

% In FSM B:
handle_event(cast, {notification, APid, AData}, State, Data) ->
    % Process A's notification asynchronously
    ProcessedData = process_notification(AData),
    % When done, notify A (but don't wait for reply)
    gen_statem:cast(APid, {result, self(), ProcessedData}),
    {keep_state, Data}.

% Back in FSM A, handle the result asynchronously:
handle_event(cast, {result, BPid, ProcessedData}, State, Data) ->
    NewData = merge_results(Data, ProcessedData),
    {keep_state, NewData}.
```

### Solution 4: Strict Ordering with Sequence Numbers

If you must have synchronous calls, use sequence numbers to detect circular waits:

```erlang
-record(request, {
    id :: non_neg_integer(),           % Unique request ID
    caller :: pid(),                   % Who initiated request
    sequence :: [atom()],              % Sequence of FSMs to visit
    depth :: non_neg_integer()         % Current depth in sequence
}).

% Detect circular waits:
handle_event({call, From}, {rpc, Request}, State, Data) ->
    case Request of
        #request{sequence = Seq, caller = Caller} ->
            % If caller is already in sequence, circular wait detected
            case lists:member(Caller, Seq) of
                true -> {keep_state_and_data, [{reply, From, {error, circular_dependency}}]};
                false ->
                    % Safe to proceed
                    forward_request(Request, From, Data)
            end
    end.
```

### Solution 5: Resource Ordering

If multiple FSMs access shared resources, always acquire in same order:

```erlang
% SAFE: Always acquire in lexicographic order
acquire_resources([Resource1, Resource2]) when Resource1 < Resource2 ->
    % Acquire Resource1 first, then Resource2
    lock_resource(Resource1),
    lock_resource(Resource2);

acquire_resources([Resource1, Resource2]) ->
    % Acquire Resource2 first, then Resource1
    lock_resource(Resource2),
    lock_resource(Resource1).
```

---

## Timeout Strategies and Escalation

### 1. Simple Fixed Timeout

Transition to new state after fixed duration:

```erlang
init({TenantId, EntitlementId}) ->
    Data = #entitlement_data{
        tenant_id = TenantId,
        entitlement_id = EntitlementId,
        created_at = timestamp(),
        expires_at = timestamp() + 86400000  % 24 hours from now
    },

    % Automatically expire after 24 hours
    Timeout = 86400000,
    {ok, unentitled, Data, [{state_timeout, Timeout, expire}]}.

% Handle timeout event
handle_event(state_timeout, expire, archived, Data) ->
    emit_receipt(Data, archived, expired, <<"auto_expiration">>, #{}),
    {next_state, expired, Data}.
```

### 2. Escalation Chain (Progressive Timeouts)

Move through states with increasing severity:

```erlang
% Billing - escalate unpaid invoices
handle_event(state_timeout, escalate, pending_payment, Data) ->
    % Send first reminder after 3 days
    send_reminder(Data, first_reminder),
    {next_state, first_reminder_sent, Data,
     [{state_timeout, 5*24*3600000, escalate}]}.

handle_event(state_timeout, escalate, first_reminder_sent, Data) ->
    % Send second reminder after 5 more days (total 8 days)
    send_reminder(Data, second_reminder),
    {next_state, second_reminder_sent, Data,
     [{state_timeout, 7*24*3600000, escalate}]}.

handle_event(state_timeout, escalate, second_reminder_sent, Data) ->
    % Suspend service after 7 more days (total 15 days)
    suspend_service(Data),
    {next_state, suspended, Data,
     [{state_timeout, 10*24*3600000, escalate}]}.

handle_event(state_timeout, escalate, suspended, Data) ->
    % Terminate account after 10 more days (total 25 days)
    terminate_account(Data),
    {next_state, terminated, Data}.
```

### 3. Event-Driven Timeout Reset

Reset timeout when activity occurs:

```erlang
handle_event({call, From}, {process_payment, Amount}, pending_payment, Data) ->
    case process_payment_transaction(Amount) of
        {ok, TxId} ->
            % Payment successful - transition and clear timeout
            NewData = Data#{last_payment = timestamp()},
            {next_state, paid, NewData, [{reply, From, {ok, TxId}}]};
        {error, Reason} ->
            % Payment failed - reset timeout (give customer more time)
            {keep_state, Data,
             [{state_timeout, 3*24*3600000, escalate},
              {reply, From, {error, Reason}}]}
    end.
```

### 4. Conditional Timeout Based on Data

Timeout depends on entitlement properties:

```erlang
init({TenantId, EntitlementId, Opts}) ->
    Data = parse_opts(Opts),

    % Timeout based on subscription tier
    Timeout = case Data#{tier := undefined} of
        #{tier := <<"premium">>} -> 30*24*3600000;      % 30 days
        #{tier := <<"standard">>} -> 14*24*3600000;     % 14 days
        #{tier := <<"free">>} -> 7*24*3600000           % 7 days
    end,

    {ok, unentitled, Data, [{state_timeout, Timeout, expire}]}.
```

### 5. Retry With Exponential Backoff

Retry failed operations with increasing delays:

```erlang
handle_event({call, From}, {upload_receipt, Receipt}, syncing, Data) ->
    case send_to_cloud(Receipt) of
        {ok, ReceiptId} ->
            {next_state, synced, Data, [{reply, From, {ok, ReceiptId}}]};
        {error, network_error} ->
            % Retry after 1 second
            NewData = Data#{retry_count := 1},
            {keep_state, NewData,
             [{state_timeout, 1000, {retry, Receipt}},
              {reply, From, {error, retrying}}]}
    end.

% Exponential backoff:
handle_event(state_timeout, {retry, Receipt}, syncing, Data) ->
    case send_to_cloud(Receipt) of
        {ok, ReceiptId} ->
            {next_state, synced, Data};
        {error, _} ->
            RetryCount = maps:get(retry_count, Data, 0),
            case RetryCount of
                N when N >= 5 ->
                    % Give up after 5 retries
                    emit_receipt(Data, syncing, sync_failed, <<"max_retries_exceeded">>, #{}),
                    {next_state, sync_failed, Data};
                N ->
                    % Exponential backoff: 2^n seconds
                    NextDelay = trunc(math:pow(2, N)) * 1000,
                    NewData = Data#{retry_count := N + 1},
                    {keep_state, NewData, [{state_timeout, NextDelay, {retry, Receipt}}]}
            end
    end.
```

### 6. Absolute Deadline (Not Relative)

Use absolute time for compliance requirements:

```erlang
-record(entitlement_data, {
    created_at :: non_neg_integer(),
    deadline :: non_neg_integer(),  % Absolute UNIX timestamp
    ...
}).

handle_event({call, From}, {grant}, unentitled, Data) ->
    % Absolute deadline: must complete within SLA
    Now = erlang:system_time(millisecond),
    Deadline = Now + 86400000,  % 24 hours

    NewData = Data#{deadline := Deadline},
    {next_state, pending_review, NewData,
     [{state_timeout, Deadline - Now, {deadline_exceeded, pending_review}},
      {reply, From, {ok, pending_review}}]}.

handle_event(state_timeout, {deadline_exceeded, State}, State, Data) ->
    emit_receipt(Data, State, escalated, <<"sla_deadline_missed">>, #{}),
    {next_state, escalated, Data}.
```

---

## Event Handling: cast vs call

### When to Use `gen_statem:call/2`

**Synchronous request-response**, caller needs confirmation:

```erlang
% Caller blocks until response received
{ok, NewState} = gen_statem:call(Pid, {grant_entitlement, CustomerId, Meta}),
io:format("Entitlement granted: ~p~n", [NewState]).

% Handle in FSM:
handle_event({call, From}, {grant_entitlement, CustomerId, Meta}, unentitled, Data) ->
    % Response sent back to caller
    {next_state, pending_review, Data, [{reply, From, {ok, pending_review}}]}.
```

**When:**
- Caller must verify the operation succeeded
- State change is critical to subsequent operations
- Exception handling needed (timeouts, errors)
- Financial transactions, access control decisions

### When to Use `gen_statem:cast/2`

**Asynchronous fire-and-forget**, caller doesn't wait:

```erlang
% Caller doesn't wait for response
ok = gen_statem:cast(Pid, {update_downstream, CustomerId, Data}),
% Continue immediately

% Handle in FSM:
handle_event(cast, {update_downstream, CustomerId, Data}, State, FsmData) ->
    % No response sent back
    {keep_state, FsmData}.
```

**When:**
- Result doesn't affect caller's next operation
- Notification or informational message
- Broadcasting updates to multiple FSMs
- Decoupling components to prevent deadlocks

### Pattern: Request-Response with cast

Achieve request-response semantics without blocking:

```erlang
% Client sends request and provides callback
gen_statem:cast(ServerPid, {async_request, {State, Metadata}, self()}),
% Continue immediately

% Server handles request
handle_event(cast, {async_request, {State, Metadata}, ClientPid}, FsmState, Data) ->
    % Process request
    Result = process_request(State, Metadata),
    % Send result back via cast
    gen_statem:cast(ClientPid, {async_response, Result}),
    {keep_state, Data}.

% Client receives response later
handle_event(cast, {async_response, Result}, State, Data) ->
    % Handle result asynchronously
    NewData = merge_response(Data, Result),
    {keep_state, NewData}.
```

### Comparison

| Feature | call | cast |
|---------|------|------|
| **Blocking** | Yes | No |
| **Response** | Required | Not possible |
| **Error handling** | Automatic | Manual |
| **Timeout** | Built-in | Manual |
| **Use case** | RPC-style | Notifications |
| **Deadlock risk** | Higher | Lower |

---

## Real Governor Examples

### Example 1: Billing Governor (Payment States)

```erlang
-module(billing_governor).
-behaviour(gen_statem).

-export([start_link/2, initialize_billing/3, process_payment/2,
         update_billing_cycle/2, get_state/1, list_receipts/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

-record(billing_state, {
    tenant_id :: binary(),
    billing_id :: binary(),
    customer_id :: binary() | undefined,
    amount_usd :: float(),
    billing_cycle_days :: pos_integer(),
    next_due_date :: non_neg_integer() | undefined,
    failed_attempts :: non_neg_integer(),
    receipt_table :: atom(),
    created_at :: non_neg_integer(),
    last_payment_date :: non_neg_integer() | undefined
}).

-type billing_state() :: draft | active | pending_payment |
                        payment_failed | disputed | paid | suspended | terminated.

% FSM States:
% draft → active → pending_payment → (payment_failed → pending_payment) → paid → (active) → pending_payment → ...
%      ↓                ↓
%      └── suspended ──→ disputed
%      └── terminated

start_link(TenantId, BillingId) ->
    gen_statem:start_link({local, {billing, TenantId, BillingId}}, ?MODULE,
        {TenantId, BillingId}, []).

% API
initialize_billing(Pid, CustomerId, Options) ->
    gen_statem:call(Pid, {initialize, CustomerId, Options}).

process_payment(Pid, PaymentDetails) ->
    gen_statem:call(Pid, {process_payment, PaymentDetails}, 10000).

update_billing_cycle(Pid, NewCycleDays) ->
    gen_statem:call(Pid, {update_cycle, NewCycleDays}).

get_state(Pid) ->
    gen_statem:call(Pid, get_state).

list_receipts(Pid) ->
    gen_statem:call(Pid, list_receipts).

% Callbacks
init({TenantId, BillingId}) ->
    ReceiptTable = create_receipt_table(TenantId, BillingId),
    Data = #billing_state{
        tenant_id = TenantId,
        billing_id = BillingId,
        created_at = timestamp(),
        failed_attempts = 0,
        receipt_table = ReceiptTable
    },
    emit_receipt(Data, draft, draft, <<"initialized">>, #{}),
    {ok, draft, Data}.

callback_mode() -> handle_event_function.

% Draft state - initialize billing
handle_event({call, From}, {initialize, CustomerId, Opts}, draft, Data) ->
    Amount = maps:get(amount_usd, Opts, 99.99),
    CycleDays = maps:get(cycle_days, Opts, 30),

    NewData = Data#billing_state{
        customer_id = CustomerId,
        amount_usd = Amount,
        billing_cycle_days = CycleDays,
        next_due_date = timestamp() + (CycleDays * 86400000)
    },

    emit_receipt(NewData, draft, active, <<"billing_initialized">>, Opts),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

% Active state - payment is due
handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

% Pending payment - process payment
handle_event({call, From}, {process_payment, PaymentDetails}, pending_payment, Data) ->
    case charge_customer(Data#billing_state.customer_id, Data#billing_state.amount_usd) of
        {ok, TxId} ->
            NewData = Data#billing_state{
                last_payment_date = timestamp(),
                failed_attempts = 0
            },
            emit_receipt(NewData, pending_payment, paid, <<"payment_success">>,
                #{tx_id => TxId, payment_details => PaymentDetails}),

            % Schedule next billing cycle
            NextPaymentTime = timestamp() + (Data#billing_state.billing_cycle_days * 86400000),
            {next_state, paid, NewData,
             [{reply, From, {ok, paid}},
              {state_timeout, Data#billing_state.billing_cycle_days * 86400000, next_cycle}]};

        {error, Reason} ->
            FailedCount = Data#billing_state.failed_attempts + 1,
            NewData = Data#billing_state{failed_attempts = FailedCount},

            case FailedCount >= 3 of
                true ->
                    emit_receipt(NewData, pending_payment, suspended, <<"payment_failed_max_attempts">>,
                        #{attempts => FailedCount, reason => Reason}),
                    {next_state, suspended, NewData, [{reply, From, {error, payment_suspended}}]};
                false ->
                    emit_receipt(Data, pending_payment, payment_failed, <<"payment_failed">>,
                        #{attempts => FailedCount, reason => Reason}),
                    % Retry after 1 hour
                    {next_state, payment_failed, NewData,
                     [{state_timeout, 3600000, retry_payment},
                      {reply, From, {error, {payment_failed, Reason}}}]}
            end
    end;

% Payment failed - automatic retry
handle_event(state_timeout, retry_payment, payment_failed, Data) ->
    % Transition back to pending_payment for retry
    {next_state, pending_payment, Data};

% Paid state - move to next cycle
handle_event(state_timeout, next_cycle, paid, Data) ->
    {next_state, active, Data};

% Handle suspended state - require manual intervention
handle_event({call, From}, {process_payment, _}, suspended, Data) ->
    {keep_state_and_data, [{reply, From, {error, account_suspended}}]};

% Generic catch-all
handle_event({call, From}, list_receipts, _State, Data) ->
    Receipts = ets:match_object(Data#billing_state.receipt_table, {receipt, '_'}),
    FormattedReceipts = [R || {receipt, R} <- Receipts],
    {keep_state_and_data, [{reply, From, {ok, FormattedReceipts}}]};

handle_event({call, From}, _, _, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_event}}]}.

terminate(_Reason, _State, Data) ->
    ets:delete(Data#billing_state.receipt_table).

% Helpers
charge_customer(_CustomerId, _Amount) ->
    % Simulate payment processing (50% success rate)
    case rand:uniform() > 0.5 of
        true -> {ok, <<"tx_", (base64:encode(crypto:strong_rand_bytes(8)))/binary>>};
        false -> {error, declined}
    end.

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    ReceiptTable = Data#billing_state.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        billing_id => Data#billing_state.billing_id,
        state_from => FromState,
        state_to => ToState,
        reason => Reason,
        metadata => Metadata
    },
    ets:insert(ReceiptTable, {receipt, Receipt}).

create_receipt_table(TenantId, BillingId) ->
    TableName = binary_to_atom(
        <<"billing_receipts_", TenantId/binary, "_", BillingId/binary>>, utf8),
    ets:new(TableName, [public, named_table, {write_concurrency, true}]),
    TableName.

generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

timestamp() ->
    erlang:system_time(millisecond).
```

### Example 2: Quota SLA Governor

```erlang
-module(quota_sla_governor).
-behaviour(gen_statem).

-export([start_link/2, set_quota/3, record_usage/4, reset_quota/2,
         get_state/1, get_usage/2]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3]).

-type quota_state() :: initializing | active | quota_exceeded |
                       reset | paused | archived.

-record(quota_data, {
    tenant_id :: binary(),
    quota_id :: binary(),
    metric_name :: binary() | undefined,
    quota_limit :: pos_integer() | undefined,
    current_usage :: non_neg_integer(),
    window_minutes :: pos_integer() | undefined,
    reset_date :: non_neg_integer() | undefined,
    burst_multiplier :: float(),
    receipt_table :: atom(),
    created_at :: non_neg_integer(),
    last_reset :: non_neg_integer()
}).

start_link(TenantId, QuotaId) ->
    gen_statem:start_link({local, {quota, TenantId, QuotaId}}, ?MODULE,
        {TenantId, QuotaId}, []).

% API
set_quota(Pid, MetricName, QuotaConfig) ->
    gen_statem:call(Pid, {set_quota, MetricName, QuotaConfig}).

record_usage(Pid, MetricName, Amount, Context) ->
    gen_statem:call(Pid, {record_usage, MetricName, Amount, Context}, 5000).

reset_quota(Pid, Reason) ->
    gen_statem:call(Pid, {reset_quota, Reason}).

get_state(Pid) ->
    gen_statem:call(Pid, get_state).

get_usage(Pid, MetricName) ->
    gen_statem:call(Pid, {get_usage, MetricName}).

% Callbacks
init({TenantId, QuotaId}) ->
    ReceiptTable = create_receipt_table(TenantId, QuotaId),
    Data = #quota_data{
        tenant_id = TenantId,
        quota_id = QuotaId,
        current_usage = 0,
        burst_multiplier = 1.0,
        receipt_table = ReceiptTable,
        created_at = timestamp(),
        last_reset = timestamp()
    },
    emit_receipt(Data, initializing, initializing, <<"quota_initialized">>, #{}),
    {ok, initializing, Data}.

callback_mode() -> handle_event_function.

% Initializing → Active
handle_event({call, From}, {set_quota, MetricName, Config}, initializing, Data) ->
    Limit = maps:get(limit, Config, 10000),
    WindowMinutes = maps:get(window_minutes, Config, 60),
    BurstMultiplier = maps:get(burst_multiplier, Config, 1.0),

    NewData = Data#quota_data{
        metric_name = MetricName,
        quota_limit = Limit,
        window_minutes = WindowMinutes,
        burst_multiplier = BurstMultiplier,
        reset_date = timestamp() + (WindowMinutes * 60000)
    },

    emit_receipt(NewData, initializing, active, <<"quota_configured">>, Config),
    {next_state, active, NewData,
     [{reply, From, {ok, active}},
      {state_timeout, WindowMinutes * 60000, reset_window}]};

% Active → Quota Exceeded (when usage exceeds limit)
handle_event({call, From}, {record_usage, MetricName, Amount, Context}, active, Data) ->
    case validate_metric(MetricName, Data#quota_data.metric_name) of
        {ok, _} ->
            NewUsage = Data#quota_data.current_usage + Amount,
            Limit = Data#quota_data.quota_limit,
            BurstLimit = trunc(Limit * Data#quota_data.burst_multiplier),

            case NewUsage > Limit of
                true when NewUsage =< BurstLimit ->
                    % Over soft limit but within burst
                    NewData = Data#quota_data{current_usage = NewUsage},
                    emit_receipt(NewData, active, active, <<"usage_recorded_in_burst">>,
                        #{usage => NewUsage, limit => Limit, context => Context}),
                    {keep_state, NewData, [{reply, From, {ok, in_burst}}]};

                true ->
                    % Exceeded burst limit
                    NewData = Data#quota_data{current_usage = NewUsage},
                    emit_receipt(NewData, active, quota_exceeded, <<"quota_exceeded">>,
                        #{usage => NewUsage, limit => Limit, context => Context}),
                    {next_state, quota_exceeded, NewData, [{reply, From, {error, quota_exceeded}}]};

                false ->
                    % Usage OK
                    NewData = Data#quota_data{current_usage = NewUsage},
                    Percentage = (NewUsage / Limit) * 100,
                    emit_receipt(NewData, active, active, <<"usage_recorded">>,
                        #{usage => NewUsage, limit => Limit, percentage => Percentage}),
                    {keep_state, NewData, [{reply, From, {ok, recorded}}]}
            end;
        {error, Reason} ->
            {keep_state_and_data, [{reply, From, {error, Reason}}]}
    end;

% Quota Exceeded state - block usage
handle_event({call, From}, {record_usage, _MetricName, _Amount, _Context}, quota_exceeded, _Data) ->
    {keep_state_and_data, [{reply, From, {error, quota_exceeded}}]};

% Reset quota window
handle_event(state_timeout, reset_window, active, Data) ->
    NewData = Data#quota_data{
        current_usage = 0,
        last_reset = timestamp(),
        reset_date = timestamp() + (Data#quota_data.window_minutes * 60000)
    },
    emit_receipt(NewData, active, active, <<"window_reset">>, #{}),
    {keep_state, NewData, [{state_timeout, Data#quota_data.window_minutes * 60000, reset_window}]};

% Reset when in quota_exceeded
handle_event(state_timeout, reset_window, quota_exceeded, Data) ->
    NewData = Data#quota_data{current_usage = 0},
    emit_receipt(NewData, quota_exceeded, active, <<"window_reset_from_exceeded">>, #{}),
    {next_state, active, NewData, [{state_timeout, Data#quota_data.window_minutes * 60000, reset_window}]};

% Manual reset
handle_event({call, From}, {reset_quota, Reason}, State, Data) ->
    NewData = Data#quota_data{current_usage = 0},
    emit_receipt(NewData, State, active, <<"manual_reset">>, #{reason => Reason}),
    {next_state, active, NewData, [{reply, From, {ok, active}}]};

% Query
handle_event({call, From}, get_state, State, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, State}}]};

handle_event({call, From}, {get_usage, MetricName}, _State, Data) ->
    case validate_metric(MetricName, Data#quota_data.metric_name) of
        {ok, _} ->
            Usage = #{
                current => Data#quota_data.current_usage,
                limit => Data#quota_data.quota_limit,
                percent => (Data#quota_data.current_usage / Data#quota_data.quota_limit) * 100
            },
            {keep_state_and_data, [{reply, From, {ok, Usage}}]};
        {error, Reason} ->
            {keep_state_and_data, [{reply, From, {error, Reason}}]}
    end;

handle_event({call, From}, _Event, _State, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_event}}]}.

terminate(_Reason, _State, Data) ->
    ets:delete(Data#quota_data.receipt_table).

% Helpers
validate_metric(MetricName, ConfiguredMetric) ->
    case MetricName =:= ConfiguredMetric of
        true -> {ok, MetricName};
        false -> {error, {metric_mismatch, ConfiguredMetric, MetricName}}
    end.

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    ReceiptTable = Data#quota_data.receipt_table,
    Receipt = #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        quota_id => Data#quota_data.quota_id,
        state_from => FromState,
        state_to => ToState,
        reason => Reason,
        metadata => Metadata
    },
    ets:insert(ReceiptTable, {receipt, Receipt}).

create_receipt_table(TenantId, QuotaId) ->
    TableName = binary_to_atom(
        <<"quota_receipts_", TenantId/binary, "_", QuotaId/binary>>, utf8),
    ets:new(TableName, [public, named_table, {write_concurrency, true}]),
    TableName.

generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

timestamp() ->
    erlang:system_time(millisecond).
```

---

## Advanced Patterns

### Pattern 1: State Machine Composition

Combine multiple FSMs for complex workflows:

```erlang
% Customer account = entitlement + billing + quota
customer_setup_workflow(TenantId, CustomerId) ->
    % 1. Create entitlement FSM
    {ok, EntitlementPid} = entitlement_governor:start_link(
        TenantId, <<CustomerId/binary, "_ent">>),

    % 2. Create billing FSM
    {ok, BillingPid} = billing_governor:start_link(
        TenantId, <<CustomerId/binary, "_bill">>),

    % 3. Create quota FSM
    {ok, QuotaPid} = quota_sla_governor:start_link(
        TenantId, <<CustomerId/binary, "_quota">>),

    % 4. Coordinate state transitions
    entitlement_governor:grant_entitlement(EntitlementPid, CustomerId, #{}),
    billing_governor:initialize_billing(BillingPid, CustomerId,
        #{amount_usd => 99.99, cycle_days => 30}),
    quota_sla_governor:set_quota(QuotaPid, <<"API calls">>,
        #{limit => 10000, window_minutes => 60}),

    {EntitlementPid, BillingPid, QuotaPid}.
```

### Pattern 2: Self-Healing Timeout Recovery

FSM recovers from failures automatically:

```erlang
handle_event({call, From}, {sync_to_cloud, Data}, syncing, FsmData) ->
    case sync_operation(Data) of
        {ok, SyncId} ->
            {next_state, synced, FsmData, [{reply, From, {ok, synced}}]};
        {error, network_error} ->
            % Auto-retry after 5 seconds
            {keep_state, FsmData,
             [{state_timeout, 5000, {retry_sync, Data}},
              {reply, From, {error, retrying}}]}
    end;

handle_event(state_timeout, {retry_sync, Data}, syncing, FsmData) ->
    % Automatically retry
    case sync_operation(Data) of
        {ok, SyncId} -> {next_state, synced, FsmData};
        {error, _} -> {keep_state, FsmData,
             [{state_timeout, 5000, {retry_sync, Data}}]}
    end.
```

---

## Anti-Patterns to Avoid

### 1. ❌ Hidden State in Data Record

```erlang
% BAD: State information scattered in data
Data = #{
    is_active => true,
    is_locked => false,
    is_escalated => false,
    needs_review => true
}.
% Unclear what combination is valid!

% GOOD: State is explicit
State = active,  % Implies is_locked=false, is_escalated=false
```

### 2. ❌ Mixing Sync and Async Without Discipline

```erlang
% BAD: Unpredictable mix
handle_event({call, From}, {op1}, State, Data) ->
    % Some calls block, some don't
    case do_something() of
        ok -> gen_statem:call(OtherFsm, {op2}),  % BLOCKS!
        error -> gen_statem:cast(OtherFsm, {op2})  % ASYNC
    end.

% GOOD: Consistent patterns
% All external FSM calls are async (cast)
% Only local operations can be sync (call)
```

### 3. ❌ Unbounded Timeouts

```erlang
% BAD: Timeout could be infinite
{state_timeout, infinity, some_event}  % Process hangs forever!

% GOOD: Always have max timeout
MaxTimeout = 86400000,  % 24 hours max
{state_timeout, min(CalculatedTimeout, MaxTimeout), some_event}
```

### 4. ❌ Losing Receipts on Crash

```erlang
% BAD: No backup of receipts
emit_receipt(...) ->
    ets:insert(Table, Receipt),  % Lost if process crashes before persistence!

% GOOD: Persist receipts immediately
emit_receipt(...) ->
    ets:insert(Table, Receipt),
    persist_to_disk(Receipt),  % Or send to queue
    publish_to_pubsub(Receipt).
```

### 5. ❌ Infinite State Machine

```erlang
% BAD: FSM that never ends
State = processing,
handle_event(..., processing, Data) ->
    % Always stays in processing, no exit path!
    {keep_state, Data}.

% GOOD: Explicit terminal states
% Terminal states: archived, expired, terminated
% All paths eventually lead to terminal state
```

---

## Performance Considerations

### State Lookup Time

```erlang
% Fast: Direct state matching
handle_event({call, _From}, _Event, unentitled, Data) ->
    % O(1) - pattern matched directly

% Slower: State lookup in data
Data = #{state => unentitled, ...},
handle_event({call, _From}, _Event, _State, Data) ->
    case maps:get(state, Data) of
        unentitled -> ...  % O(1) but less clear
    end.
```

### Receipt Table Tuning

```erlang
% For high-throughput receipts, use optimized ETS
ets:new(ReceiptTable, [
    public,                  % Accessible by other processes
    named_table,            % Named for easy reference
    {write_concurrency, true},  % Multiple writers
    {read_concurrency, true}    % Multiple readers (OTP 24+)
]).
```

### Memory Optimization

```erlang
% Store only essential data
-record(data, {
    % DO store:
    tenant_id, entitlement_id, state,
    created_at, receipt_table_ref,

    % DON'T store:
    % - Full customer profiles (reference by ID instead)
    % - Full request bodies (store metadata only)
    % - Derivable values
}).
```

---

**Last Updated**: January 2026
**OTP Version**: OTP 25+
**Status**: Production-Ready
