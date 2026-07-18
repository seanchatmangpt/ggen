# Erlang Quick Start Guide for GCP Autonomics

## Table of Contents
1. [Setup Erlang Environment](#setup-erlang-environment)
2. [Project Structure](#project-structure)
3. [Compiling Governors](#compiling-governors)
4. [Running Individual FSMs](#running-individual-fsms)
5. [Connecting to Cluster](#connecting-to-cluster)
6. [Real Examples](#real-examples)
7. [Troubleshooting](#troubleshooting)

---

## Setup Erlang Environment

### Prerequisites
- **Erlang OTP 25+** (OTP 26 recommended for best performance)
- **Rebar3** 3.20+
- **Git**
- **Make**

### Installation on Linux/macOS

#### Using Homebrew (macOS)
```bash
brew install erlang rebar3
erl -version
```

#### Using apt (Ubuntu/Debian)
```bash
sudo apt-get update
sudo apt-get install erlang erlang-rebar3
erl -version
```

#### Using asdf (Recommended - allows multiple versions)
```bash
# Install asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf
cd ~/.asdf && git checkout latest
source ~/.asdf/asdf.sh

# Add Erlang plugin
asdf plugin add erlang
asdf plugin add rebar3

# Install OTP 26 (latest stable)
asdf install erlang 26.2.1
asdf global erlang 26.2.1

# Verify installation
erl -version
rebar3 version
```

### Verify Installation
```bash
$ erl
Erlang/OTP 26 [erts-14.1.1] [source] [64-bit] [smp:12:12] [ds:12:12:10]
```

---

## Project Structure

### Directory Layout
```
gcp-erlang-autonomics/
├── erlang_src/                    # All Erlang source files
│   ├── autonomics_sup.erl         # Root supervisor (entry point)
│   ├── governance_sup.erl         # Governance supervisor tree
│   ├── entitlement_sup.erl        # Entitlement governor supervisor
│   ├── entitlement_governor.erl   # Entitlement state machine (gen_statem)
│   ├── billing_sup.erl            # Billing supervisor
│   ├── quota_sla_sup.erl          # Quota/SLA supervisor
│   ├── subscription_sup.erl       # Subscription supervisor
│   ├── customer_account_sup.erl   # Customer account supervisor
│   ├── product_catalog_sup.erl    # Product catalog supervisor
│   ├── src/                       # Support modules
│   ├── test/                      # Test suites
│   └── priv/                      # Configuration files
├── rebar.config                   # Rebar3 build configuration
└── docs/                          # Documentation (this file)
```

### Key Files
- **autonomics_sup.erl**: Root of supervision tree - starts all subsystems
- **entitlement_governor.erl**: Example gen_statem with 8-state FSM
- **rebar.config**: Build configuration, dependencies, compilation options

---

## Compiling Governors

### Quick Compile (Development)
```bash
# Clean and compile all modules
cd /home/user/ggen/examples/gcp-erlang-autonomics/erlang_src
rebar3 clean
rebar3 compile

# Expected output:
# ===> Compiling gcp_erlang_autonomics
# ===> Verifying dependencies...
# ===> Analyzing applications...
# ===> Compiling [supervisor modules, governors, utilities]
```

### Compile with Warnings-as-Errors (Production)
```bash
rebar3 as prod compile
```

### Incremental Compile
```bash
# Rebar3 automatically detects changed files
rebar3 compile
```

### Check Compilation Status
```bash
# List compiled modules and dependencies
rebar3 status

# Inspect specific module (see exported functions)
erl -pa /home/user/ggen/examples/gcp-erlang-autonomics/erlang_src/_build/default/lib/*/ebin
(erlang@localhost)1> entitlement_governor:module_info().
```

### BEAM File Output Location
```bash
# Compiled .beam files appear in:
_build/default/lib/gcp_erlang_autonomics/ebin/

# View compiled modules:
ls _build/default/lib/*/ebin/*.beam
```

---

## Running Individual FSMs

### Start Interactive Erlang Shell
```bash
# Navigate to project root
cd /home/user/ggen/examples/gcp-erlang-autonomics

# Start shell with code paths configured
erl -pa _build/default/lib/*/ebin

# Or with application boot:
erl -pa _build/default/lib/*/ebin -s autonomics_sup
```

### Manual FSM Test (Entitlement Governor)

#### 1. Start the Entitlement Governor
```erlang
% In erl shell:
(erlang@localhost)1> c(entitlement_governor).
{ok,entitlement_governor}

% Start a single governor instance
(erlang@localhost)2> {ok, Pid} = entitlement_governor:start_link(
    <<"tenant123">>,
    <<"entitlement456">>
).
{ok,<0.89.0>}
```

#### 2. Check Initial State
```erlang
% Get current state (unentitled)
(erlang@localhost)3> entitlement_governor:get_state(Pid, <<"entitlement456">>).
{ok,unentitled}
```

#### 3. Grant Entitlement
```erlang
% Grant entitlement to customer
(erlang@localhost)4> entitlement_governor:grant_entitlement(
    Pid,
    <<"customer789">>,
    #{<<"reason">> => <<"user_signup">>}
).
{ok,pending_review}
```

#### 4. Verify State Transition
```erlang
(erlang@localhost)5> entitlement_governor:get_state(Pid, <<"entitlement456">>).
{ok,pending_review}
```

#### 5. Revoke Entitlement (will fail - invalid transition)
```erlang
% Can only revoke from 'entitled' state, not 'pending_review'
(erlang@localhost)6> entitlement_governor:revoke_entitlement(
    Pid,
    <<"admin_decision">>,
    #{}
).
{error,{invalid_transition,pending_review}}
```

#### 6. List Receipt History
```erlang
% View all state transitions
(erlang@localhost)7> entitlement_governor:list_receipts(
    Pid,
    <<"entitlement456">>
).
{ok,[
  #{
    id => <<_/binary>>,
    timestamp => 1704067200000,
    tenant_id => <<"tenant123">>,
    entitlement_id => <<"entitlement456">>,
    state_from => unentitled,
    state_to => unentitled,
    reason => <<"initialization">>,
    metadata => #{}
  },
  #{
    id => <<_/binary>>,
    timestamp => 1704067200500,
    tenant_id => <<"tenant123">>,
    entitlement_id => <<"entitlement456">>,
    state_from => unentitled,
    state_to => pending_review,
    reason => <<"grant_requested">>,
    metadata => #{<<"reason">> => <<"user_signup">>}
  }
]}
```

#### 7. Kill Process
```erlang
(erlang@localhost)8> exit(Pid, kill).
true
```

---

## Connecting to Cluster

### Single Node (Development)

#### Start Root Supervisor
```erlang
% In erl shell:
(erlang@localhost)1> application:start(autonomics_app).
ok

% Or manually start the root supervisor:
(erlang@localhost)1> autonomics_sup:start_link().
{ok, <0.45.0>}

% Verify supervision tree is running:
(erlang@localhost)2> supervisor:which_children(autonomics_sup).
[
  {governance_sup, <0.46.0>, supervisor, [governance_sup]},
  {receipt_ledger_sup, <0.47.0>, supervisor, [receipt_ledger_sup]},
  {cluster_sup, <0.48.0>, supervisor, [cluster_sup]},
  {observability_sup, <0.49.0>, supervisor, [observability_sup]}
]
```

### Multi-Node Cluster (Production)

#### Terminal 1: Start Node 1 (Primary)
```bash
# Start Erlang node with name and cookie for clustering
erl -name autonomics1@127.0.0.1 -setcookie autonomics_cluster \
    -pa _build/default/lib/*/ebin

# In erl shell:
(autonomics1@127.0.0.1)1> application:start(autonomics_app).
ok
```

#### Terminal 2: Start Node 2 (Secondary)
```bash
erl -name autonomics2@127.0.0.1 -setcookie autonomics_cluster \
    -pa _build/default/lib/*/ebin

(autonomics2@127.0.0.1)1> application:start(autonomics_app).
ok
```

#### Terminal 1: Connect Nodes
```erlang
% Back in node 1 shell
(autonomics1@127.0.0.1)2> net_kernel:connect_node('autonomics2@127.0.0.1').
true

% Verify cluster membership
(autonomics1@127.0.0.1)3> nodes().
['autonomics2@127.0.0.1']

% Verify all nodes see each other
(autonomics1@127.0.0.1)4> erlang:nodes().
['autonomics2@127.0.0.1']
```

#### Monitor Cluster Health
```erlang
% In either node, monitor node status
(autonomics1@127.0.0.1)5> net_kernel:monitor_nodes(true).
nodeup

(autonomics1@127.0.0.1)6> % If autonomics2 crashes, you'll see:
% nodedown

% List connected nodes
(autonomics1@127.0.0.1)7> erlang:nodes(connected).
['autonomics2@127.0.0.1']
```

---

## Real Examples

### Example 1: Complete Subscription Lifecycle

```erlang
%% Subscribe customer to Premium plan, track state transitions
subscription_flow_example() ->
    % 1. Start governor
    {ok, SubPid} = subscription_governor:start_link(
        <<"acme_corp">>,          % tenant_id
        <<"sub_12345">>           % subscription_id
    ),

    % 2. Initiate subscription (unentitled -> pending_approval)
    {ok, pending_approval} = subscription_governor:request_subscription(
        SubPid,
        <<"customer_789">>,       % customer_id
        #{
            <<"plan">> => <<"premium">>,
            <<"term_months">> => 12,
            <<"auto_renew">> => true
        }
    ),

    % 3. Approve subscription (pending_approval -> active)
    {ok, active} = subscription_governor:approve_subscription(
        SubPid,
        <<"admin_user">>,         % approved_by
        #{<<"reason">> => <<"payment_verified">>}
    ),

    % 4. Get current state
    {ok, active} = subscription_governor:get_state(SubPid),

    % 5. View audit trail
    {ok, Receipts} = subscription_governor:list_receipts(SubPid),
    io:format("Subscription history: ~p~n", [Receipts]),

    % 6. Handle renewal (active -> renewing -> active)
    {ok, renewing} = subscription_governor:request_renewal(SubPid, #{}),
    {ok, active} = subscription_governor:confirm_renewal(SubPid, #{}),

    % 7. Cancel subscription (active -> cancellation_requested -> cancelled)
    {ok, cancellation_requested} = subscription_governor:request_cancellation(
        SubPid,
        <<"customer_initiated">>,
        #{<<"reason">> => <<"too_expensive">>}
    ),
    {ok, cancelled} = subscription_governor:confirm_cancellation(SubPid, #{}),

    ok.
```

### Example 2: Billing with Quota Enforcement

```erlang
%% Handle billing with quota escalation
billing_with_quota_example() ->
    % 1. Start billing and quota governors
    {ok, BillingPid} = billing_governor:start_link(
        <<"acme_corp">>,
        <<"bill_001">>
    ),
    {ok, QuotaPid} = quota_sla_governor:start_link(
        <<"acme_corp">>,
        <<"quota_001">>
    ),

    % 2. Initialize billing for customer
    {ok, active} = billing_governor:initialize_billing(
        BillingPid,
        <<"customer_789">>,
        #{
            <<"billing_cycle">> => <<"monthly">>,
            <<"amount_usd">> => 99.99,
            <<"currency">> => <<"USD">>
        }
    ),

    % 3. Set quota limits
    {ok, active} = quota_sla_governor:set_quota(
        QuotaPid,
        <<"API calls">>,
        #{
            <<"limit">> => 10000,
            <<"window_minutes">> => 60,
            <<"burst_multiplier">> => 1.5
        }
    ),

    % 4. Record usage (doesn't exceed quota)
    {ok, active} = quota_sla_governor:record_usage(
        QuotaPid,
        <<"API calls">>,
        5000,
        #{}
    ),
    io:format("Usage recorded: 5000/10000 calls~n", []),

    % 5. Exceed quota (active -> quota_exceeded)
    {ok, quota_exceeded} = quota_sla_governor:record_usage(
        QuotaPid,
        <<"API calls">>,
        7000,  % Total would be 12000, exceeds 10000 limit
        #{<<"user_id">> => <<"user_456">>}
    ),

    % 6. Reset quota after billing cycle (quota_exceeded -> reset -> active)
    {ok, reset} = quota_sla_governor:reset_quota(QuotaPid, #{}),
    {ok, active} = quota_sla_governor:resume_quota(QuotaPid, #{}),

    % 7. View audit trail for compliance
    {ok, BillingReceipts} = billing_governor:list_receipts(BillingPid),
    io:format("Billing audit: ~p records~n", [length(BillingReceipts)]),

    ok.
```

### Example 3: Multi-Tenant Entitlements

```erlang
%% Manage entitlements for multiple tenants
multi_tenant_example() ->
    % 1. Create entitlements for multiple tenants
    Tenants = [<<"acme_corp">>, <<"xyz_inc">>, <<"startup_io">>],
    Pids = lists:map(fun(TenantId) ->
        {ok, Pid} = entitlement_governor:start_link(
            TenantId,
            <<"feature_advanced_analytics">>
        ),
        {TenantId, Pid}
    end, Tenants),

    % 2. Grant entitlements in parallel
    lists:foreach(fun({TenantId, Pid}) ->
        entitlement_governor:grant_entitlement(
            Pid,
            <<"cust_", TenantId/binary>>,
            #{<<"plan">> => <<"enterprise">>}
        )
    end, Pids),

    % 3. Verify all tenants have pending_review state
    States = lists:map(fun({TenantId, Pid}) ->
        {ok, State} = entitlement_governor:get_state(Pid, <<"feature_advanced_analytics">>),
        {TenantId, State}
    end, Pids),
    io:format("Tenant states: ~p~n", [States]),

    % 4. Suspend one tenant's entitlement (compliance issue)
    [{<<"acme_corp">>, AcmePid} | _] = Pids,
    {ok, suspended} = entitlement_governor:suspend_entitlement(
        AcmePid,
        <<"compliance_violation">>,
        #{<<"violation_code">> => <<"GDPR_017">>}
    ),

    % 5. Check states again
    {ok, State1} = entitlement_governor:get_state(AcmePid, <<"feature_advanced_analytics">>),
    io:format("ACME entitlement now: ~p~n", [State1]),

    ok.
```

### Example 4: Error Handling and Invalid Transitions

```erlang
%% Demonstrate error handling with invalid state transitions
error_handling_example() ->
    {ok, Pid} = entitlement_governor:start_link(
        <<"error_test">>,
        <<"ent_001">>
    ),

    % Initial state is 'unentitled'
    {ok, unentitled} = entitlement_governor:get_state(Pid, <<"ent_001">>),

    % 1. Try invalid transition: can't revoke from unentitled
    Result1 = entitlement_governor:revoke_entitlement(
        Pid,
        <<"invalid_transition">>,
        #{}
    ),
    io:format("Revoke from unentitled: ~p~n", [Result1]),
    % Expected: {error, {invalid_transition, unentitled}}

    % 2. Valid transition: grant (unentitled -> pending_review)
    {ok, pending_review} = entitlement_governor:grant_entitlement(
        Pid,
        <<"cust_123">>,
        #{<<"reason">> => <<"request_granted">>}
    ),

    % 3. Try invalid transition: can't suspend from pending_review...
    % Actually, suspend IS valid from pending_review!
    {ok, suspended} = entitlement_governor:suspend_entitlement(
        Pid,
        <<"request_denied">>,
        #{<<"reason">> => <<"customer_fraud_suspected">>}
    ),

    % 4. Try invalid transition: can't grant from suspended
    Result4 = entitlement_governor:grant_entitlement(
        Pid,
        <<"cust_123">>,
        #{}
    ),
    io:format("Grant from suspended: ~p~n", [Result4]),
    % Expected: {error, {invalid_transition, suspended}}

    % 5. Valid escalation path: suspended -> escalated
    {ok, escalated} = entitlement_governor:escalate_issue(
        Pid,
        <<"fraud_investigation_initiated">>,
        #{}
    ),

    % 6. Archive (escalated -> archived)
    {ok, archived} = entitlement_governor:archive_entitlement(
        Pid,
        <<"fraud_cleared">>,
        #{<<"case_id">> => <<"FRAUD_12345">>}
    ),

    % 7. View complete history
    {ok, Receipts} = entitlement_governor:list_receipts(Pid, <<"ent_001">>),
    io:format("Complete transition history:~n", []),
    lists:foreach(fun(Receipt) ->
        #{state_from := From, state_to := To, reason := Reason} = Receipt,
        io:format("  ~p -> ~p (~p)~n", [From, To, Reason])
    end, Receipts),

    ok.
```

---

## Troubleshooting

### Common Issues

#### 1. Module Not Found
```erlang
% Error: {error, nofile}
% Cause: Module not compiled or not in code path

% Solution:
% a) Verify compilation
rebar3 compile
rebar3 shell  % Automatically adds to code path

% b) Or manually add path
(erlang@localhost)1> code:add_path("_build/default/lib/*/ebin").
```

#### 2. Supervisor Child Crashes
```erlang
% When child exits abnormally, supervisor may restart or crash entire tree
% Check supervisor restart strategy

% In autonomics_sup.erl:
% strategy => one_for_all  % If any child crashes, restart all

% Mitigation:
% - Add error_logger to capture crash dumps
% - Use timer:sleep/1 in init to delay startup
% - Check logs for root cause
```

#### 3. Memory Leak in ETS Tables
```erlang
% If many entitlements created, ETS tables accumulate
% Solution: Ensure terminate/3 callback deletes tables

handle_event(...) -> {next_state, NewState, Data};
terminate(_Reason, _State, Data) ->
    % CRITICAL: Clean up ETS table
    ets:delete(Data#entitlement_data.receipt_table).
```

#### 4. Hot Code Reloading Issues
```erlang
% In production, can reload module without restarting:
(erlang@localhost)1> c(entitlement_governor).
{ok,entitlement_governor}

% But running processes keep old code version
% Must restart process to use new code:
(erlang@localhost)2> exit(OldPid, code_change).
true
(erlang@localhost)3> {ok, NewPid} = entitlement_governor:start_link(...).
{ok, <0.99.0>}
```

#### 5. gen_statem Handle Event Function Errors
```erlang
% Common error: Forgetting to return {keep_state_and_data}
% Solution: All handle_event clauses must return valid state/action tuple

% Valid returns:
{next_state, NewState, NewData}
{next_state, NewState, NewData, Actions}
{keep_state, NewData}
{keep_state_and_data}
{keep_state_and_data, Actions}
{stop, Reason, Data}
```

### Debug Techniques

#### 1. Enable Debug Tracing
```erlang
% Trace all messages to a specific process
(erlang@localhost)1> dbg:tracer().
{ok,<0.89.0>}

(erlang@localhost)2> dbg:p(Pid, [m, c, s]).  % message, call, return
% Now all messages to Pid are printed

% Disable:
(erlang@localhost)3> dbg:stop().
ok
```

#### 2. Get Process Information
```erlang
% Check process state and memory usage
(erlang@localhost)1> process_info(Pid).
[{current_function,{gen_statem,loop_receive,3}},
 {initial_call,{gen,init_it,6}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.45.0>]},
 {dictionary,[]}]

% Check memory usage of all processes
(erlang@localhost)2> [{Pid, Memory} || {Pid, _, Memory} <-
    [{P, undefined, erlang:process_info(P, memory)} || P <- processes()]].
```

#### 3. Inspect ETS Table Contents
```erlang
% View all receipts for an entitlement
(erlang@localhost)1> ets:all().
[
  'acme_corp_ent_001_receipts',
  'xyz_inc_ent_002_receipts',
  ...
]

(erlang@localhost)2> ets:tab2list('acme_corp_ent_001_receipts').
[{receipt, #{...}}, {receipt, #{...}}, ...]

% Check table statistics
(erlang@localhost)3> ets:info('acme_corp_ent_001_receipts').
[{memory,1024},
 {owner,<0.45.0>},
 {name,'acme_corp_ent_001_receipts'},
 {size,5},
 {node,nonode@nohost},
 {named_table,true},
 {type,set},
 {keypos,1},
 {protection,public},
 {write_concurrency,true},
 {read_concurrency,false},
 {compressed,false}]
```

#### 4. Check Supervisor Tree Status
```erlang
% View current supervision tree
(erlang@localhost)1> supervisor:which_children(autonomics_sup).
[
  {governance_sup, <0.46.0>, supervisor, [governance_sup]},
  {receipt_ledger_sup, <0.47.0>, supervisor, [receipt_ledger_sup]},
  {cluster_sup, <0.48.0>, supervisor, [cluster_sup]},
  {observability_sup, <0.49.0>, supervisor, [observability_sup]}
]

% Count running processes
(erlang@localhost)2> length(supervisor:which_children(governance_sup)).
8  % 8 subsystem supervisors
```

---

## Next Steps

1. **Read GEN_STATEM_PATTERNS.md** for advanced state machine design
2. **Explore ERLANG_DISTRIBUTION.md** for clustering and high availability
3. **Check PERFORMANCE_TUNING.md** for optimization techniques
4. **Review gen_statem documentation**: http://erlang.org/doc/man/gen_statem.html

---

**Last Updated**: January 2026
**Erlang Version**: OTP 25+
**Status**: Production-Ready
