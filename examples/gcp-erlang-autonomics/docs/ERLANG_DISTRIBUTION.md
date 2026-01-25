# Erlang Distribution, Clustering, and Node Management

## Table of Contents
1. [Distribution Basics](#distribution-basics)
2. [RPC Calls Between Nodes](#rpc-calls-between-nodes)
3. [Global Processes with gproc](#global-processes-with-gproc)
4. [Distributed Transactions with mnesia](#distributed-transactions-with-mnesia)
5. [Node Failures and Reconnection](#node-failures-and-reconnection)
6. [Cluster Debugging](#cluster-debugging)
7. [Production Patterns](#production-patterns)

---

## Distribution Basics

### What is Erlang Distribution?

Erlang distribution enables multiple Erlang nodes to communicate transparently:

```
Node 1 (autonomics1@host1)
    └─ Autonomics App
       ├─ Entitlement FSM
       ├─ Billing FSM
       └─ Quota SLA FSM

Node 2 (autonomics2@host2)
    └─ Autonomics App
       ├─ Entitlement FSM
       ├─ Billing FSM
       └─ Quota SLA FSM

        [Network Link]
         RPC Calls
         Message Passing
         Pub/Sub
```

### Key Concepts

**Node**: Single Erlang runtime (erl process)
**Cluster**: Multiple interconnected nodes
**RPC**: Remote procedure call between nodes
**Distribution**: Transparent message passing across networks

### Enable Distribution

#### 1. Start with Distribution Parameters

```bash
# Start Erlang node with distribution
erl -name autonomics1@127.0.0.1 -setcookie autonomics_cluster

# Or with sname (short name, same host only)
erl -sname node1 -setcookie autonomics_cluster

# Verify distribution is enabled
(autonomics1@127.0.0.1)1> erlang:node().
'autonomics1@127.0.0.1'

(autonomics1@127.0.0.1)2> nodes().
[]  % No other nodes connected yet
```

#### 2. Cookie Configuration

The **cookie** is a shared secret for authentication:

```bash
# Set cookie via command line
erl -setcookie mySecret

# Or environment variable
export ERL_EPMD_ADDRESS=localhost
export ERL_DIST_PORT=9999
erl -name node1@localhost

# Or via erlang.cookie file (~/.erlang.cookie)
echo "mySecret" > ~/.erlang.cookie
chmod 400 ~/.erlang.cookie
erl -name node1@localhost  # Uses ~/.erlang.cookie automatically
```

### Network Requirements

1. **EPMD (Erlang Port Mapper Daemon)**: Maps node names to ports
   - Listens on port 4369 (default)
   - Must be running on each host

2. **Node Communication**: Each node needs unique port
   - Default: Kernel assigns dynamic ports
   - Explicit: Use `-kernel inet_dist_listen_min` and `_max`

3. **Firewall**: Open ports for inter-node communication

```bash
# Check EPMD running
epmd -names

# Output:
# name node1 at port 45678
# name node2 at port 45679
# name node3 at port 45680
```

---

## RPC Calls Between Nodes

### Basic RPC Patterns

#### Pattern 1: Direct RPC (rpc:call)

```erlang
% From node1, call function on node2
(autonomics1@127.0.0.1)1> rpc:call('autonomics2@127.0.0.1', erlang, node, []).
'autonomics2@127.0.0.1'

% Call custom module function
(autonomics1@127.0.0.1)2> rpc:call('autonomics2@127.0.0.1',
    entitlement_governor, grant_entitlement,
    [Pid, CustomerId, Metadata]).
{ok, pending_review}
```

#### Pattern 2: RPC with Timeout

```erlang
% Prevent hanging on unreachable node
(autonomics1@127.0.0.1)1> rpc:call('autonomics2@127.0.0.1',
    billing_governor, process_payment,
    [Pid, PaymentInfo],
    5000).  % 5 second timeout

% Returns {badrpc, timeout} if node unreachable
{badrpc, timeout}
```

#### Pattern 3: Async RPC (rpc:async_call)

```erlang
% Start RPC without blocking
(autonomics1@127.0.0.1)1> Key = rpc:async_call(
    'autonomics2@127.0.0.1',
    billing_governor, process_payment,
    [Pid, PaymentInfo]).
{key, ...}

% Continue with other work...

% Retrieve result later
(autonomics1@127.0.0.1)2> Result = rpc:yield(Key).
{ok, paid}

% Or with timeout
(autonomics1@127.0.0.1)3> Result = rpc:nb_yield(Key, 1000).
{value, {ok, paid}}
% or timeout
{value, {error, timeout}}
```

#### Pattern 4: Batch RPC (rpc:multicall)

```erlang
% Call same function on multiple nodes
(autonomics1@127.0.0.1)1> Nodes = [
    'autonomics2@127.0.0.1',
    'autonomics3@127.0.0.1',
    'autonomics4@127.0.0.1'
].
['autonomics2@127.0.0.1',...]

(autonomics1@127.0.0.1)2> {Results, BadNodes} = rpc:multicall(
    Nodes,
    erlang, node, [],
    5000).

Results = [
    'autonomics2@127.0.0.1',
    'autonomics3@127.0.0.1',
    'autonomics4@127.0.0.1'
],
BadNodes = []  % All nodes responded
```

### RPC Error Handling

```erlang
make_rpc_call(Node, Module, Function, Args) ->
    try rpc:call(Node, Module, Function, Args, 5000) of
        Result ->
            case Result of
                {badrpc, Reason} ->
                    {error, {rpc_failed, Reason}};
                {error, Reason} ->
                    {error, Reason};
                Value ->
                    {ok, Value}
            end
    catch
        error:badarg ->
            {error, invalid_node};
        error:undef ->
            {error, function_not_exported}
    end.

% Usage
case make_rpc_call('autonomics2@127.0.0.1', billing_governor,
        process_payment, [Pid, PaymentInfo]) of
    {ok, Result} ->
        io:format("Payment successful: ~p~n", [Result]);
    {error, {rpc_failed, timeout}} ->
        io:format("RPC timeout - node unreachable~n", []);
    {error, {rpc_failed, nodedown}} ->
        io:format("Node is down~n", []);
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason])
end.
```

### RPC in gen_statem

```erlang
-module(distributed_billing_governor).
-behaviour(gen_statem).

% FSM that coordinates with other nodes
handle_event({call, From}, {process_payment, PaymentDetails}, active, Data) ->
    % Try local processing first
    case local_process_payment(PaymentDetails) of
        {ok, TxId} ->
            {keep_state_and_data, [{reply, From, {ok, TxId}}]};

        {error, insufficient_balance} ->
            % Route to backup node
            case get_backup_node() of
                undefined ->
                    {keep_state_and_data, [{reply, From, {error, no_backup}}]};

                BackupNode ->
                    % RPC to backup for processing
                    case rpc:call(BackupNode, billing_governor,
                            process_payment, [self(), PaymentDetails], 5000) of
                        {ok, TxId} ->
                            {keep_state_and_data, [{reply, From, {ok, TxId}}]};
                        {badrpc, timeout} ->
                            {next_state, backup_sync_required, Data,
                             [{reply, From, {error, {rpc_timeout, BackupNode}}}]};
                        {error, Reason} ->
                            {keep_state_and_data, [{reply, From, {error, Reason}}]}
                    end
            end
    end.
```

---

## Global Processes with gproc

### What is gproc?

**gproc**: Global PRocess registry and Coordination
- Registers processes globally across cluster
- Enables location-transparent process lookup
- Supports pub/sub, property lists, counters

### Installation

Add to rebar.config:
```erlang
{deps, [
    {gproc, {git, "https://github.com/uwiger/gproc.git", {tag, "0.9.1"}}}
]}.
```

Compile:
```bash
rebar3 compile
```

### Basic Usage

#### Register a Process Globally

```erlang
% Start FSM and register globally
{ok, Pid} = entitlement_governor:start_link(
    <<"tenant123">>,
    <<"entitlement456">>
),

% Register with gproc
gproc:reg({n, l, {entitlement, <<"tenant123">>, <<"entitlement456">>}}),

% Now other nodes can find it
```

#### Lookup Global Process

```erlang
% From any node, find the process
Pid = gproc:where({n, l, {entitlement, <<"tenant123">>, <<"entitlement456">>}}),

% If registered, Pid is the process handle
% If not registered, undefined

case gproc:where({n, l, {entitlement, <<"tenant123">>, <<"entitlement456">>}}) of
    undefined ->
        % Create it
        {ok, NewPid} = create_entitlement(...);
    Pid ->
        % Use existing
        ok
end
```

#### Register with Properties

```erlang
% Register with additional metadata
gproc:reg(
    {n, l, {billing, <<"tenant123">>, <<"bill001">>}},
    #{
        status => active,
        tier => premium,
        monthly_limit => 9999.99
    }
),

% Retrieve property
{Pid, Props} = gproc:lookup_value({n, l, {billing, <<"tenant123">>, <<"bill001">>}}),
Tier = maps:get(tier, Props).
```

#### Update Property

```erlang
% Update property of registered process
gproc:set_value({n, l, {billing, TenantId, BillId}},
    #{status => suspended}).

% Retrieve updated value
{_Pid, UpdatedProps} = gproc:lookup_value({n, l, {billing, TenantId, BillId}}).
```

### Pub/Sub with gproc

```erlang
% Publisher registers topic
gproc:reg({p, l, {billing_event, TenantId}}),

% ... later ...

% Publish event
gproc:send({p, l, {billing_event, TenantId}},
    {payment_processed, #{tx_id => TxId, amount => Amount}}).

% Subscriber receives message in mailbox
receive
    {payment_processed, PaymentData} ->
        io:format("Payment received: ~p~n", [PaymentData])
end.
```

### Service Discovery Pattern

```erlang
% Supervisor starts entitlement governors and registers them
init({TenantId}) ->
    SupFlags = #{strategy => one_for_one},

    ChildSpecs = [
        #{
            id => {entitlement, TenantId},
            start => {entitlement_governor, start_link, [TenantId, auto]},
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

% In entitlement_governor:init/1
init({TenantId, EntitlementId}) ->
    % ... initialization ...

    % Register globally
    gproc:reg({n, l, {entitlement, TenantId, EntitlementId}}),

    {ok, unentitled, Data}.

% In terminate/3
terminate(_Reason, _State, Data) ->
    % Auto-unregister on shutdown
    catch gproc:unreg({n, l, {entitlement,
        Data#entitlement_data.tenant_id,
        Data#entitlement_data.entitlement_id
    }}).
```

---

## Distributed Transactions with mnesia

### What is mnesia?

**mnesia**: Distributed database built into Erlang/OTP
- ACID transactions
- Replication across nodes
- Disk and memory storage
- No external dependencies

### Setup

```erlang
% Start mnesia (usually in application start hook)
mnesia:start().

% Create schema (one-time setup)
mnesia:create_schema([node()]).

% Create table with replicas
mnesia:create_table(receipt_log, [
    {disc_copies, ['autonomics1@127.0.0.1', 'autonomics2@127.0.0.1']},
    {attributes, [id, timestamp, tenant_id, data]}
]).
```

### Store Receipts in mnesia

```erlang
-record(receipt_log, {
    id :: binary(),
    timestamp :: non_neg_integer(),
    tenant_id :: binary(),
    data :: map()
}).

emit_receipt(Data, FromState, ToState, Reason, Metadata) ->
    Receipt = {receipt_log,
        generate_receipt_id(),
        timestamp(),
        Data#entitlement_data.tenant_id,
        #{
            state_from => FromState,
            state_to => ToState,
            reason => Reason,
            metadata => Metadata
        }
    },

    % ACID transaction
    Fun = fun() ->
        mnesia:write(Receipt)
    end,

    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, Reason} ->
            % Log error but don't crash FSM
            error_logger:warning_msg(
                "Failed to store receipt: ~p~n", [Reason])
    end.
```

### Query Receipts

```erlang
get_receipts(TenantId) ->
    Fun = fun() ->
        % Match all receipts for tenant
        mnesia:match_object(
            #receipt_log{
                id = '_',
                timestamp = '_',
                tenant_id = TenantId,
                data = '_'
            }
        )
    end,

    case mnesia:transaction(Fun) of
        {atomic, Receipts} -> {ok, Receipts};
        {aborted, Reason} -> {error, Reason}
    end.
```

### Distributed Transactions Across FSMs

```erlang
% Coordinate payment + subscription state change
process_subscription_payment(SubPid, BillingPid, Amount) ->
    Fun = fun() ->
        % Read both FSM states atomically
        {ok, SubState} = gen_statem:call(SubPid, get_state),
        {ok, BillingState} = gen_statem:call(BillingPid, get_state),

        % Validate consistency
        case {SubState, BillingState} of
            {active, active} ->
                % Both in good state - proceed
                % Write to mnesia (atomic)
                mnesia:write(#payment_log{
                    id => generate_id(),
                    timestamp => timestamp(),
                    amount => Amount
                }),

                % Update FSMs
                gen_statem:call(BillingPid, {process_payment, Amount}),
                {ok, amount_billed};

            _ ->
                % State mismatch - abort transaction
                mnesia:abort({state_mismatch, SubState, BillingState})
        end
    end,

    case mnesia:transaction(Fun) of
        {atomic, Result} -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.
```

---

## Node Failures and Reconnection

### Monitoring Node Status

#### Node Down Detection

```erlang
% Start monitoring node status changes
net_kernel:monitor_nodes(true, [
    {node_type, visible}  % Only visible nodes (default)
]).

% Receive nodeup/nodedown messages
handle_info({nodeup, Node}, State, Data) ->
    io:format("Node connected: ~p~n", [Node]),
    {keep_state, Data};

handle_info({nodedown, Node}, State, Data) ->
    io:format("Node disconnected: ~p~n", [Node]),
    % Trigger recovery logic
    {keep_state, Data}.
```

#### Manual Node Health Check

```erlang
check_node_health(Node) ->
    case catch rpc:call(Node, erlang, node, [], 2000) of
        Node ->
            % Node is alive and responding
            {ok, healthy};
        {badrpc, timeout} ->
            {error, timeout};
        {badrpc, nodedown} ->
            {error, nodedown};
        _ ->
            {error, unknown}
    end.

% Monitor multiple nodes
monitor_cluster_health(Nodes) ->
    Results = lists:map(fun(Node) ->
        case check_node_health(Node) of
            {ok, healthy} -> {Node, up};
            {error, Reason} -> {Node, {down, Reason}}
        end
    end, Nodes),

    DownNodes = [N || {N, {down, _}} <- Results],
    {length(Nodes) - length(DownNodes), length(Nodes), DownNodes}.
```

### Automatic Reconnection

```erlang
-module(cluster_manager).
-behaviour(gen_statem).

% Try to reconnect to cluster periodically
handle_event(state_timeout, reconnect, disconnected, Data) ->
    TargetNodes = get_target_nodes(),
    case try_connect_any(TargetNodes) of
        {ok, ConnectedNode} ->
            io:format("Reconnected to cluster via ~p~n", [ConnectedNode]),
            {next_state, connected, Data};
        {error, all_failed} ->
            % Backoff: retry after exponential delay
            RetryCount = maps:get(retry_count, Data, 0),
            NextDelay = calculate_backoff(RetryCount),
            NewData = Data#{retry_count := RetryCount + 1},
            {next_state, disconnected, NewData,
             [{state_timeout, NextDelay, reconnect}]}
    end.

try_connect_any([]) ->
    {error, all_failed};

try_connect_any([Node | Rest]) ->
    case net_kernel:connect_node(Node) of
        true -> {ok, Node};
        false -> try_connect_any(Rest)
    end.

calculate_backoff(RetryCount) ->
    % Exponential backoff: 1s, 2s, 4s, 8s, ... up to 60s
    BaseDelay = 1000,
    MaxDelay = 60000,
    Delay = trunc(BaseDelay * math:pow(2, RetryCount)),
    min(Delay, MaxDelay).
```

### Handling Split-Brain (Network Partition)

When a cluster splits into separate partitions:

```erlang
% In cluster_manager - detect split-brain
handle_event(info, {nodedown, Node}, State, Data) ->
    % Check if we're isolated
    VisibleNodes = erlang:nodes(visible),
    io:format("Nodes still visible: ~p~n", [VisibleNodes]),

    case VisibleNodes of
        [] ->
            % We're isolated (split-brain)
            {next_state, isolated, Data,
             [{state_timeout, 60000, heal_split}]};
        _ ->
            % Still connected to some nodes
            {keep_state, Data}
    end.

% Healing split-brain
handle_event(state_timeout, heal_split, isolated, Data) ->
    % Try to rejoin cluster
    TargetNodes = get_configured_nodes(),
    case try_connect_any(TargetNodes) of
        {ok, Node} ->
            io:format("Split-brain healed, rejoined via ~p~n", [Node]),
            % Run reconciliation
            reconcile_state(),
            {next_state, connected, Data};
        {error, _} ->
            % Still isolated, stay isolated and try again later
            {next_state, isolated, Data,
             [{state_timeout, 30000, heal_split}]}
    end.

reconcile_state() ->
    % Handle any diverged state between partitions
    % This depends on your specific domain
    % Example: resync receipts, billing records, etc.
    ok.
```

### Graceful Shutdown with Cluster Awareness

```erlang
% In application:stop/1
stop(_State) ->
    % Notify cluster we're shutting down
    cluster_manager:notify_shutdown(),

    % Give processes time to migrate state
    timer:sleep(5000),

    % Gracefully stop all supervisors
    ok.

% In cluster_manager
notify_shutdown() ->
    Nodes = erlang:nodes(visible),
    lists:foreach(fun(Node) ->
        gen_statem:cast({cluster_manager, Node}, {peer_shutting_down, node()})
    end, Nodes).

handle_event(cast, {peer_shutting_down, Node}, State, Data) ->
    % Migrate state from Node before it shuts down
    migrate_processes_from_node(Node),
    {keep_state, Data}.
```

---

## Cluster Debugging

### Inspect Cluster Topology

```erlang
% Show all connected nodes
(autonomics1@127.0.0.1)1> erlang:nodes().
['autonomics2@127.0.0.1','autonomics3@127.0.0.1']

% Show only visible nodes (connected)
(autonomics1@127.0.0.1)2> erlang:nodes(visible).
['autonomics2@127.0.0.1','autonomics3@127.0.0.1']

% Show hidden nodes (not distributed)
(autonomics1@127.0.0.1)3> erlang:nodes(hidden).
[]

% Show nodes by connectivity
(autonomics1@127.0.0.1)4> erlang:nodes(connected).
['autonomics2@127.0.0.1','autonomics3@127.0.0.1']
```

### Monitor RPC Calls

```erlang
% Enable RPC tracing
(autonomics1@127.0.0.1)1> rpc:call('autonomics2@127.0.0.1',
    erlang, trace, [all, true, [call]]).
{badrpc, ...}  % Trace output goes to node2's console

% Or manually trace RPC
dbg:tracer(),
dbg:p(all, [call, return_to]),
dbg:tpl(rpc, '_', [{'_', [], [{'return_trace'}]}]).
```

### Inspect Global Registry (gproc)

```erlang
% List all registered processes globally
(autonomics1@127.0.0.1)1> gproc:info(all).
{{n,l,{entitlement,<<"tenant1">>,<<"ent1">>}}, <0.45.0>, undefined}
{{n,l,{billing,<<"tenant1">>,<<"bill1">>}}, <0.46.0>, undefined}
...

% List by key pattern
(autonomics1@127.0.0.1)2> gproc:select([{{{n,l,{entitlement,'_','_'}},'_','_'},[],[{{'$1','$2','$3'}}]}]).
[{{n,l,{entitlement,<<"tenant1">>,<<"ent1">>}}, <0.45.0>, undefined},
 {{n,l,{entitlement,<<"tenant2">>,<<"ent2">>}}, <0.67.0>, undefined}]
```

### Trace Cluster Events

```erlang
% Comprehensive cluster monitoring
-module(cluster_debug).

enable_tracing() ->
    % Trace all cluster join/leave events
    net_kernel:monitor_nodes(true),

    % Trace RPC calls
    dbg:tracer(),
    dbg:p(all, [c, m]),  % calls and message passing
    dbg:tpl(rpc, '_', []),

    % Trace gen_statem events
    dbg:tpl(gen_statem, handle_event, []),

    ok.

disable_tracing() ->
    net_kernel:monitor_nodes(false),
    dbg:stop().
```

### Network Inspection

```bash
# Check EPMD status
epmd -names

# Output:
# name autonomics1 at port 45678
# name autonomics2 at port 45679

# Check network connectivity between nodes
netstat -an | grep 45678  # Look for ESTABLISHED connections

# Trace network traffic (Linux)
tcpdump -n -i any port 4369  # EPMD port
```

### Mnesia Cluster Inspection

```erlang
% Check mnesia tables
(autonomics1@127.0.0.1)1> mnesia:info().

% Lists all tables, their replicas, and status

% Check table replicas
(autonomics1@127.0.0.1)2> mnesia:table_info(receipt_log, disc_copies).
['autonomics1@127.0.0.1', 'autonomics2@127.0.0.1']

% Check replication lag
(autonomics1@127.0.0.1)3> mnesia:table_info(receipt_log, load_order).
0  % 0 = no loading required, replicas in sync

% Force table reload from disk
mnesia:wait_for_tables([receipt_log], 5000).
{ok, [receipt_log]}  % All replicas synced
```

---

## Production Patterns

### Pattern 1: High Availability with Automatic Failover

```erlang
-module(ha_entitlement_group).
-behaviour(gen_statem).

% Manage replicated entitlements across nodes
-record(ha_data, {
    tenant_id :: binary(),
    entitlement_id :: binary(),
    primary_node :: atom(),
    backup_nodes :: [atom()],
    current_state :: atom(),
    state_version :: pos_integer()
}).

start_link(TenantId, EntitlementId, Nodes) ->
    gen_statem:start_link({global, {entitlement_ha, TenantId, EntitlementId}}, ?MODULE,
        {TenantId, EntitlementId, Nodes}, []).

init({TenantId, EntitlementId, [Primary | Backups]}) ->
    % Monitor primary node
    net_kernel:monitor_nodes(true),

    Data = #ha_data{
        tenant_id = TenantId,
        entitlement_id = EntitlementId,
        primary_node = Primary,
        backup_nodes = Backups,
        current_state = unentitled,
        state_version = 0
    },

    case node() of
        Primary ->
            % We're the primary - start local FSM
            {ok, _LocalPid} = entitlement_governor:start_link(
                TenantId, EntitlementId),
            {ok, primary, Data};

        _ ->
            % We're a backup - wait for operations
            {ok, backup, Data}
    end.

% Primary handles operations
handle_event({call, From}, {grant_entitlement, CustomerId, Metadata}, primary, Data) ->
    % Process locally
    {ok, NewState} = execute_grant(Data, CustomerId, Metadata),

    % Replicate to backups
    replicate_to_backups(Data, NewState),

    NewData = Data#ha_data{
        current_state = NewState,
        state_version = Data#ha_data.state_version + 1
    },

    {keep_state, NewData, [{reply, From, {ok, NewState}}]};

% Primary node goes down - promote backup
handle_info({nodedown, FailedNode}, primary, Data)
    when FailedNode =:= Data#ha_data.primary_node ->
    % Shouldn't happen (we're primary), but handle gracefully
    {keep_state, Data};

handle_info({nodedown, Node}, backup, Data) ->
    case Node =:= Data#ha_data.primary_node of
        true ->
            % Primary is down - check if we should promote
            case should_promote(Data) of
                true ->
                    % Become primary and start FSM
                    {ok, _LocalPid} = entitlement_governor:start_link(
                        Data#ha_data.tenant_id,
                        Data#ha_data.entitlement_id),
                    {next_state, primary, Data};
                false ->
                    {keep_state, Data}
            end;
        false ->
            % Another backup is down
            {keep_state, Data}
    end.

replicate_to_backups(Data, NewState) ->
    lists:foreach(fun(BackupNode) ->
        rpc:cast(BackupNode, ?MODULE, replicate_state,
            [Data#ha_data.entitlement_id, NewState])
    end, Data#ha_data.backup_nodes).

should_promote(Data) ->
    % Promote if we're the first backup
    case Data#ha_data.backup_nodes of
        [FirstBackup | _] -> FirstBackup =:= node();
        _ -> false
    end.
```

### Pattern 2: Load-Balancing Across Nodes

```erlang
-module(load_balanced_fsm_manager).

% Distribute FSM instances across cluster nodes
spawn_fsm(TenantId, EntitlementId) ->
    % Get list of available nodes
    AvailableNodes = get_available_nodes(),

    case AvailableNodes of
        [] -> {error, no_available_nodes};

        Nodes ->
            % Pick node with lowest load
            SelectedNode = select_least_loaded(Nodes),

            % Spawn FSM on selected node
            {ok, Pid} = rpc:call(SelectedNode, entitlement_governor, start_link,
                [TenantId, EntitlementId]),

            % Register globally with node hint
            gproc:reg({n, l, {entitlement, TenantId, EntitlementId}},
                #{node => SelectedNode}),

            {ok, Pid}
    end.

select_least_loaded(Nodes) ->
    Loads = lists:map(fun(Node) ->
        {ok, Load} = rpc:call(Node, erlang, statistics, [run_queue]),
        {Node, Load}
    end, Nodes),

    % Sort by load and pick first
    [{LeastLoadedNode, _} | _] = lists:keysort(2, Loads),
    LeastLoadedNode.

get_available_nodes() ->
    AllNodes = erlang:nodes(visible),
    lists:filter(fun(Node) ->
        case rpc:call(Node, erlang, node, [], 1000) of
            {badrpc, _} -> false;
            Node -> true
        end
    end, AllNodes).
```

### Pattern 3: Eventual Consistency with Reconciliation

```erlang
-module(eventual_consistency_manager).

% For systems where nodes may temporarily diverge
% Reconcile state periodically

reconcile_cluster_state() ->
    % Get state from all nodes
    Nodes = erlang:nodes(visible),
    States = collect_states(Nodes),

    % Find conflicts
    Conflicts = detect_conflicts(States),

    % Resolve conflicts with conflict resolution rules
    Resolved = resolve_conflicts(Conflicts),

    % Propagate resolved state back to all nodes
    propagate_resolved_state(Resolved, Nodes).

collect_states(Nodes) ->
    lists:map(fun(Node) ->
        {ok, NodeState} = rpc:call(Node, ?MODULE, get_local_state, []),
        {Node, NodeState}
    end, Nodes).

detect_conflicts(States) ->
    % Group by entitlement ID
    ById = lists:foldl(fun({Node, State}, Acc) ->
        EntId = maps:get(entitlement_id, State),
        case maps:get(EntId, Acc, []) of
            [] -> Acc#{EntId => [{Node, State}]};
            Existing -> Acc#{EntId => [{Node, State} | Existing]}
        end
    end, #{}, States),

    % Find entries with >1 state
    maps:filter(fun(_EntId, States) -> length(States) > 1 end, ById).

resolve_conflicts(Conflicts) ->
    maps:map(fun(EntId, ConflictingStates) ->
        % Apply conflict resolution rules
        % Example: most recent wins (based on timestamp)
        Sorted = lists:sort(fun({_N1, S1}, {_N2, S2}) ->
            maps:get(timestamp, S1, 0) > maps:get(timestamp, S2, 0)
        end, ConflictingStates),

        {_WinnerNode, WinnerState} = hd(Sorted),
        WinnerState
    end, Conflicts).

propagate_resolved_state(ResolvedStates, Nodes) ->
    maps:foreach(fun(EntId, ResolvedState) ->
        lists:foreach(fun(Node) ->
            rpc:cast(Node, ?MODULE, apply_resolved_state,
                [EntId, ResolvedState])
        end, Nodes)
    end, ResolvedStates).
```

---

**Last Updated**: January 2026
**OTP Version**: OTP 25+
**Status**: Production-Ready
