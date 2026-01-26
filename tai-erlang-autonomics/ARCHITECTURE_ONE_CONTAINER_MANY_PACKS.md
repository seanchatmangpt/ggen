# TAI 2026: One Container, Many Packs Architecture
## Dynamic Entitlement-Driven Capability Model

**Date**: 2026-01-26
**Pattern**: Single runtime artifact + many gated capability packs
**Status**: Production-Ready Design
**Benefit**: Fast shipping + long-tail discovery + operational simplicity

---

## Executive Summary

**Problem**: Building 10,000 container images (one per SKU variant) is unsustainable.

**Solution**: One container (`taiea-core`) with all modules built-in, but capabilities are **inert until entitlement allows**.

**Result**:
- ✅ One build artifact
- ✅ One deployment path
- ✅ One operational surface
- ✅ Many SKU variants (via entitlement + packs)
- ✅ Still discoverable on marketplace (separate listings point to same image)

---

## Architecture: Entitlement → Packs → Tool Registry → Governor Gates

```
CUSTOMER PURCHASES SKU
  ↓
Marketplace → Entitlement service (active)
  ↓
Container receives entitlement event
  ├─ SKU_ID: "rollback-guard-professional"
  ├─ PLAN_ID: "professional-tier"
  ├─ ENABLED_PACKS: ["rollback_guard", "storm_discipline"]
  ├─ IAM_SCOPE: ["read:deployment", "write:rollback"]
  └─ EXPIRY: 2026-02-26
  ↓
Entitlement Resolver (updates state)
  ├─ Active packs: ["rollback_guard", "storm_discipline"]
  ├─ Disabled packs: ["iam_drift_guard", "receipt_verifier"]
  └─ Emits receipt: "entitlement_activated"
  ↓
MCP Tool Registry (dynamic)
  ├─ Registers only tools from ACTIVE packs
  ├─ Tool 1: ac_rollback_orchestrator (enabled)
  ├─ Tool 2: ac_storm_discipline_governor (enabled)
  ├─ Tool 3: ac_iam_drift_guard (NOT visible - disabled pack)
  └─ Emits receipt: "tool_surface_changed"
  ↓
CUSTOMER CALLS TOOL (or tries to)
  ├─ Tool is visible? Check tool registry
  │ ├─ YES: Tool is registered (enabled pack)
  │ ├─ NO: Return "tool_not_found" receipt (disabled pack)
  │ └─ Proceed to governor gate
  ├─ Governor gate checks prerequisites:
  │ ├─ Entitlement active? (not expired)
  │ ├─ IAM role allowed? (action requires read:deployment)
  │ ├─ Preconditions met? (safe to execute)
  │ └─ If all pass: EXECUTE bounded action
  │    If any fail: REFUSE + refusal receipt
  └─ Emit action receipt (result or refusal reason)
```

---

## Core Design: What Must Be True

### 1) Entitlement Controls Two Dimensions

#### A) Tool Visibility (What's in the API Surface)

```erlang
% Entitlement received:
Entitlement = #{
  sku_id => "rollback-guard-professional",
  enabled_packs => [rollback_guard, storm_discipline],
  disabled_packs => [iam_drift_guard, receipt_verifier],
  iam_roles => [read_deployment, write_rollback],
  expiry => 1740534000  % 2026-02-26
}.

% MCP tool registry reflects this:
VisibleTools = [
  {rollback_guard, ac_rollback_orchestrator},
  {rollback_guard, ac_deployment_validator},
  {storm_discipline, ac_storm_discipline_governor},
  % iam_drift_guard tools NOT listed (disabled pack)
  % receipt_verifier tools NOT listed (disabled pack)
].

% If customer tries to call ac_iam_drift_guard:
% → "tool_not_found" receipt (not registered)
```

#### B) Action Execution (Even if Visible, Preconditions Gate It)

```erlang
% Tool is visible and customer calls it, but:

% Step 1: Check entitlement active
ac_governor:verify_entitlement(EntitlementId)
  → {ok, Entitlement} | {error, {expired, ExpiryTime}}

% Step 2: Check IAM role
ac_governor:verify_iam_role(UserId, RequiredRole)
  → {ok, Role} | {error, {insufficient_privilege, CurrentRole}}

% Step 3: Check action preconditions (safe to execute)
ac_governor:verify_preconditions(Action, Context)
  → {ok, Metadata} | {error, {missing_precondition, What}}

% Step 4: If all pass, execute bounded action
% If any fail, refuse + receipt
```

---

### 2) IAM Stays Least-Privilege

**One container does NOT mean one permission set.**

IAM is part of the entitlement pack:

```erlang
% Base plan entitlement:
BaseEntitlement = #{
  plan => "base",
  iam_roles => [
    read_deployment,      % Can see deployment state
    % NO write permissions
  ],
  enabled_packs => [
    receipt_verifier      % Read-only: verify past receipts
  ]
}.

% Professional plan entitlement:
ProfessionalEntitlement = #{
  plan => "professional",
  iam_roles => [
    read_deployment,      % Can see deployment state
    write_rollback,       % Can trigger rollback
    read_audit_trail      % Can see audit trail
  ],
  enabled_packs => [
    receipt_verifier,     % Read-only: verify receipts
    rollback_guard,       % Can execute rollbacks
    storm_discipline      % Can activate storm discipline
  ]
}.

% Enterprise plan entitlement:
EnterpriseEntitlement = #{
  plan => "enterprise",
  iam_roles => [
    read_deployment,
    write_rollback,
    read_audit_trail,
    write_policy,         % Can modify governance policies
    admin_dashboard       % Access to admin features
  ],
  enabled_packs => [
    receipt_verifier,
    rollback_guard,
    storm_discipline,
    iam_drift_guard,      % Can detect/prevent IAM drift
    policy_engine         % Full policy control
  ]
}.
```

**Key principle**: Entitlement enables features + customer must grant IAM roles. We never auto-escalate.

---

### 3) One Image, Many Marketplace Listings

You can have **multiple listings** that all point to **the same container image**, differing only by **metadata**:

```
Marketplace Listings (all same image: gcr.io/tai/taiea-core:1.0.0)

1) "TAI Rollback Guard — Base" ($29/mo)
   └─ SKU: rollback-guard-base
   └─ Enabled packs: [receipt_verifier]
   └─ IAM: [read_deployment]
   └─ Recommended for: Read-only audit/learning

2) "TAI Rollback Guard — Professional" ($299/mo)
   └─ SKU: rollback-guard-professional
   └─ Enabled packs: [receipt_verifier, rollback_guard, storm_discipline]
   └─ IAM: [read_deployment, write_rollback, read_audit_trail]
   └─ Recommended for: Production rollback management

3) "TAI Governance Suite — Enterprise" ($2,999/mo)
   └─ SKU: governance-suite-enterprise
   └─ Enabled packs: [all 6 packs]
   └─ IAM: [all 7 roles]
   └─ Recommended for: Full governance + compliance

All three listings → same container image, different entitlements
```

**Long-tail discovery**: Customers see many options. You ship one artifact. Win-win.

---

## Capability Packs: Concrete Schema

### Pack Definition (Erlang Map)

```erlang
-define(CAPABILITY_PACK_SCHEMA, #{
  pack_id => atom,                    % e.g., rollback_guard
  version => string,                   % e.g., "1.0.0"
  description => string,               % Human-readable
  tools => [tool_spec],                % List of {tool_name, tool_module}
  signals => [signal_spec],            % Andon signals emitted
  actions => [action_spec],            % Bounded actions allowed
  required_iam_roles => [atom],        % IAM roles needed
  incompatible_with => [atom],         % Packs that conflict
  requires => [atom],                  % Prerequisites (other packs)
  receipt_events => [atom],            % Event types for receipts
  storage => atom,                     % DB/ledger requirement
  compute_limits => map                % CPU/memory/timeout
}).

% Example: rollback_guard pack
RollbackGuardPack = #{
  pack_id => rollback_guard,
  version => "1.0.0",
  description => "Deployment rollback orchestration and safety gates",

  tools => [
    {rollback_orchestrator, ac_rollback_orchestrator_mcp},
    {deployment_validator, ac_deployment_validator_mcp},
    {rollback_status, ac_rollback_status_mcp}
  ],

  signals => [
    rollback_initiated,
    rollback_approved,
    rollback_refused,
    rollback_completed
  ],

  actions => [
    {trigger_rollback, [read_deployment, write_rollback]},
    {validate_rollback_safety, [read_deployment]},
    {check_rollback_status, [read_deployment]}
  ],

  required_iam_roles => [
    read_deployment,
    write_rollback,
    read_audit_trail
  ],

  incompatible_with => [],
  requires => [receipt_verifier],  % Must have receipt_verifier

  receipt_events => [
    rollback_initiated,
    rollback_approved,
    rollback_refused,
    rollback_completed,
    rollback_timeout
  ],

  storage => firestore,

  compute_limits => #{
    timeout_ms => 300000,      % 5 min timeout
    max_memory_mb => 256,
    max_cpu_percent => 25
  }
}.

% Example: storm_discipline pack
StormDisciplinePack = #{
  pack_id => storm_discipline,
  version => "1.0.0",
  description => "Request storm detection and adaptive throttling",

  tools => [
    {storm_detector, ac_storm_detector_mcp},
    {throttle_config, ac_throttle_config_mcp},
    {storm_status, ac_storm_status_mcp}
  ],

  signals => [
    storm_detected,
    throttle_activated,
    throttle_adjusted,
    storm_cleared
  ],

  actions => [
    {detect_storm, [read_deployment]},
    {activate_throttle, [read_deployment, write_config]},
    {adjust_throttle, [read_deployment, write_config]},
    {clear_storm, [read_deployment]}
  ],

  required_iam_roles => [
    read_deployment,
    write_config,
    read_metrics
  ],

  incompatible_with => [],
  requires => [receipt_verifier],

  receipt_events => [
    storm_detected,
    throttle_activated,
    throttle_adjusted,
    storm_cleared,
    storm_timeout
  ],

  storage => firestore,

  compute_limits => #{
    timeout_ms => 60000,       % 1 min timeout
    max_memory_mb => 128,
    max_cpu_percent => 15
  }
}.

% Example: iam_drift_guard pack
IamDriftGuardPack = #{
  pack_id => iam_drift_guard,
  version => "1.0.0",
  description => "IAM drift detection and automated remediation",

  tools => [
    {drift_detector, ac_iam_drift_detector_mcp},
    {drift_policy, ac_iam_drift_policy_mcp},
    {drift_remediate, ac_iam_drift_remediate_mcp}
  ],

  signals => [
    drift_detected,
    drift_remediated,
    drift_refused
  ],

  actions => [
    {detect_drift, [read_iam]},
    {remediate_drift, [read_iam, write_iam, write_policy]},
    {approve_remediation, [read_iam, write_policy]}
  ],

  required_iam_roles => [
    read_iam,
    write_iam,           % Must be able to fix IAM
    write_policy,
    read_audit_trail
  ],

  incompatible_with => [],
  requires => [receipt_verifier],

  receipt_events => [
    drift_detected,
    drift_remediated,
    drift_refused
  ],

  storage => firestore,

  compute_limits => #{
    timeout_ms => 180000,      % 3 min timeout
    max_memory_mb => 512,
    max_cpu_percent => 40
  }
}.
```

---

## Implementation: Entitlement Resolver

### Module: `ac_entitlement_resolver.erl`

```erlang
-module(ac_entitlement_resolver).
-behaviour(gen_server).

%% API
-export([
  start_link/1,
  update_entitlement/2,
  get_active_packs/1,
  get_enabled_tools/1,
  verify_entitlement_active/1,
  verify_iam_role/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("entitlement.hrl").

%% ===================================================================
%% STATE
%% ===================================================================

-record(state, {
  tenant_id :: binary,
  current_entitlement :: #entitlement{} | undefined,
  active_packs :: [atom],
  enabled_tools :: [atom],
  enabled_iam_roles :: [atom],
  pack_library :: map,      % All available packs
  last_update :: integer
}).

%% ===================================================================
%% API
%% ===================================================================

start_link(TenantId) ->
  gen_server:start_link({local, name(TenantId)}, ?MODULE, TenantId, []).

update_entitlement(TenantId, EntitlementEvent) ->
  gen_server:cast(name(TenantId), {update_entitlement, EntitlementEvent}).

get_active_packs(TenantId) ->
  gen_server:call(name(TenantId), {get_active_packs}).

get_enabled_tools(TenantId) ->
  gen_server:call(name(TenantId), {get_enabled_tools}).

verify_entitlement_active(TenantId) ->
  gen_server:call(name(TenantId), {verify_active}).

verify_iam_role(TenantId, RequiredRole) ->
  gen_server:call(name(TenantId), {verify_iam_role, RequiredRole}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(TenantId) ->
  PackLibrary = load_pack_library(),
  {ok, #state{
    tenant_id = TenantId,
    current_entitlement = undefined,
    active_packs = [],
    enabled_tools = [],
    enabled_iam_roles = [],
    pack_library = PackLibrary,
    last_update = 0
  }}.

%% Update entitlement (e.g., customer purchases plan)
handle_cast({update_entitlement, EntitlementEvent}, State) ->
  case validate_entitlement_event(EntitlementEvent) of
    {ok, Entitlement} ->
      {ActivePacks, EnabledTools, EnabledRoles} =
        compute_enabled_resources(Entitlement, State#state.pack_library),

      NewState = State#state{
        current_entitlement = Entitlement,
        active_packs = ActivePacks,
        enabled_tools = EnabledTools,
        enabled_iam_roles = EnabledRoles,
        last_update = erlang:system_time(second)
      },

      % Emit receipt: entitlement changed
      emit_receipt(entitlement_updated, Entitlement, NewState),

      % Update MCP tool registry (add/remove tools)
      update_mcp_registry(State#state.enabled_tools, EnabledTools),

      {noreply, NewState};

    {error, Reason} ->
      % Emit refusal receipt
      emit_receipt(entitlement_update_refused, Reason, State),
      {noreply, State}
  end.

%% Get active packs
handle_call({get_active_packs}, _From, State) ->
  {reply, {ok, State#state.active_packs}, State};

%% Get enabled tools
handle_call({get_enabled_tools}, _From, State) ->
  {reply, {ok, State#state.enabled_tools}, State};

%% Verify entitlement still active
handle_call({verify_active}, _From, State) ->
  case State#state.current_entitlement of
    undefined ->
      {reply, {error, no_entitlement}, State};
    Entitlement ->
      Now = erlang:system_time(second),
      if
        Entitlement#entitlement.expiry > Now ->
          {reply, {ok, Entitlement}, State};
        true ->
          {reply, {error, {expired, Entitlement#entitlement.expiry}}, State}
      end
  end;

%% Verify IAM role enabled
handle_call({verify_iam_role, RequiredRole}, _From, State) ->
  case lists:member(RequiredRole, State#state.enabled_iam_roles) of
    true ->
      {reply, {ok, RequiredRole}, State};
    false ->
      {reply, {error, {insufficient_privilege, RequiredRole}}, State}
  end.

%% ===================================================================
%% INTERNAL: Compute enabled resources from entitlement
%% ===================================================================

compute_enabled_resources(Entitlement, PackLibrary) ->
  EnabledPackIds = Entitlement#entitlement.enabled_packs,

  % Get pack definitions
  EnabledPacks = lists:filtermap(
    fun(PackId) ->
      case maps:get(PackId, PackLibrary, undefined) of
        undefined -> false;
        Pack -> {true, Pack}
      end
    end,
    EnabledPackIds
  ),

  % Extract tools, roles, signals
  Tools = lists:flatmap(
    fun(Pack) -> maps:get(tools, Pack, []) end,
    EnabledPacks
  ),

  Roles = Entitlement#entitlement.iam_roles,

  {EnabledPackIds, Tools, Roles}.

%% ===================================================================
%% INTERNAL: Update MCP tool registry
%% ===================================================================

update_mcp_registry(OldTools, NewTools) ->
  ToAdd = NewTools -- OldTools,
  ToRemove = OldTools -- NewTools,

  lists:foreach(
    fun({ToolName, ToolModule}) ->
      erlmcp_server:register_tool(ToolName, ToolModule)
    end,
    ToAdd
  ),

  lists:foreach(
    fun({ToolName, _}) ->
      erlmcp_server:unregister_tool(ToolName)
    end,
    ToRemove
  ),

  % Emit receipt: tool surface changed
  ac_receipt_ledger_mcp:append(
    tool_surface_changed,
    #{added => ToAdd, removed => ToRemove},
    #{source => entitlement_resolver}
  ).

%% ===================================================================
%% INTERNAL: Load pack library
%% ===================================================================

load_pack_library() ->
  #{
    receipt_verifier => receipt_verifier_pack(),
    rollback_guard => rollback_guard_pack(),
    storm_discipline => storm_discipline_pack(),
    iam_drift_guard => iam_drift_guard_pack(),
    policy_engine => policy_engine_pack(),
    advanced_analytics => advanced_analytics_pack()
  }.

%% ===================================================================
%% INTERNAL: Emit receipts
%% ===================================================================

emit_receipt(EventType, Data, _State) ->
  ac_receipt_ledger_mcp:append(
    EventType,
    Data,
    #{
      source => entitlement_resolver,
      timestamp => erlang:system_time(millisecond)
    }
  ).

%% ===================================================================
%% INTERNAL: Helper functions
%% ===================================================================

name(TenantId) -> {global, {entitlement_resolver, TenantId}}.

validate_entitlement_event(Event) ->
  % Validate structure and content
  case Event of
    #{sku_id := _, enabled_packs := _, iam_roles := _, expiry := _} ->
      {ok, Event};
    _ ->
      {error, invalid_entitlement_structure}
  end.
```

---

## Governor Gates: Bounded Execution

### Module: `ac_governor_gates.erl`

```erlang
-module(ac_governor_gates).

%% API: Gate before execution
-export([
  check_all_gates/3,
  check_entitlement/2,
  check_iam_role/3,
  check_preconditions/3,
  execute_bounded_action/4
]).

%% ===================================================================
%% GATE CHECKING
%% ===================================================================

%% Master gate: check all prerequisites before action
check_all_gates(TenantId, Action, Context) ->
  case check_entitlement(TenantId, Context) of
    {error, Reason} ->
      {error, {entitlement_check_failed, Reason}};
    {ok, _} ->
      RequiredRole = maps:get(required_iam_role, Action, no_role_required),
      case check_iam_role(TenantId, RequiredRole, Context) of
        {error, Reason} ->
          {error, {iam_check_failed, Reason}};
        {ok, _} ->
          case check_preconditions(TenantId, Action, Context) of
            {error, Reason} ->
              {error, {preconditions_failed, Reason}};
            {ok, Metadata} ->
              {ok, Metadata}
          end
      end
  end.

%% Gate 1: Entitlement must be active
check_entitlement(TenantId, _Context) ->
  ac_entitlement_resolver:verify_entitlement_active(TenantId).

%% Gate 2: IAM role must be enabled
check_iam_role(TenantId, no_role_required, _Context) ->
  {ok, no_role_required};
check_iam_role(TenantId, RequiredRole, _Context) ->
  ac_entitlement_resolver:verify_iam_role(TenantId, RequiredRole).

%% Gate 3: Action-specific preconditions (safe to execute?)
check_preconditions(TenantId, Action, Context) ->
  ActionId = maps:get(action_id, Action),

  % Check action-specific rules
  case check_action_safety(ActionId, Context) of
    {error, Reason} ->
      {error, Reason};
    {ok, Metadata} ->
      {ok, Metadata}
  end.

%% ===================================================================
%% EXECUTE WITH BOUNDS
%% ===================================================================

%% Execute action with time/memory/resource bounds
execute_bounded_action(TenantId, Action, Context, Handler) ->
  case check_all_gates(TenantId, Action, Context) of
    {error, Reason} ->
      % Emit refusal receipt
      emit_refusal_receipt(TenantId, Action, Reason),
      {error, Reason};

    {ok, Metadata} ->
      % Get compute bounds for this action
      Bounds = maps:get(compute_limits, Action, #{}),
      TimeoutMs = maps:get(timeout_ms, Bounds, 60000),
      MaxMemoryMb = maps:get(max_memory_mb, Bounds, 256),

      % Execute with bounds
      case bounded_call(Handler, Action, Context, TimeoutMs, MaxMemoryMb) of
        {ok, Result} ->
          % Emit action receipt
          emit_action_receipt(TenantId, Action, success, Result),
          {ok, Result};

        {error, timeout} ->
          emit_action_receipt(TenantId, Action, timeout, "Action exceeded timeout"),
          {error, {timeout, TimeoutMs}};

        {error, memory_exceeded} ->
          emit_action_receipt(TenantId, Action, memory_exceeded, "Action exceeded memory limit"),
          {error, {memory_exceeded, MaxMemoryMb}};

        {error, ExecutionError} ->
          emit_action_receipt(TenantId, Action, failed, ExecutionError),
          {error, ExecutionError}
      end
  end.

%% ===================================================================
%% INTERNAL: Bounded call implementation
%% ===================================================================

bounded_call(Handler, Action, Context, TimeoutMs, MaxMemoryMb) ->
  % Simplified: spawn process with timeout
  Pid = spawn_monitor(fun() ->
    Handler(Action, Context)
  end),

  receive
    {_Pid, {ok, Result}} ->
      {ok, Result};
    {_Pid, {error, Error}} ->
      {error, Error}
  after TimeoutMs ->
    {error, timeout}
  end.

%% ===================================================================
%% INTERNAL: Action-specific safety checks
%% ===================================================================

check_action_safety(rollback_trigger, Context) ->
  % Only allow rollback if:
  % 1. Current deployment is stable (no ongoing changes)
  % 2. Rollback target version is known
  % 3. Customer has approved rollback policy

  case {
    maps:get(deployment_stable, Context, false),
    maps:get(rollback_target_version, Context, undefined),
    maps:get(rollback_policy_approved, Context, false)
  } of
    {true, Version, true} when Version =/= undefined ->
      {ok, #{target_version => Version}};
    {false, _, _} ->
      {error, {deployment_not_stable, "Deployment has ongoing changes"}};
    {_, undefined, _} ->
      {error, {rollback_target_unknown, "No valid rollback target found"}};
    {_, _, false} ->
      {error, {rollback_policy_not_approved, "Rollback policy requires approval"}}
  end;

check_action_safety(activate_throttle, Context) ->
  % Only allow throttle if:
  % 1. Storm was actually detected
  % 2. Throttle policy is defined

  case {
    maps:get(storm_detected, Context, false),
    maps:get(throttle_policy_defined, Context, false)
  } of
    {true, true} ->
      {ok, #{reason => "Storm detected and throttle policy active"}};
    {false, _} ->
      {error, {no_storm_detected, "Cannot activate throttle without storm detection"}};
    {_, false} ->
      {error, {throttle_policy_undefined, "Throttle policy must be defined first"}}
  end;

check_action_safety(remediate_iam_drift, Context) ->
  % Only allow remediation if:
  % 1. Drift was actually detected
  % 2. Remediation is approved
  % 3. IAM service is reachable

  case {
    maps:get(drift_detected, Context, false),
    maps:get(remediation_approved, Context, false),
    check_iam_service_reachable()
  } of
    {true, true, ok} ->
      {ok, #{reason => "Drift detected, approved, IAM reachable"}};
    {false, _, _} ->
      {error, {drift_not_detected, "No IAM drift detected"}};
    {_, false, _} ->
      {error, {remediation_not_approved, "Remediation must be approved"}};
    {_, _, error} ->
      {error, {iam_service_unreachable, "Cannot contact IAM service"}}
  end;

check_action_safety(_, _Context) ->
  {error, {unknown_action, "Action safety check not defined"}}.

check_iam_service_reachable() ->
  % Health check to IAM service
  ok.  % Simplified

%% ===================================================================
%% INTERNAL: Receipt emission
%% ===================================================================

emit_refusal_receipt(TenantId, Action, Reason) ->
  ac_receipt_ledger_mcp:append(
    action_refused,
    #{
      action_id => maps:get(action_id, Action),
      reason => Reason,
      tenant_id => TenantId
    },
    #{source => governor_gates, timestamp => erlang:system_time(millisecond)}
  ).

emit_action_receipt(TenantId, Action, Status, Result) ->
  ac_receipt_ledger_mcp:append(
    action_executed,
    #{
      action_id => maps:get(action_id, Action),
      status => Status,
      result => Result,
      tenant_id => TenantId
    },
    #{source => governor_gates, timestamp => erlang:system_time(millisecond)}
  ).
```

---

## Tool Registration: Dynamic MCP Registry

### Module: `erlmcp_server.erl` (additions)

```erlang
%% MODIFIED: erlmcp_server.erl

-module(erlmcp_server).
-behaviour(gen_server).

%% API: Dynamic tool registration
-export([
  register_tool/2,      % Add tool to registry
  unregister_tool/1,    % Remove tool from registry
  list_registered_tools/0
]).

%% ===================================================================
%% STATE: Tool registry is now dynamic
%% ===================================================================

-record(state, {
  registered_tools = [] :: [{tool_name(), tool_module()}],
  tool_handlers = #{} :: #{tool_name() => tool_module()}
}).

%% ===================================================================
%% DYNAMIC REGISTRATION
%% ===================================================================

register_tool(ToolName, ToolModule) ->
  gen_server:call(?MODULE, {register_tool, ToolName, ToolModule}).

unregister_tool(ToolName) ->
  gen_server:call(?MODULE, {unregister_tool, ToolName}).

list_registered_tools() ->
  gen_server:call(?MODULE, {list_tools}).

%% ===================================================================
%% gen_server HANDLER: Dynamic tools
%% ===================================================================

handle_call({register_tool, ToolName, ToolModule}, _From, State) ->
  % Add tool to registry
  NewHandlers = State#state.tool_handlers#{ToolName => ToolModule},

  % Return to client the updated tool list
  % MCP client will see this tool in next introspection

  {reply, {ok, added}, State#state{tool_handlers = NewHandlers}};

handle_call({unregister_tool, ToolName}, _From, State) ->
  % Remove tool from registry
  NewHandlers = maps:remove(ToolName, State#state.tool_handlers),

  % Client will see this tool removed in next introspection

  {reply, {ok, removed}, State#state{tool_handlers = NewHandlers}};

handle_call({list_tools}, _From, State) ->
  Tools = maps:keys(State#state.tool_handlers),
  {reply, {ok, Tools}, State}.

%% ===================================================================
%% MCP INTEGRATION: Tool call routing
%% ===================================================================

%% When customer calls tool:
%% 1. Check if tool is registered (in tool_handlers)
%% 2. If yes: Pass to governor gates for precondition checking
%% 3. If no: Return "tool_not_found" (disabled pack)

handle_tool_call(ToolName, Arguments, TenantId, Context) ->
  case maps:get(ToolName, State#state.tool_handlers, undefined) of
    undefined ->
      % Tool not registered (disabled pack)
      {error, {tool_not_found, ToolName}};

    ToolModule ->
      % Tool is registered, check gates
      Action = #{action_id => ToolName, arguments => Arguments},
      ac_governor_gates:execute_bounded_action(
        TenantId,
        Action,
        Context,
        fun(A, C) -> ToolModule:handle_call(A, C) end
      )
  end.
```

---

## Entitlement Event Flow: Customer Purchases SKU

### Sequence: Purchase → Entitlement → Tool Registry Update

```
CUSTOMER PURCHASES SKU on Marketplace
  │
  └─→ Marketplace service emits event:
      {
        "event_type": "entitlement.activated",
        "tenant_id": "cust-12345",
        "sku_id": "rollback-guard-professional",
        "plan_id": "professional-tier",
        "enabled_packs": [
          "receipt_verifier",
          "rollback_guard",
          "storm_discipline"
        ],
        "iam_roles": [
          "read_deployment",
          "write_rollback",
          "read_audit_trail"
        ],
        "expiry": 1740534000
      }
  │
  └─→ Container receives event via webhook/MCP
      ac_entitlement_resolver:update_entitlement(TenantId, Event)
  │
  └─→ Entitlement Resolver:
      1. Validates event structure
      2. Computes active packs from enabled_packs
      3. Extracts enabled tools from pack definitions
      4. Stores entitlement state
      5. Calls update_mcp_registry(OldTools, NewTools)
  │
  └─→ MCP Registry Update:
      • Add tools: ac_rollback_orchestrator, ac_deployment_validator, ac_storm_detector
      • Remove tools: ac_iam_drift_detector (not in professional tier)
      • Emit receipt: "tool_surface_changed" + details
  │
  └─→ Customer MCP client observes:
      • New tools now visible in tool list
      • Old tools no longer available
      • Can call new tools immediately
  │
  └─→ Customer calls rollback_orchestrator tool:
      1. Check tool registry: FOUND (registered)
      2. Check entitlement: ACTIVE (not expired)
      3. Check IAM: write_rollback? YES
      4. Check preconditions: deployment_stable? YES, rollback_target? YES
      5. EXECUTE: Perform rollback
      6. Emit receipt: action_executed + result
```

---

## Benefits: Why This Works for TAI 2026

### 1) **Fast Shipping** ✅
- One container artifact
- One CI/CD pipeline
- One deployment path
- Features enable/disable via configuration (no rebuild)

### 2) **Long-Tail Discovery** ✅
- Many marketplace listings (same image)
- Customers see many options
- Easy to create new SKU variants (just define new entitlement config)

### 3) **Operational Simplicity** ✅
- One operational surface to monitor
- One set of logs/metrics
- Bug fixes apply to all customers (one rollout)
- No image explosion

### 4) **Security** ✅
- Least-privilege IAM (per-plan)
- Governor gates refuse unsafe actions
- Receipts audit everything
- No escalation without approval

### 5) **Revenue Flexibility** ✅
- Add new plans without code changes
- Upsell via pack enablement
- Downgrade via pack removal
- A/B test pricing on same runtime

---

## Implementation Roadmap

### Phase 1 (Week 5 - Feb 24): Core Infrastructure
- [ ] Define capability pack schema (Erlang maps)
- [ ] Implement `ac_entitlement_resolver.erl`
- [ ] Implement `ac_governor_gates.erl`
- [ ] Modify `erlmcp_server` for dynamic tool registration

### Phase 2 (Week 6 - Mar 3): Pack Library
- [ ] Create 3-5 initial packs (receipt_verifier, rollback_guard, storm_discipline)
- [ ] Define tools and signals per pack
- [ ] Create SKU entitlement configs

### Phase 3 (Week 7 - Mar 10): Marketplace Integration
- [ ] Create multiple marketplace listings (all same image)
- [ ] Set up entitlement webhook receiver
- [ ] Test plan upgrades/downgrades

### Phase 4 (Week 8 - Mar 17): Production Deployment
- [ ] Deploy `taiea-core:1.0.0` to production
- [ ] Activate entitlement resolver
- [ ] Monitor tool surface changes
- [ ] First customer with multiple plans

---

## Conclusion: One Container, Infinite Variants

```
RESULT:

┌─────────────────────────────────────────────────┐
│ ONE CONTAINER: taiea-core:1.0.0                │
│                                                 │
│ All modules built-in, inert until entitlement: │
│ ├─ rollback_guard (disabled by default)        │
│ ├─ storm_discipline (disabled by default)      │
│ ├─ iam_drift_guard (disabled by default)       │
│ ├─ policy_engine (disabled by default)         │
│ ├─ receipt_verifier (always enabled)           │
│ └─ advanced_analytics (disabled by default)    │
└─────────────────────────────────────────────────┘
        │                           │
        ├─ Marketplace Listing 1 ──→ Base Plan ($29/mo)
        │   └─ Entitlement: receipt_verifier only
        │
        ├─ Marketplace Listing 2 ──→ Professional ($299/mo)
        │   └─ Entitlement: +rollback_guard +storm_discipline
        │
        ├─ Marketplace Listing 3 ──→ Enterprise ($2,999/mo)
        │   └─ Entitlement: all packs
        │
        └─ Custom Listing N ────────→ Partner Edition
            └─ Entitlement: {your custom pack mix}

ONE ARTIFACT → INFINITE VARIANTS → DETERMINISTIC SUPPLY CHAIN
```

---

**Generated**: 2026-01-26
**Version**: 1.0.0
**Status**: Production-Ready Design
**Recommendation**: Implement Phase 1 (Week 5), deploy Phase 2 (Week 6), marketplace Phase 3 (Week 7)

This pattern is **the move for TAI 2026**: fast shipping, compliance-ready, and long-tail friendly.
