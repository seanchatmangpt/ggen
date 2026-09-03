# TAI Entitlement Resolver - Interactive Demo

## Quick Start

This demo shows the entitlement resolver in action with 5 test tenants and various state transitions.

## Test Data Overview

### Tenant 1: Base SKU (tenant-001)
```
SKU: base
Tools: [health, support_model]
Roles: [read_only]
Packs: [base]
Expiry: infinity
```

### Tenant 2: Professional SKU (tenant-002)
```
SKU: professional
Tools: [health, support_model, entitlement_apply, receipts_verify]
Roles: [read_only, write_rollback]
Packs: [base, professional]
Expiry: infinity
```

### Tenant 3: Enterprise SKU (tenant-003)
```
SKU: enterprise
Tools: [health, support_model, entitlement_apply, receipts_verify, policy_evaluate, audit_log, custom_integrations]
Roles: [read_only, write_rollback, admin, custom_role]
Packs: [base, professional, enterprise]
Expiry: infinity
```

### Tenant 4: Professional with Expiry (tenant-004)
```
SKU: professional
Tools: [health, support_model, entitlement_apply, receipts_verify]
Roles: [read_only, write_rollback]
Packs: [base, professional]
Expiry: 30 days from creation
```

### Tenant 5: New Signup (tenant-005)
```
SKU: base
Tools: []
Roles: []
Packs: []
Expiry: infinity
```

## Common Operations

### 1. Check Tenant Entitlements

```erlang
%% Get complete entitlement state
{ok, Entitlement} = taiea_entitlement:get_entitlement(<<"tenant-001">>).

%% Get active packs
Packs = taiea_entitlement:get_active_packs(<<"tenant-001">>).
%% Returns: [base]

%% Get enabled tools
Tools = taiea_entitlement:get_enabled_tools(<<"tenant-001">>).
%% Returns: [health, support_model]

%% Verify entitlement is active
taiea_entitlement:verify_entitlement_active(<<"tenant-001">>).
%% Returns: ok
```

### 2. Upgrade Tenant SKU

**Scenario**: Tenant-005 (new signup) wants to upgrade from base to professional.

```erlang
%% Before upgrade
{ok, Before} = taiea_entitlement:get_entitlement(<<"tenant-005">>).
%% sku => base, enabled_tools => []

%% Apply SKU change
{ok, After} = taiea_entitlement:apply_event(
    <<"tenant-005">>,
    {sku_changed, professional}
).

%% After upgrade
taiea_entitlement:get_active_packs(<<"tenant-005">>).
%% Returns: [base, professional]

taiea_entitlement:get_enabled_tools(<<"tenant-005">>).
%% Returns: [health, support_model, entitlement_apply, receipts_verify]

%% Check roles
{ok, Updated} = taiea_entitlement:get_entitlement(<<"tenant-005">>).
maps:get(enabled_iam_roles, Updated).
%% Returns: [read_only, write_rollback]
```

### 3. Enable Feature Pack

**Scenario**: Tenant-001 needs to upgrade to professional pack while staying on base SKU.

```erlang
%% Enable professional pack
{ok, NewState} = taiea_entitlement:apply_event(
    <<"tenant-001">>,
    {pack_enabled, professional}
).

%% Check new packs
Packs = maps:get(enabled_packs, NewState).
%% Returns: [base, professional]

%% Check new tools
Tools = maps:get(enabled_tools, NewState).
%% Contains: [health, support_model, entitlement_apply, receipts_verify]

%% Check new roles
Roles = maps:get(enabled_iam_roles, NewState).
%% Contains: [read_only, write_rollback]
```

### 4. Verify Access to Specific Tool

**Scenario**: Check if tenant can access receipts_verify tool before allowing operation.

```erlang
%% Check tenant-001 (base)
Tools1 = taiea_entitlement:get_enabled_tools(<<"tenant-001">>).
case lists:member(receipts_verify, Tools1) of
    true -> perform_receipt_verification();
    false -> {error, tool_not_available}
end.
%% Result: {error, tool_not_available}

%% Check tenant-002 (professional)
Tools2 = taiea_entitlement:get_enabled_tools(<<"tenant-002">>).
case lists:member(receipts_verify, Tools2) of
    true -> perform_receipt_verification();
    false -> {error, tool_not_available}
end.
%% Result: Success, proceeds to verification
```

### 5. Verify IAM Role

**Scenario**: Check if tenant can rollback transactions (requires write_rollback role).

```erlang
%% Check tenant-001 (base SKU, no write_rollback)
Result1 = taiea_entitlement:verify_iam_role(<<"tenant-001">>, write_rollback).
%% Returns: {error, not_enabled}

%% Check tenant-002 (professional, has write_rollback)
Result2 = taiea_entitlement:verify_iam_role(<<"tenant-002">>, write_rollback).
%% Returns: ok

%% Check tenant-003 (enterprise, has admin)
Result3 = taiea_entitlement:verify_iam_role(<<"tenant-003">>, admin).
%% Returns: ok
```

### 6. Disable Feature Pack

**Scenario**: Remove professional pack from tenant, reverting to base.

```erlang
%% Disable professional pack
{ok, Downgraded} = taiea_entitlement:apply_event(
    <<"tenant-002">>,
    {pack_disabled, professional}
).

%% Check packs
Packs = maps:get(enabled_packs, Downgraded).
%% Returns: [base]

%% Check tools (professional tools removed)
Tools = maps:get(enabled_tools, Downgraded).
%% Returns: [health, support_model]

%% Check roles (professional roles removed)
Roles = maps:get(enabled_iam_roles, Downgraded).
%% Returns: [read_only]
```

### 7. Add Custom IAM Role

**Scenario**: Grant admin role to professional tenant for elevated access.

```erlang
%% Add admin role
{ok, NewState} = taiea_entitlement:apply_event(
    <<"tenant-002">>,
    {iam_role_added, admin}
).

%% Verify role added
taiea_entitlement:verify_iam_role(<<"tenant-002">>, admin).
%% Returns: ok

%% Check all roles
Roles = maps:get(enabled_iam_roles, NewState).
%% Returns: [admin, read_only, write_rollback]
```

### 8. Remove IAM Role

**Scenario**: Revoke admin access from tenant.

```erlang
%% Remove admin role
{ok, Revoked} = taiea_entitlement:apply_event(
    <<"tenant-002">>,
    {iam_role_removed, admin}
).

%% Verify role removed
taiea_entitlement:verify_iam_role(<<"tenant-002">>, admin).
%% Returns: {error, not_enabled}

%% Check remaining roles
Roles = maps:get(enabled_iam_roles, Revoked).
%% Returns: [read_only, write_rollback]
```

## Multi-Step Workflow Example

**Scenario**: New customer onboarding journey

```erlang
%% 1. Tenant starts as base with no features (tenant-005)
{ok, S1} = taiea_entitlement:get_entitlement(<<"tenant-005">>).
%% sku => base, enabled_tools => [], enabled_packs => []

%% 2. Enable base pack
{ok, S2} = taiea_entitlement:apply_event(
    <<"tenant-005">>,
    {pack_enabled, base}
).
%% enabled_tools => [health, support_model]
%% enabled_packs => [base]

%% 3. Verify basic tools available
Tools2 = maps:get(enabled_tools, S2),
true = lists:member(health, Tools2),  %% Passed
false = lists:member(entitlement_apply, Tools2).  %% Still unavailable

%% 4. Upgrade to professional
{ok, S3} = taiea_entitlement:apply_event(
    <<"tenant-005">>,
    {sku_changed, professional}
).
%% sku => professional
%% enabled_tools => [health, support_model, entitlement_apply, receipts_verify]
%% enabled_packs => [base, professional]
%% enabled_iam_roles => [read_only, write_rollback]

%% 5. Verify new tools available
Tools3 = maps:get(enabled_tools, S3),
true = lists:member(entitlement_apply, Tools3),  %% Now available
true = lists:member(receipts_verify, Tools3).

%% 6. Grant custom role for analytics
{ok, S4} = taiea_entitlement:apply_event(
    <<"tenant-005">>,
    {iam_role_added, custom_role}
).

%% Final state
{ok, Final} = taiea_entitlement:get_entitlement(<<"tenant-005">>),
io:format("Final state: ~p~n", [Final]).
```

## Error Handling Examples

### Duplicate Operation
```erlang
%% Try to enable pack that's already enabled
{error, {pack_already_enabled, base}} =
    taiea_entitlement:apply_event(
        <<"tenant-001">>,
        {pack_enabled, base}
    ).
```

### Tenant Not Found
```erlang
%% Try to operate on non-existent tenant
{error, {entitlement_not_found, <<"unknown-tenant">>}} =
    taiea_entitlement:apply_event(
        <<"unknown-tenant">>,
        {pack_enabled, base}
    ).
```

### Invalid State Transition
```erlang
%% Try to disable pack that's not enabled
{error, {pack_not_enabled, enterprise}} =
    taiea_entitlement:apply_event(
        <<"tenant-001">>,
        {pack_disabled, enterprise}
    ).
```

## List All Entitlements

```erlang
%% Get all entitlements in the system
All = taiea_entitlement:list_all_entitlements().

%% Print each tenant's SKU
lists:foreach(fun(E) ->
    TenantId = maps:get(tenant_id, E),
    SKU = maps:get(sku, E),
    io:format("~s: ~w~n", [TenantId, SKU])
end, All).

%% Output:
%% <<"tenant-001">>: base
%% <<"tenant-002">>: professional
%% <<"tenant-003">>: enterprise
%% <<"tenant-004">>: professional
%% <<"tenant-005">>: base
```

## Reinitialize Test Data

```erlang
%% Reset to original test data (5 tenants)
taiea_entitlement:init_test_data().

%% Verify reset
All = taiea_entitlement:list_all_entitlements(),
length(All) = 5.  %% Should be 5 tenants
```

## Receipt System (Audit Trail)

Every operation generates a receipt for compliance:

```erlang
%% Apply event
{ok, _NewState} = taiea_entitlement:apply_event(
    <<"tenant-001">>,
    {pack_enabled, professional}
).

%% Receipt generated internally with:
%% - Unique ID
%% - Timestamp
%% - Tenant ID
%% - Event details
%% - State before transition
%% - State after transition
%% - Reason string

%% In Phase 2: Receipts will be queryable and auditable
```

## Performance Characteristics

### Fast Operations (O(1))
- `get_entitlement/1` - Direct ETS lookup
- `get_active_packs/1` - Direct map access
- `get_enabled_tools/1` - Direct map access
- `verify_entitlement_active/1` - Direct timestamp comparison
- `verify_iam_role/2` - List lookup (small list)
- `apply_event/2` - Single ETS update

### Linear Operations (O(n))
- `list_all_entitlements/0` - Full table scan

### Throughput
- Estimated 10,000+ events/second
- Suitable for real-time authorization checks

## Integration Checklist

Before integrating with other systems:

- [ ] Test basic CRUD operations
- [ ] Test error handling with invalid input
- [ ] Verify receipts are generated
- [ ] Check performance under load
- [ ] Integrate with authorization middleware
- [ ] Set up monitoring for entitlement changes
- [ ] Document entitlement requirements for features
- [ ] Create escalation process for expiry handling

## Next Steps (Future Phases)

### Phase 2: Persistence
- Add Firestore backend for durable storage
- Implement receipt archival
- Add historical queries

### Phase 3: Advanced Features
- Time-based expiry checking
- Complex policy evaluation
- Bulk operations
- Grace period handling

### Phase 4: Analytics
- Entitlement adoption metrics
- SKU upgrade/downgrade trends
- Feature utilization analysis
- Churn prediction

---

**Try it yourself**: Start an Erlang shell and load the module:
```bash
erl -pz /path/to/tai_autonomics/_build/default/lib/tai_autonomics/ebin
```

Then interact with the entitlement system using the examples above.

**Last Updated**: 2026-01-26
