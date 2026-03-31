# TAI Entitlement Resolver (taiea_entitlement)

## Overview

The TAI Entitlement Resolver is a robust Erlang gen_server that manages per-tenant entitlement state tracking and application. It enforces SKU-based feature and tool availability, tracks IAM role access, and generates cryptographic receipts for all state transitions.

**Module**: `taiea_entitlement.erl`
**Supervisor**: `taiea_entitlement_sup.erl`
**Tests**: `test/taiea_entitlement_SUITE.erl`

## Entitlement Model

### SKU Levels

The system defines three SKU tiers with increasing feature availability:

#### base
- **Tools**: health, support_model
- **Roles**: read_only
- **Packs**: base
- **Use Case**: Minimal feature set, read-only access

#### professional
- **Tools**: health, support_model, entitlement_apply, receipts_verify
- **Roles**: read_only, write_rollback
- **Packs**: base, professional
- **Use Case**: Core functionality with entitlement management and receipt verification

#### enterprise
- **Tools**: health, support_model, entitlement_apply, receipts_verify, policy_evaluate, audit_log, custom_integrations
- **Roles**: read_only, write_rollback, admin, custom_role
- **Packs**: base, professional, enterprise
- **Use Case**: Full platform access with policy evaluation, audit logging, and custom integrations

### Entitlement State Structure

Each tenant maintains an entitlement state map with the following fields:

```erlang
#{
    tenant_id => binary(),                    % Unique tenant identifier
    sku => base | professional | enterprise, % Current SKU level
    enabled_packs => [pack_name()],          % List of enabled feature packs
    enabled_tools => [tool_name()],          % List of accessible tools
    enabled_iam_roles => [iam_role()],       % List of assigned IAM roles
    expiry => timestamp() | infinity,        % Expiration timestamp (ms)
    created_at => timestamp(),               % Creation timestamp
    updated_at => timestamp()                % Last update timestamp
}
```

## Events

The system supports five event types for entitlement transitions:

### sku_changed
Upgrade or downgrade to a different SKU tier.

```erlang
{sku_changed, NewSku}

%% Example
taiea_entitlement:apply_event(<<"tenant-001">>, {sku_changed, professional})
```

**Effect**:
- Updates SKU level
- Resets enabled_packs to default for new SKU
- Updates enabled_tools to match new SKU defaults
- Updates enabled_iam_roles to match new SKU defaults

### pack_enabled
Enable a specific feature pack for the tenant.

```erlang
{pack_enabled, PackName}

%% Example
taiea_entitlement:apply_event(<<"tenant-001">>, {pack_enabled, professional})
```

**Effect**:
- Adds pack to enabled_packs
- Includes all tools associated with the pack
- Includes all roles associated with the pack
- Fails if pack already enabled

### pack_disabled
Disable a specific feature pack.

```erlang
{pack_disabled, PackName}

%% Example
taiea_entitlement:apply_event(<<"tenant-001">>, {pack_disabled, professional})
```

**Effect**:
- Removes pack from enabled_packs
- Removes tools associated only with this pack
- Removes roles associated only with this pack
- Fails if pack not enabled

### iam_role_added
Grant an additional IAM role to the tenant.

```erlang
{iam_role_added, RoleName}

%% Example
taiea_entitlement:apply_event(<<"tenant-001">>, {iam_role_added, custom_role})
```

**Effect**:
- Adds role to enabled_iam_roles
- Fails if role already enabled

### iam_role_removed
Revoke an IAM role from the tenant.

```erlang
{iam_role_removed, RoleName}

%% Example
taiea_entitlement:apply_event(<<"tenant-001">>, {iam_role_removed, admin})
```

**Effect**:
- Removes role from enabled_iam_roles
- Fails if role not enabled

## API Reference

### apply_event/2
Apply an entitlement event to transition a tenant's entitlement state.

```erlang
-spec apply_event(tenant_id(), event()) ->
    {ok, entitlement_state()} | {error, term()}.

apply_event(<<"tenant-001">>, {sku_changed, professional})
%% Returns:
%% {ok, #{tenant_id => <<"tenant-001">>, sku => professional, ...}}
```

**Error Cases**:
- `{error, {entitlement_not_found, TenantId}}` - Tenant not found
- `{error, {pack_already_enabled, PackName}}` - Pack already enabled
- `{error, {pack_not_enabled, PackName}}` - Pack not currently enabled
- `{error, {role_already_enabled, RoleName}}` - Role already enabled
- `{error, {role_not_enabled, RoleName}}` - Role not currently enabled
- `{error, {invalid_event, Event}}` - Unknown event type

### get_active_packs/1
Retrieve the list of enabled packs for a tenant.

```erlang
-spec get_active_packs(tenant_id()) -> [pack_name()].

get_active_packs(<<"tenant-001">>)
%% Returns: [base]
```

**Returns**: Empty list if tenant not found.

### get_enabled_tools/1
Retrieve the list of enabled tools for a tenant.

```erlang
-spec get_enabled_tools(tenant_id()) -> [tool_name()].

get_enabled_tools(<<"tenant-002">>)
%% Returns: [health, support_model, entitlement_apply, receipts_verify]
```

**Returns**: Empty list if tenant not found.

### verify_entitlement_active/1
Verify that a tenant's entitlement is currently active and not expired.

```erlang
-spec verify_entitlement_active(tenant_id()) -> ok | {error, inactive}.

verify_entitlement_active(<<"tenant-001">>)
%% Returns: ok | {error, inactive}
```

**Returns**:
- `ok` - Entitlement is active (not expired)
- `{error, inactive}` - Entitlement expired or not found

### verify_iam_role/2
Verify that a tenant has a specific IAM role enabled.

```erlang
-spec verify_iam_role(tenant_id(), iam_role()) -> ok | {error, not_enabled}.

verify_iam_role(<<"tenant-002">>, write_rollback)
%% Returns: ok | {error, not_enabled}
```

**Returns**:
- `ok` - Role is enabled for this tenant
- `{error, not_enabled}` - Role not enabled or tenant not found

### get_entitlement/1
Retrieve the complete entitlement state for a tenant.

```erlang
-spec get_entitlement(tenant_id()) -> {ok, entitlement_state()} | {error, not_found}.

get_entitlement(<<"tenant-001">>)
%% Returns:
%% {ok, #{tenant_id => <<"tenant-001">>, sku => base, enabled_tools => [health, support_model], ...}}
```

### list_all_entitlements/0
Retrieve all entitlements in the system.

```erlang
-spec list_all_entitlements() -> [entitlement_state()].

list_all_entitlements()
%% Returns: [#{...}, #{...}, ...]
```

### init_test_data/0
Initialize or reinitialize the test data (5 sample tenants).

```erlang
-spec init_test_data() -> ok.

init_test_data()
```

## Test Data

The system initializes with 5 sample tenants for demonstration:

### tenant-001 (base)
- **SKU**: base
- **Tools**: health, support_model
- **Roles**: read_only
- **Packs**: base
- **Expiry**: infinity

### tenant-002 (professional)
- **SKU**: professional
- **Tools**: health, support_model, entitlement_apply, receipts_verify
- **Roles**: read_only, write_rollback
- **Packs**: base, professional
- **Expiry**: infinity

### tenant-003 (enterprise)
- **SKU**: enterprise
- **Tools**: health, support_model, entitlement_apply, receipts_verify, policy_evaluate, audit_log, custom_integrations
- **Roles**: read_only, write_rollback, admin, custom_role
- **Packs**: base, professional, enterprise
- **Expiry**: infinity

### tenant-004 (professional with expiry)
- **SKU**: professional
- **Tools**: health, support_model, entitlement_apply, receipts_verify
- **Roles**: read_only, write_rollback
- **Packs**: base, professional
- **Expiry**: 30 days from creation (timestamp() + 2,592,000,000 ms)

### tenant-005 (new signup)
- **SKU**: base
- **Tools**: [] (empty)
- **Roles**: [] (empty)
- **Packs**: [] (empty)
- **Expiry**: infinity

## Receipts

Every entitlement event generates a cryptographic receipt for audit and compliance:

```erlang
#{
    id => binary(),                  % Unique receipt ID (base64-encoded)
    timestamp => timestamp(),        % Event timestamp (milliseconds)
    tenant_id => binary(),           % Affected tenant
    event => event(),                % Event type and data
    state_before => entitlement_state(), % State before transition
    state_after => entitlement_state(),  % State after transition
    reason => binary()               % Human-readable reason
}
```

Receipts are stored in the `taiea_entitlement_receipts` ETS table and can be retrieved for audit, compliance, or forensic analysis.

## Storage

**Phase 1 (Current)**: Memory-based ETS tables
- `taiea_entitlements` - Entitlement state per tenant
- `taiea_entitlement_receipts` - Audit trail of all events

**Phase 2 (Future)**: Firestore persistence
- Migrate to Google Firestore for durable storage
- Implement backup and recovery

**Phase 3 (Future)**: Policy evaluation
- Complex entitlement rules based on customer profile
- Dynamic feature availability based on usage patterns

## Usage Examples

### Retrieve base SKU tenant info
```erlang
{ok, Ent} = taiea_entitlement:get_entitlement(<<"tenant-001">>),
Tools = maps:get(enabled_tools, Ent),
%% Tools = [health, support_model]
```

### Upgrade tenant to professional
```erlang
{ok, NewEnt} = taiea_entitlement:apply_event(
    <<"tenant-005">>,
    {sku_changed, professional}
),
taiea_entitlement:verify_iam_role(<<"tenant-005">>, write_rollback)
%% Returns: ok
```

### Enable additional IAM role
```erlang
{ok, UpdatedEnt} = taiea_entitlement:apply_event(
    <<"tenant-001">>,
    {iam_role_added, admin}
),
```

### Verify entitlement before operation
```erlang
case taiea_entitlement:verify_entitlement_active(<<"tenant-004">>) of
    ok ->
        %% Proceed with operation
        perform_operation();
    {error, inactive} ->
        %% Reject operation, return error to client
        {error, entitlement_expired}
end
```

### Check if tenant can access specific tool
```erlang
Tools = taiea_entitlement:get_enabled_tools(<<"tenant-002">>),
case lists:member(receipts_verify, Tools) of
    true ->
        %% Tenant can verify receipts
        verify_receipt();
    false ->
        %% Tenant cannot access this tool
        {error, tool_not_available}
end
```

## Error Handling

All functions return consistent error patterns:

- **Resource not found**: `{error, {resource_not_found, Details}}`
- **State conflict**: `{error, {state_conflict, Details}}`
- **Invalid input**: `{error, {invalid_event, Event}}`
- **Active/Inactive states**: `ok | {error, inactive}` or `ok | {error, not_enabled}`

## Integration Points

### TAI Governor
The entitlement resolver integrates with the TAI Governor as part of the broader autonomic system:
- Provides entitlement context for authorization decisions
- Supplies feature availability for feature gates

### Compliance & Audit
- All state transitions generate receipts
- Receipts stored for audit trail (Phase 2: Firestore)
- Receipt IDs include cryptographic hashing

### Monitoring & Observability
- ETS tables expose real-time entitlement metrics
- Receipt table provides audit trail
- Future: OpenTelemetry spans for state transitions

## Performance Characteristics

### Time Complexity
- `apply_event/2`: O(1) - Direct ETS lookup and update
- `get_*` functions: O(1) - Direct ETS lookup
- `list_all_entitlements/0`: O(n) - Scan all entries

### Space Complexity
- Per tenant: ~1KB (entitlement state)
- Per event: ~2KB (receipt)
- ETS tables: In-memory, configurable eviction policies

### Throughput
- Single gen_server handles all tenant operations
- Lock-free ETS writes with `{write_concurrency, true}`
- Estimated: 10,000+ events/second on standard hardware

## Testing

Comprehensive test suite with 28 test cases:

```bash
# Run all tests
rebar3 ct --suite=test/taiea_entitlement_SUITE

# Run specific test
rebar3 ct --suite=test/taiea_entitlement_SUITE --case test_apply_event_sku_changed
```

### Test Coverage Areas
- SKU-based entitlements (base, professional, enterprise)
- Event application and transitions
- Tool and role verification
- Error cases and boundary conditions
- Multi-step event sequences
- Entitlement expiry
- Receipt generation
- Metadata tracking

## Future Enhancements

### Phase 2: Persistence
- Firestore integration for durable storage
- Automatic receipt archival
- Historical analysis and compliance reporting

### Phase 3: Advanced Features
- Time-based expiry checking and notifications
- Complex policy evaluation (usage-based, cohort-based)
- Dynamic feature availability
- Entitlement grace periods
- Bulk operations (multi-tenant updates)

### Phase 4: Analytics
- Entitlement adoption metrics
- SKU upgrade/downgrade patterns
- Feature utilization analysis
- Churn prediction based on entitlement changes

## Debugging

### Inspect entitlement state
```erlang
TenantId = <<"tenant-001">>,
{ok, State} = taiea_entitlement:get_entitlement(TenantId),
io:format("~p~n", [State]).
```

### List all entitlements
```erlang
All = taiea_entitlement:list_all_entitlements(),
lists:foreach(fun(E) -> io:format("~p~n", [E]) end, All).
```

### Reinitialize test data
```erlang
taiea_entitlement:init_test_data().
```

### Direct ETS inspection (advanced)
```erlang
%% List all entitlements
ets:match_object(taiea_entitlements, {'_', '$1'}).

%% List all receipts
ets:match_object(taiea_entitlement_receipts, {'_', '$1'}).

%% Count receipts
ets:info(taiea_entitlement_receipts, size).
```

## References

- **Module**: `/apps/tai_autonomics/src/taiea_entitlement.erl`
- **Supervisor**: `/apps/tai_autonomics/src/taiea_entitlement_sup.erl`
- **Tests**: `/test/taiea_entitlement_SUITE.erl`
- **Related**: TAI Governor, Entitlement Governor, Product Catalog Governor

---

**Last Updated**: 2026-01-26
**Version**: 1.0.0 (Phase 1: Memory-based storage)
