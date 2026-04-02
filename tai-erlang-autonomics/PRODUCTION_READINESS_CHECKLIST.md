# TAI Erlang Autonomics - Production Readiness Checklist

**Status**: NOT YET PRODUCTION READY
**Review Date**: 2026-01-25
**Required Fixes**: 7 Critical + 9 Major items

---

## Pre-Deployment Quality Gates

### PHASE 1: CRITICAL FIXES (MUST COMPLETE)

Each critical fix must be:
- [ ] Code implemented and tested
- [ ] Unit test added/updated
- [ ] Dialyzer warnings resolved
- [ ] Code review approved
- [ ] Documented in CHANGELOG

#### 1. GCP Configuration Loading - CRITICAL

**Issue**: `gcp_config.erl` pattern matching will never succeed
**Impact**: Configuration always defaults, never loads from app environment
**Files Affected**:
- `apps/tai_autonomics/src/gcp_config.erl` (all 5 functions)
- `apps/tai_autonomics/src/gcp_erlang_autonomics_app.erl` (if it uses gcp_config)

**Fix Steps**:
```erlang
% Step 1: Choose approach (recommended: Option A)
% Option A: Use application:get_env/3 with default
% Option B: Handle both bare value and {ok, Value} tuple

% Step 2: Update get_project_id/0
-spec get_project_id() -> string().
get_project_id() ->
    case os:getenv("GCP_PROJECT_ID") of
        false ->
            application:get_env(tai_autonomics, gcp_project_id, "local-dev");
        ProjectId ->
            ProjectId
    end.

% Step 3: Apply same pattern to:
%   - get_region/0
%   - get_zone/0
%   - get_firestore_database_id/0
%   - get_pubsub_subscription/0

% Step 4: Test
%   - Unit test: verify config loads from app env
%   - Unit test: verify env vars override app config
%   - Unit test: verify defaults work
```

**Test Template**:
```erlang
-module(gcp_config_tests).
-include_lib("eunit/include/eunit.hrl").

get_project_id_from_app_env_test() ->
    application:set_env(tai_autonomics, gcp_project_id, "test-project"),
    Result = gcp_config:get_project_id(),
    ?assertEqual("test-project", Result),
    application:unset_env(tai_autonomics, gcp_project_id).

get_project_id_from_os_env_test() ->
    os:putenv("GCP_PROJECT_ID", "os-project"),
    Result = gcp_config:get_project_id(),
    ?assertEqual("os-project", Result),
    os:putenv("GCP_PROJECT_ID", "").

get_project_id_default_test() ->
    application:unset_env(tai_autonomics, gcp_project_id),
    os:putenv("GCP_PROJECT_ID", ""),
    Result = gcp_config:get_project_id(),
    ?assertEqual("local-dev", Result).
```

**Verification**:
- [ ] `rebar3 eunit` passes all gcp_config tests
- [ ] `rebar3 dialyzer` shows 0 errors for gcp_config
- [ ] Manual test: start node, call gcp_config:get_project_id()

---

#### 2. Remove Unknown Function Calls - CRITICAL

**Issue**: Code calls functions that don't exist or aren't available
**Impact**: Runtime crashes when features are used
**Functions**:
- `httpc:request/4` (inets library)
- `dbg:stop_clear/0` (wrong function name)
- `dbg:tracer/2`, `dbg:tpl/3` (may not be available)
- `lists:max_by/2` (doesn't exist)

**Fix 2A: httpc:request/4 - Add inets to rebar.config**

```erlang
% File: rebar.config
{deps, [
    %% HTTP Server
    {cowboy, "2.10.0"},

    %% JSON Parsing
    {jsx, "3.1.0"},

    %% ... other deps ...

    %% HTTP Client (ADD THIS)
    {inets, ".*"}  % Bundled with OTP, but explicit for clarity
]}.

% In sys.config, ensure inets starts:
{kernel, [{applications, [..., inets, ...]}]}.

% Or in your app resource file (tai_autonomics.app.src):
{applications, [kernel, stdlib, sasl, inets]}
```

**Fix 2B: Fix dbg function names - Update trace_handler.erl**

```erlang
% File: apps/tai_autonomics/src/trace_handler.erl

% BEFORE (WRONG):
stop_tracing() ->
    dbg:stop_clear(),  % This function doesn't exist!
    {ok, stopped}.

% AFTER (CORRECT):
stop_tracing() ->
    dbg:stop(),        % Correct OTP function
    dbg:clear(),       % Clear trace patterns separately if needed
    {ok, stopped}.
```

**Fix 2C: Replace lists:max_by/2 - Update tps_tracing_analyzer.erl**

```erlang
% Location: Line 202, 405 in tps_tracing_analyzer.erl

% BEFORE (uses non-existent function):
find_bottleneck_component(ComponentLatencies) ->
    ComponentList = maps:to_list(ComponentLatencies),
    {MaxComponent, _MaxLatency} = lists:max_by(
        fun({_C1, L1}, {_C2, L2}) -> L1 >= L2 end,
        ComponentList
    ),
    MaxComponent.

% AFTER (Option 1 - foldl):
find_bottleneck_component(ComponentLatencies) ->
    ComponentList = maps:to_list(ComponentLatencies),
    case lists:foldl(
        fun({Component, Latency}, {_, MaxLatency} = Max) ->
            case Latency > MaxLatency of
                true -> {Component, Latency};
                false -> Max
            end
        end,
        {undefined, -1},
        ComponentList
    ) of
        {Component, _} -> Component;
        {undefined, _} -> undefined
    end.

% AFTER (Option 2 - sort and take first):
find_bottleneck_component(ComponentLatencies) ->
    case maps:to_list(ComponentLatencies) of
        [] -> undefined;
        ComponentList ->
            Sorted = lists:sort(
                fun({_, L1}, {_, L2}) -> L1 >= L2 end,
                ComponentList
            ),
            {MaxComponent, _} = hd(Sorted),
            MaxComponent
    end.
```

**Verification**:
- [ ] `rebar3 compile` produces 0 errors
- [ ] `rebar3 dialyzer` finds 0 "unknown function" warnings
- [ ] Integration test: HTTP trace export works
- [ ] Integration test: Bottleneck detection works

---

#### 3. Fix ETS Pattern Matching - CRITICAL

**Issue**: `tps_tracing_analyzer.erl` uses ETS patterns in record construction
**Impact**: Invalid terms created, ETS operations fail
**File**: `apps/tai_autonomics/src/tps_tracing_analyzer.erl` (line 258)

```erlang
% BEFORE (WRONG - ETS patterns in record):
create_ets_match_pattern() ->
    #trace_analysis{
        trace_id='_',      % ETS pattern, not valid in record!
        timestamp='$1',
        total_latency='_',
        component_latencies='_',
        waste_breakdown='_',
        error_count='_',
        is_anomaly='_'
    }.

% AFTER (CORRECT - Use ETS match specs):
% Option 1: Use ets:match/2 with tuple pattern
query_traces(Table) ->
    Pattern = {trace_analysis, '_', '$1', '_', '_', '_', '_', '_'},
    ets:match(Table, Pattern).

% Option 2: Use helper for record conversion
analysis_to_tuple(Analysis) ->
    {
        trace_analysis,
        Analysis#trace_analysis.trace_id,
        Analysis#trace_analysis.timestamp,
        Analysis#trace_analysis.total_latency,
        Analysis#trace_analysis.component_latencies,
        Analysis#trace_analysis.waste_breakdown,
        Analysis#trace_analysis.error_count,
        Analysis#trace_analysis.is_anomaly
    }.

% Option 3: Switch to record storage (easier)
store_analysis(Analysis, Table) ->
    ets:insert(Table, {analysis, Analysis}),  % Store record directly
    ok.

lookup_analysis(Table, TraceId) ->
    case ets:match_object(Table, {analysis, #trace_analysis{trace_id=TraceId, _='_'}}) of
        [{analysis, Result}] -> {ok, Result};
        [] -> {error, not_found}
    end.
```

**Verification**:
- [ ] `rebar3 dialyzer` shows 0 errors for ETS patterns
- [ ] Unit test: verify ETS operations work
- [ ] Unit test: verify pattern matching finds records

---

#### 4. Fix Governor start_link Specs - CRITICAL

**Issue**: `start_link` specs don't include `ignore` return value
**Impact**: Calling code may not handle all return values, causing crashes
**Files**:
- `apps/tai_autonomics/src/tai_governor.erl`
- `apps/tai_autonomics/src/billing_governor.erl`
- `apps/tai_autonomics/src/compliance_audit_governor.erl`
- `apps/tai_autonomics/src/customer_account_governor.erl`
- `apps/tai_autonomics/src/entitlement_governor.erl`
- `apps/tai_autonomics/src/multi_tenant_governance.erl`

```erlang
% BEFORE:
-spec start_link(TenantId, GovernorId) -> {ok, pid()} | {error, term()}
  when TenantId :: binary(),
       GovernorId :: binary().

% AFTER (add | ignore):
-spec start_link(TenantId, GovernorId) ->
    {ok, pid()} | {error, term()} | ignore
  when TenantId :: binary(),
       GovernorId :: binary().

% Update calling code to handle ignore:
case tai_governor:start_link(TenantId, GovernorId) of
    {ok, Pid} -> {ok, Pid};
    ignore -> {error, governor_init_ignored};
    {error, Reason} -> {error, Reason}
end.
```

**Verification**:
- [ ] All 6 governor modules updated
- [ ] `rebar3 dialyzer` shows 0 start_link spec errors
- [ ] Code that calls start_link handles ignore case

---

#### 5. Fix Application start Specs - CRITICAL

**Issue**: Application start/2 specs don't include `ignore` return
**Impact**: Application won't start if supervisor returns ignore
**Files**:
- `apps/tai_autonomics/src/gcp_erlang_autonomics_app.erl`
- `apps/tai_autonomics/src/observability_app.erl`
- `apps/tai_autonomics/src/autonomics_example.erl`

```erlang
% BEFORE:
-spec start(StartType, StartArgs) -> {ok, pid()} | {error, term()}.

% AFTER (add | ignore):
-spec start(StartType, StartArgs) ->
    {ok, pid()} | {ok, pid(), term()} | {error, term()} | ignore.

% Application module contract allows:
% - {ok, Pid}
% - {ok, Pid, State}  (if supervisor returns application state)
% - {error, Reason}
% - ignore            (if supervisor init fails gracefully)
```

**Verification**:
- [ ] All 3 application modules updated
- [ ] `rebar3 dialyzer` shows 0 start/2 spec errors

---

### PHASE 2: MAJOR FIXES (THIS SPRINT)

#### 6. Fix Supervisor init/1 Specs

**Severity**: MAJOR
**Files**: 11 supervisor modules
**Issue**: Specs claim `ignore` is possible return but it never is

```erlang
% BEFORE (all supervisors):
-spec init(Args) -> {ok, {sup_flags(), [child_spec()]}} | ignore.

% AFTER (correct - supervisors never return ignore):
-spec init(Args) -> {ok, {sup_flags(), [child_spec()]}}.
```

**List of files**:
- `autonomics_sup.erl`
- `billing_sup.erl`
- `cluster_sup.erl`
- `compliance_audit_sup.erl`
- `customer_account_sup.erl`
- `entitlement_sup.erl`
- `governance_sup.erl`
- `multi_tenant_sup.erl`
- Plus any others (check for `-behaviour(supervisor)`)

**Bash Script to Find**:
```bash
grep -r "init(Args) ->" apps/tai_autonomics/src/*sup*.erl | grep "ignore"
```

---

#### 7. Fix gen_server Callback Specs

**Severity**: MAJOR
**Files**: 12+ gen_server modules
**Issue**: `handle_call`, `handle_cast`, `handle_info` specs too permissive

**Pattern**: Create message type definition first

```erlang
% Define message types
-type alert_manager_call() ::
    stop |
    check_thresholds |
    get_active_incidents |
    {add_alert_rule, atom(), alert_rule()} |
    {remove_alert_rule, atom()}.

-type alert_manager_cast() ::
    {interval, pos_integer()}.

-type alert_manager_info() ::
    check_interval_timer.

% Then use in callback specs
-spec handle_call(Request, From, State) ->
    {'reply', Reply, State}
  when
    Request :: alert_manager_call(),
    From :: {pid(), term()},
    Reply :: ok | {ok, [any()]} | {error, unknown_request},
    State :: #state{}.
```

---

#### 8. Verify HTTP Handler Integration

**Severity**: MAJOR
**File**: `apps/tai_autonomics/src/tai_http.erl`
**Issue**: HTTP handlers may not be properly registered with Cowboy

**Checklist**:
- [ ] `tai_http.erl` properly initializes Cowboy routes
- [ ] HTTP handler functions match Cowboy expectations
- [ ] Content-type headers are correct
- [ ] CORS headers (if needed) are present
- [ ] Error responses follow API specification

---

#### 9. Code Review of gen_statem Usage

**Severity**: MAJOR
**Issue**: Governor modules use `gen_server` instead of `gen_statem` for state machines
**Discussion**: This is a design choice, but should be documented:

```erlang
% Considerations:
% 1. gen_server: simpler, familiar, state implicit in callback
% 2. gen_statem: formal state machine, prevents invalid transitions, better visualization

% If staying with gen_server, add state validation:
boot({call, From}, {signal, Signal}, State) ->
    case check_entitlement(State#state.tenant_id) of
        {ok, active} ->
            NewState = State#state{current_state = stable},
            {next_state, stable, NewState, ...};
        {ok, inactive} ->
            {keep_state, State, [{reply, From, {error, entitlement_inactive}}]}
    end.
```

---

### PHASE 3: QUALITY POLISH (OPTIONAL)

#### 10. Tighten Type Specifications

**Scope**: All 57 modules
**Priority**: After critical/major fixes
**Examples**:
- Replace `Reason :: term()` with specific error types
- Replace `any()` with union types
- Add type aliases for common patterns

#### 11. Remove Unreachable Code

**Scope**: Identified during dialyzer analysis
**Examples**:
- `trace_handler.erl` (lines 297, 309) - unused functions
- Any other detected unreachable patterns

#### 12. Add Missing Function Specs

**Scope**: Test modules and internal functions
**Files**:
- `governor_perf_bench_SUITE.erl`
- Any other modules with `warn_missing_spec` warnings

---

## Compiler Configuration Improvements

### Current rebar.config Issues

```erlang
% CURRENT - ISSUES:
{erl_opts, [
    debug_info,
    warn_missing_spec,        % Warning only
    warn_unused_vars          % Warning only
]}.

{dialyzer, [
    {warnings, [underspecs, overspecs, specdiffs]},
    {get_warnings, true}
]}.
```

### Recommended Configuration

```erlang
% RECOMMENDED:
{erl_opts, [
    debug_info,
    warn_missing_spec,
    warn_unused_vars,
    warn_untyped_record,      % ADD: Catch type errors earlier
    warn_bif_clash,           % ADD: Prevent BIF shadowing
    warn_deprecated_function  % ADD: Track deprecations
]}.

{dialyzer, [
    {warnings, [
        underspecs,           % Function implementation more general than spec
        overspecs,           % Function implementation more restrictive than spec
        specdiffs,           % Spec different from success typing
        unknown,             % Unknown functions/types
        error_handling       % ADD: Missing error handling
    ]},
    {plt_apps, [erts, kernel, stdlib, sasl, cowboy, jsx, jose, inets]},
    {get_warnings, true}
]}.

% For production builds, enable as errors:
{profiles, [
    {prod, [
        {erl_opts, [
            debug_info,
            warn_missing_spec,
            warn_unused_vars,
            warn_untyped_record,
            {warnings_as_errors, true}  % ENFORCE: No warnings in production
        ]}
    ]}
]}.
```

---

## Testing Requirements

### Unit Tests (Minimum Coverage)

- [ ] All CRITICAL modules have unit tests
- [ ] Test coverage >= 80% for fixed modules
- [ ] All edge cases tested for gcp_config
- [ ] All HTTP handler responses tested

### Integration Tests

- [ ] GCP configuration loading (dev + prod modes)
- [ ] HTTP endpoints return correct status codes
- [ ] Governor state transitions complete without errors
- [ ] Trace export to Jaeger works
- [ ] Bottleneck detection algorithm correct

### Manual Tests

- [ ] Start application with dev config
- [ ] Start application with prod config
- [ ] Call each HTTP endpoint
- [ ] Trigger trace export
- [ ] Verify metrics collection

---

## Deployment Checklist

Before deploying to production:

### Code Quality
- [ ] All CRITICAL fixes implemented
- [ ] `rebar3 compile` shows 0 errors
- [ ] `rebar3 dialyzer` shows 0 critical warnings
- [ ] `rebar3 eunit` shows all tests passing
- [ ] Code review approved by 2+ reviewers

### Configuration
- [ ] Prod config documented in README
- [ ] Environment variables documented
- [ ] All required OTP apps listed (inets, etc.)
- [ ] Sys.config has all settings
- [ ] VM args configured for production load

### Documentation
- [ ] CHANGELOG updated with all fixes
- [ ] API documentation current
- [ ] Deployment guide updated
- [ ] Troubleshooting guide includes known issues
- [ ] HTTP endpoints documented with examples

### Testing
- [ ] Unit tests: 80%+ coverage
- [ ] Integration tests: All critical paths tested
- [ ] Load tests: Response times acceptable
- [ ] Failure injection: System recovers properly
- [ ] Manual smoke tests: All features working

### Monitoring & Alerting
- [ ] Metrics collection verified
- [ ] Logs configurable (Cloud Logging)
- [ ] Traces exported to Jaeger
- [ ] Alerts defined for SLAs
- [ ] Dashboards created

### Release
- [ ] Release notes prepared
- [ ] Rollback plan documented
- [ ] Deployment procedure tested
- [ ] Team trained on operation
- [ ] On-call support assigned

---

## Success Criteria

**MUST HAVE**:
- ✅ All 7 CRITICAL issues resolved
- ✅ Zero compilation errors
- ✅ Zero "unknown function" warnings in dialyzer
- ✅ All HTTP endpoints tested and working
- ✅ Configuration loading verified (prod mode)
- ✅ Trace export working
- ✅ Code review approved

**SHOULD HAVE**:
- ✅ All 9 MAJOR issues resolved
- ✅ 80%+ test coverage
- ✅ Zero dialyzer warnings (or documented exceptions)
- ✅ Updated documentation
- ✅ Load test results documented

**NICE TO HAVE**:
- ✅ All type specs optimized
- ✅ Dead code removed
- ✅ Full property-based test suite
- ✅ Performance benchmarks

---

## Current Status

| Item | Status | Owner | ETA |
|------|--------|-------|-----|
| CRITICAL Fixes | NOT STARTED | - | - |
| MAJOR Fixes | NOT STARTED | - | - |
| Quality Checks | NOT STARTED | - | - |
| Documentation | IN PROGRESS | - | - |
| Testing | NOT STARTED | - | - |
| Deployment | NOT READY | - | - |

**Overall**: NOT READY FOR PRODUCTION

---

## References

- Code Quality Analysis: `CODE_QUALITY_ANALYSIS.md`
- Detailed Warnings: `DIALYZER_WARNINGS_DETAILED.md`
- RFCs:
  - RFC-001: Governor state machine design (gen_server vs gen_statem)
  - RFC-002: Error type standardization
  - RFC-003: HTTP handler architecture

