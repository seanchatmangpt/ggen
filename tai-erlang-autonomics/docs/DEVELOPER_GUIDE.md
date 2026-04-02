# TAI Erlang Autonomics - Developer Guide

**Status**: Phase 1 (Eval-only)
**Last Updated**: 2026-01-26
**Audience**: Development team, contributors

---

## 1. Development Environment Setup

### Prerequisites

- **Erlang/OTP 24+** (25+ recommended, 26 current stable)
- **Rebar3 3.20+** (Erlang build tool)
- **Docker** (for containerized testing)
- **Git** (version control)

### macOS Installation

```bash
# Using Homebrew
brew install erlang
brew install rebar3
brew install docker

# Verify installation
erl -version
rebar3 --version
docker --version
```

### Linux Installation (Ubuntu/Debian)

```bash
# Install Erlang repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
sudo apt-get update

# Install dependencies
sudo apt-get install erlang-dev rebar3 docker.io

# Verify installation
erl -version
rebar3 --version
```

### Windows Installation

Use WSL2 (Windows Subsystem for Linux) and follow Linux instructions, or:

```powershell
# Using Chocolatey
choco install erlang rebar3 docker-desktop

# Verify
erl -version
rebar3 --version
```

### Environment Configuration

Create `.tool-versions` file (asdf compatibility):

```
erlang 26.1.2
rebar 3.22.1
```

Or set shell environment:

```bash
# ~/.bashrc or ~/.zshrc
export ERLANG_HOME="/usr/local/lib/erlang"
export PATH="$ERLANG_HOME/bin:$PATH"
```

---

## 2. Building the Project

### Quick Start

```bash
cd tai-erlang-autonomics

# Compile all applications
rebar3 compile

# Run in development mode
rebar3 shell
```

### Build Profiles

```bash
# Development (default, includes debug info)
rebar3 compile

# Production (optimized, no debug)
rebar3 as prod compile

# Test (includes test dependencies)
rebar3 as test compile
```

### Common Build Errors

| Error | Cause | Solution |
|-------|-------|----------|
| `erl: command not found` | Erlang not installed | Install via Homebrew/package manager |
| `rebar3: command not found` | Rebar3 not installed | Install via Homebrew/package manager |
| `Dependency lock mismatch` | rebar.lock out of sync | Run `rebar3 unlock` then `rebar3 compile` |
| `Port binding failed (8080)` | Port already in use | Kill process: `lsof -ti:8080 \| xargs kill -9` |
| `_build/ stale files` | Build cache corrupted | Run `rebar3 clean && rebar3 compile` |

### Incremental Compilation

```bash
# Watch mode (recompile on file changes)
rebar3 compile --watch

# Rebuild specific app
rebar3 compile --only taiea_core
```

---

## 3. Running Tests

### Unit Tests (Common Test)

```bash
# Run all tests
rebar3 ct

# Run specific test suite
rebar3 ct --suite=tai_governor_SUITE

# Run with verbose output
rebar3 ct --verbose

# Run with coverage report
rebar3 ct --cover --cover_export_json=true
```

### Property-Based Tests

```bash
# Run PropEr tests
rebar3 proper

# With verbose output
rebar3 proper --verbose

# Generate HTML report
rebar3 proper --html_report=/tmp/proper_report.html
```

### Test Coverage Analysis

```bash
# Generate coverage report
rebar3 ct --cover

# View coverage
open _build/test/cover/index.html

# Minimum coverage threshold
rebar3 ct --cover --cover_min_percent=80
```

### Integration Tests

```bash
# Run integration test suite
rebar3 ct --suite=integration_SUITE

# Run performance benchmarks
rebar3 ct --suite=performance_SUITE
```

### Debugging Tests

```bash
# Run with debugger attached
rebar3 ct --debug

# Run single test with trace output
rebar3 ct --trace --suite=tai_governor_SUITE

# Interactive Erlang shell after test failure
rebar3 ct --interactive
```

---

## 4. Development Workflow

### Chicago School TDD (State-Based Testing)

TAI Autonomics follows Chicago School TDD principles:

1. **Arrange**: Set up test state
2. **Act**: Execute behavior
3. **Assert**: Verify state changes

#### Example: Governor State Test

```erlang
%% Test: Governor transitions from boot to active
boot_to_active_test() ->
    %% ARRANGE: Start governor with initial state
    {ok, Pid} = tai_governor:start_link(#{
        tenant_id => <<"tenant_1">>,
        entitlement_id => <<"ent_1">>,
        quota => 100
    }),

    %% ACT: Send activate message
    tai_governor:activate(Pid),

    %% ASSERT: Verify state is now active
    {ok, State} = tai_governor:get_state(Pid),
    ?assertEqual(active, maps:get(status, State)),

    %% Cleanup
    tai_governor:stop(Pid)
.
```

### Code Organization

Each module follows this structure:

```erlang
%% 1. Module declaration and includes
-module(my_module).
-include("headers.hrl").

%% 2. Behavior declaration (if applicable)
-behavior(gen_server).  % or gen_statem, supervisor, etc.

%% 3. Exports
-export([
    start_link/1,
    stop/1,
    get_state/1
]).

%% 4. Callback exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).

%% 5. Type definitions
-type my_state() :: #{
    field1 => term(),
    field2 => integer()
}.

%% 6. Callback record (if using gen_statem)
-record(my_state, {
    field1 :: term(),
    field2 :: integer()
}).

%% 7. Implementation
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% ... rest of implementation
```

### Adding a New Module

1. **Create source file**:
   ```bash
   touch apps/tai_autonomics/src/my_new_module.erl
   ```

2. **Implement with spec**:
   ```erlang
   -module(my_new_module).
   -export([my_function/1]).

   -spec my_function(term()) -> {ok, term()} | {error, term()}.
   my_function(Input) ->
       {ok, Input}.
   ```

3. **Create test suite**:
   ```bash
   touch apps/tai_autonomics/test/my_new_module_SUITE.erl
   ```

4. **Implement tests**:
   ```erlang
   -module(my_new_module_SUITE).
   -include_lib("common_test/include/ct.hrl").

   all() -> [my_function_test].

   my_function_test(_Config) ->
       {ok, result} = my_new_module:my_function(input),
       ok.
   ```

5. **Compile and test**:
   ```bash
   rebar3 compile
   rebar3 ct --suite=my_new_module_SUITE
   ```

---

## 5. Adding New MCP Tools

### Step 1: Define Tool Specification

Create `apps/taiea_mcp/src/tools/my_tool.erl`:

```erlang
-module(my_tool).
-export([spec/0, handle/2]).

%% Tool specification for MCP
-spec spec() -> map().
spec() ->
    #{
        name => <<"my_tool">>,
        description => <<"Description of what this tool does">>,
        input_schema => #{
            type => <<"object">>,
            properties => #{
                <<"param1">> => #{
                    type => <<"string">>,
                    description => <<"First parameter">>
                },
                <<"param2">> => #{
                    type => <<"integer">>,
                    description => <<"Second parameter">>
                }
            },
            required => [<<"param1">>]
        }
    }.

%% Tool implementation
-spec handle(map(), map()) -> {ok, map()} | {error, term()}.
handle(Input, _Context) ->
    Param1 = maps:get(<<"param1">>, Input),
    Param2 = maps:get(<<"param2">>, Input, 0),

    Result = do_work(Param1, Param2),

    {ok, #{
        result => Result,
        status => <<"success">>
    }}.

-spec do_work(binary(), integer()) -> term().
do_work(Param1, Param2) ->
    %% Implementation here
    {Param1, Param2}.
```

### Step 2: Register Tool

Add to `apps/taiea_mcp/src/taiea_mcp_registry.erl`:

```erlang
get_tools() ->
    [
        my_tool,  %% Add your tool module
        other_tool
    ].
```

### Step 3: Create Tests

Create `apps/taiea_mcp/test/my_tool_SUITE.erl`:

```erlang
-module(my_tool_SUITE).
-include_lib("common_test/include/ct.hrl").

all() -> [
    spec_test,
    handle_success_test,
    handle_error_test
].

spec_test(_Config) ->
    Spec = my_tool:spec(),
    ?assertEqual(<<"my_tool">>, maps:get(name, Spec)),
    ?assertMatch(#{type := <<"object">>}, maps:get(input_schema, Spec)).

handle_success_test(_Config) ->
    Input = #{<<"param1">> => <<"value">>},
    {ok, Result} = my_tool:handle(Input, #{}),
    ?assertMatch(#{status := <<"success">>}, Result).

handle_error_test(_Config) ->
    Input = #{},  % Missing required param1
    {error, _} = my_tool:handle(Input, #{}).
```

### Step 4: Build and Test

```bash
rebar3 compile
rebar3 ct --suite=my_tool_SUITE
```

---

## 6. Adding New Gates

Gates are validation checkpoints in the entitlement flow. Add to `apps/tai_autonomics/src/taiea_gates.erl`:

### Step 1: Define Gate Function

```erlang
%% New gate: user_tier_gate
%% Checks if user meets tier requirements
-spec user_tier_gate(map()) -> {ok, map()} | {deny, atom()}.
user_tier_gate(#{tier := <<"premium">>} = State) ->
    {ok, State};
user_tier_gate(#{tier := Tier}) ->
    {deny, {insufficient_tier, Tier}}.
```

### Step 2: Add to Gate Sequence

In `taiea_gates:execute_sequence/1`:

```erlang
execute_sequence(State) ->
    case quota_gate(State) of
        {ok, State1} ->
            case user_tier_gate(State1) of  %% Add new gate
                {ok, State2} ->
                    case compliance_gate(State2) of
                        {ok, Final} -> {ok, Final};
                        {deny, Reason} -> {deny, Reason}
                    end;
                {deny, Reason} -> {deny, Reason}
            end;
        {deny, Reason} -> {deny, Reason}
    end.
```

### Step 3: Test the Gate

```erlang
user_tier_gate_test(_Config) ->
    %% Premium tier should pass
    {ok, _} = taiea_gates:user_tier_gate(#{tier => <<"premium">>}),

    %% Basic tier should fail
    {deny, {insufficient_tier, _}} =
        taiea_gates:user_tier_gate(#{tier => <<"basic">>}),

    ok.
```

---

## 7. Working with Receipts

### Receipt Emission

Receipts are emitted on every state change. Example:

```erlang
emit_receipt(Type, TenantId, Data) ->
    Receipt = #{
        id => gen_receipt_id(),
        type => Type,
        timestamp => system_time_ms(),
        tenant_id => TenantId,
        body => Data,
        previous_hash => get_last_receipt_hash(TenantId),
        signature => sign_receipt(Data)
    },

    ReceiptHash = hash_receipt(Receipt),
    Receipt#{receipt_hash => ReceiptHash}.
```

### Extending Receipt Schema

Add new fields in `apps/tai_autonomics/src/taiea_receipts.erl`:

```erlang
%% Add custom field to receipt
emit_custom_receipt(TenantId, EntitlementId, CustomField) ->
    BaseReceipt = emit_receipt(action_success, TenantId, #{
        entitlement_id => EntitlementId
    }),

    %% Extend with custom data
    BaseReceipt#{
        custom_field => CustomField,
        body => maps:merge(
            maps:get(body, BaseReceipt),
            #{custom_data => CustomField}
        )
    }.
```

### Receipt Verification

```erlang
%% Verify receipt hash chain
verify_hash_chain([]) -> {ok, verified};
verify_hash_chain([Receipt | Rest]) ->
    case verify_receipt_hash(Receipt) of
        ok ->
            case verify_hash_chain(Rest) of
                {ok, verified} -> {ok, verified};
                Error -> Error
            end;
        Error -> Error
    end.
```

---

## 8. Debugging Techniques

### Using Erlang Shell

```bash
# Start interactive shell
rebar3 shell

# In shell, inspect running governors
(tai@localhost)1> gproc:lookup_pids({n, l, {tai_governor, <<"tenant_1">>}}).
[<0.123.0>]

# Get governor state
(tai@localhost)2> sys:get_state(<0.123.0>).
#{status => active, quota => 100, used => 25}

# Send synchronous call
(tai@localhost)3> gen_server:call(<0.123.0>, get_state).
{ok, #{...}}

# Send asynchronous message
(tai@localhost)4> gen_server:cast(<0.123.0>, {activate}).
ok

# Enable tracing
(tai@localhost)5> dbg:tracer().
{ok, <0.456.0>}

(tai@localhost)6> dbg:p(all, call).
{ok, [{"<0.0.0>", [call]}]}

(tai@localhost)7> dbg:tpl(tai_governor, '_', []).
{error,nomatch}
```

### Logging Configuration

Edit `config/sys.config`:

```erlang
{kernel, [
    {logger, [
        {default, {debug}},  % off, critical, error, warning, notice, info, debug
        {handlers, [
            {logger_std_h, primary, #{
                formatter => {logger_formatter, #{
                    template => [time, " [", level, "] ", msg, "\n"]
                }}
            }}
        ]}
    ]}
]}.
```

### Using recon for Runtime Analysis

```bash
# In Erlang shell
(tai@localhost)1> recon:proc_count(memory, 10).
[{<0.1.0>, 1024000},
 {<0.2.0>, 512000},
 ...]

(tai@localhost)2> recon:info(<0.123.0>).
[{type, gen_server},
 {links, [<0.456.0>]},
 ...]

(tai@localhost)3> recon:bin_leak(10).
[{<0.123.0>, 5242880},  % 5MB of binary data
 ...]
```

---

## 9. Performance Optimization

### Memory Profiling

```erlang
%% In test
{ok, _} = memory_logger:start_link(),

%% Run test scenario
test_scenario(),

%% Get report
memory_logger:report().
```

### Benchmarking

```bash
# Run performance test suite
rebar3 ct --suite=performance_SUITE

# Generate benchmark report
rebar3 ct --suite=performance_SUITE --cover
```

### Common Performance Issues

| Issue | Symptom | Solution |
|-------|---------|----------|
| Memory leak | Process size grows | Check for undeferred timers, cycles |
| Slow governor | High latency | Profile with fprof, optimize hot paths |
| CPU spike | High CPU usage | Check for busy loops, excessive logging |
| DB bottleneck | Receipt writes lag | Batch writes, use connection pool |

---

## 10. Code Style Guidelines

### Formatting

```bash
# Format all code
rebar3 format

# Format specific file
rebar3 format --file apps/tai_autonomics/src/my_module.erl
```

### Naming Conventions

- **Atoms**: lowercase_with_underscores
- **Variables**: CapitalizedCamelCase
- **Functions**: lowercase_with_underscores/Arity
- **Records**: lowercase_with_underscores
- **Type specs**: lowercase_with_underscores()

### Type Specifications

All public functions MUST have type specs:

```erlang
%% ✅ GOOD
-spec activate_entitlement(binary(), binary()) -> {ok, map()} | {error, term()}.
activate_entitlement(TenantId, EntitlementId) ->
    %...
.

%% ❌ BAD (missing spec)
activate_entitlement(TenantId, EntitlementId) ->
    %...
.
```

### Documentation Comments

```erlang
%% Public API documentation (EDoc format)
-doc """
Activates an entitlement for a tenant.

This function transitions the entitlement governor from boot to active state,
enabling action execution and receipt emission.

## Parameters
- `TenantId`: Binary tenant identifier
- `EntitlementId`: Binary entitlement identifier

## Returns
- `{ok, State}`: Entitlement activated, returns new state
- `{error, already_active}`: Entitlement already in active state
- `{error, {gate_failed, Reason}}`: Gate validation failed

## Examples
```erlang
{ok, State} = tai_governor:activate(<<"tenant_1">>, <<"ent_1">>)
```
""".
```

---

## 11. Running Release Build

### Development Release

```bash
# Create dev release (symlinked to source)
rebar3 release

# Start development release
_build/default/rel/tai_autonomics/bin/tai_autonomics foreground
```

### Production Release

```bash
# Create production release (optimized, bundled)
rebar3 as prod release

# Package for deployment
tar czf tai-autonomics-1.0.0.tar.gz _build/prod/rel/tai_autonomics

# Start production release
_build/prod/rel/tai_autonomics/bin/tai_autonomics start
_build/prod/rel/tai_autonomics/bin/tai_autonomics status
_build/prod/rel/tai_autonomics/bin/tai_autonomics stop
```

### Release Troubleshooting

| Issue | Solution |
|-------|----------|
| `{exception error, undefined shell_default}` | Ensure proper sys.config |
| Port binding fails on start | Check PORT env var, ensure port available |
| Release won't start | Review config/vm.args and config/sys.config |

---

## 12. Continuous Integration

### GitHub Actions Workflow

See `.github/workflows/ci.yml`:

```yaml
name: CI

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - uses: erlef/setup-beam@v1
        with:
          otp-version: "26"
          rebar3-version: "3.22"

      - run: rebar3 compile
      - run: rebar3 ct
      - run: rebar3 format --check
```

---

## 13. Contributing Guidelines

### Before Submitting PR

1. Ensure all tests pass: `rebar3 ct`
2. Format code: `rebar3 format`
3. Add type specs to all public functions
4. Write tests for new functionality
5. Update docs if adding new features
6. Verify no compiler warnings

### PR Review Checklist

- [ ] Tests passing
- [ ] Code formatted
- [ ] Type specs present
- [ ] Tests added for new code
- [ ] Documentation updated
- [ ] No compiler warnings

---

## 14. Common Errors & Solutions

### Compilation Errors

```
** exception error: bad field in record
```

**Cause**: Typo in record field name
**Solution**: Check record definition and field references

```
undefined function my_module:my_function/1
```

**Cause**: Function not exported or doesn't exist
**Solution**: Add to -export list, check module name

### Runtime Errors

```
{error, eaddrinuse}
```

**Cause**: Port already in use
**Solution**: `lsof -ti:8080 | xargs kill -9` then restart

```
gen_server timeout
```

**Cause**: gen_server:call timeout (default 5000ms)
**Solution**: Increase timeout or optimize handler

### Test Errors

```
Test incomplete: No 'end_per_testcase' found
```

**Cause**: Missing test cleanup
**Solution**: Add end_per_testcase/2 callback

---

## 15. Additional Resources

- **Erlang Documentation**: https://www.erlang.org/docs
- **OTP Design Principles**: https://www.erlang.org/doc/design_principles/
- **Common Test Guide**: https://www.erlang.org/doc/apps/common_test/
- **Cowboy Documentation**: https://ninenines.eu/docs/
- **Project ARCHITECTURE.md**: See docs/ARCHITECTURE.md

---

**Questions?** Open an issue or contact the development team.
