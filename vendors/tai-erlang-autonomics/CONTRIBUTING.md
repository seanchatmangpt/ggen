# Contributing to TAI Erlang Autonomics

**Status**: Phase 1 (Eval-only)
**Last Updated**: 2026-01-26

Thank you for your interest in contributing to TAI Erlang Autonomics! This guide covers development practices, testing expectations, and the review process.

---

## Code Style

### Erlang Conventions

Follow standard Erlang/OTP conventions:

**Module Structure**:
```erlang
-module(my_module).
-include("header.hrl").

%% Behavior declaration
-behavior(gen_server).  % if applicable

%% Public API exports
-export([
    start_link/1,
    stop/1,
    get_state/1
]).

%% Internal callback exports
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).

%% Type definitions
-type my_state() :: #{
    field1 => term(),
    field2 => integer()
}.

%% Records (if used)
-record(my_state, {
    field1 :: term(),
    field2 :: integer()
}).

%% Implementation follows...
```

**Naming**:
- **Functions**: `lowercase_with_underscores/Arity`
- **Variables**: `CapitalizedCamelCase`
- **Atoms**: `lowercase_with_underscores`
- **Records**: `lowercase_with_underscores`
- **Types**: `lowercase_with_underscores()`

**Documentation**:
```erlang
%% Public function documentation (EDoc)
-doc """
Brief description of what this function does.

More detailed explanation if needed, including examples.

## Parameters
- `Param1`: Description
- `Param2`: Description

## Returns
- `{ok, Result}`: Success case
- `{error, Reason}`: Error case

## Examples
```erlang
{ok, State} = my_module:my_function(input)
```
""".

-spec my_function(term()) -> {ok, term()} | {error, term()}.
my_function(Input) ->
    {ok, Input}.
```

### Type Specifications

**ALL public functions MUST have type specs**:

```erlang
%% ✓ GOOD - Spec present
-spec activate_entitlement(binary(), binary()) -> {ok, map()} | {error, term()}.
activate_entitlement(TenantId, EntitlementId) ->
    %...
.

%% ✗ BAD - No spec
activate_entitlement(TenantId, EntitlementId) ->
    %...
.
```

### Formatting

Use `rebar3 format` for automatic formatting:

```bash
# Format all code
rebar3 format

# Format and check without modifying
rebar3 format --check

# Format specific file
rebar3 format --file apps/tai_autonomics/src/my_module.erl
```

**Formatter Rules**:
- Line length: 100 characters
- Indentation: 4 spaces
- Sorted exports and imports

---

## Test Coverage Expectations

### Minimum Coverage: 80%

All new code must maintain ≥80% test coverage.

```bash
# Run tests with coverage report
rebar3 ct --cover

# View coverage
open _build/test/cover/index.html

# Check coverage percentage
rebar3 ct --cover --cover_min_percent=80
```

### Chicago School TDD (State-Based)

Follow AAA pattern (Arrange/Act/Assert):

```erlang
-module(my_feature_SUITE).
-include_lib("common_test/include/ct.hrl").

all() -> [
    my_feature_success_test,
    my_feature_error_test
].

my_feature_success_test(_Config) ->
    %% ARRANGE: Set up initial state
    Input = #{data => "test_value"},

    %% ACT: Execute behavior
    {ok, Result} = my_module:my_feature(Input),

    %% ASSERT: Verify state changed as expected
    ?assertEqual(<<"test_value">>, maps:get(data, Result)),
    ok.

my_feature_error_test(_Config) ->
    %% ARRANGE: Set up error condition
    BadInput = #{},  % Missing required field

    %% ACT: Execute behavior
    {error, missing_data} = my_module:my_feature(BadInput),

    %% ASSERT: Error handled correctly
    ok.
```

### Test Organization

```
apps/tai_autonomics/
├── src/
│   └── my_module.erl          # Implementation
└── test/
    └── my_module_SUITE.erl    # Tests (1:1 mapping)
```

### Test Naming

- Test suite: `{module}_SUITE.erl`
- Test case: `{feature}_test/1` or `{feature}_{case}_test/1`
- Helper: `{purpose}_helper/1`

```erlang
all() -> [
    activation_test,           % Test activation feature
    activation_success_test,   % Test successful activation
    activation_error_test,     % Test activation error
    state_transition_test      % Test state machine
].
```

### Property-Based Testing

Use PropEr for property-based tests:

```erlang
-module(my_props).
-include_lib("proper/include/proper.hrl").

% Property: Activation always moves from boot to active
prop_activation_transition() ->
    ?FORALL(TenantId <- binary(),
            {ok, PrevState} = tai_governor:get_state(Pid),
            PrevState#governor_state.status = boot,
            tai_governor:activate(Pid),
            {ok, NewState} = tai_governor:get_state(Pid),
            NewState#governor_state.status = active).
```

---

## Pull Request Process

### Before Submitting

1. **Run tests**
   ```bash
   rebar3 ct
   ```
   All tests must pass.

2. **Format code**
   ```bash
   rebar3 format
   ```
   No formatting issues.

3. **Check coverage**
   ```bash
   rebar3 ct --cover
   ```
   Must be ≥80%.

4. **Check for warnings**
   ```bash
   rebar3 compile
   ```
   Zero compiler warnings.

5. **Type specs**
   - All public functions have specs
   - No `any()` or untyped parameters

6. **Documentation**
   - New public functions documented
   - Complex logic explained
   - Examples provided where helpful

### PR Description Template

```markdown
## Description
Brief description of changes and why they're needed.

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Refactoring
- [ ] Documentation

## Related Issues
Fixes #(issue number)

## Testing
Describe how you tested this change:
- [ ] Added unit tests
- [ ] Added integration tests
- [ ] Verified coverage ≥80%
- [ ] Manual testing on local dev

## Checklist
- [ ] Tests passing (`rebar3 ct`)
- [ ] Code formatted (`rebar3 format`)
- [ ] No compiler warnings (`rebar3 compile`)
- [ ] Type specs added
- [ ] Documentation updated
- [ ] Coverage ≥80%
```

### Review Checklist

Reviewers will check:

- [ ] Tests passing
- [ ] Code formatted
- [ ] Type specs present
- [ ] Tests added for new code
- [ ] Coverage maintained
- [ ] Documentation updated
- [ ] No compiler warnings
- [ ] Design follows conventions
- [ ] No unsafe operations (unwrap, expect)

---

## Adding New Features

### Step 1: Create Specification

Describe the feature in a GitHub issue:

```
## Feature: [Brief Description]

### Problem
What problem does this solve?

### Solution
How should this be implemented?

### Acceptance Criteria
- [ ] Criterion 1
- [ ] Criterion 2
- [ ] Tests passing
```

### Step 2: Design & Tests First

```erlang
% Write failing test first
-module(new_feature_SUITE).

all() -> [
    new_feature_basic_test,
    new_feature_error_test
].

new_feature_basic_test(_Config) ->
    {ok, result} = new_feature:do_something(input),
    ok.
```

Run test (RED):
```bash
rebar3 ct --suite=new_feature_SUITE
```

### Step 3: Implement Feature

```erlang
-module(new_feature).
-export([do_something/1]).

-spec do_something(term()) -> {ok, term()}.
do_something(Input) ->
    {ok, Input}.
```

Run test (GREEN):
```bash
rebar3 ct --suite=new_feature_SUITE
```

### Step 4: Refactor

```bash
rebar3 format
rebar3 compile
rebar3 ct --cover
```

### Step 5: Submit PR

---

## Release Process (Phase 1)

### Version Numbering

Semantic Versioning: `MAJOR.MINOR.PATCH`

- **MAJOR**: Incompatible API changes
- **MINOR**: New features (backward compatible)
- **PATCH**: Bug fixes (backward compatible)

Example: `1.0.0` → `1.1.0` (new feature) or `1.0.1` (bug fix)

### Cutting a Release

**Phase 1 is eval-only**: Releases are manual.

1. **Update version**
   - `rebar.config`: Increment version in `{release, {tai_autonomics, "1.0.1"}, ...}`
   - `README.md`: Update version reference

2. **Run final tests**
   ```bash
   rebar3 clean
   rebar3 ct
   rebar3 format --check
   ```

3. **Create release notes**
   ```
   ## TAI Erlang Autonomics v1.0.1

   ### Features
   - Add feature 1
   - Add feature 2

   ### Fixes
   - Fix bug 1

   ### Breaking Changes
   None
   ```

4. **Tag and push**
   ```bash
   git tag -a v1.0.1 -m "Release v1.0.1"
   git push origin master --tags
   ```

5. **Build container**
   ```bash
   docker build -f container/Containerfile \
     -t gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.1 .
   docker push gcr.io/${PROJECT_ID}/tai-autonomics:v1.0.1
   ```

---

## Code Review Standards

### What Gets Reviewed

1. **Correctness**: Does it work as intended?
2. **Testing**: Are tests comprehensive?
3. **Performance**: Any performance concerns?
4. **Security**: Any security issues?
5. **Style**: Does it follow conventions?
6. **Maintainability**: Is it easy to understand?

### Common Review Feedback

**Type Specs**:
> Every public function needs a type spec for discoverability and error detection.

**Test Coverage**:
> This feature needs unit tests. Please add tests for both success and error cases.

**Documentation**:
> Can you add an example in the docstring showing how to use this function?

**Performance**:
> This creates a list of all items. For large datasets, consider lazy evaluation.

---

## Conflict Resolution

If you have questions or disagreements about review feedback:

1. **Ask for clarification**: "Can you explain why this pattern is preferred?"
2. **Suggest alternative**: "What about this approach instead?"
3. **Escalate**: If unresolved, tag the lead maintainer

---

## Community Guidelines

### Be Respectful

- Assume good intentions
- Give constructive feedback
- Acknowledge good work
- Welcome new contributors

### Share Knowledge

- Help others learn
- Explain complex code
- Document decisions
- Share resources

---

## Getting Help

- **Questions**: Open GitHub discussion or ask in issue comments
- **Stuck?**: Comment on PR or issue - team will help
- **Code Review**: Ask for specific review feedback
- **Design**: Discuss design before implementing large features

---

## Additional Resources

- **[DEVELOPER_GUIDE.md](docs/DEVELOPER_GUIDE.md)**: Development setup
- **[ARCHITECTURE.md](docs/ARCHITECTURE.md)**: System design
- **Erlang Docs**: https://www.erlang.org/docs
- **OTP Design Principles**: https://www.erlang.org/doc/design_principles/
- **Common Test Guide**: https://www.erlang.org/doc/apps/common_test/

---

Thank you for contributing to TAI Erlang Autonomics!

**Questions?** Open an issue or reach out to the team.
