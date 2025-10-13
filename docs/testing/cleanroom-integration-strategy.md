# Cleanroom Integration Testing Strategy for Ggen CLI

## Overview

This document outlines the comprehensive testing strategy for the ggen CLI using the cleanroom deterministic testing framework. The goal is to achieve production confidence for the v1 release through isolated, reproducible integration tests.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│              Ggen CLI Integration Tests                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐ │
│  │  Cleanroom   │  │  Isolation   │  │  Validation  │ │
│  │  Framework   │  │   Layer      │  │   Layer      │ │
│  └──────────────┘  └──────────────┘  └──────────────┘ │
│  ┌──────────────────────────────────────────────────┐  │
│  │           Test Execution Environment             │  │
│  │  • Isolated temp directories                     │  │
│  │  • Controlled environment variables              │  │
│  │  • Deterministic output validation               │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

## Key Components

### 1. CleanroomCliTestEnvironment

The core test harness that provides:
- **Isolation**: Each test runs in a separate temp directory
- **Repeatability**: Deterministic environment setup and teardown
- **Validation**: Comprehensive output and exit code checking

```rust
pub struct CleanroomCliTestEnvironment {
    cleanroom: CleanroomEnvironment,
    temp_dir: TempDir,
    ggen_binary: PathBuf,
}
```

### 2. Helper Functions

#### `run_ggen_command(args: &[&str]) -> Result<Output>`
Executes ggen commands in isolated environment with:
- Controlled working directory
- Isolated GGEN_HOME and cache directories
- Captured stdout/stderr
- Exit code tracking

#### `assert_ggen_success(output: &Output)`
Validates successful command execution:
- Checks exit code is 0
- Provides detailed failure diagnostics
- Shows stdout/stderr on failure

#### `assert_ggen_failure(output: &Output)`
Validates expected command failures:
- Checks exit code is non-zero
- Verifies error messages present
- Provides failure context

#### `assert_output_contains(output: &Output, expected: &str)`
Validates output content:
- Checks both stdout and stderr
- Case-sensitive matching
- Detailed failure messages

#### `assert_ggen_marketplace_works(env: &CleanroomCliTestEnvironment) -> Result<()>`
Comprehensive marketplace validation:
- Tests search functionality
- Validates result formatting
- Checks connectivity

### 3. Test Categories

#### Basic Functionality Tests
- `test_ggen_version`: Version information
- `test_ggen_help`: Help system
- `test_ggen_invalid_command`: Error handling

#### Marketplace Tests
- `test_ggen_market_search`: Basic search
- `test_ggen_market_list`: Package listing
- `test_ggen_market_search_with_filters`: Advanced search
- `test_ggen_market_search_json_output`: JSON output format
- `test_ggen_market_search_no_results`: Empty result handling

#### Lifecycle Tests
- `test_ggen_lifecycle_init`: Project initialization
- `test_ggen_lifecycle_list`: Stage listing
- Planned: test, build, deploy stages

#### Template Tests
- `test_ggen_template_help`: Template help
- Planned: template generation, validation

#### Integration Tests
- `test_ggen_multiple_commands_sequentially`: Command sequences
- `test_ggen_environment_isolation`: Isolation verification
- `test_ggen_deterministic_output`: Reproducibility

#### Error Handling Tests
- `test_ggen_error_handling_missing_args`: Missing arguments
- `test_ggen_invalid_command`: Invalid commands

#### Performance Tests
- `test_ggen_command_performance`: Execution time validation

## Test Execution Flow

```
1. Setup Phase
   ├── Create CleanroomEnvironment
   ├── Create isolated temp directory
   ├── Locate ggen binary
   └── Set environment variables

2. Execution Phase
   ├── Run ggen command with args
   ├── Capture stdout/stderr
   ├── Record exit code
   └── Measure execution time

3. Validation Phase
   ├── Check exit code
   ├── Validate output content
   ├── Verify expected behavior
   └── Check side effects

4. Cleanup Phase
   ├── Close cleanroom environment
   ├── Remove temp directory
   └── Release resources
```

## Error Handling Patterns

### NO .unwrap() or .expect()
All error handling uses proper `Result<T>` returns:

```rust
// ❌ BAD
let output = command.output().unwrap();

// ✅ GOOD
let output = command.output()
    .map_err(|e| anyhow::anyhow!("Failed to execute command: {}", e))?;
```

### Comprehensive Error Context

```rust
// ❌ BAD
let dir = env.temp_dir().parent().unwrap();

// ✅ GOOD
let dir = env.temp_dir().parent()
    .ok_or_else(|| anyhow::anyhow!("Failed to get parent directory"))?;
```

## Memory Coordination Integration

Tests integrate with the Hive Mind memory system:

```bash
# Store test results
npx claude-flow@alpha hooks post-edit \
  --file "tests/cli_integration_cleanroom.rs" \
  --memory-key "hive/code/ggen-tests"

# Store test metadata
npx claude-flow@alpha hooks notify \
  --message "CLI integration tests completed: 30/30 passed"
```

## Test Coverage Goals

### Phase 1: Basic Commands (Complete)
- ✅ Version and help commands
- ✅ Invalid command handling
- ✅ Basic marketplace search
- ✅ Lifecycle listing
- ✅ Template help

### Phase 2: Marketplace (In Progress)
- ✅ Search with filters
- ✅ JSON output format
- ✅ Empty results handling
- ⏳ Package installation
- ⏳ Package removal

### Phase 3: Lifecycle (Planned)
- ⏳ Project initialization
- ⏳ Test execution
- ⏳ Build process
- ⏳ Deployment validation

### Phase 4: Templates (Planned)
- ⏳ Template generation
- ⏳ Variable substitution
- ⏳ Output validation

## Performance Requirements

All commands must meet these benchmarks:
- `--version`: < 1 second
- `market search`: < 5 seconds
- `lifecycle init`: < 10 seconds
- `template generate`: < 15 seconds

## Reproducibility Requirements

Tests must be:
1. **Deterministic**: Same input → same output
2. **Isolated**: No shared state between tests
3. **Idempotent**: Multiple runs produce same results
4. **Self-contained**: No external dependencies

## Integration with CI/CD

```yaml
# .github/workflows/cleanroom-tests.yml
name: Cleanroom Integration Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
      - name: Run Cleanroom Tests
        run: cargo test --test cli_integration_cleanroom
```

## Troubleshooting

### Common Issues

1. **Binary Not Found**
   - Ensure `cargo build` runs before tests
   - Check binary path resolution

2. **Permission Errors**
   - Verify temp directory permissions
   - Check file system isolation

3. **Timeout Errors**
   - Increase test timeout limits
   - Check for blocking operations

4. **Flaky Tests**
   - Review environment isolation
   - Check for race conditions
   - Verify deterministic setup

## Future Enhancements

1. **Parallel Test Execution**
   - Leverage cleanroom's concurrency support
   - Run independent tests in parallel

2. **Advanced Scenarios**
   - Multi-step workflows
   - Complex lifecycle testing
   - Integration with external services

3. **Performance Profiling**
   - Command execution timing
   - Resource usage monitoring
   - Bottleneck identification

4. **Coverage Tracking**
   - CLI command coverage
   - Error path coverage
   - Integration scenario coverage

## Success Metrics

- ✅ 100% of critical CLI commands tested
- ✅ All tests reproducible and deterministic
- ✅ Zero .unwrap() or .expect() in test code
- ✅ < 5 minute full test suite execution
- ✅ Integration with memory coordination
- ✅ Production-ready error handling

## References

- [Cleanroom Framework Documentation](/Users/sac/ggen/cleanroom/README.md)
- [Ggen CLI Documentation](/Users/sac/ggen/docs/cli.md)
- [Test-Driven Development Guide](/Users/sac/ggen/docs/development/tdd-guide.md)
