# Cleanroom Examples

This directory contains example programs demonstrating the cleanroom crate's capabilities.

## PoC Cleanroom Example

The `poc_cleanroom.rs` example demonstrates a micro-DSL over the cleanroom crate for hermetic execution across different backends.

### Features Demonstrated

1. **Basic Command Execution**: Simple command execution with assertions
2. **Scenario Execution**: Multi-step workflows with the scenario DSL
3. **Backend Comparison**: Testing across different execution backends (local, docker, podman)
4. **Determinism**: Deterministic execution with seeded random number generation
5. **Policy Enforcement**: Security policy demonstration
6. **Service Management**: Optional service orchestration demo

### Running the Example

```bash
# Run the basic example
cargo run --example poc_cleanroom

# Enable services demo (requires Docker/Podman)
CLEANROOM_DEMO_SERVICES=1 cargo run --example poc_cleanroom
```

### Expected Output

The example will:
- Execute commands using the local backend
- Skip Docker/Podman backends if features are not enabled
- Demonstrate deterministic random number generation
- Show policy enforcement capabilities
- Optionally demonstrate service management

### Architecture

The example implements a micro-DSL that provides:
- **PocConfig**: Configuration for backend selection and timeouts
- **PocRunResult**: Enhanced result structure with metadata
- **PocScenario**: Scenario builder for multi-step workflows
- **Assertions**: Fluent API for validating execution results

This demonstrates how cleanroom can be used as a foundation for building higher-level testing frameworks and DSLs.

