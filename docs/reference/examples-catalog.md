# ggen Examples Catalog

This catalog provides a comprehensive overview of all example code included in the ggen project, demonstrating various features and capabilities of the code generation framework.

## Quick Reference

| Example | Description | Dependencies |
|----------|-------------|--------------|
| [mcp_tool_agent](#mcp_tool_agent) | MCP Tool Agent with A2A protocol | tokio |
| [agent_workflow_bridge](#agent_workflow_bridge) | Agent-Workflow integration | tokio |
| [drift-detection-example](#drift-detection-example) | Drift detection system | ggen-core |
| [path_validation_example](#path_validation-example) | Security path validation | ggen-utils |
| [etl-kgc-workflow-integration](#etl-kgc-workflow-integration) | ETL-KGC-4D-Workflow | knhk-orchestrator |

---

## MCP Tool Agent Example

**File**: `examples/mcp_tool_agent.rs`

**What This Demonstrates**:
- MCP Tool definition with input/output schemas
- Agent system for tool execution
- Bidirectional protocol manager for A2A communication
- Performance metrics tracking
- Execution history management

**How to Run**:
```bash
cargo run --example mcp_tool_agent
```

**Expected Output**:
```
MCP Tool Agent Example
========================
Registered tool: 1 for agent: data-agent
Registered tool: 2 for agent: ml-agent

Executing tools via bidirectional protocol...
Executing tool: data_processor with agent: data-agent
Tool execution completed in 500ms
Data processor result: Data processed successfully
Executing tool: model_trainer with agent: ml-agent
Tool execution completed in 500ms
Model trainer result: Model trained with 95% accuracy

Agent capabilities:
  data-agent: data_processor
  ml-agent: model_trainer

Performance metrics:
  Total executions: 2
  Average time: 500.00ms
  Success rate: 100.0%

Execution history:
  data_processor: OK via data-agent in 500ms
  model_trainer: OK via ml-agent in 500ms
```

**Dependencies**:
- `tokio` - Async runtime
- `uuid` - UUID generation (via workspace dependencies)

---

## Agent Workflow Bridge Example

**File**: `examples/agent_workflow_bridge.rs`

**What This Demonstrates**:
- Agent-workflow bridge system with event channels
- Workflow registration and execution
- Agent-based vs system-based step execution
- Progress tracking via event bridge
- Status monitoring for workflow execution

**How to Run**:
```bash
cargo run --example agent_workflow_bridge
```

**Expected Output**:
```
Agent Workflow Bridge Example
================================
Workflow registered: 1

Executing workflow...
Executing agent step: Data Collection with agent: ml-agent
Executing system step: Data Validation
Executing agent step: Model Training with agent: ml-agent
Workflow result: Workflow ml_pipeline completed
Generated artifacts: ["data_collection.result", "data_validation.result", "model_training.result"]
Status: idle - 0% complete

Generating bridge code...
Generated 2 lines of bridge code

Example completed successfully!
```

**Dependencies**:
- `tokio` - Async runtime

---

## Drift Detection Example

**File**: `examples/drift-detection-example.rs`

**What This Demonstrates**:
- Creating a DriftDetector for project state tracking
- Checking for drift between current files and last sync
- Saving state after a successful sync operation
- Displaying drift warnings with change details

**How to Run**:
```bash
cargo run --example drift-detection-example
```

**Expected Output**:
```
=== Drift Detection Example ===

1. Created drift detector for state directory: .ggen

2. Checking drift between current files and last sync...
   No drift detected - code is up to date!

3. Example: Saving state after sync...
   Drift state saved to: .ggen/sync-state.json

=== Example Complete ===
```

**Dependencies**:
- `ggen-core` - Drift detection module

**Setup**:
This example requires a `.ggen` directory. The example will create one automatically if it doesn't exist.

---

## Path Validation Example

**File**: `examples/path_validation_example.rs`

**What This Demonstrates**:
- Template loading with extension validation
- RDF ontology loading security
- Output file path validation
- Batch validation for multiple paths
- Attack prevention demonstrations

**How to Run**:
```bash
cargo run --example path_validation_example
```

**Expected Output**:
```
Path Validation Security Examples

=== Example 1: Template Loading ===

=== Example 2: RDF Ontology Loading ===

=== Example 3: Output File Validation ===

=== Example 4: Batch Validation ===
All 3 templates validated successfully:
  1. templates/struct.tera
  2. templates/enum.tera
  3. templates/trait.tera

=== Example 5: Attack Prevention ===

Attack 1: Path Traversal
  Blocked: ../../../etc/passwd - relative path outside workspace
  Blocked: ../../secrets/api_keys.txt - relative path outside workspace
  Blocked: subdir/../../outside/file.txt - relative path outside workspace

Attack 2: Null Byte Injection
  Blocked: null byte injection - null bytes not allowed

Attack 3: Absolute Path Escape
  Blocked: /etc/passwd - absolute paths not allowed
  Blocked: /var/log/secrets.txt - absolute paths not allowed

Attack 4: Extension Bypass
  Blocked: malware.exe - extension not in allowed list
  Blocked: script.sh - extension not in allowed list
  Blocked: backdoor.so - extension not in allowed list

Attack 5: Depth Limit Bypass
  Blocked: depth limit exceeded - path depth exceeds maximum
```

**Dependencies**:
- `ggen-utils` - PathValidator module

---

## ETL-KGC-4D-Workflow Integration Example

**File**: `examples/etl-kgc-workflow-integration.rs`

**What This Demonstrates**:
- ETL triple event generation with timestamps
- Orchestrator batch processing
- Process variable aggregation
- Workflow trigger submission
- Andon signal monitoring (GREEN/YELLOW/RED)

**How to Run**:
```bash
cargo run --example etl-kgc-workflow-integration
```

**Expected Output**:
```
═══════════════════════════════════════════════════════════════
ETL-KGC-4D-Workflow Engine Integration Example
Holographic Orchestration: A = μ(O)
═══════════════════════════════════════════════════════════════

STAGE 1: ETL Event Generation
─────────────────────────────────────────────────────────────
  Event 1: order
  Event 2: order
  Event 3: order
  Event 4: customer
  Event 5: customer

STAGE 2: Process Batch Through Orchestrator
─────────────────────────────────────────────────────────────
  Trigger generated: uuid-...

STAGE 3: Process Variables
─────────────────────────────────────────────────────────────
  orderId: "123"
  customerEmail: "alice@example.com"
  orderStatus: "PENDING"
  orderTotal: "99.99"

STAGE 4: Workflow Trigger Submission
─────────────────────────────────────────────────────────────
  Submitting trigger to workflow-engine: order-processing-workflow
  Correlation ID: uuid-...
  Span ID: uuid-...
  Workflow execution initiated

STAGE 5: Workflow Execution & Feedback
─────────────────────────────────────────────────────────────
  Process Instance: uuid-...
  State: Running
  Timestamp: nanos since epoch

STAGE 6: Metrics & Andon Signals
─────────────────────────────────────────────────────────────
  Total Events Processed: 5
  Aggregation Ratio: 0.20
  Pending ETL Events: 0
  Andon Signal: Green: All systems operational

═══════════════════════════════════════════════════════════════
Integration Summary
═══════════════════════════════════════════════════════════════

✓ ETL → Orchestrator: 5 events processed
✓ KGC-4D Context: Temporal coordinates injected
✓ Variable Aggregation: 4 variables extracted
✓ Workflow Trigger: uuid-... submitted
✓ Workflow Execution: Process instance created
✓ Span Correlation: End-to-end tracing enabled

The holographic projection (A = μ(O)) succeeded!
Integration closure achieved.
```

**Dependencies**:
- `knhk-orchestrator` - Integration bridge for ETL-KGC-4D-Workflow
- `tokio` - Async runtime
- `serde_json` - JSON serialization

**Architecture**:
Implements the Chatman Equation: **A = μ(O)** where:
- **O** = ETL output (RDF triples with receipts)
- **μ** = Orchestrator's five-stage transformation pipeline
- **A** = Workflow trigger events ready for execution

---

## Running Examples

### Build All Examples
```bash
cargo build --examples
```

### Run a Specific Example
```bash
cargo run --example <name>
```

### List Available Examples
```bash
cargo build --example --help
```

---

## Example Development Guidelines

When creating new examples:

1. **Add Header Documentation**: Include what the example demonstrates, how to run it, expected output, and dependencies
2. **Use Standard Library When Possible**: Minimize external dependencies to keep examples self-contained
3. **Follow ggen Code Style**: Apply the same formatting and naming conventions as the main codebase
4. **Test Your Example**: Ensure it compiles with `cargo build --example <name>`
5. **Document Dependencies**: List any workspace crates or external dependencies required

---

## Example File Organization

Examples are located in the `examples/` directory at the project root. Each example is a single Rust source file (`.rs`) that can be built and run independently using Cargo.

For examples that require additional files or resources, consider creating a subdirectory under `examples/` to keep related files together.

---

## Troubleshooting

### Example Fails to Compile

1. **Check Dependencies**: Ensure all required workspace crates are available
2. **Update Dependencies**: Run `cargo check` to update dependency lock file
3. **Clean Build**: Try `cargo clean` followed by `cargo build --example <name>`

### Example Runtime Errors

1. **Check Data Files**: Some examples may require specific input files to exist
2. **Verify Setup**: Run the example from the project root directory
3. **Check Features**: Some examples may require specific features: `cargo run --example <name> --features <feature>`

### Missing Dependencies

If you see "unresolved crate" errors, the example may depend on a workspace crate that isn't included in the build. Check the root `Cargo.toml` `[dev-dependencies]` section to ensure the dependency is listed.
