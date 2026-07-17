# Chicago TDD Tools

[![Rust](https://img.shields.io/badge/rust-1.70%2B-orange.svg)](https://www.rust-lang.org)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

**Rust testing framework enforcing Chicago-style TDD (Classicist Test-Driven Development) through compile-time guarantees.**

If it compiles, correctness follows. Type system encodes invariants. Quality is the default, not an afterthought.

---

## Why Chicago TDD?

Chicago-style TDD (Classicist approach) focuses on **behavior verification** using **real collaborators** instead of mocks. This framework enforces that philosophy through Rust's type system:

- **Type-First Design**: Compiler prevents invalid test states before runtime. State machines encoded at type level—if your test compiles, the AAA (Arrange-Act-Assert) pattern is enforced.
- **Error Prevention (Poka-Yoke)**: Mistakes caught at compile time, not in CI. No `.unwrap()` in production code. No `panic!()`. Git hooks prevent them from being committed.
- **Zero-Cost Abstractions**: All safety guarantees compiled away—performance equals unsafe manual code.
- **80/20 Focus**: Framework solves 80% of testing problems with 20% extra effort via generics, const generics, and macros.

**Result**: Tests that actually verify behavior. Bugs prevented before code review. Production panic rate: ~zero.

---

## Quick Start (Choose Your Path)

### 1️⃣ First Time User? → 5-Minute Setup

```bash
# Install just (required)
cargo install just

# Create test file: tests/my_first_test.rs
mkdir -p tests
cat > tests/my_first_test.rs << 'EOF'
use chicago_tdd_tools::prelude::*;

test!(test_addition, {
    // Arrange
    let x = 5;
    let y = 3;
    // Act
    let result = x + y;
    // Assert
    assert_eq!(result, 8);
});

async_test!(test_async_example, {
    let result = async { 5 + 3 }.await;
    assert_eq!(result, 8);
});

fixture_test!(test_with_fixture, fixture, {
    let counter = fixture.test_counter();
    assert!(counter >= 0);
});
EOF

# Run tests
just test
```

**✓ Installation complete** when `just test` shows 3 passing tests.

---

### 2️⃣ Just Want Examples? → Examples Directory

```bash
# Browse working examples (18 included, all tested)
ls examples/

# Run a specific example
cargo run --example basic_test
cargo run --example fail_fast_verification
cargo run --example sector_stacks_workflows
```

**📖 Complete examples guide**: See [examples/README.md](examples/README.md) for all 18 examples organized by category.

---

### 3️⃣ Need Full Reference? → API Documentation

```bash
just docs   # Generate and open Rustdoc
```

---

### 4️⃣ Using Docker/Containers? → Integration Guide

See [Integration Testing](#integration-testing) section below.

---

### 5️⃣ Testing Observability/OTEL? → Weaver Setup

See [Observability & Weaver](#observability--weaver) section below.

---

### 6️⃣ Proving Process Conformance? → OCEL Process Mining

See [Process Mining (OCEL)](#process-mining-ocel) section below.

---

## Core Capabilities (With Real Examples)

### 1. Essential Testing Macros

**Synchronous tests** (no async runtime needed):

```rust
use chicago_tdd_tools::prelude::*;

test!(test_sync_behavior, {
    // Arrange: Set up test data
    let input = vec![1, 2, 3, 4, 5];

    // Act: Execute code under test
    let sum: i32 = input.iter().sum();

    // Assert: Verify behavior
    assert_eq!(sum, 15);
});
```

**Async tests** (1s default timeout):

```rust
async_test!(test_async_operation, {
    // Arrange
    let data = async { vec![1, 2, 3] }.await;

    // Act: Run async code
    tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    let result = data.len();

    // Assert
    assert_eq!(result, 3);
});
```

**Fixture-based tests** (automatic setup/teardown):

```rust
fixture_test!(test_with_fixture_isolation, fixture, {
    // Arrange: Fixture created automatically, isolated per test
    let counter = fixture.test_counter();

    // Act: Use fixture
    let incremented = counter + 1;

    // Assert
    assert!(incremented > counter);
    // Cleanup: Automatic on scope exit
});
```

**Performance tests** (tick budget validation):

```rust
performance_test!(test_performance_constraint, {
    // Arrange
    let work = || {
        let mut sum = 0;
        for i in 0..100 {
            sum += i;
        }
        sum
    };

    // Act
    let result = work();

    // Assert within tick budget (default: ≤8 ticks)
    assert_within_tick_budget!(result > 0);
});
```

---

### 2. Advanced Assertion Helpers

Result type assertions:

```rust
test!(test_result_assertions, {
    let success: Result<i32, String> = Ok(42);
    let failure: Result<i32, String> = Err("oops".to_string());

    // Assert success case
    assert_ok!(success);
    assert_eq!(success.unwrap_or(0), 42);

    // Assert error case with detailed messages
    assert_err!(failure);
    assert_in_range!(success.unwrap_or(0), 40, 50);

    // Custom assertion messages
    assert_eq_msg!(success.unwrap_or(0), 42, "Expected 42, got: {:?}", success);
});
```

---

### 3. Fail-Fast Verification (v1.4.0)

Zero-tolerance execution context with 12-phase verification pipeline:

```rust
use chicago_tdd_tools::core::fail_fast::*;

test!(test_fail_fast_verification, {
    // Arrange: Create strict execution context
    let mut ctx = StrictExecutionContext::new("contract-123")?;

    // Act: Execute phases with fail-fast semantics
    ctx.phase_1_contract_definition(12)?;
    ctx.phase_2_thermal_testing(5, 8)?; // τ ≤ 8 enforced
    
    // Any violation causes immediate failure
    match ctx.phase_2_thermal_testing(10, 8) {
        Ok(PhaseResult::Violation(v)) => {
            panic!("Thermal bound exceeded: {}", v);
        }
        _ => {}
    }
});
```

**Key features**:
- 47 invariant violations covering all failure modes
- 12 distinct phases from Contract Definition to Quality Dashboard
- Self-validating receipts with version and checksum
- No degradation, no warnings ignored, no partial success

**📖 Example**: See `examples/fail_fast_verification.rs`

---

### 4. Sector-Grade Reference Stacks (v1.4.0)

Production-grade implementations demonstrating the Chatman Equation in real-world workflows:

```rust
use chicago_tdd_tools::sector_stacks::academic::*;

test!(test_academic_workflow, {
    // Arrange: Create paper submission
    let paper = PaperSubmission {
        paper_id: "paper-001".to_string(),
        title: "Advanced Testing".to_string(),
        authors: vec!["Dr. Smith".to_string()],
        abstract_text: "Abstract...".to_string(),
        file_size_bytes: 500_000,
    };

    // Act: Process through workflow
    let operation = AcademicOperation::new(paper.clone(), vec![]);
    let assignment = operation.assign_reviewers();
    
    // Collect reviews
    let reviews = vec![/* ... */];
    let operation = AcademicOperation::new(paper, reviews);
    
    // Assert: Generate receipt
    let receipt = operation.generate_receipt(OperationStatus::Success);
    assert_eq!(receipt.sector, "Academic");
});
```

**Available sectors**:
- **Academic Publishing**: Paper review lifecycle with deterministic decision algorithms
- **Enterprise Claims**: Insurance claims processing with fraud detection and settlement

**📖 Example**: See `examples/sector_stacks_workflows.rs`

---

### 5. RDF-Driven Validation (v1.4.0)

Ontologies as single source of truth for workflow validation:

```rust
use chicago_tdd_tools::sector_stacks::rdf::*;

test!(test_rdf_validation, {
    // Arrange: Create ontology
    let mut ontology = SectorOntology::new("Academic".to_string());
    ontology.add_stage(WorkflowStage {
        id: "submission".to_string(),
        name: "Submission".to_string(),
        stage_number: 1,
        is_deterministic: true,
        max_latency_seconds: 60,
    });

    // Act: Validate operations
    let validator = RdfOperationValidator::new().with_ontology(ontology);
    let result = validator.validate_operation_defined("submission");

    // Assert
    assert!(result.is_ok());
});
```

**📖 Example**: See `examples/rdf_validation.rs`

---

### 6. Swarm Coordination (v1.4.0)

Distributed multi-sector coordination with task receipts:

```rust
use chicago_tdd_tools::swarm::*;

test!(test_swarm_coordination, {
    // Arrange: Create coordinator
    let mut coordinator = SwarmCoordinator::new();
    coordinator.register_member(
        SwarmMember::new("agent-1".to_string(), "Agent 1".to_string())
            .with_sector("Academic".to_string())
            .with_capacity(10),
    );

    // Act: Submit and distribute task
    coordinator.submit_task(TaskRequest::new(
        "task-001".to_string(),
        "Academic".to_string(),
        "desk-review".to_string(),
        "paper-123".to_string(),
    ));
    
    let (task_id, member_id) = coordinator.distribute_next_task()?;

    // Assert
    assert_eq!(task_id, "task-001");
});
```

**📖 Example**: See `examples/swarm_coordination.rs`

---

### 7. Property-Based Testing

Generate random test data and verify properties hold **for all inputs**:

```rust
use chicago_tdd_tools::property::*;

test!(test_commutativity_property, {
    // Arrange: Create property test generator
    let mut generator = PropertyTestGenerator::<100, 5>::new().with_seed(42);

    // Generate random test data
    let test_data = generator.generate_test_data();

    // Act & Assert: Verify property holds for all generated data
    for _item in test_data {
        // Property: a + b == b + a (commutativity)
        let a = rand::random::<u32>();
        let b = rand::random::<u32>();
        assert_eq!(a.wrapping_add(b), b.wrapping_add(a), "Addition is commutative");
    }
});
```

**With proptest** (requires `property-testing` feature):

```rust
test!(test_distributivity_with_proptest, {
    use proptest::prelude::*;

    let strategy = ProptestStrategy::new().with_cases(100);

    // Test: a * (b + c) == (a * b) + (a * c)
    strategy.test(
        proptest::prelude::any::<(u32, u32, u32)>(),
        |(a, b, c)| a.wrapping_mul(b.wrapping_add(c)) == a.wrapping_mul(b).wrapping_add(a.wrapping_mul(c))
    );
});
```

**When to use**: Edge cases are hard to imagine. Random generation finds them automatically.

---

### 8. Mutation Testing

Verify test quality by **intentionally breaking code** and checking tests catch it:

```rust
use chicago_tdd_tools::mutation::*;
use std::collections::HashMap;

test!(test_mutation_detection, {
    // Arrange: Create mutation tester with test data
    let mut data = HashMap::new();
    data.insert("key1", "value1");
    let mut tester = MutationTester::new(data);

    // Act: Apply mutation (remove a key)
    tester.apply_mutation(MutationOperator::RemoveKey("key1".to_string()));

    // Assert: Test should fail because we removed data
    let mutated = tester.current_data();
    assert!(mutated.is_empty(), "Mutation was not caught!");

    // Calculate mutation score
    let score = MutationScore::calculate(95, 100);  // 95 mutations caught of 100
    assert!(score.is_acceptable(), "Score: {}%", score.score());
});
```

**Mutation operators**: `RemoveKey`, `AddKey`, `ChangeValue`, `NegateCondition`

**Target**: ≥80% mutation score indicates thorough test coverage.

---

### 9. Snapshot Testing

Verify complex outputs (JSON, HTML, serialized data) don't change unexpectedly:

```rust
test!(test_snapshot_comparison, {
    use insta::assert_snapshot;  // Requires snapshot-testing feature

    let data = serde_json::json!({
        "user": "alice",
        "status": "active"
    });

    // Assert snapshot matches expected output
    assert_snapshot!(data.to_string());

    // Workflow:
    // 1. First run: creates snapshot file
    // 2. Second run: compares against snapshot
    // 3. Change output? Update snapshot with: just snapshot-accept
});
```

**Snapshot management**:
```bash
just snapshot-review    # Review changes
just snapshot-accept    # Accept new snapshots
just snapshot-reject    # Reject and revert
```

---

### 10. Concurrency Testing

Detect race conditions with deterministic thread-safe testing:

```rust
test!(test_concurrent_safety, {
    use chicago_tdd_tools::concurrency::*;

    // Arrange: Use loom for deterministic testing
    loom::model(|| {
        let data = std::sync::Arc::new(std::sync::Mutex::new(0));
        let data_clone = data.clone();

        // Act: Spawn thread accessing shared data
        let handle = loom::thread::spawn(move || {
            let mut guard = data_clone.lock().unwrap();
            *guard += 1;
        });

        // Verify no panics (loom exhaustively tests interleavings)
        handle.join().unwrap();
    });
});
```

---

### 11. CLI Testing

Test command-line interfaces like they're black boxes:

```rust
test!(test_cli_invocation, {
    use chicago_tdd_tools::testing::cli::*;

    // Arrange: Prepare CLI test
    let mut cli_test = CliTest::new("my-cli-tool");

    // Act: Run command
    let output = cli_test
        .arg("--help")
        .run()
        .expect("CLI should run");

    // Assert: Check output
    assert!(output.stdout.contains("Usage:"));
    assert_eq!(output.exit_code, 0);
});
```

---

## Integration Testing

### Docker + Testcontainers

Test with real services (Postgres, Redis, etc.) without manual Docker commands:

```rust
fixture_test!(test_with_postgres, fixture, {
    use chicago_tdd_tools::integration::testcontainers::*;

    // Arrange: Fixture automatically spins up Postgres container
    let container = fixture.postgres_container()
        .expect("Postgres container should start");

    // Get connection string
    let conn_string = container.connection_string();
    println!("Connected to: {}", conn_string);

    // Act: Execute query
    let result = container.execute_query(
        "SELECT count(*) FROM pg_tables;"
    ).await;

    // Assert: Verify result
    assert_ok!(result);

    // Cleanup: Container automatically stopped on fixture drop
});
```

**Enable with**:
```toml
[dev-dependencies]
chicago-tdd-tools = { path = "../chicago-tdd-tools", features = ["testcontainers"] }
```

**Run with**:
```bash
just test-integration  # Requires Docker running
```

---

## Observability & Weaver

Test OpenTelemetry (OTEL) instrumentation and semantic convention compliance with **Weaver live-check**.

### 1. Bootstrap Weaver (First Time)

```bash
# Download Weaver CLI + semantic convention registry
just weaver-bootstrap

# This creates:
# - target/<profile>/weaver (executable)
# - registry/ (semantic conventions)
```

### 2. Quick Smoke Test (No Docker Required)

```bash
# Verify Weaver works + send test span
just weaver-smoke

# Output: Weaver version + telemetry validation
```

### 3. OTEL Span Validation

```rust
use chicago_tdd_tools::observability::otel::*;

test!(test_otel_span_validation, {
    // Arrange: Create OTEL span context
    let trace_id = TraceId::new(12345);
    let span_id = SpanId::new(67890);
    let context = SpanContext::new(trace_id, span_id);

    // Act: Create span with attributes
    let span = Span::new("http.request", context)
        .with_attribute("http.method", "GET")
        .with_attribute("http.url", "https://example.com");

    // Assert: Validate span structure
    assert_eq!(span.name(), "http.request");
    assert!(span.has_attribute("http.method"));
});
```

### 4. Weaver Live-Check (Full Validation)

Validates spans/metrics against OpenTelemetry semantic conventions in real-time:

```rust
fixture_test!(test_weaver_live_check, fixture, {
    use chicago_tdd_tools::observability::weaver::*;

    // Arrange: Create Weaver validator (requires weaver feature + bootstrap)
    let weaver = fixture.weaver_instance()
        .expect("Weaver should initialize");

    // Act: Send OTEL span
    let span = create_http_span("GET", "/api/users");
    send_otel_span(span.clone());

    // Assert: Validate against semantic conventions
    let result = weaver.validate_span("http.request", &span);
    assert_ok!(result, "Span should comply with semantic conventions");
});
```

**Enable with**:
```toml
[dev-dependencies]
chicago-tdd-tools = {
    path = "../chicago-tdd-tools",
    features = ["weaver", "otel"]  # otel auto-enabled with weaver
}
```

**Run integration tests**:
```bash
just test-integration  # Includes Weaver tests
# Or skip if Docker unavailable:
WEAVER_ALLOW_SKIP=1 just test-integration
```

---

## Process Mining (OCEL)

Turn test execution into an **Object-Centric Event Log (OCEL 2.0)** and prove that the runtime process actually conformed to the declared one. Built on the published [`wasm4pm-compat`](https://crates.io/crates/wasm4pm-compat) crate, which enforces a one-way evidence lifecycle: events start `Raw`, must pass an admission gate to become `Admitted`, and are finalized into a `Receipted` log with a digest. There is no bypass — invalid evidence cannot reach the receipted state.

```rust
use chicago_tdd_tools::observability::ocel::*;   // feature = "ocel-generation"

test!(test_ocel_collector, {
    // Arrange: a collector that captures test diagnostics as OCEL events
    let collector = OcelCollector::new();

    // Act: events are admitted (case_id, timestamp monotonicity, and
    // object references are validated) then sealed into a receipted log
    // … collector accumulates events during the run …

    // Assert: the sealed log carries a stable digest you can pin in CI
    // let (receipted_log, digest_hex) = seal_run(&collector, run_id)?;
    // assert!(!digest_hex.is_empty());
});
```

**What you get**:
- **OCEL 2.0 event logs** — `TestOcelEvent`, `OcelLog`, `TestObject` typed carriers
- **Admission gate** — events validated for lawful case IDs, timestamp monotonicity, and object references before they count
- **Receipted logs** — each sealed run yields a digest for tamper-evident pinning
- **Process discovery** (`ocel-generation-discovery`) — surfaces `GraduationCandidate`s where the mined process diverges from the declared model

> **Doctrine**: *If the code says it worked but the event log cannot prove a lawful process happened, then it did not work.* Model-vs-log mismatch is a defect, not a discrepancy.

**📖 Example**: See `tests/ocel_tests.rs`

---

## Multi-Agent Teamwork Orchestration

Coordinate multi-agent swarms with high-integrity guarantees and structured execution workflows. The teamwork subsystem supports dry-running, validation, and multi-agent execution.

### 👥 Trigger Mechanics & Two-Phase Workflow

Orchestration proceeds in a structured two-phase lifecycle:
1. **Triggering via slash commands**: Run `/teamwork-preview` in the CLI or IDE interface to dry-run swarm execution plans.
2. **Requirements Elicitation**: The orchestrator looks for `prompt_draft.md` at the project root to perform static requirements elicitation, ensuring all acceptance criteria and target directories are explicitly defined before execution begins.

### 🛡️ Configurable Integrity Modes

Governing agent behavior is done through three different compliance profiles:
- **Permissive**: Relaxed validation. Warnings for test failures or style guide non-compliance are logged, but do not halt progression. Good for quick iteration.
- **Strict**: Strong validation. Code modifications must compile, all unit tests must pass, and style guidelines must have zero violations before agent submissions are accepted.
- **Cryptographic / Audit**: Maximum accountability. Every action, validation result, and status transition is hashed and chained into a tamper-evident audit log using BLAKE3 digests, providing cryptographic assurance of the agent's work.

### 📂 Folder-Isolated Coordination & Artifacts

Swarm members coordinate via the filesystem under the `.agents/` folder:
- **Workspace Isolation**: Each subagent gets a folder-isolated directory (e.g. `.agents/worker_doc_writer/`). Agents can read from any folder but can only write to their own folder.
- **Liveness Heartbeats**: Active agents regularly write to their `progress.md` with a `Last visited: [timestamp]` header for liveness tracking.
- **Strict Handoffs**: When transitioning work or completing tasks, agents compile a formal `handoff.md` containing the five mandatory sections: Observation, Logic Chain, Caveats, Conclusion, and Verification Method.

### 🔌 Model Context Protocol (MCP) Tool Bridging

Integration with host-level services and environment tools is supported through bridging components:
- **`McpAgentBridge`**: Exposes host-side capabilities, databases, and compilers dynamically to subagents via MCP JSON-RPC pipes.
- **`A2aTaskHarness`**: Provides HTTP stubbing and A2A testing interfaces to validate tool calls, schema adherence, and multi-agent interactions.

**📖 Example**: See [tests/teamwork_preview_integration.rs](tests/teamwork_preview_integration.rs) and the [Release Notes for v26.7.2](docs/releases/RELEASE_NOTES_v26.7.2.md) for more details.

---

## Build System (Important!)

**⚠️ Always use `just`, never raw `cargo`:**

```bash
just check              # Compilation check (fast)
just test               # Unit tests only
just test-unit          # Same as test
just test-integration   # Integration tests (Docker, Weaver)
just test-all           # Unit + integration
just lint               # Clippy checks
just fmt                # Code formatting
just pre-commit         # fmt + lint + unit tests (always run before commit)
just ci-local           # Simulate full CI pipeline
```

**Why mandatory?**
- Handles proc-macro crates correctly
- Enforces timeouts (prevents hanging)
- Consistent build environment
- Single source of truth for build process

**Essential for safety**:
```bash
just install-hooks  # Install git hooks that prevent unwrap/expect in production
```

---

## Quality Standards (Poka-Yoke Enforcement)

### Compile-Time Prevention

**Type-level AAA enforcement**: If test compiles, AAA pattern is correct.

**Sealed traits**: Can't create invalid test states.

**Const assertions**: Size and alignment checked at compile time.

### Build-Time Prevention

**Git hooks**: Prevent `.unwrap()`, `.expect()`, `panic!()` from being committed.

**Clippy enforcement**: All warnings treated as errors (`-D warnings`).

**Timeout SLAs** enforced:
- Quick checks: 5s (fmt, check)
- Compilation: 5-30s depending on profile
- Lint: 300s (CI cold-start)
- Unit tests: 1s per test
- Integration tests: 30s with Docker
- Coverage: 30s

### Runtime Safety

**Result-based errors**: No panics in production code.

```rust
// ❌ Never in production
let value = result.unwrap();

// ✅ Always do this
let value = result?;  // Propagate errors
// OR
let value = match result {
    Ok(v) => v,
    Err(e) => { alert_warning!("Failed: {}", e); default_value }
};
```

**Alert macros** for structured logging:

```rust
alert_critical!("Database unreachable");   // 🚨 Must stop
alert_warning!("Retry attempt {}", n);     // ⚠️ Should stop
alert_info!("Processing {} items", count);  // ℹ️ Informational
alert_success!("Backup complete");          // ✅ Success
alert_debug!("State: {:?}", state);         // 🔍 Diagnostics
```

### Risk Reduction (FMEA)

| Risk | Original | Current | Mitigation |
|------|----------|---------|-----------|
| Production panics (unwrap/expect) | RPN 180 | RPN 36 | Git hooks, CI checks, lint deny |
| Tests pass locally, fail CI | RPN 105 | RPN 21 | Multi-OS, pre-commit simulation |
| Clippy warnings accumulate | RPN 112 | RPN 11 | CI enforcement, pre-commit |
| Flaky tests | RPN 120 | RPN 24 | Retry logic (3x), test isolation |
| Coverage regressions | RPN 336 | RPN 67 | Coverage tracking, Codecov |

---

## Feature Flags

**Core** (always available): `test!`, `async_test!`, `fixture_test!`, builders, assertions

**Enable as needed**:

```toml
[dev-dependencies]
chicago-tdd-tools = {
    path = "../chicago-tdd-tools",
    features = [
        "testing-extras",            # property-testing + snapshot-testing + fake data (most common)
        "otel",                      # OpenTelemetry span/metric validation
        "weaver",                    # Weaver semantic convention live-check (implies otel)
        "testcontainers",            # Docker container support
        "async",                     # Async fixture providers (Rust 1.75+)
        "ocel-generation",           # OCEL 2.0 event logs from test runs (wasm4pm-compat)
        "ocel-generation-discovery", # + process discovery / graduation candidates
    ]
}
```

**Recommended bundles**:
- **80% use case**: `["testing-extras"]` (property + snapshot + fake data)
- **Full testing**: `["testing-extras", "testcontainers"]`
- **With observability**: `["testing-extras", "otel", "weaver"]`
- **With process mining**: `["testing-extras", "ocel-generation"]`
- **Everything**: `["testing-extras", "otel", "weaver", "testcontainers", "async", "ocel-generation-discovery"]`

> **Process mining** (`ocel-generation`) builds on the published [`wasm4pm-compat`](https://crates.io/crates/wasm4pm-compat) `26.6.11` crate, which supplies the one-way `Evidence` lifecycle (`Raw → Admitted → Receipted`) and OCEL 2.0 standard types. See [Process Mining (OCEL)](#process-mining-ocel) below.

---

## Documentation Portal

### 📚 Learning Path (Start Here)

1. **[Getting Started](docs/getting-started/GETTING_STARTED.md)** - Installation, first test, troubleshooting
2. **[Quick Guide](docs/getting-started/QUICK_GUIDE.md)** - Essential patterns (80% of use cases, 15 min read)
3. **[User Guide](docs/getting-started/USER_GUIDE.md)** - Comprehensive usage (deep dive, 1 hour)

### 🔧 How-to Guides (Solve Specific Problems)

- **[Weaver Live-Check](docs/features/WEAVER_LIVE_CHECK.md)** - Full OTEL + Weaver setup
- **[CLI Testing Guide](docs/testing/cli-testing-guide.md)** - Test command-line tools
- **[Observability Testing](docs/observability/observability-testing-guide.md)** - OTEL testing patterns
- **[Timeout Enforcement](docs/features/TIMEOUT_ENFORCEMENT.md)** - Custom timeout SLAs

### 📖 Reference (Lookup Technical Details)

- **[API Reference](docs/reference/API_REFERENCE.md)** - Complete API documentation
- **[Architecture](docs/reference/ARCHITECTURE.md)** - Design principles, module organization
- **[SLA Reference](docs/reference/SLA_REFERENCE.md)** - Service level agreements, quality standards

### 🎓 Understanding (Deep Dives)

- **[SPR Guide](docs/process/SPR_GUIDE.md)** - Elite Rust standards, best practices
- **[Code Review Checklist](docs/process/CODE_REVIEW_CHECKLIST.md)** - What reviewers look for
- **[FMEA: Tests, Build, Actions](docs/process/FMEA_TESTS_BUILD_ACTIONS.md)** - Risk analysis, improvements
- **[FMEA Executive Summary](docs/analysis/FMEA_EXECUTIVE_SUMMARY.md)** - Unit test GitHub Actions FMEA & RCA
- **[Kaizen Improvement Plan](docs/process/KAIZEN_IMPROVEMENT_PLAN.md)** - Error message consistency plans
- **[Mura (Unevenness) Inventory](docs/analysis/MURA_INVENTORY.md)** - Code/doc standardization inventory
- **[LaTeX Formalization Summary](docs/latex/LATEX_FORMALIZATION_SUMMARY.md)** - Mathematical formalization of the Chatman Equation
- **[Test Isolation Guide](docs/process/TEST_ISOLATION_GUIDE.md)** - Preventing test interdependencies
- **[Pattern Cookbook](cookbook/src/README.md)** - Alexander-style patterns (20 documented)

### 🔍 Troubleshooting

**Problem**: "command not found: just"
- **Fix**: `cargo install just`

**Problem**: "cannot find macro 'test!'"
- **Fix**: Add `use chicago_tdd_tools::prelude::*;` to your test file

**Problem**: "feature 'X' is required for module Y"
- **Fix**: Enable feature in `Cargo.toml`: `features = ["feature-name"]`

**Problem**: Tests pass locally but fail in CI
- **Fix**: Run `just ci-local` to simulate CI environment

**More help**: See [Getting Started - Troubleshooting](docs/getting-started/GETTING_STARTED.md#troubleshooting)

---

## Examples

**18 complete, runnable examples** are included, all with tests. Browse them:

```bash
# List examples
ls examples/

# Run an example
cargo run --example basic_test
cargo run --example fail_fast_verification
cargo run --example sector_stacks_workflows
cargo run --example rdf_validation
cargo run --example swarm_coordination
cargo run --example operator_registry
```

**📖 Complete examples guide**: See [examples/README.md](examples/README.md) for full documentation.

**Example categories**:

**Tutorials** (Learning-oriented):
- `basic_test.rs` - Fixtures, builders, assertions
- `macro_examples.rs` - Test/assertion macros
- `sector_stacks_workflows.rs` - Production-grade workflows (v1.4.0)

**How-To Guides** (Task-oriented):
- `property_testing.rs` - Random test generation, properties
- `mutation_testing.rs` - Test quality validation
- `snapshot_testing.rs` - Output comparison (enhanced in v1.4.0)
- `concurrency_testing.rs` - Thread safety with loom
- `cli_testing.rs` - Command-line testing
- `testcontainers_example.rs` - Docker integration
- `otel_weaver_testing.rs` - Observability testing
- `fail_fast_verification.rs` - 12-phase verification pipeline (v1.4.0)
- `rdf_validation.rs` - RDF-driven validation (v1.4.0)
- `swarm_coordination.rs` - Distributed coordination (v1.4.0)

**Explanation** (Understanding-oriented):
- `go_extra_mile.rs` - 1st/2nd/3rd idea progression, 80/20 thinking
- `advanced_features.rs` - Type-level guarantees, zero-cost abstractions

**Reference**:
- `operator_registry.rs` - Pattern registration and guard system (v1.4.0)
- `all_phases_pipeline.rs` - Complete 12-phase pipeline demonstration
- `hyper_advanced_microkernel.rs` - Hyper-advanced μ-kernel features

---

## Requirements

| Component | Minimum | Verify | Install |
|-----------|---------|--------|---------|
| Rust | 1.70 | `rustc --version` | [rustup](https://rustup.rs/) |
| Cargo | Latest stable | `cargo --version` | Included with Rust |
| just | Latest | `just --version` | `cargo install just` |
| Tokio | 1.0+ | (add to Cargo.toml) | [tokio](https://tokio.rs) |
| Docker* | Latest | `docker ps` | [Docker Desktop](https://www.docker.com) |
| Rust 1.75+* | For async fixtures | `rustc --version` | `rustup update stable` |

\* Optional—only needed for specific features (Docker, async fixtures)

---

## Contributing & Community

- **Issues/Questions**: [GitHub Issues](https://github.com/seanchatmangpt/chicago-tdd-tools/issues)
- **Documentation Feedback**: Create issue with `[docs]` tag
- **Code Contributions**: Follow [Code Review Checklist](docs/process/CODE_REVIEW_CHECKLIST.md)

---

## License

MIT

---

## Quick Commands Reference

```bash
# Development
just pre-commit      # Format + lint + test (before every commit)
just ci-local        # Simulate full CI pipeline

# Testing
just test            # Unit tests (fast)
just test-all        # Unit + integration
just test-property   # Property-based tests
just test-mutation   # Mutation testing
just test-snapshot   # Snapshot tests

# Observability
just weaver-bootstrap  # Setup Weaver (once)
just weaver-smoke      # Verify Weaver works
just test-integration  # Full integration tests

# Code Quality
just lint            # Clippy checks (strict)
just fmt             # Code formatting
just coverage        # Test coverage report
just docs            # Generate & open Rustdoc

# Build
just check           # Compilation check
just build           # Debug binary
just build-release   # Optimized binary
```

---

## What's New in v26.7.2

**Multi-Agent Swarm & High-Integrity Coordination** (latest):

- 👥 **`/teamwork-preview` Slash Command** - A structured workflow for dry-running and orchestrating multi-agent tasks in classical TDD swarms.
- 📝 **Two-Phase Workflow** - Separates task execution into **Requirements Elicitation** (parsing `prompt_draft.md` for clear acceptance criteria) and **Swarm Delegation** (assigning tasks to subagents).
- ⚙️ **Configurable Integrity Modes** - Controls agent constraints across three levels:
  - *Permissive*: Warning-only mode for fast local iteration.
  - *Strict*: Blocks unless formatting, compilation, style checks, and all tests pass.
  - *Cryptographic / Audit*: Seals every agent step and verification run inside a tamper-evident BLAKE3-hashed receipt chain.
- 💓 **Swarm Coordination Heartbeats** - Folder-isolated agent execution under the `.agents/` directory, tracked via automated `progress.md` liveness heartbeats and concluded via structured `handoff.md` reports.
- 🔌 **MCP Agent Bridging** - Enables agents to seamlessly discover and execute external tools configured in `mcp_config.json` via the Model Context Protocol.

**Earlier: Process Truth & Governance** (v26.6.121):

- 🔬 **OCEL 2.0 Process Mining** - `observability::ocel` turns test runs into Object-Centric Event Logs with a one-way `Raw → Admitted → Receipted` evidence lifecycle. Built on the published [`wasm4pm-compat 26.6.11`](https://crates.io/crates/wasm4pm-compat) crate (migrated off the in-tree vendor). See [Process Mining (OCEL)](#process-mining-ocel).
- ⚖️ **Governance Module** - `core::governance` adds diagnostic/severity types, law enforcement primitives, channels, and sectors as the baseline for compile-time law-enforcement macros.
- 🌊 **Wave Orchestration** - `swarm::wave` runs N-phase sequential waves with M parallel tasks, with wave-state observability and failure classification (`ResidualClass`).
- 📋 **Full YAWL Operator Registry** - All **43** YAWL workflow control patterns now registered, each characterized by its control-flow law.
- 🧩 **chicago-tdd-lsp** - Editor guard that emits `CTDD-DEV-001` whenever `chicago-tdd-tools` lands in `[dependencies]` instead of `[dev-dependencies]`.

**Backward compatible** — existing `prelude` and crate-root imports continue to work; the new capabilities are additive and feature-gated.

**📖 Documentation**:
- [Release Notes v26.7.2](docs/releases/RELEASE_NOTES_v26.7.2.md) - Complete feature documentation
- [GitHub Release v26.7.2](docs/releases/GITHUB_RELEASE_v26.7.2.md) - GitHub release notes
- [Release Notes v26.6.121](docs/releases/RELEASE_NOTES_v26.6.121.md) - v26.6.121 documentation
- [Changelog](docs/releases/CHANGELOG.md) - Full change history
- [OCEL 2.0 Process Mining](docs/OCEL.md) · [Agent Governance](docs/governance_architecture.md) - New module guides

---

**Next Step**: Follow the [Quick Start](#quick-start-choose-your-path) path that matches your need, or jump to [Learning Path](#-learning-path-start-here) for structured learning.

**Questions?** See [Troubleshooting](#-troubleshooting) or check [Getting Started](docs/getting-started/GETTING_STARTED.md).
