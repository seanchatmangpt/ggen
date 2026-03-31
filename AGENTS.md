# AGENTS.md — ggen Enforcement Constitution

## Governing Doctrine

This repository follows **Pasadena TDD** and **Cleanroom Engineering**.

Every claim must survive real execution. Plausible output is not success.
Only witnessed, constrained, repeatable proof counts as success.

---

## Stop Conditions

Do not claim success because output looks good.
Do not trust LLM judgment as proof.
Do not replace real production semantic boundaries with mocks in authoritative tests.
Do not declare done without externally checkable evidence.
Do not optimize for token savings over defect prevention.
Do not silently weaken tests to make them pass.

---

## Mission

ggen is a specification-driven Rust code generation CLI. Formula: A = μ(O) — code precipitates from RDF via five-stage pipeline (μ₁–μ₅).

The objective is to produce artifacts that survive real execution, real validation, and real downstream use — not plausible code or cosmetically acceptable output.

Stack: Rust 1.91.1 | Tokio | Oxigraph | Tera | Clap | 28 crates

---

## Non-Negotiable Laws

1. **Evidence-first**: If you didn't read it from the codebase or capture it from execution, don't write it.
2. **No fabrication**: No made-up OTEL traces, JSON schemas, or documentation. Read actual code first.
3. **Real collaborators**: Chicago TDD only. No mocks, no test doubles, no `mockall::mock!`.
4. **Cargo make only**: NEVER use direct `cargo` commands. Always `cargo make [target]`.
5. **Result<T,E> required**: Zero `unwrap()/expect()` in production code.
6. **Type-first**: Encode invariants in types. Compiler as design tool.
7. **RDF is truth**: Edit `.specify/*.ttl` (source). Never edit generated `.md`.
8. **No root files**: NEVER save to project root. Use `crates/*/src/`, `tests/`, `docs/`.
9. **Batch operations**: 1 message = ALL related operations.
10. **Andon protocol**: Compiler errors or test failures = STOP THE LINE. Fix before proceeding.
11. **OTEL spans are proof**: For LLM/external service features, OTEL spans must exist. Tests passing is NOT sufficient.
12. **Never git reset**: No `git reset --hard`. Fix forward only. Only `git revert` (creates new commit) is allowed.

---

## Proof Stack

Every substantive task must be validated through as many layers as apply:

### Layer 1: Local Deterministic Correctness
Parsing, transforms, formatting, internal logic.

### Layer 2: Artifact Correctness
Required files emitted. Required structures present. Omission-safe fields included.

### Layer 3: Static Validation
Compile/build passes. Lint/format rules pass. Schema/type validation passes.

**Real commands:**
```bash
cargo make check          # Compilation check
cargo make lint           # Clippy + rustfmt (strict mode)
cargo make fmt            # Format check
```

### Layer 4: Protocol Correctness
Interfaces behave correctly. Required capabilities exposed. Lifecycle is coherent.

### Layer 5: Semantic Boundary Correctness
Real LLM or real semantic collaborator tested at production boundary. Hard constraints enforced. Invariants checked externally.

**Real evidence from `crates/ggen-ai/src/client.rs`:**
```rust
// OTEL span emission — llm.complete
let span = tracing::info_span!(
    "llm.complete",
    "operation.name" = "llm.complete",
    "operation.type" = "llm",
    "llm.model" = %self.config.model,
    prompt_len = prompt.len(),
);

// OTEL span emission — llm.complete_stream
let span = tracing::info_span!(
    "llm.complete_stream",
    "operation.name" = "llm.complete_stream",
    "operation.type" = "llm",
    "llm.model" = %self.config.model,
    prompt_len = prompt.len(),
);

// Real OTEL attributes recorded via Span::current().record()
Span::current().record(otel_attrs::LLM_PROMPT_TOKENS, prompt_tokens);
Span::current().record(otel_attrs::LLM_COMPLETION_TOKENS, completion_tokens);
Span::current().record(otel_attrs::LLM_TOTAL_TOKENS, total_tokens);
```

**Real attribute constants from `crates/ggen-ai/src/lib.rs`:**
```rust
pub const LLM_PROMPT_TOKENS: &str = "llm.prompt_tokens";
pub const LLM_COMPLETION_TOKENS: &str = "llm.completion_tokens";
pub const LLM_TOTAL_TOKENS: &str = "llm.total_tokens";
```

**Real OTEL verification command:**
```bash
export RUST_LOG=trace,ggen_ai=trace
cargo test -p ggen-cli-lib --test llm_e2e_test -- --nocapture 2>&1 | tee otel_output.txt

# Verify required spans exist
grep -E "llm\.complete|llm\.complete_stream" otel_output.txt
grep -E "llm\.prompt_tokens|llm\.completion_tokens|llm\.total_tokens" otel_output.txt
```

### Layer 6: Operational Evidence
Logs, traces, receipts, hashes, or reproducible outputs exist. Downstream artifact can be inspected and replayed.

**Real receipt chain from `crates/ggen-receipt/src/receipt.rs`:**
```rust
pub fn sign(mut self, signing_key: &SigningKey) -> Result<Self> { ... }
pub fn verify(&self, verifying_key: &VerifyingKey) -> Result<()> { ... }
pub fn generate_keypair() -> (SigningKey, VerifyingKey) { ... }
```

**Real receipt chain verification from `crates/ggen-receipt/src/chain.rs`:**
```rust
pub fn verify(&self, verifying_key: &VerifyingKey) -> Result<()> { ... }
```

No single layer is sufficient by itself.

---

## Authoritative vs Non-Authoritative Tests

### Authoritative Tests
A test is authoritative only if it exercises the same boundary reality will cross in production and produces externally checkable evidence.

Real examples from this codebase:
- `CompileGate::validate_crate()` runs real `cargo check` on real crates — `crates/ggen-core/src/validation/compile_gate.rs`
- `Graph::new()` + `graph.insert_turtle()` loads real RDF triples
- Tera rendering with real template files and real context
- Real LLM API calls with OTEL span verification (`llm.complete`, `llm.complete_stream`)
- Real filesystem I/O via `tempfile::TempDir`

### Non-Authoritative Tests
These may support development but NEVER certify completion:
- Mocked LLM tests
- Stubbed protocol responses
- Canned JSON fixtures standing in for real semantic behavior
- Tests that verify only formatting or internal calls
- Evaluator-model approval without harder external proof

---

## LLM Rule

LLMs inherit human shortcut-seeking behavior from human-written code, human-generated corpora, and human reward systems.

Therefore:
- Expect rubric gaming
- Expect shallow compliance
- Expect plausible but ungrounded output
- Expect minimum-effort satisfaction of visible constraints

The agent must treat every LLM output as suspect until it survives external proof.

**The model is not the judge. Reality is the judge.**

---

## Testing Doctrine: Pasadena TDD

### Required Loop

```
Red -> Real -> Green -> Receipt -> Refactor
```

Where:
- **Red** = meaningful failing condition exists
- **Real** = tested against real collaborator/boundary when required
- **Green** = all required gates pass
- **Receipt** = evidence artifact exists
- **Refactor** = improve without weakening proof

### Chicago TDD (Mandatory)

Real collaborators. Real execution. State-based verification. AAA pattern.

```rust
// ACCEPTABLE: Real filesystem
let temp_dir = TempDir::new()?;
std::fs::write(temp_dir.path().join("test.txt"), "content")?;
assert!(temp_dir.path().join("test.txt").exists());

// ACCEPTABLE: Real compile gate
let gate = CompileGate::new(workspace_root()).with_timeout(300);
let result = gate.validate_crate("ggen-core");
assert!(result.passed, "Expected ggen-core to compile, but got:\n{}", result.output);

// ACCEPTABLE: Real LLM call with OTEL verification
let client = GenAiClient::new(config)?;
let response = client.complete("Generate code").await?;
assert!(!response.content.is_empty());
```

### Forbidden: London TDD Patterns

```rust
// FORBIDDEN: mockall
use mockall::mock!;
mock! { pub HttpClient {} ... }

// FORBIDDEN: behavior verification
mock_client.expect_get().with(eq("url")).times(1).returning(Ok("..."));

// FORBIDDEN: test doubles for isolation
struct InMemoryStorage { /* fake */ }
struct FakeDatabase { /* fake */ }
```

### Verification Commands

```bash
cargo make check          # Compilation (no errors)
cargo make test           # Full test suite (unit + integration)
cargo make test-unit      # Fast unit tests only
cargo make lint           # Clippy + rustfmt
cargo make pre-commit     # check -> lint -> test-unit (parallel)
cargo make slo-check      # Performance SLOs
cargo make audit          # Security vulnerabilities
```

---

## Anti-Patterns

| Anti-Pattern | Why It's Dangerous | What To Do Instead |
|---|---|---|
| Fake-green via mocks | Tests pass but production breaks | Use real collaborators |
| "Looks correct" acceptance | Plausible ≠ proven | Run commands, read output |
| Skipping compile because output seems reasonable | Hidden type errors propagate | Always run `cargo make check` |
| Weakening assertions after failure | Self-deception | Fix the implementation |
| Substituting prose for evidence | Narration masks weakness | Show commands and output |
| Claiming success without reproducible artifacts | Unverifiable claims | Emit evidence artifacts |
| Direct `cargo` commands | Inconsistent build pipeline | Always use `cargo make` |
| `unwrap()/expect()` in production | Panics crash services | Use `Result<T,E>` |
| `git reset --hard` | Destroys work irreversibly | Fix forward only |
| Saving files to root | Clutters workspace | Use proper subdirectories |

---

## Definition of Done

A task is done ONLY when ALL applicable conditions hold:

- [ ] `cargo make check` passes (zero compiler errors)
- [ ] `cargo make lint` passes (zero clippy warnings)
- [ ] `cargo make test` passes (all tests green)
- [ ] Real semantic boundary exercised if production depends on it
- [ ] OTEL spans verified for LLM/external service features
- [ ] Required evidence artifacts produced (receipts, traces, hashes)
- [ ] No known failing gate is being ignored
- [ ] No mock/test-double used in place of real collaborator in authoritative tests

**If any required layer is missing, the task is not done.**

---

## OTEL Validation Checklist

Before claiming any LLM/external service feature is complete:

| Check | Required Span/Attribute | Source |
|---|---|---|
| Span exists | `llm.complete` or `llm.complete_stream` | `ggen-ai/src/client.rs:205,285` |
| Model attribute | `llm.model` populated | `ggen-ai/src/client.rs:209` |
| Token counts | `llm.prompt_tokens`, `llm.completion_tokens`, `llm.total_tokens` | `ggen-ai/src/lib.rs:99-101` |
| Timing recorded | `elapsed_ms` in response log | `ggen-ai/src/client.rs:261` |
| Error spans | `error=true`, `error.type`, `error.message` on failure | `ggen-ai/src/client.rs:240-242` |
| Real latency | Span duration ~2-3s (network call, not mock) | Empirical observation |

**Interpretation:**

| Observation | Conclusion |
|---|---|
| `llm.complete` span exists | Real LLM API call was made |
| `llm.model=groq::openai/gpt-oss-20b` | Correct endpoint used |
| `llm.total_tokens > 0` | LLM returned actual content |
| Span duration ~2-3 seconds | Real network call (not mock) |
| No spans found | Feature is NOT working |

---

## Compile Gate: Real Evidence

The compile gate runs the REAL `cargo check` process — no mocks.

**Real code from `crates/ggen-core/src/validation/compile_gate.rs`:**
```rust
pub fn validate_crate(&self, crate_name: &str) -> CompileResult {
    let start = Instant::now();
    let output = Command::new("cargo")
        .arg("check")
        .arg("--package")
        .arg(crate_name)
        .current_dir(&self.workspace_root)
        .output();
    // ...
}
```

**Real test from `crates/ggen-core/tests/compile_gate_test.rs`:**
```rust
#[test]
fn test_compile_gate_ggen_core_succeeds() {
    let gate = CompileGate::new(workspace_root()).with_timeout(300);
    let result = gate.validate_crate("ggen-core");
    assert!(result.passed, "Expected ggen-core to compile, but cargo check failed:\n{}", result.output);
}
```

---

## Agent Output Requirements

When reporting work, distinguish clearly between:

- **proven** — verified by real execution
- **observed** — seen in code or output but not independently verified
- **inferred** — deduced from context, not directly verified
- **unverified** — not checked at all

The agent must never present inference as proof.

Every completion report should include:
- What changed
- What was tested
- What real boundaries were exercised
- What evidence artifacts were produced
- What remains unverified

---

## Execution Workflow

```bash
# 1. Create RDF Spec (source of truth)
mkdir -p .specify/specs/NNN-feature && vim .specify/specs/NNN-feature/feature.ttl

# 2. Chicago TDD (RED -> GREEN -> REFACTOR)
vim crates/*/tests/feature_test.rs    # Write failing test (RED)
cargo make test-unit                   # Verify fails
vim crates/*/src/feature.rs           # Implement (GREEN)
cargo make test-unit                   # Verify passes
cargo make pre-commit                  # Refactor (maintain GREEN)

# 3. Validation (Definition of Done)
cargo make check && cargo make lint && cargo make test && cargo make slo-check

# 4. Generate from Ontology
ggen sync --audit true

# 5. OTEL Trace Validation (for LLM/external service features)
RUST_LOG=trace,ggen_ai=trace cargo test -- --nocapture 2>&1 | grep -E "llm\."
```

---

## Preferred Engineering Bias

**Bias toward:**
- Real collaborators over test doubles
- Hermetic, deterministic execution
- Typed contracts and compiler-enforced invariants
- Receipts and traces as proof
- Hard failure over silent degradation
- Explicit uncertainty over fake confidence
- `cargo make` over direct `cargo`

**Bias against:**
- Convenience abstractions that hide reality
- Mocks standing in for semantic truth
- Polished language masking weak proof
- "Should work" without evidence
- Token savings over defect prevention

---

## Pre-Push Hook (Real Evidence)

The real pre-push hook at `.git/hooks/pre-push` runs 4 gates with a 300s timeout:

```
Gate 1: Cargo check (60% of defects)
Gate 2: Test suite
Gate 3: Clippy lint
Gate 4: Format check
```

Target: <90 seconds cold build, ~42 seconds hot build.
Philosophy: 97% defect detection at 80/20 optimization.

---

## Cargo Make Targets (Real Evidence)

From `Makefile.toml`:

| Target | Purpose | Key Detail |
|---|---|---|
| `check` | `cargo check --workspace` | 60s timeout |
| `lint` | Clippy strict + rustfmt | Single pass |
| `test` | Full workspace tests | 30s quick, 120s escalation |
| `test-unit` | Unit tests only | 150s timeout |
| `test-integration` | Integration tests | 30s timeout |
| `pre-commit` | Format + lint + test-unit (parallel) | Runs git hook if present |
| `slo-check` | Performance SLOs | <15s first build, <2s incremental |
| `audit` | Security vulnerabilities | `cargo audit` |

---

## Trust Model

| Source | Trust Level | Verification Required |
|---|---|---|
| Agent claims | 0% | Independent verification |
| Test output | 90% | Read full output, not summary |
| OTEL spans | 95% | External truth |
| Compiler errors | 100% | Cannot be disputed |
| "It should work" | 10% | Demand proof |

**Rule**: Never trust claims. Always verify with evidence.
