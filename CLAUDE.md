# ggen v6.0.1 -- Operating Constitution

A = mu(O). Code precipitates from RDF via five-stage pipeline (mu_1-mu_5).
Rust 1.91.1 | Tokio | Oxigraph | Tera | Clap | 30 crates | 87% coverage

## Your Failure Modes

These are the mistakes you make most often. Learn their shape so you stop making them.

| Name | What You Do | Stop Condition |
|------|------------|----------------|
| NARRATION | Describe what the proof would show instead of running it | You write "this should work" without a test or span to prove it |
| SELF-CERT | Declare "this looks correct" without mechanical verification | You write an evaluation paragraph instead of a failing-turned-passing test |
| TEST MURDER | Weaken an assertion to get green | You change `assert_eq!(x, 42)` to `assert!(result.is_ok())` or remove a check |
| SHALLOW GREEN | Write vacuous assertions that pass trivially | Your test never checks a specific value, only that no panic occurred |
| MOCK COMFORT | Reach for mockall, fakes, or InMemoryStore | You introduce a test double instead of using the real collaborator |
| LAZY JUDGE | Use yourself or an LLM to evaluate output quality | You inspect code visually and say "looks good" instead of running it |

### The Corrigibility Law

> If the only way to make the test pass is to change the test, stop and report.

Do not weaken assertions. Do not broaden matchers. Do not delete the test. Open the
implementation, find the defect, and fix it there. If you cannot fix it, report what
you found and what you tried.

## Operating Principles

### Soundness Contract

Every claim you make about this codebase must be grounded in one of four evidence
levels. You label your evidence or you retract your claim.

| Level | Meaning | Example |
|-------|---------|---------|
| PROVEN | Mechanically verified by compiler, test runner, or OTEL span | `cargo test` exit 0 with 347/347 passed |
| OBSERVED | Seen in execution output, logs, or traces | OTEL span `llm.complete` with `total_tokens=770` |
| INFERRED | Deduced from reading source code | "This function returns early on empty input" |
| UNVERIFIED | Assumption, placeholder, or noise | "This should handle edge cases" |

PROVEN beats OBSERVED beats INFERRED beats UNVERIFIED. A claim at UNVERIFIED is
not a claim. State it as a question or a risk, not a fact.

### Type-First Discipline

Encode invariants in the type system. Prefer newtype wrappers over runtime checks.
Prefer `PhantomData` state machines over boolean flags. Prefer `const generics` over
runtime configuration. If the compiler does not reject misuse, the design is not done.

See `rules/rust/elite-mindset.md` for the full type-first canon.

### Minimal and Complete

Write the smallest code that satisfies the test. Write the strongest test that
rejects wrong code. Do not add "helpful" abstractions before a second use case
demands them. Do not leave TODOs as excuses for incomplete implementations.

## Verification Gates

### Three-Layer Proof

Every feature claim requires all three layers:

1. **Execution proof** -- An OTEL span or test-pass event captured from a real run
2. **Semantic proof** -- A test assertion that checks a specific, non-trivial value
3. **Evaluation proof** -- Weaver schema conformance or equivalent structural validation

Missing any layer means the claim is unverified. "Tests pass" alone satisfies only
layer 2 and is never sufficient for features involving LLM calls, external services,
or pipeline stages.

### Chicago TDD

You write tests against real collaborators: real filesystems, real HTTP clients, real
databases, real LLM APIs. You assert on observable state, not on mock interactions.

You do not use `mockall`, `#[automock]`, `MockXxx` structs, `.expect_x().times(1)`,
`.with(eq(...))`, or InMemoryStore/FakeDatabase test doubles. These are forbidden.

See `rules/rust/testing.md` and `rules/rust/testing-forbidden.md`.

### OTEL Validation

For any feature involving LLM calls, MCP tools, external APIs, or pipeline stages,
you capture and verify OpenTelemetry spans. Required spans include `llm.complete`,
`mcp.tool.call`, and `pipeline.*` (load, extract, generate, validate, emit).

If spans are missing, the feature is not done -- even if tests pass.

See `rules/otel-validation.md` for required attributes and verification commands.

## Rules Index

| Rule File | Scope |
|-----------|-------|
| `rules/_core/absolute.md` | Six non-negotiable rules (always active) |
| `rules/_core/workflow.md` | Four-step development cycle |
| `rules/rust/lsp.md` | LSP-first navigation for .rs files |
| `rules/rust/elite-mindset.md` | Type-first, zero-cost, performance patterns |
| `rules/rust/testing.md` | Chicago TDD requirements |
| `rules/rust/testing-forbidden.md` | Forbidden London TDD patterns |
| `rules/rust/performance.md` | SLO targets and validation |
| `rules/andon/signals.md` | Stop-the-line protocol |
| `rules/otel-validation.md` | OTEL span/trace verification |
| `rules/README.md` | Rules directory overview |

Architecture reference (crate map, trait index, cross-cutting patterns) lives in
`rules/architecture.md`, not here.

## Commands

| Command | Purpose |
|---------|---------|
| `cargo make check` | Compilation check |
| `cargo make test` | Full test suite (unit + integration + property) |
| `cargo make test-mutation` | Mutation testing (score >= 60%) |
| `cargo make lint` | Clippy + rustfmt |
| `cargo make pre-commit` | check, lint, test-unit in sequence |
| `cargo make slo-check` | Performance SLO validation |
| `cargo make audit` | Security vulnerability scan |
| `ggen sync` | Full mu_1-mu_5 pipeline |
| `ggen validate <ttl>` | SHACL validation |

## Workflow

```bash
# 1. Create RDF Spec (source of truth)
mkdir -p .specify/specs/NNN-feature
vim .specify/specs/NNN-feature/feature.ttl
ggen validate .specify/specs/NNN-feature/feature.ttl

# 2. Chicago TDD (RED -> GREEN -> REFACTOR)
vim crates/ggen-core/tests/feature_test.rs   # Write failing test (RED)
cargo make test-unit                         # Verify it fails
vim crates/ggen-core/src/feature.rs          # Implement (GREEN)
cargo make test-unit                         # Verify it passes
cargo make pre-commit                        # Refactor (maintain GREEN)

# 3. Definition of Done
cargo make check && cargo make lint && cargo make test && cargo make slo-check

# 4. Generate from Ontology
ggen sync --audit true

# 5. OTEL Validation (for LLM/external service features)
RUST_LOG=trace,ggen_ai=trace cargo test <test_name> 2>&1 | grep -E "llm\.|mcp\."
```

## Agent Verification Protocol

After any agent completes work, you verify the output before accepting it:

1. **Read** every file the agent created or modified
2. **Check** that the content references real code paths, real error messages, real
   OTEL output -- not generic templates or fabricated examples
3. **Run** the affected tests to confirm they actually pass
4. **Reject** fabricated output. Delete it and redo with grounded prompts

When launching agents, provide real context: actual file paths, actual errors,
actual OTEL output. Include "read code first, use real examples only" in every
agent prompt.

## Tool Restrictions

| Tool | Verdict | Use Instead |
|------|---------|-------------|
| `mcp__desktop-commander__*` | Forbidden | Read, Write, Edit, Glob, Grep, Bash |
| Direct `cargo` commands | Forbidden | `cargo make <target>` |
| Grep on .rs files | Discouraged | LSP (workspaceSymbol, goToDefinition, findReferences) |
| `mockall` / `#[automock]` | Forbidden | Real collaborators (see testing rules) |
| `unwrap()` / `expect()` | Forbidden in production | `Result<T,E>` with proper error propagation |

## Agent Coordination

| Rule | Requirement |
|------|-------------|
| Andon signals | Compiler errors or test failures mean stop. Fix before proceeding. |
| Cargo make only | Never use direct cargo commands. |
| Batch operations | One message contains all related operations. |
| File organization | Never save to root. Use `crates/*/src/`, `tests/`, `docs/`. |
| RDF is truth | Edit `.specify/*.ttl` (source). Never edit `.md` (generated). |
| No unwrap | Zero `unwrap()/expect()` in production paths. |
| Correctness over speed | Real evidence always beats fast output. |
| Definition of done | check + lint + test + slo-check + OTEL all pass. No signals. |

## Support

- Repository: https://github.com/seanchatmangpt/ggen
- Documentation: `/Users/sac/ggen/docs/`
- Rules: `/Users/sac/ggen/.claude/rules/`
