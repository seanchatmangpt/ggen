---
auto_load: true
priority: critical
version: 6.0.1
---

# Absolute Rules

Six rules. Zero exceptions. You violate any of these, you broke the doctrine.

## 1. Concurrent Operations

You batch ALL independent operations into a single message. If two reads have no data dependency, you issue them in the same call. If three file edits target different regions, you issue them in the same call. You do not serialize what can run in parallel. Ever.

## 2. No Root Files

You never save files to the repository root. You use the correct subdirectory: `crates/*/src/` for source, `tests/` for tests, `docs/` for documentation, `.specify/` for specs. If you are about to write to root, stop. Find the right directory.

## 3. Task Tool Required

You use the Claude Code Agent tool for multi-step autonomous work. You do not hand-roll ad-hoc loops or pretend a single Bash call is an agent. You spawn agents with full context, verify their output, and reject fabrication.

## 4. Cargo Make Only

You run `cargo make <target>`. You never run `cargo build`, `cargo test`, `cargo check`, or any other direct cargo command. The project uses `cargo make` for all build orchestration. This is non-negotiable.

## 5. Batch Creation

You create 10 or more todos in a single call. You do not trickle them in one at a time. If a task has fewer than 5 sub-steps, you probably do not need a todo list for it. If it has more than 5, you batch the whole set at once.

## 6. Andon Protocol

When you see a compiler error, a test failure, or a clippy warning, you stop. You do not proceed past a broken build. You do not commit on red. You investigate the root cause, fix it, and verify the fix before taking the next step. You never hide a failure with `|| true`.

## The Golden Rule

1 MESSAGE = ALL RELATED OPERATIONS.

You batch TodoWrite, Task tool spawns, file reads, file writes, and Bash commands into a single message. You chain sequential Bash commands with `&&`. You parallelize everything else.

## Named Failure Modes

You guard against these by name:

- **NARRATION** -- Describing what you will do instead of doing it. You act first, report after.
- **SELF-CERT** -- Declaring your own work correct without running verification. You prove it with output.
- **TEST MURDER** -- Deleting or weakening a failing test instead of fixing the code. You fix the code.
- **SHALLOW GREEN** -- Making tests pass trivially (empty assertions, hardcoded values). You test real behavior.
- **MOCK COMFORT** -- Reaching for mocks to avoid wiring real collaborators. You use real dependencies.
- **LAZY JUDGE** -- Accepting output without reading it. You verify every file, every agent result, every test.

## Evidence Hierarchy

You rank claims by their proof strength:

1. **PROVEN** -- Ran the command, captured the output, the output matches the claim.
2. **OBSERVED** -- Read the code, the code supports the claim.
3. **INFERRED** -- Pattern matches known behavior, but not directly verified.
4. **UNVERIFIED** -- Assumption, guess, or template. Worthless until verified.

You never present an UNVERIFIED claim as PROVEN. You never skip from INFERRED to PROVEN without running the verification step.

## Three-Layer Proof

You provide three layers before claiming any feature is complete:

1. **Execution** -- The code compiles, tests pass, lints pass. You show the output.
2. **Semantics** -- The code does what the spec says. You trace the logic against the requirement.
3. **Evaluation** -- The code meets quality standards (coverage, SLOs, OTEL traces). You show the metrics.

Missing any layer means the claim is incomplete.
