---
auto_load: true
priority: high
version: 6.0.1
---

# Development Workflow

Four steps. No ceremony. You follow them in order.

## Step 1: Create the RDF Spec

You write the spec in Turtle (.ttl) under `.specify/specs/NNN-feature/`. This is the source of truth. You run `ggen validate` against it. You generate markdown from it with `cargo make speckit-render`. You never edit the generated markdown. RDF is truth. You edit .ttl files. You never edit generated .md files.

```bash
mkdir -p .specify/specs/NNN-feature
vim .specify/specs/NNN-feature/feature.ttl
ggen validate .specify/specs/NNN-feature/feature.ttl
cargo make speckit-render
```

## Step 2: Chicago TDD

You write the test first. Always. If you wrote implementation before the test, you are doing it wrong. You write a failing test (RED). You verify it fails with `cargo make test-unit`. You write the minimum implementation to make it pass (GREEN). You verify it passes. Then you refactor while keeping it green.

You use real collaborators: real filesystems via `tempfile::TempDir`, real HTTP clients via `reqwest`, real databases via `sqlx` with SQLite in-memory. You never use mocks, test doubles, or `mockall::mock!`. You never assert on mock interaction counts. You assert on observable state and real outputs.

```bash
vim crates/*/tests/feature_test.rs   # Write failing test (RED)
cargo make test-unit                 # Verify it fails
vim crates/*/src/feature.rs          # Implement (GREEN)
cargo make test-unit                 # Verify it passes
cargo make pre-commit                # Refactor (maintain GREEN)
```

## Step 3: Verify

You run the full verification chain. You do not skip steps. You do not proceed on failure. Every gate must pass before you commit.

```bash
cargo make check        # Compilation -- no errors
cargo make lint         # Clippy + rustfmt -- no warnings
cargo make test         # Full test suite -- all pass
cargo make slo-check    # Performance SLOs -- all met
```

If any gate fails, you stop. You apply the Andon Protocol from `absolute.md`: investigate, fix, verify. You do not commit on red.

For features involving LLM calls or external services, you also verify OTEL spans exist. You run with `RUST_LOG=trace` and confirm the spans and attributes are present. Tests passing without OTEL evidence means nothing.

## Step 4: Commit with Evidence

You commit with a receipt in the message body. The receipt shows which gates passed and how many tests ran. You use conventional commit format: `type(scope): description`.

```bash
git commit -m "feat(NNN): Implement feature

[Receipt] cargo make pre-commit: 3/3 gates
[Receipt] cargo make test: 347/347 tests"
```

You never amend commits. You never force-push to master. You never run `git reset --hard`. You fix forward or revert with a new commit.

## Failure Modes in This Workflow

- Skipping Step 1 and writing code without a spec means you have no source of truth. You are guessing.
- Writing implementation before tests (Step 2) means you have no proof the requirement was met. You are self-certifying.
- Skipping verification (Step 3) and committing on red means you are introducing debt. You are lazy judging.
- Committing without evidence (Step 4) means the commit history is unreliable. You are narrating instead of proving.
