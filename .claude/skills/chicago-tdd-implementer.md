---
name: Chicago TDD Implementer
description: Implement Chicago TDD tests with real collaborators for Rust code in ggen
paths: ["crates/**/*.rs", "crates/**/tests/**"]
type: skill
---

# Skill: chicago-tdd-implementer

> **2026-07-17 notice:** this skill was written against `ggen-core`, the legacy pipeline. As of
> the `2026-ggen-core-replacement` migration (`docs/jira/v26.7.16/`), `ggen-core` is
> **disconnected** from the workspace (`Cargo.toml`'s `members` → `exclude`) and its source is
> frozen byte-identical on disk (fix-forward/non-deletion doctrine) — it receives no new tests or
> features. **`crates/ggen-engine` is the live crate new Chicago TDD work targets.** Confirmed
> live, 2026-07-17: `cargo test -p ggen-core ...` now hard-fails with `package ID specification
> did not match any packages` (`ggen-core/Cargo.toml` inherits ~25 fields via `workspace = true`
> with no workspace left to inherit from post-`exclude`) — do not follow this skill's older
> `-p ggen-core` command literally; see "Test Execution" below for the real, currently-runnable
> equivalent. The `crates/ggen-core/tests/*.rs` paths below still exist on disk and are kept as
> historical record of what Chicago TDD coverage looked like pre-migration; they are not a
> growth target and cannot currently be run standalone.

## Purpose
Implement comprehensive tests using Arrange-Act-Assert (AAA) pattern verifying observable behavior and state changes

## Triggers (WHEN)
- `#[test]` annotation context
- `Arrange` / `Act` / `Assert` pattern needed
- Test coverage discussion
- Observable state verification
- Behavior validation
- Chicago School TDD mentioned
- 95%+ coverage target discussion

## Don't Trigger (WHEN NOT)
- Mock object discussion
- Internal implementation details
- Unit testing only (without behavior focus)

## Responsibilities

### Test Implementation
- Write unit tests using AAA pattern
- Verify observable state changes
- Create integration tests
- Implement edge case testing
- Achieve 95%+ code coverage target

### Test Organization
- Create test modules: `tests/common/mod.rs`
- Set up test fixtures (temp directories, ontologies)
- Implement test helpers
- Create reusable test macros

### Coverage Areas
- Task 1.1: Audit trail tests (audit.json creation, structure, hashing)
- Task 1.2: Force flag tests (protected files, non-protected files)
- Task 1.3: Test scaffold (fixtures, helpers, macros)
- Task 2.1: Merge mode tests (markers, preservation, injection)
- Task 2.2: Watch mode tests (change detection, debouncing, queuing)
- Task 2.3: Conditional execution tests (ASK evaluation, rule skipping)
- Task 4.1-4.2: Validation tests (SHACL, SPARQL, severity)

## Test Pattern (AAA)

```rust
#[test]
fn test_audit_trail_created_with_flag() {
    // Arrange: Set up test environment
    let temp_dir = TempDir::new().unwrap();
    let manifest = load_fixture("minimal.toml");
    
    // Act: Execute sync with --audit flag
    let result = execute_sync(&manifest, &["--audit"]).unwrap();
    
    // Assert: Verify observable state changed
    assert!(temp_dir.path().join("output/audit.json").exists());
    assert_eq!(result.files_written, 2);
    let audit = parse_audit_json(&temp_dir);
    assert!(!audit.rules_executed.is_empty());
}
```

## Chicago School Principles
✅ **State-based testing**: Verify real object state changes
✅ **Observable behavior**: Test outputs, file writes, returned values
✅ **No mocks**: Use real collaborators (TempDir, actual files)
✅ **Fail fast**: Tests should panic on assertion failure

## Test Categories

### Unit Tests
- `crates/ggen-core/tests/audit_trail_integration_tests.rs`
- `crates/ggen-core/tests/force_flag_integration_tests.rs`
- `crates/ggen-core/tests/merge_mode_tests.rs`
- `crates/ggen-core/src/codegen/watch_mode.rs`, `watch_mode_enhanced.rs`, `watch.rs` (inline `#[cfg(test)] mod tests` — no standalone `tests/watch_mode_tests.rs` file exists)
- `crates/ggen-core/tests/conditional_execution_tests.rs`
- `crates/ggen-core/tests/validation_tests.rs`

### Integration Tests
- `crates/ggen-engine/tests/sync_e2e.rs` (real; the `tests/integration/sync_e2e_tests.rs` path
  this line previously named does not exist anywhere in the repo)
- `crates/ggen-engine/tests/cli_boundary.rs` (real; covers CLI flag combinations across
  sync/graph/receipt/doctor verbs — the `tests/integration/cli_flag_combinations.rs` path this
  line previously named does not exist anywhere in the repo)

## Coverage Target
- **Minimum**: 95% for `crates/ggen-engine/src/` (live pipeline; `crates/ggen-core/src/codegen/`
  was the pre-migration target — frozen, no longer where new coverage work lands, see notice
  above).
- **Verification**: no coverage tooling is currently wired into `just` or CI (confirmed —
  `.github/workflows/quality.yml` explicitly lists "tarpaulin coverage" among gates it does
  NOT run; no `test-coverage`/`coverage-report` recipe exists in the justfile). This target is
  currently unverified/unenforced, not just run under a different command.

## Dependencies
- `tempfile` - Temp directory management
- `assert_cmd` - CLI testing
- `serde_json` - JSON parsing

## Related Skills
- `sync-executor` - Code being tested
- `rust-executor` - Running tests
- `architecture-validator` - Test organization

## Files to Create (historical, ggen-core -- frozen, not a growth target; see notice above)
- `crates/ggen-core/tests/common/mod.rs` (already exists)
- `crates/ggen-core/tests/fixtures/` (already exists — ontology, toml, templates)
- `crates/ggen-core/tests/audit_trail_integration_tests.rs` (already exists)
- `crates/ggen-core/tests/force_flag_integration_tests.rs` (already exists)
- `crates/ggen-core/tests/merge_mode_tests.rs` (already exists)
- watch-mode tests already exist, inline in `crates/ggen-core/src/codegen/{watch,watch_mode,watch_mode_enhanced}.rs` — no separate file needed
- `crates/ggen-core/tests/conditional_execution_tests.rs` (already exists)
- `crates/ggen-core/tests/validation_tests.rs` (already exists)

New Chicago TDD coverage lands under `crates/ggen-engine/tests/` instead. Real, current files
covering the same intent (verified to exist, 2026-07-17): `receipt_chain_e2e.rs` (audit trail /
receipt equivalent), `lint_validate_e2e.rs` + `sparql_refusals_e2e.rs` (validation), `sync_e2e.rs`
+ `generation_rules_e2e.rs` (core sync/generation), `frontmatter_fields_e2e.rs` +
`frontmatter_rdf_e2e.rs` (frontmatter), `determinism_query_reexecution_e2e.rs` +
`multi_template_determinism.rs` (determinism/watch-equivalent), `cli_boundary.rs` +
`cli_read_only_invariant_matrix.rs` (CLI flag matrices).

## Test Execution
```bash
# Run all tests (mandated entry point per CLAUDE.md -- never bare cargo/cargo make)
just test

# Run a specific test file (scoped cargo test is fine for fast iteration) --
# ggen-engine, the live crate. -p ggen-core no longer resolves at all (confirmed
# live, 2026-07-17: "package ID specification did not match any packages" -- see
# notice above), so don't use it even for the historical files it still names.
cargo test -p ggen-engine --test receipt_chain_e2e

# Coverage: no working command exists today (see Coverage Target above) -- this
# section previously listed `cargo make test-coverage`/`coverage-report`, neither
# of which is wired to any real tool in this workspace.
```

## Quality Standards
- ✅ 100% of test names describe behavior (not implementation)
- ✅ No test interdependencies (each test independent)
- ✅ Fixtures are deterministic and reproducible
- ✅ Tests run in <10s total

## Constitution Alignment
- **Chicago School TDD**: Observable state verification
- **Type Safety**: Result<T,E> in test setups
- **Deterministic**: Reproducible fixtures, no randomness
- **Coverage**: 95%+ enforces comprehensive behavior validation
