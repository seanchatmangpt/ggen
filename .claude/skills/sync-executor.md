# Skill: sync-executor

## Purpose
Execute ggen sync pipeline with all CLI features (audit trail, force flag, merge mode, watch mode, conditional execution)

## Triggers (WHEN)
- `ggen sync` command context
- `--audit` flag mentioned
- `--force` flag mentioned
- `--watch` flag mentioned
- `SyncExecutor` code reference
- `generation rules` discussion
- Merge marker context (`<<<<<<< GENERATED`)
- SPARQL ASK context

## Don't Trigger (WHEN NOT)
- `spec-writer` or specification context
- Documentation generation
- Architectural discussion

## Responsibilities

### Core Execution
- Modify `crates/ggen-core/src/codegen/executor.rs`
- Integrate audit trail writing
- Implement force flag override logic
- Wire merge mode handler
- Connect watch mode monitor
- Implement conditional rule execution

### CLI Integration
- Wire CLI flags to executor (`--audit`, `--force`, `--dry-run`, `--validate-only`, `--watch`)
- Document flag precedence order
- Handle flag combinations

### File Operations
- Verify protected_paths handling
- Implement file write with audit logging
- Support merge mode file updates
- Create output directory structure

## Test Focus (Chicago TDD)

### Observable State Changes
- Verify files are written to disk
- Confirm audit.json created with correct structure
- Validate protected files handled per flags
- Check merge markers are processed correctly
- Verify watch mode triggers sync on file changes

### Test Pattern
```rust
#[test]
fn test_sync_with_audit_flag() {
    // Arrange: Create temp sync environment
    let env = TestEnv::new();

    // Act: Execute sync with --audit flag
    let result = env.sync_with_args(&["--audit"]).unwrap();

    // Assert: Verify observable state
    assert!(env.audit_json_exists());
    assert_eq!(result.files_written, 2);
}
```

## Related Tasks
- Task 1.1: Audit trail writing
- Task 1.2: Force flag implementation
- Task 2.1: Merge mode handler
- Task 2.2: Watch mode monitor
- Task 2.3: Conditional executor

## Code Quality Standards
- ✅ `Result<T, E>` error handling (no unwrap in production)
- ✅ Chicago TDD with observable state verification
- ✅ 95%+ code coverage target
- ✅ No panics in production code

## Related Skills
- `audit-trail-writer` - Handles audit.json creation
- `merge-mode-handler` - Handles merge marker logic
- `watch-mode-monitor` - Handles file watching
- `conditional-executor` - Handles SPARQL ASK evaluation
- `chicago-tdd-implementer` - Tests for all features
- `rust-executor` - Verification via cargo make

## SLOs
- Sync execution: ≤5s (100 rules, 90th percentile)
- CLI flag processing: <100ms
- File write operations: <1s

## Files to Modify
- `crates/ggen-core/src/codegen/executor.rs`
- `crates/ggen-core/src/codegen/pipeline.rs`
- `crates/ggen-cli/src/sync.rs`
- `crates/ggen-core/src/lib.rs`

## Files to Create
- `crates/ggen-core/src/audit/mod.rs`
- `crates/ggen-core/src/codegen/merge.rs`
- `crates/ggen-core/src/codegen/watch.rs`

## Architecture Compliance
- ✅ Commands layer: CLI flag parsing (no I/O)
- ✅ Operations layer: Pure sync logic
- ✅ Runtime layer: File I/O via dedicated module

## Constitution Alignment
- Type-safe: All operations return `Result<T, E>`
- Deterministic: Audit trail ensures reproducibility
- Zero-cost: Feature flags use composition pattern
- TDD: Observable state verification (Chicago School)
