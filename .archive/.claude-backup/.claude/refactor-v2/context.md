# ggen v2.0 Refactoring Context

## Goal
Refactor ggen from v1.x to v2.0.0 using clap-noun-verb v3.0.0 with complete architectural changes.

## Critical Constraints

### 1. Async/Sync Compatibility ⚠️ CRITICAL
- clap-noun-verb v3.0.0 uses sync-only functions (dyn compatibility requirement)
- ggen has 94 async functions in CLI commands
- Solution: Use sync CLI wrappers that spawn async runtimes:
  ```rust
  #[verb("command", "noun")]
  fn noun_command(...) -> Result<Output> {
      let rt = tokio::runtime::Runtime::new()
          .map_err(|e| clap_noun_verb::NounVerbError::execution_error(...))?;
      rt.block_on(async {
          crate::domain::noun::command(...).await
              .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))
      })
  }
  ```

### 2. Error Handling
- NEVER use unwrap() or expect() in production code
- Always use Result<T> types with proper error propagation
- Use ? operator for error propagation

### 3. Business Logic Separation
- CLI layer: cli/src/commands/ (sync wrappers)
- Business logic: cli/src/domain/ (async functions)

### 4. Command Renames (v2.0 Breaking Changes)
- market → marketplace (all 14 commands)
- doctor → utils doctor
- help-me → utils help-me
- ggen gen → ggen template generate
- --vars flags → --rdf flag

## Detailed Plan

See /Users/sac/clap-noun-verb/docs/book/GGEN_V2_REFACTORING_PLAN.md for complete file-by-file instructions.
