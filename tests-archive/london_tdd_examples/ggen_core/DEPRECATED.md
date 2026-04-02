# DEPRECATED: London TDD Educational Examples

**Archived:** 2026-03-30
**Source:** crates/ggen-core/tests/
**Reason:** Chicago TDD is now the ONLY acceptable testing style

## Archived Files

### london_tdd_examples.rs (7 tests)
- Used mockall::automock on 4 traits
- Demonstrated London TDD patterns (educational)
- Not production code

### lifecycle_bdd.rs (14 tests)
- BDD-style tests with custom mocks
- MockCommandExecutor, MockStateRepository, MockLifecycleObserver
- Behavior verification patterns

**Status:** Archived as deprecated educational examples

crates/ggen-core/tests/ is now 98.1% Chicago TDD compliant.
Remaining 1.9% were demonstrative examples only.

See Chicago TDD examples in:
- crates/ggen-domain/tests/ (model Chicago TDD codebase)
- crates/ggen-a2a-mcp/tests/ (real LLM/MCP integration)
