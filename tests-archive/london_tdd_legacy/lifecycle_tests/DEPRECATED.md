# DEPRECATED: London TDD Test Suite

**Archived:** 2026-03-30
**Reason:** Chicago TDD is now the ONLY acceptable testing style

This test suite used FORBIDDEN London TDD patterns:
- Hand-rolled mocks (MockCommandExecutor, MockStateRepository, MockHookRegistry, MockObserver)
- Behavior verification (verify_called, verify_call_order, etc.)
- Testing conversations between objects, not state

**Status:** All 43 tests archived as deprecated

Do NOT use these tests as examples. See Chicago TDD tests in:
- tests/integration_test.rs
- crates/ggen-domain/tests/ (real collaborators, no mocks)
- crates/ggen-a2a-mcp/tests/ (real LLM/MCP calls)
