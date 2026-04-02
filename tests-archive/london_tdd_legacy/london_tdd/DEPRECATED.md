# DEPRECATED: London TDD Educational Examples

**Archived:** 2026-03-30
**Reason:** Chicago TDD is now the ONLY acceptable testing style

This directory contained 960 London TDD tests using FORBIDDEN patterns:
- mockall::automock on 6+ traits
- MockXxx structs with behavior verification
- .expect_x().times(1) pattern
- Testing mock interactions, not real system behavior

**Status:** All 960 tests archived as deprecated

This was an educational example of London School TDD.
Do NOT use these tests as examples for new code.

See Chicago TDD examples in:
- crates/ggen-domain/tests/ (real filesystem, real RDF)
- crates/ggen-a2a-mcp/tests/ (real LLM/MCP calls)
