# Contracts: N/A

**Feature**: 003-optimize-aci-anthropic
**Date**: 2025-12-11
**Reason**: No API contracts required for this feature

## Why No Contracts?

This feature optimizes the Agent-Computer Interface (ACI) through documentation and tooling enhancements. It does **not** introduce any programmatic APIs, REST endpoints, GraphQL schemas, or inter-service communication contracts.

## Interface Types in This Feature

All interfaces are **human-readable documentation** and **test validation logic**:

### 1. Makefile.toml Task Descriptions
- **Type**: Inline TOML documentation strings
- **Consumer**: AI agents (Claude Code) reading tool descriptions
- **Format**: Multi-line strings with purpose, timing, SLOs, examples, recovery procedures
- **Example**:
  ```toml
  [tasks.check]
  description = """
  Fast compilation check (<5s target, 1.95s measured).
  Verifies code compiles without running tests.
  Returns RED Andon signal on errors, GREEN on success.

  Usage: cargo make check
  When: Before every commit, after code changes
  SLO: <5s first build, <2s incremental

  Example output:
    GREEN: "Finished dev [unoptimized + debuginfo] target(s) in 1.95s"
    RED: "error[E0425]: cannot find value `x` in this scope"
  """
  ```

### 2. Constitution Skill Files
- **Type**: YAML frontmatter + Markdown content
- **Consumer**: Claude Code skill system (auto-invoked based on keywords)
- **Format**: YAML metadata with trigger/exclusion keywords, Markdown body
- **Location**: `~/.claude/skills/ggen/constitution.md`
- **Example**:
  ```yaml
  ---
  description: >
    ggen Constitution v1.0.0 architectural principles.
    Auto-invoke WHEN: "ggen", "cargo make", "unwrap", "TDD"
    Do NOT load for: "general Rust", "external project"
  trigger_keywords: [cargo make, unwrap, TDD, RDF projection]
  exclusion_keywords: [other project, external codebase]
  version: "1.0.0"
  ---

  [Constitution content follows...]
  ```

### 3. Test Validation Logic
- **Type**: Rust test functions (internal to ggen test suite)
- **Consumer**: `cargo test` and ggen developers
- **Format**: Standard Rust `#[test]` functions in `tests/aci/` directory
- **Example**:
  ```rust
  #[test]
  fn test_agent_selects_check_for_compilation() {
      let task = "Verify the code compiles without running tests";
      let available_targets = load_makefile_targets();
      let selected = agent_select_tool(task, available_targets);
      assert_eq!(selected, "cargo make check");
  }
  ```

## What Would Require Contracts?

Contracts would be necessary if this feature included:

- ❌ REST API endpoints (e.g., `POST /api/v1/tools/select`)
- ❌ GraphQL schema for tool queries
- ❌ gRPC service definitions for agent coordination
- ❌ Message queue contracts (e.g., Kafka event schemas)
- ❌ Inter-service communication protocols
- ❌ External library public APIs (for distribution)
- ❌ Database schema migrations (though we use filesystem, not DB)

None of these apply to ACI optimization.

## Validation Strategy

Instead of API contracts, this feature uses:

1. **Chicago TDD State-Based Tests**: Verify observable behavior of tool selection, timeout enforcement, and skill invocation (see `tests/aci/`)
2. **Data Model Validation Rules**: Enforce constraints on entities (see `data-model.md`)
3. **Constitution Compliance Gates**: Ensure adherence to ggen principles (see `plan.md` Constitution Check)
4. **Success Criteria Metrics**: Measure 90% tool selection accuracy, 60% SLO violation reduction, etc. (see `spec.md`)

## Conclusion

**Contracts are N/A for this feature.** All interfaces are documentation-based and validated through comprehensive test suites measuring agent behavior and system quality improvements.
