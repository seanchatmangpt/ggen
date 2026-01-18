# Implementation Plan: Optimize Agent-Computer Interface with Anthropic Patterns

**Branch**: `003-optimize-aci-anthropic` | **Date**: 2025-12-11 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/003-optimize-aci-anthropic/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Optimize ggen's Agent-Computer Interface (ACI) following Anthropic's guidance to improve AI agent effectiveness, reduce errors, and enforce architectural principles automatically. Primary requirements:

1. **Enhanced Tool Documentation** (P1): Add comprehensive descriptions to all cargo make targets explaining purpose, timing, SLOs, Andon signals, and error recovery
2. **Poka-Yoke Tool Design** (P2): Implement automatic timeout enforcement, warnings-as-errors, and quality gate verification
3. **Auto-Invoked Constitution Skill** (P3): Package constitution as skill with keyword-triggered auto-loading

**Technical Approach**: Documentation-driven ACI optimization using Anthropic's 3 core principles (Simplicity, Transparency, ACI Quality). Leverages existing Makefile.toml structure, Claude Code skill system, and constitution file. No new crates or dependencies required—focuses on improving existing tool interfaces and documentation.

## Technical Context

**Language/Version**: Rust 1.74+ (edition 2021) - existing ggen toolchain
**Primary Dependencies**:
- cargo-make 0.37+ (build system) - already installed
- Claude Code skill system - available in user environment
- Existing constitution at `.specify/memory/constitution.md`

**Storage**: Filesystem-based
- Makefile.toml enhancement (tool descriptions)
- Skill files in `~/.claude/skills/ggen/` (constitution, patterns)
- No database or external storage required

**Testing**: Chicago TDD with cargo test
- State-based tests for tool behavior (selection accuracy, timeout enforcement)
- Integration tests for skill auto-invocation
- Quality gate validation tests
- Target: 80%+ coverage on new validation logic

**Target Platform**: Development environment (macOS/Linux/Windows)
- CLI tools (cargo make) - cross-platform
- Claude Code - runs in developer workspace
- No runtime deployment required (developer tooling only)

**Project Type**: Single project (documentation and tooling enhancement)
- No new source code crates
- Modifications to existing Makefile.toml
- New skill files in user config directory
- Test additions to existing test suite

**Performance Goals**:
- Tool selection: 90% first-attempt accuracy (measured via test scenarios)
- Time to compilation: 40% reduction (measured: baseline → optimized)
- SLO violation rate: 60% reduction (measured: before → after)
- Agent task completion: 35% improvement in first-attempt success

**Constraints**:
- Must not break existing cargo make targets (backward compatible)
- Must not modify constitution content (only package as skill)
- Skill auto-invocation must not trigger on non-ggen projects
- Documentation must fit in Makefile.toml (no external doc files)

**Scale/Scope**:
- 20-30 cargo make targets to document (Makefile.toml)
- 4-5 skill files to create (constitution + patterns)
- 10-15 test scenarios for validation
- Single-repository, local-only changes (no distributed system)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: N/A - This is documentation/tooling enhancement, not a new crate. Modifies existing Makefile.toml and creates skill files in user config. No code crates required.
- [x] **II. Deterministic RDF Projections**: N/A - This feature doesn't generate code from RDF. It enhances tool documentation and creates static skill files. No determinism concerns.
- [x] **III. Chicago TDD**: YES - Tests will be written first for tool selection accuracy, timeout enforcement, and skill auto-invocation. State-based tests using real cargo make targets and actual Claude Code skill loading. 80%+ coverage achievable for validation logic.
- [x] **IV. cargo make Protocol**: YES - Feature enhances cargo make documentation and adds poka-yoke mechanisms (timeouts, warnings-as-errors). All development uses `cargo make` targets exclusively.
- [x] **V. Type-First Thinking**: N/A - Minimal production code (test validation logic only). No complex type systems required. Skill files are markdown documentation.
- [x] **VI. Andon Signal Protocol**: YES - Feature explicitly implements and documents RED/YELLOW/GREEN signals in cargo make tool descriptions. Development will monitor signals continuously.
- [x] **VII. Error Handling**: N/A - Minimal production code. Test validation logic will use `Result<T,E>` patterns where applicable. No panic-prone operations.
- [x] **VIII. Concurrent Execution**: YES - Implementation batches all file operations (Makefile.toml edits, skill file creation) in single messages. No root folder saves (skills go to `~/.claude/skills/ggen/`).
- [x] **IX. Lean Six Sigma Quality**: YES - Feature enhances quality infrastructure (tool docs, poka-yoke design, quality gates). Pre-commit hooks, linting, and type coverage remain enforced.

**Quality Gates Pass?**: [x] YES

**Gate Evaluation**:
- ✅ **No violations**: All applicable principles (III, IV, VI, VIII, IX) are followed
- ✅ **N/A principles justified**: I, II, V, VII don't apply to documentation/tooling enhancements
- ✅ **Feature improves compliance**: Explicitly enhances cargo make protocol (IV), Andon signals (VI), and Lean Six Sigma quality (IX)
- ✅ **Ready for Phase 0**: Proceed to research phase

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
# Modified files (no new crates)
/Makefile.toml                          # Enhanced task descriptions with ACI documentation
/tests/aci/                             # New test directory for ACI validation
├── tool_selection_tests.rs             # Test cargo make target selection accuracy
├── timeout_enforcement_tests.rs        # Test SLO timeout mechanisms
└── skill_invocation_tests.rs           # Test constitution skill auto-loading

# User configuration (Claude Code)
~/.claude/skills/ggen/                  # New skill files
├── constitution.md                     # Constitution packaged as skill
├── cargo-make-patterns.md              # cargo make usage patterns
├── error-handling.md                   # Result<T,E> patterns
└── chicago-tdd.md                      # Chicago TDD patterns

# Evidence artifacts
/specs/003-optimize-aci-anthropic/evidence/
├── tool-selection-accuracy.md          # Test results for SC-001
├── slo-violation-metrics.md            # Before/after measurements
└── skill-invocation-logs.md            # Auto-loading verification
```

**Structure Decision**: Single project (Option 1) with no new crates. This is a documentation and tooling enhancement that:
1. **Modifies existing Makefile.toml** with comprehensive task descriptions following Anthropic's ACI guidelines
2. **Creates new test directory** (`tests/aci/`) for validation of tool selection accuracy, timeout enforcement, and skill invocation
3. **Adds skill files** to user's Claude Code configuration (`~/.claude/skills/ggen/`) for auto-invoked context
4. **Generates evidence artifacts** in feature spec directory for success criteria verification

No source code changes to existing crates. All modifications are documentation, configuration, and test additions.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |
