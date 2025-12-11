# Feature Specification: Optimize Agent-Computer Interface with Anthropic Patterns

**Feature Branch**: `003-optimize-aci-anthropic`
**Created**: 2025-12-11
**Status**: Draft
**Input**: User description: "Optimize ggen ACI (Agent-Computer Interface) with Anthropic patterns"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Clear Tool Documentation for AI Agents (Priority: P1)

AI agents working on ggen need clear, comprehensive documentation for each cargo make target so they understand when to use each tool, what it does, expected outputs, and how to interpret results. Currently, agents must infer tool behavior from sparse descriptions, leading to incorrect tool usage and wasted iterations.

**Why this priority**: Foundation for all agent interactions. Without clear tool documentation, agents cannot effectively use ggen's build system, leading to compilation errors, test failures, and violated SLOs. This directly impacts development velocity and quality.

**Independent Test**: Can be fully tested by providing an AI agent with cargo make targets and verifying it correctly selects and uses tools based solely on their descriptions. Delivers immediate value by reducing agent errors and improving first-attempt success rates.

**Acceptance Scenarios**:

1. **Given** an AI agent needs to verify code compiles, **When** it reviews cargo make targets, **Then** it selects `cargo make check` and understands it provides fast compilation feedback (<5s) with RED/GREEN Andon signals
2. **Given** an AI agent encounters a compilation error, **When** it reads the tool documentation, **Then** it understands the error format, knows to fix the issue, and knows to re-run `cargo make check` to verify
3. **Given** an AI agent needs to run tests, **When** it reviews available targets, **Then** it distinguishes between `test-unit` (fast, <16s), `test` (comprehensive, 31s), and selects the appropriate one based on context
4. **Given** an AI agent hits an SLO timeout, **When** it reviews the tool documentation, **Then** it understands the timeout threshold, knows this is a RED Andon signal, and knows to investigate performance before proceeding

---

### User Story 2 - Poka-Yoke Tool Design (Priority: P2)

AI agents need tools designed to prevent common mistakes automatically. Currently, agents can inadvertently violate SLOs (e.g., compilation taking too long), skip quality checks, or use incorrect command variants. Tools should be designed with fail-safes that make mistakes impossible or immediately visible.

**Why this priority**: Prevents defects at the source using Design for Lean Six Sigma (DfLSS) principles. Complements clear documentation (P1) by adding compile-time/runtime guards that catch errors before they compound.

**Independent Test**: Can be tested by intentionally attempting to misuse tools (e.g., running cargo without timeout) and verifying the tool prevents or flags the mistake. Delivers value by reducing debugging time and preventing defect propagation.

**Acceptance Scenarios**:

1. **Given** an AI agent attempts to run a command that might hang, **When** the tool executes, **Then** it automatically enforces a timeout matching the SLO (e.g., 5s for check, 60s for test)
2. **Given** an AI agent tries to use a direct cargo command, **When** reviewing available tools, **Then** only `cargo make` targets are documented and accessible, preventing direct cargo usage
3. **Given** an AI agent runs tests, **When** compilation warnings exist, **Then** the tool treats warnings as errors (poka-yoke design) and returns RED Andon signal
4. **Given** an AI agent completes a task, **When** marking it complete, **Then** automated quality gates verify all signals are GREEN before allowing completion

---

### User Story 3 - Auto-Invoked Constitution Skill (Priority: P3)

AI agents need automatic access to ggen's architectural principles (constitution) without manual invocation. Currently, agents must be explicitly told to reference the constitution, leading to principle violations and inconsistent development patterns. The constitution should load automatically when working on ggen code.

**Why this priority**: Ensures consistent application of ggen principles across all development sessions. Builds on P1 (tool docs) and P2 (poka-yoke) by providing the "why" behind tool design and usage patterns.

**Independent Test**: Can be tested by starting a new conversation about ggen development and verifying the constitution loads automatically based on context keywords (e.g., "cargo make", "unwrap", "RDF", "Chicago TDD"). Delivers value by eliminating manual context loading and ensuring principle compliance.

**Acceptance Scenarios**:

1. **Given** an AI agent starts working on Rust code in ggen, **When** it mentions "cargo make" or "compilation", **Then** the constitution skill automatically loads with cargo make protocol and SLO guidance
2. **Given** an AI agent encounters an error handling situation, **When** it mentions "unwrap" or "Result<T,E>", **Then** the constitution skill automatically loads with error handling standards (Principle VII)
3. **Given** an AI agent writes tests, **When** it mentions "TDD" or "testing", **Then** the constitution skill automatically loads with Chicago TDD methodology (Principle III)
4. **Given** an AI agent works on a general Rust project outside ggen, **When** the constitution keywords are absent, **Then** the skill does NOT load, preventing contamination

---

### Edge Cases

- What happens when an AI agent tries to mark a task complete but quality gates fail (RED signals present)?
- How does the system handle agents working on non-ggen Rust code where ggen principles shouldn't apply?
- What occurs when tool documentation and actual tool behavior diverge (e.g., SLO threshold changes but docs aren't updated)?
- How do agents recover when a poka-yoke mechanism prevents an action they believe is necessary?
- What feedback does an agent receive when attempting to use a prohibited pattern (e.g., direct `cargo test` instead of `cargo make test`)?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: cargo make targets MUST include comprehensive descriptions explaining purpose, timing (when to use), SLOs, example outputs (RED/YELLOW/GREEN signals), and error recovery procedures
- **FR-002**: All cargo make targets MUST enforce timeouts matching documented SLOs to prevent hanging builds and violations
- **FR-003**: cargo make MUST treat compiler warnings as errors (poka-yoke design) to prevent defects from propagating
- **FR-004**: Tool documentation MUST include "when to use" vs "when NOT to use" guidance to prevent tool misselection
- **FR-005**: Constitution MUST be packaged as an auto-invoked skill that loads based on ggen-specific keywords (cargo make, RDF, unwrap, TDD, crate architecture, SLO, Andon signal)
- **FR-006**: Constitution skill MUST include WHEN + WHEN NOT patterns to prevent loading on non-ggen projects
- **FR-007**: Tool documentation MUST be co-located with tool definitions (in Makefile.toml) to ensure single source of truth
- **FR-008**: Quality gates MUST verify all Andon signals are GREEN before allowing task completion
- **FR-009**: Error messages from tools MUST provide actionable recovery guidance (not just error codes)
- **FR-010**: Tool documentation MUST include usage examples showing expected inputs and outputs for common scenarios

### Key Entities

- **cargo make Target**: A documented build/test/lint command with description, purpose, timing, SLO, example outputs, and error recovery. Represents the primary interface between AI agents and ggen's build system.
- **Andon Signal**: A quality indicator (RED/YELLOW/GREEN) emitted by tools to indicate system health. RED = stop immediately, YELLOW = investigate, GREEN = continue. Represents real-time quality feedback.
- **Constitution Skill**: An auto-invoked context file containing ggen's 9 core principles, SLOs, quality gates, and development workflow. Loads automatically based on keywords to ensure principle compliance.
- **SLO (Service Level Objective)**: A measurable performance target for tools (e.g., "compilation <5s", "tests <60s"). Represents quality thresholds that must not be violated.
- **Quality Gate**: An automated checkpoint that verifies compliance before allowing progression (e.g., "all tests pass", "no warnings", "signals GREEN"). Represents defect prevention mechanism.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: AI agents successfully select correct cargo make target on first attempt 90% of the time when given a development task (measured by tool selection accuracy in test scenarios)
- **SC-002**: AI agents correctly interpret Andon signals (RED/YELLOW/GREEN) and take appropriate action 95% of the time (measured by signal response accuracy)
- **SC-003**: SLO violations (timeout exceeded, quality gate failed) decrease by 60% after ACI optimization (measured by comparing SLO violation rates before and after implementation)
- **SC-004**: AI agents reference constitution principles when making architectural decisions 80% of the time without manual prompting (measured by principle citations in agent reasoning)
- **SC-005**: Time to first successful compilation decreases by 40% due to clearer tool guidance (measured by average time from task start to clean `cargo make check`)
- **SC-006**: Agents attempting to use prohibited patterns (direct cargo, unwrap in production) self-correct within 1 iteration 85% of the time (measured by correction speed after poka-yoke feedback)
- **SC-007**: Agent task completion accuracy increases by 35% as measured by tasks completed correctly on first attempt without human intervention
- **SC-008**: Defect escape rate (bugs making it past quality gates) decreases by 50% due to poka-yoke tool design and automated verification

### Assumptions

- AI agents can read and understand structured documentation (markdown with clear headings and examples)
- Agents have access to Claude Code's skill system for auto-invocation based on keywords
- Existing cargo make targets in Makefile.toml can be enhanced with descriptions without breaking functionality
- The constitution file at `.specify/memory/constitution.md` accurately reflects current ggen principles
- Anthropic's guidance on Agent-Computer Interface (ACI) quality applies to ggen's build tooling
- Agents respond to poka-yoke feedback (errors, timeouts, quality gate failures) by adjusting behavior
