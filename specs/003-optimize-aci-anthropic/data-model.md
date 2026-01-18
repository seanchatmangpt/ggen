# Data Model: Optimize Agent-Computer Interface with Anthropic Patterns

**Feature**: 003-optimize-aci-anthropic
**Date**: 2025-12-11
**Purpose**: Define entities, attributes, relationships, and validation rules for ACI optimization

## Core Entities

### 1. cargo make Target

**Description**: A documented build/test/lint command that serves as the primary interface between AI agents and ggen's build system.

**Attributes**:
- `name` (string, required): Target identifier (e.g., "check", "test-unit", "lint")
- `description` (string, required): Comprehensive multi-line documentation including:
  - Purpose: What the target does
  - Timing: When to use it (e.g., "Before every commit")
  - SLO: Performance threshold (e.g., "<5s")
  - Example outputs: RED/YELLOW/GREEN signal formats
  - Error recovery: What to do when it fails
- `command` (string, required): Executable to run (e.g., "timeout", "cargo")
- `args` (string[], required): Command arguments (e.g., ["5s", "cargo", "check"])
- `env` (map<string, string>, optional): Environment variables (e.g., `RUSTFLAGS="-D warnings"`)
- `timeout_seconds` (integer, optional): Explicit timeout if not in args (e.g., 5, 60)

**Relationships**:
- Emits → **Andon Signal** (one-to-many): Each execution produces RED, YELLOW, or GREEN
- Enforces → **SLO** (one-to-one): Each target has associated performance threshold
- Validates via → **Quality Gate** (many-to-many): Multiple gates may check target outputs

**Validation Rules**:
- `description` MUST include all 5 components (purpose, timing, SLO, examples, recovery)
- `description` MUST be >100 characters (ensures comprehensive documentation)
- `timeout_seconds` or timeout in `args` MUST be present (poka-yoke: prevent hanging)
- `name` MUST match pattern `^[a-z][a-z0-9-]*$` (cargo make convention)

**State Transitions**: N/A (stateless - each execution is independent)

**Example**:
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
command = "timeout"
args = ["5s", "cargo", "check", "--all-targets"]
env = { RUSTFLAGS = "-D warnings" }
```

---

### 2. Andon Signal

**Description**: A quality indicator (RED/YELLOW/GREEN) emitted by tools to indicate system health state.

**Attributes**:
- `type` (enum, required): One of { RED, YELLOW, GREEN }
- `source` (string, required): Tool that emitted signal (e.g., "cargo make check")
- `message` (string, required): Human-readable description
- `timestamp` (datetime, required): When signal was emitted
- `action_required` (enum, required): One of { STOP, INVESTIGATE, CONTINUE }

**Relationships**:
- Emitted by → **cargo make Target** (many-to-one): Each target execution produces a signal
- Checked by → **Quality Gate** (many-to-many): Gates verify signal states before allowing progression

**Validation Rules**:
- `type=RED` → `action_required=STOP` (mandatory stop-the-line)
- `type=YELLOW` → `action_required=INVESTIGATE` (assess before proceeding)
- `type=GREEN` → `action_required=CONTINUE` (safe to proceed)
- `message` MUST be non-empty
- `timestamp` MUST be ≤ current time (no future signals)

**State Transitions**:
```
Initial State → Emitted (with type RED/YELLOW/GREEN)
Emitted → Resolved (agent addresses issue)
Resolved → Cleared (subsequent execution returns GREEN)
```

**Example**:
```json
{
  "type": "RED",
  "source": "cargo make check",
  "message": "error[E0425]: cannot find value `unwrap_used` in this scope",
  "timestamp": "2025-12-11T10:15:30Z",
  "action_required": "STOP"
}
```

---

### 3. Constitution Skill

**Description**: An auto-invoked context file containing ggen's 9 core principles, SLOs, quality gates, and development workflow. Loads automatically based on keywords.

**Attributes**:
- `name` (string, required): Skill identifier (e.g., "ggen-constitution")
- `description` (string, required): WHEN + WHEN NOT pattern for keyword matching
- `trigger_keywords` (string[], required): Keywords that invoke skill (e.g., ["cargo make", "unwrap", "TDD"])
- `exclusion_keywords` (string[], required): Keywords that prevent invocation (e.g., ["other project", "external codebase"])
- `content` (markdown, required): Constitution v1.0.0 text
- `version` (semver, required): Constitution version (e.g., "1.0.0")

**Relationships**:
- References → **SLO** (one-to-many): Constitution documents all SLO thresholds
- Documents → **Quality Gate** (one-to-many): Constitution defines all quality gates
- Guides → **cargo make Target** (one-to-many): Provides "why" behind tool design

**Validation Rules**:
- `trigger_keywords` MUST contain ≥10 ggen-specific terms (prevent generic matches)
- `exclusion_keywords` MUST contain ≥3 boundary conditions (prevent contamination)
- `description` MUST include both "Auto-invoke WHEN" and "Do NOT load for" sections
- `content` MUST match `.specify/memory/constitution.md` exactly (single source of truth)

**State Transitions**:
```
Idle → Triggered (keyword match in conversation)
Triggered → Loaded (content delivered to agent context)
Loaded → Active (agent uses principles in reasoning)
Active → Unloaded (conversation ends or non-ggen keyword detected)
```

**Example**:
```yaml
---
description: >
  ggen Constitution v1.0.0 architectural principles and development standards.
  Auto-invoke WHEN: "ggen", "cargo make", "unwrap", "TDD", "RDF projection"
  Do NOT load for: "general Rust", "external project", "casual conversation"
trigger_keywords:
  - cargo make
  - unwrap
  - expect
  - Result<T,E>
  - Chicago TDD
  - RDF projection
  - Andon signal
  - SLO
  - quality gates
  - Lean Six Sigma
exclusion_keywords:
  - other project
  - external codebase
  - general Rust
version: "1.0.0"
---

[Constitution content follows...]
```

---

### 4. SLO (Service Level Objective)

**Description**: A measurable performance target for tools representing quality thresholds that must not be violated.

**Attributes**:
- `name` (string, required): SLO identifier (e.g., "first_build_time")
- `target` (string, required): Threshold value (e.g., "≤15s", "<5s", "≤2s")
- `measured` (string, required): Actual performance (e.g., "0.79s", "1.95s")
- `unit` (enum, required): One of { SECONDS, MILLISECONDS, PERCENTAGE, BYTES }
- `compliance_percentage` (float, required): How far under target (e.g., 84%, 96%)
- `violation_action` (enum, required): What to do when violated { STOP, INVESTIGATE, WARN }

**Relationships**:
- Enforced by → **cargo make Target** (many-to-one): Each target has associated SLO
- Documented in → **Constitution Skill** (many-to-one): Constitution lists all SLOs
- Triggers → **Andon Signal** (one-to-many): Violation produces RED or YELLOW signal

**Validation Rules**:
- `compliance_percentage` MUST be >0% (if negative, SLO is violated)
- `violation_action=STOP` for critical SLOs (e.g., compilation timeout)
- `measured` MUST be updated after each execution (not stale)

**State Transitions**:
```
Met → Measured (within threshold)
Measured → Violated (exceeds threshold) → Andon Signal emitted
Violated → Restored (subsequent execution meets SLO)
```

**Example**:
```json
{
  "name": "first_build_time",
  "target": "≤15s",
  "measured": "0.79s",
  "unit": "SECONDS",
  "compliance_percentage": 84.0,
  "violation_action": "STOP"
}
```

---

### 5. Quality Gate

**Description**: An automated checkpoint that verifies compliance before allowing progression. Represents defect prevention mechanism.

**Attributes**:
- `name` (string, required): Gate identifier (e.g., "pre_commit_quality_gate")
- `checks` (string[], required): List of validations (e.g., ["all_tests_pass", "no_warnings", "signals_green"])
- `blocking` (boolean, required): If true, failure prevents progression
- `bypass_allowed` (boolean, required): If true, can be overridden (default: false)
- `enforcement_point` (enum, required): When gate runs { PRE_COMMIT, PRE_PUSH, PRE_MERGE, TASK_COMPLETION }

**Relationships**:
- Validates → **Andon Signal** (many-to-many): Gates check that all signals are GREEN
- Enforces → **SLO** (many-to-many): Gates verify SLOs are met
- Blocks → **cargo make Target** (many-to-many): Gates can prevent target execution if violations exist

**Validation Rules**:
- `blocking=true` gates CANNOT have `bypass_allowed=true` (poka-yoke: no skipping critical checks)
- `checks` array MUST be non-empty
- `enforcement_point` MUST be defined (gates without enforcement are useless)

**State Transitions**:
```
Pending → Executing (running checks)
Executing → Passed (all checks GREEN) → Allow Progression
Executing → Failed (any check RED) → Block Progression
Failed → Overridden (if bypass_allowed=true AND justified)
```

**Example**:
```json
{
  "name": "pre_commit_quality_gate",
  "checks": [
    "cargo_check_passed",
    "no_compiler_warnings",
    "all_tests_passing",
    "andon_signals_green",
    "slo_compliance_met"
  ],
  "blocking": true,
  "bypass_allowed": false,
  "enforcement_point": "PRE_COMMIT"
}
```

---

## Entity Relationships Diagram

```
┌─────────────────────┐
│ Constitution Skill  │
│  (auto-invoked)     │
└──────┬──────────────┘
       │ documents
       │
       ▼
┌─────────────────────┐      emits      ┌──────────────┐
│ cargo make Target   │───────────────>│ Andon Signal │
└──────┬──────────────┘                 └──────┬───────┘
       │ enforces                              │
       │                                       │ checked by
       ▼                                       │
┌─────────────────────┐                       ▼
│       SLO           │             ┌──────────────────┐
│ (performance target)│<────────────│  Quality Gate    │
└─────────────────────┘   enforces  └──────────────────┘
```

---

## Data Validation Summary

| Entity | Key Validations | Purpose |
|--------|----------------|---------|
| **cargo make Target** | Description >100 chars, timeout present | Ensure comprehensive documentation |
| **Andon Signal** | Type-action consistency (RED→STOP) | Enforce stop-the-line protocol |
| **Constitution Skill** | ≥10 triggers, ≥3 exclusions | Prevent false positives/contamination |
| **SLO** | Compliance >0%, measured not stale | Ensure accurate threshold tracking |
| **Quality Gate** | Blocking gates cannot be bypassed | Prevent skipping critical checks |

All validation rules support the constitution's Lean Six Sigma Quality principle (IX) and poka-yoke design patterns.
