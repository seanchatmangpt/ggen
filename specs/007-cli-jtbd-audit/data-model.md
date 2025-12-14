# Data Model: CLI Jobs-to-be-Done Audit

**Feature Branch**: `007-cli-jtbd-audit`
**Date**: 2024-12-14

## Entity Relationship Overview

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│     Command     │────<│   AuditResult   │>────│  AvatarPersona  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
        │                       │
        │                       │
        v                       v
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│ MaturityLevel   │     │   CaseStudy     │────<│ CommandMapping  │
└─────────────────┘     └─────────────────┘     └─────────────────┘
```

---

## Core Entities

### Command

Represents a ggen CLI command or subcommand being audited.

```yaml
Command:
  # Identity
  id: string           # Unique identifier (e.g., "template-generate")
  name: string         # Full command path (e.g., "ggen template generate")
  category: enum       # workflow | template | project | ontology | graph | marketplace | fmea | ai | ci | utils

  # Metadata
  description: string  # Brief description from --help
  version: string      # ggen version (e.g., "4.0.0")
  deprecated: boolean  # Is command deprecated?

  # Structure
  parent: string?      # Parent command if subcommand (e.g., "ggen template")
  subcommands: string[]  # Child subcommands if any
  arguments: Argument[]  # Required/optional arguments
  options: Option[]      # Flags and options

  # Relationships
  audit_result: AuditResult?  # One-to-one
  case_studies: CaseStudy[]   # Many-to-many (via CommandMapping)
```

### Argument

```yaml
Argument:
  name: string
  type: string         # string | path | integer | etc.
  required: boolean
  description: string
  default: any?
```

### Option

```yaml
Option:
  short: string?       # -o
  long: string         # --output
  type: string         # boolean | string | path | etc.
  description: string
  default: any?
```

---

### AuditResult

Captures the evaluation outcome for a single command.

```yaml
AuditResult:
  # Identity
  id: string           # Generated UUID
  command_id: string   # FK to Command

  # Audit Metadata
  version_tested: string   # "4.0.0"
  date: date              # 2024-12-14
  tester: string          # "claude-code" | "human:<name>"

  # Functional Correctness (5 checks)
  functional_correctness:
    executes: boolean      # Command runs without crash
    help_works: boolean    # --help provides info
    happy_path: boolean    # Valid inputs succeed
    error_handling: boolean # Invalid inputs handled gracefully
    exit_codes: boolean    # 0=success, non-zero=failure

  # Agent Accessibility Score (0-100)
  agent_score: integer     # Weighted sum of criteria
  agent_breakdown:
    parseable_output: integer   # 0-25
    error_messages: integer     # 0-20
    idempotency: integer        # 0-15
    progress_feedback: integer  # 0-10
    dry_run: integer            # 0-10
    documentation: integer      # 0-10
    exit_codes: integer         # 0-10

  # Avatar-specific notes
  avatar_notes: Map<AvatarId, string>

  # Maturity Classification
  maturity_level: MaturityLevel  # L0 | L0-DEP | L1 | L2 | L3 | L4 | L5
  maturity_blockers: string[]    # Reasons preventing higher level

  # Evidence
  evidence_files: string[]       # Paths to log files

  # Recommendations
  recommendations: Recommendation[]

# Validation Rules:
# - agent_score = sum of agent_breakdown values
# - 0 <= agent_score <= 100
# - maturity_level = floor(agent_score / 20) unless blockers apply
# - If non-deterministic output detected → maturity_level <= L3
# - If deprecated → maturity_level = L0-DEP
```

### Recommendation

```yaml
Recommendation:
  priority: enum       # P1 | P2 | P3
  description: string
  effort: enum         # low | medium | high
  impact: enum         # low | medium | high
```

---

### AvatarPersona

Represents an AI coding agent archetype.

```yaml
AvatarPersona:
  # Identity
  id: string           # claude_code | cursor_ai | copilot | aider | devin | openhands | windsurf
  name: string         # Display name
  type: enum           # cli_agent | ide_integrated | code_completion | autonomous_agent

  # Capabilities
  context_window: integer       # Token limit
  context_window_chat: integer? # Chat mode limit (if different)

  capabilities: string[]        # What it can do
  limitations: string[]         # What it struggles with

  # Evaluation Focus
  jtbd_focus:
    primary: string             # Main use case
    secondary: string           # Secondary use case

  evaluation_criteria: string[] # What to check for this avatar
  optimal_commands: string[]    # Commands this avatar handles well

  # Minimum Requirements
  minimum_maturity: MaturityLevel  # Minimum level to be usable
  critical_features: string[]      # Must-have features
```

---

### MaturityLevel

Enumeration of maturity levels with criteria.

```yaml
MaturityLevel:
  level: enum          # L0 | L0-DEP | L1 | L2 | L3 | L4 | L5
  name: string         # Absent | Deprecated | Initial | Managed | Defined | Quantified | Optimized
  description: string
  agent_friendliness: integer  # 0 | 0 | 20 | 40 | 60 | 80 | 100

  criteria: Criterion[]        # Checklist items for this level

  # Progression Rules
  required_for_next: string[]  # What's needed to advance
  blockers: string[]           # What prevents this level

Criterion:
  id: string
  description: string
  verification: string         # How to check
  required: boolean            # Must pass to achieve level
```

**Level Definitions**:

| Level | Name | Agent-Friendly | Key Criteria |
|-------|------|----------------|--------------|
| L0 | Absent | 0% | Command crashes or not found |
| L0-DEP | Deprecated | 0% | Intentionally unsupported |
| L1 | Initial | 20% | Executes, --help works |
| L2 | Managed | 40% | Happy path works, exit codes |
| L3 | Defined | 60% | Actionable errors, input validation |
| L4 | Quantified | 80% | JSON output, semantic exit codes, verbose metrics |
| L5 | Optimized | 100% | JSON schema, idempotent, dry-run, deterministic |

---

### CaseStudy

Represents a Fortune 500 enterprise scenario.

```yaml
CaseStudy:
  # Identity
  id: string           # jpmorgan | amazon | pfizer | boeing | netflix | toyota | goldman
  name: string         # Company name
  industry: string     # Banking/Finance, Technology/Retail, etc.
  fortune_rank: integer  # Fortune 500 ranking

  # Context
  description: string  # Use case narrative
  scale:
    entities: integer
    services: integer
    other_metric: string

  # JTBD
  jtbd_statement: string  # "When X, I need Y, so that Z"

  # Command Requirements
  required_commands: CommandMapping[]

  # SLO Requirements
  slo_requirements:
    generation_time: string
    consistency: string
    other: string

  # Validation
  validation_status: enum  # pending | passed | partial | failed | blocked
  gaps: Gap[]

CommandMapping:
  command_id: string
  purpose: string
  criticality: enum      # P1 | P2 | P3
  current_support: enum  # full | partial | none
  gaps: string[]

Gap:
  command_id: string
  issue: string
  severity: enum         # P1 | P2 | P3
  workaround: string?
  enhancement_request: string?
```

---

## State Transitions

### AuditResult Lifecycle

```
┌─────────┐     ┌──────────┐     ┌───────────┐     ┌──────────┐
│ Pending │ ──> │ Running  │ ──> │ Completed │ ──> │ Reviewed │
└─────────┘     └──────────┘     └───────────┘     └──────────┘
                     │                                   │
                     v                                   v
               ┌──────────┐                       ┌──────────┐
               │  Failed  │                       │ Approved │
               └──────────┘                       └──────────┘
```

### CaseStudy Validation Lifecycle

```
┌─────────┐     ┌───────────┐     ┌────────┐
│ Pending │ ──> │ Validating│ ──> │ Passed │
└─────────┘     └───────────┘     └────────┘
                     │
                     ├─────────────> ┌─────────┐
                     │               │ Partial │ (some commands missing/broken)
                     │               └─────────┘
                     │
                     ├─────────────> ┌────────┐
                     │               │ Failed │ (critical commands broken)
                     │               └────────┘
                     │
                     └─────────────> ┌─────────┐
                                     │ Blocked │ (dependency not available)
                                     └─────────┘
```

---

## Aggregation Views

### Maturity Matrix View

Aggregates all commands by maturity level:

```yaml
MaturityMatrixView:
  total_commands: integer
  by_level:
    L0: CommandSummary[]
    L0-DEP: CommandSummary[]
    L1: CommandSummary[]
    L2: CommandSummary[]
    L3: CommandSummary[]
    L4: CommandSummary[]
    L5: CommandSummary[]

  distribution:
    L0: integer
    L0-DEP: integer
    L1: integer
    L2: integer
    L3: integer
    L4: integer
    L5: integer

  agent_usable_count: integer    # L4+
  agent_usable_percentage: float

CommandSummary:
  command: string
  score: integer
  blockers: string[]
```

### Avatar Compatibility Matrix View

Cross-reference of commands × avatars:

```yaml
AvatarCompatibilityView:
  matrix: Map<CommandId, Map<AvatarId, CompatibilityCell>>

  avatar_summaries:
    - avatar_id: string
      usable_commands: integer
      percentage: float
      common_issues: string[]

CompatibilityCell:
  score: integer     # 0-100
  usable: boolean    # score >= 80
  notes: string
```

### Fortune 500 Gap Analysis View

Aggregates case study validation:

```yaml
Fortune500GapView:
  case_studies:
    - id: string
      status: enum
      commands_required: integer
      commands_passing: integer
      gaps: Gap[]

  overall_readiness: float       # % of case studies passing
  critical_gaps: Gap[]           # P1 gaps across all studies
  roadmap: RoadmapItem[]

RoadmapItem:
  command: string
  improvement: string
  impacts_case_studies: string[]
  priority: enum
  effort: enum
```

---

## File Format Standards

### Evidence Files

**YAML Format** (`evidence/<category>/<command>.yaml`):
- Must validate against AuditResult schema
- UTF-8 encoding
- 2-space indentation

**Log Files** (`evidence/<category>/<command>.log`):
- Plain text
- Includes: command executed, stdout, stderr, exit code, timestamp
- Max size: 1MB per log

### Report Files

**Markdown Format** (`reports/*.md`):
- GitHub-flavored markdown
- Tables for matrices
- Links to evidence files

---

## Validation Rules Summary

| Entity | Rule | Enforcement |
|--------|------|-------------|
| AuditResult | agent_score = sum(breakdown) | Calculated field |
| AuditResult | 0 <= score <= 100 | Schema validation |
| AuditResult | non-deterministic → L3 max | Business rule |
| AuditResult | deprecated → L0-DEP | Business rule |
| Command | unique name per category | Schema validation |
| CaseStudy | validation_status based on gaps | Calculated field |
| MaturityLevel | level derived from score | Calculation: floor(score/20) |
