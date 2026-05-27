# Feature Specification: CLI Jobs-to-be-Done Audit

**Feature Branch**: `007-cli-jtbd-audit`
**Created**: 2024-12-14
**Status**: Draft
**Input**: User description: "Jobs to be done audit of every single command line interface command. One to evaluate if it works. Two to evaluate if it helps coding agents to use the CLI. Seven coding agent avatars. Seven Fortune 500 project case studies. ggen maturity matrix"

---

## Clarifications

### Session 2024-12-14

- Q: Where should audit results be persisted? → A: `specs/007-cli-jtbd-audit/evidence/` (current feature directory)
- Q: How should deprecated commands be classified in the maturity matrix? → A: Mark as L0 with "DEPRECATED" flag (counted but labeled)
- Q: What is the minimum acceptable maturity level for a command to be considered "agent-usable"? → A: L4 (80%) - Full observability with JSON output required
- Q: What is the target timeframe for completing the full audit of all 47+ commands? → A: 1 week (rapid assessment, ~10 commands/day)
- Q: How should non-deterministic command output be handled in the audit? → A: Flag as defect, cap maturity at L3 max (blocks L4+ agent-usable status)

---

## Executive Summary

This specification defines a comprehensive Jobs-to-be-Done (JTBD) audit framework for all 47+ ggen CLI commands. The audit evaluates each command from two perspectives:
1. **Functional Correctness**: Does the command work as intended?
2. **Agent Accessibility**: Can AI coding agents effectively use this command?

The framework includes 7 coding agent avatars representing different AI assistant archetypes, 7 Fortune 500 case studies demonstrating enterprise applicability, and a maturity matrix for tracking CLI evolution.

---

## Coding Agent Avatars (7 Personas)

These avatars represent the spectrum of AI coding agents that may interact with ggen:

### Avatar 1: Claude Code (Anthropic)
- **Context Window**: 200K tokens
- **Strengths**: Long-form reasoning, multi-file understanding, tool use
- **Weaknesses**: Execution confirmation bias, hallucination on file paths
- **JTBD Focus**: Complex multi-step generation workflows

### Avatar 2: Cursor AI (Tab Completion)
- **Context Window**: 8K tokens (autocomplete), 32K (chat)
- **Strengths**: Fast inline suggestions, IDE integration
- **Weaknesses**: Limited context, no shell access in autocomplete mode
- **JTBD Focus**: Quick template scaffolding, single-command operations

### Avatar 3: GitHub Copilot
- **Context Window**: 8K tokens
- **Strengths**: Code completion, inline suggestions, multi-language
- **Weaknesses**: No shell execution, limited planning
- **JTBD Focus**: Template generation suggestions, Cargo.toml edits

### Avatar 4: Aider (CLI Agent)
- **Context Window**: Model-dependent (4K-200K)
- **Strengths**: Git-aware, shell execution, diff-based editing
- **Weaknesses**: Requires human confirmation, context juggling
- **JTBD Focus**: Full CLI orchestration, project workflows

### Avatar 5: Devin (Autonomous Agent)
- **Context Window**: Long-term memory + retrieval
- **Strengths**: Autonomous multi-hour execution, web browsing
- **Weaknesses**: Expensive, slow iteration, over-engineering
- **JTBD Focus**: Complete project setup, CI/CD integration

### Avatar 6: OpenHands (Open Source)
- **Context Window**: Model-dependent
- **Strengths**: Self-hosted, customizable, shell access
- **Weaknesses**: Setup complexity, variable model quality
- **JTBD Focus**: Custom template development, ontology management

### Avatar 7: Windsurf (IDE-Integrated)
- **Context Window**: 32K tokens
- **Strengths**: Deep IDE integration, codebase understanding
- **Weaknesses**: Vendor lock-in, limited CLI orchestration
- **JTBD Focus**: In-editor generation, template preview

---

## Fortune 500 Case Studies (7 Enterprises)

### Case Study 1: JPMorgan Chase - Financial Services
- **Industry**: Banking/Finance
- **ggen Use Case**: Regulatory compliance code generation from RDF ontologies
- **Key Commands**: `ontology validate`, `template generate`, `graph query`
- **Scale**: 50K+ Java entities, 200+ microservices
- **JTBD**: "When regulatory requirements change, I need to regenerate compliant code across all services without manual intervention"

### Case Study 2: Amazon - E-commerce/Cloud
- **Industry**: Technology/Retail
- **ggen Use Case**: API client SDK generation from OpenAPI → RDF transformation
- **Key Commands**: `project generate`, `ai generate`, `marketplace install`
- **Scale**: 1M+ API endpoints, multi-language SDKs
- **JTBD**: "When I update an API spec, I need consistent SDK updates across 8 languages within the same commit"

### Case Study 3: Pfizer - Pharmaceutical
- **Industry**: Healthcare/Biotech
- **ggen Use Case**: Clinical trial data models from CDISC ontologies
- **Key Commands**: `ontology extract`, `graph load`, `template generate-tree`
- **Scale**: 500+ clinical studies, FDA submission compliance
- **JTBD**: "When I design a new trial, I need data models that automatically comply with FDA CDISC requirements"

### Case Study 4: Boeing - Aerospace
- **Industry**: Manufacturing/Defense
- **ggen Use Case**: Systems engineering model code generation (SysML → RDF → Code)
- **Key Commands**: `fmea report`, `workflow analyze`, `graph visualize`
- **Scale**: 10M+ parts, safety-critical systems
- **JTBD**: "When I modify a system model, I need to trace code changes back to requirements for certification"

### Case Study 5: Netflix - Media/Streaming
- **Industry**: Entertainment/Technology
- **ggen Use Case**: Content metadata schema generation and validation
- **Key Commands**: `ontology init`, `marketplace validate`, `template lint`
- **Scale**: 100M+ content items, real-time recommendations
- **JTBD**: "When content taxonomy evolves, I need backward-compatible schema migrations across all services"

### Case Study 6: Toyota - Automotive
- **Industry**: Manufacturing/Automotive
- **ggen Use Case**: Vehicle configuration code generation from product ontologies
- **Key Commands**: `fmea pareto`, `project watch`, `graph export`
- **Scale**: 1000+ vehicle configurations, just-in-time manufacturing
- **JTBD**: "When a new vehicle variant is defined, I need manufacturing system code generated within hours, not weeks"

### Case Study 7: Goldman Sachs - Investment Banking
- **Industry**: Finance/Trading
- **ggen Use Case**: Trading system entity generation from market data ontologies
- **Key Commands**: `ai analyze`, `template regenerate`, `ci` integration
- **Scale**: $500B daily trading volume, microsecond latency requirements
- **JTBD**: "When market structure changes, I need type-safe trading code that passes compliance review automatically"

---

## ggen CLI Maturity Matrix

| Level | Name | Description | Commands Ready | Agent-Friendly |
|-------|------|-------------|----------------|----------------|
| **L0** | Absent | Command doesn't exist or fails completely | 0% | N/A |
| **L0-DEP** | Deprecated | Command exists but is deprecated (counted, labeled) | 0% | N/A |
| **L1** | Initial | Command executes but output is unpredictable | Basic help | 20% |
| **L2** | Managed | Command works with known inputs, errors are cryptic | Happy path | 40% |
| **L3** | Defined | Command has clear inputs/outputs, errors are actionable | Most paths | 60% |
| **L4** | Quantified | Command has metrics, JSON output, exit codes | All paths + metrics | 80% |
| **L5** | Optimized | Command is self-documenting, agent-optimized, composable | Full + agents | 100% |

### Maturity Criteria

#### L1 → L2: Basic Functionality
- [ ] Command executes without crash
- [ ] `--help` provides usage information
- [ ] Exit code 0 on success, non-zero on failure

#### L2 → L3: Error Handling
- [ ] Error messages include actionable guidance
- [ ] Invalid inputs produce helpful error messages
- [ ] File not found errors specify the missing file

#### L3 → L4: Observability
- [ ] `--output-format json` support for structured output
- [ ] Exit codes follow conventions (1=error, 2=usage)
- [ ] Timing/metrics available via `--verbose`

#### L4 → L5: Agent Optimization
- [ ] JSON schema for outputs (machine-parseable)
- [ ] Idempotent operations (safe to retry)
- [ ] Dry-run mode (`--dry-run`)
- [ ] Progress indicators for long operations
- [ ] Deterministic output (same inputs = same outputs) — **REQUIRED for L4+; non-determinism caps at L3**

---

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Command Functionality Audit (Priority: P1)

As a **QA engineer**, I need to verify that every ggen CLI command executes correctly with expected inputs and produces predictable outputs, so that I can ensure the CLI is production-ready.

**Why this priority**: Foundation for all other audit activities. Cannot evaluate agent-friendliness if commands don't work.

**Independent Test**: Can be fully tested by running each command with valid inputs and verifying exit codes, output format, and error handling. Delivers verified command inventory.

**Acceptance Scenarios**:

1. **Given** a fresh ggen installation, **When** I run `ggen --help`, **Then** I see all available subcommands listed with descriptions
2. **Given** a valid ontology file, **When** I run `ggen ontology validate ./test.ttl`, **Then** exit code is 0 and output confirms validity
3. **Given** an invalid ontology file, **When** I run `ggen ontology validate ./invalid.ttl`, **Then** exit code is non-zero and error message indicates the problem
4. **Given** no arguments, **When** I run `ggen template generate`, **Then** I receive helpful usage instructions (not a stack trace)

---

### User Story 2 - Agent Accessibility Evaluation (Priority: P1)

As a **coding agent developer**, I need to evaluate each ggen command for AI agent usability, so that I can determine which commands agents can reliably orchestrate.

**Why this priority**: Core deliverable of the JTBD audit. Enables agent ecosystem growth.

**Independent Test**: Can be tested by having each avatar persona attempt to use commands and recording success/failure patterns. Delivers agent compatibility matrix.

**Acceptance Scenarios**:

1. **Given** Claude Code avatar context, **When** evaluating `ggen project generate`, **Then** I can determine if the command provides sufficient feedback for multi-step orchestration
2. **Given** Cursor AI avatar context (8K tokens), **When** evaluating `ggen template new`, **Then** I can determine if the command fits within context limits
3. **Given** any avatar, **When** a command fails, **Then** the error output is parseable and actionable without human interpretation

---

### User Story 3 - Maturity Classification (Priority: P2)

As a **product manager**, I need each CLI command classified by maturity level (L0-L5), so that I can prioritize improvements and communicate CLI readiness.

**Why this priority**: Enables roadmap planning and stakeholder communication.

**Independent Test**: Can be tested by applying maturity criteria checklist to each command and recording scores. Delivers maturity dashboard.

**Acceptance Scenarios**:

1. **Given** the maturity criteria checklist, **When** I evaluate `ggen template generate`, **Then** I can assign a level (L0-L5) with evidence
2. **Given** all commands evaluated, **When** I generate a report, **Then** I see distribution across maturity levels
3. **Given** a command at L2, **When** I review the checklist, **Then** I see specific items needed to reach L3

---

### User Story 4 - Fortune 500 Scenario Validation (Priority: P2)

As an **enterprise architect**, I need to validate that ggen CLI commands support Fortune 500 scale use cases, so that I can recommend ggen for enterprise adoption.

**Why this priority**: Validates real-world applicability beyond toy examples.

**Independent Test**: Can be tested by mapping each case study's JTBD to specific command sequences and verifying feasibility. Delivers enterprise readiness assessment.

**Acceptance Scenarios**:

1. **Given** JPMorgan compliance case study, **When** I execute the required command sequence, **Then** all commands complete successfully at scale
2. **Given** Toyota manufacturing case study, **When** I run `ggen project watch`, **Then** file changes trigger regeneration within SLO (5s for 1k triples)
3. **Given** any case study, **When** a command lacks required capability, **Then** the gap is documented with workaround or enhancement request

---

### User Story 5 - JTBD Documentation (Priority: P3)

As a **technical writer**, I need each command documented with its Jobs-to-be-Done, so that users understand when and why to use each command.

**Why this priority**: Improves discoverability and reduces support burden.

**Independent Test**: Can be tested by verifying documentation completeness and user comprehension. Delivers JTBD command reference.

**Acceptance Scenarios**:

1. **Given** any command, **When** I read its JTBD documentation, **Then** I understand the user's goal, not just the command syntax
2. **Given** a new user, **When** they search for "generate code from ontology", **Then** they find the relevant command(s) via JTBD search

---

### Edge Cases

- **Deprecated commands**: Classified as L0-DEP (counted in totals but clearly labeled as deprecated)
- **Non-deterministic output**: Flagged as defect, maturity capped at L3 max (blocks L4+ agent-usable status)
- How does the audit handle platform-specific behavior (macOS vs Linux)?
- How are experimental/unstable commands classified?
- What if agent success varies by model size (GPT-4 vs GPT-3.5)?

---

## Requirements *(mandatory)*

### Functional Requirements

#### Audit Framework
- **FR-001**: System MUST provide a structured evaluation template for each CLI command
- **FR-002**: System MUST capture both functional correctness and agent accessibility scores
- **FR-003**: System MUST track maturity level (L0-L5) for each command
- **FR-004**: System MUST map commands to Fortune 500 case study requirements

#### Agent Evaluation
- **FR-005**: System MUST evaluate each command against all 7 avatar personas
- **FR-006**: System MUST record specific failure modes per avatar (context limits, parsing failures, etc.)
- **FR-007**: System MUST provide recommendations for improving agent accessibility

#### Reporting
- **FR-008**: System MUST generate a maturity matrix report showing all commands
- **FR-009**: System MUST generate a command-by-avatar compatibility matrix
- **FR-010**: System MUST identify gaps between current state and Fortune 500 requirements

#### Evidence Collection
- **FR-011**: System MUST capture command execution logs as audit evidence in `specs/007-cli-jtbd-audit/evidence/`
- **FR-012**: System MUST store screenshots/recordings of agent interaction attempts in `specs/007-cli-jtbd-audit/evidence/`
- **FR-013**: System MUST version audit results for trend analysis (git-tracked in feature directory)

### Key Entities

- **Command**: CLI command with name, subcommand path, arguments, options
- **AuditResult**: Evaluation result linking Command to scores, evidence, recommendations
- **AvatarPersona**: AI agent archetype with capabilities, limitations, context window
- **CaseStudy**: Enterprise scenario with industry, scale, JTBD, required commands
- **MaturityLevel**: L0-L5 classification with criteria checklist

---

## CLI Command Inventory (47+ Commands)

### Core Commands
| Command | Subcommands |
|---------|-------------|
| `ggen workflow` | analyze, init, report, event, discover |
| `ggen utils` | (various utilities) |
| `ggen ai` | generate, chat, analyze |
| `ggen ci` | (CI/CD integration) |

### Marketplace Commands
| Command | Subcommands |
|---------|-------------|
| `ggen marketplace` | sparql, install, metrics, validate, info, rdf_stats, versions, publish, validate_fmea, search |

### Template Commands
| Command | Subcommands |
|---------|-------------|
| `ggen template` | new, list, lint, generate, get, show, generate-tree, regenerate |

### Ontology Commands
| Command | Subcommands |
|---------|-------------|
| `ggen ontology` | generate, extract, validate, init |

### Graph Commands
| Command | Subcommands |
|---------|-------------|
| `ggen graph` | query, load, visualize, export |

### Project Commands
| Command | Subcommands |
|---------|-------------|
| `ggen project` | new, plan, apply, generate, init, watch, gen |

### FMEA Commands
| Command | Subcommands |
|---------|-------------|
| `ggen fmea` | show, pareto, report, export, list |

---

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-000**: Complete audit within 1 week (~10 commands/day rapid assessment pace)
- **SC-001**: 100% of CLI commands (47+) evaluated with functional correctness score
- **SC-002**: 100% of CLI commands evaluated against all 7 avatar personas
- **SC-003**: All commands assigned maturity level (L0-L5) with evidence
- **SC-004**: All 7 Fortune 500 case studies mapped to command sequences
- **SC-005**: Identified gaps documented with severity and remediation plan
- **SC-006**: Agent accessibility score average ≥ 80% (L4+ maturity required for "agent-usable" classification)
- **SC-007**: Zero commands at L0 (completely broken) in production release
- **SC-008**: JTBD documentation exists for all L4+ commands (agent-usable threshold)

### Audit Evidence Requirements

Each command audit MUST include:
1. Execution log (success/failure)
2. Exit code verification
3. Output format verification (text/JSON)
4. Error handling verification
5. Avatar compatibility notes (per avatar)
6. Maturity level assignment with checklist
7. Recommendations for improvement

---

## Agent-Friendliness Scoring Rubric

| Criterion | Weight | L1 (20%) | L3 (60%) | L5 (100%) |
|-----------|--------|----------|----------|-----------|
| **Parseable Output** | 25% | Text only | JSON option | JSON schema |
| **Error Messages** | 20% | Stack trace | Human-readable | Machine-parseable |
| **Idempotency** | 15% | Destructive | Mostly safe | Fully idempotent |
| **Progress Feedback** | 10% | Silent | Final status | Streaming |
| **Dry-Run Support** | 10% | None | Partial | Full preview |
| **Documentation** | 10% | --help only | Examples | JTBD + examples |
| **Exit Codes** | 10% | 0/1 only | Differentiated | Semantic |

### Scoring Formula
```
Agent Score = Σ (Criterion Score × Weight)
Maturity Level = floor(Agent Score / 20)  // L0-L5
```

---

## Appendix: Evaluation Templates

### Command Audit Template
```yaml
command: ggen <subcommand>
version_tested: 4.0.0
date: YYYY-MM-DD
tester: <name>

functional_correctness:
  executes: true/false
  help_works: true/false
  happy_path: true/false
  error_handling: true/false
  exit_codes: true/false

agent_accessibility:
  claude_code: { score: 0-100, notes: "" }
  cursor_ai: { score: 0-100, notes: "" }
  copilot: { score: 0-100, notes: "" }
  aider: { score: 0-100, notes: "" }
  devin: { score: 0-100, notes: "" }
  openhands: { score: 0-100, notes: "" }
  windsurf: { score: 0-100, notes: "" }

maturity_level: L0-L5
evidence_path: ./evidence/<command>/

recommendations:
  - priority: P1/P2/P3
    description: ""
```

### Case Study Mapping Template
```yaml
case_study: <name>
industry: <industry>
jtbd: "<job statement>"

required_commands:
  - command: ggen <subcommand>
    purpose: ""
    current_support: full/partial/none
    gaps: []

scale_requirements:
  entities: <count>
  files: <count>
  latency_slo: <seconds>

validation_status: passed/failed/blocked
```
