# ggen Examples Rewrite - Automation Specification

**Document**: Automation Specification for EPIC 9 Parallel Execution
**Date**: 2026-01-05
**Status**: Ready for Parallel Agent Execution
**Target**: 33 examples, 100% coverage, GREEN andon signal

---

## 1. Execution Model: EPIC 9 Atomic Cycle

### Six Mandatory Phases

```
FAN-OUT (Sequential) ─────────────────────┐
                                          │
                    ┌─────────────────────┴────┐
                    │                          │
         INDEPENDENT CONSTRUCTION (Parallel)   │
         (10 agents work simultaneously)       │
                    │                          │
                    └──────────────┬───────────┘
                                   │
                        COLLISION DETECTION
                                   │
                        CONVERGENCE & SYNTHESIS
                                   │
                        REFACTORING & CLOSURE
                                   │
                        ✓ GREEN ANDON SIGNAL
```

---

## 2. Agent Specifications

### Agent Configuration

Each agent receives:
- **Specification**: Complete automation specification (this document)
- **Gold Standard**: RW-001 template pattern (regeneration-framework-setup/)
- **Templates**: RW-002 & RW-003 as additional references
- **Output Format**: Structured as ggen.toml, ontology.ttl, templates/*.tera, golden/, validate.mjs, README.md
- **Success Criteria**: All 9 quality gates pass

### 10 Parallel Agents (Optimal Configuration)

| Agent | Focus | Examples | Time | Output |
|-------|-------|----------|------|--------|
| **Agent 1** | Foundation | RW-004, RW-005 | 60-90m | 2 examples |
| **Agent 2** | Batch 2a | RW-006-RW-009 | 90-120m | 4 examples |
| **Agent 3** | Batch 2b | RW-010-RW-013 | 90-120m | 4 examples |
| **Agent 4** | Batch 2c | RW-014-RW-015 | 60m | 2 examples |
| **Agent 5** | Batch 3a | RW-016-RW-019 | 90-120m | 4 examples |
| **Agent 6** | Batch 3b | RW-020-RW-023 | 90-120m | 4 examples |
| **Agent 7** | Batch 4a | RW-024-RW-027 | 90-120m | 4 examples |
| **Agent 8** | Batch 4b | RW-028-RW-031 | 90-120m | 4 examples |
| **Agent 9** | Batch 4c | RW-032-RW-035 | 60-90m | 4 examples |
| **Agent 10** | Synthesis | Test infrastructure + Final validation | 120-150m | All tests + reports |

---

## 3. Infrastructure Template (Repeating Pattern)

Each agent must produce per example:

### A. ggen.toml (85-100 lines)

```toml
[project]
name = "example-name"
version = "0.1.0"
description = "..."

[ontology]
source = "ontology/*.ttl"

[[inference.rules]]
name = "..." (2-3 rules)

[[generation.rules]]
name = "..." (10 rules)

[validation]
validate_syntax = true
```

**Rules to include**:
1. Core domain generation rule
2. Documentation guide generation
3. Command reference generation (4 reference docs)
4. Example/workflow guide generation
5-10. Specialized rules per example type

### B. ontology/*.ttl (250-300 lines)

**Structure**:
```turtle
# Classes (8-10)
ExampleClass a rdfs:Class

# Properties (15+ across all classes)
property a rdf:Property

# SHACL Shapes (4-5 validation shapes)
ExampleShape a sh:NodeShape

# Instances (20+ examples)
<#instance1> a ExampleClass
```

**Template approach**:
- Examine RW-001 ontology (regeneration-framework-setup/ontology/)
- Adapt schema for example domain
- Maintain SHACL constraint structure
- Include 20+ realistic instances

### C. templates/*.tera (350-600 lines total)

**Minimum set**:
1. **Core template** (150-200 lines): Main code/structure generation
2. **Guide template** (80-120 lines): Learning guide generation
3. **Reference templates** (4): Filters, syntax, examples, troubleshooting guides

**Template structure**:
```tera
---
to: "output/{{ var | filter }}.rs"
vars:
  var: string
  flag: boolean
---
// Tera body with {{ }} variables
```

**Must include**:
- YAML frontmatter with `to:` and `vars:`
- Variable substitution: `{{ var }}`
- Filters: `{{ var | snake_case }}`
- Conditionals: `{% if flag %}`
- Loops: `{% for item in items %}`
- Comments: `{# comment #}`

### D. golden/ (200-400 lines)

**Structure**:
```
golden/
├── generated/
│   ├── output1.rs (100-200 lines)
│   └── output2.md (50-100 lines)
└── expected_structure.txt
```

**What to include**:
- Example outputs from running ggen sync
- Real generated code (valid, compilable Rust)
- Documentation examples
- Test fixtures

### E. validate.mjs (200-250 lines)

**Checks** (minimum 6):
1. ggen installation verification
2. Specification closure (SHACL validation)
3. Template file presence & syntax
4. Configuration validity (ggen.toml)
5. Golden file structure
6. README completeness

**Structure**:
```javascript
function checkmark(msg) { log(`✅ ${msg}`, 'green'); }
function cross(msg) { log(`❌ ${msg}`, 'red'); }
function warning(msg) { log(`⚠️ ${msg}`, 'yellow'); }

// 6+ validation checks
section('CHECK 1: ...');
// validation logic
```

### F. README.md (500-650 lines)

**9-Section Standard**:

1. **Overview** (50-80 lines)
   - What this example teaches
   - Key files and concepts
   - Learning objectives

2. **Prerequisites** (40-60 lines)
   - Required knowledge
   - Required tools
   - Environment setup

3. **Quick Start** (30-50 lines)
   - 5-10 minute demo
   - Key commands
   - Expected output

4. **Architecture & Key Concepts** (80-120 lines)
   - System diagram
   - Key data structures
   - Design patterns

5. **File Structure** (30-50 lines)
   - Directory layout
   - File purposes
   - Organization rationale

6. **Step-by-Step Tutorial** (100-150 lines)
   - 5-8 progressive steps
   - Code examples
   - Expected outcomes

7. **Configuration Reference** (60-100 lines)
   - TOML structure
   - SPARQL patterns
   - Template patterns

8. **Troubleshooting** (50-100 lines)
   - Common errors
   - Solutions
   - Debugging tips

9. **Next Steps** (50-80 lines)
   - Learning progression
   - Practice exercises
   - Related examples

---

## 4. Quality Gates (9-Gate System)

### Pre-Implementation Gates (Must Pass: 3/3)

**GATE 1: Specification Closure**
- Validate SHACL constraints
- Check all required properties present
- Verify RDF syntax validity
- **Status**: Pass/Fail (MANDATORY)

**GATE 2: README Completeness**
- Check all 9 sections present
- Verify section content completeness
- Validate markdown syntax
- **Status**: Pass/Fail (MANDATORY)

**GATE 3: ggen.toml Validity**
- Validate TOML syntax
- Verify 10+ generation rules
- Check SPARQL query syntax
- **Status**: Pass/Fail (MANDATORY)

### Post-Implementation Gates (Target: 9/9)

**GATE 4-9**: Require built ggen binary (deferred until build completes)

---

## 5. Agent Task Template

Each agent receives this execution template:

```
TASK: Implement RW-XXX through RW-YYY

INPUTS:
- Specification: This document
- Gold Standard: examples/regeneration-framework-setup/
- References: RW-002, RW-003
- Output Branch: claude/rewrite-examples-3Sf77

PROCESS:
1. Examine gold standard (RW-001)
2. For each example (RW-XXX):
   a. Create ggen.toml (85-100 lines)
   b. Create ontology/*.ttl (250-300 lines)
   c. Create templates/*.tera (6-10 templates, 350-600 lines)
   d. Create golden/ outputs
   e. Create validate.mjs script
   f. Create README.md (9-section, 500-650 lines)
   g. Run pre-implementation gates (3/3 must pass)
3. Commit: "feat: RW-XXX Complete rewrite of example-name"
4. Push: git push -u origin claude/rewrite-examples-3Sf77
5. Report: Status, lines of code, gate results

DELIVERABLES:
- Complete example infrastructure per example
- All files committed and pushed
- Gate pass/fail report
- Token usage report

MAXIMUM EXECUTION TIME:
- 2 examples per agent: 60-90 minutes
- 3-4 examples per agent: 90-120 minutes
```

---

## 6. Commit Message Template

```
feat: RW-XXX Complete rewrite of example-name

Implemented comprehensive example following RW-001 gold standard:

INFRASTRUCTURE:
- ggen.toml: [X] rules description
- ontology/name.ttl: [X] classes, SHACL validation, [Y] instances
- templates/: [Z] Tera templates (description)
- golden/: Expected outputs
- validate.mjs: Node.js validation script ([N] checks)

DOCUMENTATION:
- README.md: 9-section format ([X] lines)
- Key topics: description

METRICS:
- ggen.toml: X lines
- ontology.ttl: Y lines
- Templates: Z lines total
- README: W lines

QUALITY GATES:
- Gate 1 (Spec Closure): ✓ PASS
- Gate 2 (README): ✓ PASS
- Gate 3 (ggen.toml): ✓ PASS
- Gates 4-9: Deferred (requires built binary)

Next: RW-YYY description
```

---

## 7. Gold Standard Reference (RW-001)

Located at: `examples/regeneration-framework-setup/`

**Must extract from RW-001**:
- Ontology design patterns (classes, properties, SHACL)
- Template structure (frontmatter + body)
- README section organization
- Validation script patterns
- Golden file approach
- ggen.toml rule organization

**Every example should mirror RW-001 structure** (within domain-specific variations)

---

## 8. Parallel Execution Constraints

### No Dependencies Between Agents
- Each agent works completely independently
- No agent waits for another
- Agents can produce identical solutions (collision = high confidence)

### Output Isolation
- Each agent produces complete example infrastructure
- Files written to: `examples/EXAMPLE-NAME/`
- No cross-agent file modifications

### Git Operations
- Each agent: `git add`, `git commit`, `git push`
- All push to same branch: `claude/rewrite-examples-3Sf77`
- Sequential commits (each agent commits independently)

### Failure Isolation
- If agent N fails: agents 1-9 continue (no cascading failure)
- Failed agent outputs can be restarted/merged later
- Partial completion is acceptable

---

## 9. Success Criteria

### Per Agent
- [ ] All assigned examples completed
- [ ] All files committed and pushed
- [ ] Pre-implementation gates: 3/3 pass
- [ ] No token limit exceeded
- [ ] Clear commit messages

### Collective (EPIC 9 Level)
- [ ] FAN-OUT: Specification validated
- [ ] CONSTRUCTION: 30+ examples completed
- [ ] COLLISION: Overlaps identified and documented
- [ ] CONVERGENCE: Best solutions selected
- [ ] REFACTORING: All outputs standardized
- [ ] CLOSURE: Completion verified
- [ ] ANDON: GREEN signal achieved

---

## 10. Estimated Timeline (Parallel)

**Total Wall-Clock Time** (with 10 parallel agents):
- FAN-OUT: 5 minutes
- CONSTRUCTION: 90-120 minutes (parallel)
- COLLISION: 30 minutes
- CONVERGENCE: 30 minutes
- REFACTORING: 30 minutes
- CLOSURE: 30 minutes
- **Total**: 3-3.5 hours (vs 20+ hours sequential)

**2.8-4.4x speedup achieved through parallelization**

---

## 11. Specification Closure Verification

This specification is CLOSED when:

- [x] Architecture diagrams (C4) complete
- [x] Example infrastructure template defined
- [x] Quality gates specified
- [x] Agent task template defined
- [x] Gold standard documented
- [x] Success criteria clear
- [x] Timeline estimated

**CLOSURE STATUS**: ✓ COMPLETE

Proceed to EPIC 9 FAN-OUT phase.

---

**Next Action**: Execute `/bb80-parallel` to spawn 10 agents
