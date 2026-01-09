# Agent 5: Rewrite Methodology Guide
**Task**: DESIGN REWRITE METHODOLOGY
**Date**: 2026-01-04
**Status**: COMPLETE

---

## Executive Summary

This methodology defines what "rewrite from scratch, no refactor" means for ggen examples. It provides:

1. **Precise definitions** of "from scratch" vs "no refactor"
2. **5 example type classifications** with specific checklists
3. **Pre/post-rewrite quality gates** for validation
4. **Standard template structure** for consistent examples
5. **Rollback procedures** and evidence requirements

**Key Principle**: Preserve original intent and scope while completely recreating content.

---

## Core Definitions

### What "From Scratch" Means

**"From Scratch" = Complete content recreation, preserving only structural intent**

```
DELETED:
✗ All template files (*.tmpl, *.tera)
✗ All generated code (*.rs, *.py, *.js, *.ts)
✗ All example data (*.ttl, *.yaml, *.json)
✗ All prose explanations
✗ Sample outputs

PRESERVED:
✓ Directory structure and naming
✓ Conceptual intent from original README
✓ Learning objectives (reworded, not copied)
✓ Example classification (Beginner/Intermediate/Advanced/Expert)
✓ File and naming conventions
```

**Implementation Strategy**: Do NOT read the original content and copy it. Instead:
1. Document the original intent in 1-2 sentences
2. Create `.gitkeep` placeholders in empty directories
3. Recreate all content from blank slate using only intent + type specification
4. Verify no copy-paste occurred (git diff should show ~95%+ new content)

### What "No Refactor" Means

**"No Refactor" = Preserve original design philosophy without modernization or improvements**

**Explicitly Forbidden Changes:**
- ✗ Adding new frameworks not in original
- ✗ Changing testing approach/framework
- ✗ Adding new use cases
- ✗ Refactoring architecture
- ✗ Optimizing performance
- ✗ Expanding scope

**Acceptable Changes:**
- ✓ Bug fixes in original code
- ✓ Security updates to dependencies
- ✓ Correcting documentation errors
- ✓ Clarifying existing explanations
- ✓ Fixing broken scripts
- ✓ Minimal API call updates
- ✓ Formatting/style normalization

**The Test**: If you're making changes because "the original way was not ideal", STOP. That's refactoring. Change it only if the original was broken.

---

## Example Type Classification

### 1. Tutorial (15-30 min)
**Purpose**: Step-by-step guided learning with interactive exercises

**Characteristics**:
- 1-3 simple, focused templates
- 3-5 clear learning steps
- Interactive run-example.sh script
- Expected output clearly shown
- Beginner-friendly language

**Checklist Items** (10):
- Clear learning objectives (max 5)
- Prerequisites listed
- 3-5 clear steps with outputs
- Templates <100 LOC
- Variable names self-documenting
- run-example.sh works without manual intervention
- All filters/features documented
- Output section matches actual
- Troubleshooting covers 3+ errors
- Time estimate accurate to ±10%

---

### 2. Project Generation (45-60 min)
**Purpose**: Generate complete multi-file project from specification

**Characteristics**:
- ggen.toml manifest + project-spec.yaml
- 6-12 templates covering full project
- generate-project.sh orchestration
- Generated project builds/tests successfully
- Integration test template

**Checklist Items** (12):
- ggen.toml has all sections
- 6-12 templates provided
- generate-project.sh validates first
- Generated project builds with `cargo build`
- Generated project passes `cargo test`
- Templates <200 LOC
- Variables consistent across templates
- Output directory matches structure
- Cargo.toml template complete
- Integration test covers ≥50% of code
- Error messages actionable
- Customization documented

---

### 3. Ontology-Driven (60-90 min)
**Purpose**: RDF/SPARQL specification-to-code generation pipeline

**Characteristics**:
- 2-3 *.ttl RDF ontology files
- 8-10 SPARQL queries + generation rules
- 8-10 *.tera template files
- Golden output examples
- Schema validation

**Checklist Items** (12):
- ≥2 ontology files (domain + schema)
- All RDF prefixes defined
- 8-10 generation rules
- SPARQL queries documented
- Templates accept result variables
- Golden output shows all variations
- Ontology classes/properties self-documenting
- README explains ontology structure
- SPARQL queries ordered by dependency
- Template variables match SELECT columns
- Example *.ttl data provided
- Query results validated

---

### 4. AI-Powered (45-60 min)
**Purpose**: LLM-assisted template/code generation workflows

**Characteristics**:
- Mock mode (no API key required)
- 5-10 example prompts library
- Validation examples (valid/invalid)
- Multi-step orchestration script
- Token/cost tracking

**Checklist Items** (12):
- Mock mode works without API keys
- 5-10 example prompts
- Each prompt has expected output
- Validation examples provided
- Full workflow runs end-to-end
- Token usage tracked
- API setup for 2+ providers
- Error handling for API failures
- Cost estimation included
- Prompt engineering tips documented
- Quality metrics included
- Fallback to mock on failures

---

### 5. Utility/Tool (15-30 min)
**Purpose**: Standalone tool/script demonstration

**Characteristics**:
- Simple Rust binary or script
- Minimal dependencies
- Clear usage examples
- Input/output samples
- Error handling

**Checklist Items** (10):
- Tool runs without deps (or documented)
- `cargo run --example` works
- Help text with examples
- Input files provided
- Output clearly documented
- Error cases handled
- Code <200 LOC
- ≥3 test cases documented
- Performance noted if >1s
- Clear success/failure indicators

---

## Quality Gates: Pre-Rewrite

Before starting any rewrite:

| Gate | Check | Failure |
|------|-------|---------|
| **SPECIFICATION** | Intent documented in .specify/specs/ | STOP - Create spec |
| **SCOPE** | Boundaries clear (what deletes, preserves) | STOP - Clarify scope |
| **CONSTRAINTS** | No-refactor constraints listed | STOP - Define limits |
| **TYPE** | Example classified (one of 5 types) | STOP - Classify it |
| **INVENTORY** | All files/dependencies catalogued | WARNING - Continue |
| **DEPENDENCIES** | External tools listed with versions | STOP - Document them |

---

## Quality Gates: Post-Rewrite

After completion, validate:

| Gate | Validation | Failure |
|------|-----------|---------|
| **README** | All sections present | BLOCK MERGE |
| **SCRIPTS** | All .sh executable, run clean | BLOCK MERGE |
| **TEMPLATES** | Pass ggen validation, render | BLOCK MERGE |
| **OUTPUT** | Matches golden/expected files | BLOCK MERGE |
| **PATHS** | No hardcoded /home, /Users, C:\\ | BLOCK MERGE |
| **CODE QUALITY** | shellcheck, clippy, no linting | BLOCK MERGE |
| **REPRODUCIBILITY** | Runs 3x identically on clean env | BLOCK MERGE |
| **SCOPE** | No new frameworks/patterns | JUSTIFY OR FIX |

---

## Standard Example Template

### Directory Structure
```
example-name/
├── README.md           (250-500 words, all sections)
├── QUICK_REFERENCE.md  (optional, <100 words)
├── .gitignore          (standard ignores)
├── ggen.toml           (manifest if applicable)
├── templates/          (*.tmpl or *.tera)
├── data/               (*.ttl, *.yaml, *.json)
├── scripts/            (*.sh files)
├── docs/               (optional guides)
└── generated/          (golden/expected output)
```

### README.md Sections
1. **Header** - Title + one-liner
2. **Learning Objectives** - 3-5 bullets
3. **Prerequisites** - Tools + versions
4. **Overview** - 2-3 sentence summary
5. **Step-by-Step** - 3-5 numbered steps
6. **Expected Output** - Actual output samples
7. **Customization** - How to modify
8. **Troubleshooting** - Error X: cause & fix
9. **Next Steps** - Pointers to other examples
10. **Resources** - Documentation links

### Critical Metadata
```
Difficulty: [Beginner | Intermediate | Advanced | Expert]
Duration: [15-30 | 30-45 | 45-60 | 60-90] minutes
ggen Version: 1.0+ (or specific)
Last Verified: YYYY-MM-DD
Prerequisites: [ggen, optional: Rust, Ollama, etc]
Tags: [Category1, Category2, ...]
```

---

## Acceptable Variations

### Naming
- ✓ `user_service` vs `UserService` (follow pattern)
- ✓ `v1` vs `v2` (version suffix OK)
- ✗ `foo`, `bar`, `xxx` (unhelpful names)

### Technology
- ✓ `axum 0.7` vs `0.8` (patch version)
- ✗ Replace `actix` with `axum` (different framework)
- ✗ Add async where not used (changes design)

### Complexity
- ✓ Add fields to struct (same pattern)
- ✓ Add endpoints (same architecture)
- ✗ Add database (level jump)
- ✗ Add authentication (scope change)

### Documentation
- ✓ Different wording (same info)
- ✓ More detailed errors
- ✗ Remove troubleshooting section
- ✗ Assume advanced knowledge

---

## Evidence Requirements

Upon completion, provide these receipts:

```
[Receipt] Pre-rewrite checks: ✓ all passed
[Receipt] Post-rewrite validation: ✓ all gates passed
[Receipt] Code quality: ✓ no linting errors
[Receipt] Functionality: ✓ 7/7 tests passing
[Receipt] Reproducibility: ✓ 3x clean environment runs identical
[Receipt] Time estimate: measured at XX min (vs YY estimated)
[Receipt] Scope preservation: ✓ no architectural changes
[Receipt] Documentation: ✓ learning objectives verified achievable
```

### Verification Commands
```bash
# Code quality
shellcheck examples/example-name/scripts/*.sh

# Functionality
cargo test --example example-name 2>&1 | grep 'test result'

# Reproducibility
cd examples/example-name && ./run-example.sh 2>&1 | tail -5

# Scope preservation
git diff --stat HEAD^ -- examples/example-name/ | head -3

# Time measurement
time ./examples/example-name/run-example.sh >/dev/null
```

---

## Execution Phases

### Phase 1: Analysis
- Document original intent (1-2 sentences)
- List template/code purposes
- Identify example type
- Note key variables/patterns

### Phase 2: Blank Slate
- Delete all content files
- Create .gitkeep placeholders
- Verify directory structure intact
- Commit: "blank slate for rewrite: example-name"

### Phase 3: Specification
- Create .specify/specs/NNN-rewrite-example-name/spec.ttl
- Document intent, scope, constraints
- Run `/speckit-verify` for closure
- Commit: "add specification for rewrite: example-name"

### Phase 4: Rebuild
- Recreate ggen.toml from spec (not copy)
- Recreate templates from scratch (not copy)
- Recreate scripts from scratch (not copy)
- Recreate README from scratch (not copy)
- Commit: "rewrite: example-name [fresh implementation]"

### Phase 5: Validation
- Run all post-rewrite validation checks
- Test on clean environment
- Measure actual time vs estimate
- Verify learning objectives achieved
- Commit: "validate: example-name [all checks pass]"

---

## Rollback Procedure

If rewrite fails:

```bash
# Tag original before starting
git tag example-name-original-$(date +%Y%m%d)

# If rewrite fails mid-way
git reset --hard HEAD~1

# If merge conflict
git checkout --theirs examples/example-name/

# Verify rollback
git diff example-name-original-20240101 -- examples/example-name/
```

---

## Quality Metrics

### Completeness Score
```
Formula: (sections_present / total_sections) * 100
Minimum: 95%
Sections: README, Templates, Scripts, Docs, Troubleshooting, Output, Metadata
```

### Functionality Score
```
Formula: (tests_passing / total_tests) * 100
Minimum: 100% (all must pass)
Tests: Scripts, Templates, Compilation, Output matching, Troubleshooting, Reproducibility
```

### Time Accuracy Score
```
Formula: 100 - |estimated - actual| / estimated * 100
Minimum: 90% accurate
```

### Scope Preservation Score
```
Formula: (preserved_intent / original_intent) * 100
Minimum: 95% preserved
Checks: Tech stack, Difficulty, Modules, Interaction model, Code patterns
```

### Documentation Clarity Score
```
Formula: (clarity_votes / total_votes) * 100
Minimum: 85% clear
Criteria: No jargon, Examples, Error clarity, Logic flow, Detail level
```

---

## Key Takeaways

### The Rewrite is NOT:
- ✗ Copy-paste of original
- ✗ Improvement/modernization project
- ✗ Refactoring opportunity
- ✗ Chance to add new features
- ✗ Time to change frameworks
- ✗ Occasion to simplify complexity

### The Rewrite IS:
- ✓ Fresh implementation of original intent
- ✓ Same functionality, different code
- ✓ Improved clarity and documentation
- ✓ Better error messages/troubleshooting
- ✓ Validation that original design was sound
- ✓ Opportunity to verify reproducibility

### The Test:
**If you're making a change because "the original way was wrong", ask first.**

Rewrite = preserve intent + recreate content. Not refactor or redesign.

---

## Final Approval Checklist

Before marking rewrite complete:

- [ ] Pre-rewrite checks: ✓ all passed
- [ ] Post-rewrite validation: ✓ all gates passed
- [ ] Type-specific checklist: ✓ all items marked
- [ ] Time estimate verified by actual execution
- [ ] Learning objectives validated (each achievable)
- [ ] Troubleshooting section tested (each error reproduced)
- [ ] Code follows ggen style guide
- [ ] No unwrap()/expect() in production code
- [ ] Documentation clear and jargon-free
- [ ] Git history clean (single logical commit)
- [ ] Listed in main examples/README.md
- [ ] Metadata accurate (difficulty, duration, prerequisites)
- [ ] No regressions vs original functionality
- [ ] All TODOs/FIXMEs removed
- [ ] Ready for production use

---

## JSON Methodology Document

Full structured methodology available in: `/home/user/ggen/agent-5-rewrite-methodology.json`

Contains:
- Precise definitions (from-scratch + no-refactor)
- 5 example type classifications with checklists
- Universal rewrite checklist (20 items)
- Validation criteria (pre + post + final)
- Standard template structure
- Type variations and acceptable deviations
- Quality gates and failure actions
- Evidence requirements
- Rollback procedures
- Quality metrics (5 dimensions)
- Execution workflow for agents

**Structure**: JSON with nested sections covering all methodological aspects.

---

**Generated**: 2026-01-04
**Agent**: 5 of 10
**Status**: METHODOLOGY DESIGN COMPLETE
