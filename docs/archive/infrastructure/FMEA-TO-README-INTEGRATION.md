<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [FMEA Findings → README Restructuring Integration](#fmea-findings-%E2%86%92-readme-restructuring-integration)
  - [Critical Failure Modes & README Solutions](#critical-failure-modes--readme-solutions)
    - [1. Failure: "User Gets Lost in README" (RPN 567 → 150)](#1-failure-user-gets-lost-in-readme-rpn-567-%E2%86%92-150)
    - [2. Failure: "User Doesn't Understand Workflow" (RPN 504 → 80)](#2-failure-user-doesnt-understand-workflow-rpn-504-%E2%86%92-80)
    - [3. Failure: "Error Messages Unhelpful" (RPN 432 → 100)](#3-failure-error-messages-unhelpful-rpn-432-%E2%86%92-100)
    - [❌ Error: "Invalid Turtle syntax on line 5"](#-error-invalid-turtle-syntax-on-line-5)
    - [Step 3: Validate Your Template (1 min)](#step-3-validate-your-template-1-min)
    - [Step 4: Use Your Template (1 min)](#step-4-use-your-template-1-min)
    - [Troubleshooting: Template Errors](#troubleshooting-template-errors)
    - [Next Steps](#next-steps)
    - [Step 2: Check ontology matches template](#step-2-check-ontology-matches-template)
    - [Step 3: Run generated code through rustfmt](#step-3-run-generated-code-through-rustfmt)
    - [Step 4: Get help](#step-4-get-help)
    - [Common Turtle mistakes](#common-turtle-mistakes)
    - [Validate your Turtle](#validate-your-turtle)
    - [Sync modes:](#sync-modes)
    - [Best practice](#best-practice)
    - [Oops, I overwrote my code!](#oops-i-overwrote-my-code)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# FMEA Findings → README Restructuring Integration

**Purpose**: Map FMEA failure modes to specific README changes that prevent them

**Status**: Integration guide for implementing FMEA mitigations via Diataxis restructuring

---

## Critical Failure Modes & README Solutions

### 1. Failure: "User Gets Lost in README" (RPN 567 → 150)

**Root Cause**: Current README mixes 4 Diataxis types without clear structure

**FMEA Mitigation**: Restructure into Diataxis format

**README Changes**:

```markdown
OLD STRUCTURE (488 lines, confusing):
├── Explanation: "What is ggen?"
├── Tutorial: Quick Start
├── How-To: 6 guides
├── Reference: Commands/Config
└── Links

NEW STRUCTURE (Diataxis, scannable):
├── Hero (30 lines) - "Is this for me?"
├── Tutorials (350 lines) - 4 learning paths
│   ├── 1. Your First Code (5 min)
│   ├── 2. Existing Schema (10 min)
│   ├── 3. Custom Template (15 min)
│   └── 4. CI/CD Setup (10 min)
├── How-To (150 lines) - "When to use each guide"
├── Explanation (150 lines) - "Why" questions
├── Reference (100 lines) - Essential only
└── What's Next (20 lines) - By user type
```

**Specific Implementation**:
- ✅ Move explanation AFTER tutorials
- ✅ Replace generic "Quick Start" with 4 distinct tutorial paths
- ✅ Add "When to use" for every how-to guide
- ✅ Add breadcrumbs: "You are here →"
- ✅ Add time estimates: "⏱️ 5 minutes"
- ✅ Add progress: "Step 2 of 4"

**Verification Criteria**:
- [ ] New user can identify their path in < 30 seconds
- [ ] Tutorials ordered by complexity (simple → advanced)
- [ ] No section heading leaves user wondering "Is this for me?"
- [ ] Clear next step at end of each section

---

### 2. Failure: "User Doesn't Understand Workflow" (RPN 504 → 80)

**Root Cause**: No explanation of "Ontology → SPARQL → Template → Output" flow

**FMEA Mitigation**: Add workflow diagrams and "What just happened" sections

**README Changes in Tutorial 1**:

```markdown
## Tutorial 1: Your First Generated Code

### What You're About to Do
[ASCII Diagram showing flow]

    schema/domain.ttl  →  ggen sync  →  src/generated/domain.rs
    (RDF Ontology)      (What ggen does)   (Generated Rust code)

### Step 1-4: [Tutorial content]

### What Just Happened

ggen did 3 things:

1. **Read your ontology** (schema/domain.ttl)
   - Found RDF triples about Person, name, email

2. **Passed data to template** (templates/rust-struct.tera)
   - Template said: "For each class, create a struct"

3. **Generated code** (src/generated/domain.rs)
   - Result: Rust struct with fields matching ontology

### Why This Matters
- Your ontology is the SOURCE OF TRUTH
- Change ontology → run ggen sync → code updates
- Templates stay in sync with your domain model

### Next Step
→ [Tutorial 2: Generate from Your Existing Schema]
```

**Specific Implementation**:
- ✅ Add ASCII flowchart at START of each tutorial
- ✅ Add "What just happened" explanation AFTER first sync
- ✅ Add "Why this matters" (motivation)
- ✅ Add "Next step" (direction)

**Verification Criteria**:
- [ ] New user can explain to friend: "ggen reads ontology, feeds to template, generates code"
- [ ] After Tutorial 1, user understands that changing ontology triggers regeneration
- [ ] User can identify which file to edit if they want to change generated output

---

### 3. Failure: "Error Messages Unhelpful" (RPN 432 → 100)

**Root Cause**: Error messages don't explain "what, why, or how to fix"

**FMEA Mitigation**: Structured error format + links to troubleshooting

**README Changes - New TROUBLESHOOTING.md**:

```markdown
# Troubleshooting Guide

## Common Errors & Solutions

### ❌ Error: "Ontology file not found"

**What happened**:
ggen looked for schema/domain.ttl but couldn't find it

**Why it happened**:
1. File path in ggen.toml doesn't match actual file location
2. File doesn't exist yet
3. Relative path is wrong (should be relative to ggen.toml location)

**How to fix**:

**Option 1**: Create the missing file
```bash
# If schema/ directory doesn't exist:
mkdir schema/
touch schema/domain.ttl  # Add your RDF content
```

**Option 2**: Fix path in ggen.toml
```toml
# Wrong:
ontology_dir = "schemas/"  # but file is in "schema/"

# Right:
ontology_dir = "schema/"
```

**Verify**:
```bash
# Check what ggen is looking for:
ggen sync --verbose
# Output shows: "Looking for ontology in: schema/domain.ttl"

ls -la schema/domain.ttl  # Verify file exists
```

**Learn more**:
- [ggen.toml Reference](ADVANCED.md#configuration)
- [Project Structure](README.md#project-structure)

---

### ❌ Error: "Invalid Turtle syntax on line 5"

**What happened**:
ggen tried to parse your RDF ontology but failed on line 5

**Why it happened**:
Turtle format has strict syntax (like JSON/TOML)

**Common causes**:
```turtle
# ❌ WRONG: Missing semicolon before next property
ex:Person a rdfs:Class ;
    rdfs:label "Person"  # ← MISSING SEMICOLON!
    rdfs:comment "A person" .

# ✓ CORRECT:
ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A person" .
```

**How to fix**:

1. **Check the exact line**:
   - Error says: "line 5"
   - Open schema/domain.ttl, go to line 5

2. **Common fixes**:
   - Add missing `;` between properties
   - Add missing `.` at end of statement
   - Check for typos in prefixes
   - Ensure quoted strings are closed

3. **Validate**:
```bash
ggen validate-ontology schema/domain.ttl
# Shows: "Line 5: expected `;` or `.`, found identifier `rdfs:comment`"
```

**Learn more**:
- [Turtle Syntax Guide](docs/explanations/fundamentals/turtle-syntax.md)
- [Example Ontologies](examples/)

---

[10+ more common errors with same structure]
```

**Specific Implementation in README**:
- ✅ Add line: "If you get stuck: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)"
- ✅ Every error message in CLI includes link: "Learn more: ggen.io/errors#ontology-not-found"
- ✅ Add error code system for consistency

**Verification Criteria**:
- [ ] User encountering error can find solution in < 2 minutes
- [ ] Troubleshooting guide covers top 20 errors
- [ ] Every solution includes "Why" + "How to fix" + "Verify" sections

---

### 4. Failure: "Template Has Syntax Errors" (RPN 216 → 40)

**Root Cause**: Tera syntax unfamiliar; errors cryptic; no validator

**FMEA Mitigation**: Add validator command + example templates + tutorial

**README Changes in Tutorial 3**:

```markdown
## Tutorial 3: Create Your First Template

### Goal
Build a custom Tera template that generates code your way.

### Prerequisites
- Completed Tutorial 1
- Know basic Jinja syntax (5-min optional read)

### Step 1: Understand Template Anatomy (3 min)

Templates have 2 parts:
1. **Jinja syntax** (loops, conditionals, variables)
2. **Code generation** (the output)

### Step 2: Create Your First Template (7 min)

Example: templates/my-model.tera

```tera
// Generated by ggen
{% for entity in ontology.entities %}
pub struct {{ entity.name }} {
    {% for field in entity.fields %}
    pub {{ field.name }}: {{ field.rust_type }},
    {% endfor %}
}
{% endfor %}
```

**Template syntax breakdown**:
- `{% for ... %}` - Loop over items
- `{{ variable }}` - Insert variable value
- `{# comment #}` - Template comment (won't appear in output)

### Step 3: Validate Your Template (1 min)

```bash
# NEW COMMAND: Validate before using
ggen validate-template templates/my-model.tera

# Output:
# ✅ Syntax valid
# ✅ All filters recognized
# ✅ Block statements closed properly
# Ready to use in ggen sync
```

### Step 4: Use Your Template (1 min)

```bash
ggen sync
```

### Troubleshooting: Template Errors

**Common error: Unclosed for block**
```
Error: Template error: unclosed for block
  in template at line 3
```

**Why**:
```tera
{% for field in entity.fields %}
  pub {{ field.name }}: {{ field.type }},
  {# MISSING: {% endfor %} #}
```

**Fix**:
```tera
{% for field in entity.fields %}
  pub {{ field.name }}: {{ field.type }},
{% endfor %}  {# ← ADD THIS #}
```

**Prevent**:
```bash
# Always validate before sync:
ggen validate-template your-template.tera
```

### Next Steps
→ [How-to: Use SPARQL CONSTRUCT for Inference]
→ [Tera Template Reference](docs/reference/templates/)
→ [Example Templates](examples/)
```

**Specific Implementation**:
- ✅ Add validator command: `ggen validate-template`
- ✅ Include example templates with comments
- ✅ Add "Troubleshooting" subsection to Template tutorial
- ✅ Link to Tera docs and examples

**Verification Criteria**:
- [ ] New user can create working template without external help
- [ ] Template validator catches syntax errors before render
- [ ] Error messages suggest specific fixes

---

### 5. Failure: "Generated Code Doesn't Compile" (RPN 240 → 50)

**Root Cause**: Mismatch between ontology and template output types

**FMEA Mitigation**: Type-safe template validation + pre-render checks

**README Changes**:

```markdown
## If Your Generated Code Doesn't Compile

**Problem**: Generated code has compilation errors

**Causes**:
1. Template creates invalid Rust syntax
2. Template references ontology fields that don't exist
3. Type mismatch between ontology and generated code

**Diagnostic steps**:

### Step 1: Check template output
```bash
# See what template would generate WITHOUT writing:
ggen sync --dry-run

# If output looks wrong, check template syntax:
ggen validate-template templates/my-template.tera
```

### Step 2: Check ontology matches template
```bash
# Verify ontology has all fields template expects:
ggen validate-ontology schema/domain.ttl

# Should show:
# ✅ All required properties present
# ✅ No syntax errors
```

### Step 3: Run generated code through rustfmt
```bash
# Formatted code might reveal syntax issues:
rustfmt src/generated/domain.rs

# If rustfmt fails, template is generating invalid Rust
```

### Step 4: Get help
- [Template Troubleshooting](TROUBLESHOOTING.md#template-errors)
- [Type System Guide](docs/reference/templates/type-system.md)
- [GitHub Issues](https://github.com/seanchatmangpt/ggen/issues)
```

**Specific Implementation**:
- ✅ Add pre-render validation step in CLI
- ✅ Show which template line generated problematic code
- ✅ Link generated code back to template source
- ✅ Add TROUBLESHOOTING.md section with diagnostic steps

**Verification Criteria**:
- [ ] When generated code fails to compile, user can identify root cause in < 5 minutes
- [ ] Error message includes: template line number + ontology field + expected type

---

## Medium Failure Modes & Solutions

### 6. Failure: "ggen.toml Malformed" (RPN 168 → 30)

**README Changes**:
```markdown
# In Quick Reference section:

## Configuration: ggen.toml

Minimal example:
```toml
[project]
name = "my-project"      # ← Must be quoted string
version = "0.1.0"

[generation]
ontology_dir = "schema/"
templates_dir = "templates/"
output_dir = "src/generated/"
```

**Validate**:
```bash
ggen validate-config ggen.toml
# Shows: ✅ Valid TOML syntax
#        ✅ All required fields present
#        ✅ All paths exist
```

**See also**: [Complete ggen.toml Reference](ADVANCED.md#ggen-toml-reference)
```

---

### 7. Failure: "Invalid Turtle Syntax" (RPN 168 → 30)

**README Changes - Add Turtle Primer**:
```markdown
# In Explanation section:

## Understanding Turtle Syntax (RDF ontologies)

Turtle is like JSON for RDF - simple syntax, strict rules.

### Basic structure
```turtle
@prefix ex: <https://example.com/> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "Represents a person" .
```

**Parts**:
- `@prefix` - Define namespace shortcuts
- `;` - Semicolon between properties (required!)
- `.` - Period at end of statement (required!)
- `a` - Short for rdf:type
- String must be quoted

### Common Turtle mistakes
1. Missing semicolon: `rdfs:label "Person"` instead of `rdfs:label "Person" ;`
2. Missing period at end: No `.` after final property
3. Typo in prefix: `rdfs:Comment` instead of `rdfs:comment`

### Validate your Turtle
```bash
ggen validate-ontology schema/domain.ttl
# Shows exact line and column of error
```

**Learn more**: [Full Turtle Guide](docs/explanations/fundamentals/turtle-syntax.md)
```

---

### 8. Failure: "Manual Edits Overwritten" (RPN 180 → 40)

**README Changes in Tutorial 1**:
```markdown
## Preserving Your Custom Code

Generated files can be updated when your ontology changes.
To protect custom code:

### Method: MANUAL Markers
```rust
// Generated section (regenerated each sync)
#[derive(Debug)]
pub struct Person {
    pub name: String,
}

// MANUAL: Your custom code (preserved during incremental sync)
impl Person {
    pub fn validate(&self) -> Result<(), Error> {
        // This code survives regeneration
        Ok(())
    }
}
```

### Sync modes:

**full mode** (default): Regenerate everything
```bash
ggen sync --mode full
# ⚠️  Will overwrite ENTIRE file (including MANUAL sections)
# Use only during setup
```

**incremental mode** (safe for development): Update only changed parts
```bash
ggen sync --mode incremental
# ✅ Preserves MANUAL sections
# ✅ Only updates changed fields
# Use during active development
```

### Best practice
1. Set incremental as default in ggen.toml:
```toml
[generation]
incremental = true
```

2. Use MANUAL markers for custom logic:
```rust
// MANUAL: Custom validation
impl Validation { ... }
```

3. Run incremental sync:
```bash
ggen sync  # Uses incremental by default
```

### Oops, I overwrote my code!
See: [TROUBLESHOOTING.md#manual-code-lost](TROUBLESHOOTING.md#manual-code-lost)
```

---

### 9-10. Remaining Mitigations

These map to specific docs:

| Failure | Document | Solution |
|---------|----------|----------|
| Installation fails | README Quick Start + TROUBLESHOOTING.md | Pre-flight check; multiple install methods |
| Docker volume mount fails | DOCKER.md | Full Docker guide with troubleshooting |
| Ontology file not found | TROUBLESHOOTING.md | Path validation; suggest nearby files |
| Marketplace package missing | ADVANCED.md | Better error messages; search suggestions |
| Performance issues | ADVANCED.md | Progress indicator; SLO targets |

---

## Integration Checklist: Implementing FMEA Mitigations

### README Restructuring Tasks

- [ ] **Move explanation AFTER tutorials** (prevents RPN 567)
- [ ] **Add 4 distinct tutorial paths** (prevents RPN 504)
  - [ ] Tutorial 1: Your First Code (includes "what happened")
  - [ ] Tutorial 2: Existing Schema
  - [ ] Tutorial 3: Custom Template (includes validator)
  - [ ] Tutorial 4: CI/CD Setup
- [ ] **Add breadcrumbs** ("You are here →")
- [ ] **Add time estimates** ("⏱️ 5 minutes")
- [ ] **Add "What's Next"** after each section
- [ ] **Move advanced content to ADVANCED.md**

### New Document Creation Tasks

- [ ] **TROUBLESHOOTING.md** (20+ common errors) - prevents RPN 432
- [ ] **DOCKER.md** (full Docker guide) - prevents RPN 80
- [ ] **QUICK_REFERENCE.md** (one-page cheat sheet)
- [ ] **ADVANCED.md** (advanced config, performance, etc.)

### CLI/Tool Enhancement Tasks

- [ ] **Improve error messages** (structured format)
- [ ] **Add validator commands**:
  - [ ] `ggen validate-config` (TOML)
  - [ ] `ggen validate-template` (Tera)
  - [ ] `ggen validate-ontology` (Turtle)
- [ ] **Add pre-render validation**
- [ ] **Add dry-run improvements**

### Verification Tasks

- [ ] **Test Tutorial 1 end-to-end** (5 min)
- [ ] **Test Tutorial 2 with actual OpenAPI spec**
- [ ] **Test Tutorial 3 with template errors** (verify validator catches them)
- [ ] **Test Tutorial 4 GitHub Actions workflow**
- [ ] **Test error messages** (structure, helpfulness)
- [ ] **Test TROUBLESHOOTING.md solutions**

---

## Risk Reduction Summary

| Critical RPN | Before | After | Reduction |
|---|---|---|---|
| User lost in README | 567 | 150 | 73% ↓ |
| Don't understand workflow | 504 | 80 | 84% ↓ |
| Error messages unhelpful | 432 | 100 | 77% ↓ |
| Template syntax errors | 216 | 40 | 81% ↓ |
| Generated code doesn't compile | 240 | 50 | 79% ↓ |
| **TOTAL CRITICAL** | **1,959** | **420** | **78% ↓** |

| Medium RPN | Before | After |
|---|---|---|
| Malformed ggen.toml | 168 | 30 |
| Invalid Turtle syntax | 168 | 30 |
| Manual edits overwritten | 180 | 40 |
| Installation fails | 108 | 25 |
| Docker fails | 80 | 20 |
| **TOTAL MEDIUM** | **704** | **145** |

**Overall risk reduction: 80%**

---

## Next Steps

1. **Implement README restructuring** using plan from PLAN-DIATAXIS-RESTRUCTURING.md
2. **Create supporting documents** (TROUBLESHOOTING.md, DOCKER.md, ADVANCED.md, QUICK_REFERENCE.md)
3. **Enhance CLI error messages** with structured format
4. **Add validator commands** (validate-config, validate-template, validate-ontology)
5. **Test with real new users** - verify FMEA mitigations work
6. **Monitor and iterate** - collect feedback, update docs

---

**Status**: Ready for implementation
**Branch**: `claude/restructure-readme-diataxis-IGoXt`
**Owner**: Claude Code
**Last updated**: 2026-01-03
