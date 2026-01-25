<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [FMEA: New User Onboarding Failure Modes](#fmea-new-user-onboarding-failure-modes)
  - [Executive Summary](#executive-summary)
  - [FMEA Table: Prioritized by RPN](#fmea-table-prioritized-by-rpn)
  - [CRITICAL FAILURES: Deep Dives & Mitigations](#critical-failures-deep-dives--mitigations)
    - [Failure 1: User Gets Lost in README (RPN 567)](#failure-1-user-gets-lost-in-readme-rpn-567)
    - [Failure 2: User Doesn't Understand Workflow (RPN 504)](#failure-2-user-doesnt-understand-workflow-rpn-504)
    - [Failure 3: Error Messages Unhelpful/Cryptic (RPN 432)](#failure-3-error-messages-unhelpfulcryptic-rpn-432)
    - [Failure 4: Template Has Syntax Errors (RPN 216)](#failure-4-template-has-syntax-errors-rpn-216)
    - [Failure 5: Generated Code Doesn't Compile (RPN 240)](#failure-5-generated-code-doesnt-compile-rpn-240)
  - [MEDIUM FAILURES: Summary & Mitigations](#medium-failures-summary--mitigations)
    - [Failure 6: ggen.toml Malformed (RPN 168)](#failure-6-ggentoml-malformed-rpn-168)
    - [Failure 7: Invalid Turtle Syntax (RPN 168)](#failure-7-invalid-turtle-syntax-rpn-168)
    - [Failure 8: Manual Edits Overwritten (RPN 180)](#failure-8-manual-edits-overwritten-rpn-180)
    - [Failure 9: Installation Fails (RPN 108)](#failure-9-installation-fails-rpn-108)
  - [Prevention Strategy: Integration Back to README](#prevention-strategy-integration-back-to-readme)
    - [Phase 1: Tutorial Design (Prevent Failures 1, 2, 8)](#phase-1-tutorial-design-prevent-failures-1-2-8)
    - [Phase 2: Error Message Improvements (Prevent Failure 3)](#phase-2-error-message-improvements-prevent-failure-3)
    - [Phase 3: Validator Tools (Prevent Failures 4, 6, 7)](#phase-3-validator-tools-prevent-failures-4-6-7)
    - [Phase 4: Documentation (Prevent All)](#phase-4-documentation-prevent-all)
  - [Success Criteria: Verify FMEA Mitigations](#success-criteria-verify-fmea-mitigations)
    - [By Failure Mode](#by-failure-mode)
    - [Testing & Validation Approach](#testing--validation-approach)
  - [Recommended Mitigation Priority](#recommended-mitigation-priority)
    - [Phase 1 (Week 1): Critical Failures](#phase-1-week-1-critical-failures)
    - [Phase 2 (Week 2): High Failures](#phase-2-week-2-high-failures)
    - [Phase 3 (Week 3): Medium Failures](#phase-3-week-3-medium-failures)
  - [Appendix: FMEA Scoring Scale](#appendix-fmea-scoring-scale)
    - [Severity (1-10)](#severity-1-10)
    - [Occurrence (1-10)](#occurrence-1-10)
    - [Detection (1-10)](#detection-1-10)
    - [RPN (Risk Priority Number)](#rpn-risk-priority-number)
  - [Document Review](#document-review)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# FMEA: New User Onboarding Failure Modes

**Document**: Failure Mode & Effects Analysis for ggen new user experience
**Version**: 1.0
**Date**: 2026-01-03
**Scope**: Complete new user journey from "I heard about ggen" to "I generated working code"

---

## Executive Summary

**14 failure modes identified** across installation, configuration, and workflow understanding.

**Critical Failures** (RPN > 300):
1. **User doesn't understand workflow** (RPN 504) → Mitigation: Tutorial with clear goals
2. **User gets lost in README** (RPN 567) → Mitigation: Diataxis restructuring
3. **Error messages unhelpful** (RPN 432) → Mitigation: Structured error context
4. **Template syntax errors** (RPN 216) → Mitigation: Template validator
5. **Generated code doesn't compile** (RPN 240) → Mitigation: Type-safe templates

**Medium Failures** (RPN 50-300):
- Malformed ggen.toml (RPN 168)
- Invalid Turtle ontology (RPN 168)
- Files overwritten losing manual edits (RPN 180)
- Installation failures (RPN 108)
- Docker volume mounting fails (RPN 80)

---

## FMEA Table: Prioritized by RPN

| # | Failure Mode | Severity | Occur | Detect | RPN | Priority | Root Cause | Mitigation Strategy |
|---|---|---|---|---|---|---|---|---|
| 1 | **User gets lost in README** | 7 | 9 | 9 | **567** | 🔴 CRITICAL | Current structure not Diataxis; mixed content types | Restructure README: tutorials first, scannable sections, clear "what's next" paths |
| 2 | **User doesn't understand workflow** | 7 | 8 | 9 | **504** | 🔴 CRITICAL | No clear explanation of "ggen reads ontology → feeds to template → generates code" | Add "What just happened" explanations in every tutorial; add architecture diagram |
| 3 | **Error messages unhelpful/cryptic** | 6 | 8 | 9 | **432** | 🔴 CRITICAL | Error messages don't provide context; users don't know where to find help | Structured error output: "What happened" + "Why" + "How to fix" + "Learn more" |
| 4 | **Template has syntax errors** | 6 | 9 | 4 | **216** | 🔴 CRITICAL | Tera templates unfamiliar; hard to debug; errors occur at render time | Template validator tool; example templates with comments; error message improvement |
| 5 | **Generated code doesn't compile** | 8 | 6 | 5 | **240** | 🔴 CRITICAL | Mismatch between ontology structure and template assumptions; no type validation | Type-safe template API; validate template against ontology before render |
| 6 | **ggen.toml malformed** | 8 | 7 | 3 | **168** | 🟡 HIGH | TOML syntax unfamiliar; no inline schema validation | Config validator; generated config template; inline schema hints in CLI |
| 7 | **Invalid Turtle syntax in ontology** | 7 | 8 | 3 | **168** | 🟡 HIGH | Turtle format unfamiliar; RDF validation errors cryptic | Turtle validator; example ontologies; error message shows "line X: expected Y, got Z" |
| 8 | **Manual edits overwritten** | 9 | 5 | 4 | **180** | 🟡 HIGH | Users don't know about incremental mode or MANUAL markers | Incremental mode default; MANUAL marker explained in Tutorial 1; warning before full sync |
| 9 | **Installation fails (platform-specific)** | 9 | 6 | 2 | **108** | 🟡 MEDIUM | Homebrew/Cargo/Docker issues; network problems; OS conflicts | Pre-flight check script; platform detection; fallback installation paths |
| 10 | **Docker volume mount fails** | 8 | 5 | 2 | **80** | 🟡 MEDIUM | Path permission issues; platform differences; user confusion | Dedicated Docker guide; permission troubleshooting; docker-compose examples |
| 11 | **Ontology file not found** | 8 | 8 | 2 | **128** | 🟡 MEDIUM-HIGH | Path mismatches between ggen.toml and actual files; user confusion about relative paths | Path validation in CLI; suggest nearest matching file; error shows "looked in X, Y, Z" |
| 12 | **Marketplace package doesn't exist** | 5 | 4 | 2 | **40** | 🟢 LOW | Typo or outdated package name | Better error message with search suggestions; package registry search help |
| 13 | **Performance issues (sync too slow)** | 4 | 3 | 7 | **84** | 🟡 MEDIUM | Large ontologies or complex templates | Progress indicator; SLO targets; performance troubleshooting guide in ADVANCED.md |
| 14 | **Path traversal attempt** | 10 | 2 | 2 | **40** | 🟢 LOW | User tries to write to protected paths | Type-safe path protection enforced; clear error explains why path blocked |

---

## CRITICAL FAILURES: Deep Dives & Mitigations

### Failure 1: User Gets Lost in README (RPN 567)

**What Could Go Wrong**:
- New user reads README, sees 488 lines of mixed content
- Doesn't know where to start: installation? explanation? how-to?
- Reads "What is ggen?" section (explanation) before understanding what it does
- Gets confused by RDF/SPARQL terminology upfront
- Gives up before reaching actionable content

**Current Evidence of Problem**:
- README currently has explanation BEFORE tutorial
- How-to guides scattered throughout
- No clear path for different user types
- No scannable section headings

**Severity: 7** (User gives up, never returns)
**Occurrence: 9** (Very common for new users with long docs)
**Detection: 9** (Detected too late - user already gone)

**Mitigation (PREVENT)**:
```markdown
# NEW README STRUCTURE (Diataxis)

## 🚀 Quick Start (30 seconds to "Aha!")
   → Installation + One command

## 📚 Tutorials (4 learning paths, 5-15 min each)
   1. Your First Generated Code (copy-paste, no thinking)
   2. Generate from Your Existing Schema (adapt example)
   3. Create Your First Template (customize)
   4. Set Up CI/CD (automate)

## 🛠️ How-To Guides (Problem-oriented)
   → When to use each guide

## 📖 Explanations (Understanding)
   → Why ontologies matter
   → Core concepts (RDF, SPARQL, Tera, Manifest)

## 📋 Reference (Facts)
   → Essential commands/config only
   → Links to full docs

## What's Next (By user type)
   → I want to learn → [tutorials]
   → I want to solve X problem → [how-to]
   → I'm stuck → [troubleshooting]
```

**Mitigation (DETECT)**:
- Add "You are here →" breadcrumbs in each section
- Add estimated time for each tutorial
- Add "When to use this guide" for how-tos
- Add progress indicator: "Step 2 of 4"

**Mitigation (REACT)**:
- Add TROUBLESHOOTING.md for common problems
- Add QUICK_REFERENCE.md for users who prefer one-page guides
- Add links to Discord/GitHub issues from every doc

---

### Failure 2: User Doesn't Understand Workflow (RPN 504)

**What Could Go Wrong**:
- User reads tutorials but doesn't understand the conceptual flow
- "So ggen takes my Turtle file and... what happens next?"
- User confused about what ontology vs template vs config does
- After first sync, user says "OK now what?" with no direction

**Current Evidence of Problem**:
- README explains RDF/SPARQL upfront (too abstract)
- No "What just happened" explanation after Tutorial 1
- No clear mental model: "Ontology → SPARQL → Template → Output"
- Missing architecture diagram showing the flow

**Severity: 7** (User gives up or uses wrong features)
**Occurrence: 8** (Normal for all new users)
**Detection: 9** (Detected when user asks "what now?")

**Mitigation (PREVENT)**:
```markdown
### What Just Happened

You just ran ggen through 3 steps:

1. **Read ontology** (schema/domain.ttl)
   └─ ggen loaded RDF triples about Person, name, email

2. **Match to template** (templates/rust-struct.tera)
   └─ Template said "for each class, create a struct"

3. **Generate code** (src/generated/domain.rs)
   └─ Output: Rust struct with fields from ontology

Next time you change domain.ttl, run `ggen sync` again.
New code will be generated automatically.

### ASCII Flow Diagram

    ontology/          templates/            output/
    domain.ttl    →    rust-struct.tera  →   domain.rs
    (what exists) (how to generate)     (what to create)
```

**Mitigation (DETECT)**:
- Add flowchart/ASCII diagram in every tutorial
- Add "What just happened" section explaining each step
- Add "Why" explanations, not just "how"

**Mitigation (REACT)**:
- Add FAQ section: "I generated code, now what?"
- Add "Next steps" at end of each tutorial
- Link to example projects showing full workflow

---

### Failure 3: Error Messages Unhelpful/Cryptic (RPN 432)

**What Could Go Wrong**:
```bash
$ ggen sync
Error: Manifest version mismatch (expected 2, found 1)
```

User thinks: "What's a manifest? How do I fix this? Where do I look?"

User is stuck. Gives up.

**Current Evidence of Problem**:
- Error messages don't explain "why"
- No link to help/troubleshooting
- RDF validation errors very cryptic
- No suggestion for how to fix

**Severity: 6** (User blocked, confused)
**Occurrence: 8** (Common with CLI tools)
**Detection: 9** (Detected when user can't proceed)

**Mitigation (PREVENT)**:
```rust
// Structured error format:
pub fn format_error(error: Error) -> String {
    format!(
        "❌ {}\n\n\
         📝 What happened:\n{}\n\n\
         🤔 Why:\n{}\n\n\
         🔧 How to fix:\n{}\n\n\
         📚 Learn more:\n{}",
        error.title,
        error.what_happened,
        error.why,
        error.how_to_fix,
        error.learn_more_url
    )
}

// Example output:
// ❌ Manifest Version Mismatch
//
// 📝 What happened:
//    Found manifest version 1, expected version 2
//
// 🤔 Why:
//    Your ggen.toml was created with an older ggen version.
//
// 🔧 How to fix:
//    Run: ggen migrate-manifest --from 1 --to 2
//
// 📚 Learn more:
//    https://docs.ggen.io/manifest-versions
```

**Mitigation (DETECT)**:
- Validate config during load, catch errors early
- Show "suggestion" for common mistakes
- Include file:line info when possible
- Colorize output (error in red, suggestion in yellow)

**Mitigation (REACT)**:
- TROUBLESHOOTING.md lists 20+ common errors
- Every error message links to troubleshooting section
- GitHub issue template includes error output

---

### Failure 4: Template Has Syntax Errors (RPN 216)

**What Could Go Wrong**:
```tera
{% for class in classes %}
  pub struct {{ class.name }} {
    {% for prop in class.properties %}  {# MISSING # #}
      pub {{ prop.name }}: {{ prop.type }},
    {% endfor %}
  }
{% endfor %}
```

Error:
```
Error: Template error: unclosed for block
  in template at line 2
```

User: "What does that mean? What's a 'for block'? Where's the problem?"

**Severity: 6** (Generated code broken)
**Occurrence: 9** (Tera unfamiliar, syntax errors very common)
**Detection: 4** (Errors occur at render time, message cryptic)

**Mitigation (PREVENT)**:
```bash
# Add template validator command:
$ ggen validate-template templates/rust-struct.tera

✅ Syntax valid
✅ All variables available
✅ Filters recognized
⚠️  Unused filter: |capitalize

Ready to use in ggen sync
```

**Mitigation (DETECT)**:
- Run template validation before full sync
- Show line number of error with context
- Suggest fix (e.g., "unclosed {%for%} - add {% endfor %}")
- Provide example template with comments

**Mitigation (REACT)**:
- Example templates in EXAMPLE_TEMPLATES.md
- Tera syntax guide in EXPLANATIONS.md
- Error message links to Tera docs

---

### Failure 5: Generated Code Doesn't Compile (RPN 240)

**What Could Go Wrong**:
```rust
// Generated by ggen
pub struct Person {
    pub name: String,
    pub age: NonExistentType,  // Template error
}
```

```bash
$ cargo build
error[E0433]: cannot find type `NonExistentType` in this scope
   --> src/generated/domain.rs:5:17
```

User thinks: "The template is broken? The ontology is wrong? How do I debug this?"

**Severity: 8** (Broken output, blocks user)
**Occurrence: 6** (Happens when ontology and template mismatch)
**Detection: 5** (User runs cargo build, but output suggests problem in ggen)

**Mitigation (PREVENT)**:
```rust
// Type-safe template generation:
pub struct TypedTemplate {
    // Enforce that template can only use valid Rust types
    filters: HashMap<String, ValidRustType>,
}

impl TypedTemplate {
    pub fn render(&self, ontology: &Ontology) -> Result<String> {
        // Validate all template variables match ontology BEFORE rendering
        for var in self.template.variables() {
            self.validate_variable(var, ontology)?;
        }
        // Now safe to render
        Ok(self.tera_template.render_ontology(ontology)?)
    }
}
```

**Mitigation (DETECT)**:
- Add pre-render validation step
- Check: "Does template produce valid Rust syntax?"
- Run `rustfmt` on generated code before writing
- Link generated code back to template line numbers

**Mitigation (REACT)**:
- TROUBLESHOOTING.md section: "Generated code doesn't compile"
- Show which ontology field caused the problem
- Suggest template line that needs fixing
- Link to template validation guide

---

## MEDIUM FAILURES: Summary & Mitigations

### Failure 6: ggen.toml Malformed (RPN 168)

```toml
# ❌ COMMON MISTAKES
[project]
name = my-project        # Missing quotes!
version = 0.1.0

[generation]
ontology_dir = "schema/
                       # Missing closing quote!
output_dir = "generated"
```

**Mitigations**:
- ✅ **PREVENT**: Config validator that validates on CLI load
- ✅ **DETECT**: Show exact line/column of error
- ✅ **REACT**: Suggest valid TOML; offer to auto-fix common issues

---

### Failure 7: Invalid Turtle Syntax (RPN 168)

```turtle
# ❌ COMMON MISTAKES
ex:Person a rdfs:Class
    rdfs:label "Person" .  # Missing semicolon before next property

ex:email a rdf:Property ;
    rdfs:domain ex:Person
    rdfs:range rdfs:Literal .  # Missing semicolon
```

**Mitigations**:
- ✅ **PREVENT**: Turtle validator command
- ✅ **DETECT**: Show "line 3: expected ';' or '.', found identifier"
- ✅ **REACT**: TROUBLESHOOTING.md with common Turtle errors

---

### Failure 8: Manual Edits Overwritten (RPN 180)

User creates `domain.rs`, adds validation logic, then runs `ggen sync --mode full` again → manual code lost

**Mitigations**:
- ✅ **PREVENT**: Default to incremental mode
- ✅ **DETECT**: Warn before full sync if existing file has manual edits
- ✅ **REACT**: MANUAL marker explained in Tutorial 1; recovery guide in TROUBLESHOOTING.md

---

### Failure 9: Installation Fails (RPN 108)

```bash
# Homebrew fails on Apple Silicon
brew install seanchatmangpt/ggen/ggen
# Error: Formula not available for ARM64

# Cargo fails due to dependency conflicts
cargo install ggen-cli-lib
# error: feature `std` is required for this crate
```

**Mitigations**:
- ✅ **PREVENT**: Pre-flight check script that detects platform/arch
- ✅ **DETECT**: Clear error message showing OS/arch/requirements
- ✅ **REACT**: Installation troubleshooting guide; fallback methods

---

## Prevention Strategy: Integration Back to README

### Phase 1: Tutorial Design (Prevent Failures 1, 2, 8)

✅ Tutorial 1: "Your First Generated Code"
- Clear goal: "I'll generate Rust code in 5 minutes"
- Includes: "What just happened" explanation
- Includes: MANUAL marker explanation
- Includes: "What's next" direction

✅ Tutorial 2: "Generate from Your Existing Schema"
- For users with existing OpenAPI/schemas
- Shows workflow: convert → configure → generate

✅ Tutorial 3: "Create Your First Template"
- Template validator preview
- Example template with comments
- Common errors explained

✅ Tutorial 4: "Set Up CI/CD"
- Shows how to validate in GitHub Actions
- Explains manifest file purpose
- Shows dry-run usage

### Phase 2: Error Message Improvements (Prevent Failure 3)

```rust
// Every error should follow:
// ❌ [Title]
// 📝 What happened: [description]
// 🤔 Why: [root cause]
// 🔧 How to fix: [steps]
// 📚 Learn more: [link]
```

### Phase 3: Validator Tools (Prevent Failures 4, 6, 7)

```bash
# Add new validation commands
ggen validate-config         # Check ggen.toml syntax
ggen validate-template FILE  # Check Tera template
ggen validate-ontology FILE  # Check Turtle RDF
```

### Phase 4: Documentation (Prevent All)

✅ README (Diataxis structure)
✅ TROUBLESHOOTING.md (20+ common errors)
✅ QUICK_REFERENCE.md (one-page guide)
✅ DOCKER.md (Docker-specific guide)
✅ ADVANCED.md (advanced config)

---

## Success Criteria: Verify FMEA Mitigations

### By Failure Mode

| Failure | Before | After | Verification |
|---------|--------|-------|---|
| User lost in README | 567 | 150 | (Diataxis structure + breadcrumbs) |
| User doesn't understand workflow | 504 | 80 | (Workflow diagrams + "what happened" explanations) |
| Error messages cryptic | 432 | 100 | (Structured error format + links) |
| Template syntax errors | 216 | 40 | (Validator command + example templates) |
| Generated code doesn't compile | 240 | 50 | (Pre-render validation + type-safe templates) |
| **Total Risk** | **2,389** | **420** | **82% risk reduction** |

### Testing & Validation Approach

**Test 1: New User Follows Tutorial 1 (5 minutes)**
- Can user get from zero to generated code in 5 min?
- Can user understand "what just happened"?
- Does user know what's next?

**Test 2: New User Encounters Error**
- Is error message helpful?
- Can user find troubleshooting guide?
- Can user fix problem without external help?

**Test 3: New User with Docker**
- Does Docker-based user have clearer path?
- Are volume mount issues documented?
- Are platform-specific issues addressed?

**Test 4: New User with Existing Schema**
- Can Tutorial 2 path work end-to-end?
- Is conversion process clear?
- Are next steps obvious?

---

## Recommended Mitigation Priority

### Phase 1 (Week 1): Critical Failures
1. Restructure README (Diataxis) → RPN 567 → 150
2. Add workflow diagrams → RPN 504 → 80
3. Improve error messages → RPN 432 → 100
4. Create TROUBLESHOOTING.md → All failures

### Phase 2 (Week 2): High Failures
5. Add template validator → RPN 216 → 40
6. Add config validator → RPN 168 → 30
7. Add ontology validator → RPN 168 → 30
8. Add pre-render validation → RPN 240 → 50

### Phase 3 (Week 3): Medium Failures
9. Create DOCKER.md guide → RPN 80
10. Add incremental mode default → RPN 180 → 40
11. Create QUICK_REFERENCE.md → RPN 567
12. Add pre-flight check script → RPN 108

---

## Appendix: FMEA Scoring Scale

### Severity (1-10)
- **10**: System crash, data loss, security breach
- **8-9**: Feature broken, user blocked, critical path affected
- **6-7**: Feature degraded, workaround exists
- **4-5**: Minor inconvenience, low impact
- **1-3**: Cosmetic issue, no functional impact

### Occurrence (1-10)
- **9-10**: Daily, very common
- **7-8**: Weekly, common
- **5-6**: Monthly, moderate
- **3-4**: Quarterly, rare
- **1-2**: Yearly, very rare

### Detection (1-10)
- **9-10**: Almost never detected by customer
- **7-8**: Detected by user after significant use
- **5-6**: Detected during integration testing
- **3-4**: Detected during unit testing
- **1-2**: Detected before release, impossible to miss

### RPN (Risk Priority Number)
- **RPN > 300**: CRITICAL - Stop and fix immediately
- **RPN 150-300**: HIGH - Fix in current phase
- **RPN 50-150**: MEDIUM - Plan and schedule
- **RPN < 50**: LOW - Monitor and document

---

## Document Review

- **Created**: 2026-01-03
- **For branch**: `claude/restructure-readme-diataxis-IGoXt`
- **Status**: Ready for implementation
- **Next step**: Integrate FMEA findings into README restructuring

---

## See Also

- [README.md](README.md) - Current (to be restructured)
- [Plan Output](PLAN-DIATAXIS-RESTRUCTURING.md) - README restructuring plan
- [Poka-Yoke Patterns](https://www.lean.org/lexicon-terms/poka-yoke/) - Reference
- [Toyota FMEA Guide](https://www.sae.org/) - Best practices
