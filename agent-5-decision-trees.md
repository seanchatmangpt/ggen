# Agent 5: Decision Trees & Workflows

Reference guide for making methodology decisions during example rewrites.

---

## Decision Tree 1: Example Type Classification

```
START: What is this example?

├─ Does it teach a concept step-by-step?
│  └─ YES: Can complete in 15-30 min?
│     ├─ YES → TYPE: Tutorial
│     └─ NO (45-60 min) → TYPE: Tutorial (Extended)
│
├─ Does it generate a complete project?
│  └─ YES: From specification/manifest?
│     ├─ YES → TYPE: Project Generation
│     └─ NO → TYPE: Utility/Tool
│
├─ Does it use RDF/SPARQL/ontology?
│  └─ YES: Specification-driven generation?
│     ├─ YES → TYPE: Ontology-Driven
│     └─ NO → TYPE: Utility/Tool
│
├─ Does it use AI/LLM?
│  └─ YES: Generates code/templates?
│     ├─ YES → TYPE: AI-Powered
│     └─ NO → TYPE: Utility/Tool
│
└─ Single tool/script demo?
   └─ YES → TYPE: Utility/Tool
```

---

## Decision Tree 2: Scope Preservation Check

```
START: Validating no-refactor compliance

├─ Question 1: Am I changing the framework/library?
│  ├─ YES → STOP: Document reason, ask user
│  └─ NO → Continue
│
├─ Question 2: Am I adding features not in original?
│  ├─ YES → STOP: Clarify scope
│  └─ NO → Continue
│
├─ Question 3: Am I changing the testing approach?
│  ├─ YES → STOP: Not permitted (no refactor)
│  └─ NO → Continue
│
├─ Question 4: Am I making it async/concurrent if not originally?
│  ├─ YES → STOP: Not permitted (design change)
│  └─ NO → Continue
│
├─ Question 5: Am I changing the learning difficulty?
│  ├─ YES → STOP: Would change example audience
│  └─ NO → Continue
│
├─ Question 6: Am I fixing a bug or security issue?
│  ├─ YES → ALLOWED: Document in commit
│  └─ NO → Continue
│
├─ Question 7: Am I updating dependencies (patch/minor)?
│  ├─ YES → ALLOWED: Normal maintenance
│  └─ NO → Continue
│
└─ Question 8: Would original author recognize the design?
   ├─ YES → PROCEED: Scope preserved
   └─ NO → STOP: Re-evaluate changes
```

---

## Decision Tree 3: Validation Gate Failures

```
START: Post-rewrite validation failed

├─ Gate: README Incomplete
│  ├─ Missing sections?
│  │  └─ Add: [Learning Objectives | Prerequisites | Steps | Output | Troubleshooting]
│  └─ Sections too vague?
│     └─ Expand with examples and specific steps
│
├─ Gate: Scripts Not Executable
│  ├─ .sh not marked executable?
│  │  └─ chmod +x *.sh
│  └─ Scripts have errors?
│     ├─ Test on clean environment
│     ├─ Add error handling (set -e)
│     └─ Add helpful error messages
│
├─ Gate: Templates Invalid
│  ├─ YAML frontmatter malformed?
│  │  └─ Check: YAML syntax, required 'to:', 'vars:' keys
│  └─ Tera syntax errors?
│     ├─ Check: {{ variable }} for undefined vars
│     ├─ Check: {% for %} has matching {% endfor %}
│     └─ Validate with ggen template validation
│
├─ Gate: Output Doesn't Match Golden
│  ├─ Variables wrong?
│  │  └─ Check variables in template vs expected
│  └─ Template logic wrong?
│     ├─ Debug step-by-step with simpler templates
│     ├─ Verify filters (snake_case, pascal_case, etc)
│     └─ Test conditionals ({% if %}) separately
│
├─ Gate: Hardcoded Paths Found
│  ├─ /home/user/ paths?
│  │  └─ Replace with ~/
│  ├─ /Users/... paths?
│  │  └─ Replace with ~/
│  └─ C:\\ paths?
│     └─ Use env variables or relative paths
│
├─ Gate: Code Quality Issues
│  ├─ shellcheck errors?
│  │  └─ Fix warnings (add quotes, check -f, etc)
│  └─ clippy warnings?
│     └─ Fix all warnings (denies at compile time)
│
└─ Gate: Reproducibility Failed
   ├─ Different output on 2nd run?
   │  ├─ Check for randomness (timestamps, UUIDs)
   │  ├─ Check for temporary files not cleaned up
   │  └─ Fix: Use deterministic seeds, clean trap
   └─ Scripts hang on clean environment?
      ├─ Add timeouts where applicable
      └─ Add feedback (echo progress)
```

---

## Decision Tree 4: Example Type → Checklist Mapping

```
Tutorial
├─ Learning objectives (3-5, clear)
├─ Prerequisites listed
├─ Step 1: Understand concept
├─ Step 2: Try basic example
├─ Step 3: Modify and experiment
├─ Step 4: Expected output documented
├─ Step 5: Troubleshooting guide
├─ Time ±10% accurate
└─ All variables self-documenting

Project Generation
├─ ggen.toml complete
├─ 6-12 templates (by module)
├─ project-spec.yaml with examples
├─ generate-project.sh orchestration
├─ Output structure matches spec
├─ Generated code compiles (cargo build)
├─ Generated code passes tests (cargo test)
├─ Integration test covers ≥50%
├─ Customization documented
├─ Multiple examples in README
├─ Time ±10% accurate
└─ Dependencies listed with versions

Ontology-Driven
├─ 2+ ontology files (domain + schema)
├─ All RDF prefixes defined
├─ 8-10 SPARQL generation rules
├─ SPARQL queries ordered by dependency
├─ 8-10 *.tera templates
├─ Golden outputs for all variations
├─ Class/property hierarchy documented
├─ Template vars match SPARQL SELECT
├─ Example *.ttl data provided
├─ Query results validated
├─ SHACL schema (optional)
└─ Time ±10% accurate

AI-Powered
├─ Mock mode (no API key)
├─ 5-10 example prompts
├─ Expected outputs documented
├─ Valid/invalid examples
├─ Full workflow runs end-to-end
├─ Token usage tracked
├─ 2+ provider setup documented
├─ Cost estimation included
├─ Error handling (API failures)
├─ Prompt engineering tips
├─ Quality metrics (if applicable)
└─ Fallback to mock on errors

Utility/Tool
├─ Single responsibility
├─ Minimal dependencies
├─ Usage/help text
├─ 3+ example inputs
├─ Expected outputs shown
├─ Error cases handled
├─ Code <200 LOC
├─ Performance noted
├─ Success/failure clear
└─ Can run multiple times (idempotent)
```

---

## Checklist Selection Flowchart

```
START: Which checklist to use?

├─ Universal checklist (20 items)
│  └─ ALWAYS use for all rewrites
│
├─ Type-specific checklist
│  └─ SELECT based on example type:
│     ├─ Tutorial → 10 items
│     ├─ Project Generation → 12 items
│     ├─ Ontology-Driven → 12 items
│     ├─ AI-Powered → 12 items
│     └─ Utility/Tool → 10 items
│
├─ Example variation checklist
│  └─ SELECT based on complexity:
│     ├─ Tutorial Basic → templates: 1
│     ├─ Tutorial Intermediate → templates: 2
│     ├─ ProjectGen Small → templates: 4-6
│     ├─ ProjectGen Medium → templates: 7-10
│     ├─ ProjectGen Large → templates: 11-15
│     ├─ Ontology Simple → rules: 4-5
│     ├─ Ontology Complex → rules: 8-12
│     ├─ AI Mock-only → requires_api: false
│     └─ AI Mock+Real → requires_api: true
│
└─ Pre/post-rewrite gates
   ├─ Pre-rewrite → 6 gates (fail fast)
   └─ Post-rewrite → 8 gates (quality)
```

---

## Error Recovery Flowchart

```
START: Rewrite complete but validation failed

├─ How many gates failed?
│  ├─ 1 gate → Quick fix (see Decision Tree 3)
│  ├─ 2-3 gates → Moderate rework needed
│  └─ 4+ gates → Consider rollback
│
├─ Are template outputs generated?
│  ├─ YES but wrong → Debug templates (Decision Tree 3)
│  ├─ NO, error message → Fix template errors
│  └─ NO, timeout → Add debugging, increase timeout
│
├─ Do scripts run to completion?
│  ├─ YES but exit non-zero → Fix exit codes
│  ├─ Partial completion → Add error handling
│  └─ Hangs → Add timeout, add debug output
│
├─ Is scope preserved?
│  ├─ Accidentally added features?
│     └─ Remove them (scope creep)
│  ├─ Changed framework?
│     └─ Consider rollback (major change)
│  └─ OK → Continue
│
├─ Time estimate realistic?
│  ├─ Actual >> Estimate → Tighten or extend
│  ├─ Actual << Estimate → Over-estimated
│  └─ Within ±10% → Acceptable
│
└─ Ready to resubmit?
   ├─ YES: All gates passing?
   │  └─ Submit with receipts
   └─ NO: Uncertain about fix?
      └─ Ask user for guidance
```

---

## Time Estimation Guide

### Template Complexity Scoring
```
Points per template:
  Simple substitution (1-2 vars)      = 5 min
  Filters + loops                     = 10 min
  Conditionals (if/else)              = 15 min
  Complex nesting + multiple loops    = 20 min
  SPARQL integration                  = 25 min

Example calculation:
  - 3 simple templates        = 15 min
  - 2 with loops/filters      = 20 min
  - 1 complex conditional     = 15 min
  - 1 SPARQL integration      = 25 min
  - Script/orchestration      = 10 min
  - Documentation + testing   = 15 min
  - Buffer (20%)              = 20 min
  ├─ TOTAL: 120 min = 2 hours (60 min Tutorial variant)
```

### Document Generation Time
```
README writing              = 30 min
Template/config creation    = varies (see above)
Script writing              = 15 min
Testing on clean env        = 20 min
Troubleshooting section     = 15 min
Documentation review        = 10 min

Add 20% buffer for unknowns
```

### Type Duration Ranges
```
Tutorial:           15-30 min (total)
Project Generation: 45-60 min (total)
Ontology-Driven:    60-90 min (total)
AI-Powered:         45-60 min (total)
Utility/Tool:       15-30 min (total)
```

---

## Quality Gate Pass/Fail Matrix

| Gate | Example Type | Why Might Fail | How to Fix |
|------|-------|--------|-----------|
| README Completeness | All | Missing sections | Add all sections from template |
| Scripts Executable | Tutorial, ProjectGen, Tool | Not marked +x or errors | chmod +x, test on clean env |
| Templates Valid | Tutorial, ProjectGen, Ontology | YAML/Tera errors | Validate syntax, test render |
| Output Matches Golden | All | Variables wrong, logic wrong | Debug templates step-by-step |
| No Hardcoded Paths | All | Found /home, /Users, C:\\ | Replace with ~/ or env vars |
| Code Quality | All | Clippy/shellcheck errors | Fix warnings (required) |
| Reproducibility | All | Non-deterministic output | Add timestamps, clean temp files |
| Scope Preserved | All | Added features/framework | Remove changes, re-focus |

---

## When to Escalate vs Continue

### Continue (Fix It)
```
✓ Small issues (typos, missing docs)
✓ Simple logic errors in templates
✓ Script exits need error handling
✓ Time estimate off by <20%
✓ One validation gate failing
✓ Documentation needs clarity
```

### Escalate (Ask User)
```
✗ Scope change needed (new framework)
✗ Time estimate off by >30%
✗ Multiple gates failing
✗ Original design seems wrong
✗ Conflict between original intent and code
✗ Example is outdated (deprecated API)
✗ Unclear what "no refactor" means for this
```

### Rollback (Start Over)
```
✗ 4+ validation gates failing
✗ Scripts can't run at all
✗ Output completely wrong
✗ Can't determine original intent
✗ Scope accidentally changed significantly
✗ Time estimate radically off
```

---

## Pre-Commit Checklist

Before pushing rewrite:

```bash
# 1. Verify git status
git status                          # No unstaged changes

# 2. Run all validations
shellcheck examples/example-name/scripts/*.sh
cargo test --example example-name
cd examples/example-name && ./run-example.sh

# 3. Check for hardcoded paths
grep -r "/home\|/Users\|C:\\\\" examples/example-name/

# 4. Verify metadata
grep -i "last_verified\|duration\|difficulty" examples/example-name/README.md

# 5. Check git diff for scope creep
git diff --stat HEAD^ -- examples/example-name/ | head -5

# 6. Measure time
time ./examples/example-name/run-example.sh >/dev/null

# 7. Git log to verify clean history
git log -3 --oneline examples/example-name/

# 8. Final validation message
echo "[Receipt] All validations: ✓ PASSED"
echo "[Receipt] Time measured: XX minutes"
echo "[Receipt] Scope preserved: ✓ YES"
echo "[Receipt] Ready for merge: ✓ YES"
```

---

## Communication Templates

### For Gate Failures

**Template 1: Single Gate**
```
Gate: [Gate Name]
Status: FAILED
Issue: [Specific problem]
Fix: [Action taken]
Verification: [How verified it's fixed]
```

**Template 2: Multiple Gates**
```
Failed Gates: X of 8 post-rewrite gates
Critical: [List of blocking gates]
Non-Critical: [List of warnings]
Recovery Plan: [Steps to fix]
Timeline: [Estimated fix time]
```

### For Escalation

**Template: Scope Question**
```
Question: [Specific concern about no-refactor rule]
Example: [Specific change that might violate rule]
Original: [How original did it]
Proposed: [How we want to do it]
Impact: [Why this matters]
Request: [Guidance from user]
```

**Template: Time Estimate Off**
```
Estimated: XX minutes
Actual: YY minutes
Variance: ±ZZ% [acceptable if <±10%]
Reason: [Why took longer/shorter]
Learning: [What to estimate differently next time]
```

---

## Metadata Accuracy Checklist

Before final approval, verify:

```
Difficulty Level:
  ├─ [ ] Matches example type
  └─ [ ] Realistic for prerequisites listed

Duration:
  ├─ [ ] Measured on clean environment
  ├─ [ ] Within ±10% of estimate
  └─ [ ] Includes all required steps (not shortcuts)

Prerequisites:
  ├─ [ ] ggen version specified
  ├─ [ ] All external tools listed
  └─ [ ] All optional tools marked "(optional)"

Learning Objectives:
  ├─ [ ] 3-5 objectives
  ├─ [ ] Achievable after following steps
  └─ [ ] Each objective tested

Complexity Scoring:
  ├─ [ ] Tutorial: 1-3 templates
  ├─ [ ] ProjectGen: 6-12 templates
  ├─ [ ] Ontology: 8-10 rules
  ├─ [ ] AI: 5-10 prompts
  └─ [ ] Correct variation class

Tags:
  ├─ [ ] Lists technology areas
  ├─ [ ] Enables discovery
  └─ [ ] Accurate to content
```

---

**Last Updated**: 2026-01-04
**Version**: 1.0
**Status**: COMPLETE
