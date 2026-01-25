# Hands-On Exercise: [Exercise Title]

> **Template Instructions**: Replace all `[bracketed sections]` with actual content. Delete this instruction block when creating a real exercise.

## Overview

**Learning Objective**: By the end of this exercise, you will be able to [specific, measurable objective].

**Prerequisites**:
- [Prerequisite 1, e.g., "Completed Exercise 1: Your First Ontology"]
- [Prerequisite 2, e.g., "Basic understanding of Turtle syntax"]
- [Prerequisite 3, e.g., "ggen installed and configured"]

**Estimated Time**: [X] minutes to [Y] minutes

**Difficulty Level**: [Beginner / Intermediate / Advanced]

**What You'll Build**: [Brief description of the end result]

---

## Table of Contents

1. [Setup](#setup)
2. [Step-by-Step Instructions](#step-by-step-instructions)
3. [Expected Outcomes](#expected-outcomes)
4. [Verification](#verification)
5. [Troubleshooting](#troubleshooting)
6. [Going Further](#going-further)
7. [Key Takeaways](#key-takeaways)

---

## Setup

### Environment Check

Before starting, verify your environment:

```bash
# Check ggen is installed
ggen --version
# Expected: ggen 6.0.0 or later

# Check timeout command exists
cargo make timeout-check
# Expected: ✓ timeout command found

# Check Rust toolchain
rustc --version
# Expected: rustc 1.91.1 or later
```

### Project Structure

Create the following directory structure:

```bash
mkdir -p .specify/specs/[NNN]-[exercise-name]
mkdir -p .specify/specs/[NNN]-[exercise-name]/evidence
```

### Required Tools

Ensure you have:
- [ ] ggen CLI installed (`cargo install ggen`)
- [ ] Text editor with Turtle syntax support (recommended: VS Code with RDF extension)
- [ ] Terminal with bash
- [ ] Git (for version control)

### Starting Files

[If there are starting files provided, list them here with download/copy instructions]

**Option 1: Clone starter repository**
```bash
git clone https://github.com/[repo]/[exercise-starter].git
cd [exercise-starter]
```

**Option 2: Create from scratch**
```bash
# Commands to create initial files
```

---

## Step-by-Step Instructions

### Step 1: [First Step Name]

**Goal**: [What this step accomplishes]

**Instructions**:

1. [Detailed instruction 1]
   ```[language]
   [Code or command example]
   ```

2. [Detailed instruction 2]
   ```[language]
   [Code or command example]
   ```

3. [Detailed instruction 3]

**Explanation**: [Why you're doing this step and what it means]

**Checkpoint**:
```bash
[Command to verify step completed correctly]
```

**Expected Output**:
```
[What you should see if step was successful]
```

**Common Mistakes**:
- ❌ [Common mistake 1] → [How to fix]
- ❌ [Common mistake 2] → [How to fix]

---

### Step 2: [Second Step Name]

**Goal**: [What this step accomplishes]

**Instructions**:

1. [Detailed instruction 1]

2. [Detailed instruction 2]

**Key Concepts**:
- **[Concept 1]**: [Explanation]
- **[Concept 2]**: [Explanation]

**Example**:
```[language]
[Full example code with comments]

# Example:
@prefix sk: <http://github.com/github/spec-kit#> .
@prefix : <http://example.com/exercise#> .

# Define a user story
:us-001 a sk:UserStory ;
    sk:storyIndex 1 ;
    sk:title "[Title]" ;
    sk:priority "P1" ;
    sk:description "[Description]" ;
    sk:priorityRationale "[Rationale]" ;
    sk:independentTest "[Independent test]" ;
    sk:hasAcceptanceScenario :us-001-as-001 .

# Define acceptance scenario
:us-001-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "[Given state]" ;
    sk:when "[When action]" ;
    sk:then "[Then outcome]" .
```

**Checkpoint**:
```bash
[Verification command]
```

**Expected Output**:
```
[Expected output]
```

---

### Step 3: [Third Step Name]

**Goal**: [What this step accomplishes]

**Instructions**:

[Continue pattern for each step...]

**Validation**:
```bash
# Validate ontology with SHACL
ggen validate .specify/specs/[NNN]-[exercise]/feature.ttl
```

**Expected Output**:
```
✓ All SHACL constraints satisfied
✓ Priority values valid
✓ Required fields present
✓ Minimum scenarios met
```

---

### Step 4: [Generation Step]

**Goal**: Generate artifacts from your ontology

**Instructions**:

1. Run the generation pipeline:
   ```bash
   ggen sync --audit true
   ```

2. Examine the generated receipt:
   ```bash
   cat .ggen/receipts/latest.json | jq '.'
   ```

3. Review generated files:
   ```bash
   # List generated files
   find . -name "*.rs" -o -name "*.md" | grep -v target
   ```

**What Just Happened?**:

The `ggen sync` command executed the five-stage pipeline:

1. **μ₁ (Normalize)**: Validated your ontology against SHACL shapes
2. **μ₂ (Extract)**: Executed SPARQL queries to extract data
3. **μ₃ (Emit)**: Rendered Tera templates to generate code/docs
4. **μ₄ (Canonicalize)**: Applied rustfmt and computed hashes
5. **μ₅ (Receipt)**: Generated cryptographic proof

**Generated Artifacts**:

Examine each generated file:

**File 1: [Path]**
```[language]
[Show excerpt of generated file with annotations]

// Example:
// Generated from ontology: :us-001
// DO NOT EDIT - Regenerate with `ggen sync`

pub struct [Name] {
    // Generated from :us-001 sk:title
    pub title: String,
    // ...
}
```

**File 2: [Path]**
```[language]
[Show excerpt]
```

---

### Step 5: [Testing Step]

**Goal**: Verify the generated code works correctly

**Instructions**:

1. Run the generated tests:
   ```bash
   cargo make test
   ```

2. Examine test output:
   ```bash
   # Should show all tests passing
   # Example:
   # test test_us_001_as_001_scenario ... ok
   # test test_us_001_as_002_scenario ... ok
   ```

3. Run a specific test to see details:
   ```bash
   cargo make test test_us_001_as_001
   ```

**Understanding the Tests**:

Each test corresponds to an acceptance scenario in your ontology:

```rust
// Generated from :us-001-as-001
#[test]
fn test_us_001_as_001_scenario() {
    // Arrange: [sk:given from ontology]
    // Act: [sk:when from ontology]
    // Assert: [sk:then from ontology]
}
```

**Checkpoint**:
```bash
cargo make test
```

**Expected Output**:
```
test result: ok. X passed; 0 failed; 0 ignored; 0 measured
```

---

### Step 6: [Modification Step]

**Goal**: Make a change and see the RDF-first workflow in action

**Instructions**:

1. Modify the ontology (NOT the generated code):
   ```bash
   vim .specify/specs/[NNN]-[exercise]/feature.ttl
   ```

2. Add a new acceptance scenario:
   ```turtle
   # Add this to your feature.ttl
   :us-001-as-003 a sk:AcceptanceScenario ;
       sk:scenarioIndex 3 ;
       sk:given "[Your given]" ;
       sk:when "[Your when]" ;
       sk:then "[Your then]" .

   # Link it to the user story
   :us-001 sk:hasAcceptanceScenario :us-001-as-003 .
   ```

3. Regenerate artifacts:
   ```bash
   ggen sync --audit true
   ```

4. Observe what changed:
   ```bash
   git diff
   ```

**What to Notice**:

- New test generated automatically from :us-001-as-003
- Documentation updated to include new scenario
- All changes traceable to ontology modification
- New receipt generated with updated hashes

**Key Insight**: You never edited code directly. You edited the ontology, and code was regenerated. This is the core of RDF-first development.

---

## Expected Outcomes

### Files You Should Have

```
[Exercise directory structure]

Example:
.specify/specs/[NNN]-[exercise]/
├── feature.ttl          # Your ontology (SOURCE OF TRUTH)
├── spec.md              # Generated documentation
└── evidence/
    └── receipts/
        └── [timestamp].json  # Cryptographic receipt

src/
├── [module]/
│   ├── [file].rs        # Generated code
│   └── tests.rs         # Generated tests
└── ...

.ggen/
├── receipts/
│   └── latest.json      # Latest generation receipt
└── audit/
    └── [date].json      # Audit trail
```

### Generated Code

Your generated code should:
- [ ] Compile without errors (`cargo make check`)
- [ ] Pass all tests (`cargo make test`)
- [ ] Pass linting (`cargo make lint`)
- [ ] Include TODO comments referencing ontology URIs (e.g., `// Generated from :us-001`)

### Documentation

Your generated documentation should:
- [ ] Include all user stories from ontology
- [ ] Include all acceptance scenarios
- [ ] Be readable in GitHub (properly formatted Markdown)
- [ ] Match ontology content exactly

### Receipt

Your receipt should contain:
- [ ] Execution ID and timestamp
- [ ] Manifest hash (SHA-256)
- [ ] Ontology hash (SHA-256)
- [ ] List of files generated with hashes
- [ ] Timing information for each stage

---

## Verification

### Automated Verification

Run the verification script:

```bash
# Verify ontology is valid
ggen sync --validate_only true

# Verify all quality gates pass
cargo make pre-commit

# Verify receipt is valid
ggen verify-receipt .ggen/receipts/latest.json
```

**Expected Output**:
```
✓ Ontology validation passed (6/6 quality gates)
✓ Code compiles (0 errors, 0 warnings)
✓ All tests pass (X/X)
✓ Linting passed (0 issues)
✓ Receipt verification passed
```

### Manual Verification

**Checklist**:
- [ ] Ontology validates against SHACL (`ggen validate`)
- [ ] All acceptance scenarios have Given/When/Then
- [ ] Priority is exactly "P1", "P2", or "P3"
- [ ] Generated code compiles
- [ ] Generated tests pass
- [ ] Generated docs match ontology
- [ ] Receipt hashes match

### Visual Verification

**Compare ontology to generated artifacts**:

1. Find a user story in your ontology:
   ```turtle
   :us-001 a sk:UserStory ;
       sk:title "User can do X" .
   ```

2. Find corresponding code:
   ```rust
   // Generated from :us-001
   pub struct [Something] { ... }
   ```

3. Find corresponding test:
   ```rust
   // Generated from :us-001-as-001
   #[test]
   fn test_us_001_as_001() { ... }
   ```

4. Find corresponding docs:
   ```markdown
   ## US-001: User can do X
   ```

**All four should be synchronized because they share the same source (ontology).**

---

## Troubleshooting

### Problem: SHACL validation fails

**Symptom**:
```
Error: SHACL validation failed
  - Priority must be exactly P1, P2, or P3 (found: "HIGH")
```

**Cause**: Priority value doesn't match allowed values

**Solution**:
```turtle
# ❌ WRONG
sk:priority "HIGH" ;

# ✅ CORRECT
sk:priority "P1" ;
```

**Verification**:
```bash
ggen validate .specify/specs/[NNN]-[exercise]/feature.ttl
```

---

### Problem: Generated code doesn't compile

**Symptom**:
```
error[E0425]: cannot find value `x` in this scope
```

**Cause**: Template has errors or ontology is missing required fields

**Solution**:

1. Check template for errors:
   ```bash
   vim .specify/templates/[template].tera
   ```

2. Verify ontology has all required fields:
   ```sparql
   # Query to find user stories missing required fields
   PREFIX sk: <http://github.com/github/spec-kit#>

   SELECT ?story ?missing
   WHERE {
       ?story a sk:UserStory .
       FILTER NOT EXISTS { ?story sk:title ?title }
       BIND("title" as ?missing)
   }
   ```

3. Regenerate:
   ```bash
   ggen sync --force true --audit true
   ```

---

### Problem: Tests fail

**Symptom**:
```
test test_us_001_as_001 ... FAILED
```

**Cause**: Generated test logic doesn't match acceptance scenario semantics

**Solution**:

1. Review acceptance scenario in ontology:
   ```turtle
   :us-001-as-001 a sk:AcceptanceScenario ;
       sk:given "User is logged in" ;
       sk:when "User clicks logout" ;
       sk:then "User is logged out" .
   ```

2. Check if template correctly translates scenario to test:
   ```tera
   # In template
   {{ scenario.given | generate_test_setup }}
   ```

3. Fix template or ontology semantics

4. Regenerate:
   ```bash
   ggen sync --audit true
   ```

---

### Problem: Receipt verification fails

**Symptom**:
```
Error: Receipt hash mismatch
  Expected: sha256:abc123...
  Actual:   sha256:def456...
```

**Cause**: Files were manually edited after generation

**Solution**:

1. Identify modified files:
   ```bash
   git diff
   ```

2. Revert manual edits:
   ```bash
   git checkout [file]
   ```

3. Make changes in ontology instead:
   ```bash
   vim .specify/specs/[NNN]-[exercise]/feature.ttl
   ```

4. Regenerate:
   ```bash
   ggen sync --audit true
   ```

---

### Problem: "No such file or directory"

**Symptom**:
```
Error: No such file or directory: '.specify/specs/[NNN]-[exercise]/feature.ttl'
```

**Cause**: File path is incorrect or file wasn't created

**Solution**:

1. Verify directory structure:
   ```bash
   ls -la .specify/specs/
   ```

2. Create missing directories:
   ```bash
   mkdir -p .specify/specs/[NNN]-[exercise]
   ```

3. Create ontology file:
   ```bash
   touch .specify/specs/[NNN]-[exercise]/feature.ttl
   ```

---

## Going Further

### Challenge 1: [Extension Challenge]

**Goal**: [What to achieve]

**Instructions**:
[Step-by-step instructions for extending the exercise]

**Hints**:
- [Hint 1]
- [Hint 2]

**Solution**: [Link to solution or description]

---

### Challenge 2: [Advanced Challenge]

**Goal**: [What to achieve]

**Instructions**:
[More advanced extension]

**Learning Objectives**:
- [What you'll learn from this challenge]

---

### Related Exercises

If you enjoyed this exercise, try:
- [Exercise 1]: [Brief description and link]
- [Exercise 2]: [Brief description and link]
- [Exercise 3]: [Brief description and link]

---

### Further Reading

**Concepts Covered**:
- [Concept 1]: [Link to reference documentation]
- [Concept 2]: [Link to reference documentation]

**Advanced Topics**:
- [Topic 1]: [Link]
- [Topic 2]: [Link]

---

## Key Takeaways

### What You Learned

By completing this exercise, you should now understand:

1. **[Takeaway 1]**: [Explanation of what was learned]
   - [Specific skill or insight gained]

2. **[Takeaway 2]**: [Explanation]
   - [Specific skill or insight gained]

3. **[Takeaway 3]**: [Explanation]
   - [Specific skill or insight gained]

### Mental Model Shifts

This exercise demonstrated:

- **From [Old] to [New]**: [Explanation of paradigm shift]
  - Example: "From editing code to editing ontology"

- **From [Old] to [New]**: [Explanation]

### Practical Skills

You can now:
- [ ] [Skill 1, e.g., "Write Turtle ontologies with user stories and acceptance scenarios"]
- [ ] [Skill 2, e.g., "Validate ontologies with SHACL"]
- [ ] [Skill 3, e.g., "Generate code from ontologies using ggen sync"]
- [ ] [Skill 4, e.g., "Interpret cryptographic receipts"]
- [ ] [Skill 5, e.g., "Debug by modifying ontology, not code"]

### The RDF-First Workflow

You experienced:
```
1. Edit ontology (.ttl file)
   ↓
2. Validate with SHACL
   ↓
3. Generate with ggen sync
   ↓
4. Verify with tests
   ↓
5. Commit receipt as proof
```

This is the core workflow you'll use for all RDF-first development.

---

## Next Steps

### Continue Learning

**Recommended Path**:
1. [Next exercise]: [Brief description]
2. [Following exercise]: [Brief description]
3. [Advanced topic]: [Brief description]

### Apply to Real Project

**Try This**:
1. Identify a small feature in your current project
2. Model it as an RDF ontology (start with 1-2 user stories)
3. Create templates to generate your project's specific code style
4. Compare traditional vs RDF-first development time
5. Share your experience with the team

### Contribute

**Help Others Learn**:
- Share your exercise solution on [platform]
- Write a blog post about your experience
- Create a case study of your real-world usage
- Contribute improvements to this exercise

---

## Appendix

### Complete Ontology Solution

```turtle
[Full ontology solution for the exercise]

Example:
@prefix sk: <http://github.com/github/spec-kit#> .
@prefix : <http://example.com/exercise#> .

:ExerciseFeature a sk:Feature ;
    sk:featureName "[Feature Name]" ;
    sk:featureBranch "[branch-name]" ;
    sk:status "completed" ;
    sk:hasUserStory :us-001, :us-002 .

:us-001 a sk:UserStory ;
    sk:storyIndex 1 ;
    sk:title "[Title]" ;
    sk:priority "P1" ;
    sk:description "[Description]" ;
    sk:priorityRationale "[Rationale]" ;
    sk:independentTest "[Test]" ;
    sk:hasAcceptanceScenario :us-001-as-001, :us-001-as-002 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:scenarioIndex 1 ;
    sk:given "[Given]" ;
    sk:when "[When]" ;
    sk:then "[Then]" .

:us-001-as-002 a sk:AcceptanceScenario ;
    sk:scenarioIndex 2 ;
    sk:given "[Given]" ;
    sk:when "[When]" ;
    sk:then "[Then]" .

# [More user stories and scenarios...]
```

### Generated Code Examples

**Rust Code**:
```rust
[Complete generated code]
```

**Tests**:
```rust
[Complete generated tests]
```

**Documentation**:
```markdown
[Complete generated documentation]
```

### Sample Receipt

```json
{
  "execution_id": "20260124T120000Z-example",
  "timestamp": "2026-01-24T12:00:00.000000Z",
  "manifest_hash": "sha256:abc123...",
  "ontology_hash": "sha256:def456...",
  "files_generated": [
    {
      "path": "src/exercise/mod.rs",
      "hash": "sha256:789xyz...",
      "size_bytes": 1024
    }
  ],
  "inference_rules_executed": ["rdfs:subClassOf"],
  "generation_rules_executed": ["user-story-struct", "acceptance-test"],
  "timings": {
    "normalize": "234μs",
    "extract": "156μs",
    "emit": "378μs",
    "canonicalize": "189μs",
    "receipt": "67μs",
    "total": "1024μs"
  }
}
```

### Reference Commands

```bash
# Create ontology
mkdir -p .specify/specs/[NNN]-[name]
vim .specify/specs/[NNN]-[name]/feature.ttl

# Validate
ggen validate .specify/specs/[NNN]-[name]/feature.ttl

# Generate
ggen sync --audit true

# Test
cargo make test

# Verify receipt
ggen verify-receipt .ggen/receipts/latest.json

# Commit
git add .specify/specs/[NNN]-[name]/feature.ttl
git add .ggen/receipts/latest.json
git commit -m "feat([NNN]): [description]"
```

---

**Exercise Status**: [Template/Complete/Under Review]
**Difficulty**: [Beginner/Intermediate/Advanced]
**Estimated Time**: [X-Y minutes]
**Version**: [Version number]
**Last Updated**: [Date]
**Author**: [Name and contact]
