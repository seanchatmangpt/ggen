# Jobs To Be Done: Incremental Validation for Development Workflow

**JTBD Story:** "Validating only changed files during development"

**Created:** 2026-03-31
**MCP Tool:** `validate_incremental`
**Target User:** Developer actively working on a ggen project

---

## JTBD Statement

**When** I'm actively developing a feature and making frequent edits to SPARQL queries, templates, and ontology files,

**I want** to automatically validate only the files I've changed (plus their dependencies),

**So that** I can get fast feedback on my changes without waiting for full project validation, stay in my development flow state, and catch issues early before committing.

---

## The 5 Whys Analysis

### Why #1: Why does the user need incremental validation?

**Pain Point:** Full validation is too slow for rapid iteration cycles.

**Context:**
- A typical ggen project has 50-200 files (SPARQL queries, templates, RDF ontologies, Rust code)
- Full validation takes 15-30 seconds (or longer for large projects)
- Developers make 10-50 edits per hour during active development
- Waiting 15-30 seconds after every edit breaks flow state and kills productivity

**Calculation:**
- 20 edits/hour × 20 seconds/validation = 400 seconds (6.7 minutes) of waiting per hour
- That's **11% of development time** wasted on validation latency

**What the user actually needs:**
- Validate only the 1-3 files they just edited (not all 200 files)
- Get feedback in <2 seconds (not 20 seconds)
- Iterate quickly without breaking flow

---

### Why #2: Why auto-detect changed files via git?

**Pain Point:** Manual file tracking is error-prone and cognitive overhead.

**Risks of manual tracking:**
- **Forgot what I changed:** After 20 minutes of coding, I lose track of which files I edited
- **Miss affected files:** I edit `templates/base.tera` but forget to validate `templates/child.tera` that extends it
- **False confidence:** I validate file A, but file B that depends on A is now broken
- **Context switching:** I have to switch from "coding mode" to "audit mode" to remember changes

**Why git is the source of truth:**
- Git already tracks exactly what changed (no cognitive load)
- `git diff --name-only` gives me the precise list of changed files
- No risk of forgetting changes (git doesn't lie)
- Works across all file types (SPARQL, templates, Rust, Markdown)

**What auto-detection provides:**
- Zero cognitive load (git tracks for me)
- Accuracy (no human error)
- Speed (instant file list)
- Consistency (same every time)

---

### Why #3: Why trace template dependencies?

**Pain Point:** A change to a parent template can break child templates, but they won't be re-validated by simple git diff.

**The dependency cascade problem:**

```
templates/base.tera (changed)
  ├── extends → templates/child.tera (needs re-validation)
  │     └── extends → templates/grandchild.tera (needs re-validation)
  └── includes → templates/partial.tera (needs re-validation)
```

**Scenario:**
1. I edit `templates/base.tera` (add a new block)
2. Git diff shows only `base.tera` changed
3. I validate `base.tera` → passes
4. But `child.tera` extends `base.tera` and uses the old block structure
5. When I render `child.tera`, it breaks
6. I don't discover this until runtime (or in production)

**Why dependency tracing matters:**
- Template inheritance creates implicit dependencies
- A change to a parent affects all children
- Includes create transitive dependencies
- Without tracing, I get false positives (validation passes, but code is broken)

**What tracing provides:**
- Find all templates that extend/include the changed template
- Validate the full dependency chain
- Catch breaking changes before runtime
- Give me confidence that my changes are safe

---

### Why #4: Why map file extensions to validation tools?

**Pain Point:** Manual tool selection is tedious and error-prone.

**The tool selection problem:**

| File Extension | Validation Tool | Command |
|----------------|-----------------|---------|
| `.rq` | `validate_sparql` | `ggen validate query.rq` |
| `.tera` | `validate_template_syntax` | `ggen validate template.tera` |
| `.ttl` | `validate_ontology` | `ggen validate ontology.ttl` |
| `.rs` | `cargo check` | `cargo check --message-format=json` |
| `.md` | `markdownlint` | `markdownlint README.md` |

**Without automatic mapping:**
- I edit 5 files: 2 SPARQL, 2 templates, 1 Rust
- I need to remember which tool to use for each file
- I need to run 5 different validation commands
- I might forget to validate one file type
- I might use the wrong tool for a file

**With automatic mapping:**
- One command validates everything
- File extension → tool mapping is automatic
- No risk of using the wrong tool
- No risk of forgetting a file type
- Consistent validation every time

**What mapping provides:**
- Correct tool for every file type
- No cognitive load (no need to remember)
- No errors (no wrong tool selection)
- Speed (one command instead of five)

---

### Why #5: Why use MCP tool instead of manual git diff + validation?

**Pain Point:** Manual workflow is slow, error-prone, and breaks flow state.

**Manual workflow (slow):**
```bash
# 1. Find changed files
git diff --name-only HEAD~1

# 2. Manually categorize by file type
#    (cognitive load: I have to remember which tool goes with which extension)

# 3. Run validation for each file type
ggen validate queries/extract-skills.rq
ggen validate templates/hello.tmpl
cargo check --message-format=json

# 4. Check exit codes
#    (if one fails, I have to figure out which one)

# 5. Parse output to find errors
#    (manual error hunting)
```

**Time:** 30-60 seconds (mostly cognitive load and context switching)

**MCP tool workflow (fast):**
```bash
# 1. One command
mcp.call("validate_incremental", {
  project_root: "./my-ggen-project",
  since_commit: "HEAD~1"
})

# 2. Get structured response
#    (JSON with clear pass/fail per file)
```

**Time:** 2-3 seconds (automated)

**Why MCP is better:**
- **Faster:** 2 seconds vs 60 seconds (30x speedup)
- **More accurate:** Auto-detects dependencies (manual workflow misses them)
- **Lower cognitive load:** No need to remember tools or file types
- **Better error reporting:** Structured JSON vs manual log parsing
- **Consistent:** Same workflow every time (no variation)
- **Composable:** Can be integrated into editor hooks, pre-commit, CI/CD

**What MCP provides:**
- Speed (30x faster than manual)
- Accuracy (no human error)
- Flow state preservation (no context switching)
- Integration (works with other tools)

---

## Context Factors

### Development Phase

| Phase | Change Pattern | Validation Strategy |
|-------|---------------|---------------------|
| **Active feature development** | Many small edits (10-50/hour) | Incremental validation (every 5-10 edits) |
| **Bug fixing** | Focused changes (1-5 files) | Incremental validation (per edit) |
| **Refactoring** | Cascading changes (10-20 files) | Incremental + dependency tracing |
| **Maintenance** | Few changes (1-2 edits/day) | Incremental validation (per edit) |
| **Pre-commit** | All changes in commit | Full validation (final check) |

### Change Frequency

| Frequency | Validation Strategy |
|-----------|---------------------|
| **High frequency** (10+ edits/hour) | Validate every 5-10 edits (batch) |
| **Medium frequency** (5-10 edits/hour) | Validate every 3-5 edits (batch) |
| **Low frequency** (1-5 edits/hour) | Validate every edit (immediate) |

### Project Size

| Size | Full Validation Time | Incremental Validation Time |
|------|---------------------|----------------------------|
| **Small** (<10 files) | 2-5 seconds | <1 second (not worth it) |
| **Medium** (10-50 files) | 10-20 seconds | 1-2 seconds (worth it) |
| **Large** (50-200 files) | 20-40 seconds | 2-3 seconds (definitely worth it) |
| **Very large** (200+ files) | 40+ seconds | 3-5 seconds (essential) |

**Rule of thumb:** Use incremental validation if full validation takes >10 seconds.

### Feedback Speed Requirements

| Requirement | Validation Strategy |
|-------------|---------------------|
| **Ultra-fast feedback** (<1 second) | Incremental + caching |
| **Fast feedback** (1-3 seconds) | Incremental validation |
| **Medium feedback** (5-10 seconds) | Incremental validation (batched) |
| **Slow feedback** (10+ seconds) | Full validation (not ideal for dev) |

---

## Forces (Trade-offs)

### Force 1: Time Pressure (Stay in Flow State)

**Desire:** Stay in coding flow, minimize interruptions

**Conflict:** Validation interrupts flow, but not validating risks breaking code

**Resolution:** Incremental validation minimizes interruption time
- Full validation: 20-30 seconds (flow break)
- Incremental validation: 1-2 seconds (micro-pause)
- Result: Stay in flow state

---

### Force 2: Cognitive Load (Don't Want to Remember)

**Desire:** Focus on coding, not on what I changed

**Conflict:** Need to know what changed to validate it

**Resolution:** Auto-detection via git
- Git tracks changes (zero memory)
- No need to remember file types or tools
- Result: Cognitive load reduced to zero

---

### Force 3: Safety (Don't Want to Break Dependencies)

**Desire:** Fast validation, but not at the cost of safety

**Conflict:** Fast validation might miss dependency issues

**Resolution:** Dependency tracing
- Find all affected files (not just changed files)
- Validate full dependency chain
- Result: Speed + safety

---

### Force 4: Performance (Full Validation Too Slow)

**Desire:** Validate everything (safest approach)

**Conflict:** Full validation takes too long for rapid iteration

**Resolution:** Incremental validation + periodic full validation
- Development: Incremental (fast)
- Pre-commit: Full validation (safe)
- Result: Speed during dev, safety before commit

---

## Expected Outcomes

### Primary Outcome: Validate Only What Changed (Fast Feedback)

**Before (full validation):**
- Edit 1 file → validate 200 files → wait 20 seconds

**After (incremental validation):**
- Edit 1 file → validate 1 file (+ dependencies) → wait 1 second

**Result:** 20x faster feedback loop

---

### Secondary Outcome: Catch Issues Early (Before Commit/Push)

**Before:**
- Edit file → hope it works → commit → CI fails → fix → re-commit
- Time to discover error: 5-30 minutes

**After:**
- Edit file → validate immediately → see error → fix immediately
- Time to discover error: 1-2 seconds

**Result:** 1000x faster error detection

---

### Tertiary Outcome: Understand Impact of Changes (Dependencies Affected)

**Before:**
- Edit `templates/base.tera` → don't know what breaks
- Discover breakage at runtime (worst case: production)

**After:**
- Edit `templates/base.tera` → see that 3 child templates are affected
- Validate all 4 files → catch breaking changes immediately
- Know exactly what was impacted

**Result:** Predictable change impact

---

### Quaternary Outcome: Stay in Development Flow (Minimal Context Switching)

**Before:**
- Edit file → switch to terminal → run `ggen validate` → wait → switch back
- Context switch: Every 2 minutes
- Flow recovery time: 15 minutes per interruption

**After:**
- Edit file → background validation → notification if fails
- Context switch: Only on failure
- Flow recovery: Minimal

**Result:** Maximum flow state time

---

## MCP Tool Usage

### Basic Usage

```bash
# Validate changes since last commit
mcp.call("validate_incremental", {
  project_root: "./my-ggen-project",
  since_commit: "HEAD~1"
})
```

### Advanced Usage

```bash
# Validate changes since specific commit
mcp.call("validate_incremental", {
  project_root: "./my-ggen-project",
  since_commit: "abc123def"
})

# Validate uncommitted changes
mcp.call("validate_incremental", {
  project_root: "./my-ggen-project",
  since_commit: "HEAD"
})

# Validate changes in specific directory
mcp.call("validate_incremental", {
  project_root: "./my-ggen-project",
  since_commit: "HEAD~1",
  filter: "templates/"  # Only validate templates
})
```

### Integration Examples

#### VS Code Task

```json
{
  "label": "Validate Incremental",
  "type": "shell",
  "command": "ggen",
  "args": ["mcp", "call", "validate_incremental", "--project-root", "${workspaceFolder}", "--since-commit", "HEAD~1"],
  "problemMatcher": "$rustc"
}
```

#### Git Hook

```bash
#!/bin/bash
# .git/hooks/post-commit

# Validate incremental after commit
mcp.call("validate_incremental", {
  project_root: ".",
  since_commit: "HEAD~1"
})

if [ $? -ne 0 ]; then
  echo "⚠️  Validation failed. Fix before pushing."
  exit 1
fi
```

#### Pre-commit Integration

```bash
#!/bin/bash
# .git/hooks/pre-commit

# Run incremental validation
mcp.call("validate_incremental", {
  project_root: ".",
  since_commit: "HEAD"
})

if [ $? -ne 0 ]; then
  echo "❌ Validation failed. Commit aborted."
  exit 1
fi
```

---

## MCP Tool Response

### Success Response

```json
{
  "is_valid": true,
  "duration_ms": 1234,
  "files_validated": [
    {
      "path": "queries/extract-skills.rq",
      "tool": "validate_sparql",
      "status": "pass",
      "duration_ms": 123
    },
    {
      "path": "templates/hello.tmpl",
      "tool": "validate_template_syntax",
      "status": "pass",
      "duration_ms": 456
    },
    {
      "path": "crates/ggen-core/src/lib.rs",
      "tool": "cargo_check",
      "status": "pass",
      "duration_ms": 567
    }
  ],
  "dependencies_affected": [
    {
      "path": "templates/goodbye.tmpl",
      "reason": "extends hello.tmpl",
      "status": "pass"
    }
  ],
  "summary": {
    "total_files": 4,
    "passed": 4,
    "failed": 0,
    "skipped": 0
  }
}
```

### Failure Response

```json
{
  "is_valid": false,
  "duration_ms": 2345,
  "files_validated": [
    {
      "path": "queries/extract-skills.rq",
      "tool": "validate_sparql",
      "status": "fail",
      "errors": [
        {
          "line": 42,
          "column": 15,
          "message": "Unexpected token ')'",
          "severity": "error"
        }
      ],
      "duration_ms": 234
    },
    {
      "path": "templates/hello.tmpl",
      "tool": "validate_template_syntax",
      "status": "pass",
      "duration_ms": 456
    }
  ],
  "dependencies_affected": [],
  "summary": {
    "total_files": 2,
    "passed": 1,
    "failed": 1,
    "skipped": 0
  }
}
```

---

## Success Criteria

### Functional Criteria

- [ ] Only changed files validated (not entire project)
- [ ] Dependencies correctly traced (all affected files validated)
- [ ] File extensions mapped to correct validation tools
- [ ] Git diff auto-detection works accurately
- [ ] Clear pass/fail status per file
- [ ] Structured error reporting with line/column numbers

### Performance Criteria

- [ ] Validation completes in <2 seconds (fast feedback)
- [ ] Overhead of dependency tracing <500ms
- [ ] Scales to 200+ file projects
- [ ] No false positives (validation passes but code is broken)

### Usability Criteria

- [ ] Zero cognitive load (no need to remember changes)
- [ ] One-command invocation (no complex setup)
- [ ] Clear output (easy to understand what failed)
- [ ] Integrates with existing tools (git, VS Code, CI/CD)

### Safety Criteria

- [ ] No dependency cascades missed
- [ ] Breaking changes caught before runtime
- [ ] Template inheritance changes detected
- [ ] Transitive dependencies validated

---

## Development Workflow Examples

### Scenario 1: Active Feature Development

**Context:** Adding a new SPARQL query to extract skills

**Workflow:**

1. Edit `queries/extract-skills.rq` (add new FILTER clause)
2. Edit `templates/skills-list.tera` (use new query result)
3. Edit `crates/ggen-core/src/lib.rs` (call new query)
4. Run incremental validation:

```bash
mcp.call("validate_incremental", {
  project_root: ".",
  since_commit: "HEAD~1"
})
```

**Result:**
- Validates 3 files (not 200)
- Completes in 1.2 seconds
- Finds SPARQL syntax error in line 42
- Fix error → re-validate → passes

**Time saved:** 18.8 seconds (vs full validation)

---

### Scenario 2: Template Refactoring

**Context:** Refactoring base template, affecting 5 child templates

**Workflow:**

1. Edit `templates/base.tera` (add new block)
2. Run incremental validation:

```bash
mcp.call("validate_incremental", {
  project_root: ".",
  since_commit: "HEAD~1"
})
```

**Result:**
- Validates `base.tera` (pass)
- Detects 5 child templates that extend `base.tera`
- Validates all 5 child templates
- Finds that 2 child templates have breaking changes
- Fix templates → re-validate → passes

**Time saved:** 15 seconds (vs manual hunting)

---

### Scenario 3: Bug Fixing

**Context:** Fixing bug in RDF ontology

**Workflow:**

1. Edit `ontologies/skills.ttl` (fix typo)
2. Run incremental validation:

```bash
mcp.call("validate_incremental", {
  project_root: ".",
  since_commit: "HEAD~1"
})
```

**Result:**
- Validates `skills.ttl` (pass)
- No dependencies affected
- Completes in 0.8 seconds
- Confidence: Fix is safe

**Time saved:** 19.2 seconds (vs full validation)

---

## Anti-Patterns (What Not To Do)

### Anti-Pattern 1: Manual File Selection

```bash
# BAD: Manually specifying files
ggen validate queries/extract-skills.rq
ggen validate templates/hello.tmpl
cargo check
```

**Why it's bad:**
- High cognitive load (must remember what changed)
- Risk of missing files
- Risk of using wrong tool
- Slow (multiple commands)

**Use instead:**
```bash
# GOOD: Auto-detect changes
mcp.call("validate_incremental", { project_root: ".", since_commit: "HEAD~1" })
```

---

### Anti-Pattern 2: Ignoring Dependencies

```bash
# BAD: Only validate changed files (not dependencies)
git diff --name-only | xargs ggen validate
```

**Why it's bad:**
- Misses template inheritance issues
- Misses include dependencies
- False positives (validation passes but code breaks)

**Use instead:**
```bash
# GOOD: Validate changed files + dependencies
mcp.call("validate_incremental", { project_root: ".", since_commit: "HEAD~1" })
```

---

### Anti-Pattern 3: Full Validation for Every Edit

```bash
# BAD: Full validation after every edit
ggen sync  # Validates all 200 files
```

**Why it's bad:**
- Too slow (20-30 seconds)
- Breaks flow state
- Wastes time (validating unchanged files)

**Use instead:**
```bash
# GOOD: Incremental validation during dev, full validation before commit
# Development:
mcp.call("validate_incremental", { project_root: ".", since_commit: "HEAD~1" })

# Pre-commit:
ggen sync --audit true
```

---

## Metrics and KPIs

### Developer Productivity

| Metric | Before (Full Validation) | After (Incremental) | Improvement |
|--------|-------------------------|---------------------|-------------|
| Validation time | 20-30 seconds | 1-2 seconds | **15-30x faster** |
| Edits per hour | 10-20 | 30-50 | **2-3x more edits** |
| Flow state time | 40% of session | 85% of session | **2x more flow** |
| Context switches | 30/hour | 5/hour | **6x fewer switches** |

### Error Detection

| Metric | Before | After | Improvement |
|--------|--------|-------|-------------|
| Time to detect error | 5-30 minutes | 1-2 seconds | **1000x faster** |
| Errors caught before commit | 30% | 95% | **3x more** |
| CI failures per week | 15 | 2 | **7x fewer** |

### Project Scale

| Project Size | Full Validation | Incremental | Speedup |
|--------------|-----------------|-------------|---------|
| Small (10 files) | 2-5 seconds | 0.5-1 second | 2-5x |
| Medium (50 files) | 10-20 seconds | 1-2 seconds | 10-20x |
| Large (200 files) | 20-40 seconds | 2-3 seconds | 10-20x |

---

## Future Enhancements

### Enhancement 1: Watch Mode

```bash
# Watch for file changes and auto-validate
mcp.call("validate_incremental", {
  project_root: ".",
  watch: true,
  debounce_ms: 1000  # Wait 1 second after last edit
})
```

**Benefit:** Zero-command validation (automatic feedback)

---

### Enhancement 2: Caching

```bash
# Cache validation results
mcp.call("validate_incremental", {
  project_root: ".",
  since_commit: "HEAD~1",
  cache: true
})
```

**Benefit:** Sub-second validation for unchanged files

---

### Enhancement 3: Dependency Graph Visualization

```bash
# Show dependency graph for changed files
mcp.call("validate_incremental", {
  project_root: ".",
  since_commit: "HEAD~1",
  visualize_dependencies: true
})
```

**Benefit:** Understand impact of changes (visual feedback)

---

### Enhancement 4: Fix Suggestions

```json
{
  "files_validated": [
    {
      "path": "queries/extract-skills.rq",
      "status": "fail",
      "errors": [
        {
          "message": "Unexpected token ')'",
          "fix_suggestion": "Remove extra ')' on line 42"
        }
      ]
    }
  ]
}
```

**Benefit:** Faster error resolution (guided fixes)

---

## Conclusion

### The Job To Be Done

**When** I'm actively developing a ggen project and making frequent edits,

**I want** to automatically validate only the files I've changed (plus their dependencies),

**So that** I can get fast feedback, catch issues early, stay in flow state, and understand the impact of my changes.

### The 5 Whys Summary

1. **Why incremental?** Full validation is too slow (20-30 seconds) for rapid iteration
2. **Why git auto-detect?** Manual tracking is error-prone and breaks flow state
3. **Why dependency tracing?** Parent template changes break child templates (cascading errors)
4. **Why extension mapping?** Manual tool selection is tedious and error-prone
5. **Why MCP tool?** 30x faster than manual workflow, with better accuracy and safety

### The Value Proposition

- **15-30x faster** validation (1-2 seconds vs 20-30 seconds)
- **1000x faster** error detection (seconds vs minutes)
- **2x more** flow state time (85% vs 40%)
- **3x more** errors caught before commit (95% vs 30%)
- **7x fewer** CI failures (2 vs 15 per week)

### The Success Formula

**Incremental validation = Speed + Safety + Flow State**

- **Speed:** Only validate what changed (1-2 seconds)
- **Safety:** Trace dependencies (no breaking changes)
- **Flow State:** Zero cognitive load (git auto-detects)

---

**End of JTBD Story**
