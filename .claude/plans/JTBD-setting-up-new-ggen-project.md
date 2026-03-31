# Jobs To Be Done (JTBD) Story: Setting Up a New ggen Project

**User Persona:** Developer new to ggen
**Context:** First-time project setup with MCP tool integration
**Date:** 2026-03-31

---

## The 5 Whys Analysis

### Why #1: Why does the user need to set up a new ggen project?

**Problem to Solve:** The user needs to generate code from RDF ontologies but lacks a properly configured project structure.

**Pain Points:**
- Manual project setup is error-prone and time-consuming
- Risk of misconfiguring the μ₁-μ₅ pipeline (Load→Extract→Generate→Validate→Write)
- Uncertainty about correct ontology file structure and SPARQL query placement
- Need to ensure generated code will compile and pass quality checks

**Motivation:** Accelerate development by leveraging ggen's specification-driven code generation while ensuring correctness from the start.

---

### Why #2: Why do they need to validate the ggen.toml manifest?

**What Happens If Invalid:**
- **Pipeline Failure:** The μ₁-μ₅ pipeline will crash during Load stage
- **Cryptic Errors:** TOML parse errors don't explain WHAT is missing or WHY it's needed
- **Wasted Time:** Discovering configuration issues only after running `ggen sync`
- **Broken State:** Project in non-functional state with unclear recovery path

**Validation Benefits:**
- ✅ **Early Detection:** Catch syntax errors before running the pipeline
- ✅ **Schema Compliance:** Ensure all required fields (`[project]`, `[ontology]`, `[[generation.rules]]`) are present
- ✅ **Type Safety:** Verify field types (e.g., `max_sparql_timeout_ms: u64`, not string)
- ✅ **Path Resolution:** Confirm referenced files exist before generation starts

---

### Why #3: Why do they need to check file dependencies?

**Risk of Missing Files:**
- **Orphaned References:** `ggen.toml` points to `ontology.ttl` but file doesn't exist
- **Broken Imports:** Turtle files import non-existent ontologies
- **Template Not Found:** Generation rule references `missing.tera` template
- **Query Path Errors:** SPARQL `.rq` files in wrong directory

**Risk of Circular Dependencies:**
- **Infinite Loops:** Import cycles cause stack overflow during ontology loading
- **Generation Failures:** Missing templates cause pipeline to abort at μ₃ (Generate)
- **Non-Deterministic Behavior:** Dependency order affects output (violates "correct-by-construction" philosophy)

**Dependency Graph Validation:**
- ✅ Detects circular imports via DFS algorithm
- ✅ Verifies all file references exist
- ✅ Builds directed graph to find critical path
- ✅ Identifies orphan nodes (unused files)

---

### Why #4: Why run all 11 quality gates?

**Cost of Skipping Quality Validation:**

| Gate | What Happens If Skipped | Example Failure |
|------|------------------------|-----------------|
| **1. Manifest Schema** | Pipeline crashes on parse error | `Missing required field: version` |
| **2. Ontology Dependencies** | Stack overflow on circular imports | `Thread 'main' has overflowed its stack` |
| **3. SPARQL Validation** | Query execution fails at runtime | `SPARQL parse error: unexpected token` |
| **4. Template Validation** | Generated code has syntax errors | `Template error: variable not found` |
| **5. File Permissions** | Cannot write generated files | `Permission denied: src/generated.rs` |
| **6. Rule Validation** | Generation rule references non-existent files | `Template not found: missing.tera` |
| **7. Define (DMAIC)** | Project goals unclear, scope creep | Generated code doesn't match requirements |
| **8. Measure (DMAIC)** | No baseline metrics, can't prove improvement | "Is this faster? Unknown." |
| **9. Analyze (DMAIC)** | Root cause not identified, fixes symptoms | "Fixed the bug but it came back" |
| **10. Improve (DMAIC)** | Changes not validated, regression risk | "New feature broke existing code" |
| **11. Control (DMAIC)** | No monitoring, issues recur in production | "Same bug deployed 3 times" |

**Philosophy: "Correct-by-Construction"**
- ggen's design philosophy is that quality is built in, not inspected in
- Quality gates enforce this by preventing invalid configurations from proceeding
- Each gate is a **mandatory checkpoint** — failure triggers 🔴 RED signal (Andon protocol)

**Time Tradeoff:**
- **Skipping Gates:** 0 seconds upfront, hours/days of debugging later
- **Running Gates:** ~5 seconds upfront, confidence in correctness

---

### Why #5: Why use MCP tools instead of manual CLI commands?

**Manual CLI Workflow (Painful):**
```bash
# 1. Create ggen.toml manually (vim/nano)
vim ggen.toml

# 2. Try to run sync (fails with cryptic error)
ggen sync
# Error: Failed to parse ggen.toml: missing field `version`

# 3. Fix and retry (iteration loop)
vim ggen.toml
ggen sync
# Error: Ontology file not found: ontology.ttl

# 4. Create ontology file
touch ontology/domain.ttl
ggen sync
# Error: SPARQL query syntax error

# ... many iterations later ...
```

**MCP Tool Workflow (Programmatic & Fast):**
```python
# Single API call validates everything
result = mcp.call("validate_project", {
    "project_root": "./my-project",
    "validation_level": "all"
})

# Returns structured JSON with ALL issues
{
    "is_valid": false,
    "validations_run": [
        {"tool": "validate_manifest_parse", "status": "fail"},
        {"tool": "validate_manifest_dependencies", "status": "pass"},
        # ... 9 more validations
    ],
    "critical_errors": [
        "Missing required field: version",
        "Ontology file not found: ontology.ttl"
    ],
    "warnings": [
        "SPARQL query has unused variable: ?foo"
    ],
    "total_duration_ms": 342
}
```

**Benefits of MCP Tools:**

1. **Batch Validation:** Run all 11 quality gates in parallel (~5 seconds total)
2. **Structured Output:** Get machine-readable JSON (not human-only text)
3. **Early Exit:** Stop on critical errors (don't waste time on non-critical checks)
4. **Incremental Mode:** Validate only changed files (for dev workflow)
5. **Dependency Graph:** Visualize import cycles and critical path
6. **Recovery Suggestions:** Get actionable fixes (not just error messages)

---

## JTBD Story Framework

### When [situation]

**Context:** A developer is starting a new code generation project using ggen. They have:

- **Background:** Familiar with Rust and basic RDF concepts, but new to ggen
- **Environment:** Local development machine with ggen CLI installed
- **Constraints:** Need to deliver working code generator in < 1 hour
- **Pressure:** Team is waiting for generated code to proceed with feature work

**Current State (Before):**
- No ggen project structure
- Unclear what files are needed
- Unsure about correct `ggen.toml` configuration
- Risk of wasting time on trial-and-error setup

---

### I want to [motivation]

**Core Job:** Set up a valid ggen project that passes all quality checks.

**Sub-Jobs:**

1. **Create Project Structure:**
   - Initialize `ggen.toml` manifest
   - Create ontology directory
   - Set up queries directory
   - Prepare templates directory

2. **Validate Configuration:**
   - Ensure manifest schema is valid
   - Verify all file dependencies exist
   - Check for circular imports
   - Confirm file permissions

3. **Run Quality Gates:**
   - Execute all 11 mandatory checkpoints
   - Review any failures or warnings
   - Fix issues until all gates pass

4. **Generate Code:**
   - Run `ggen sync` with confidence
   - Verify generated code compiles
   - Check output matches expectations

---

### So that [expected outcome]

**Success Criteria:**

- ✅ **Validated Manifest:** `ggen.toml` passes schema validation
- ✅ **No Circular Dependencies:** Dependency graph is acyclic
- ✅ **All Files Present:** Ontologies, queries, templates all exist
- ✅ **Quality Gates Pass:** 11/11 checkpoints return ✓
- ✅ **Code Generation Works:** `ggen sync` produces valid Rust code
- ✅ **Confidence:** Developer understands project structure and can iterate

**Measurable Outcomes:**

| Metric | Target | Actual |
|--------|--------|--------|
| Setup Time | < 10 minutes | ___ |
| Validation Time | < 5 seconds | ___ |
| Quality Gate Pass Rate | 100% (11/11) | ___ |
| First-Compile Success | Yes | ___ |

**Emotional Outcome:**
- **Confidence:** "I know this project is configured correctly"
- **Efficiency:** "I didn't waste time on trial-and-error"
- **Clarity:** "I understand what each quality gate checks"
- **Momentum:** "Ready to generate code and ship features"

---

## MCP Tool Usage Examples

### Tool 1: `validate_manifest_parse`

**Purpose:** Ensure `ggen.toml` has valid TOML syntax and all required fields.

**Usage:**
```python
result = mcp.call("validate_manifest_parse", {
    "manifest": """
[project]
name = "my-project"
version = "0.1.0"

[ontology]
source = "ontology/domain.ttl"

[[generation.rules]]
name = "generate-structs"
query = { inline = "SELECT ?class WHERE { ?class a owl:Class }" }
template = { inline = "struct {{ class.name }} {}" }
output_file = "src/models.rs"
"""
})
```

**Success Response:**
```json
{
    "is_valid": true,
    "parsed_fields": ["project", "ontology", "generation"],
    "rule_count": 1
}
```

---

### Tool 2: `validate_manifest_dependencies`

**Purpose:** Verify all referenced files exist and detect circular dependencies.

**Usage:**
```python
result = mcp.call("validate_manifest_dependencies", {
    "manifest_path": "./ggen.toml",
    "project_root": "."
})
```

**Success Response:**
```json
{
    "is_valid": true,
    "files_checked": 5,
    "ontology_files": ["ontology/domain.ttl", "ontology/common.ttl"],
    "sparql_files": ["queries/extract-classes.rq"],
    "template_files": ["templates/structs.tera"],
    "circular_dependencies": [],
    "orphan_nodes": []
}
```

---

### Tool 3: `validate_manifest_quality_gates`

**Purpose:** Run all 11 quality gates and return detailed results.

**Usage:**
```python
result = mcp.call("validate_manifest_quality_gates", {
    "manifest_path": "./ggen.toml",
    "gates": ["all"]
})
```

**Success Response:**
```json
{
    "is_valid": true,
    "gates_passed": 11,
    "gates_total": 11,
    "duration_ms": 342,
    "checkpoints": [
        {
            "name": "Manifest Schema",
            "status": "pass",
            "checks": ["TOML syntax valid", "All required fields present"]
        },
        {
            "name": "Ontology Dependencies",
            "status": "pass",
            "checks": ["All .ttl files exist", "No circular imports"]
        },
        {
            "name": "SPARQL Validation",
            "status": "pass",
            "checks": ["All queries have valid syntax", "No unused variables"]
        },
        {
            "name": "Template Validation",
            "status": "pass",
            "checks": ["All templates exist", "Tera syntax valid"]
        },
        {
            "name": "File Permissions",
            "status": "pass",
            "checks": ["Output directory writable", "All files readable"]
        },
        {
            "name": "Rule Validation",
            "status": "pass",
            "checks": ["All rules reference existing templates", "All queries valid"]
        },
        {
            "name": "Define (DMAIC)",
            "status": "pass",
            "checks": ["Project goals defined", "Success criteria specified"]
        },
        {
            "name": "Measure (DMAIC)",
            "status": "pass",
            "checks": ["Baseline metrics established", "Measurement system valid"]
        },
        {
            "name": "Analyze (DMAIC)",
            "status": "pass",
            "checks": ["Root cause identified", "Hypothesis tested"]
        },
        {
            "name": "Improve (DMAIC)",
            "status": "pass",
            "checks": ["Solution implemented", "Results validated"]
        },
        {
            "name": "Control (DMAIC)",
            "status": "pass",
            "checks": ["Monitoring in place", "Process standardized"]
        }
    ]
}
```

---

## Success Criteria

### All 3 Tools Return `is_valid: true`

**Tool 1 (Parse):**
```json
{"is_valid": true, "parsed_fields": ["project", "ontology", "generation"]}
```

**Tool 2 (Dependencies):**
```json
{"is_valid": true, "circular_dependencies": [], "orphan_nodes": []}
```

**Tool 3 (Quality Gates):**
```json
{"is_valid": true, "gates_passed": 11, "gates_total": 11}
```

---

### Quality Gates Pass (11/11)

| Gate | Status | Duration |
|------|--------|----------|
| Manifest Schema | ✓ | 12ms |
| Ontology Dependencies | ✓ | 45ms |
| SPARQL Validation | ✓ | 89ms |
| Template Validation | ✓ | 34ms |
| File Permissions | ✓ | 8ms |
| Rule Validation | ✓ | 56ms |
| Define (DMAIC) | ✓ | 23ms |
| Measure (DMAIC) | ✓ | 18ms |
| Analyze (DMAIC) | ✓ | 31ms |
| Improve (DMAIC) | ✓ | 15ms |
| Control (DMAIC) | ✓ | 11ms |
| **Total** | **11/11** | **342ms** |

---

### Ready to Run `ggen sync`

**Pre-Flight Checklist:**

- [x] `ggen.toml` exists and is valid
- [x] All ontology files (`*.ttl`) exist
- [x] All SPARQL queries (`*.rq`) have valid syntax
- [x] All templates (`*.tera`) have valid syntax
- [x] Output directory is writable
- [x] No circular dependencies
- [x] All 11 quality gates pass

**Command:**
```bash
ggen sync --audit true
```

**Expected Output:**
```
✅ Sync complete: 3 file(s) in 1234ms
Files: src/models.rs, src/traits.rs, src/factory.rs
Receipt: sha256:abc123...
Soundness violations: none
```

---

## Conclusion

The JTBD story for "Setting up a new ggen project" is about **confidence through validation**. The user doesn't just want to create files — they want to KNOW their project is correctly configured BEFORE investing time in code generation.

By using the 3 MCP tools (`validate_manifest_parse`, `validate_manifest_dependencies`, `validate_manifest_quality_gates`), the developer:

1. **Saves Time:** Catch all errors in ~5 seconds (vs. hours of trial-and-error)
2. **Reduces Risk:** Know project is valid before running pipeline
3. **Learns Faster:** Understand requirements through structured error messages
4. **Maintains Quality:** Enforce "correct-by-construction" philosophy from the start

**The Job isn't "create a ggen project" — it's "create a VALID ggen project that generates working code."**

---

**Version:** 1.0  
**Last Updated:** 2026-03-31  
**Author:** Claude Code Agent (JTBD Analysis Framework)
