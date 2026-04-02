<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [FMEA: ggen sync User Simulation & Failure Mode Analysis](#fmea-ggen-sync-user-simulation--failure-mode-analysis)
  - [Executive Summary](#executive-summary)
  - [User Simulation Scenarios](#user-simulation-scenarios)
    - [Scenario 1: The Confused Developer](#scenario-1-the-confused-developer)
    - [Scenario 2: The "I'll Just Copy-Paste" User](#scenario-2-the-ill-just-copy-paste-user)
    - [Scenario 3: The Circular Dependency Creator](#scenario-3-the-circular-dependency-creator)
    - [Scenario 4: The Template Enthusiast](#scenario-4-the-template-enthusiast)
    - [Scenario 5: The File Overwriter](#scenario-5-the-file-overwriter)
    - [Scenario 6: The Timeout Sufferer](#scenario-6-the-timeout-sufferer)
    - [Scenario 7: The Output Format Confuser](#scenario-7-the-output-format-confuser)
    - [Scenario 8: The Cache Corrupted User](#scenario-8-the-cache-corrupted-user)
    - [Scenario 9: The Rule Name Typer](#scenario-9-the-rule-name-typer)
    - [Scenario 10: The Permission Denied User](#scenario-10-the-permission-denied-user)
  - [FMEA Analysis Table](#fmea-analysis-table)
    - [Legend](#legend)
  - [Priority Breakdown](#priority-breakdown)
    - [🔴 CRITICAL (RPN > 200)](#-critical-rpn--200)
    - [🟡 HIGH (RPN 100-200)](#-high-rpn-100-200)
  - [Poka-Yoke Mitigations](#poka-yoke-mitigations)
    - [MITIGATION STRATEGY 1: Prevention (Best)](#mitigation-strategy-1-prevention-best)
      - [M1.1: Manifest Template Generation](#m11-manifest-template-generation)
      - [M1.2: Structured Validation with Error Guidance](#m12-structured-validation-with-error-guidance)
      - [M1.3: Ontology Import Validation](#m13-ontology-import-validation)
      - [M1.4: Circular Dependency Detection](#m14-circular-dependency-detection)
      - [M1.5: Template Pre-Validation](#m15-template-pre-validation)
    - [MITIGATION STRATEGY 2: Detection (Second Best)](#mitigation-strategy-2-detection-second-best)
      - [M2.1: Improved Error Messages with Context](#m21-improved-error-messages-with-context)
      - [M2.2: Validate-Only Mode as Quick Feedback](#m22-validate-only-mode-as-quick-feedback)
      - [M2.3: Rule Name Validation](#m23-rule-name-validation)
      - [M2.4: Cache Validation Checksum](#m24-cache-validation-checksum)
      - [M2.5: Pre-flight File Permission Check](#m25-pre-flight-file-permission-check)
    - [MITIGATION STRATEGY 3: Reaction (Last Resort)](#mitigation-strategy-3-reaction-last-resort)
      - [M3.1: User Confirmation for Destructive Operations](#m31-user-confirmation-for-destructive-operations)
      - [M3.2: Stop-the-Line on Critical Errors (Andon Signal)](#m32-stop-the-line-on-critical-errors-andon-signal)
      - [M3.3: Comprehensive Audit Trail](#m33-comprehensive-audit-trail)
  - [Poka-Yoke Framework for ggen sync](#poka-yoke-framework-for-ggen-sync)
    - [Core Poka-Yoke Principles Applied](#core-poka-yoke-principles-applied)
    - [Poka-Yoke Type Examples in ggen sync](#poka-yoke-type-examples-in-ggen-sync)
      - [Type 1: Automatic Prevention](#type-1-automatic-prevention)
      - [Type 2: Physical/Logical Constraints](#type-2-physicallogical-constraints)
      - [Type 3: Automatic Detection & Warning](#type-3-automatic-detection--warning)
    - [Implementation Priority](#implementation-priority)
      - [Phase 1: CRITICAL (Week 1) - Prevention](#phase-1-critical-week-1---prevention)
      - [Phase 2: HIGH (Week 2-3) - Detection](#phase-2-high-week-2-3---detection)
      - [Phase 3: MEDIUM (Week 4) - Reaction](#phase-3-medium-week-4---reaction)
  - [Andon Signal System for ggen sync](#andon-signal-system-for-ggen-sync)
    - [Signal Levels](#signal-levels)
    - [Andon Implementation in ggen sync](#andon-implementation-in-ggen-sync)
      - [1. RED Signal Handler](#1-red-signal-handler)
      - [2. RED Signal Examples](#2-red-signal-examples)
      - [3. YELLOW Signal Examples](#3-yellow-signal-examples)
  - [Quality Gate System](#quality-gate-system)
    - [Pre-Sync Quality Gates (Preflight)](#pre-sync-quality-gates-preflight)
    - [Gate Implementation](#gate-implementation)
  - [Stop-the-Line Protocol (Andon Pull Cord)](#stop-the-line-protocol-andon-pull-cord)
    - [When to Pull the Andon Cord (RED Signal)](#when-to-pull-the-andon-cord-red-signal)
    - [Protocol Flow](#protocol-flow)
    - [Example: Stop-the-Line in Action](#example-stop-the-line-in-action)
  - [Error Classification & Responses](#error-classification--responses)
  - [Deterministic Validation (Evidence-First)](#deterministic-validation-evidence-first)
    - [Receipt Format](#receipt-format)
  - [Constitutional Rules Enforcement](#constitutional-rules-enforcement)
  - [Success Criteria](#success-criteria)
  - [User Experience Journey (After Mitigations)](#user-experience-journey-after-mitigations)
    - [New User Flow (Day 1)](#new-user-flow-day-1)
    - [Experienced User Flow (Iteration)](#experienced-user-flow-iteration)
    - [Troubleshooting Flow](#troubleshooting-flow)
  - [Conclusion](#conclusion)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# FMEA: ggen sync User Simulation & Failure Mode Analysis

## Executive Summary

**Objective**: Identify failure modes when confused users attempt to use `ggen sync`

**Analysis Date**: 2026-01-06

**Scope**: All user-facing aspects of `ggen sync` from initial setup through code generation

**Total Failure Modes Identified**: 12 (High Priority: 5, Medium: 4, Low: 3)

---

## User Simulation Scenarios

### Scenario 1: The Confused Developer

**User Profile**:
- Familiar with Rust/Cargo but never used ggen
- Has heard "ontology-driven" but doesn't understand RDF
- Read CLAUDE.md but didn't understand TTL files
- Assumption: Can figure it out by trial and error

**First Actions**:
```bash
# Step 1: Create ggen.toml (guesses at structure)
$ cat > ggen.toml <<EOF
[package]
name = "my-project"

[generation]
output_dir = "src/generated"
EOF

# Step 2: Try to run
$ ggen sync
❌ ERROR: Manifest validation failed
   └─ ontology.source is required

# Step 3: Create empty ontology
$ touch ontology.ttl
$ ggen sync
❌ ERROR: No triples in ontology
   └─ Cannot generate code from empty RDF store

# Step 4: Copy random TTL from docs (incorrect structure)
$ cat ontology.ttl
@prefix ex: <http://example.org/> .
ex:MyClass a owl:Class .

$ ggen sync
⚠️  No generation rules matched → creates empty output
   └─ User has no idea why nothing was generated
```

### Scenario 2: The "I'll Just Copy-Paste" User

**User Profile**:
- Found example TTL + ggen.toml
- Copy-pasted without understanding structure
- Has multiple ontology files but manifest only references one

**Actions**:
```bash
# Manifest references main ontology only
$ cat ggen.toml
[ontology]
source = "ontology/schema.ttl"  # Missing: domain.ttl, types.ttl
imports = []

# But user created:
# - ontology/schema.ttl (CONSTRUCT rules)
# - ontology/domain.ttl (domain classes)
# - ontology/types.ttl (type definitions)

$ ggen sync
⚠️  "domain.ttl and types.ttl ignored"
   └─ No error message (files just silently not imported)
   └─ Generated code is incomplete
   └─ User blames "ggen sync doesn't work"
```

### Scenario 3: The Circular Dependency Creator

**User Profile**:
- Tried to organize ontologies logically
- Doesn't understand dependency graphs
- Created: A imports B, B imports A

**Actions**:
```bash
$ cat ontology/a.ttl
@prefix ex: <http://example.org/> .
@base <http://example.org/a/> .
@import <../b.ttl> .

ex:ClassA a owl:Class .

$ cat ontology/b.ttl
@prefix ex: <http://example.org/> .
@base <http://example.org/b/> .
@import <../a.ttl> .

ex:ClassB a owl:Class .

$ ggen sync
❌ HANGS (or timeout after 30s)
   └─ No clear error message
   └─ User: "ggen sync is broken"
```

### Scenario 4: The Template Enthusiast

**User Profile**:
- Understands SPARQL/templates
- Created generation rule with BROKEN template
- Template syntax error not caught until rendering

**Actions**:
```bash
$ cat ggen.toml
[[generation.rules]]
name = "generate_structs"
query = """
  SELECT ?name ?field WHERE {
    ?class a ex:Class ;
      rdfs:label ?name ;
      ex:field ?field .
  }
"""
template = "templates/struct.tera"
output_file = "{{ incorrect_syntax }}.rs"

$ ggen sync
❌ ERROR: Template rendering failed
   └─ "undefined variable: incorrect_syntax"
   └─ File: templates/struct.tera, Line: 42
   └─ Context: [Query results had: name, field]
```

### Scenario 5: The File Overwriter

**User Profile**:
- Doesn't realize generated files are regenerated
- Manually edited generated code to add business logic
- Ran `ggen sync` again

**Actions**:
```bash
# First run
$ ggen sync
✓ Created src/generated/models.rs

# User edits generated file
$ cat src/generated/models.rs
pub struct User {
    pub id: i32,
    pub name: String,
    // User added custom methods
    pub fn get_display_name(&self) -> String {
        format!("{} (ID: {})", self.name, self.id)
    }
}

# Second run (mistake!)
$ ggen sync
✓ src/generated/models.rs updated
❌ ALL CUSTOM CODE IS GONE
   └─ Panic/frustration
   └─ User: "Never using ggen again"
```

### Scenario 6: The Timeout Sufferer

**User Profile**:
- Created large/complex ontology
- First run takes 45 seconds
- Default 30s timeout exceeded

**Actions**:
```bash
$ ggen sync
⏱️  TIMEOUT: Execution exceeded 30000ms
   └─ No partial results
   └─ No guidance on why it timed out
   └─ No suggestion to increase timeout
   └─ User doesn't know what to do
```

### Scenario 7: The Output Format Confuser

**User Profile**:
- Ran `ggen sync --format json` accidentally
- Doesn't understand why output looks weird
- Tried to paste it in Slack thinking something broke

**Actions**:
```bash
$ ggen sync --format json
{"status":"success","files_synced":5,"duration_ms":234,"files":[...]}

# User expects text output with progress indicators
# Instead gets compact JSON
# User: "Is it working? Why no progress?"
```

### Scenario 8: The Cache Corrupted User

**User Profile**:
- Used incremental cache mode (`--cache` flag)
- Modified TTL file in a way that invalidates cache
- Cache not invalidated automatically

**Actions**:
```bash
$ ggen sync --cache
# ... uses cached results ...

# User edits ontology/schema.ttl
$ ggen sync --cache
⚠️  Stale cache used (dependencies not detected)
   └─ Generated code doesn't reflect ontology changes
   └─ User doesn't realize cache is stale
   └─ Spends hours debugging "broken" generation
```

### Scenario 9: The Rule Name Typer

**User Profile**:
- Created generation rules
- Used `--rule generate_struct` (typo)
- Manifest has `generate_structs` (plural)

**Actions**:
```bash
$ ggen sync --rule generate_struct
❌ ERROR: Rule "generate_struct" not found
   └─ Available rules: [generate_structs, generate_enums]
   └─ User: "Which one do I use?"
```

### Scenario 10: The Permission Denied User

**User Profile**:
- Output directory is read-only
- Didn't realize manifest points to protected path
- ggen sync silently fails to write

**Actions**:
```bash
$ ls -la src/generated/
dr-xr-xr-x  user  group  src/generated/  # Read-only!

$ ggen sync
✓ Sync completed
# But files were never written!
# User wonders why code isn't regenerated
```

---

## FMEA Analysis Table

### Legend
- **S (Severity)**: Impact when failure occurs (1-10)
  - 10: Data loss, system crash, security breach
  - 5: Feature broken, workaround exists
  - 1: Minor inconvenience
- **O (Occurrence)**: How often this happens (1-10)
  - 10: Happens daily / with every user
  - 5: Happens monthly / predictable scenario
  - 1: Rare / happens once per year
- **D (Detection)**: How detectable is the failure (1-10)
  - 10: Almost impossible to detect until user discovers
  - 5: Detected during testing
  - 1: Impossible to miss (immediate feedback)
- **RPN**: Risk Priority Number = S × O × D
  - > 200: CRITICAL (fix immediately)
  - 100-200: HIGH (plan mitigation)
  - 50-100: MEDIUM (monitor, schedule fix)
  - < 50: LOW (document, monitor)

| # | Failure Mode | Scenario | Severity | Occur | Detect | RPN | Current State | Risk |
|---|--------------|----------|----------|-------|--------|-----|---------------|------|
| **1** | **Manifest validation unclear** | User creates minimal ggen.toml, required fields missing | 7 | 8 | 9 | **504** | ❌ Error message generic | **CRITICAL** |
| **2** | **Missing ontology imports** | User creates multiple TTL files but only references one in manifest | 8 | 6 | 9 | **432** | ⚠️  Silent failure, no warning | **CRITICAL** |
| **3** | **Circular dependency detection fails** | User creates A→B→A imports, causes hang/timeout | 9 | 4 | 10 | **360** | ⚠️  Timeout but no clear error | **CRITICAL** |
| **4** | **Template syntax errors not pre-validated** | User creates template with undefined variables, error only at render time | 6 | 7 | 7 | **294** | ❌ Error late in pipeline | **HIGH** |
| **5** | **Generated files overwrite user code** | User manually edits generated file, ggen sync overwrites changes | 10 | 5 | 1 | **50** | ✓ User aware of risk, but no guard rails | **MEDIUM** |
| **6** | **Default timeout too short for large ontologies** | Complex SPARQL inference hits 30s default timeout | 7 | 3 | 2 | **42** | ⚠️  No guidance on resolution | **LOW** |
| **7** | **Output format misunderstanding** | User runs with `--format json` expecting text, gets JSON, thinks failure | 3 | 2 | 8 | **48** | ✓ Help text available | **LOW** |
| **8** | **Cache invalidation silent** | User modifies ontology, incremental cache not invalidated, stale code generated | 9 | 2 | 10 | **180** | ❌ No cache dependency tracking | **HIGH** |
| **9** | **Typo in rule name goes undetected** | User runs `--rule generate_struct` (typo), silently skips that rule | 6 | 4 | 8 | **192** | ⚠️  Error message exists but unclear | **HIGH** |
| **10** | **File write permissions not checked** | Output directory is read-only, ggen sync silently fails to write | 8 | 3 | 10 | **240** | ❌ No permission validation | **CRITICAL** |
| **11** | **SPARQL query syntax errors** | User writes invalid SPARQL, error message is cryptic | 7 | 5 | 6 | **210** | ⚠️  Error from Oxigraph library, unclear | **HIGH** |
| **12** | **"What is TTL?" fundamental misunderstanding** | User doesn't understand RDF/Turtle, creates invalid ontology, silent failure | 8 | 7 | 10 | **560** | ❌ No onboarding/validation | **CRITICAL** |

---

## Priority Breakdown

### 🔴 CRITICAL (RPN > 200)

1. **Failure Mode #12**: "What is TTL?" fundamental misunderstanding
   - **RPN: 560**
   - **Root Cause**: No validation that user understands RDF/Turtle
   - **Impact**: User creates invalid ontologies with no error feedback
   - **Likelihood**: High (first-time users)
   - **Detectability**: Very low (silently produces no output)

2. **Failure Mode #1**: Manifest validation unclear
   - **RPN: 504**
   - **Root Cause**: Error messages don't guide user to solution
   - **Impact**: User stuck at first step
   - **Likelihood**: Very high (every new project)
   - **Detectability**: Immediate (error message exists but unclear)

3. **Failure Mode #2**: Missing ontology imports
   - **RPN: 432**
   - **Root Cause**: No validation that all referenced TTLs are listed in imports
   - **Impact**: Silent incomplete code generation
   - **Likelihood**: High (modular ontology design)
   - **Detectability**: Very low (no warnings generated)

4. **Failure Mode #3**: Circular dependency detection fails
   - **RPN: 360**
   - **Root Cause**: No cycle detection in dependency validation
   - **Impact**: Hang or timeout with confusing error
   - **Likelihood**: Medium (complex projects)
   - **Detectability**: Low (caught by timeout, not clear error)

5. **Failure Mode #10**: File write permissions not checked
   - **RPN: 240**
   - **Root Cause**: No pre-flight validation of write permissions
   - **Impact**: User thinks sync succeeded but code not updated
   - **Likelihood**: Medium (e.g., read-only artifact dirs)
   - **Detectability**: Very low (silent failure)

### 🟡 HIGH (RPN 100-200)

6. **Failure Mode #11**: SPARQL query syntax errors
   - **RPN: 210**
   - **Root Cause**: Oxigraph error messages are cryptic
   - **Likelihood**: Medium (users writing custom SPARQL)

7. **Failure Mode #9**: Typo in rule name goes undetected
   - **RPN: 192**
   - **Root Cause**: `--rule` flag accepts any string, should validate against manifest
   - **Likelihood**: Medium (users calling with typos)

8. **Failure Mode #8**: Cache invalidation silent
   - **RPN: 180**
   - **Root Cause**: No dependency tracking for cache validation
   - **Likelihood**: Low (only affects users with incremental cache enabled)

9. **Failure Mode #4**: Template syntax errors not pre-validated
   - **RPN: 294**
   - **Root Cause**: Templates not validated until render phase
   - **Likelihood**: High (users editing templates)

---

## Poka-Yoke Mitigations

### MITIGATION STRATEGY 1: Prevention (Best)

#### M1.1: Manifest Template Generation

**Problem**: User creates minimal, incomplete manifest → validation errors

**Prevention Mechanism**:
```bash
# New command:
$ ggen init --name my-project
✓ Created ggen.toml (fully annotated template)
✓ Created ontology/schema.ttl (starter ontology)
✓ Created templates/model.tera (starter template)
✓ Created .ggen/config.toml (advanced options)

# Template ggen.toml includes:
# - All required fields with defaults
# - Inline comments explaining each section
# - Examples for common patterns
# - Links to documentation
```

**Benefits**:
- User doesn't create manifest from scratch
- All required fields present (can't forget)
- Comments guide user through options
- Examples reduce guessing

**Poka-Yoke Type**: Prevent
- **Before**: User creates empty/minimal manifest → validation error
- **After**: User starts with complete template → validation passes

**Implementation**:
```rust
// New: crates/ggen-cli/src/cmds/init.rs

pub fn handle_init(name: String) -> Result<()> {
    // 1. Create project directory
    fs::create_dir_all(&name)?;

    // 2. Create ggen.toml from template
    let manifest_content = include_str!("templates/ggen.toml.template");
    let manifest = manifest_content
        .replace("${PROJECT_NAME}", &name);
    fs::write("ggen.toml", manifest)?;

    // 3. Create ontology/schema.ttl from template
    fs::create_dir_all("ontology")?;
    let ontology_content = include_str!("templates/schema.ttl.template");
    fs::write("ontology/schema.ttl", ontology_content)?;

    // 4. Create templates/ directory
    fs::create_dir_all("templates")?;
    let template_content = include_str!("templates/model.tera.template");
    fs::write("templates/model.tera", template_content)?;

    println!("✓ Project initialized: {}", name);
    println!("✓ Next: ggen sync");
    Ok(())
}
```

---

#### M1.2: Structured Validation with Error Guidance

**Problem**: Validation errors are cryptic, user doesn't know how to fix

**Prevention Mechanism**:
```rust
// Enhanced: crates/ggen-core/src/manifest/validation.rs

pub struct ValidationError {
    pub error: String,
    pub field: String,
    pub reason: String,
    pub suggestion: String,     // NEW: How to fix
    pub examples: Vec<String>,  // NEW: Examples of correct usage
    pub docs_link: String,      // NEW: Link to documentation
}

impl Display for ValidationError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "❌ Manifest Validation Error")?;
        writeln!(f)?;
        writeln!(f, "Field: {}", self.field)?;
        writeln!(f, "Error: {}", self.error)?;
        writeln!(f, "Reason: {}", self.reason)?;
        writeln!(f)?;
        writeln!(f, "💡 Suggestion: {}", self.suggestion)?;
        writeln!(f)?;
        writeln!(f, "Examples:")?;
        for example in &self.examples {
            writeln!(f, "  {}", example)?;
        }
        writeln!(f)?;
        writeln!(f, "📖 Learn more: {}", self.docs_link)?;
        Ok(())
    }
}
```

**Example Output**:
```
❌ Manifest Validation Error

Field: ontology.source
Error: File not found
Reason: The ontology source file "ontology/schema.ttl" does not exist

💡 Suggestion: Create the ontology file or update the path in ggen.toml

Examples:
  [ontology]
  source = "ontology/schema.ttl"

Or use `ggen init` to auto-generate:
  $ ggen init --name my-project

📖 Learn more: https://ggen.dev/docs/manifest
```

**Benefits**:
- Clear explanation of what's wrong
- Actionable suggestion to fix
- Examples of correct usage
- Link to documentation

---

#### M1.3: Ontology Import Validation

**Problem**: User creates multiple TTL files, only imports one → silent failure

**Prevention Mechanism**:
```rust
// New: crates/ggen-core/src/manifest/import_validation.rs

pub fn validate_ontology_imports(
    manifest: &GgenManifest,
) -> Result<ImportValidationReport> {
    let ontology_dir = manifest.ontology.source.parent().unwrap();

    // 1. Collect all .ttl files in ontology directory
    let all_ttl_files = fs::read_dir(ontology_dir)?
        .filter_map(|entry| {
            let path = entry?.path();
            if path.extension() == Some("ttl") {
                Some(path)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    // 2. Collect all imported files from manifest
    let mut imported_files = HashSet::new();
    imported_files.insert(manifest.ontology.source.clone());
    for import in &manifest.ontology.imports {
        imported_files.insert(import.clone());
    }

    // 3. Check for unused TTL files
    let unused: Vec<_> = all_ttl_files
        .iter()
        .filter(|file| !imported_files.contains(file))
        .collect();

    if !unused.is_empty() {
        return Err(Error::UnusedOntologyFiles {
            files: unused,
            suggestion: "Add to [ontology] imports section".to_string(),
        });
    }

    Ok(ImportValidationReport {
        total_ttl_files: all_ttl_files.len(),
        imported_files: imported_files.len(),
        unused_files: vec![],
    })
}
```

**Example Output**:
```
⚠️  Ontology Import Validation

Directory: ontology/
Found .ttl files:
  ✓ schema.ttl (imported as source)
  ✓ domain.ttl (imported)
  ❌ types.ttl (NOT IMPORTED)
  ❌ utils.ttl (NOT IMPORTED)

Suggestion: Add unused files to [ontology] imports:
  [ontology]
  imports = [
      "ontology/domain.ttl",
      "ontology/types.ttl",      # ADD THIS
      "ontology/utils.ttl",       # ADD THIS
  ]

Run again with --force to ignore this warning.
```

**Benefits**:
- Prevents silent incomplete imports
- User aware of all TTL files
- Clear suggestion to fix

---

#### M1.4: Circular Dependency Detection

**Problem**: A→B→A imports cause hang/timeout with no clear error

**Prevention Mechanism**:
```rust
// New: crates/ggen-core/src/manifest/cycle_detection.rs

pub struct DependencyGraph {
    edges: HashMap<PathBuf, Vec<PathBuf>>,
}

impl DependencyGraph {
    pub fn detect_cycles(&self) -> Result<Vec<Vec<PathBuf>>> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();
        let mut cycles = Vec::new();

        for node in self.edges.keys() {
            if !visited.contains(node) {
                self.dfs_cycle(node, &mut visited, &mut rec_stack, &mut cycles, vec![])?;
            }
        }

        if cycles.is_empty() {
            Ok(vec![])
        } else {
            Err(Error::CyclicDependency { cycles })
        }
    }

    fn dfs_cycle(
        &self,
        node: &Path,
        visited: &mut HashSet<PathBuf>,
        rec_stack: &mut HashSet<PathBuf>,
        cycles: &mut Vec<Vec<PathBuf>>,
        path: Vec<PathBuf>,
    ) -> Result<()> {
        visited.insert(node.to_path_buf());
        rec_stack.insert(node.to_path_buf());

        let mut new_path = path.clone();
        new_path.push(node.to_path_buf());

        if let Some(neighbors) = self.edges.get(node) {
            for neighbor in neighbors {
                if rec_stack.contains(neighbor) {
                    // Found cycle
                    let cycle_start_idx = new_path.iter()
                        .position(|p| p == neighbor)
                        .unwrap();
                    cycles.push(new_path[cycle_start_idx..].to_vec());
                } else if !visited.contains(neighbor) {
                    self.dfs_cycle(
                        neighbor,
                        visited,
                        rec_stack,
                        cycles,
                        new_path.clone(),
                    )?;
                }
            }
        }

        rec_stack.remove(node);
        Ok(())
    }
}
```

**Example Output**:
```
❌ Cyclic Dependency Detected

Ontology imports form a cycle:

  ontology/a.ttl
    → ontology/b.ttl
      → ontology/c.ttl
        → ontology/a.ttl  [CYCLE CLOSES HERE]

This creates an infinite loop in dependency resolution.

Solution: Restructure imports to be acyclic:

Option 1: Create a root ontology
  root.ttl (imports: a.ttl, b.ttl, c.ttl)
  a.ttl (imports: nothing)
  b.ttl (imports: a.ttl)
  c.ttl (imports: a.ttl)

Option 2: Merge redundant ontologies
  schema.ttl (contains all definitions)

Learn more: https://ggen.dev/docs/circular-dependencies
```

**Benefits**:
- Detects cycles BEFORE execution
- Clear visual representation
- Suggests solutions
- No timeout/hang

---

#### M1.5: Template Pre-Validation

**Problem**: Template syntax errors only caught at render time (late in pipeline)

**Prevention Mechanism**:
```rust
// New: crates/ggen-core/src/codegen/template_validation.rs

pub fn validate_templates(
    manifest: &GgenManifest,
    template_dir: &Path,
) -> Result<TemplateValidationReport> {
    let mut tera = Tera::new(&format!("{}/**/*.tera", template_dir.display()))?;
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // 1. Load all templates
    // 2. Check syntax by attempting to compile
    // 3. Validate variables against query schema

    for rule in &manifest.generation.rules {
        let template_path = template_dir.join(&rule.template);

        // Parse template
        let template_source = fs::read_to_string(&template_path)?;
        match tera.add_raw_template(&rule.name, &template_source) {
            Ok(_) => {
                // 3. Validate variables
                let required_vars = extract_template_variables(&template_source);
                let provided_vars = extract_query_columns(&rule.query)?;

                for var in required_vars {
                    if !provided_vars.contains(&var) {
                        errors.push(TemplateError {
                            file: rule.template.clone(),
                            line: find_variable_line(&template_source, &var),
                            message: format!("Variable '{}' not in query results", var),
                            suggestion: format!(
                                "Add to SELECT: {} (available: {:?})",
                                var,
                                provided_vars
                            ),
                        });
                    }
                }
            }
            Err(e) => {
                errors.push(TemplateError {
                    file: rule.template.clone(),
                    line: 0,
                    message: format!("Template syntax error: {}", e),
                    suggestion: "Check Tera documentation".to_string(),
                });
            }
        }
    }

    Ok(TemplateValidationReport {
        errors,
        warnings,
        valid: errors.is_empty(),
    })
}
```

**Example Output**:
```
⚠️  Template Validation

File: templates/model.tera
Line: 42
Error: Variable 'field_count' not in query results
Current variables: name, description, fields

Suggestion: Either add to query SELECT:
  SELECT ?name ?description ?fields ?field_count WHERE { ... }

Or fix template variable (available: name, description, fields):
  {% for field in fields %}
    - {{ field }}
  {% endfor %}

Learn more: https://ggen.dev/docs/templates
```

**Benefits**:
- Errors caught BEFORE generation phase
- Specific line numbers and variables
- Suggestions for fixes
- Prevents pipeline failures

---

### MITIGATION STRATEGY 2: Detection (Second Best)

#### M2.1: Improved Error Messages with Context

**Problem**: Error messages are cryptic (from Oxigraph, Tera, etc.)

**Detection Mechanism**:
```rust
// Enhanced: crates/ggen-core/src/error/mod.rs

pub enum GgenError {
    OxigraphError(String, String),  // NEW: error + context
    TeraError(String, String),
    ManifestError(String, String),
    // ...
}

impl Display for GgenError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            GgenError::OxigraphError(err, context) => {
                writeln!(f, "❌ RDF Processing Error")?;
                writeln!(f)?;
                writeln!(f, "Raw Error: {}", err)?;
                writeln!(f)?;
                writeln!(f, "Context: {}", context)?;
                writeln!(f)?;
                writeln!(f, "Common causes:")?;
                writeln!(f, "  - Invalid Turtle syntax in .ttl files")?;
                writeln!(f, "  - Undefined namespace prefixes")?;
                writeln!(f, "  - Invalid IRI format")?;
                writeln!(f)?;
                writeln!(f, "Try: ggen sync --validate-only (checks syntax)")?;
                Ok(())
            }
            GgenError::TeraError(err, context) => {
                writeln!(f, "❌ Template Rendering Error")?;
                writeln!(f, "Error: {}", err)?;
                writeln!(f, "Template: {}", context)?;
                writeln!(f)?;
                writeln!(f, "Try: ggen sync --dry-run (shows what would render)")?;
                Ok(())
            }
            // ...
            _ => write!(f, "{:?}", self),
        }
    }
}
```

**Benefits**:
- Users understand what went wrong
- Suggested next steps
- Links to documentation
- Example of correct format

---

#### M2.2: Validate-Only Mode as Quick Feedback

**Problem**: Errors discovered late in generation pipeline

**Detection Mechanism** (already exists, but improve):
```bash
# NEW: Use validate-only before full sync
$ ggen sync --validate-only
✓ Manifest schema: OK
✓ Ontology files: OK
✓ Ontology syntax: OK
✓ Ontology dependencies: OK (no cycles)
✓ SPARQL queries: OK
✓ Templates: OK
✓ Output directory permissions: OK

Ready to run: ggen sync
```

**Benefits**:
- Quick feedback (5-10s)
- Catches errors before generation
- User confidence before full run
- Can be added to CI/CD

---

#### M2.3: Rule Name Validation

**Problem**: User typos `--rule` flag, silently skips that rule

**Detection Mechanism**:
```rust
// Enhanced: crates/ggen-core/src/codegen/executor.rs

pub fn execute_full_sync(options: &SyncOptions) -> Result<SyncResult> {
    // ... load manifest ...

    // NEW: Validate rule names
    if let Some(rules) = &options.selected_rules {
        let available_rules: HashSet<_> = manifest
            .generation.rules
            .iter()
            .map(|r| r.name.clone())
            .collect();

        for rule in rules {
            if !available_rules.contains(rule) {
                // Suggest did-you-mean
                let suggestions = find_similar_rules(rule, available_rules);
                return Err(Error::RuleNotFound {
                    requested: rule.clone(),
                    available: available_rules.into_iter().collect(),
                    suggestions,
                });
            }
        }
    }

    // ... continue sync ...
}

fn find_similar_rules(typo: &str, available: HashSet<String>) -> Vec<String> {
    available
        .iter()
        .map(|rule| (rule, levenshtein_distance(typo, rule)))
        .filter(|(_, distance)| distance < &3)  // Within 2 edits
        .sorted_by_key(|(_, distance)| distance)
        .take(3)
        .map(|(rule, _)| rule.clone())
        .collect()
}
```

**Example Output**:
```
❌ Rule Not Found

Rule: "generate_struct" does not exist in manifest

Available rules:
  - generate_structs
  - generate_enums
  - generate_traits

Did you mean: "generate_structs" (1 character different)?

Usage: ggen sync --rule generate_structs
```

**Benefits**:
- Catches typos immediately
- Suggests correct names
- Prevents silent skips

---

#### M2.4: Cache Validation Checksum

**Problem**: Incremental cache becomes stale if ontology changes

**Detection Mechanism**:
```rust
// Enhanced: crates/ggen-core/src/codegen/cache.rs

pub struct CacheEntry {
    pub hash: String,                    // Hash of ontology files
    pub dependency_hashes: HashMap<String, String>,  // NEW: Hash deps
    pub generated_files: Vec<GeneratedFile>,
    pub timestamp: Instant,
}

pub fn validate_cache_fresh(
    cache: &CacheEntry,
    manifest: &GgenManifest,
) -> Result<bool> {
    // 1. Hash current ontology files
    let current_ontology_hash = hash_file(&manifest.ontology.source)?;

    // 2. Hash all imports
    let mut current_deps = HashMap::new();
    for import in &manifest.ontology.imports {
        let dep_hash = hash_file(import)?;
        current_deps.insert(import.display().to_string(), dep_hash);
    }

    // 3. Compare with cached hashes
    let ontology_changed = cache.hash != current_ontology_hash;
    let deps_changed = cache.dependency_hashes != current_deps;

    if ontology_changed || deps_changed {
        return Ok(false);  // Cache is stale
    }

    Ok(true)  // Cache is fresh
}

impl SyncExecutor {
    pub fn execute_with_cache(&self) -> Result<SyncResult> {
        let cache = self.load_cache()?;

        // NEW: Validate cache before using
        if !validate_cache_fresh(&cache, &self.manifest)? {
            eprintln!("⚠️  Cache is stale (ontology changed), regenerating");
            return self.execute_full_sync();  // Force regenerate
        }

        // Cache is fresh, use it
        Ok(self.use_cached_result(cache))
    }
}
```

**Example Output**:
```
⚠️  Cache Validation

Previous run: 5 minutes ago
Ontology state: CHANGED

✓ ontology/schema.ttl (modified 2m ago)
✗ ontology/domain.ttl (modified 30s ago - CHANGED!)

Cache is STALE, regenerating...
```

**Benefits**:
- Detects cache invalidation
- No silent stale code
- Automatic regeneration
- User aware of recalculation

---

#### M2.5: Pre-flight File Permission Check

**Problem**: User tries to write to read-only directory, failure is silent

**Detection Mechanism**:
```rust
// New: crates/ggen-core/src/codegen/preflight.rs

pub fn check_output_directory(manifest: &GgenManifest) -> Result<()> {
    let output_dir = &manifest.generation.output_dir;

    // 1. Check existence
    if !output_dir.exists() {
        fs::create_dir_all(output_dir)
            .with_context(|| format!("Cannot create output directory: {}", output_dir.display()))?;
    }

    // 2. Check writable
    let test_file = output_dir.join(".ggen-write-test");
    match fs::write(&test_file, "") {
        Ok(_) => {
            fs::remove_file(&test_file)?;
        }
        Err(e) => {
            return Err(Error::OutputDirNotWritable {
                path: output_dir.clone(),
                reason: e.to_string(),
                suggestion: "Check directory permissions: chmod u+w <dir>".to_string(),
            });
        }
    }

    // 3. Check available space (optional)
    let available = disk_available(output_dir)?;
    if available < 1_000_000 {  // 1MB minimum
        eprintln!(
            "⚠️  Warning: Only {}MB available in output directory",
            available / 1_000_000
        );
    }

    Ok(())
}

impl SyncExecutor {
    pub fn execute_full_sync(&self) -> Result<SyncResult> {
        // NEW: First step - preflight check
        check_output_directory(&self.manifest)?;

        // Then continue with sync...
        Ok(...)
    }
}
```

**Example Output**:
```
❌ Output Directory Not Writable

Path: src/generated/
Reason: Permission denied (os error 13)

Current permissions:
  dr-xr-xr-x  user  group  src/generated/

Solutions:
1. Make directory writable:
   $ chmod u+w src/generated/

2. Or change output_dir in ggen.toml:
   [generation]
   output_dir = "src/generated_output"

3. Or check if running in wrong directory
```

**Benefits**:
- Catches write failures BEFORE attempting generation
- Clear error message
- Actionable solutions
- Prevents data consistency issues

---

### MITIGATION STRATEGY 3: Reaction (Last Resort)

#### M3.1: User Confirmation for Destructive Operations

**Problem**: Generated files overwrite user code with no option to recover

**Reaction Mechanism**:
```rust
// Enhanced: crates/ggen-core/src/codegen/file_writer.rs

pub enum FileWriteMode {
    Overwrite,           // Default: silently overwrite
    AskIfUserModified,   // NEW: Warn if user edited
    Append,              // NEW: Append to existing
    CreateNew,           // NEW: Error if exists
}

pub struct FileWriter {
    mode: FileWriteMode,
}

impl FileWriter {
    pub fn write_file(
        &self,
        path: &Path,
        content: &str,
    ) -> Result<()> {
        match self.mode {
            FileWriteMode::AskIfUserModified => {
                if path.exists() {
                    // Check if file was modified after last generation
                    let file_metadata = fs::metadata(path)?;
                    let file_mtime = file_metadata.modified()?;

                    // Assume last generation was ~now
                    // If file newer than generated markers, user modified it
                    if self.is_likely_user_modified(path)? {
                        println!("⚠️  File appears to contain user modifications:");
                        println!("  {}", path.display());
                        println!();
                        println!("Options:");
                        println!("  [O] Overwrite (lose modifications)");
                        println!("  [S] Skip this file");
                        println!("  [B] Backup original first");
                        println!("  [A] Abort sync");
                        println!();
                        print!("Enter choice (O/S/B/A): ");

                        let mut choice = String::new();
                        std::io::stdin().read_line(&mut choice)?;

                        match choice.trim() {
                            "O" => fs::write(path, content)?,
                            "S" => return Ok(()),  // Skip
                            "B" => {
                                let backup = format!("{}.backup", path.display());
                                fs::copy(path, &backup)?;
                                println!("  ✓ Backed up to {}", backup);
                                fs::write(path, content)?;
                            }
                            "A" => return Err(Error::SyncAborted),
                            _ => {
                                println!("Invalid choice");
                                return self.write_file(path, content);  // Retry
                            }
                        }
                    } else {
                        fs::write(path, content)?;
                    }
                } else {
                    fs::write(path, content)?;
                }
                Ok(())
            }
            _ => fs::write(path, content).map_err(Into::into),
        }
    }

    fn is_likely_user_modified(&self, path: &Path) -> Result<bool> {
        // Heuristic: Check if file contains user markers
        let content = fs::read_to_string(path)?;

        // Look for hand-written code patterns
        let user_markers = [
            "// USER CODE",
            "// MANUAL EDIT",
            "// TODO",
            "// FIXME",
        ];

        Ok(user_markers.iter().any(|marker| content.contains(marker)))
    }
}
```

**Example Output**:
```
⚠️  File appears to contain user modifications:
  src/generated/models.rs

Options:
  [O] Overwrite (lose modifications)
  [S] Skip this file
  [B] Backup original first
  [A] Abort sync

Enter choice (O/S/B/A): B
  ✓ Backed up to src/generated/models.rs.backup
  ✓ Updated src/generated/models.rs

Review changes:
  $ diff src/generated/models.rs.backup src/generated/models.rs
```

**Benefits**:
- Prevents accidental data loss
- Gives user options
- Creates backup
- User can review changes

---

#### M3.2: Stop-the-Line on Critical Errors (Andon Signal)

**Problem**: Generation proceeds even with serious issues

**Reaction Mechanism** (already exists, improve):
```rust
// Enhanced: crates/ggen-core/src/andon/signal.rs

pub enum AndonSignal {
    Red {        // STOP IMMEDIATELY
        message: String,
        action_required: String,
    },
    Yellow {     // CAUTION
        message: String,
        suggestion: String,
    },
    Green,       // PROCEED
}

impl AndonSignal {
    pub fn enforce(&self, force: bool) -> Result<()> {
        match self {
            AndonSignal::Red { message, action_required } => {
                eprintln!("🔴 CRITICAL ERROR");
                eprintln!("{}", message);
                eprintln!();
                eprintln!("REQUIRED ACTION:");
                eprintln!("{}", action_required);
                eprintln!();

                if force {
                    eprintln!("⚠️  Override with --force (not recommended)");
                }

                Err(Error::CriticalError(message.clone()))
            }
            AndonSignal::Yellow { message, suggestion } => {
                eprintln!("🟡 WARNING");
                eprintln!("{}", message);
                eprintln!();
                eprintln!("Suggestion: {}", suggestion);
                eprintln!();
                eprintln!("Continuing (use --strict to error on warnings)");
                Ok(())
            }
            AndonSignal::Green => Ok(()),
        }
    }
}
```

**Example**: Manifest Error → RED Signal
```
$ ggen sync

🔴 CRITICAL ERROR
Manifest validation failed:
  - Field 'ontology.source' is required
  - Field 'generation.output_dir' is required
  - Field 'generation.rules' must have at least 1 rule

REQUIRED ACTION:
1. Open ggen.toml
2. Add required fields
3. Run `ggen init` for template

Or override: ggen sync --force (not recommended)
```

**Benefits**:
- Clear, unmistakable error signals
- User knows must stop and fix
- Emergency override available
- Strict mode for CI/CD

---

#### M3.3: Comprehensive Audit Trail

**Problem**: When things go wrong, user has no visibility into what happened

**Reaction Mechanism**:
```rust
// Enhanced: crates/ggen-core/src/codegen/audit.rs

pub struct AuditTrail {
    pub execution_id: String,
    pub timestamp: DateTime<Utc>,
    pub manifest_hash: String,
    pub ontology_hash: String,
    pub events: Vec<AuditEvent>,
}

pub enum AuditEvent {
    ManifestLoaded { path: String },
    OntologyLoaded { path: String, triple_count: usize },
    RuleExecuted { name: String, duration_ms: u64, output_count: usize },
    FileWritten { path: String, hash: String, byte_count: usize },
    Error { stage: String, message: String, timestamp: u64 },
}

impl AuditTrail {
    pub fn write_report(&self, output_dir: &Path) -> Result<()> {
        let report_path = output_dir.join(".ggen")
            .join("audit")
            .join(format!("{}.json", self.execution_id));

        fs::create_dir_all(report_path.parent().unwrap())?;
        let json = serde_json::to_string_pretty(&self)?;
        fs::write(&report_path, json)?;

        println!("📋 Audit trail: {}", report_path.display());
        Ok(())
    }
}

impl SyncExecutor {
    pub fn execute_full_sync(&self) -> Result<SyncResult> {
        let mut audit = AuditTrail::new();

        // ... execution ...

        if self.options.audit {
            audit.write_report(&self.manifest.generation.output_dir)?;
        }

        Ok(result)
    }
}
```

**Audit File** (`.ggen/audit/sync-20260106-143022.json`):
```json
{
  "execution_id": "sync-20260106-143022",
  "timestamp": "2026-01-06T14:30:22Z",
  "manifest_hash": "abc123...",
  "events": [
    {
      "type": "ManifestLoaded",
      "path": "ggen.toml",
      "timestamp": 1000
    },
    {
      "type": "OntologyLoaded",
      "path": "ontology/schema.ttl",
      "triple_count": 248,
      "timestamp": 1100
    },
    {
      "type": "RuleExecuted",
      "name": "generate_structs",
      "duration_ms": 245,
      "output_count": 12,
      "timestamp": 2345
    },
    {
      "type": "FileWritten",
      "path": "src/generated/models.rs",
      "hash": "def456...",
      "byte_count": 12450,
      "timestamp": 2500
    }
  ]
}
```

**Benefits**:
- Complete visibility into execution
- Can debug failures post-mortem
- Track what changed and when
- Useful for CI/CD diagnostics

---

## Poka-Yoke Framework for ggen sync

### Core Poka-Yoke Principles Applied

**1. Prevention (Yokuten)**: Make mistakes physically or logically impossible

**2. Detection (Shakokugel)**: Make mistakes immediately visible

**3. Reaction (Kakunin)**: Stop the process when mistakes detected

### Poka-Yoke Type Examples in ggen sync

#### Type 1: Automatic Prevention

**Mechanism**: NewType pattern prevents invalid states at compile time

```rust
// PREVENT: Can't create invalid manifest without validation
#[derive(Clone)]
pub struct ValidatedManifest(GgenManifest);

impl ValidatedManifest {
    pub fn new(path: &Path) -> Result<Self> {
        let manifest = GgenManifest::parse(path)?;

        // Validation enforced here - can't bypass
        validate_manifest(&manifest)?;
        validate_ontology_imports(&manifest)?;
        validate_circular_dependencies(&manifest)?;

        Ok(ValidatedManifest(manifest))
    }

    // Can only get inner manifest after validation
    pub fn inner(&self) -> &GgenManifest {
        &self.0
    }
}

// Usage - validation is mandatory:
let manifest = ValidatedManifest::new("ggen.toml")?;  // MUST validate
let m = manifest.inner();  // Now safe to use
```

**Effect**: User cannot proceed without passing validation

---

#### Type 2: Physical/Logical Constraints

**Mechanism**: Directory structure prevents mixing concerns

```
ggen-project/
├── ggen.toml              # Configuration (protected)
├── ontology/              # RDF source of truth (read-only during sync)
│   ├── schema.ttl         # Main ontology
│   ├── domain.ttl         # Domain-specific
│   └── types.ttl          # Type definitions
├── templates/             # Generation templates (validated)
│   ├── model.tera
│   ├── enum.tera
│   └── validation.tera
├── src/generated/         # AUTO-GENERATED (regenerated each sync)
│   ├── models.rs          # 🔴 DON'T EDIT - will be overwritten
│   ├── enums.rs
│   └── generated.rs
├── src/manual/            # USER CODE (protected from overwrites)
│   ├── implementations.rs
│   ├── custom_logic.rs
│   └── business_rules.rs
└── .ggen/                 # Internal
    ├── audit/             # Execution audit trail
    ├── cache/             # Incremental cache
    └── markers/           # Generation markers
```

**Effect**: Physical directory structure prevents mistakes

---

#### Type 3: Automatic Detection & Warning

**Mechanism**: Pre-execution validation catches issues before generation

```rust
// DETECT: Warn before problem occurs
pub fn preflight_checks(manifest: &GgenManifest) -> Result<PreflightReport> {
    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    // Check 1: Unused TTL files
    for unused in detect_unused_ttl_files(manifest)? {
        warnings.push(format!(
            "Unused ontology file: {} (add to imports if needed)",
            unused.display()
        ));
    }

    // Check 2: Missing template files
    for rule in &manifest.generation.rules {
        if !rule.template.exists() {
            errors.push(format!(
                "Template not found for rule '{}': {}",
                rule.name,
                rule.template.display()
            ));
        }
    }

    // Check 3: Circular dependencies
    if let Some(cycles) = detect_circular_dependencies(manifest)? {
        errors.push(format!(
            "Circular dependencies detected: {:?}",
            cycles
        ));
    }

    // Check 4: Output directory writability
    if !is_writable(&manifest.generation.output_dir) {
        errors.push(format!(
            "Output directory not writable: {}",
            manifest.generation.output_dir.display()
        ));
    }

    // Check 5: Manifest schema compliance
    validate_manifest_schema(manifest)?;

    if !errors.is_empty() {
        return Err(Error::PreflightCheckFailed { errors, warnings });
    }

    Ok(PreflightReport { warnings })
}
```

**Effect**: Issues caught BEFORE generation phase starts

---

### Implementation Priority

#### Phase 1: CRITICAL (Week 1) - Prevention
- [ ] M1.1: Manifest template generation (`ggen init`)
- [ ] M1.2: Enhanced validation error messages
- [ ] M1.4: Circular dependency detection (AUTO-PREVENT)
- [ ] M2.5: Pre-flight permission checks (AUTO-DETECT)

#### Phase 2: HIGH (Week 2-3) - Detection
- [ ] M1.3: Ontology import validation
- [ ] M1.5: Template pre-validation
- [ ] M2.1: Improved error context
- [ ] M2.2: Improve validate-only mode
- [ ] M2.3: Rule name validation

#### Phase 3: MEDIUM (Week 4) - Reaction
- [ ] M2.4: Cache validation checksum
- [ ] M3.1: User confirmation for overwrites
- [ ] M3.2: Andon signals enforcement
- [ ] M3.3: Comprehensive audit trail

---

## Andon Signal System for ggen sync

**Andon** (安頓): Japanese "stop the line" protocol. When a defect is detected, stop immediately, don't hide it.

### Signal Levels

```
🔴 RED SIGNAL (STOP)
   ├─ Compilation error
   ├─ Manifest validation failure
   ├─ Circular dependency detected
   ├─ Permission denied on output
   ├─ SPARQL syntax error
   └─ Timeout exceeded

   Action: STOP immediately, fix required before proceeding

🟡 YELLOW SIGNAL (CAUTION)
   ├─ Unused TTL files in ontology/
   ├─ Template contains unused variables
   ├─ Cache invalidated (regenerating)
   ├─ Clippy warnings in generated code
   ├─ Performance: generation took >5s
   └─ Large output files (>100KB)

   Action: Investigate, review before release

🟢 GREEN SIGNAL (GO)
   ├─ All checks passed
   ├─ Manifest valid
   ├─ No circular dependencies
   ├─ Templates valid
   ├─ Permissions OK
   ├─ All rules executed
   └─ 0 warnings

   Action: PROCEED safely
```

### Andon Implementation in ggen sync

#### 1. RED Signal Handler

```rust
// crates/ggen-core/src/andon/signals.rs

pub enum AndonSignal {
    Red(CriticalError),
    Yellow(Warning),
    Green,
}

pub struct CriticalError {
    pub code: String,                    // e.g., "MANIFEST_INVALID"
    pub message: String,
    pub context: String,
    pub recovery_steps: Vec<String>,
    pub documentation_link: String,
}

impl AndonSignal {
    pub fn enforce(&self) -> Result<()> {
        match self {
            AndonSignal::Red(error) => {
                eprintln!("╔════════════════════════════════════════════╗");
                eprintln!("║ 🔴 ANDON SIGNAL: RED - STOP IMMEDIATELY   ║");
                eprintln!("╚════════════════════════════════════════════╝");
                eprintln!();
                eprintln!("Error Code: {}", error.code);
                eprintln!("Message: {}", error.message);
                eprintln!();
                eprintln!("Context:");
                for line in error.context.lines() {
                    eprintln!("  {}", line);
                }
                eprintln!();
                eprintln!("Recovery Steps:");
                for (i, step) in error.recovery_steps.iter().enumerate() {
                    eprintln!("  {}. {}", i + 1, step);
                }
                eprintln!();
                eprintln!("📖 Learn more: {}", error.documentation_link);
                eprintln!();
                eprintln!("Sync STOPPED. Fix error above and retry.");

                Err(Error::AndonRedSignal(error.code))
            }
            AndonSignal::Yellow(warning) => {
                eprintln!("⚠️  ANDON SIGNAL: YELLOW - CAUTION");
                eprintln!("{}", warning.message);
                eprintln!("  Suggestion: {}", warning.suggestion);
                eprintln!();
                eprintln!("Continuing (use --strict to error on warnings)");
                Ok(())
            }
            AndonSignal::Green => {
                println!("✓ 🟢 All Andon checks GREEN - proceeding");
                Ok(())
            }
        }
    }
}
```

#### 2. RED Signal Examples

**Example 1: Manifest Validation Failure**

```
╔════════════════════════════════════════════╗
║ 🔴 ANDON SIGNAL: RED - STOP IMMEDIATELY   ║
╚════════════════════════════════════════════╝

Error Code: MANIFEST_INVALID
Message: Manifest validation failed - 2 required fields missing

Context:
  File: ggen.toml
  Issues:
    1. [ontology].source is required (e.g., "ontology/schema.ttl")
    2. [generation].rules must contain at least 1 rule

Recovery Steps:
  1. Open ggen.toml in editor
  2. Add [ontology] section with source = "ontology/schema.ttl"
  3. Add [generation] section with at least one rule
  4. Or use `ggen init` to create valid template

📖 Learn more: https://ggen.dev/docs/manifest-format

Sync STOPPED. Fix error above and retry.
```

**Example 2: Circular Dependency Detected**

```
╔════════════════════════════════════════════╗
║ 🔴 ANDON SIGNAL: RED - STOP IMMEDIATELY   ║
╚════════════════════════════════════════════╝

Error Code: CIRCULAR_DEPENDENCY
Message: Circular dependency detected in ontology imports

Context:
  Cycle found:
    ontology/a.ttl → ontology/b.ttl
                  ↓
    ontology/c.ttl → ontology/a.ttl (CLOSES CYCLE)

  This creates infinite loop in dependency resolution

Recovery Steps:
  1. Review import statements in affected files
  2. Restructure as directed acyclic graph (DAG)
  3. Option A: Create root ontology that imports all
  4. Option B: Move shared definitions to base ontology
  5. Use `ggen sync --validate-only` to verify

📖 Learn more: https://ggen.dev/docs/ontology-structure

Sync STOPPED. Fix error above and retry.
```

**Example 3: Permission Denied**

```
╔════════════════════════════════════════════╗
║ 🔴 ANDON SIGNAL: RED - STOP IMMEDIATELY   ║
╚════════════════════════════════════════════╝

Error Code: OUTPUT_DIR_NOT_WRITABLE
Message: Cannot write to output directory

Context:
  Path: src/generated/
  Permissions: dr-xr-xr-x (755 - read-only)
  User: alice
  Group: developers

Recovery Steps:
  1. Make directory writable:
     $ chmod u+w src/generated/
  2. Or use different output directory:
     [generation]
     output_dir = "src/gen"  # Change in ggen.toml
  3. Or run with sudo (not recommended):
     $ sudo ggen sync

📖 Learn more: https://ggen.dev/docs/permissions

Sync STOPPED. Fix error above and retry.
```

#### 3. YELLOW Signal Examples

```
⚠️  ANDON SIGNAL: YELLOW - CAUTION

Found 3 unused ontology files:
  - ontology/deprecated.ttl (8 months old)
  - ontology/experimental.ttl (2 weeks old)
  - ontology/old_schema.ttl (1 year old)

Suggestion: If these aren't needed, delete them.
Otherwise add to imports: ontology.imports = [...].

Continuing (use --strict to error on warnings)
```

```
⚠️  ANDON SIGNAL: YELLOW - CAUTION

Template variable mismatch in templates/model.tera:
  Variable '?author' used but not in SELECT results
  Available: ?name, ?description, ?created_at

Suggestion: Fix query to include ?author:
  SELECT ?name ?description ?created_at ?author WHERE { ... }

Continuing (use --strict to error on warnings)
```

```
⚠️  ANDON SIGNAL: YELLOW - CAUTION

Performance: Generation took 8.5s (target: <5s)
  Inference rules: 3.2s
  Template rendering: 4.1s
  File writing: 1.2s

Suggestion: Consider optimizing SPARQL queries or caching.

Continuing (use --strict to error on warnings)
```

---

## Quality Gate System

**Quality Gate**: Mandatory checkpoints that must PASS before proceeding. Failure stops the pipeline.

### Pre-Sync Quality Gates (Preflight)

```
$ ggen sync

[Quality Gate: Manifest Schema]
  ✓ TOML parsing succeeds
  ✓ All required fields present
  ✓ Field types correct

[Quality Gate: Ontology Dependencies]
  ✓ ontology.source file exists
  ✓ All imports exist and readable
  ✓ No circular dependencies
  ✓ All files are valid Turtle syntax

[Quality Gate: SPARQL Validation]
  ✓ All SELECT queries are valid SPARQL
  ✓ All CONSTRUCT rules are valid SPARQL
  ✓ Query variables don't contain typos

[Quality Gate: Template Validation]
  ✓ All template files exist
  ✓ Template syntax is valid Tera
  ✓ Template variables match query results
  ✓ No undefined variable references

[Quality Gate: File Permissions]
  ✓ Output directory exists
  ✓ Output directory is writable
  ✓ Sufficient disk space (>10MB)

[Quality Gate: Rule Validation]
  ✓ All generation rules reference existing templates
  ✓ All selected rules (--rule) exist in manifest

All Gates: ✅ PASSED → Proceeding to generation phase
```

### Gate Implementation

```rust
// crates/ggen-core/src/quality_gates/mod.rs

pub trait QualityGate: Send + Sync {
    fn name(&self) -> &str;
    fn check(&self, manifest: &GgenManifest) -> Result<()>;
}

pub struct QualityGateRunner {
    gates: Vec<Box<dyn QualityGate>>,
}

impl QualityGateRunner {
    pub fn new() -> Self {
        QualityGateRunner {
            gates: vec![
                Box::new(ManifestSchemaGate),
                Box::new(OntologyDependenciesGate),
                Box::new(SparqlValidationGate),
                Box::new(TemplateValidationGate),
                Box::new(FilePermissionsGate),
                Box::new(RuleValidationGate),
            ],
        }
    }

    pub fn run_all(&self, manifest: &GgenManifest) -> Result<()> {
        for gate in &self.gates {
            print!("[Quality Gate: {}]", gate.name());
            match gate.check(manifest) {
                Ok(_) => println!(" ✓"),
                Err(e) => {
                    println!(" ✗");
                    return Err(AndonSignal::Red(CriticalError {
                        code: format!("GATE_{}", gate.name().to_uppercase()),
                        message: format!("Quality gate failed: {}", gate.name()),
                        context: e.to_string(),
                        recovery_steps: gate.recovery_suggestions(&e),
                        documentation_link: gate.docs_link(),
                    }));
                }
            }
        }

        println!("\nAll Gates: ✅ PASSED → Proceeding to generation phase");
        Ok(())
    }
}

// Individual gates:

pub struct ManifestSchemaGate;
impl QualityGate for ManifestSchemaGate {
    fn name(&self) -> &str { "Manifest Schema" }
    fn check(&self, manifest: &GgenManifest) -> Result<()> {
        // Validate manifest structure
        Ok(())
    }
}

pub struct OntologyDependenciesGate;
impl QualityGate for OntologyDependenciesGate {
    fn name(&self) -> &str { "Ontology Dependencies" }
    fn check(&self, manifest: &GgenManifest) -> Result<()> {
        // Check files exist, no cycles, valid TTL
        Ok(())
    }
}

pub struct SparqlValidationGate;
impl QualityGate for SparqlValidationGate {
    fn name(&self) -> &str { "SPARQL Validation" }
    fn check(&self, manifest: &GgenManifest) -> Result<()> {
        // Validate all queries
        Ok(())
    }
}

pub struct TemplateValidationGate;
impl QualityGate for TemplateValidationGate {
    fn name(&self) -> &str { "Template Validation" }
    fn check(&self, manifest: &GgenManifest) -> Result<()> {
        // Validate templates exist, syntax, variables
        Ok(())
    }
}

pub struct FilePermissionsGate;
impl QualityGate for FilePermissionsGate {
    fn name(&self) -> &str { "File Permissions" }
    fn check(&self, manifest: &GgenManifest) -> Result<()> {
        // Check write permissions, disk space
        Ok(())
    }
}

pub struct RuleValidationGate;
impl QualityGate for RuleValidationGate {
    fn name(&self) -> &str { "Rule Validation" }
    fn check(&self, manifest: &GgenManifest) -> Result<()> {
        // Check rules exist, templates referenced
        Ok(())
    }
}
```

---

## Stop-the-Line Protocol (Andon Pull Cord)

**Stop-the-Line**: When a defect is detected, STOP PRODUCTION immediately. Don't hide problems.

### When to Pull the Andon Cord (RED Signal)

```
PULL CORD IF ANY OF THESE OCCUR:

1. Manifest Validation Fails
   → Cannot proceed without valid manifest

2. Circular Dependencies Detected
   → Would cause infinite loops

3. File Permissions Error
   → Cannot write output

4. SPARQL Syntax Error
   → Query will fail at runtime

5. Template Validation Fails
   → Generation would fail mid-pipeline

6. Execution Timeout
   → Something is stuck/hanging

7. Cache Corruption Detected
   → Could generate stale code

8. Unexpected Exception
   → Unknown error state
```

### Protocol Flow

```
START ggen sync
    ↓
[Preflight Quality Gates]
    ├─ ANY GATE FAILS? → 🔴 RED SIGNAL
    │   ├─ Display error clearly
    │   ├─ Show recovery steps
    │   ├─ STOP immediately
    │   └─ Require manual fix
    │
    └─ ALL GATES PASS? → Continue
        ↓
    [Load Ontology]
        ├─ Error? → 🔴 RED SIGNAL
        └─ Success? → Continue
            ↓
        [Execute Inference Rules]
            ├─ Error/Timeout? → 🔴 RED SIGNAL
            └─ Success? → Continue
                ↓
            [Execute Generation Rules]
                ├─ Error? → 🔴 RED SIGNAL
                └─ Success? → Continue
                    ↓
                [Validate Output]
                    ├─ Warning? → 🟡 YELLOW SIGNAL (non-blocking)
                    └─ Error? → 🔴 RED SIGNAL
                        ↓
                    [Write Files]
                        ├─ Permission Error? → 🔴 RED SIGNAL
                        └─ Success? → Continue
                            ↓
                        [Generate Audit Trail]
                            ↓
                        [Done: 🟢 GREEN SIGNAL]
```

### Example: Stop-the-Line in Action

**User attempts sync with manifest error**:
```bash
$ ggen sync

[Quality Gate: Manifest Schema]
  ✗ FAILED - field 'ontology.source' is required

╔════════════════════════════════════════════╗
║ 🔴 ANDON SIGNAL: RED - STOP IMMEDIATELY   ║
╚════════════════════════════════════════════╝

Error Code: MANIFEST_INVALID
Message: Manifest validation failed

Recovery Steps:
  1. Add [ontology] section
  2. Set source = "ontology/schema.ttl"
  3. Try again

Sync STOPPED.
```

**User must fix the issue**:
```bash
$ cat >> ggen.toml <<EOF

[ontology]
source = "ontology/schema.ttl"
EOF

$ ggen sync

[Quality Gate: Manifest Schema] ✓
[Quality Gate: Ontology Dependencies] ✓
[Quality Gate: SPARQL Validation] ✓
[Quality Gate: Template Validation] ✓
[Quality Gate: File Permissions] ✓
[Quality Gate: Rule Validation] ✓

All Gates: ✅ PASSED → Proceeding to generation phase

✓ Sync completed: 5 files generated (234ms)
🟢 GREEN SIGNAL - All checks passed
```

---

## Error Classification & Responses

| Error Class | Severity | Response | Examples |
|-------------|----------|----------|----------|
| **Manifest** | CRITICAL | 🔴 RED STOP | Missing required fields, invalid TOML |
| **Dependency** | CRITICAL | 🔴 RED STOP | Missing files, circular imports |
| **Syntax** | CRITICAL | 🔴 RED STOP | Invalid TTL, invalid SPARQL |
| **Permission** | CRITICAL | 🔴 RED STOP | Cannot write output, file locked |
| **Timeout** | CRITICAL | 🔴 RED STOP | Execution exceeded limit |
| **Validation** | HIGH | 🟡 YELLOW WARN | Unused files, mismatched variables |
| **Performance** | MEDIUM | 🟡 YELLOW WARN | Slow queries, large outputs |
| **Info** | LOW | 🟢 GREEN OK | Cache hit, files updated |

---

## Deterministic Validation (Evidence-First)

**Instead of**: "Looks good!" (opinion)

**Use**: "[Receipt] Quality Gates: ✓ 6/6, No RED signals" (evidence)

### Receipt Format

```
╔════════════════════════════════════════════╗
║ SYNC RECEIPT                               ║
╚════════════════════════════════════════════╝

Execution ID: sync-20260106-143022
Status: ✅ SUCCESS
Duration: 234ms

QUALITY GATES: 6/6 ✓
  [✓] Manifest Schema
  [✓] Ontology Dependencies
  [✓] SPARQL Validation
  [✓] Template Validation
  [✓] File Permissions
  [✓] Rule Validation

ANDON SIGNALS:
  [🟢] All GREEN - No RED signals
  [🟡] 2 YELLOW warnings (--strict mode would fail)
        - Unused: ontology/deprecated.ttl
        - Performance: inference took 3.2s

GENERATION RESULTS:
  Ontology: ontology/schema.ttl (248 triples)
  Rules Executed:
    ✓ generate_structs (5 files, 2.1s)
    ✓ generate_enums (2 files, 0.8s)
    ✓ generate_traits (3 files, 1.3s)
  Files Written: 10 files (12.4 KB total)

CHECKSUMS:
  Input Hash: abc123def456...
  Output Hash: xyz789uvw012...
  Cache Valid: YES

AUDIT TRAIL: .ggen/audit/sync-20260106-143022.json

NEXT STEPS:
  ✓ Review generated files: src/generated/
  ✓ Run tests: cargo test
  ✓ Commit: git add src/generated/
```

---

## Constitutional Rules Enforcement

```rust
// Enforce constitution at runtime

pub struct ConstituationalGuard;

impl ConstituationalGuard {
    pub fn enforce_cargo_make_only() {
        // 🔴 RED: Never allow direct cargo commands
        // Only allow: cargo make check/test/lint
    }

    pub fn enforce_no_unwrap_in_production() {
        // 🟡 YELLOW: Warn if unwrap/expect found in generated code
        // 🔴 RED: Block if found in critical paths
    }

    pub fn enforce_andon_signals() {
        // All errors mapped to RED/YELLOW/GREEN
        // No silent failures allowed
        // Errors must have recovery steps
    }

    pub fn enforce_rdf_first() {
        // 🔴 RED: Reject edits to .md files (edit .ttl)
        // Warn if .md generated but .ttl source not updated
    }

    pub fn enforce_error_handling() {
        // 🔴 RED: No panics in production code
        // All external APIs must return Result<T, E>
        // All user input must be validated
    }
}
```

---

## Success Criteria

After implementing all mitigations:

| Failure Mode | Before | After | Status |
|--------------|--------|-------|--------|
| #1: Manifest unclear | RPN 504 | RPN 100 | M1.1 + M1.2 |
| #2: Missing imports | RPN 432 | RPN 60 | M1.3 |
| #3: Circular deps | RPN 360 | RPN 30 | M1.4 |
| #4: Template errors | RPN 294 | RPN 80 | M1.5 |
| #5: Overwrite code | RPN 50 | RPN 20 | M3.1 |
| #6: Timeout | RPN 42 | RPN 20 | Improve docs |
| #7: Format confusion | RPN 48 | RPN 15 | Better help |
| #8: Cache stale | RPN 180 | RPN 40 | M2.4 |
| #9: Rule typo | RPN 192 | RPN 60 | M2.3 |
| #10: Permission denied | RPN 240 | RPN 30 | M2.5 |
| #11: SPARQL errors | RPN 210 | RPN 100 | M2.1 |
| #12: TTL fundamental | RPN 560 | RPN 150 | M1.1 + Docs |

**Target**:
- All CRITICAL failures → HIGH/MEDIUM (> 50% reduction)
- Zero silent failures
- All errors have actionable guidance
- 95%+ of new users succeed with `ggen init`

---

## User Experience Journey (After Mitigations)

### New User Flow (Day 1)
```bash
$ ggen init --name my-project
✓ Project initialized

$ cd my-project

$ ggen sync --validate-only
✓ Manifest: OK
✓ Ontology: OK (3 classes, 12 properties)
✓ SPARQL queries: OK
✓ Templates: OK

$ ggen sync
✓ Sync completed: 5 files generated (234ms)
✓ Audit trail: .ggen/audit/sync-20260106-143022.json
```

### Experienced User Flow (Iteration)
```bash
$ ggen sync --dry-run
Would generate:
  ✓ src/generated/models.rs (125 lines)
  ✓ src/generated/queries.rs (89 lines)

$ ggen sync
⚠️  File appears to contain user modifications:
  src/generated/models.rs

Options:
  [O] Overwrite  [S] Skip  [B] Backup  [A] Abort
Enter choice: B
  ✓ Backed up to src/generated/models.rs.backup
  ✓ Updated 2 files (156ms)
```

### Troubleshooting Flow
```bash
$ ggen sync
❌ SPARQL Query Error

Rule: "generate_structs"
Error: Undefined variable ?nonexistent

Suggestion:
  Check variable names in SELECT clause:
    SELECT ?nonexistent ?name ?type WHERE { ... }

Available variables from ontology:
  - ?class, ?name, ?description, ?field

Learn more: https://ggen.dev/docs/sparql

$ ggen sync --audit
📋 Detailed execution log: .ggen/audit/sync-20260106-143045.json
```

---

## Conclusion

The FMEA analysis identifies **12 major failure modes** affecting confused users attempting `ggen sync`. The highest-risk issues (RPN > 200) stem from:

1. **Lack of guided initialization** - Users create invalid manifests with no template
2. **Silent failures** - Missing imports, incomplete ontologies, stale caches not detected
3. **Late error detection** - Template/SPARQL errors found during generation, not validation
4. **Destructive operations** - Generated files overwrite user code without confirmation
5. **Unclear error messages** - Cryptic library errors without context or suggestions

The proposed **Poka-Yoke mitigations** address these through:

- **Prevention** (M1): Make mistakes impossible (ggen init, validation, cycle detection)
- **Detection** (M2): Catch issues early (validate-only, pre-flight checks, audit trails)
- **Reaction** (M3): Confirm before destructive operations, comprehensive error feedback

Implementation of these mitigations will reduce the highest-risk failures by 60-75%, transforming `ggen sync` from a frustrating experience for confused users into a reliable, guidance-focused tool.

---

## References

- Poka-Yoke Principles: Toyota Production System (mistake-proofing)
- FMEA Methodology: AIAG Potential Failure Mode and Effects Analysis
- Error Handling Patterns: Rust Result<T,E> and Context traits
- UX Principles: Clear feedback, actionable suggestions, prevention over cure
