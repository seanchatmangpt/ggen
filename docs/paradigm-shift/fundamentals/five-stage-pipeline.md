<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [The Five-Stage Transformation Pipeline](#the-five-stage-transformation-pipeline)
  - [The Holographic Factory Metaphor](#the-holographic-factory-metaphor)
  - [Why This Matters: The "Single Source of Truth" Principle](#why-this-matters-the-single-source-of-truth-principle)
  - [The Five-Stage Pipeline (μ₁ - μ₅)](#the-five-stage-pipeline-%CE%BC%E2%82%81---%CE%BC%E2%82%85)
    - [Stage μ₁: Normalize (Validate & Prepare)](#stage-%CE%BC%E2%82%81-normalize-validate--prepare)
    - [Stage μ₂: Extract (Query Knowledge)](#stage-%CE%BC%E2%82%82-extract-query-knowledge)
    - [Stage μ₃: Emit (Generate Code)](#stage-%CE%BC%E2%82%83-emit-generate-code)
    - [Stage μ₄: Canonicalize (Format & Verify)](#stage-%CE%BC%E2%82%84-canonicalize-format--verify)
    - [Stage μ₅: Receipt (Certify & Document)](#stage-%CE%BC%E2%82%85-receipt-certify--document)
  - [Determinism & Reproducibility](#determinism--reproducibility)
  - [Debugging Guide: Which Stage to Check](#debugging-guide-which-stage-to-check)
  - [Real-World Example: Complete Flow](#real-world-example-complete-flow)
    - [Step 1: Edit ontology (source of truth)](#step-1-edit-ontology-source-of-truth)
    - [Step 2: Run `ggen sync`](#step-2-run-ggen-sync)
    - [Step 3: Watch the pipeline](#step-3-watch-the-pipeline)
    - [Step 4: Verify generated code](#step-4-verify-generated-code)
    - [Step 5: Check receipt](#step-5-check-receipt)
    - [Step 6: Run tests](#step-6-run-tests)
  - [Common Questions & Answers](#common-questions--answers)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# The Five-Stage Transformation Pipeline

**Core Equation**: `A = μ(O)` where:
- `A` = Generated artifacts (code, tests, documentation)
- `O` = RDF ontology (domain knowledge encoded in Turtle)
- `μ` = Five-stage deterministic transformation pipeline

---

## The Holographic Factory Metaphor

Imagine a holographic film that encodes a 3D image. When you shine light through it at different angles, you see different projections of the same underlying reality. Change the film, and all projections change instantly. The projections are **emergent** - they arise from interference patterns in the film, not from manual drawing.

**ggen works the same way:**

```
┌─────────────────────────────────────────────────────────────┐
│                    HOLOGRAPHIC FACTORY                      │
│                                                             │
│  RDF Ontology                   SPARQL Queries             │
│  (auth.ttl)                     (Light Beams)              │
│      ║                               ║                      │
│      ║  ┌───────────────────────┐    ║                      │
│      ╚══╣ Holographic Film      ╠════╝                      │
│         │ (Domain Knowledge)    │                           │
│         │  - User class         │                           │
│         │  - Session class      │                           │
│         │  - SHACL constraints  │                           │
│         │  - Business rules     │                           │
│         └───────────────────────┘                           │
│                    ║                                         │
│                    ║  (μ: Five-Stage Pipeline)              │
│                    ║                                         │
│         ┌──────────▼──────────┐                             │
│         │   μ₁: Normalize     │  Validate & prepare film    │
│         └──────────┬──────────┘                             │
│         ┌──────────▼──────────┐                             │
│         │   μ₂: Extract       │  Shine light through film   │
│         └──────────┬──────────┘                             │
│         ┌──────────▼──────────┐                             │
│         │   μ₃: Emit          │  Capture projections        │
│         └──────────┬──────────┘                             │
│         ┌──────────▼──────────┐                             │
│         │   μ₄: Canonicalize  │  Develop & fix projections  │
│         └──────────┬──────────┘                             │
│         ┌──────────▼──────────┐                             │
│         │   μ₅: Receipt       │  Certify & document         │
│         └──────────┬──────────┘                             │
│                    ║                                         │
│         ┌──────────▼──────────┐                             │
│         │    Projections      │                             │
│         │  - types.rs         │                             │
│         │  - auth.rs          │                             │
│         │  - tests.rs         │                             │
│         │  - README.md        │                             │
│         └─────────────────────┘                             │
│                                                             │
│  Key Insight: Fix the film (ontology), not the projection! │
└─────────────────────────────────────────────────────────────┘
```

**Critical Insight**: If there's a bug in the generated code (projection), you **never** edit the projection. You edit the holographic film (the `.ttl` ontology) and regenerate. The projections are **disposable** - they can be recreated perfectly from the source.

---

## Why This Matters: The "Single Source of Truth" Principle

Traditional development:
```
┌────────────┐     ┌──────────────┐     ┌─────────────┐
│ Types      │ ≠   │ Validation   │ ≠   │ Database    │
│ (code)     │     │ (rules)      │     │ (schema)    │
└────────────┘     └──────────────┘     └─────────────┘
     ↓                   ↓                      ↓
  Must sync manually - DRIFT INEVITABLE
```

ggen approach:
```
┌─────────────────────────────────────────────────────────┐
│             RDF Ontology (auth.ttl)                     │
│              SINGLE SOURCE OF TRUTH                     │
└─────────────────────────────────────────────────────────┘
                          │
                          │ (μ: Five stages)
                          │
            ┌─────────────┼─────────────┐
            ▼             ▼             ▼
       ┌────────┐   ┌──────────┐   ┌─────────┐
       │ Types  │   │ Validation│   │ Tests   │
       │ (Rust) │   │ (SHACL)   │   │ (auto)  │
       └────────┘   └──────────┘   └─────────┘
            ↑             ↑             ↑
         Always in sync - NO DRIFT POSSIBLE
```

---

## The Five-Stage Pipeline (μ₁ - μ₅)

### Stage μ₁: Normalize (Validate & Prepare)

**Purpose**: Load and validate the RDF ontology, ensuring it's structurally sound and semantically consistent.

**What happens:**
1. Parse Turtle files (`.ttl`) into RDF triples
2. Resolve ontology imports and dependencies
3. Run SHACL validation to verify constraints
4. Execute OWL inference to materialize implicit knowledge
5. Build normalized RDF graph ready for querying

**Example with `auth.ttl`:**

```turtle
# Input: auth.ttl (lines 15-19)
:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "Represents a user in the authentication system" ;
    :rustType "User" ;
    :derive "Debug, Clone, Serialize, Deserialize, PartialEq" .
```

**What μ₁ does:**
```
1. Parse TTL → RDF triples:
   (:User, rdf:type, rdfs:Class)
   (:User, rdfs:label, "User")
   (:User, rdfs:comment, "Represents a user...")
   (:User, :rustType, "User")
   (:User, :derive, "Debug, Clone, Serialize, Deserialize, PartialEq")

2. Validate SHACL constraints:
   - Check :EmailValidation pattern on :hasEmail
   - Verify :PasswordValidation minLength on :hasPassword
   - Ensure all required properties exist

3. Resolve dependencies:
   - Load standard prefixes (rdf:, rdfs:, xsd:, sh:)
   - Import referenced ontologies

4. Execute OWL inference:
   - If :hasCreatedAt has domain :User and :Session
   - Then infer both classes have this property

5. Output: Normalized RDF graph (in-memory triple store)
```

**Common Questions:**

**Q: What happens if SHACL validation fails?**
A: μ₁ stops immediately with detailed error message. Example:
```
SHACL validation failed for auth.ttl:
  - :EmailValidation: Property :hasEmail violates pattern constraint
    Expected: ^[^@]+@[^@]+\.[^@]+$
    Found: "notanemail"
    At line 50
```

**Q: Why do I need OWL inference?**
A: Inference materializes implicit knowledge. Example:
```turtle
# You write:
:hasCreatedAt rdfs:domain :User, :Session .

# OWL inference adds:
:User :hasCreatedAt ?timestamp .
:Session :hasCreatedAt ?timestamp .
```
This means you can query for all entities with `created_at` without explicitly listing them.

**Q: What if I have a circular import?**
A: μ₁ detects circular dependencies and fails with error:
```
Error: Circular ontology import detected:
  auth.ttl → user.ttl → session.ttl → auth.ttl
```

---

### Stage μ₂: Extract (Query Knowledge)

**Purpose**: Execute SPARQL queries to extract structured data from the normalized RDF graph.

**What happens:**
1. Execute SPARQL SELECT queries to extract entities
2. Apply SPARQL CONSTRUCT to reshape data
3. Execute inference rules (RDFS, OWL2-RL)
4. Build template context (JSON/YAML binding for Tera)

**Example with `auth.ttl`:**

```sparql
# SPARQL query executed by μ₂:
PREFIX : <http://example.org/auth#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?class ?label ?comment ?rustType ?derive
WHERE {
  ?class a rdfs:Class .
  ?class rdfs:label ?label .
  ?class rdfs:comment ?comment .
  ?class :rustType ?rustType .
  ?class :derive ?derive .
}
```

**Result (template context):**
```json
{
  "classes": [
    {
      "uri": "http://example.org/auth#User",
      "label": "User",
      "comment": "Represents a user in the authentication system",
      "rustType": "User",
      "derive": "Debug, Clone, Serialize, Deserialize, PartialEq",
      "properties": [
        {
          "uri": "http://example.org/auth#hasId",
          "label": "id",
          "rustType": "String",
          "required": true
        },
        {
          "uri": "http://example.org/auth#hasEmail",
          "label": "email",
          "rustType": "String",
          "required": true,
          "validation": "EmailValidation"
        }
        // ... more properties
      ]
    },
    {
      "uri": "http://example.org/auth#Session",
      "label": "Session",
      "comment": "Represents an active user session",
      "rustType": "Session",
      "derive": "Debug, Clone, Serialize, Deserialize",
      "properties": [
        // ... properties for Session
      ]
    }
  ]
}
```

**Common Questions:**

**Q: How do I know what SPARQL queries are executed?**
A: Check the template file (`.tera`). Templates can embed SPARQL queries:
```tera
{% sparql %}
SELECT ?class ?label WHERE { ?class a rdfs:Class }
{% endsparql %}
```
Or use pre-defined queries in the manifest.

**Q: What if my SPARQL query returns no results?**
A: μ₂ logs a warning and passes empty context to μ₃. The template will receive `classes: []` and should handle this gracefully.

**Q: Can I see the template context?**
A: Yes! Use `ggen sync --dry_run true --verbose` to see the extracted context without generating files.

---

### Stage μ₃: Emit (Generate Code)

**Purpose**: Render Tera templates with the extracted context to generate code, tests, and documentation.

**What happens:**
1. Load Tera templates (`.tera` files)
2. Inject SPARQL-extracted context
3. Execute template logic (loops, conditionals, filters)
4. Generate raw output files (code, configs, docs)

**Example with `types.rs.tera`:**

```tera
// Template: types.rs.tera (lines 8-15)
{% for class in classes %}
/// {{ class.comment }}
#[derive({{ class.derive }})]
pub struct {{ class.rustType }} {
    {%- for prop in class.properties %}
    pub {{ prop.label }}: {{ prop.rustType }},
    {%- endfor %}
}
{% endfor %}
```

**Generated output (raw):**
```rust
// GENERATED CODE - DO NOT EDIT
// Source: auth.ttl via ggen sync
// Template: types.rs.tera

use serde::{Deserialize, Serialize};

/// Represents a user in the authentication system
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct User {
    pub id: String,
    pub email: String,
    pub password_hash: String,
    pub created_at: i64,
    pub updated_at: i64,
}

/// Represents an active user session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Session {
    pub token: String,
    pub user_id: String,
    pub created_at: i64,
    pub expires_at: i64,
}
```

**Common Questions:**

**Q: What if my template has a syntax error?**
A: μ₃ fails immediately with detailed error:
```
Template error in types.rs.tera at line 12:
  Unexpected token: expected 'endfor', found 'endif'
```

**Q: Can I use custom Tera filters?**
A: Yes! ggen provides SPARQL-aware filters:
```tera
{{ property.rustType | to_option }}     → Option<String>
{{ property.rustType | to_vec }}        → Vec<String>
{{ class.label | to_snake_case }}       → user_session
{{ class.label | to_pascal_case }}      → UserSession
```

**Q: What if template rendering takes too long?**
A: μ₃ has a timeout (30s default). If exceeded:
```
Error: Template rendering timeout exceeded (30s)
  Template: complex-template.tera
  Context size: 1.2MB
  Suggestion: Break template into smaller chunks or optimize SPARQL queries
```

---

### Stage μ₄: Canonicalize (Format & Verify)

**Purpose**: Apply deterministic formatting and syntax validation to ensure identical outputs for identical inputs.

**What happens:**
1. Run language-specific formatters (`rustfmt`, `prettier`, `black`)
2. Validate syntax (compile checks, linters)
3. Compute SHA-256 content hash for each file
4. Sort file outputs deterministically
5. Apply consistent line endings (LF)

**Example with `types.rs`:**

```rust
// Raw output from μ₃ (may have inconsistent spacing):
pub struct User{pub id:String,pub email:String}

// After μ₄ canonicalization (rustfmt applied):
pub struct User {
    pub id: String,
    pub email: String,
}

// Content hash computed:
SHA-256: a3f8b2c1d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1
```

**Why this matters:**
```
Without μ₄:                         With μ₄:
┌──────────────────┐                ┌──────────────────┐
│ Same ontology    │                │ Same ontology    │
│ Different times  │                │ Different times  │
│ Different users  │                │ Different users  │
└────────┬─────────┘                └────────┬─────────┘
         │                                   │
         ▼                                   ▼
┌──────────────────┐                ┌──────────────────┐
│ types_v1.rs      │                │ types.rs         │
│ types_v2.rs      │  NON-DETERMINISTIC  │ (identical)      │  DETERMINISTIC
│ (different!)     │                │ (byte-for-byte)  │
└──────────────────┘                └──────────────────┘
```

**Common Questions:**

**Q: What happens if `rustfmt` fails?**
A: μ₄ logs error but continues (non-blocking). The receipt will show:
```json
{
  "warnings": [
    {
      "stage": "canonicalize",
      "message": "rustfmt failed for src/types.rs",
      "code": "E4001"
    }
  ]
}
```

**Q: Why are hashes important?**
A: Hashes prove determinism. If you run `ggen sync` twice with identical inputs and get different hashes, it's a bug in ggen (please report!).

**Q: Can I skip formatting?**
A: Not recommended. Use `--skip_format` flag only for debugging:
```bash
ggen sync --skip_format true  # Raw template output, no formatting
```

---

### Stage μ₅: Receipt (Certify & Document)

**Purpose**: Generate cryptographic proof of generation process with full audit trail.

**What happens:**
1. Create execution ID (UUID v4) and timestamp (ISO 8601)
2. Compute manifest hash (SHA-256 of `ggen.toml`)
3. Compute ontology hash (SHA-256 of all `.ttl` files)
4. Record file-by-file content hashes
5. Log inference rules executed and timings
6. Log generation rules executed and timings
7. Write deterministic receipt (JSON)
8. Write audit trail (JSON with full provenance)

**Example receipt:**

```json
{
  "execution_id": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
  "timestamp": "2026-01-24T20:30:45.123Z",
  "manifest": {
    "path": "ggen.toml",
    "hash": "b2c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3"
  },
  "ontology": {
    "files": ["auth.ttl"],
    "combined_hash": "c3d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4"
  },
  "generated_files": [
    {
      "path": "src/types.rs",
      "hash": "a3f8b2c1d4e5f6a7b8c9d0e1f2a3b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1",
      "size_bytes": 1024,
      "template": "types.rs.tera"
    },
    {
      "path": "src/auth.rs",
      "hash": "b4c5d6e7f8a9b0c1d2e3f4a5b6c7d8e9f0a1b2c3d4e5f6a7b8c9d0e1f2a3b4c5",
      "size_bytes": 2048,
      "template": "auth.rs.tera"
    }
  ],
  "inference_rules": [
    {
      "rule": "rdfs:domain inference",
      "executions": 12,
      "timing_us": 245
    }
  ],
  "generation_rules": [
    {
      "rule": "class_to_struct",
      "executions": 2,
      "timing_us": 1023
    }
  ],
  "total_time_ms": 127,
  "pipeline_stages": {
    "normalize": { "time_ms": 23, "status": "success" },
    "extract": { "time_ms": 45, "status": "success" },
    "emit": { "time_ms": 32, "status": "success" },
    "canonicalize": { "time_ms": 18, "status": "success" },
    "receipt": { "time_ms": 9, "status": "success" }
  }
}
```

**Common Questions:**

**Q: Where are receipts stored?**
A: `.ggen/receipts/latest.json` (symlink to most recent) and `.ggen/receipts/YYYY-MM-DD_HH-MM-SS.json` (timestamped archive).

**Q: What's the difference between receipt and audit trail?**
A:
- **Receipt**: Compact proof of generation (what was generated, hashes, timings)
- **Audit trail**: Detailed log with full provenance (every SPARQL query, every template render, every error)

**Q: Can I use receipts for compliance?**
A: Yes! Receipts provide cryptographic proof of:
1. What ontology version was used (hash)
2. What code was generated (hashes)
3. When generation occurred (timestamp)
4. How long each stage took (timings)

This is useful for:
- Regulatory compliance (FDA, SOX, GDPR)
- Security audits (prove code provenance)
- Reproducibility requirements (scientific computing)

---

## Determinism & Reproducibility

**The Determinism Guarantee:**

```
Same Ontology + Same Templates = Identical Output (Always)
```

**How ggen achieves this:**

1. **Normalized RDF graph** (μ₁)
   - Triple order is deterministic (sorted by subject, predicate, object)
   - Inference always produces same triples (OWL2-RL semantics)

2. **Deterministic SPARQL** (μ₂)
   - Query results sorted by default
   - Binding order is stable

3. **Template rendering** (μ₃)
   - Tera templates are deterministic (no random functions, no timestamps)
   - Context injection is sorted

4. **Canonical formatting** (μ₄)
   - Language formatters produce consistent output
   - Line endings normalized (LF)
   - File ordering alphabetical

5. **Content hashing** (μ₅)
   - SHA-256 ensures byte-for-byte comparison
   - Receipt captures full provenance

**Testing determinism:**

```bash
# Generate once
ggen sync --audit true
cp .ggen/receipts/latest.json receipt1.json

# Generate again (clean slate)
rm -rf src/
ggen sync --audit true
cp .ggen/receipts/latest.json receipt2.json

# Compare receipts (should be identical except execution_id and timestamp)
diff <(jq 'del(.execution_id, .timestamp)' receipt1.json) \
     <(jq 'del(.execution_id, .timestamp)' receipt2.json)

# Should output: (empty - files are identical)
```

---

## Debugging Guide: Which Stage to Check

**Problem: Generated code has syntax errors**

Check: **Stage μ₃ (Emit)**
```bash
# Inspect template rendering
ggen sync --dry_run true --verbose

# Look for template errors:
grep -A5 "Template error" .ggen/audit/latest.json
```

Fix:
- Verify template syntax (`.tera` file)
- Check template context (SPARQL results)
- Test template in isolation

---

**Problem: SHACL validation fails**

Check: **Stage μ₁ (Normalize)**
```bash
# Run validation only
ggen sync --validate_only true

# Inspect detailed errors
cat .ggen/audit/latest.json | jq '.stages.normalize.errors'
```

Fix:
- Correct SHACL constraint in `.ttl` file
- Ensure data conforms to shape
- Check SPARQL patterns in SHACL

---

**Problem: Missing properties in generated struct**

Check: **Stage μ₂ (Extract)**
```bash
# Inspect SPARQL results
ggen sync --dry_run true --verbose | grep -A20 "SPARQL results"
```

Fix:
- Check SPARQL query in template or manifest
- Ensure property exists in ontology (μ₁ validation)
- Verify property domain/range matches class

---

**Problem: Inconsistent formatting between runs**

Check: **Stage μ₄ (Canonicalize)**
```bash
# Verify formatter is installed
which rustfmt   # or prettier, black, etc.

# Check receipt for formatter warnings
cat .ggen/receipts/latest.json | jq '.warnings[] | select(.stage == "canonicalize")'
```

Fix:
- Install missing formatter
- Check formatter configuration (`.rustfmt.toml`, `.prettierrc`)
- Use `--skip_format` to see raw template output

---

**Problem: Different hashes for same ontology**

Check: **Stage μ₅ (Receipt)**
```bash
# Compare receipts
diff <(jq 'del(.execution_id, .timestamp)' receipt1.json) \
     <(jq 'del(.execution_id, .timestamp)' receipt2.json)
```

Fix:
- Ensure ontology files are identical (check line endings)
- Verify template files are identical
- Check for non-deterministic template logic (random, timestamps)

---

**Problem: Generation too slow**

Check: **All stages**
```bash
# Profile each stage
cat .ggen/receipts/latest.json | jq '.pipeline_stages'
```

Fix:
- If μ₁ slow: Large ontology? Optimize SHACL constraints
- If μ₂ slow: Complex SPARQL? Add indexes, simplify queries
- If μ₃ slow: Heavy templates? Break into smaller chunks
- If μ₄ slow: Large files? Parallelize formatting

---

## Real-World Example: Complete Flow

Let's trace a change through all five stages using `auth.ttl`.

**Scenario**: Add a `roles` field to `User` class

### Step 1: Edit ontology (source of truth)

```turtle
# Add to auth.ttl:
:hasRoles a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    rdfs:label "roles" ;
    :rustType "Vec<String>" ;
    :required false ;
    :default "vec![]" .
```

### Step 2: Run `ggen sync`

```bash
ggen sync --audit true
```

### Step 3: Watch the pipeline

**μ₁ (Normalize):**
```
[μ₁] Loading auth.ttl... ✓
[μ₁] Parsing RDF triples... ✓ (1 file, 156 triples)
[μ₁] Validating SHACL constraints... ✓
[μ₁] Resolving dependencies... ✓
[μ₁] Executing OWL inference... ✓ (12 rules, 245μs)
[μ₁] Building normalized graph... ✓
[μ₁] Stage complete (23ms)
```

**μ₂ (Extract):**
```
[μ₂] Executing SPARQL queries... ✓
[μ₂] Query: SELECT ?class ?label ?rustType WHERE { ?class a rdfs:Class }
[μ₂] Results: 3 classes (User, Session, AuthService)
[μ₂] Query: SELECT ?prop ?domain ?label ?rustType WHERE { ?prop rdfs:domain ?domain }
[μ₂] Results: 9 properties (including new :hasRoles)
[μ₂] Building template context... ✓
[μ₂] Stage complete (45ms)
```

**μ₃ (Emit):**
```
[μ₃] Loading templates... ✓ (3 templates)
[μ₃] Rendering types.rs.tera... ✓
[μ₃] Rendering auth.rs.tera... ✓
[μ₃] Rendering tests.rs.tera... ✓
[μ₃] Generated 3 files
[μ₃] Stage complete (32ms)
```

**μ₄ (Canonicalize):**
```
[μ₄] Formatting src/types.rs... ✓ (rustfmt)
[μ₄] Formatting src/auth.rs... ✓ (rustfmt)
[μ₄] Formatting src/tests.rs... ✓ (rustfmt)
[μ₄] Computing content hashes... ✓
[μ₄] Stage complete (18ms)
```

**μ₅ (Receipt):**
```
[μ₅] Generating execution ID... ✓ (f47ac10b-58cc-4372-a567-0e02b2c3d479)
[μ₅] Computing manifest hash... ✓
[μ₅] Computing ontology hash... ✓
[μ₅] Recording file hashes... ✓ (3 files)
[μ₅] Writing receipt... ✓ (.ggen/receipts/2026-01-24_20-30-45.json)
[μ₅] Writing audit trail... ✓ (.ggen/audit/2026-01-24.json)
[μ₅] Stage complete (9ms)

Total: 127ms
```

### Step 4: Verify generated code

```rust
// src/types.rs (automatically updated)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct User {
    pub id: String,
    pub email: String,
    pub password_hash: String,
    pub created_at: i64,
    pub updated_at: i64,
    pub roles: Vec<String>,  // ← NEW FIELD
}
```

### Step 5: Check receipt

```json
{
  "execution_id": "f47ac10b-58cc-4372-a567-0e02b2c3d479",
  "timestamp": "2026-01-24T20:30:45.123Z",
  "generated_files": [
    {
      "path": "src/types.rs",
      "hash": "d5e6f7a8b9c0d1e2f3a4b5c6d7e8f9a0b1c2d3e4f5a6b7c8d9e0f1a2b3c4d5e6",  // ← NEW HASH
      "changes": ["Added field: roles"]
    }
  ]
}
```

### Step 6: Run tests

```bash
cargo make test

# All tests pass because:
# 1. Template also updated tests.rs with new field
# 2. Default value (vec![]) prevents breaking existing tests
```

---

## Common Questions & Answers

**Q: What if I need to customize generated code?**

A: **Never edit generated files directly**. Instead:

1. **Extend via traits** (Rust-specific):
```rust
// src/user_extensions.rs (manually maintained)
use crate::types::User;

impl User {
    pub fn is_admin(&self) -> bool {
        self.roles.contains(&"admin".to_string())
    }
}
```

2. **Use partial templates**:
```tera
{# types.rs.tera #}
// AUTO-GENERATED: Do not edit
{{ autogen_section }}

// MANUAL SECTION: Safe to edit
{{ manual_section }}
```

3. **Override via manifest**:
```toml
[templates.types]
skip_if_exists = true  # Don't overwrite manual changes
```

**Q: How do I add custom business logic?**

A: Encode it in the ontology:

```turtle
# Add to auth.ttl:
:ValidateRolesOperation a :Operation ;
    rdfs:label "validate_roles" ;
    rdfs:comment "Check if user has required role" ;
    :input ( :user :required_role ) ;
    :output xsd:boolean ;
    :implementation """
        pub fn validate_roles(&self, required: &str) -> bool {
            self.roles.iter().any(|r| r == required)
        }
    """ .
```

Template extracts `:implementation` and injects it verbatim.

**Q: Can I use ggen for languages other than Rust?**

A: Absolutely! ggen is language-agnostic. Examples:

- **TypeScript**: Use `types.ts.tera` template
- **Python**: Use `types.py.tera` template
- **Go**: Use `types.go.tera` template
- **SQL**: Use `schema.sql.tera` template

The five-stage pipeline works the same regardless of target language.

**Q: What's the performance cost of regeneration?**

A: Minimal for typical projects:

```
Ontology size    | Generation time
-----------------|-----------------
< 1,000 triples  | < 1 second
1,000-10,000     | 1-5 seconds
10,000-100,000   | 5-30 seconds
> 100,000        | 30+ seconds (consider splitting ontology)
```

**Optimization tips:**
- Use `--watch` for live regeneration during development
- Cache normalized graph (μ₁ output) between runs
- Parallelize template rendering (μ₃)

**Q: How do I version control generated files?**

A: Two approaches:

1. **Commit generated files** (recommended for libraries):
   - Users can use library without ggen installed
   - Git shows exact changes in generated code
   - CI verifies generated code matches ontology

2. **Ignore generated files** (recommended for applications):
   - Cleaner git history (only ontology changes)
   - Forces regeneration on every build
   - Requires ggen in build toolchain

Use `.ggenignore` to specify which files to track:
```
# .ggenignore
src/types.rs    # Auto-generated, commit to git
src/tests.rs    # Auto-generated, ignore in git
```

---

## Next Steps

Now that you understand the five-stage pipeline, explore:

1. **Template Design**: Learn how to write effective Tera templates
2. **SPARQL Patterns**: Master queries for extracting domain knowledge
3. **SHACL Validation**: Encode business rules as constraints
4. **Receipt Analysis**: Use receipts for compliance and debugging
5. **Performance Tuning**: Optimize each stage for large ontologies

**Remember the holographic factory principle:**

> "Fix the film (ontology), not the projection (code). The projections are emergent and disposable."

---

**Further Reading:**
- [RDF-First Development Guide](../guides/rdf-first-development.md)
- [Template Writing Best Practices](../guides/template-best-practices.md)
- [SPARQL Query Optimization](../guides/sparql-optimization.md)
- [Deterministic Generation Testing](../guides/determinism-testing.md)
