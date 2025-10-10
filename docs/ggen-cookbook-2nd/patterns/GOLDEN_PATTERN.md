# GOLDEN PATTERN: DELTA-DRIVEN PROJECTION ***

## Context

This is the **synthesis pattern** that unifies all foundational GGen patterns into a coherent system for semantic code generation. Building upon:

- **Pattern 001: KNOWLEDGE-FIRST PROJECTION** - Semantic graphs as single source of truth
- **Pattern 002: DETERMINISTIC ENGINE** - Identical inputs produce identical outputs
- **Pattern 004: NOUN-VERB CLI** - Entity-first command structure

We now address the ultimate challenge: **How do you regenerate code without overwriting manual changes, breaking builds, or creating merge conflicts?**

Traditional code generators fail at iterative development because they either:
1. **Overwrite everything** (destroying manual edits)
2. **Never regenerate** (accumulating drift from the model)
3. **Require complex merges** (manual conflict resolution)

GGen solves this with **delta-driven projection**: track what changed in the *graph*, regenerate only affected artifacts, and preserve manual modifications.

## Problem

**How do you safely regenerate code as the semantic model evolves without destroying developer customizations or creating fragile build pipelines?**

Traditional approaches fail because:
- Full regeneration overwrites hand-written code
- Partial regeneration misses transitive dependencies
- Diffing generated code shows changes, not *causes* of changes
- Merge tools can't distinguish generated vs. manual sections
- Developers fear regeneration and avoid model updates

## Forces

- Developers customize generated code (add logging, error handling, tests)
- Domain models evolve continuously (new entities, relationships, properties)
- Templates improve over time (better patterns, new language features)
- Some files are 100% generated, others are hybrid (generated scaffolds + manual logic)
- Build systems need to know when regeneration is required
- CI/CD pipelines must detect drift between model and code

## Solution

**Therefore, track deltas at the semantic level and project only what changed.**

The delta-driven projection workflow:

1. **Baseline Snapshot**: Store hash of graph state after generation
2. **Detect Graph Deltas**: Compare current graph to baseline snapshot
3. **Query Affected Artifacts**: SPARQL queries identify impacted templates
4. **Project Changes Only**: Regenerate only files affected by graph deltas
5. **Three-Way Merge**: Preserve manual edits in hybrid files
6. **Update Snapshot**: Record new graph hash for next iteration

```
┌─────────────────────────────────────────────────────────────────┐
│                   DELTA-DRIVEN PROJECTION FLOW                   │
└─────────────────────────────────────────────────────────────────┘

    Time T0 (Initial)         Time T1 (Model Change)      Time T2
    ═════════════════         ══════════════════════      ═══════

    user.ttl                  user.ttl                    user.ttl
    (hash: abc123)            (hash: def456) ← CHANGED!  (hash: def456)
         │                         │                           │
         │ generate                │ diff detection            │
         ▼                         ▼                           ▼
    user.rs                   [DELTA ANALYSIS]            user.rs
    (generated)                    │                      (regenerated)
         │                         ├─ :User → :email     ✓ preserved
         │                         │  (new property)      ✓ updated
         │                         │                           │
    + manual edits                 ▼                      + manual edits
    (custom validate())       Regenerate ONLY             (still present!)
         │                    affected sections
         │                         │
    SNAPSHOT:                      ▼                      SNAPSHOT:
    graph=abc123              Three-way merge:            graph=def456
    file=xyz789               - baseline (T0)             file=uvw012
                               - generated (T1)
                               - manual (T0→T1)
                                    │
                                    ▼
                               user.rs (merged)
                               ✓ new :email field
                               ✓ custom validate()
                               ✓ NO conflicts

    KEY INSIGHT:
    ═══════════
    We diff the GRAPH (semantic), not the CODE (syntax).
    Changes flow from meaning → implementation.
```

## Result

You achieve:

- **Safe Regeneration**: Never lose manual customizations
- **Incremental Updates**: Only regenerate what changed semantically
- **Zero Merge Conflicts**: Three-way merge handles hybrid files automatically
- **CI/CD Integration**: Detect model-code drift in pipelines
- **Fearless Evolution**: Developers can update models confidently
- **Audit Trail**: Complete history of what changed and why

This pattern **synthesizes** all previous patterns:
- Uses **001: KNOWLEDGE-FIRST** to define semantic deltas
- Uses **002: DETERMINISTIC ENGINE** to detect real changes (not spurious diffs)
- Uses **004: NOUN-VERB CLI** via `ggen graph diff` and `ggen template regenerate`

## Graph Example

**File**: `docs/examples/user_v1.ttl` (Baseline)

```turtle
@prefix : <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:User a rdfs:Class ;
    rdfs:label "User" .

:name a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string .
```

**File**: `docs/examples/user_v2.ttl` (Updated Model)

```turtle
@prefix : <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

:User a rdfs:Class ;
    rdfs:label "User" .

:name a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string .

# NEW: Email property added
:email a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:string .

# NEW: Age property added
:age a rdf:Property ;
    rdfs:domain :User ;
    rdfs:range xsd:integer .
```

## Template Example

**File**: `templates/user_struct.tmpl`

```handlebars
---
query: |
  PREFIX : <http://example.org/>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  SELECT ?propName ?propType
  WHERE {
    :User a rdfs:Class .
    ?prop rdfs:domain :User ;
          rdfs:label ?propName ;
          rdfs:range ?propType .
  }
  ORDER BY ?propName
---
// GENERATED: DO NOT EDIT THIS SECTION
// Generator: user_struct.tmpl
// Graph: user.ttl
// Timestamp: {{generation_timestamp}}

pub struct User {
{{#each results}}
    pub {{propName}}: {{map_type propType}},
{{/each}}
}

// END GENERATED SECTION

// MANUAL: Safe to edit below this line
impl User {
    pub fn validate(&self) -> Result<(), String> {
        // Custom validation logic added by developer
        if self.name.is_empty() {
            return Err("Name cannot be empty".into());
        }
        Ok(())
    }
}
```

## CLI Invocation

```bash
# ========================================
# STEP 1: Initial generation (T0)
# ========================================

# Load baseline graph
ggen graph load docs/examples/user_v1.ttl --name user

# Generate initial code
ggen template apply templates/user_struct.tmpl > src/user.rs

# Create snapshot (stores graph hash + file hash)
ggen snapshot create --graph user --file src/user.rs --name user_baseline

# Developer adds manual validation logic to src/user.rs...

# ========================================
# STEP 2: Model evolution (T0 → T1)
# ========================================

# Update graph to v2 (adds :email and :age properties)
ggen graph load docs/examples/user_v2.ttl --name user --replace

# Detect semantic deltas
ggen graph diff --baseline user_baseline --current user

# Output shows:
# ADDED: :email (rdf:Property, domain=:User, range=xsd:string)
# ADDED: :age (rdf:Property, domain=:User, range=xsd:integer)

# ========================================
# STEP 3: Delta-driven regeneration
# ========================================

# Query which templates are affected by deltas
ggen template affected --graph-delta user_baseline..user

# Output shows:
# templates/user_struct.tmpl (queries :User properties)

# Regenerate with three-way merge
ggen template regenerate templates/user_struct.tmpl \
  --baseline user_baseline \
  --output src/user.rs \
  --merge auto

# Three-way merge:
# - Base: Original generated code (T0)
# - Theirs: New generated code (T1)
# - Mine: Manual edits (validate() function)
# - Result: Merged code preserving manual edits

# Update snapshot
ggen snapshot update user_baseline --graph user --file src/user.rs

# ========================================
# STEP 4: Verify results
# ========================================

# Show merge summary
ggen snapshot diff user_baseline

# Compile to ensure no breakage
cargo check
```

## Expected Output

**File**: `src/user.rs` (After delta-driven regeneration)

```rust
// GENERATED: DO NOT EDIT THIS SECTION
// Generator: user_struct.tmpl
// Graph: user.ttl
// Timestamp: 2025-10-09T12:34:56Z

pub struct User {
    pub age: i32,        // ← NEW (from delta)
    pub email: String,   // ← NEW (from delta)
    pub name: String,    // ← EXISTING
}

// END GENERATED SECTION

// MANUAL: Safe to edit below this line
impl User {
    pub fn validate(&self) -> Result<(), String> {
        // Custom validation logic added by developer
        // ✓ PRESERVED during regeneration!
        if self.name.is_empty() {
            return Err("Name cannot be empty".into());
        }
        Ok(())
    }
}
```

**Key outcomes**:
- ✅ New fields `email` and `age` added automatically
- ✅ Manual `validate()` function preserved
- ✅ No merge conflicts
- ✅ Code still compiles

## Verification

```bash
# Test 1: Detect graph deltas
ggen graph diff docs/examples/user_v1.ttl docs/examples/user_v2.ttl

# Expected output:
# + :email a rdf:Property ; rdfs:domain :User ; rdfs:range xsd:string .
# + :age a rdf:Property ; rdfs:domain :User ; rdfs:range xsd:integer .

# Test 2: Verify deterministic delta detection
hash1=$(ggen graph diff v1.ttl v2.ttl --format hash)
hash2=$(ggen graph diff v1.ttl v2.ttl --format hash)
test "$hash1" == "$hash2" && echo "✓ Deterministic delta detection"

# Test 3: Verify manual edits preserved
grep -q "validate(&self)" src/user.rs && echo "✓ Manual code preserved"

# Test 4: Verify new fields added
grep -q "pub email: String" src/user.rs && echo "✓ New field added"
grep -q "pub age: i32" src/user.rs && echo "✓ New field added"

# Test 5: Snapshot integrity
ggen snapshot verify user_baseline
# Expected: ✓ Graph hash matches, ✓ File hash matches

# Test 6: CI/CD drift detection
ggen graph diff --baseline user_baseline --current user --exit-code
# Returns 0 if no drift, 1 if drift detected (for CI pipelines)
```

## Delta Detection Algorithm

### 1. **Graph-Level Deltas**

Compute RDF graph diff using set operations:

```sparql
# Additions (in v2, not in v1)
PREFIX : <http://example.org/>

SELECT ?s ?p ?o
WHERE {
  GRAPH <user_v2> { ?s ?p ?o }
  FILTER NOT EXISTS {
    GRAPH <user_v1> { ?s ?p ?o }
  }
}
```

```sparql
# Deletions (in v1, not in v2)
PREFIX : <http://example.org/>

SELECT ?s ?p ?o
WHERE {
  GRAPH <user_v1> { ?s ?p ?o }
  FILTER NOT EXISTS {
    GRAPH <user_v2> { ?s ?p ?o }
  }
}
```

### 2. **Template Impact Analysis**

For each delta, find affected templates:

```sparql
# Which templates query :User properties?
PREFIX : <http://example.org/>

SELECT ?template ?queryText
WHERE {
  ?template a :Template ;
            :hasQuery ?queryText .
  FILTER(CONTAINS(?queryText, ":User"))
  FILTER(CONTAINS(?queryText, "rdfs:domain"))
}
```

### 3. **Three-Way Merge Strategy**

```
Base (T0):          Theirs (T1):       Mine (T0→T1):        Result:
─────────           ────────────       ─────────────        ───────
struct User {       struct User {      struct User {        struct User {
  name: String        age: i32           name: String         age: i32
}                     email: String      name: String         email: String
                      name: String     }                      name: String
                    }                                        }
                                       impl User {
                                         validate() {}      impl User {
                                       }                      validate() {}
                                                            }

ALGORITHM:
1. Detect generated vs manual regions (via markers)
2. Apply structural diff to generated region only
3. Preserve manual region unchanged
4. Reorder fields alphabetically (deterministic)
```

## Snapshot Format

**File**: `.ggen/snapshots/user_baseline.json`

```json
{
  "name": "user_baseline",
  "created_at": "2025-10-09T12:00:00Z",
  "graph": {
    "name": "user",
    "path": "docs/examples/user_v1.ttl",
    "hash": "sha256:abc123...",
    "triples": 6
  },
  "files": [
    {
      "path": "src/user.rs",
      "hash": "sha256:xyz789...",
      "generated_regions": [
        {"start": 1, "end": 10, "marker": "GENERATED"}
      ],
      "manual_regions": [
        {"start": 12, "end": 20, "marker": "MANUAL"}
      ]
    }
  ],
  "templates": [
    {
      "path": "templates/user_struct.tmpl",
      "hash": "sha256:def456...",
      "query_hash": "sha256:query123..."
    }
  ]
}
```

## Anti-Patterns

❌ **Overwrite Everything**: Regenerating without delta detection loses manual edits
❌ **Never Regenerate**: Avoiding regeneration leads to drift and stale code
❌ **Manual Merge Hell**: Requiring developers to resolve conflicts manually
❌ **Syntax-Level Diffs**: Diffing generated code instead of semantic graphs
❌ **No Snapshot Tracking**: Can't detect what changed without baselines
❌ **Ignore Template Changes**: Template updates should also trigger regeneration

## Advanced Features

### 1. **Selective Regeneration**

```bash
# Regenerate only files affected by :User changes
ggen template regenerate --filter "domain=:User"

# Regenerate only Rust files
ggen template regenerate --filter "language=rust"

# Dry-run to preview changes
ggen template regenerate --dry-run --show-diff
```

### 2. **Conflict Resolution**

```bash
# Interactive merge for conflicts
ggen template regenerate --merge interactive

# Auto-accept generated changes
ggen template regenerate --merge theirs

# Auto-accept manual changes
ggen template regenerate --merge mine
```

### 3. **CI/CD Integration**

```yaml
# .github/workflows/model-drift.yml
name: Detect Model Drift

on: [pull_request]

jobs:
  check-drift:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Check for semantic drift
        run: |
          ggen snapshot verify production_baseline
          if [ $? -ne 0 ]; then
            echo "ERROR: Generated code is out of sync with model"
            echo "Run: ggen template regenerate --all"
            exit 1
          fi
```

### 4. **Audit Trail**

```bash
# Show history of regenerations
ggen snapshot log user_baseline

# Output:
# 2025-10-09 12:00:00 - Created (graph: abc123, file: xyz789)
# 2025-10-09 14:30:00 - Updated (graph: def456, file: uvw012)
#   + Added: :email property
#   + Added: :age property
# 2025-10-09 16:45:00 - Updated (graph: def456, file: rst345)
#   ~ Modified: validate() function (manual edit)
```

## Related Patterns

- **Pattern 001: KNOWLEDGE-FIRST PROJECTION** - Provides semantic deltas
- **Pattern 002: DETERMINISTIC ENGINE** - Ensures diff-able outputs
- **Pattern 004: NOUN-VERB CLI** - `ggen graph diff`, `ggen template regenerate`
- **Pattern 006: LOCKFILE VERSIONING** - Tracks dependency hashes
- **Pattern 007: SNAPSHOT TESTING** - Baseline for delta detection

---

**This is the GOLDEN PATTERN because it makes GGen usable in real-world iterative development. Without delta-driven projection, code generation is a one-shot tool. With it, generation becomes an ongoing workflow that evolves with your domain model.**