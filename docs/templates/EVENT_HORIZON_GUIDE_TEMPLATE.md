# Crossing the Event Horizon: The RDF-First Paradigm Shift

> **Event Horizon**: The boundary beyond which traditional code-first thinking no longer applies. Beyond this point, ontology becomes reality, and code becomes a projection.

## Executive Summary

**What is the Event Horizon?**

In physics, an event horizon is the boundary around a black hole beyond which nothing can return. In software development, crossing the event horizon means fully committing to RDF-first, ontology-driven development. Traditional approaches stop working. There is no going back to code-first thinking.

**The Fundamental Equation**:
```
A = μ(O)
```
Where:
- **A** = Artifacts (code, docs, configs, tests)
- **μ** = Deterministic transformation pipeline (5-stage)
- **O** = Ontology (RDF/Turtle specifications)

**Key Insight**: Code doesn't define truth. Ontology defines truth. Code is merely a projection.

**Quick Navigation**:
- [Before vs After](#before-vs-after-the-event-horizon)
- [Key Concepts](#key-concepts)
- [Mental Model Shifts](#mental-model-shifts)
- [The Transformation Journey](#transformation-journey)
- [Visual Guides](#visual-guides)
- [Decision Framework](#decision-framework)
- [Success Metrics](#success-metrics)

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Before vs After the Event Horizon](#before-vs-after-the-event-horizon)
3. [Key Concepts](#key-concepts)
   - [The Holographic Factory](#the-holographic-factory)
   - [Five-Stage Pipeline (μ)](#five-stage-pipeline-μ)
   - [Deterministic Receipts](#deterministic-receipts)
   - [RDF as Source of Truth](#rdf-as-source-of-truth)
4. [Mental Model Shifts](#mental-model-shifts)
   - [From Files to Ontologies](#from-files-to-ontologies)
   - [From Code to Projections](#from-code-to-projections)
   - [From Tests to Evidence](#from-tests-to-evidence)
   - [From Bugs to Interference Patterns](#from-bugs-to-interference-patterns)
5. [The Transformation Journey](#transformation-journey)
   - [Recognition Phase](#phase-1-recognition)
   - [Resistance Phase](#phase-2-resistance)
   - [Experimentation Phase](#phase-3-experimentation)
   - [Commitment Phase](#phase-4-commitment)
   - [Mastery Phase](#phase-5-mastery)
6. [Visual Guides](#visual-guides)
7. [Decision Framework](#decision-framework)
8. [Success Metrics](#success-metrics)
9. [Common Pitfalls](#common-pitfalls)
10. [Further Resources](#further-resources)

---

## Before vs After the Event Horizon

### Traditional Code-First Approach (Before)

```
┌─────────────────────────────────────────────────────┐
│ TRADITIONAL WORKFLOW                                 │
├─────────────────────────────────────────────────────┤
│                                                      │
│  Requirements (Word/Markdown)                       │
│         ↓ (manual interpretation)                   │
│  Code (Python/Rust/TypeScript)                      │
│         ↓ (manual writing)                          │
│  Tests (separate from requirements)                 │
│         ↓ (manual documentation)                    │
│  Documentation (drifts from code)                   │
│                                                      │
│  PROBLEMS:                                           │
│  ❌ Specs drift from code                           │
│  ❌ Docs drift from implementation                  │
│  ❌ Tests don't trace to requirements               │
│  ❌ No single source of truth                       │
│  ❌ Manual consistency checking                     │
│  ❌ Non-deterministic outputs                       │
│  ❌ Tribal knowledge required                       │
│                                                      │
└─────────────────────────────────────────────────────┘
```

### RDF-First Approach (After)

```
┌─────────────────────────────────────────────────────┐
│ RDF-FIRST WORKFLOW (A = μ(O))                       │
├─────────────────────────────────────────────────────┤
│                                                      │
│  RDF Ontology (.ttl) - SINGLE SOURCE OF TRUTH       │
│         ↓                                           │
│  μ₁: Normalize (SHACL validation)                  │
│         ↓                                           │
│  μ₂: Extract (SPARQL queries)                      │
│         ↓                                           │
│  μ₃: Emit (Tera templates → code/docs)            │
│         ↓                                           │
│  μ₄: Canonicalize (rustfmt, prettier)             │
│         ↓                                           │
│  μ₅: Receipt (cryptographic proof)                 │
│         ↓                                           │
│  Artifacts (code, docs, tests, configs)            │
│                                                      │
│  BENEFITS:                                           │
│  ✅ Single source of truth (ontology)              │
│  ✅ Specs = code = docs (always synchronized)      │
│  ✅ Deterministic outputs (SHA-256 verified)       │
│  ✅ Machine-readable requirements                   │
│  ✅ Automated consistency checking                  │
│  ✅ Cryptographic audit trail                      │
│  ✅ Zero drift by construction                     │
│                                                      │
└─────────────────────────────────────────────────────┘
```

### Side-by-Side Comparison

| Aspect | Code-First (Before) | RDF-First (After) |
|--------|---------------------|-------------------|
| **Source of Truth** | Code files | RDF ontology (.ttl) |
| **Documentation** | Manual, drifts | Generated from ontology |
| **Requirements** | Separate Word/MD docs | Embedded in ontology |
| **Tests** | Written independently | Generated from acceptance scenarios |
| **Consistency** | Manual checking | Automatic (SHACL validation) |
| **Traceability** | Tribal knowledge | SPARQL queries |
| **Determinism** | Non-deterministic | Cryptographically verified |
| **Change Management** | Git diff of code | Git diff of ontology |
| **Bug Fixing** | Patch code | Fix ontology, regenerate |
| **Onboarding** | Weeks of code reading | Query ontology with SPARQL |
| **Audit Trail** | Git log (partial) | Cryptographic receipts |

---

## Key Concepts

### The Holographic Factory

**Metaphor**: Think of the RDF ontology as a holographic film. Just as a holographic plate contains interference patterns that can project 3D images, an RDF ontology contains semantic patterns that project code, documentation, tests, and configurations.

**Key Properties**:
1. **Substrate (unrdf)**: The ontology is high-dimensional, encoding all domain knowledge
2. **Projection**: Code is a 2D projection of the high-dimensional ontology
3. **Coherence**: Multiple projections (code, docs, tests) are coherent because they share the same source
4. **Temporal Waypoints**: Git commits are 4D slices of ontology evolution

**Implications**:
- Changing the code doesn't change reality (the ontology)
- To fix a bug, fix the interference pattern (ontology), then regenerate the projection
- Multiple languages/frameworks are just different projection angles

### Five-Stage Pipeline (μ)

The transformation μ is not magic - it's a deterministic, 5-stage pipeline:

```
μ = μ₅ ∘ μ₄ ∘ μ₃ ∘ μ₂ ∘ μ₁
```

#### Stage 1: Normalize (μ₁)
**Purpose**: Validate and normalize the ontology

**Inputs**: Raw RDF ontology (.ttl, .rdf, .nt files)

**Operations**:
- Parse RDF syntax (Turtle, RDF/XML, N-Triples)
- SHACL shape validation (schema conformance)
- Dependency resolution (imports, external ontologies)
- OWL inference (materialize implicit triples)

**Outputs**: Validated, normalized RDF graph

**Quality Gates** (Poka-Yoke):
1. Manifest schema validation
2. Ontology dependency resolution
3. SPARQL query syntax validation
4. Template syntax validation
5. File permission checks
6. Rule validation

**Example**:
```bash
ggen sync --validate_only true
# Output: ✓ 6/6 quality gates passed
```

#### Stage 2: Extract (μ₂)
**Purpose**: Query the ontology to extract structured data

**Inputs**: Validated RDF graph

**Operations**:
- Execute SPARQL SELECT queries
- Execute SPARQL CONSTRUCT queries
- Apply inference rules (RDFS, OWL2-RL)
- Build template context (JSON/YAML)

**Outputs**: Structured data bindings for templates

**Example**:
```sparql
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?storyIndex ?title ?priority ?description
WHERE {
    ?story a sk:UserStory ;
           sk:storyIndex ?storyIndex ;
           sk:title ?title ;
           sk:priority ?priority ;
           sk:description ?description .
}
ORDER BY ?storyIndex
```

#### Stage 3: Emit (μ₃)
**Purpose**: Render templates to generate artifacts

**Inputs**: Template bindings from μ₂

**Operations**:
- Tera template rendering (SPARQL-aware)
- Multi-file generation (code, docs, configs)
- Directory structure creation

**Outputs**: Raw generated artifacts

**Example**:
```rust
// Generated from ontology via μ₃
pub struct UserStory {
    pub story_index: u32,
    pub title: String,
    pub priority: Priority,
    pub description: String,
}
```

#### Stage 4: Canonicalize (μ₄)
**Purpose**: Apply deterministic formatting

**Inputs**: Raw generated artifacts

**Operations**:
- Language-specific formatting (rustfmt, prettier, black)
- Syntax validation (compiler checks)
- Content hashing (SHA-256 per file)

**Outputs**: Canonicalized, formatted artifacts

**Determinism Guarantee**: Same ontology + same templates = identical SHA-256 hashes

#### Stage 5: Receipt (μ₅)
**Purpose**: Generate cryptographic proof of generation

**Inputs**: Canonicalized artifacts

**Operations**:
- Execution ID generation (UUID + timestamp)
- Manifest hashing (SHA-256)
- Ontology hashing (SHA-256)
- Per-file content hashing
- Timing collection (μs precision)
- Audit trail logging

**Outputs**: Deterministic receipt (JSON), audit log (JSON)

**Example Receipt**:
```json
{
  "execution_id": "20260124T120530Z-a3f8c9d1",
  "timestamp": "2026-01-24T12:05:30.123456Z",
  "manifest_hash": "sha256:abc123...",
  "ontology_hash": "sha256:def456...",
  "files_generated": [
    {
      "path": "src/domain/user_story.rs",
      "hash": "sha256:789xyz...",
      "size_bytes": 1024
    }
  ],
  "inference_rules_executed": ["rdfs:subClassOf", "owl:inverseOf"],
  "generation_rules_executed": ["user-story-struct", "acceptance-test"],
  "timings": {
    "normalize": "342μs",
    "extract": "128μs",
    "emit": "456μs",
    "canonicalize": "234μs",
    "receipt": "89μs",
    "total": "1249μs"
  }
}
```

### Deterministic Receipts

**Purpose**: Cryptographic proof that artifacts were generated from a specific ontology

**Components**:
1. Execution ID (timestamp + UUID)
2. Manifest hash (SHA-256 of ggen.toml)
3. Ontology hash (SHA-256 of all .ttl files)
4. Per-file content hashes (SHA-256)
5. Rules executed (inference + generation)
6. Timings (μs precision)

**Use Cases**:
- **Compliance**: Prove code came from validated requirements
- **Debugging**: Trace artifacts back to specific ontology version
- **Reproducibility**: Verify identical outputs from identical inputs
- **Audit**: Cryptographic chain of custody

**Verification**:
```bash
# Verify receipt against current ontology
ggen verify-receipt .ggen/receipts/20260124T120530Z-a3f8c9d1.json

# Output:
# ✓ Manifest hash matches
# ✓ Ontology hash matches
# ✓ All file hashes match
# ✓ Receipt valid
```

### RDF as Source of Truth

**Core Principle**: The ontology (.ttl files) is the single source of truth. Everything else is a generated artifact.

**Implications**:

1. **NEVER edit generated files directly**
   ```bash
   # ❌ WRONG
   vim src/domain/user_story.rs  # This is generated!

   # ✅ CORRECT
   vim .specify/specs/013-feature/feature.ttl  # Edit source
   ggen sync  # Regenerate
   ```

2. **Specs and code cannot drift**
   - Traditional: Spec says X, code does Y (drift)
   - RDF-first: Code is generated from spec (impossible to drift)

3. **Documentation is always current**
   - Traditional: Docs written once, never updated
   - RDF-first: Docs regenerated on every change

4. **Tests trace to requirements**
   - Traditional: Tests written independently
   - RDF-first: Tests generated from acceptance scenarios in ontology

**Example**:
```turtle
# .specify/specs/013-auth/feature.ttl (SOURCE OF TRUTH)
:us-001 a sk:UserStory ;
    sk:title "User can log in with email and password" ;
    sk:hasAcceptanceScenario :us-001-as-001 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:given "User has valid credentials" ;
    sk:when "User submits login form" ;
    sk:then "System displays dashboard" .
```

This ontology generates:
- **Rust code**: `pub struct LoginRequest { email: String, password: String }`
- **Test**: `#[test] fn test_login_with_valid_credentials()`
- **Docs**: "# User Authentication\n\n## User Story 1: User can log in..."
- **API spec**: OpenAPI definition for POST /auth/login

All synchronized, all traceable, all deterministic.

---

## Mental Model Shifts

### From Files to Ontologies

#### Old Mental Model (Files)
```
project/
├── src/
│   ├── auth.py        # Implementation
│   └── user.py        # More implementation
├── tests/
│   └── test_auth.py   # Tests (separate)
├── docs/
│   └── auth.md        # Documentation (drifts)
└── requirements.txt   # Dependencies
```

**Problems**:
- Files are disconnected entities
- Relationships are implicit (imports)
- No formal semantics
- Tribal knowledge required

#### New Mental Model (Ontologies)
```
.specify/
└── specs/013-auth/
    ├── feature.ttl    # User stories, acceptance scenarios
    ├── entities.ttl   # Domain model (User, Session, Token)
    ├── plan.ttl       # Architecture decisions
    └── tasks.ttl      # Implementation tasks
```

**Benefits**:
- Entities are semantic nodes in a graph
- Relationships are explicit RDF triples
- Machine-readable semantics (SPARQL)
- Self-documenting

**Query Example**:
```sparql
# Find all user stories related to authentication
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?title ?priority
WHERE {
    ?story a sk:UserStory ;
           sk:title ?title ;
           sk:priority ?priority ;
           sk:relatedTo :authentication .
}
```

### From Code to Projections

#### Old Mental Model (Code is Reality)
```
"The code is the truth. If the spec says X but the code does Y,
 the code is what actually happens, so the code is correct."
```

**Problems**:
- Specs drift from reality
- Tribal knowledge accumulates
- Onboarding requires reading code
- No formal verification

#### New Mental Model (Code is Projection)
```
"The ontology is the truth. Code is a projection.
 If the code does Y but the ontology says X,
 fix the ontology and regenerate the code."
```

**Benefits**:
- Ontology never drifts (it's the source)
- Formal semantics enable verification
- Onboarding queries ontology with SPARQL
- Deterministic regeneration

**Example**:
```turtle
# Ontology defines authentication requirements
:us-001 a sk:UserStory ;
    sk:title "User can log in with email and password" ;
    sk:hasAcceptanceScenario :us-001-as-001 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:given "User has valid credentials" ;
    sk:when "User submits login form" ;
    sk:then "System displays dashboard" .
```

μ projects this into:
- **Rust**: Login handler function
- **TypeScript**: React login component
- **Python**: FastAPI endpoint
- **Go**: HTTP handler

All from the same ontology. Same truth, different projections.

### From Tests to Evidence

#### Old Mental Model (Tests)
```rust
// Traditional test (disconnected from requirements)
#[test]
fn test_login() {
    let response = post("/auth/login", json!({"email": "test@example.com"}));
    assert_eq!(response.status, 200);
}
```

**Problems**:
- No traceability to requirements
- Arbitrary test data
- No formal specification
- Can pass even if requirement not met

#### New Mental Model (Evidence)
```turtle
# Ontology defines acceptance scenario
:us-001-as-001 a sk:AcceptanceScenario ;
    sk:given "User has email 'test@example.com' with password 'Secret123'" ;
    sk:when "User submits login form with valid credentials" ;
    sk:then "System returns status 200 and JWT token" .
```

μ generates test:
```rust
// Generated test (traces to :us-001-as-001)
#[test]
fn test_us_001_as_001_login_with_valid_credentials() {
    // Arrange: "User has email 'test@example.com' with password 'Secret123'"
    let user = create_test_user("test@example.com", "Secret123");

    // Act: "User submits login form with valid credentials"
    let response = post("/auth/login", json!({
        "email": "test@example.com",
        "password": "Secret123"
    }));

    // Assert: "System returns status 200 and JWT token"
    assert_eq!(response.status, 200);
    assert!(response.json().get("token").is_some());
}
```

**Benefits**:
- Test traces to `:us-001-as-001` (SPARQL query)
- Test data defined in ontology
- Formal specification (given/when/then)
- Evidence of requirement satisfaction

### From Bugs to Interference Patterns

#### Old Mental Model (Bugs are Code Problems)
```
"There's a bug in auth.py line 42. Let me fix that line."
```

**Process**:
1. Find buggy code
2. Patch code
3. Commit patch
4. Hope it doesn't break something else

**Problems**:
- Treats symptom, not root cause
- Spec still wrong (if spec exists)
- Docs still wrong (if docs exist)
- Tests might not cover edge case

#### New Mental Model (Bugs are Ontology Problems)
```
"There's a bug in the authentication flow. The ontology incorrectly
 models the session lifecycle. Let me fix the ontology."
```

**Process**:
1. Identify incorrect ontology pattern
2. Fix ontology (e.g., add missing acceptance scenario)
3. Regenerate all projections (code, docs, tests)
4. Cryptographic receipt proves fix

**Benefits**:
- Fixes root cause (ontology)
- Code, docs, tests all regenerated consistently
- Receipt traces fix to specific ontology change
- SHACL validation prevents similar bugs

**Example**:
```turtle
# Bug: Ontology didn't specify session timeout behavior

# Fix: Add acceptance scenario
:us-002-as-003 a sk:AcceptanceScenario ;
    sk:scenarioIndex 3 ;
    sk:given "User has been logged in for 60 minutes" ;
    sk:when "User attempts to access protected resource" ;
    sk:then "System returns 401 Unauthorized and redirects to login" .
```

After `ggen sync`:
- **Rust code**: Session timeout logic added
- **Tests**: `test_session_timeout_after_60_minutes()` generated
- **Docs**: Session timeout section added
- **Receipt**: SHA-256 hash proves all artifacts updated

---

## Transformation Journey

### Phase 1: Recognition

**Characteristics**:
- First exposure to RDF-first concepts
- Skepticism ("Why not just write code?")
- Conceptual understanding only
- Still thinking in code-first terms

**Key Activities**:
- Read [EVENT_HORIZON.md](#) (this document)
- Complete [Getting Started Tutorial](../tutorials/01_getting_started.md)
- Review [Case Studies](#case-studies)
- Ask "Why?" questions

**Duration**: 1-2 days

**Success Criteria**:
- Can explain A = μ(O) equation
- Understands 5-stage pipeline conceptually
- Sees benefits but skeptical of practicality

### Phase 2: Resistance

**Characteristics**:
- "This is overkill for my use case"
- "I can just write the code faster"
- "Learning RDF/SPARQL is too much overhead"
- Temptation to edit generated files directly

**Key Activities**:
- Pair with experienced RDF-first developer
- Complete [Hands-On Exercise 1](../exercises/01_first_ontology.md)
- Experience the pain of code drift (traditional approach)
- See deterministic receipts in action

**Duration**: 3-5 days

**Success Criteria**:
- Created first ontology (.ttl file)
- Generated artifacts with `ggen sync`
- Resisted urge to edit generated code
- Beginning to see value

### Phase 3: Experimentation

**Characteristics**:
- Actively using RDF-first for small features
- Still hybrid approach (some code-first, some RDF-first)
- Learning SPARQL and Turtle syntax
- Discovering patterns

**Key Activities**:
- Build 3-5 features using RDF-first
- Write custom SPARQL queries
- Create custom Tera templates
- Compare RDF-first vs code-first velocity

**Duration**: 2-3 weeks

**Success Criteria**:
- Comfortable writing Turtle ontologies
- Can write basic SPARQL queries
- Created at least one custom template
- Prefers RDF-first for new features

### Phase 4: Commitment

**Characteristics**:
- Fully adopted RDF-first for all new work
- Teaching others
- Contributing to ontology schemas
- Thinking in graphs, not files

**Key Activities**:
- Convert existing code to ontologies
- Optimize SPARQL queries
- Build domain-specific ontology extensions
- Integrate with CI/CD (receipts, validation)

**Duration**: 1-2 months

**Success Criteria**:
- All new features are RDF-first
- Trained at least one other developer
- Created custom SHACL shapes
- Integrated `ggen sync` into CI/CD

### Phase 5: Mastery

**Characteristics**:
- RDF-first is default mental model
- Code is an afterthought (just a projection)
- Can debug by querying ontology
- Designing ontology schemas for others

**Key Activities**:
- Design domain-specific ontologies
- Optimize μ pipeline performance
- Contribute to ggen core
- Evangelize RDF-first approach

**Duration**: 3-6 months

**Success Criteria**:
- Can design ontology schemas from scratch
- Debugs by querying ontology, not reading code
- Team velocity 2-3x traditional approach
- Zero spec/code drift incidents

---

## Visual Guides

### The Event Horizon Diagram

```
                    THE EVENT HORIZON
                           │
                           │
    ┌──────────────────────┼──────────────────────┐
    │  BEFORE               │  AFTER               │
    │  (Code-First)         │  (RDF-First)         │
    ├──────────────────────┼──────────────────────┤
    │                       │                      │
    │  Specs (Word/MD)      │  RDF Ontology        │
    │       ↓ manual        │       ↓ μ₁           │
    │  Code (Python/Rust)   │  Normalized Graph    │
    │       ↓ manual        │       ↓ μ₂           │
    │  Tests (separate)     │  SPARQL Queries      │
    │       ↓ manual        │       ↓ μ₃           │
    │  Docs (drifts)        │  Generated Code      │
    │                       │       ↓ μ₄           │
    │  ❌ Non-deterministic │  Formatted Code      │
    │  ❌ Specs drift       │       ↓ μ₅           │
    │  ❌ Docs drift        │  Cryptographic       │
    │  ❌ No traceability   │  Receipt             │
    │  ❌ Tribal knowledge  │                      │
    │                       │  ✅ Deterministic    │
    │                       │  ✅ Zero drift       │
    │                       │  ✅ Full traceability│
    │                       │  ✅ Machine-readable │
    └───────────────────────┴──────────────────────┘
                           │
                    Crossing Point
                (No Turning Back)
```

### The Holographic Projection

```
┌─────────────────────────────────────────────────────┐
│          RDF ONTOLOGY (High-Dimensional)            │
│                                                      │
│  @prefix : <http://example.com/auth#> .             │
│  :LoginFeature a sk:Feature ;                       │
│      sk:hasUserStory :us-001 .                      │
│                                                      │
│  :us-001 a sk:UserStory ;                           │
│      sk:title "User can log in" ;                   │
│      sk:hasAcceptanceScenario :us-001-as-001 .      │
└──────────────┬──────────────────────────────────────┘
               │
               │ μ (Five-Stage Pipeline)
               ├──────────┬──────────┬──────────┬──────────┐
               ↓          ↓          ↓          ↓          ↓
         ┌─────────┐┌─────────┐┌─────────┐┌─────────┐┌─────────┐
         │  Rust   ││TypeScript││ Python  ││   Go    ││  Docs   │
         │  Code   ││   Code   ││  Code   ││  Code   ││ (Markdown)│
         └─────────┘└─────────┘└─────────┘└─────────┘└─────────┘

         All projections are coherent (same source)
         All projections are deterministic (SHA-256 verified)
         All projections are traceable (SPARQL queries)
```

### The Drift Prevention

```
TRADITIONAL (Code-First):
  Spec ──────────────────────→ (manual interpretation)
     ↘                                                 ↘
       Code ─────────────────→ (manual writing)        DRIFT
     ↗                                                 ↗
  Docs ──────────────────────→ (manual documentation)

  Result: Spec says X, Code does Y, Docs say Z


RDF-FIRST (Ontology-Driven):
  Ontology (.ttl)
      ↓ μ₁ (Normalize)
  Validated Graph
      ↓ μ₂ (Extract)
  SPARQL Bindings
      ├─ μ₃ (Emit) ──→ Code
      ├─ μ₃ (Emit) ──→ Docs
      └─ μ₃ (Emit) ──→ Tests
      ↓ μ₄ (Canonicalize)
  Formatted Artifacts
      ↓ μ₅ (Receipt)
  Cryptographic Proof

  Result: Code, Docs, Tests all generated from same source
  DRIFT IS IMPOSSIBLE
```

---

## Decision Framework

### Should I Cross the Event Horizon?

Use this decision tree to determine if RDF-first is appropriate for your project.

```
START
  │
  ├─ Is your project trivial (< 1000 LOC)? ──YES──> Code-first OK
  │                                              (RDF overhead not worth it)
  NO
  │
  ├─ Do you need formal specifications? ──NO──> Maybe code-first
  │                                          (but consider future needs)
  YES
  │
  ├─ Do specs and code often drift? ──YES──> RDF-first RECOMMENDED
  │                                       (solves drift problem)
  NO
  │
  ├─ Multiple implementations (Rust + TS + Python)? ──YES──> RDF-first RECOMMENDED
  │                                                       (one ontology, many projections)
  NO
  │
  ├─ Compliance requirements (SOC2, HIPAA, etc.)? ──YES──> RDF-first RECOMMENDED
  │                                                     (cryptographic receipts)
  NO
  │
  ├─ Complex domain model (50+ entities)? ──YES──> RDF-first RECOMMENDED
  │                                             (SPARQL querying valuable)
  NO
  │
  └─ RDF-first is OPTIONAL (but still beneficial)
```

### Cost-Benefit Analysis

| Factor | Code-First | RDF-First |
|--------|------------|-----------|
| **Initial Setup** | Low (0-1 day) | Medium (2-3 days) |
| **Learning Curve** | Low (familiar) | Medium (RDF/SPARQL) |
| **First Feature Time** | Fast (2-3 days) | Medium (3-5 days) |
| **Subsequent Features** | Medium (2-3 days) | Fast (1-2 days) |
| **Spec/Code Drift Risk** | High (manual sync) | Zero (impossible) |
| **Onboarding Time** | High (read code) | Low (query ontology) |
| **Traceability** | Low (tribal knowledge) | High (SPARQL) |
| **Compliance** | Manual (error-prone) | Automated (receipts) |
| **Multi-Language** | High effort (duplicate) | Low effort (reproject) |
| **Maintenance** | High (keep in sync) | Low (regenerate) |

**Break-Even Point**: ~5-10 features (2-3 weeks)

After this point, RDF-first is consistently faster and safer.

---

## Success Metrics

### Team Metrics

**Velocity Increase**:
- Target: 2-3x increase after 3-6 months
- Measure: Story points delivered per sprint
- Baseline: Record current velocity before transition

**Drift Incidents**:
- Target: Zero spec/code/docs drift incidents
- Measure: Track discrepancies found in code review
- Baseline: Count current drift incidents per month

**Onboarding Time**:
- Target: 50% reduction in time to first commit
- Measure: Days from hire to first merged PR
- Baseline: Current average onboarding time

**Bug Resolution Time**:
- Target: 30% faster resolution (fix ontology, regenerate)
- Measure: Time from bug report to fix merged
- Baseline: Current average resolution time

### Individual Metrics

**Ontology Fluency**:
- Can write Turtle syntax without reference
- Can write SPARQL queries without reference
- Can design SHACL shapes
- Can create custom Tera templates

**Mental Model Shift**:
- Default to editing ontology, not code
- Debug by querying ontology, not reading code
- Think in graphs, not files
- Explain RDF-first to others

---

## Common Pitfalls

### Pitfall 1: Editing Generated Files

**Symptom**: Manually editing files in `src/`, `docs/`, etc.

**Problem**: Next `ggen sync` will overwrite your changes

**Solution**:
```bash
# ❌ WRONG
vim src/domain/user_story.rs

# ✅ CORRECT
vim .specify/specs/013-feature/feature.ttl
ggen sync
```

**Prevention**: Use `.gitattributes` to mark generated files:
```gitattributes
# .gitattributes
src/domain/*.rs generated
```

### Pitfall 2: Bypassing SHACL Validation

**Symptom**: Using `--force true` to skip validation

**Problem**: Ontology may be invalid, leading to incorrect code generation

**Solution**:
```bash
# ❌ WRONG
ggen sync --force true  # Bypasses validation!

# ✅ CORRECT
ggen sync --validate_only true  # Check first
# Fix validation errors in .ttl files
ggen sync  # Then generate
```

**Prevention**: Make SHACL validation a CI/CD gate

### Pitfall 3: Treating Ontology Like Code

**Symptom**: Copy-pasting large blocks of Turtle, not using SPARQL patterns

**Problem**: Misses the power of semantic queries and inference

**Solution**:
```turtle
# ❌ WRONG (Copy-paste duplicate scenarios)
:us-001-as-001 a sk:AcceptanceScenario ;
    sk:given "User has valid credentials" ;
    sk:when "User submits login form" ;
    sk:then "System displays dashboard" .

:us-002-as-001 a sk:AcceptanceScenario ;
    sk:given "User has valid credentials" ;  # Duplicate!
    sk:when "User submits login form" ;      # Duplicate!
    sk:then "System displays dashboard" .

# ✅ CORRECT (Use OWL inference and SPARQL)
:ValidCredentialsPattern a sk:ScenarioPattern ;
    sk:given "User has valid credentials" ;
    sk:when "User submits login form" .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:usesPattern :ValidCredentialsPattern ;
    sk:then "System displays dashboard" .

:us-002-as-001 a sk:AcceptanceScenario ;
    sk:usesPattern :ValidCredentialsPattern ;
    sk:then "System creates new session" .
```

### Pitfall 4: Over-Engineering the Ontology

**Symptom**: Spending weeks designing perfect ontology before any code

**Problem**: Analysis paralysis, late delivery

**Solution**: Start simple, iterate
```turtle
# ✅ Start here (minimal viable ontology)
:us-001 a sk:UserStory ;
    sk:title "User can log in" .

# ❌ Don't start here (over-engineered)
:us-001 a sk:UserStory ;
    sk:title "User can log in" ;
    sk:priority "P1" ;
    sk:estimatedEffort 5 ;
    sk:riskLevel "Medium" ;
    sk:assignedTo :developer-123 ;
    sk:relatedTo :authentication, :authorization, :session-management ;
    sk:dependsOn :us-000 ;
    sk:blockedBy :us-999 ;
    # ... 50 more predicates
```

**Prevention**: Follow 80/20 rule - start with 20% of ontology that covers 80% of use cases

### Pitfall 5: Ignoring Deterministic Receipts

**Symptom**: Running `ggen sync` without `--audit true`

**Problem**: No cryptographic proof of generation, hard to debug

**Solution**:
```bash
# ❌ WRONG
ggen sync

# ✅ CORRECT
ggen sync --audit true

# Then verify
cat .ggen/receipts/latest.json
```

**Prevention**: Make `--audit true` the default in CI/CD

---

## Further Resources

### Documentation
- [Getting Started Tutorial](../tutorials/01_getting_started.md)
- [RDF-First Specification System](../../.specify/README.md)
- [SPARQL Query Guide](../reference/SPARQL_REFERENCE_CARD.md)
- [Turtle Syntax Reference](https://www.w3.org/TR/turtle/)

### Case Studies
- [Case Study 1: Authentication System](../examples/case-studies/01_authentication.md)
- [Case Study 2: Multi-Language API](../examples/case-studies/02_multi_language_api.md)
- [Case Study 3: Compliance Automation](../examples/case-studies/03_compliance_automation.md)

### Hands-On Exercises
- [Exercise 1: Your First Ontology](../exercises/01_first_ontology.md)
- [Exercise 2: SPARQL Queries](../exercises/02_sparql_queries.md)
- [Exercise 3: Custom Templates](../exercises/03_custom_templates.md)

### Advanced Topics
- [Ontology Design Patterns](../how-to/ontology_design_patterns.md)
- [Performance Optimization](../how-to/performance_optimization.md)
- [Multi-Repository Setups](../how-to/multi_repo_setup.md)

### External Resources
- [W3C RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [W3C SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [W3C SHACL Specification](https://www.w3.org/TR/shacl/)

---

## Conclusion

Crossing the event horizon is a one-way journey. Once you experience:

- Zero spec/code drift
- Deterministic outputs
- Cryptographic receipts
- Machine-readable requirements
- SPARQL-powered debugging

...you cannot go back to code-first thinking.

The ontology becomes reality. Code becomes a projection. And software development transforms from manual craftsmanship to deterministic generation.

**Welcome to the other side of the event horizon.**

---

**Document Status**: Production Template (v1.0.0)
**Last Updated**: 2026-01-24
**Maintainer**: ggen Core Team
**License**: MIT
