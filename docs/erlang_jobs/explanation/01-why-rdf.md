# Why Use RDF for Code Generation

**Understanding the benefits of ontology-driven development and semantic modeling.**

---

## The Problem with Traditional Code Generation

Traditional code generation tools (scaffolders, boilerplate generators, templates) suffer from:

1. **No semantic model:** Templates are syntax-focused, not meaning-focused
2. **Brittle:** Change requirements → rewrite templates
3. **Not composable:** Can't query, infer, or reason about generated code
4. **No single source of truth:** Code, docs, configs all drift apart
5. **Not auditable:** No proof that generated code matches spec

**Example:** You have a job queue. Later, you need to add rate limiting. With traditional tools:
- Edit multiple templates
- Manually update tests
- Rewrite documentation
- Hope nothing breaks

---

## The RDF Solution: Ontology as Source of Truth

**RDF (Resource Description Framework)** is a W3C standard for semantic modeling. With RDF:

1. **Semantic model:** Define what things *mean*, not just how they look
2. **Single source of truth:** All code, tests, docs generated from one ontology
3. **Composable:** Query with SPARQL, infer with OWL, validate with SHACL
4. **Auditable:** Cryptographic proof that code matches ontology
5. **Evolvable:** Add properties → regenerate → everything updates

**Same example with RDF:** You have a job queue ontology. Need rate limiting?
- Add `jobs:hasRateLimiter` property to ontology
- Run `ggen sync`
- Rate limiting code, tests, docs, configs all generated automatically

---

## RDF Core Concepts

### Triples: Subject-Predicate-Object

RDF models knowledge as triples:

```turtle
jobs:JobQueue jobs:hasProperty jobs:priority .
   ↑              ↑                ↑
Subject        Predicate        Object
```

**Reads as:** "JobQueue has property 'priority'"

### Classes and Properties

```turtle
jobs:JobQueue a rdfs:Class ;
    rdfs:label "Job Queue" ;
    jobs:hasProperty jobs:priority ;
    jobs:hasProperty jobs:domain .

jobs:priority a rdf:Property ;
    rdfs:range jobs:Priority ;
    rdfs:label "Job priority level" .

jobs:Priority a rdfs:Class ;
    jobs:enumValues ("high" "normal" "low") .
```

**This models:**
- JobQueue is a class
- It has two properties: priority and domain
- priority has range Priority (enum with 3 values)

### Inference with OWL

```turtle
jobs:WorkerPool rdfs:subClassOf jobs:Component .
jobs:Component jobs:requiresMonitoring true .

# Inference: WorkerPool requires monitoring (inherited from Component)
```

OWL (Web Ontology Language) allows reasoning:
- `rdfs:subClassOf` - Inheritance
- `owl:inverseOf` - Bidirectional relationships
- `owl:TransitiveProperty` - Transitive relations

### Validation with SHACL

```turtle
jobs:JobQueueShape a sh:NodeShape ;
    sh:targetClass jobs:JobQueue ;
    sh:property [
        sh:path jobs:priority ;
        sh:in (jobs:high jobs:normal jobs:low) ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] .
```

SHACL (Shapes Constraint Language) validates:
- Every JobQueue must have exactly 1 priority
- priority must be high, normal, or low

**Before code generation, ggen runs SHACL validation.** If ontology is invalid, generation fails.

---

## Benefits for Code Generation

### 1. Deterministic Output

**Problem:** "I ran the generator twice, got different code!"

**RDF solution:** Same ontology → same code, always. Proven with SHA-256 hashes.

```bash
$ ggen sync
# Receipt: files_hash = sha256:abc123...

$ ggen sync  # Run again
# Receipt: files_hash = sha256:abc123...  # Identical!
```

---

### 2. Composability via SPARQL

**Problem:** "I need to find all entities with retry logic."

**RDF solution:** Query with SPARQL.

```sparql
SELECT ?entity WHERE {
    ?entity jobs:hasProperty jobs:maxRetries .
    ?entity jobs:hasProperty jobs:retryBackoffMs .
}

# Results: JobQueue, Worker, Scheduler
```

Now generate retry logic for all matching entities automatically.

---

### 3. Knowledge Graphs

**Problem:** "How do these components relate?"

**RDF solution:** Knowledge graph with relationships.

```turtle
jobs:JobQueue jobs:usesBackend jobs:NATS .
jobs:WorkerPool jobs:pullsFrom jobs:JobQueue .
jobs:Scheduler jobs:publishesTo jobs:JobQueue .

# Visualization:
#   NATS ← JobQueue → WorkerPool
#            ↑
#        Scheduler
```

ggen uses this graph to:
- Generate integration tests (test WorkerPool ↔ JobQueue interaction)
- Produce architecture diagrams
- Create deployment configs (NATS must start before JobQueue)

---

### 4. Multi-Target Generation

**Problem:** "I need Erlang, TypeScript, and OpenAPI specs."

**RDF solution:** One ontology, multiple templates.

```
.specify/specs/001-job-queue/feature.ttl  (single source)
    ↓
ggen sync (with different templates)
    ↓
├── src/job_queue.erl          (Erlang)
├── src/job_queue.ts           (TypeScript)
├── docs/openapi.yaml          (OpenAPI)
└── deployments/k8s.yaml       (Kubernetes)
```

All generated from the same semantic model. No drift.

---

### 5. Version Control Ontologies, Not Code

**Problem:** "Which version of generated code matches spec v1.2.3?"

**RDF solution:** Version control the ontology. Code is ephemeral.

```bash
# Git history shows ontology changes, not generated code
$ git log .specify/specs/001-job-queue/feature.ttl
commit abc123 "Add rate limiting to JobQueue"
commit def456 "Increase worker pool size to 20"

# Generated code is in .gitignore (or CI regenerates it)
```

**Benefits:**
- Smaller git diffs (ontology changes, not 10,000 lines of generated code)
- Easier code review (review ontology, not generated boilerplate)
- Reproducible builds (checkout commit, run `ggen sync`, identical code)

---

### 6. Documentation is Free

**Problem:** "Docs are outdated. Code changed, docs didn't."

**RDF solution:** Generate docs from the same ontology.

```turtle
jobs:JobQueue rdfs:comment "A priority-based job queue with pull semantics" .
```

This comment appears in:
- Erlang edoc (`@doc` comments)
- OpenAPI description
- README.md
- Architecture diagrams

**All generated. All in sync. Always.**

---

## ggen's Five-Stage Pipeline (μ)

ggen transforms RDF to code via five deterministic stages:

```
μ₁ (Normalize)   → Validate RDF, SHACL conformance
μ₂ (Extract)     → SPARQL queries, OWL inference
μ₃ (Emit)        → Tera template rendering
μ₄ (Canonicalize)→ Deterministic formatting
μ₅ (Receipt)     → Cryptographic proof generation
```

**Each stage is deterministic:**
- μ₁: Same ontology → same normalized graph
- μ₂: Same queries → same extracted entities
- μ₃: Same template → same rendered code
- μ₄: Same code → same formatted output
- μ₅: Same files → same hashes

**Result:** $A = \mu(O)$ is a pure function. No side effects. Reproducible.

---

## Real-World Example: Adding Rate Limiting

**Traditional approach:**
1. Edit job_queue.erl template (100 lines of code)
2. Edit job_worker.erl template (50 lines)
3. Update tests manually (30 lines)
4. Update docs manually (10 pages)
5. Update configs (5 files)
6. Total: ~200 edits, high error rate

**RDF approach:**
1. Add 3 triples to ontology:

```turtle
jobs:JobQueue jobs:hasRateLimiter jobs:TokenBucket .
jobs:TokenBucket jobs:rateLimit 1000 .
jobs:TokenBucket jobs:burstSize 500 .
```

2. Run `ggen sync`
3. Done. All code, tests, docs, configs updated automatically.

**Total edits:** 3 triples. **Error rate:** 0 (validated by SHACL before generation).

---

## Comparison: RDF vs Traditional

| Aspect | Traditional Templates | RDF Ontology |
|--------|----------------------|--------------|
| **Source of truth** | Templates (syntax) | Ontology (semantics) |
| **Composability** | None | SPARQL, OWL inference |
| **Validation** | Manual | SHACL (automatic) |
| **Multi-target** | Duplicate templates | One ontology, many templates |
| **Determinism** | No guarantee | Cryptographically proven |
| **Documentation** | Manual, drifts | Generated, always in sync |
| **Version control** | Code and templates | Ontology only |
| **Change propagation** | Manual edits | Automatic regeneration |

---

## When to Use RDF

**Good fit:**
- Domain-driven design (complex business logic)
- Multi-language projects (Erlang + TypeScript + OpenAPI)
- High compliance requirements (audit trails needed)
- Evolving requirements (add properties → regenerate)

**Not a good fit:**
- One-off scripts (overhead not worth it)
- UI-heavy apps (visual design doesn't map to RDF)
- Prototype/throwaway code (semantic modeling is overkill)

---

## Further Reading

- [OTP Design Patterns in the Jobs Library](02-otp-patterns.md) — How RDF models OTP
- [Deterministic Generation Benefits](05-deterministic-generation.md) — Cryptographic proofs
- [RDF Ontology Reference](../reference/02-rdf-ontology.md) — Complete RDF vocabulary

---

## Summary

RDF provides:
- **Semantic modeling:** What things mean, not just how they look
- **Single source of truth:** Ontology → all artifacts
- **Composability:** SPARQL queries, OWL inference
- **Validation:** SHACL constraints (fail fast)
- **Determinism:** Same ontology → same code (proven)
- **Multi-target:** One ontology, many outputs
- **Evolvability:** Add properties → regenerate → everything updates

**Holographic factory metaphor:** RDF is the holographic film encoding domain knowledge. Code is a projection of that film via the ggen pipeline (μ). Change the film → projection changes deterministically.

---

**Generated by ggen v6.0.0 | 2026-01-29**
