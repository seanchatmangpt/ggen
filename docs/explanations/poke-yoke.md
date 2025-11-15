# Poke-Yoke: Error Prevention in Code Generation

**Type: Explanation** | [← Back to Documentation](../README.md)

## What is Poke-Yoke?

**Poke-Yoke** (ポカヨケ, "fool-proof" in Japanese) is a quality assurance method that prevents errors **before they happen** rather than detecting them after.

Instead of finding mistakes, poke-yoke **makes mistakes impossible**.

### Real-World Examples

- **Manufacturing**: Plugs with different shapes so you can't insert them wrong way
- **Hospitals**: Medication syringes with unique connectors to prevent wrong drug administration
- **Software**: Compiler type systems that prevent invalid operations at compile time
- **ggen**: Single RDF ontology that prevents code drift across languages

---

## How ggen is a Poke-Yoke

ggen itself implements poke-yoke at the code generation level:

### The Failure Mode

```
Failure: "Code inconsistency between Rust, TypeScript, and Python"
Severity: 8/10 (causes bugs and security issues)
Occurrence: 10/10 (happens constantly without ggen)
Detection: 3/10 (found late in integration testing)
Risk Priority Number (RPN) = 240 (CRITICAL)
```

### Traditional Approach (Error-Prone)

```
Manual Process:
├─ Rust implementation (write struct)
├─ TypeScript implementation (write interface)
├─ Python implementation (write class)
├─ Update Rust → might forget TypeScript
├─ Update TypeScript → might forget Python
├─ Integration tests fail
└─ Hours debugging: "Why is TypeScript different from Rust?"
```

**Problem**: Humans are fallible. Synchronizing code across languages manually creates drift.

### ggen's Poke-Yoke Solution

```
Ontology-Driven Approach:
├─ Define domain ONCE in RDF (single source of truth)
├─ Generate Rust from ontology
├─ Generate TypeScript from ontology
├─ Generate Python from ontology
├─ Update ontology → regenerate ALL languages
└─ Guaranteed consistency (impossible to drift)
```

**Result**: The ontology is the poke-yoke. Drift is **mechanically prevented**.

---

## Example: Quality Control System with FMEA

### Scenario: Building a Failure Mode & Effects Analysis (FMEA) System

#### Without ggen (Error-Prone)

```rust
// Rust implementation
pub struct FailureMode {
    pub severity: i32,      // 1-10
    pub occurrence: i32,    // 1-10
    pub detection: i32,     // 1-10
    pub rpn: i32,           // calculated
}
```

```typescript
// TypeScript implementation (manual, error-prone)
export interface FailureMode {
    severity: number;       // 1-10
    occurrence: number;     // 1-10
    // Oops! Developer forgot 'detection' field
    rpn: number;            // calculated
}
```

**Result**: Rust and TypeScript have different fields. Integration fails. Poke-yoke FAILED.

#### With ggen (Poke-Yoke Applied)

```turtle
# Single RDF ontology
ex:FailureMode a rdfs:Class ;
    rdfs:label "Failure Mode" ;
    rdfs:comment "A potential system failure" .

ex:severity a rdf:Property ;
    rdfs:domain ex:FailureMode ;
    rdfs:range xsd:integer ;
    rdfs:comment "Severity rating 1-10" .

ex:occurrence a rdf:Property ;
    rdfs:domain ex:FailureMode ;
    rdfs:range xsd:integer ;
    rdfs:comment "Occurrence likelihood 1-10" .

ex:detection a rdf:Property ;
    rdfs:domain ex:FailureMode ;
    rdfs:range xsd:integer ;
    rdfs:comment "Detection ability 1-10" .

ex:rpn a rdf:Property ;
    rdfs:domain ex:FailureMode ;
    rdfs:range xsd:integer ;
    rdfs:comment "Risk Priority Number (S×O×D)" .
```

```bash
# Generation commands (deterministic, identical output)
$ ggen template generate-rdf --ontology fmea.ttl --template rust-models
→ Generates Rust struct with EXACTLY 4 fields

$ ggen template generate-rdf --ontology fmea.ttl --template typescript-models
→ Generates TypeScript interface with EXACTLY 4 fields

$ ggen template generate-rdf --ontology fmea.ttl --template python-pydantic
→ Generates Python class with EXACTLY 4 fields
```

**Result**: All languages have identical structure. Drift is **impossible**. Poke-yoke SUCCEEDED.

---

## Poke-Yoke Mechanisms in ggen

### 1. Single Source of Truth (Ontology)

**Mechanism**: One RDF ontology defines the entire domain

**Prevents**: Inconsistent domain models across languages

**How**: All code generation queries the same ontology using SPARQL

### 2. Deterministic Generation

**Mechanism**: Same ontology input → byte-identical output every time

**Prevents**: Non-reproducible code generation, divergent outputs

**How**: SPARQL queries are deterministic; templates have no randomness

### 3. Semantic Validation

**Mechanism**: SPARQL validates ontology structure before generation

**Prevents**: Invalid domain models becoming code

**How**: SHACL shapes validate ontology before generation runs

### 4. Type Safety Across Languages

**Mechanism**: RDF types map consistently to language types

Example:
```
xsd:integer    → i32 (Rust)    → number (TypeScript)    → int (Python)
xsd:string     → String (Rust) → string (TypeScript)     → str (Python)
xsd:decimal    → f64 (Rust)    → number (TypeScript)     → Decimal (Python)
```

**Prevents**: Type mismatches between languages

**How**: Type mapping tables ensure consistency

### 5. Automated Regeneration

**Mechanism**: Change ontology → regenerate all code automatically

**Prevents**: Manual code updates that get out of sync

**How**: `ggen template generate-rdf` with same ontology updates everything

---

## Poke-Yoke vs. Error Detection

| Aspect | Poke-Yoke (Prevention) | Error Detection (Catching) |
|--------|----------------------|---------------------------|
| **When** | Prevents at design time | Detects after failure |
| **Example (ggen)** | Impossible to have code drift | Find drift in tests |
| **Cost** | Low (free with ontology) | High (debugging, fixes) |
| **User Impact** | No errors reach users | Some errors reach users |
| **Maintenance** | Single source to maintain | Multiple sources to sync |

---

## Types of Poke-Yoke in ggen

### 1. Design Constraint Poke-Yoke

**Mechanism**: Ontology structure prevents invalid states

**Example**:
```turtle
# Define that severity must be 1-10
ex:severity sh:minInclusive 1 ;
             sh:maxInclusive 10 .
```

**Result**: Generated code includes validation; impossible to use 0 or 11

### 2. Automated Enforcement Poke-Yoke

**Mechanism**: Generation scripts enforce consistency

**Example**:
```bash
# SPARQL query enforces: every FailureMode has severity, occurrence, detection
SELECT ?fm
WHERE {
    ?fm a ex:FailureMode .
    FILTER NOT EXISTS { ?fm ex:severity ?s . }
    FILTER NOT EXISTS { ?fm ex:occurrence ?o . }
    FILTER NOT EXISTS { ?fm ex:detection ?d . }
}
# If this returns anything, generation fails
```

### 3. Structural Poke-Yoke

**Mechanism**: Language features prevent misuse

**Example** (Rust):
```rust
// Type system (Rust poke-yoke)
let severity: i32 = "invalid"; // ❌ Compiler error (poke-yoke!)
let severity: i32 = 5;         // ✅ Type safe
```

---

## Real-World Failure Mode: Documentation Drift

ggen team identified this failure mode:

**FM2: Documentation Claims Don't Match Code**
- Severity: 7/10
- Occurrence: 6/10
- Detection: 5/10
- RPN = 210

### Without Poke-Yoke

```
Developer updates code
  ↓
Forgot to update documentation
  ↓
Documentation and code are now inconsistent
  ↓
Users read old docs, misuse API
  ↓
Bugs, support tickets, frustrated users
```

### With Poke-Yoke (ggen's approach)

```
Update ontology (single source of truth)
  ↓
Regenerate code AND documentation
  ↓
Code and documentation are ALWAYS in sync
  ↓
Documentation literally comes from code
  ↓
No inconsistency possible (poke-yoke!)
```

---

## Why Poke-Yoke Matters

### Failure Without It

Without poke-yoke, you need:
- **Testing** to find mistakes (expensive, incomplete)
- **Code review** to catch drifts (slow, fallible)
- **Manual synchronization** across languages (error-prone)
- **Documentation maintenance** (never stays in sync)

### Success With It

With poke-yoke:
- **Prevention** at source (free, automatic)
- **Impossible** to violate constraints
- **Automatic** synchronization (no manual work)
- **Documentation** generated from code (always current)

---

## Learning Path

**New to error prevention concepts?**
- Start: [FMEA Analysis](../FMEA_ANALYSIS.md) - Learn about systematic failure analysis
- Then: This document - Understand how prevention works
- Next: [Ontology-Driven Development](ontology-driven.md) - See poke-yoke in practice

**Want to apply poke-yoke to your domain?**
- Tutorial: [Getting Started](../tutorials/getting-started.md) - Generate your first ontology
- Guide: [Use RDF Ontologies](../how-to-guides/use-rdf-ontologies.md) - Define constraints
- Reference: [RDF/SPARQL Reference](../reference/rdf-sparql.md) - All RDF features

**Want to understand the architecture?**
- Explanation: [Architecture](architecture.md) - How ggen implements these ideas
- Reference: [Template Reference](../reference/templates.md) - How generation works

---

## Key Takeaway

**Poke-yoke prevents errors at design time through ontology-driven code generation.**

- One ontology → All languages identical
- Impossible to drift → Consistency guaranteed
- Automatic updates → No manual synchronization needed
- Prevention > Detection → Cheaper, more reliable

This is why ggen treats code as a **projection** of knowledge graphs, not a **collection** of templated files.

---

## See Also

- **[FMEA Analysis](../FMEA_ANALYSIS.md)** - How ggen identifies failure modes
- **[Ontology-Driven Development](ontology-driven.md)** - Why this architecture
- **[Architecture](architecture.md)** - How it's implemented
- **[Code Projections](projections.md)** - How code is generated from ontology
- **[Getting Started Tutorial](../tutorials/getting-started.md)** - Try it yourself
