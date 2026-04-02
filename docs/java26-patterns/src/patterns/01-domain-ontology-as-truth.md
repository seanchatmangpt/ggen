
# 1. DOMAIN ONTOLOGY AS TRUTH ***


*Before the first line of Java is written, the domain already exists as triples.*


---

## Context

Large Java systems suffer from model drift — JPA classes, DTOs, OpenAPI specs, and documentation all diverge over time. Teams spend more energy on synchronization than on domain logic. The root cause is that no single representation is authoritative.\n\nEvery artifact is a downstream projection of some conceptual domain model that nobody ever formally specified. The JPA entity is an interpretation. The DTO is a re-interpretation. The OpenAPI schema is a third interpretation. When requirements change, each artifact must be updated manually, and the team hopes they remembered every location.\n\nThe OWL/RDF Turtle file solves this by making the ontology the single source of truth. It is not documentation — it is the domain itself, expressed in a formalism that ggen can read, validate, and project into every artifact simultaneously.

---

❖ ❖ ❖

## The Problem

**When domain knowledge lives in multiple places, they inevitably diverge; when they diverge, bugs follow.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Author the entire domain in a single OWL Turtle file (.ttl) and let ggen project every artifact — entities, records, sealed hierarchies, services, controllers — deterministically from it via A = μ(O).\n\nEvery change starts in the .ttl file. Every artifact is regenerated. If the ontology is consistent, the code is consistent. The diff of the .ttl file is the diff of the entire system.\n\nPlace the ontology at .specify/specs/domain/domain.ttl and run ggen sync. The pipeline validates SHACL constraints, constructs a normalized RDF graph, selects bindings via SPARQL, renders Tera templates, and writes formatted Java source files with a cryptographic receipt proving provenance.

---

❖ ❖ ❖

## Use This Pattern

**Step 1 — Define your domain in TTL**

Copy this into your `.specify/domain.ttl` (customize the URIs and literal values):

```turtle
# TTL snippet for this pattern
```

**Step 2 — Configure ggen.toml**

Add this rule to your `ggen.toml`:

```toml
# ggen.toml rule for this pattern
```

**Step 3 — Generate**

```bash
ggen sync
```

**Step 4 — Generated Java 26 Code**

```java
// Java preview for this pattern
```

---

## Implementation in ggen sync


ggen reads the ontology section of ggen.toml to locate the source TTL files. The [[generation.rules]] table maps each rule name to a SPARQL query, a Tera template, and an output path pattern.\n\nRunning `ggen sync` executes all five stages (μ₁-μ₅) for every rule in parallel. The result is a complete, formatted, compilation-ready Java source tree and a .ggen/receipt.json file containing a SHA-256 hash of every input ontology file and every output source file, enabling reproducible builds and audit trails.






---

## The Deeper Pattern


DOMAIN ONTOLOGY AS TRUTH represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

The pattern is not merely a technique—it is a **relationship** between context, forces, and resolution. When you find yourself in this context, with these forces at play, this pattern provides the shape of the solution.


---

## Confidence Level: ***


**Emerging Pattern** (★)

This is an emerging pattern—promising but still evolving. Use it, but expect refinements in future versions as our understanding deepens.


---

## Pattern Connections

This pattern exists within a network of related patterns:


**Category:** Super-Patterns

This pattern belongs to the Super-Patterns group, which establishes the fundamental super-structures that span the entire Java 26 application.


### Related Patterns

Explore these connected patterns to deepen your understanding:

- See the [Pattern Map](../pattern-map.md) for complete connection details

---

## When This Pattern Breaks

No pattern is universal. DOMAIN ONTOLOGY AS TRUTH may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**DOMAIN ONTOLOGY AS TRUTH** resolves the tension between When domain knowledge lives in multiple places, they inevitably diverge; when they diverge, bugs fol....

The solution: Author the entire domain in a single OWL Turtle file (.ttl) and let ggen project every artifact — entities, records, sealed hierarchies, services, controllers — deterministically from it via A = μ(O)....

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 1 of 35 in "A Pattern Language for Java 26 with ggen"*
