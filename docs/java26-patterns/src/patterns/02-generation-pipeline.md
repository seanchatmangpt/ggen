
# 2. GENERATION PIPELINE ***


*Knowledge flows through five gates and becomes code on the other side.*


---

## Context

Code generation systems often couple extraction, transformation, and output into a single, untestable blob. A change to the SPARQL query breaks template rendering. A change to the template breaks output formatting. The coupling makes the system fragile and hard to reason about.\n\nThe five-stage μ pipeline separates each concern into an independently testable stage. Each stage has a clearly defined input and output: μ₁ Construct normalizes the RDF graph, μ₂ Select extracts typed bindings via SPARQL, μ₃ Render transforms bindings into text via Tera templates, μ₄ Canonicalize formats the output, and μ₅ Receipt produces a cryptographic proof of provenance.\n\nThis structure allows each stage to be tested in isolation, swapped out independently, and optimized without affecting other stages. It is the foundational architecture on which all other patterns depend.

---

❖ ❖ ❖

## The Problem

**Without a clean pipeline, generation logic becomes a tangled mess of template string manipulation.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Apply μ₁ Construct (normalize RDF) → μ₂ Select (SPARQL extraction) → μ₃ Render (Tera templates) → μ₄ Canonicalize (format) → μ₅ Receipt (cryptographic proof) to every generation task.\n\nEach stage passes a typed artifact to the next: the raw OWL graph becomes a normalized graph (μ₁), which becomes a list of SPARQL binding maps (μ₂), which becomes a rendered string per binding (μ₃), which becomes formatted source code (μ₄), which becomes a signed receipt entry (μ₅).\n\nWrite one SPARQL query and one Tera template per artifact type. ggen executes the full pipeline for every [[generation.rules]] entry in ggen.toml whenever `ggen sync` is invoked.

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


The ggen pipeline is implemented in crates/ggen-core/src/codegen/mod.rs. Each stage is a pure function: no global state, no side effects between stages. The pipeline runner parallelizes all rules using Tokio tasks and merges outputs using three-way merge if a target file already exists.\n\nSPARQL queries live in .specify/queries/. Tera templates live in .specify/templates/. ggen validates both against the ontology at startup, failing fast if a query references an IRI not present in the ontology.






---

## The Deeper Pattern


GENERATION PIPELINE represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

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

No pattern is universal. GENERATION PIPELINE may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**GENERATION PIPELINE** resolves the tension between Without a clean pipeline, generation logic becomes a tangled mess of template string manipulation..

The solution: Apply μ₁ Construct (normalize RDF) → μ₂ Select (SPARQL extraction) → μ₃ Render (Tera templates) → μ₄ Canonicalize (format) → μ₅ Receipt (cryptographic proof) to every generation task.\n\nEach stage pa...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 2 of 35 in "A Pattern Language for Java 26 with ggen"*
