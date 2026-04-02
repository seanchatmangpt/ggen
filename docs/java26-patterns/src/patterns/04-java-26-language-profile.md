
# 4. JAVA 26 LANGUAGE PROFILE ***


*Sealed types close hierarchies; records flatten ceremony; virtual threads dissolve thread pools.*


---

## Context

Java 26 introduces transformative features — sealed interfaces, record classes, virtual threads, pattern matching for switch — that fundamentally change how we model domains. But teams often stick to Java 8 idioms from habit: abstract classes where sealed interfaces belong, verbose value objects where records suffice, thread pools where virtual threads are simpler.\n\nThe language profile is the set of Java 26 features that ggen targets by default. It is not a menu of optional choices; it is the agreed-upon idiom of the codebase. When every entity is a record or a JPA class (not a mutable POJO), when every hierarchy is sealed, when every service uses virtual threads, the codebase becomes consistent and the templates become simple.\n\nDeclaring the language profile in the ontology — using java26:SealedInterface, java26:ValueObject, java26:VirtualThreadExecutor, etc. — tells ggen precisely which Java 26 constructs to emit for each ontology type.

---

❖ ❖ ❖

## The Problem

**Using Java 26 with Java 8 thinking wastes the type-safety and conciseness that sealed types and records provide.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Establish sealed interfaces for domain hierarchies, records for value objects and DTOs, virtual threads for all I/O, and switch expressions with pattern matching for dispatch. Let ggen generate these patterns consistently.\n\nAnnotate ontology classes with the appropriate Java 26 profile type: java26:Entity for JPA entities, java26:ValueObject for immutable records, java26:SealedInterface for type hierarchies, java26:SealedRecord for sealed hierarchy members. ggen selects the correct template for each type.\n\nVirtual threads are enabled globally by annotating the application class with java26:VirtualThreadExecutor in the ontology, which causes ggen to emit the Tomcat virtual thread executor bean in the generated Spring Boot configuration.

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


The language profile is implemented as a set of OWL classes in the java26: namespace. Each class maps to a distinct Tera template in ggen. The template selection logic in crates/ggen-core/src/codegen/go.rs and python.rs is mirrored in the Java generator: the SPARQL query binds a ?profileType variable, and the Tera template is selected by matching against the java26: namespace IRI.






---

## The Deeper Pattern


JAVA 26 LANGUAGE PROFILE represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

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

No pattern is universal. JAVA 26 LANGUAGE PROFILE may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**JAVA 26 LANGUAGE PROFILE** resolves the tension between Using Java 26 with Java 8 thinking wastes the type-safety and conciseness that sealed types and reco....

The solution: Establish sealed interfaces for domain hierarchies, records for value objects and DTOs, virtual threads for all I/O, and switch expressions with pattern matching for dispatch. Let ggen generate these ...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 4 of 35 in "A Pattern Language for Java 26 with ggen"*
