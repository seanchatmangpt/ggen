
# 7. VALUE OBJECT RECORD **


*An amount of money is not an integer; it is a concept with invariants.*


---

## Context

Domain-Driven Design teaches that value objects are equal by value, not by identity. A Money(100, USD) is the same as any other Money(100, USD) — there is no meaningful concept of two different money objects that both hold 100 USD. In Java 8, implementing this correctly requires overriding equals(), hashCode(), and toString(), plus making all fields final, plus writing a constructor that validates invariants. This is eight to twenty lines of boilerplate for what should be a two-line concept.\n\nMutable objects pretending to be value objects cause subtle bugs. If Money has a setAmount() method, code that receives a Money and stores it in a map can have that map invalidated when someone mutates the money object. The mutation changes the hashCode(), making the stored entry unreachable.\n\nJava 26 records solve all of this: they are final by default, their components are implicitly private and final, equals/hashCode/toString are generated structurally, and the compact canonical constructor form allows invariant validation in three lines.

---

❖ ❖ ❖

## The Problem

**Mutable objects pretending to be value objects cause subtle bugs when they are shared and mutated.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Use java26:ValueObject in the ontology to generate Java records — immutable data carriers with structural equality, compact canonical constructors that validate invariants.\n\nAnnotate each value object class in the ontology with java26:ValueObject. Declare its fields with java26:hasField. Declare validation rules with java26:hasValidation to emit assertions in the compact canonical constructor.\n\nggen generates a record with all declared fields as components, a compact canonical constructor that runs all declared validations, and factory methods if java26:hasFactoryMethod predicates are present.

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


The value-object Tera template emits a Java record. It iterates over java26:hasValidation triples to emit validation statements in the compact canonical constructor. Validation types include java26:NotNull (Objects.requireNonNull), java26:Positive (if value <= 0 throw), java26:Pattern (String regex match), and java26:Range (min/max check).






---

## The Deeper Pattern


VALUE OBJECT RECORD represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

The pattern is not merely a technique—it is a **relationship** between context, forces, and resolution. When you find yourself in this context, with these forces at play, this pattern provides the shape of the solution.


---

## Confidence Level: **


**Emerging Pattern** (★)

This is an emerging pattern—promising but still evolving. Use it, but expect refinements in future versions as our understanding deepens.


---

## Pattern Connections

This pattern exists within a network of related patterns:


**Category:** Domain Model Patterns

This pattern belongs to the Domain Model Patterns group, which governs how domain knowledge is expressed as Java 26 records, sealed types, and value objects.


### Related Patterns

Explore these connected patterns to deepen your understanding:

- See the [Pattern Map](../pattern-map.md) for complete connection details

---

## When This Pattern Breaks

No pattern is universal. VALUE OBJECT RECORD may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**VALUE OBJECT RECORD** resolves the tension between Mutable objects pretending to be value objects cause subtle bugs when they are shared and mutated..

The solution: Use java26:ValueObject in the ontology to generate Java records — immutable data carriers with structural equality, compact canonical constructors that validate invariants.\n\nAnnotate each value obje...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 7 of 35 in "A Pattern Language for Java 26 with ggen"*
