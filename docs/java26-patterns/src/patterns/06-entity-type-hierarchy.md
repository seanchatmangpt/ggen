
# 6. ENTITY TYPE HIERARCHY **


*A class hierarchy that the compiler can see is worth more than one that lives only in documentation.*


---

## Context

Java domain models often use inheritance or marker interfaces for type hierarchies. Abstract classes allow unknown subclasses — a third-party module can extend them, or a developer can add a new subclass without updating the switch statements that dispatch on type. The compiler cannot enforce exhaustive handling.\n\nBefore sealed types, the workaround was to add an abstract method to each new operation and implement it in all subclasses. This is the Visitor pattern expressed through inheritance, and it works, but it couples operation logic to the domain class, violating separation of concerns.\n\nJava 26 sealed interfaces close the hierarchy. When you declare `sealed interface Payment permits CreditCard, BankTransfer, Crypto`, the compiler knows those are the only permitted implementations. Every switch expression on Payment must handle all three, or it will fail to compile. This compile-time exhaustiveness checking is the key benefit of sealed hierarchies.

---

❖ ❖ ❖

## The Problem

**Type hierarchies expressed via abstract classes allow unknown subclasses; the compiler cannot enforce exhaustive handling.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Use OWL class hierarchies in the ontology to generate sealed interface hierarchies in Java 26, where each variant is a record with only the fields it actually needs.\n\nDeclare the parent class as java26:SealedInterface and each variant as java26:SealedRecord with java26:implements pointing to the parent. ggen generates the sealed interface with the correct permits clause and one record per variant.\n\nEach record carries only the fields declared in the ontology for that variant. Fields common to all variants go on the sealed interface as abstract accessors. Variant-specific fields go on the individual records.

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


The sealed interface template iterates over all java26:SealedRecord instances that reference the current java26:SealedInterface via java26:implements. It collects all field names and emits them as record components. The permits clause lists all permitted record types in the order they appear in java26:permittedSubclasses, or alphabetically if no order is specified.






---

## The Deeper Pattern


ENTITY TYPE HIERARCHY represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

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

No pattern is universal. ENTITY TYPE HIERARCHY may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**ENTITY TYPE HIERARCHY** resolves the tension between Type hierarchies expressed via abstract classes allow unknown subclasses; the compiler cannot enforc....

The solution: Use OWL class hierarchies in the ontology to generate sealed interface hierarchies in Java 26, where each variant is a record with only the fields it actually needs.\n\nDeclare the parent class as jav...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 6 of 35 in "A Pattern Language for Java 26 with ggen"*
