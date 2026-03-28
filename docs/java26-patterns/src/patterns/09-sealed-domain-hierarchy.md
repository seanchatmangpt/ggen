
# 9. SEALED DOMAIN HIERARCHY **


*Name every state your domain can be in; then let the compiler enforce the naming.*


---

## Context

Domains have states — an Order is Pending, Processing, Shipped, Delivered, or Cancelled. Java 8 modeled this with enums or abstract classes, both of which have significant limitations. Enums cannot carry state-specific data: a Shipped order has a tracking number, but a Pending order does not. An abstract class hierarchy doesn't enforce exhaustive handling: a switch that forgets to handle Cancelled compiles just fine.\n\nThe combination of sealed interfaces and records solves both problems. Each state is a record that carries exactly the fields that state requires: Shipped has a trackingNumber and shippedAt, Delivered adds a deliveredAt, Cancelled has a reason. The sealed interface closes the hierarchy so the compiler enforces exhaustiveness.\n\nThis pattern is the domain-state analog of Pattern 6 (Entity Type Hierarchy). Where Pattern 6 models type variants, this pattern models state variants. States often appear in domain-event-sourced systems where the current state is determined by replaying events.

---

❖ ❖ ❖

## The Problem

**Enums cannot carry state-specific data; abstract class hierarchies don't enforce exhaustive handling.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Use java26:SealedInterface with java26:SealedRecord variants to generate sealed domain hierarchies where each state carries its own fields and the compiler enforces exhaustive handling.\n\nDeclare each state as a separate java26:SealedRecord with its own fields. Common fields (like orderId and timestamp) go on all records. State-specific fields (like trackingNumber) go only on the relevant record. The sealed interface lists all states in its permits clause.\n\nUse this pattern wherever a domain concept has distinct states with different associated data. The compiler will flag any switch expression that does not handle all declared states.

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


The sealed-domain-hierarchy template generates the sealed interface first, then each record in declaration order from java26:permittedSubclasses. Common fields are identified by SPARQL GROUP BY and emitted on both the interface (as abstract methods) and each record (as components). State-specific fields are emitted only on the record that declares them.






---

## The Deeper Pattern


SEALED DOMAIN HIERARCHY represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

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

No pattern is universal. SEALED DOMAIN HIERARCHY may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**SEALED DOMAIN HIERARCHY** resolves the tension between Enums cannot carry state-specific data; abstract class hierarchies don't enforce exhaustive handling....

The solution: Use java26:SealedInterface with java26:SealedRecord variants to generate sealed domain hierarchies where each state carries its own fields and the compiler enforces exhaustive handling.\n\nDeclare eac...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 9 of 35 in "A Pattern Language for Java 26 with ggen"*
