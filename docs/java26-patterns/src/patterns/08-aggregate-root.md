
# 8. AGGREGATE ROOT **


*The root holds the boundary; nothing enters or leaves except through it.*


---

## Context

DDD aggregates are consistency boundaries. All changes flow through the aggregate root, which enforces invariants for the entire cluster. A Customer has an Address and a list of Orders. You never modify the Address directly from outside; you call customer.updateAddress(), which validates the change and maintains consistency.\n\nIn practice, Java applications often bypass aggregate boundaries because of JPA's transparent persistence. If the Customer is a JPA entity and the Address is an @Embeddable, nothing prevents code from fetching the customer, reaching into the address field, and calling setCity() directly on the embedded object. The aggregate boundary exists in documentation, not in the type system.\n\nThe generate-all-three approach (entity + service + controller) enforces the boundary structurally: the @Service has only the repository as a dependency, so all access to the aggregate goes through service methods that enforce invariants. The @RestController has only the service, so no HTTP handler can bypass the service layer.

---

❖ ❖ ❖

## The Problem

**Direct access to child entities bypasses aggregate invariants and causes consistency violations.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Use java26:AggregateRoot to generate the complete trinity: @Entity (persistence), @Service @Transactional (application logic with invariant enforcement), @RestController (API surface).\n\nDeclare the root entity and its child entities in the ontology using java26:AggregateRoot and java26:AggregateMember. Declare domain operations with java26:hasDomainOperation. ggen generates a service method for each declared operation that enforces the aggregate invariant before delegating to the entity.\n\nThe generated @Entity uses @OneToMany(cascade = ALL, orphanRemoval = true) for child collections, ensuring that child entities are only managed through the aggregate root.

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


The aggregate-root SPARQL query selects all java26:AggregateRoot instances and their java26:AggregateMember children. The service template generates one method per java26:hasDomainOperation, each wrapped in @Transactional and containing a call to the entity's invariant-checking method before the repository save.






---

## The Deeper Pattern


AGGREGATE ROOT represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

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

No pattern is universal. AGGREGATE ROOT may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**AGGREGATE ROOT** resolves the tension between Direct access to child entities bypasses aggregate invariants and causes consistency violations..

The solution: Use java26:AggregateRoot to generate the complete trinity: @Entity (persistence), @Service @Transactional (application logic with invariant enforcement), @RestController (API surface).\n\nDeclare the ...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 8 of 35 in "A Pattern Language for Java 26 with ggen"*
