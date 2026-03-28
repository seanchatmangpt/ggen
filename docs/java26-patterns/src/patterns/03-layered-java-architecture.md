
# 3. LAYERED JAVA ARCHITECTURE ***


*Domain → Repository → Service → Controller — each layer knows only the one below.*


---

## Context

Spring Boot applications grow chaotic when layers communicate freely. Services call controllers, controllers access repositories directly, entities contain business logic, and DTOs carry domain logic they shouldn't. This architectural erosion happens gradually, one shortcut at a time, and is very hard to reverse.\n\nThe four-layer architecture — domain, persistence, application, and presentation — is a proven solution. Each layer has a single responsibility and depends only on the layer directly below it. Domain entities know nothing about persistence. Repositories know nothing about HTTP. Services orchestrate domain logic and repository calls. Controllers translate HTTP to service calls.\n\nWhen layers are generated from an ontology, the boundaries are enforced structurally: the templates physically cannot produce cross-layer dependencies because each template knows only its own layer's patterns.

---

❖ ❖ ❖

## The Problem

**Without enforced layer boundaries, Java applications become big balls of mud.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Enforce the four-layer architecture: @Entity (domain) → @Repository (persistence) → @Service @Transactional (application) → @RestController (presentation). Generate all four layers from a single ontology entity definition.\n\nA single java26:Entity declaration in the ontology triggers four generation rules: entity, repository, service, and controller. Each rule has its own SPARQL query, Tera template, and output path pattern. The rules are independent and run in parallel.\n\nThe generated code enforces layer boundaries: the @Entity has no Spring annotations, the @Repository only sees the @Entity, the @Service only sees the @Repository, and the @RestController only sees the @Service.

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


Define four [[generation.rules]] in ggen.toml — entity, repository, service, controller — each targeting a different output directory: domain/, repository/, service/, controller/. The entity template has no Spring imports. The repository template imports only the entity and Spring Data. The service template imports only the repository interface and transaction annotations. The controller template imports only the service interface and Spring Web.






---

## The Deeper Pattern


LAYERED JAVA ARCHITECTURE represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

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

No pattern is universal. LAYERED JAVA ARCHITECTURE may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**LAYERED JAVA ARCHITECTURE** resolves the tension between Without enforced layer boundaries, Java applications become big balls of mud..

The solution: Enforce the four-layer architecture: @Entity (domain) → @Repository (persistence) → @Service @Transactional (application) → @RestController (presentation). Generate all four layers from a single ontol...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 3 of 35 in "A Pattern Language for Java 26 with ggen"*
