
# 5. ONTOLOGY-DRIVEN VERSIONING ***


*The diff of the .ttl file IS the changelog.*


---

## Context

Schema migrations, API version changes, and release notes are typically written by hand, long after the actual change was made. The developer modifies the JPA entity, manually writes a Flyway migration script, updates the OpenAPI spec, writes a changelog entry, and increments the version number — four separate, error-prone steps for what is conceptually one change.\n\nThis creates documentation drift: the migration doesn't match the entity, the changelog doesn't match the API spec, the version number is bumped by convention rather than by rule. Teams spend significant time in code review chasing discrepancies that a machine could detect and prevent.\n\nOntology-driven versioning makes the TTL diff the single source of all change artifacts. The ontology has a version number (owl:versionInfo). When a field is added, removed, or renamed in the TTL, ggen detects the structural change and generates the Flyway migration SQL, the updated version bump, and a changelog entry — all from the same diff.

---

❖ ❖ ❖

## The Problem

**Manually maintained changelogs, migration scripts, and API versions diverge from the actual code.**


The forces at play create tension between competing needs. Without resolution, these forces lead to suboptimal outcomes—complexity where simplicity is needed, fragility where robustness is required, opacity where clarity is essential.


---

## Therefore

Generate Flyway migrations, semantic version bumps, and API changelogs directly from ontology diffs. When the TTL changes, `ggen sync` produces the migration SQL, the updated version number, and the changelog entry.\n\nRun `ggen diff --from v1.0.0 --to HEAD` to compute the structural diff of the ontology between two versions. The diff output drives three generation rules: flyway-migration (SQL DDL), version-bump (semantic version increment), and changelog-entry (Markdown entry). All three are deterministic given the same diff.\n\nStore the ontology version in owl:versionInfo. ggen reads this value and uses semantic versioning rules: adding a field is a minor bump, removing a field is a major bump, adding a nullable field is a patch bump.

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


The ontology diff engine in crates/ggen-core/src/codegen/canonicalize.rs computes structural differences between two RDF graphs. The diff is expressed as a list of AddTriple and RemoveTriple operations. The Flyway migration template maps triple operations to SQL DDL: AddTriple on a java26:hasField with java26:fieldType xsd:string becomes ALTER TABLE ... ADD COLUMN ... VARCHAR(255).






---

## The Deeper Pattern


ONTOLOGY-DRIVEN VERSIONING represents a fundamental approach to solving a recurring problem in Java 26 application development. Understanding the forces it resolves helps you apply it effectively and recognize when it applies to new situations.

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

No pattern is universal. ONTOLOGY-DRIVEN VERSIONING may struggle when:

- Circumstances differ significantly from the pattern's context
- Forces balance differently than the pattern assumes
- Integration with external systems introduces constraints
- Java preview features are not yet stable or available in your JDK

When the pattern doesn't fit, step back and examine the forces. The pattern may need adaptation, or a different pattern may apply.

---



## Summary

**ONTOLOGY-DRIVEN VERSIONING** resolves the tension between Manually maintained changelogs, migration scripts, and API versions diverge from the actual code..

The solution: Generate Flyway migrations, semantic version bumps, and API changelogs directly from ontology diffs. When the TTL changes, `ggen sync` produces the migration SQL, the updated version number, and the c...

---

*This chapter was generated by ggen sync from the java26-patterns.ttl ontology.*
*Pattern 5 of 35 in "A Pattern Language for Java 26 with ggen"*
