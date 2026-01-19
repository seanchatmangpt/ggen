---
name: rdf-ontologies
description: "Master RDF ontologies and Turtle (TTL) syntax for feature specifications, domain models, and architecture plans."
allowed_tools: "Read, Write, Edit, Bash(cargo make speckit:*)"
---

# RDF Ontologies Skill

## What is RDF?

**Resource Description Framework**: Semantic web standard for expressing knowledge as triples

```
Subject → Predicate → Object
Alice → hasEmail → alice@example.com
Feature-001 → hasTitle → "Test Audit"
```

## Turtle (TTL) Syntax

```turtle
@prefix spec: <http://ggen.io/spec/> .

spec:feature-001 a spec:Feature ;
    spec:title "Feature Title" ;
    spec:priority spec:P1 ;
    spec:hasUserStory spec:us-001 .

spec:us-001 a spec:UserStory ;
    spec:title "As a user..." ;
    spec:hasAcceptanceCriterion spec:ac-001 .
```

## Workflow

1. Create `.specify/specs/NNN-feature/*.ttl`
2. Validate: `cargo make speckit-validate`
3. Render markdown: `cargo make speckit-render`
4. TTL is source of truth (never edit .md)

## Reference
See CLAUDE.md sections:
- Development Workflow (Create Feature Specification)
- File Organization (.specify/)
- Agents: speckit-architect
