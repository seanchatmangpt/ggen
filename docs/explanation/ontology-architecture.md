<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Explanation: The Ontology Architecture in ggen](#explanation-the-ontology-architecture-in-ggen)
  - [Why Open Ontologies?](#why-open-ontologies)
  - [The Generation Pipeline (`ggen ontology generate`)](#the-generation-pipeline-ggen-ontology-generate)
  - [Template Engine v2 (RDF / SPARQL Integration)](#template-engine-v2-rdf--sparql-integration)
  - [Semantic Persistence (`RdfMapper`)](#semantic-persistence-rdfmapper)
  - [Strict Governance via the Control Plane](#strict-governance-via-the-control-plane)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Explanation: The Ontology Architecture in ggen

This document explains the architectural decisions behind using Open Ontologies within the `ggen` ecosystem, specifically focusing on how semantic technologies power the CLI, code generation, and the internal marketplace registry.

## Why Open Ontologies?

Traditional software systems often lock metadata and domain models into rigid, relational databases or implicit code structures. By adopting Open Ontologies (using RDF and OWL), the `ggen` ecosystem treats data models as first-class, standardized, and universally queryable graphs.

This provides several benefits:
1. **Interoperability:** Data can be natively integrated with external semantic web systems.
2. **Schema Evolution:** Properties and classes can be added incrementally without running disruptive database migrations.
3. **Graph Analysis:** Complex relationships (like deeply nested package dependencies) can be elegantly solved with graph traversals (SPARQL) rather than expensive relational joins.

## The Generation Pipeline (`ggen ontology generate`)

The CLI offers commands to map between pure RDF definitions (`.ttl` files) and native programming constructs.

When an ontology schema is processed:
1. **Extraction:** The RDF schema is parsed (often mapped using `oxigraph` internally).
2. **Validation:** The relationships are checked for domain/range violations (e.g., ensuring a `price` property is attached to a `Product` class).
3. **Transpilation:** The system identifies `owl:Class` and translates it to a `struct` or `class`. It maps `owl:DatatypeProperty` fields to strongly-typed primitives (e.g., `xsd:string` maps to Rust's `String`).

This pipeline guarantees that the single source of truth—the ontology—remains synced perfectly with the application code.

## Template Engine v2 (RDF / SPARQL Integration)

Going beyond simple structure generation, the **v2 Template Engine** seamlessly bridges Tera templates and semantic datasets. Unlike traditional code generation where the Rust backend must manually query data and serialize it into context, the v2 engine permits templates to declare their own inline SPARQL queries inside the frontmatter. The engine handles the RDF extraction, executes the SPARQL queries against a unified in-memory graph, and makes the results available directly inside the template loop variables (e.g., `{{ sparql_results.classes }}`). This dramatically simplifies data-driven generation and empowers template authors.

## Semantic Persistence (`RdfMapper`)

Instead of ORMs translating objects to SQL, the `ggen-marketplace` relies on bidirectional mapping to RDF Triples. 

The `RdfMapper` module converts an in-memory domain model (like a `Package`) into a series of discrete facts, or "triples" (Subject -> Predicate -> Object). 

For example, when a package is saved:
1. It is assigned a Subject URI: `<https://ggen.io/marketplace/packages/my-lib>`.
2. A Predicate URI defines the attribute: `<https://ggen.io/marketplace/name>`.
3. The Object holds the literal: `"my-lib"`.

This abstraction uses the robust `oxigraph` engine. To rebuild the struct, the mapper dispatches a SPARQL query that retrieves these distinct facts and merges them back into the native Rust domain model, enabling a completely schema-less yet strictly typed backend architecture.

## Strict Governance via the Control Plane

While the graph model enables extreme flexibility, the marketplace enforces strict governance over its contents via the `RdfControlPlane`. 

Directly mutating the graph is highly discouraged. Instead, operations pass through the control plane where they undergo:
- **Compile-Time Safety:** Constructing queries via POKA YOKE builders ensures invalid URIs cannot be formed.
- **FMEA Mitigation:** A security firewall intercepts any raw SPARQL queries and aborts operations containing suspicious patterns (e.g. `DROP GRAPH`).
- **Deterministic State Machines:** Rather than arbitrary updates, resources must transition through rigorous states (e.g., `draft` -> `publish`). Structural SHACL validations are evaluated dynamically before any state transition completes.
