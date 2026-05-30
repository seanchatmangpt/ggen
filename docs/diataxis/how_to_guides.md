<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How-To Guides: State Management and Provenance with `ggen-graph`](#how-to-guides-state-management-and-provenance-with-ggen-graph)
  - [1. Writing Custom SPARQL ASK/CONSTRUCT Triggers and Actions](#1-writing-custom-sparql-askconstruct-triggers-and-actions)
    - [Problem](#problem)
    - [Solution](#solution)
      - [Writing an ASK Trigger](#writing-an-ask-trigger)
      - [Writing a CONSTRUCT Action](#writing-a-construct-action)
  - [2. Registering and Executing Validation Hooks](#2-registering-and-executing-validation-hooks)
    - [Problem](#problem-1)
    - [Solution](#solution-1)
  - [3. Verifying Cryptographic Transition Receipts](#3-verifying-cryptographic-transition-receipts)
    - [Problem](#problem-2)
    - [Solution](#solution-2)
  - [4. Projecting OCEL/PROV Evidence](#4-projecting-ocelprov-evidence)
    - [Problem](#problem-3)
    - [Solution](#solution-3)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How-To Guides: State Management and Provenance with `ggen-graph`

This document provides step-by-step recipes for standard tasks in the `ggen-graph` crate. Each guide is problem-oriented, practical, and includes complete, production-ready examples using `DeterministicGraph`, `RdfDelta`, `KnowledgeHook`, and `TransitionReceipt`.

---

## 1. Writing Custom SPARQL ASK/CONSTRUCT Triggers and Actions

### Problem
You want to implement automated graph triggers:
1. **Triggers (ASK):** Detect if a constraint is violated or if a condition is met (e.g., detecting cyclic dependencies).
2. **Actions (CONSTRUCT):** Materialize new relationships or project derived schema triples based on existing state (e.g., inferring transitively closed relationships).

### Solution

#### Writing an ASK Trigger
An ASK query returns a boolean. If your query searches for invalid states, returning `true` indicates a violation.

```rust
use ggen_graph::{DeterministicGraph, GraphError};

fn check_for_cycles(graph: &DeterministicGraph) -> Result<bool, GraphError> {
    // This query returns true if there is a cycle where a package depends on itself,
    // either directly or transitively.
    let query_str = r#"
        PREFIX ex: <http://example.org/>
        ASK {
            ?a ex:dependsOn+ ?a .
        }
    "#;

    let results = graph.query(query_str)?;
    match results {
        oxigraph::sparql::QueryResults::Boolean(has_cycles) => Ok(has_cycles),
        _ => Err(GraphError::Serialization("Expected boolean results from ASK query".to_string())),
    }
}
```

#### Writing a CONSTRUCT Action
A CONSTRUCT query projects a new set of RDF triples matching the template specified.

```rust
use ggen_graph::{DeterministicGraph, GraphError};
use oxigraph::model::Quad;

fn materialize_transitive_dependencies(graph: &DeterministicGraph) -> Result<Vec<Quad>, GraphError> {
    // CONSTRUCT matches transitive paths and outputs explicit direct assertions
    let query_str = r#"
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
            ?a ex:hasTransitiveDependency ?c .
        } WHERE {
            ?a ex:dependsOn+ ?c .
        }
    "#;

    let results = graph.query(query_str)?;
    match results {
        oxigraph::sparql::QueryResults::Graph(triple_iter) => {
            let mut quads = Vec::new();
            for triple_res in triple_iter {
                let triple = triple_res.map_err(|e| GraphError::Serialization(e.to_string()))?;
                // Convert triple to quad in the default graph
                quads.push(Quad {
                    subject: triple.subject,
                    predicate: triple.predicate,
                    object: triple.object,
                    graph_name: oxigraph::model::GraphName::DefaultGraph,
                });
            }
            Ok(quads)
        }
        _ => Err(GraphError::Serialization("Expected triple graph results from CONSTRUCT query".to_string())),
    }
}
```

---

## 2. Registering and Executing Validation Hooks

### Problem
You want to guarantee graph integrity invariants (e.g., ensuring no two packages share the same ID) before applying modifications, automatically rolling back the transition if validation fails.

### Solution
Use `KnowledgeHook` combined with `DeterministicGraph::apply_delta`.

```rust
use ggen_graph::{DeterministicGraph, RdfDelta, KnowledgeHook, GraphError};

fn apply_safe_transition(
    graph: &DeterministicGraph,
    delta: &RdfDelta,
) -> Result<(), GraphError> {
    // 1. Define the invariant: No package may have multiple names
    // If this ASK query returns true, it means a violation exists, but KnowledgeHook
    // treats SELECT returning empty OR ASK returning false/true based on execution logic.
    // Let's specify a constraint: ASK query returns true when the state is valid.
    // Here we query that no entity has two distinct names:
    let query_str = r#"
        ASK {
            FILTER NOT EXISTS {
                ?pkg <http://example.org/name> ?name1 .
                ?pkg <http://example.org/name> ?name2 .
                FILTER(?name1 != ?name2)
            }
        }
    "#;

    let hook = KnowledgeHook::new(
        "single_name_invariant".to_string(),
        query_str.to_string(),
    );

    // 2. Apply delta with active hooks
    // If the hook executes and returns false, apply_delta automatically rolls back.
    let receipt = graph.apply_delta(delta, &[hook])?;
    
    println!("Transition applied. Post-state hash: {:?}", receipt.post_state_hash);
    Ok(())
}
```

> [!IMPORTANT]
> The validation engine rolls back changes in memory by applying the reverse operations if any registered `KnowledgeHook` evaluates to `false`. Ensure your queries return `true` for a **valid** state and `false` for an **invalid** state.

---

## 3. Verifying Cryptographic Transition Receipts

### Problem
You need to verify the authenticity of a graph state transition to guarantee that the transition history has not been tampered with.

### Solution
Extract the `TransitionReceipt` and run its `.verify()` method. If the hashes or timestamps have been altered, the check fails.

```rust
use ggen_graph::{TransitionReceipt, GraphError};

pub fn verify_historical_receipt(receipt: &TransitionReceipt) -> Result<(), GraphError> {
    // 1. Call verify() to run the cryptographic validity checks
    match receipt.verify() {
        Ok(()) => {
            println!("Receipt is authentic.");
            println!("Timestamp: {}", receipt.timestamp.to_rfc3339());
            println!("Pre-state:  {:x?}", receipt.pre_state_hash);
            println!("Post-state: {:x?}", receipt.post_state_hash);
            println!("Delta hash: {:x?}", receipt.delta_hash);
            Ok(())
        }
        Err(e) => {
            eprintln!("Cryptographic tampering detected: {}", e);
            Err(e)
        }
    }
}
```

> [!WARNING]
> A receipt only verifies internal alignment of the transition details (pre-state, post-state, delta, timestamp, and signature). You should separately verify that the post-state hash of the actual graph matches the `post_state_hash` inside the receipt.

---

## 4. Projecting OCEL/PROV Evidence

### Problem
You want to output standardized W3C PROV-O (Provenance Ontology) or OCEL (Object-Centric Event Log) triples from transitions to compile a tamper-proof audit trail of the generation process.

### Solution
Translate `TransitionReceipt` fields into RDF statements matching PROV-O concepts like `prov:Activity`, `prov:Agent`, and `prov:Entity`.

```rust
use ggen_graph::{DeterministicGraph, TransitionReceipt, GraphError};
use oxigraph::model::{NamedNode, Quad, Literal, Term};

pub fn project_prov_evidence(
    graph: &DeterministicGraph,
    receipt: &TransitionReceipt,
    agent_uri: &str,
) -> Result<(), GraphError> {
    let prov_prefix = "http://www.w3.org/ns/prov#";
    let xsd_prefix = "http://www.w3.org/2001/XMLSchema#";
    let ex_prefix = "http://example.org/provenance/";

    // 1. Generate unique identifiers based on receipt hashes
    let hex_pre = hex::encode(receipt.pre_state_hash);
    let hex_post = hex::encode(receipt.post_state_hash);
    let hex_sig = hex::encode(receipt.signature_or_hash);

    let activity_uri = format!("{}activity/{}", ex_prefix, hex_sig);
    let pre_entity_uri = format!("{}state/{}", ex_prefix, hex_pre);
    let post_entity_uri = format!("{}state/{}", ex_prefix, hex_post);

    // 2. Define URIs
    let rdf_type = NamedNode::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#type").map_err(|e| GraphError::Serialization(e.to_string()))?;
    let prov_activity = NamedNode::new(format!("{}Activity", prov_prefix)).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let prov_entity = NamedNode::new(format!("{}Entity", prov_prefix)).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let prov_was_generated_by = NamedNode::new(format!("{}wasGeneratedBy", prov_prefix)).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let prov_used = NamedNode::new(format!("{}used", prov_prefix)).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let prov_was_associated_with = NamedNode::new(format!("{}wasAssociatedWith", prov_prefix)).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let prov_ended_at_time = NamedNode::new(format!("{}endedAtTime", prov_prefix)).map_err(|e| GraphError::Serialization(e.to_string()))?;

    let activity_node = NamedNode::new(&activity_uri).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let pre_node = NamedNode::new(&pre_entity_uri).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let post_node = NamedNode::new(&post_entity_uri).map_err(|e| GraphError::Serialization(e.to_string()))?;
    let agent_node = NamedNode::new(agent_uri).map_err(|e| GraphError::Serialization(e.to_string()))?;

    // 3. Construct quads
    let quads = vec![
        // Activity declaration
        Quad::new(activity_node.clone(), rdf_type.clone(), prov_activity.into(), oxigraph::model::GraphName::DefaultGraph),
        // Time binding
        Quad::new(
            activity_node.clone(),
            prov_ended_at_time,
            Term::Literal(Literal::new_typed_literal(
                receipt.timestamp.to_rfc3339(),
                NamedNode::new(format!("{}dateTime", xsd_prefix)).map_err(|e| GraphError::Serialization(e.to_string()))?
            )),
            oxigraph::model::GraphName::DefaultGraph
        ),
        // State declarations
        Quad::new(pre_node.clone(), rdf_type.clone(), prov_entity.clone().into(), oxigraph::model::GraphName::DefaultGraph),
        Quad::new(post_node.clone(), rdf_type.clone(), prov_entity.into(), oxigraph::model::GraphName::DefaultGraph),
        // Relationships
        Quad::new(activity_node.clone(), prov_used, pre_node.into(), oxigraph::model::GraphName::DefaultGraph),
        Quad::new(post_node.into(), prov_was_generated_by, activity_node.clone().into(), oxigraph::model::GraphName::DefaultGraph),
        Quad::new(activity_node.into(), prov_was_associated_with, agent_node.into(), oxigraph::model::GraphName::DefaultGraph),
    ];

    // 4. Insert projected statements into the graph
    for quad in quads {
        graph.insert_quad(&quad)?;
    }

    Ok(())
}
```
