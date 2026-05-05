# Tutorial: KGC-4D Time-Travel and Temporal Reconstruction

This tutorial teaches you how to use the Knowledge Graph Compiler's 4D features (KGC-4D) to reconstruct historical states of the marketplace graph.

## Prerequisites
- You should have `ggen-marketplace` integrated into your project.
- Basic understanding of SPARQL and `xsd:dateTime` formats.

## Step 1: Create a Triple with 4D Metadata

When adding data to the graph, you must attach an "Observable" and temporal context. Use the `TripleBuilder` with the `with_kgc4d` method.

```rust
use ggen_marketplace::rdf::poka_yoke::{Triple, ResourceId, Literal, Kgc4dMetadata};

let package_id = ResourceId::new("https://ggen.io/packages/my-lib").unwrap();
let obs_id = ResourceId::new("https://ggen.io/observables/build-agent-01").unwrap();

let metadata = Kgc4dMetadata {
    observable: obs_id,
    timestamp: "2026-05-04T12:00:00Z".to_string(),
    vector_clock: "A:12,B:4".to_string(),
    commit_hash: "a1b2c3d4".to_string(),
};

let triple = Triple::builder()
    .subject(package_id)
    .predicate_from_property(Property::PackageName)
    .object_literal(Literal::String("My Library".to_string()))
    .with_kgc4d(metadata) // Attached 4D context
    .build();
```

## Step 2: Persist to the RdfControlPlane

Insert the triple through the control plane to ensure SHACL validation and Poka-Yoke safety.

```rust
let mut control_plane = RdfControlPlane::new("./config")?;
control_plane.add_triple(triple)?;
```

## Step 3: Perform Time-Travel Reconstruction

To see what the graph looked like at a specific point in time (ignoring all triples added after that timestamp), use `reconstruct_at_time`.

```rust
// Only retrieve triples valid as of 11:59 AM
let historical_graph = control_plane.reconstruct_at_time("2026-05-04T11:59:00Z")?;

println!("Graph State: {}", historical_graph);
```

## Summary
By tagging every semantic "fact" with KGC-4D metadata, you enable high-fidelity auditing and the ability to reproduce any software artifact's state exactly as it existed at the time of its generation.
