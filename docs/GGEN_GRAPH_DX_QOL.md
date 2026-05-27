# Developer Experience & Quality of Life (DX/QOL)

This guide documents the design choices, utility interfaces, and developer workflows in the `ggen-graph` crate that enhance developer efficiency, enforce safety invariants, and streamline debugging.

---

## 1. Principles of ggen-graph DX

The developer experience in `ggen-graph` is built upon three core pillars:

1. **Deterministic State Accountability**: The entire graph state is reduced to a single, stable 32-byte `BLAKE3` hash. If two systems have the same triples, they have the same hash—regardless of triple insertion order.
2. **Crash-Safe Operations**: Raw Oxigraph operations can fail or panic. `ggen-graph` encapsulates these behind type-safe `Result<T, GraphError>` interfaces, enforcing `#![deny(clippy::panic)]` at compilation time.
3. **Implicit Rollback Safety**: Developers can apply arbitrary changes (via `RdfDelta`) without worrying about manually restoring states if validation fails. The transaction manager automatically rolls back changes when hooks reject them.

---

## 2. API Design & Common Patterns

### Computing and Applying Graph Deltas
Instead of manually tracking which quads were added or deleted, `ggen-graph` allows developers to compute the difference between two graphs and apply it to a baseline graph.

```rust
use ggen_graph::{DeterministicGraph, RdfDelta, KnowledgeHook};

fn update_knowledge_base(
    base: &DeterministicGraph,
    updated: &DeterministicGraph,
    validation_hooks: &[KnowledgeHook]
) -> Result<(), Box<dyn std::error::Error>> {
    // 1. Compute the delta
    let delta = RdfDelta::compute(base, updated)?;

    // 2. Apply delta and execute validation rules (e.g. SHACL-style ASK/SELECT checks)
    let receipt = base.apply_delta(&delta, validation_hooks)?;

    // 3. Verify receipt integrity before externalizing the state change
    receipt.verify()?;
    
    println!("Graph successfully transitioned! Receipt signature: {:?}", receipt.signature_or_hash);
    Ok(())
}
```

### Writing Effective SPARQL Validation Hooks
`KnowledgeHook` structures query the graph to assert correctness rules.

#### Example 1: Schema Constraint (ASK Query)
Ensures that all subjects representing an agent have an explicit class association:
```rust
let hook = KnowledgeHook::new(
    "require_agent_class".to_string(),
    r#"
    ASK WHERE {
        ?agent <http://www.w3.org/ns/prov#wasAssociatedWith> ?activity .
        FILTER NOT EXISTS { ?agent a <http://www.w3.org/ns/prov#Agent> }
    }
    "#.to_string()
);
```

#### Example 2: Negative Constraint (SELECT Query)
Finds invalid data transitions. Returning any solution triggers a rollback:
```rust
let hook = KnowledgeHook::new(
    "no_orphaned_tasks".to_string(),
    r#"
    SELECT ?task WHERE {
        ?task a <http://ggen.dev/ontology#Task> .
        FILTER NOT EXISTS { ?task <http://ggen.dev/ontology#taskAssignedTo> ?agent }
    }
    "#.to_string()
);
```

---

## 3. Diagnostics and Troubleshooting

### 1. `GraphError::HookFailed`
* **Symptom**: `KnowledgeHook validation failed. State rolled back.`
* **Cause**: One of the supplied SPARQL validation queries returned `false` (for `ASK`) or produced result rows (for `SELECT`).
* **Resolution**: Evaluate the SPARQL query against the post-delta target graph state. Ensure all query namespaces are properly written.

### 2. `GraphError::VerificationFailed`
* **Symptom**: `Cryptographic signature mismatch on TransitionReceipt`
* **Cause**: The timestamp, delta, or state hashes in the receipt were mutated or reconstructed using fake/unauthenticated values.
* **Resolution**: Never construct receipts manually using dummy fields. Always derive them via `apply_delta` or `TransitionReceipt::new`.
