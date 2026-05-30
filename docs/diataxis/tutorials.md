<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: State Transitions in `ggen-graph`](#tutorial-state-transitions-in-ggen-graph)
  - [Prerequisites](#prerequisites)
  - [Step 1: Initialize the Deterministic Graph and Load a Dataset](#step-1-initialize-the-deterministic-graph-and-load-a-dataset)
  - [Step 2: Define and Load a Knowledge Hook Pack](#step-2-define-and-load-a-knowledge-hook-pack)
  - [Step 3: Execute a Valid State Transition (Applying Deltas)](#step-3-execute-a-valid-state-transition-applying-deltas)
  - [Step 4: Verify Cryptographic Integrity and Falsifiability](#step-4-verify-cryptographic-integrity-and-falsifiability)
  - [Step 5: Handling Validation Failures and Rollbacks](#step-5-handling-validation-failures-and-rollbacks)
  - [Complete Example Program](#complete-example-program)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Tutorial: State Transitions in `ggen-graph`

This tutorial guides you step-by-step through using `ggen-graph` to perform deterministic RDF graph updates, enforce safety checks via knowledge hook validation, and verify the resulting state transitions using cryptographic receipts.

By the end of this tutorial, you will know how to:
1. Initialize a deterministic RDF graph.
2. Load an initial RDF dataset into the graph.
3. Define and load a knowledge hook pack for validation.
4. Execute a state transition by applying an RDF delta.
5. Retrieve and verify the cryptographic integrity of a transition receipt.
6. Trigger and handle automatic state rollbacks on validation failure.

---

## Prerequisites

To follow this tutorial, you need to add `ggen-graph` and its core dependencies to your Rust project.

Create a new binary project:
```bash
cargo new ggen-tutorial --bin
cd ggen-tutorial
```

Update your `Cargo.toml` with the following dependencies (ensuring exact compatible versions):
```toml
[package]
name = "ggen-tutorial"
version = "0.1.0"
edition = "2021"

[dependencies]
ggen-graph = { path = "../ggen/crates/ggen-graph" } # Adjust path to your local ggen checkout
oxigraph = "0.4.0-alpha.5"
chrono = { version = "0.4", features = ["serde"] }
blake3 = "1.5"
serde = { version = "1.0", features = ["derive"] }
tokio = { version = "1.35", features = ["full"] }
```

---

## Step 1: Initialize the Deterministic Graph and Load a Dataset

The base structure of the library is `DeterministicGraph`. It wraps Oxigraph's storage, ensuring all quad listings are sorted deterministically so that state hashes are completely reproducible.

Let's start by initializing a graph and parsing N-Quads to insert them. Open `src/main.rs` and replace its content with the following:

```rust
use ggen_graph::{DeterministicGraph, GraphError};

fn main() -> Result<(), GraphError> {
    // 1. Initialize the empty deterministic graph
    let graph = DeterministicGraph::new()?;
    println!("Successfully initialized an empty DeterministicGraph.");

    // 2. Define our initial RDF triples/quads in N-Quads format
    let alice_quad_str = "<http://example.org/alice> <http://example.org/name> \"Alice\" <http://example.org/graph> .";
    let bob_quad_str = "<http://example.org/bob> <http://example.org/name> \"Bob\" <http://example.org/graph> .";

    // 3. Parse the strings into Oxigraph Quad structures
    let alice_quad = DeterministicGraph::parse_nquad(alice_quad_str)?;
    let bob_quad = DeterministicGraph::parse_nquad(bob_quad_str)?;

    // 4. Insert the quads into the graph
    graph.insert_quad(&alice_quad)?;
    graph.insert_quad(&bob_quad)?;
    println!("Inserted initial RDF dataset (Alice & Bob).");

    // 5. Compute the initial state hash
    let initial_hash = graph.state_hash()?;
    println!("Initial state hash (BLAKE3): {}", hex::encode(initial_hash));

    Ok(())
}
```

---

## Step 2: Define and Load a Knowledge Hook Pack

A **Knowledge Hook** allows us to enforce invariants or constraints on our graph. It runs a SPARQL query (`ASK` or `SELECT`) and expects a success criterion:
- An `ASK` query must evaluate to `true` to pass.
- A `SELECT` query must return **no solutions** (indicating no violations) to pass.

Let's define a hook pack to enforce two policies:
1. **At least one user named Bob must exist** (`ASK` query).
2. **No user named Charlie is allowed to exist** (`ASK` query checking negative existence).

Add the following to your `main.rs`:

```rust
use ggen_graph::{DeterministicGraph, GraphError, KnowledgeHook};

fn run_with_hooks() -> Result<(), GraphError> {
    let graph = DeterministicGraph::new()?;

    // Load initial data
    let alice_quad = DeterministicGraph::parse_nquad(
        "<http://example.org/alice> <http://example.org/name> \"Alice\" <http://example.org/graph> ."
    )?;
    let bob_quad = DeterministicGraph::parse_nquad(
        "<http://example.org/bob> <http://example.org/name> \"Bob\" <http://example.org/graph> ."
    )?;
    graph.insert_quad(&alice_quad)?;
    graph.insert_quad(&bob_quad)?;

    // Define hook 1: Bob must exist in the dataset
    let hook_must_have_bob = KnowledgeHook::new(
        "must_have_bob".to_string(),
        "ASK WHERE { ?person <http://example.org/name> \"Bob\" }".to_string()
    );

    // Define hook 2: Charlie must not exist in the dataset
    let hook_no_charlie = KnowledgeHook::new(
        "no_charlie".to_string(),
        "ASK WHERE { FILTER NOT EXISTS { ?person <http://example.org/name> \"Charlie\" } }".to_string()
    );

    // Group hooks into a validation hook pack
    let hook_pack = vec![hook_must_have_bob.clone(), hook_no_charlie.clone()];

    // Verify hooks execute successfully on the current state
    assert!(hook_must_have_bob.execute(&graph)?);
    assert!(hook_no_charlie.execute(&graph)?);
    println!("Knowledge hooks validated successfully against current state.");

    Ok(())
}
```

---

## Step 3: Execute a Valid State Transition (Applying Deltas)

To transition the state of our graph, we compute or specify an `RdfDelta`, which contains lists of additions and deletions as N-Quad strings. When we apply the delta via `apply_delta`, the runtime:
1. Records the pre-state hash.
2. Applies the deletions, then the additions.
3. Runs the validation hooks.
4. If successful, commits the transition and returns a `TransitionReceipt`.

Let's apply a valid delta that adds **Diana** to the graph while respecting all hooks:

```rust
use ggen_graph::{DeterministicGraph, GraphError, KnowledgeHook, RdfDelta, TransitionReceipt};

fn execute_transition() -> Result<(), GraphError> {
    let graph = DeterministicGraph::new()?;
    
    // Setup initial state
    let alice = DeterministicGraph::parse_nquad(
        "<http://example.org/alice> <http://example.org/name> \"Alice\" <http://example.org/graph> ."
    )?;
    let bob = DeterministicGraph::parse_nquad(
        "<http://example.org/bob> <http://example.org/name> \"Bob\" <http://example.org/graph> ."
    )?;
    graph.insert_quad(&alice)?;
    graph.insert_quad(&bob)?;

    // Setup hooks
    let hook_no_charlie = KnowledgeHook::new(
        "no_charlie".to_string(),
        "ASK WHERE { FILTER NOT EXISTS { ?person <http://example.org/name> \"Charlie\" } }".to_string()
    );
    let hooks = vec![hook_no_charlie];

    // Define delta: Add Diana, delete nobody
    let delta = RdfDelta::new(
        vec!["<http://example.org/diana> <http://example.org/name> \"Diana\" <http://example.org/graph> .".to_string()],
        vec![]
    );

    // Apply the delta and retrieve the receipt
    let receipt = graph.apply_delta(&delta, &hooks)?;
    println!("Transition executed successfully!");
    println!("Receipt Timestamp: {}", receipt.timestamp);
    println!("Pre-State Hash:  {}", hex::encode(receipt.pre_state_hash));
    println!("Post-State Hash: {}", hex::encode(receipt.post_state_hash));
    println!("Delta Hash:      {}", hex::encode(receipt.delta_hash));

    // Verify the receipt integrity
    receipt.verify()?;
    println!("Transition receipt verified cryptographically.");

    Ok(())
}
```

---

## Step 4: Verify Cryptographic Integrity and Falsifiability

Every `TransitionReceipt` binds the `pre_state_hash`, `post_state_hash`, `delta_hash`, and a `timestamp` into a single BLAKE3 `signature_or_hash` checksum. 

If any field of the receipt is tampered with, verifying it will immediately fail:

```rust
fn verify_receipt_falsifiability(receipt: &TransitionReceipt) -> Result<(), GraphError> {
    // 1. Verification of the unmodified receipt passes
    receipt.verify()?;
    println!("Original receipt verified.");

    // 2. Clone the receipt and tamper with the delta hash
    let mut tampered_receipt = receipt.clone();
    tampered_receipt.delta_hash[0] ^= 1; // Flip one bit

    // 3. Verification must fail
    match tampered_receipt.verify() {
        Err(GraphError::VerificationFailed(err_msg)) => {
            println!("Falsifiability test passed! Tampered receipt rejected: {}", err_msg);
        }
        _ => {
            panic!("Critical violation: Tampered receipt was incorrectly accepted!");
        }
    }

    Ok(())
}
```

---

## Step 5: Handling Validation Failures and Rollbacks

If a transition results in a state that violates any loaded validation hook, `apply_delta` automatically rolls back all graph modifications and aborts the transition, returning a `GraphError::HookFailed`.

Let's test this behavior by trying to apply a delta that adds **Charlie** (violating the `no_charlie` hook):

```rust
fn execute_invalid_transition() -> Result<(), GraphError> {
    let graph = DeterministicGraph::new()?;
    
    // Load initial Alice & Bob state
    let alice = DeterministicGraph::parse_nquad(
        "<http://example.org/alice> <http://example.org/name> \"Alice\" <http://example.org/graph> ."
    )?;
    let bob = DeterministicGraph::parse_nquad(
        "<http://example.org/bob> <http://example.org/name> \"Bob\" <http://example.org/graph> ."
    )?;
    graph.insert_quad(&alice)?;
    graph.insert_quad(&bob)?;
    
    let pre_hash = graph.state_hash()?;

    // Establish hook: Charlie must not exist
    let hook_no_charlie = KnowledgeHook::new(
        "no_charlie".to_string(),
        "ASK WHERE { FILTER NOT EXISTS { ?person <http://example.org/name> \"Charlie\" } }".to_string()
    );
    let hooks = vec![hook_no_charlie];

    // Define invalid delta adding Charlie
    let invalid_delta = RdfDelta::new(
        vec!["<http://example.org/charlie> <http://example.org/name> \"Charlie\" <http://example.org/graph> .".to_string()],
        vec![]
    );

    // Apply the invalid delta
    match graph.apply_delta(&invalid_delta, &hooks) {
        Err(GraphError::HookFailed(err)) => {
            println!("Transition blocked and rolled back: {}", err);
        }
        Ok(_) => {
            panic!("Critical violation: Hook validation failed to block invalid state!");
        }
        Err(other) => return Err(other),
    }

    // Verify the state was fully rolled back to the pre-transition state
    let post_hash = graph.state_hash()?;
    assert_eq!(pre_hash, post_hash);
    
    let charlie_quad = DeterministicGraph::parse_nquad(
        "<http://example.org/charlie> <http://example.org/name> \"Charlie\" <http://example.org/graph> ."
    )?;
    assert!(!graph.contains_quad(&charlie_quad)?);
    println!("Rollback verified. State remains untouched.");

    Ok(())
}
```

---

## Complete Example Program

Here is the complete, runnable Rust file that integrates all steps of this tutorial. Replace the contents of `src/main.rs` with the code below and run `cargo run` to see the entire pipeline in action:

```rust
use ggen_graph::{DeterministicGraph, GraphError, KnowledgeHook, RdfDelta};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Step 1: Initialize graph
    let graph = DeterministicGraph::new()?;
    let alice_str = "<http://example.org/alice> <http://example.org/name> \"Alice\" <http://example.org/graph> .";
    let bob_str = "<http://example.org/bob> <http://example.org/name> \"Bob\" <http://example.org/graph> .";
    let alice = DeterministicGraph::parse_nquad(alice_str)?;
    let bob = DeterministicGraph::parse_nquad(bob_str)?;
    graph.insert_quad(&alice)?;
    graph.insert_quad(&bob)?;
    println!("[1/5] Initialized graph with Alice and Bob.");

    // Step 2: Establish the hook pack
    let hook_no_charlie = KnowledgeHook::new(
        "no_charlie".to_string(),
        "ASK WHERE { FILTER NOT EXISTS { ?person <http://example.org/name> \"Charlie\" } }".to_string(),
    );
    let hooks = vec![hook_no_charlie];
    println!("[2/5] Hook pack initialized (no Charlie allowed).");

    // Step 3: Apply valid delta (adding Diana)
    let diana_str = "<http://example.org/diana> <http://example.org/name> \"Diana\" <http://example.org/graph> .";
    let valid_delta = RdfDelta::new(vec![diana_str.to_string()], vec![]);
    let receipt = graph.apply_delta(&valid_delta, &hooks)?;
    println!("[3/5] Transition executed successfully (added Diana).");

    // Step 4: Verify receipt integrity and falsifiability
    receipt.verify()?;
    println!("[4/5] Transition receipt verified successfully.");

    let mut tampered_receipt = receipt.clone();
    tampered_receipt.delta_hash[0] ^= 1;
    assert!(tampered_receipt.verify().is_err());
    println!("      Falsifiability check verified (tampered receipt rejected).");

    // Step 5: Test rollback with an invalid delta
    let charlie_str = "<http://example.org/charlie> <http://example.org/name> \"Charlie\" <http://example.org/graph> .";
    let invalid_delta = RdfDelta::new(vec![charlie_str.to_string()], vec![]);
    
    let pre_rollback_hash = graph.state_hash()?;
    match graph.apply_delta(&invalid_delta, &hooks) {
        Err(GraphError::HookFailed(e)) => {
            println!("[5/5] Hook validation rejected transition: {}", e);
        }
        _ => panic!("Expected hook validation failure"),
    }
    
    let post_rollback_hash = graph.state_hash()?;
    assert_eq!(pre_rollback_hash, post_rollback_hash);
    println!("      Rollback verified (state hash is unchanged).");

    Ok(())
}
```
