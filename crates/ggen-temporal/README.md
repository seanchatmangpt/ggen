# ggen-temporal

Temporal logic reasoning engine for 4D ontologies with event sourcing, time-travel debugging, and distributed causal consistency.

## Overview

`ggen-temporal` extends the ggen code generation framework with powerful temporal reasoning capabilities, treating time as a first-class dimension in knowledge graphs.

## Features

### 🕐 4D Ontology Support
Extend RDF graphs with temporal dimensions, enabling queries across time:

```rust
use ggen_temporal::ontology_4d::*;

let mut graph = TemporalGraph::new()?;

// Add temporal triple
let triple = TemporalTriple::new(
    "ex:entity".to_string(),
    "ex:property".to_string(),
    "\"value\"".to_string(),
    ValidTime::TimeRange(TimeRange {
        start: Utc::now(),
        end: None,
    }),
);

graph.insert_temporal_triple(triple)?;

// Query at specific time
let results = graph.query_at_time(
    "SELECT ?o WHERE { ex:entity ex:property ?o }",
    Utc::now()
)?;
```

### 📜 Event Sourcing with Chrono-Semantic Versioning
Immutable event log combining temporal and semantic versioning:

```rust
use ggen_temporal::event_sourcing::*;

let mut store = EventStore::new();
let mut clock = VectorClock::new("node-1".to_string());

clock.tick();
let event = Event::new(
    "entity-123".to_string(),
    EventType::Created,
    EventData::GraphDelta { added: vec![], removed: vec![] },
    clock.timestamp(),
).with_version(ChronoSemanticVersion::new(1, 0, 0));

store.append(event)?;
```

### ⏱️ Bidirectional Time-Travel Debugging
Navigate through code generation history:

```rust
use ggen_temporal::time_travel::*;

let mut debugger = TimeTravelDebugger::new(event_store);

// Navigate history
debugger.step_forward().await?;
debugger.step_backward().await?;
debugger.goto_event("evt_123").await?;

// Checkpoints
let checkpoint = debugger.create_checkpoint("important_state".to_string())?;
debugger.restore_checkpoint(&checkpoint).await?;

// Undo/redo navigation
debugger.undo_navigation().await?;
debugger.redo_navigation().await?;
```

### 🔗 Vector Clocks for Causal Consistency
Track causality in distributed code generation:

```rust
use ggen_temporal::vector_clock::*;

let mut clock_a = VectorClock::new("node-a".to_string());
let mut clock_b = VectorClock::new("node-b".to_string());

// Node A performs action
clock_a.tick();
let time_a = clock_a.timestamp();

// Node B receives and processes
clock_b.merge(&time_a);
clock_b.tick();

// Verify causality
assert!(time_a.happened_before(&clock_b.timestamp()));
```

### 🔍 Temporal Logic Operators
Express and verify temporal properties:

```rust
use ggen_temporal::temporal_logic::*;

// "Always eventually stabilizes"
let formula = TemporalFormula::always(
    TemporalFormula::eventually(
        TemporalFormula::predicate("stabilized")
    )
);

let reasoner = TemporalReasoner::new(predicate_evaluator);
let result = reasoner.evaluate(&formula, &event_stream)?;

// Check invariants and liveness
assert!(reasoner.check_invariant("safe", &stream)?);
assert!(reasoner.check_liveness("completes", &stream)?);
```

### 🌐 Distributed Semantic Projections
Maintain materialized views with causal guarantees:

```rust
use ggen_temporal::semantic_projection::*;

let mut node = ProjectionNode::new("node-1".to_string(), event_store)?;

// Create projection
let projection = SemanticProjection::new(
    "user_view".to_string(),
    "SELECT ?user ?name WHERE { ?user a :User ; :name ?name }".to_string()
);

node.add_projection(projection);

// Process events with causal consistency
node.process_events().await?;

// Sync with peers
node.sync_with_peer(peer_id, peer_time).await?;
```

## Architecture

### Components

1. **Event Sourcing** (`event_sourcing`)
   - Immutable event log
   - Chrono-semantic versioning
   - Causal dependency tracking

2. **Vector Clocks** (`vector_clock`)
   - Distributed causality tracking
   - Happened-before relationships
   - Concurrency detection

3. **Temporal Logic** (`temporal_logic`)
   - LTL operators (Always, Eventually, Until, etc.)
   - Formula evaluation
   - Invariant and liveness checking

4. **4D Ontology** (`ontology_4d`)
   - Temporal RDF triples
   - Time-aware SPARQL queries
   - Valid time specifications

5. **Time Travel** (`time_travel`)
   - Bidirectional navigation
   - State reconstruction
   - Checkpoint management

6. **Semantic Projections** (`semantic_projection`)
   - Materialized views
   - Causal consistency guarantees
   - Distributed coordination

## Use Cases

### Code Generation History
Track every change to generated code with full temporal context:
- What changed?
- When did it change?
- Why did it change? (semantic delta)
- What caused the change? (causal dependencies)

### Distributed Code Generation
Multiple agents generating code cooperatively:
- Maintain causal consistency
- Avoid conflicts
- Ensure convergence

### Time-Travel Debugging
Debug code generation issues by exploring history:
- Step through generation events
- Identify when bugs were introduced
- Understand causal chains

### Temporal Queries
Query code artifacts across time:
- "What dependencies did package X have on January 1st?"
- "Show all breaking changes in the last month"
- "Find when this API was introduced"

### Compliance and Auditing
Full audit trail of all code generation:
- Immutable event log
- Temporal provenance
- Reproducible builds

## Examples

See the `examples/` directory for comprehensive examples:

- `basic_usage.rs` - Getting started guide
- `time_travel_demo.rs` - Time-travel debugging walkthrough
- `distributed_generation.rs` - Multi-node code generation
- `temporal_queries.rs` - Querying across time

## Testing

```bash
# Run all tests
cargo test -p ggen-temporal

# Run with logging
RUST_LOG=debug cargo test -p ggen-temporal

# Run specific test
cargo test -p ggen-temporal test_vector_clock
```

## Contributing

Contributions are welcome! Please see the main project [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

## License

MIT - see [LICENSE](../../LICENSE) for details.

## Related Projects

- [ggen-core](../ggen-core) - Core code generation engine
- [ggen-domain](../ggen-domain) - Domain logic layer
- [Oxigraph](https://github.com/oxigraph/oxigraph) - RDF graph database

## References

- [Linear Temporal Logic (LTL)](https://en.wikipedia.org/wiki/Linear_temporal_logic)
- [Vector Clocks](https://en.wikipedia.org/wiki/Vector_clock)
- [Event Sourcing](https://martinfowler.com/eaaDev/EventSourcing.html)
- [W3C Time Ontology](https://www.w3.org/TR/owl-time/)
