# ggen-prob: Probabilistic Programming Framework for Uncertain Ontologies

A comprehensive probabilistic programming framework for handling uncertainty in ontologies, semantic graphs, and type systems. This crate provides Bayesian inference, fuzzy logic, statistical type synthesis, and probabilistic SPARQL extensions for reasoning over uncertain knowledge graphs.

## Features

- **🎲 Bayesian Inference**: Apply Bayesian reasoning to semantic graphs and ontologies with belief propagation
- **🌫️ Fuzzy Logic**: Handle ambiguous domain modeling with fuzzy sets, membership functions, and fuzzy inference
- **📊 Statistical Type Synthesis**: Infer types from noisy real-world data sources with confidence quantification
- **📈 Confidence Intervals**: Generate code with uncertainty quantification and confidence scores
- **🔍 Probabilistic SPARQL**: Extended SPARQL queries with probability weights and confidence filtering
- **🕸️ Probabilistic Graphs**: Semantic graphs with uncertain edges and belief propagation algorithms

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
ggen-prob = "0.1.0"
```

## Quick Start

### Basic Type Inference with Confidence

```rust
use ggen_prob::{
    synthesis::{DataSample, FieldValue, StatisticalSynthesis, SynthesisConfig},
};

// Create synthesizer
let config = SynthesisConfig::default();
let mut synthesis = StatisticalSynthesis::new(config);

// Add data samples
for i in 0..20 {
    let sample = DataSample::new(format!("sample_{}", i))
        .with_field("user_id", FieldValue::String(format!("{}", 1000 + i)))
        .with_field("age", FieldValue::Integer(20 + i));

    synthesis.add_sample(sample);
}

// Infer types
let types = synthesis.synthesize_all();
for (field, ptype) in types {
    if let Some((type_name, prob)) = ptype.most_likely_type() {
        println!("{}: {} (confidence: {:.2})", field, type_name, prob);
    }
}
```

### Bayesian Inference on Semantic Graphs

```rust
use ggen_prob::{
    bayesian::{BayesianNetwork, BeliefNode},
    graph::{BeliefPropagation, ProbabilisticGraph, NodeBelief, ProbabilisticEdge},
};

// Create a probabilistic graph
let mut graph = ProbabilisticGraph::new();
graph.add_node("Student", NodeBelief::new(0.8));
graph.add_node("Person", NodeBelief::new(0.9));
graph.add_edge(ProbabilisticEdge::new("Student", "Person", "subClassOf", 0.95));

// Run belief propagation
let mut bp = BeliefPropagation::new(graph);
bp.run(100, 1e-6);

// Get updated beliefs
for (name, node) in &bp.graph().nodes {
    println!("{}: {:.3}", name, node.belief);
}
```

### Fuzzy Logic for Ambiguous Domains

```rust
use ggen_prob::{
    fuzzy::{FuzzyLogic, FuzzySet, FuzzyRule, MembershipFunction},
};
use std::collections::HashMap;

let mut fuzzy = FuzzyLogic::new();

// Define fuzzy sets
fuzzy.add_set(FuzzySet::new(
    "high_confidence",
    MembershipFunction::Triangular { a: 0.5, b: 0.75, c: 1.0 },
    (0.0, 1.0),
));

// Add fuzzy rules
fuzzy.add_rule(
    FuzzyRule::new()
        .if_condition("confidence", "high_confidence")
        .then_action("decision", "accept")
);

// Perform inference
let mut inputs = HashMap::new();
inputs.insert("confidence".to_string(), 0.8);
let outputs = fuzzy.infer(&inputs);
```

### Probabilistic SPARQL Queries

```rust
use ggen_prob::{
    ontology::ProbabilisticOntology,
    sparql::{ProbabilisticQuery, ProbabilisticSparql},
};

let mut ontology = ProbabilisticOntology::new("example");
ontology.add_type_belief("john", "Student", 0.95);
ontology.add_type_belief("mary", "Teacher", 0.92);

let sparql = ProbabilisticSparql::new(ontology);
let query = ProbabilisticQuery::new("SELECT ?entity ?type")
    .with_min_confidence(0.9);

let result = sparql.execute(&query);
for binding in result.bindings {
    println!("Confidence: {:.2}", binding.confidence);
}
```

## Architecture

The framework is organized into several modules:

```
ggen-prob/
├── bayesian.rs       # Bayesian networks and inference
├── confidence.rs     # Confidence intervals and scores
├── fuzzy.rs          # Fuzzy logic and fuzzy sets
├── graph.rs          # Probabilistic graphs and belief propagation
├── inference.rs      # Unified inference engine
├── ontology.rs       # Probabilistic ontology structures
├── sparql.rs         # Probabilistic SPARQL extensions
├── synthesis.rs      # Statistical type synthesis
└── types.rs          # Probabilistic type system
```

## Core Concepts

### Probabilistic Types

Types with probability distributions over possible type assignments:

```rust
let mut ptype = ProbabilisticType::new("user_id");
ptype.add_hypothesis("String", 0.95);
ptype.add_hypothesis("Integer", 0.05);

assert_eq!(ptype.most_likely_type().unwrap().0, "String");
assert_eq!(ptype.confidence(), 0.95);
```

### Confidence Scores

Multi-component confidence scoring with uncertainty quantification:

```rust
use ggen_prob::confidence::ConfidenceScoreBuilder;

let score = ConfidenceScoreBuilder::new()
    .component("data_quality", 0.85)
    .component("sample_size", 0.90)
    .component("consistency", 0.80)
    .build_equal();

println!("Overall: {:.2}, Uncertainty: {:.2}",
    score.overall, score.uncertainty());
```

### Uncertain Relations

Relationships between entities with confidence scores:

```rust
use ggen_prob::ontology::UncertainRelation;

let relation = UncertainRelation::new(
    "Student",
    "subClassOf",
    "Person",
    0.95
);
```

## Integration with ggen-core

This crate integrates seamlessly with `ggen-core`'s RDF/SPARQL infrastructure:

```rust
// Use with Oxigraph stores
// Extend SPARQL queries with probabilistic features
// Generate code with confidence annotations
```

## Examples

Run the included examples:

```bash
# Basic type inference
cargo run --example basic_inference

# Fuzzy logic for ambiguous domains
cargo run --example fuzzy_ontology

# Bayesian semantic graphs
cargo run --example bayesian_semantic_graphs
```

## Testing

The framework includes comprehensive tests:

```bash
# Run all tests
cargo test

# Run with output
cargo test -- --nocapture

# Run specific test module
cargo test --lib types::tests
```

## Performance

- **Belief Propagation**: O(E × I) where E = edges, I = iterations
- **Type Synthesis**: O(N × M) where N = samples, M = fields
- **Fuzzy Inference**: O(R × V) where R = rules, V = variables

Benchmarks available:

```bash
cargo bench
```

## Use Cases

1. **Type Inference from Messy Data**: Infer types from noisy CSV, JSON, or database dumps
2. **Ontology Learning**: Build ontologies from uncertain data sources
3. **Code Generation with Uncertainty**: Generate code with confidence annotations
4. **Semantic Graph Reasoning**: Reason over knowledge graphs with uncertain relationships
5. **Ambiguous Domain Modeling**: Model domains with fuzzy boundaries using fuzzy logic

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](../../CONTRIBUTING.md) for guidelines.

## License

This project is licensed under the MIT OR Apache-2.0 license.

## References

- Pearl, J. (1988). Probabilistic Reasoning in Intelligent Systems
- Zadeh, L. A. (1965). Fuzzy sets
- Bishop, C. M. (2006). Pattern Recognition and Machine Learning
- Koller, D., & Friedman, N. (2009). Probabilistic Graphical Models

## Version

Current version: 0.1.0

Built with ❤️ for the ggen ecosystem.
