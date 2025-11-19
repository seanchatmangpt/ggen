//! Bayesian inference over semantic graphs
//!
//! This example demonstrates Bayesian inference on semantic graphs
//! with belief propagation and probabilistic reasoning.

use ggen_prob::{
    bayesian::{BayesianInference, BayesianNetwork, BeliefNode},
    graph::{BeliefPropagation, NodeBelief, ProbabilisticEdge, ProbabilisticGraph, ProbabilisticGraphBuilder},
    ontology::{ProbabilisticOntology, UncertainRelation, RelationSource},
    sparql::{ProbabilisticQuery, ProbabilisticSparql, QueryOptions},
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Bayesian Semantic Graphs Example ===\n");

    // Example 1: Build a Bayesian Network
    println!("1. Bayesian Network Construction");
    println!("-".repeat(40));

    let mut network = BayesianNetwork::new();

    // Add nodes representing beliefs
    let evidence = network.add_node(BeliefNode::new("has_quotes", 0.8));
    let hypothesis1 = network.add_node(BeliefNode::new("is_string", 0.5));
    let hypothesis2 = network.add_node(BeliefNode::new("is_text", 0.3));

    // Connect nodes (evidence supports hypotheses)
    network.add_edge(evidence, hypothesis1);
    network.add_edge(hypothesis1, hypothesis2); // String is a type of Text

    println!("Network structure:");
    println!("  has_quotes (0.8) -> is_string (0.5) -> is_text (0.3)");

    // Perform belief propagation
    println!("\nBefore propagation:");
    for node in &network.nodes {
        println!("  {}: {:.3}", node.name, node.belief);
    }

    network.propagate_beliefs(10);

    println!("\nAfter belief propagation:");
    for node in &network.nodes {
        println!("  {}: {:.3}", node.name, node.belief);
    }

    // Example 2: Probabilistic Graph with Uncertain Relations
    println!("\n\n2. Probabilistic Semantic Graph");
    println!("-".repeat(40));

    let mut graph = ProbabilisticGraph::new();

    // Add nodes with beliefs
    graph.add_node("Person", NodeBelief::new(0.9));
    graph.add_node("Student", NodeBelief::new(0.7));
    graph.add_node("Entity", NodeBelief::new(0.95));
    graph.add_node("Agent", NodeBelief::new(0.8));

    // Add uncertain relationships
    graph.add_edge(ProbabilisticEdge::new("Student", "Person", "subClassOf", 0.95));
    graph.add_edge(ProbabilisticEdge::new("Person", "Entity", "subClassOf", 0.90));
    graph.add_edge(ProbabilisticEdge::new("Person", "Agent", "subClassOf", 0.85));

    println!("Graph structure:");
    println!("  Student -> Person (0.95)");
    println!("  Person -> Entity (0.90)");
    println!("  Person -> Agent (0.85)");

    // Perform belief propagation on the graph
    let mut bp = BeliefPropagation::new(graph.clone());
    let iterations = bp.run(100, 1e-6);

    println!("\nBelief propagation converged in {} iterations", iterations);
    println!("\nNode beliefs after propagation:");
    for (name, node) in &bp.graph().nodes {
        println!("  {}: prior={:.3}, belief={:.3}", name, node.prior, node.belief);
    }

    // Example 3: Build Graph from Ontology Relations
    println!("\n\n3. Graph from Ontology Relations");
    println!("-".repeat(40));

    let relations = vec![
        UncertainRelation::new("Student", "subClassOf", "Person", 0.95)
            .with_source(RelationSource::Inference { method: "reasoning".to_string() }),
        UncertainRelation::new("Person", "subClassOf", "Entity", 0.90)
            .with_source(RelationSource::Manual),
        UncertainRelation::new("Teacher", "subClassOf", "Person", 0.92)
            .with_source(RelationSource::Inference { method: "reasoning".to_string() }),
    ];

    let graph = ProbabilisticGraphBuilder::new()
        .from_relations(&relations)
        .build();

    println!("Built graph from {} relations", relations.len());
    println!("Graph contains {} nodes and {} edges", graph.nodes.len(), graph.edges.len());

    for edge in &graph.edges {
        println!(
            "  {} --[{}]({:.2})--> {}",
            edge.source, edge.label, edge.probability, edge.target
        );
    }

    // Example 4: Probabilistic SPARQL Queries
    println!("\n\n4. Probabilistic SPARQL Queries");
    println!("-".repeat(40));

    let mut ontology = ProbabilisticOntology::new("university");

    // Add entities with type beliefs
    ontology.add_type_belief("john", "Student", 0.95);
    ontology.add_type_belief("mary", "Teacher", 0.92);
    ontology.add_type_belief("bob", "Person", 0.70);
    ontology.add_type_belief("alice", "Student", 0.88);

    // Add relations
    ontology.add_relation(
        UncertainRelation::new("john", "knows", "mary", 0.85)
    );
    ontology.add_relation(
        UncertainRelation::new("alice", "knows", "john", 0.90)
    );

    // Execute probabilistic SPARQL query
    let sparql = ProbabilisticSparql::new(ontology);
    let query = ProbabilisticQuery::new("SELECT ?entity ?type WHERE { ?entity rdf:type ?type }")
        .with_min_confidence(0.8)
        .with_options(QueryOptions {
            limit: Some(10),
            sort_by_confidence: true,
            ..Default::default()
        });

    let result = sparql.execute(&query);

    println!("\nQuery: SELECT entities with confidence >= 0.8");
    println!("Results (sorted by confidence):");
    for binding in &result.bindings {
        if let (Some(entity), Some(type_name)) = (
            binding.bindings.get("entity"),
            binding.bindings.get("type"),
        ) {
            println!("  {} rdf:type {} (confidence: {:.2})", entity, type_name, binding.confidence);
        }
    }

    println!("\nQuery metadata:");
    println!("  Total results: {}", result.metadata.result_count);
    println!("  Average confidence: {:.3}", result.metadata.average_confidence);
    println!("  Min confidence: {:.3}", result.metadata.min_confidence);
    println!("  Max confidence: {:.3}", result.metadata.max_confidence);

    // Example 5: Hybrid Inference
    println!("\n\n5. Hybrid Bayesian-Graph Inference");
    println!("-".repeat(40));

    let mut network = BayesianNetwork::new();
    let n1 = network.add_node(BeliefNode::new("data_quality", 0.85));
    let n2 = network.add_node(BeliefNode::new("type_consistency", 0.75));
    let n3 = network.add_node(BeliefNode::new("confidence", 0.5));

    network.add_edge(n1, n3);
    network.add_edge(n2, n3);

    let mut inference = BayesianInference::new(network);
    let mut evidence = std::collections::HashMap::new();
    evidence.insert("data_quality".to_string(), 0.9);
    evidence.insert("type_consistency".to_string(), 0.8);

    let posteriors = inference.infer(evidence);

    println!("Evidence:");
    println!("  data_quality = 0.9");
    println!("  type_consistency = 0.8");
    println!("\nPosterior beliefs:");
    for (var, prob) in &posteriors {
        println!("  {}: {:.3}", var, prob);
    }

    println!("\n=== Example Complete ===");

    Ok(())
}
