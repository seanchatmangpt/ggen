//! Comprehensive demonstration of the ggen-temporal system
//!
//! This example shows all major features:
//! - Event sourcing with chrono-semantic versioning
//! - Vector clocks and causal consistency
//! - Temporal logic reasoning
//! - 4D ontology queries
//! - Time-travel debugging
//! - Distributed semantic projections

use ggen_temporal::{
    event_sourcing::*,
    ontology_4d::*,
    semantic_projection::*,
    temporal_logic::*,
    time_travel::*,
    vector_clock::*,
};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 ggen-temporal Comprehensive Demo\n");

    // ========================================================================
    // Part 1: Event Sourcing with Chrono-Semantic Versioning
    // ========================================================================
    println!("📜 Part 1: Event Sourcing\n");

    let event_store = EventStore::new();
    let mut clock = VectorClock::new("code-generator".to_string());

    // Generate initial code
    clock.tick();
    let create_event = Event::new(
        "user-service".to_string(),
        EventType::Created,
        EventData::CodeDelta {
            file_path: "src/user.rs".to_string(),
            diff: "+ pub struct User { name: String }".to_string(),
        },
        clock.timestamp(),
    )
    .with_version(ChronoSemanticVersion::new(1, 0, 0))
    .with_semantic_delta(SemanticDelta::Feature {
        description: "Initial user service".to_string(),
    });

    let create_id = event_store.append(create_event)?;
    println!("✅ Created user service: {create_id}");

    // Add a feature
    clock.tick();
    let feature_event = Event::new(
        "user-service".to_string(),
        EventType::Updated,
        EventData::CodeDelta {
            file_path: "src/user.rs".to_string(),
            diff: "+ pub email: String".to_string(),
        },
        clock.timestamp(),
    )
    .with_version(ChronoSemanticVersion::new(1, 1, 0))
    .with_semantic_delta(SemanticDelta::Feature {
        description: "Add email field".to_string(),
    })
    .with_dependencies(vec![create_id.clone()]);

    let feature_id = event_store.append(feature_event)?;
    println!("✅ Added feature: {feature_id}");

    // Breaking change
    clock.tick();
    let breaking_event = Event::new(
        "user-service".to_string(),
        EventType::Updated,
        EventData::CodeDelta {
            file_path: "src/user.rs".to_string(),
            diff: "- pub name: String\n+ pub full_name: String".to_string(),
        },
        clock.timestamp(),
    )
    .with_version(ChronoSemanticVersion::new(2, 0, 0))
    .with_semantic_delta(SemanticDelta::Breaking {
        description: "Rename name to full_name".to_string(),
    })
    .with_dependencies(vec![feature_id.clone()]);

    event_store.append(breaking_event)?;
    println!("✅ Applied breaking change\n");

    // ========================================================================
    // Part 2: Vector Clocks and Causal Consistency
    // ========================================================================
    println!("🔗 Part 2: Vector Clocks & Causality\n");

    let mut clock_node_a = VectorClock::new("node-a".to_string());
    let mut clock_node_b = VectorClock::new("node-b".to_string());

    // Node A generates code
    clock_node_a.tick();
    let time_a1 = clock_node_a.timestamp();
    println!("Node A generated code: {time_a1}");

    // Node B receives notification and generates related code
    clock_node_b.merge(&time_a1);
    let time_b1 = clock_node_b.timestamp();
    println!("Node B generated code: {time_b1}");

    // Verify causality
    if time_a1.happened_before(&time_b1) {
        println!("✅ Causal relationship verified: A → B\n");
    }

    // ========================================================================
    // Part 3: Temporal Logic Reasoning
    // ========================================================================
    println!("🔍 Part 3: Temporal Logic\n");

    // Create a temporal reasoner
    let reasoner = TemporalReasoner::new(|event| {
        let mut predicates = HashMap::new();

        // Define predicates based on event type
        match event.event_type {
            EventType::Created => {
                predicates.insert("exists".to_string(), TruthValue::True);
                predicates.insert("stable".to_string(), TruthValue::False);
            }
            EventType::Updated => {
                predicates.insert("exists".to_string(), TruthValue::True);
                predicates.insert("stable".to_string(), TruthValue::False);
            }
            _ => {}
        }

        // Check semantic delta
        if let Some(ref delta) = event.semantic_delta {
            match delta {
                SemanticDelta::Breaking { .. } => {
                    predicates.insert("breaking".to_string(), TruthValue::True);
                }
                _ => {
                    predicates.insert("breaking".to_string(), TruthValue::False);
                }
            }
        }

        predicates
    });

    // Get event stream
    let stream = event_store.get_all_events();

    // Check if service always exists after creation
    let exists_formula = TemporalFormula::eventually(TemporalFormula::predicate("exists"));
    let exists_result = reasoner.evaluate(&exists_formula, &stream)?;
    println!("Eventually exists: {}", exists_result.is_true());

    // Check for breaking changes
    let breaking_formula = TemporalFormula::eventually(TemporalFormula::predicate("breaking"));
    let breaking_result = reasoner.evaluate(&breaking_formula, &stream)?;
    println!("Eventually has breaking change: {}\n", breaking_result.is_true());

    // ========================================================================
    // Part 4: 4D Ontology Queries
    // ========================================================================
    println!("🌐 Part 4: 4D Ontology\n");

    let mut temporal_graph = TemporalGraph::new()?;

    let now = chrono::Utc::now();
    let future = now + chrono::Duration::hours(24);

    // Add temporal triples
    let triple1 = TemporalTriple::new(
        "<http://example.org/UserService>".to_string(),
        "<http://example.org/hasVersion>".to_string(),
        "\"1.0.0\"".to_string(),
        ValidTime::TimeRange(TimeRange::new(now, Some(now + chrono::Duration::hours(12)))),
    );

    temporal_graph.insert_temporal_triple(triple1)?;

    let triple2 = TemporalTriple::new(
        "<http://example.org/UserService>".to_string(),
        "<http://example.org/hasVersion>".to_string(),
        "\"2.0.0\"".to_string(),
        ValidTime::TimeRange(TimeRange::new(
            now + chrono::Duration::hours(12),
            Some(future),
        )),
    );

    temporal_graph.insert_temporal_triple(triple2)?;

    println!("✅ Added temporal triples to graph");

    // Query at different times
    let query = "SELECT ?version WHERE { <http://example.org/UserService> <http://example.org/hasVersion> ?version }";

    let results_now = temporal_graph.query_at_time(query, now)?;
    println!("Version at start: {:?}", results_now);

    let results_later = temporal_graph.query_at_time(query, now + chrono::Duration::hours(18))?;
    println!("Version 18 hours later: {:?}\n", results_later);

    // ========================================================================
    // Part 5: Time-Travel Debugging
    // ========================================================================
    println!("⏱️ Part 5: Time-Travel Debugging\n");

    let mut debugger = TimeTravelDebugger::new(event_store.clone());

    // Navigate to start
    debugger.goto_start().await?;
    if let Some(event) = debugger.current_event() {
        println!("At start: {}", event.id);
    }

    // Step forward through history
    debugger.step_forward().await?;
    if let Some(event) = debugger.current_event() {
        println!("After step forward: {}", event.id);
    }

    // Create a checkpoint
    let checkpoint = debugger.create_checkpoint("before_breaking_change".to_string())?;
    println!("✅ Created checkpoint: {}", checkpoint.name);

    // Continue forward
    debugger.step_forward().await?;
    if let Some(event) = debugger.current_event() {
        println!("At breaking change: {}", event.id);
    }

    // Restore checkpoint
    debugger.restore_checkpoint(&checkpoint).await?;
    println!("✅ Restored checkpoint\n");

    // ========================================================================
    // Part 6: Distributed Semantic Projections
    // ========================================================================
    println!("🌍 Part 6: Distributed Projections\n");

    let node_store = EventStore::new();
    let mut projection_node = ProjectionNode::new("projection-node-1".to_string(), node_store)?;

    // Create a semantic projection
    let projection = SemanticProjection::new(
        "user_service_projection".to_string(),
        "SELECT ?s ?p ?o WHERE { ?s ?p ?o }".to_string(),
    );

    println!("Created projection: {}", projection.name);
    projection_node.add_projection(projection);

    // Process events
    projection_node.process_events().await?;
    println!("✅ Processed all events");

    // Show current vector time
    let current_time = projection_node.current_time();
    println!("Current vector time: {}\n", current_time);

    // ========================================================================
    // Summary
    // ========================================================================
    println!("✨ Demo Complete!\n");
    println!("Summary:");
    println!("  - {} events in store", event_store.len());
    println!("  - {} checkpoints created", debugger.checkpoints().len());
    println!("  - {} projections active", projection_node.get_projections().len());
    println!("\nAll temporal reasoning features demonstrated successfully! 🎉");

    Ok(())
}
