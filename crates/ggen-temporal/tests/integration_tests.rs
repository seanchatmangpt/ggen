//! Integration tests for ggen-temporal
//!
//! These tests verify that all components work together correctly.

use ggen_temporal::{
    event_sourcing::*,
    ontology_4d::*,
    semantic_projection::*,
    temporal_logic::*,
    time_travel::*,
    vector_clock::*,
};
use std::collections::HashMap;

#[tokio::test]
async fn test_end_to_end_workflow() {
    // Create event store and vector clock
    let event_store = EventStore::new();
    let mut clock = VectorClock::new("test-node".to_string());

    // Create events
    clock.tick();
    let event1 = Event::new(
        "entity-1".to_string(),
        EventType::Created,
        EventData::Custom {
            data: serde_json::json!({"action": "create"}),
        },
        clock.timestamp(),
    )
    .with_version(ChronoSemanticVersion::new(1, 0, 0));

    let id1 = event_store.append(event1).expect("Failed to append event1");

    clock.tick();
    let event2 = Event::new(
        "entity-1".to_string(),
        EventType::Updated,
        EventData::Custom {
            data: serde_json::json!({"action": "update"}),
        },
        clock.timestamp(),
    )
    .with_version(ChronoSemanticVersion::new(1, 1, 0))
    .with_dependencies(vec![id1]);

    event_store.append(event2).expect("Failed to append event2");

    // Verify event count
    assert_eq!(event_store.len(), 2);

    // Get events for entity
    let entity_events = event_store.get_events_for_entity("entity-1");
    assert_eq!(entity_events.len(), 2);
}

#[tokio::test]
async fn test_time_travel_with_causal_consistency() {
    let event_store = EventStore::new();
    let mut clock = VectorClock::new("node-1".to_string());

    // Create a chain of causally related events
    clock.tick();
    let event1 = Event::new(
        "service-a".to_string(),
        EventType::Created,
        EventData::Custom {
            data: serde_json::json!({}),
        },
        clock.timestamp(),
    );
    let id1 = event_store.append(event1).unwrap();

    clock.tick();
    let event2 = Event::new(
        "service-b".to_string(),
        EventType::Created,
        EventData::Custom {
            data: serde_json::json!({}),
        },
        clock.timestamp(),
    )
    .with_dependencies(vec![id1.clone()]);
    event_store.append(event2).unwrap();

    clock.tick();
    let event3 = Event::new(
        "service-c".to_string(),
        EventType::Created,
        EventData::Custom {
            data: serde_json::json!({}),
        },
        clock.timestamp(),
    )
    .with_dependencies(vec![id1]);
    event_store.append(event3).unwrap();

    // Create debugger and navigate
    let mut debugger = TimeTravelDebugger::new(event_store);

    debugger.goto_start().await.unwrap();
    let first = debugger.current_event().unwrap();
    assert_eq!(first.entity_id, "service-a");

    debugger.step_forward().await.unwrap();
    debugger.step_forward().await.unwrap();

    // Create and restore checkpoint
    let checkpoint = debugger.create_checkpoint("test".to_string()).unwrap();
    debugger.step_backward().await.unwrap();
    debugger.restore_checkpoint(&checkpoint).await.unwrap();

    let current = debugger.current_event().unwrap();
    assert_eq!(current.entity_id, "service-c");
}

#[tokio::test]
async fn test_temporal_logic_evaluation() {
    let event_store = EventStore::new();
    let mut clock = VectorClock::new("node-1".to_string());

    // Create events with different properties
    for i in 0..5 {
        clock.tick();
        let event = Event::new(
            format!("entity-{i}"),
            if i < 3 {
                EventType::Created
            } else {
                EventType::Updated
            },
            EventData::Custom {
                data: serde_json::json!({"stable": i >= 4}),
            },
            clock.timestamp(),
        );
        event_store.append(event).unwrap();
    }

    // Create reasoner
    let reasoner = TemporalReasoner::new(|event| {
        let mut predicates = HashMap::new();

        match event.event_type {
            EventType::Created => {
                predicates.insert("created".to_string(), TruthValue::True);
            }
            EventType::Updated => {
                predicates.insert("updated".to_string(), TruthValue::True);
            }
            _ => {}
        }

        if let EventData::Custom { ref data } = event.data {
            if let Some(stable) = data.get("stable").and_then(|v| v.as_bool()) {
                predicates.insert(
                    "stable".to_string(),
                    if stable {
                        TruthValue::True
                    } else {
                        TruthValue::False
                    },
                );
            }
        }

        predicates
    });

    let stream = event_store.get_all_events();

    // Test "eventually stable"
    let formula = TemporalFormula::eventually(TemporalFormula::predicate("stable"));
    let result = reasoner.evaluate(&formula, &stream).unwrap();
    assert_eq!(result, TruthValue::True);

    // Test "eventually updated"
    assert!(reasoner.check_liveness("updated", &stream).unwrap());
}

#[test]
fn test_4d_ontology_temporal_queries() {
    let mut graph = TemporalGraph::new().unwrap();

    let now = chrono::Utc::now();
    let later = now + chrono::Duration::hours(1);

    // Add temporal triples
    let triple1 = TemporalTriple::new(
        "<http://example.org/entity>".to_string(),
        "<http://example.org/status>".to_string(),
        "\"active\"".to_string(),
        ValidTime::TimeRange(TimeRange::new(now, Some(later))),
    );

    graph.insert_temporal_triple(triple1).unwrap();

    let triple2 = TemporalTriple::new(
        "<http://example.org/entity>".to_string(),
        "<http://example.org/status>".to_string(),
        "\"inactive\"".to_string(),
        ValidTime::TimeRange(TimeRange::new(later, None)),
    );

    graph.insert_temporal_triple(triple2).unwrap();

    // Query at different times
    let query = "SELECT ?status WHERE { <http://example.org/entity> <http://example.org/status> ?status }";

    let results_now = graph.query_at_time(query, now).unwrap();
    assert!(!results_now.is_empty());

    let results_later = graph.query_at_time(query, later + chrono::Duration::minutes(30)).unwrap();
    assert!(!results_later.is_empty());
}

#[tokio::test]
async fn test_distributed_projections() {
    let store1 = EventStore::new();
    let store2 = EventStore::new();

    let mut node1 = ProjectionNode::new("node-1".to_string(), store1).unwrap();
    let mut node2 = ProjectionNode::new("node-2".to_string(), store2).unwrap();

    // Add projections to both nodes
    let projection1 = SemanticProjection::new(
        "test_projection".to_string(),
        "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
    );

    let projection2 = projection1.clone();

    node1.add_projection(projection1);
    node2.add_projection(projection2);

    // Create and process events
    let mut clock = VectorClock::new("source".to_string());
    clock.tick();

    let event = Event::new(
        "entity-1".to_string(),
        EventType::Created,
        EventData::Custom {
            data: serde_json::json!({}),
        },
        clock.timestamp(),
    );

    // Both nodes receive the same event
    node1.receive_event(event.clone()).await.unwrap();
    node2.receive_event(event).await.unwrap();

    // Sync nodes
    let time1 = node1.current_time();
    let time2 = node2.current_time();

    node1
        .sync_with_peer("node-2".to_string(), time2)
        .await
        .unwrap();
    node2
        .sync_with_peer("node-1".to_string(), time1)
        .await
        .unwrap();

    // Verify both nodes have the projection
    assert_eq!(node1.get_projections().len(), 1);
    assert_eq!(node2.get_projections().len(), 1);
}

#[test]
fn test_vector_clock_causality() {
    let mut clock_a = VectorClock::new("a".to_string());
    let mut clock_b = VectorClock::new("b".to_string());
    let mut clock_c = VectorClock::new("c".to_string());

    // A happens
    clock_a.tick();
    let time_a1 = clock_a.timestamp();

    // B receives from A
    clock_b.merge(&time_a1);
    let time_b1 = clock_b.timestamp();

    // C receives from B
    clock_c.merge(&time_b1);
    let time_c1 = clock_c.timestamp();

    // Verify causal chain: A -> B -> C
    assert!(time_a1.happened_before(&time_b1));
    assert!(time_b1.happened_before(&time_c1));
    assert!(time_a1.happened_before(&time_c1));

    // A and C can still work concurrently
    clock_a.tick();
    let time_a2 = clock_a.timestamp();

    // time_a2 and time_c1 are concurrent
    assert!(time_a2.is_concurrent(&time_c1));
}

#[test]
fn test_chrono_semantic_versioning() {
    let version = ChronoSemanticVersion::new(1, 0, 0);

    // Feature addition
    let feature_delta = SemanticDelta::Feature {
        description: "New feature".to_string(),
    };
    let new_version = version.apply_delta(&feature_delta);
    assert_eq!(new_version.semantic, SemanticVersion::new(1, 1, 0));

    // Breaking change
    let breaking_delta = SemanticDelta::Breaking {
        description: "Breaking change".to_string(),
    };
    let newer_version = new_version.apply_delta(&breaking_delta);
    assert_eq!(newer_version.semantic, SemanticVersion::new(2, 0, 0));

    // Fix
    let fix_delta = SemanticDelta::Fix {
        description: "Bug fix".to_string(),
    };
    let fixed_version = newer_version.apply_delta(&fix_delta);
    assert_eq!(fixed_version.semantic, SemanticVersion::new(2, 0, 1));
}

#[tokio::test]
async fn test_complete_temporal_workflow() {
    // This test demonstrates a complete workflow combining all features

    // 1. Setup
    let event_store = EventStore::new();
    let mut clock = VectorClock::new("main-node".to_string());
    let mut temporal_graph = TemporalGraph::new().unwrap();

    // 2. Generate initial code
    clock.tick();
    let create_event = Event::new(
        "api-service".to_string(),
        EventType::Created,
        EventData::CodeDelta {
            file_path: "src/api.rs".to_string(),
            diff: "+ pub fn get_user() { }".to_string(),
        },
        clock.timestamp(),
    )
    .with_version(ChronoSemanticVersion::new(1, 0, 0))
    .with_semantic_delta(SemanticDelta::Feature {
        description: "Initial API".to_string(),
    });

    let create_id = event_store.append(create_event.clone()).unwrap();

    // Add to temporal graph
    let now = chrono::Utc::now();
    let triple = TemporalTriple::new(
        "<http://example.org/APIService>".to_string(),
        "<http://example.org/hasVersion>".to_string(),
        "\"1.0.0\"".to_string(),
        ValidTime::TimeRange(TimeRange::new(now, None)),
    )
    .with_event_id(create_id.clone())
    .with_vector_time(create_event.vector_time.clone());

    temporal_graph.insert_temporal_triple(triple).unwrap();

    // 3. Add feature
    clock.tick();
    let feature_event = Event::new(
        "api-service".to_string(),
        EventType::Updated,
        EventData::CodeDelta {
            file_path: "src/api.rs".to_string(),
            diff: "+ pub fn create_user() { }".to_string(),
        },
        clock.timestamp(),
    )
    .with_version(ChronoSemanticVersion::new(1, 1, 0))
    .with_dependencies(vec![create_id]);

    event_store.append(feature_event).unwrap();

    // 4. Time travel debugging
    let mut debugger = TimeTravelDebugger::new(event_store.clone());
    debugger.goto_start().await.unwrap();
    debugger.step_forward().await.unwrap();

    let checkpoint = debugger
        .create_checkpoint("after_feature".to_string())
        .unwrap();
    assert_eq!(checkpoint.name, "after_feature");

    // 5. Temporal logic verification
    let reasoner = TemporalReasoner::new(|event| {
        let mut predicates = HashMap::new();
        predicates.insert("generated".to_string(), TruthValue::True);
        predicates
    });

    let stream = event_store.get_all_events();
    assert!(reasoner.check_invariant("generated", &stream).unwrap());

    // 6. Verify 4D ontology
    let history = temporal_graph.get_temporal_history("<http://example.org/APIService>");
    assert!(!history.is_empty());

    // Success!
    assert_eq!(event_store.len(), 2);
}
