//! Process Mining Integration Tests
//!
//! Comprehensive integration tests for Rust4PM integration in ggen.
//! Tests cover:
//! - PetriNet <-> YAWL roundtrip conversion
//! - Alpha++ discovery on real event logs
//! - Conformance checking
//! - XES/OCEL import/export
//! - Workflow structure verification

mod alpha_plusplus_discovery;
mod conformance_checking;
mod ocel_import_export;
mod petri_net_yawl_roundtrip;
mod workflow_structure_verification;
mod xes_import_export;

#[cfg(test)]
mod helpers {
    use ggen_process_mining::petri_net::Arc as PetriArc;
    use ggen_process_mining::{Event, EventLog, Marking, PetriNet, Place, Trace, Transition};
    use std::collections::HashMap;

    /// Create a simple sequential event log for testing.
    pub fn create_simple_log() -> EventLog {
        let trace1 = Trace::new("case1")
            .with_event(Event::new("e1", "Submit", "2024-01-01T10:00:00Z").unwrap())
            .with_event(Event::new("e2", "Review", "2024-01-01T11:00:00Z").unwrap())
            .with_event(Event::new("e3", "Approve", "2024-01-01T12:00:00Z").unwrap());

        let trace2 = Trace::new("case2")
            .with_event(Event::new("e4", "Submit", "2024-01-02T10:00:00Z").unwrap())
            .with_event(Event::new("e5", "Review", "2024-01-02T11:00:00Z").unwrap())
            .with_event(Event::new("e6", "Approve", "2024-01-02T12:00:00Z").unwrap());

        EventLog::new("Simple Sequential Log")
            .with_trace(trace1)
            .with_trace(trace2)
    }

    /// Create a parallel process event log for testing.
    pub fn create_parallel_log() -> EventLog {
        let trace1 = Trace::new("case1")
            .with_event(Event::new("e1", "Start", "2024-01-01T10:00:00Z").unwrap())
            .with_event(Event::new("e2", "TaskA", "2024-01-01T11:00:00Z").unwrap())
            .with_event(Event::new("e3", "TaskB", "2024-01-01T11:30:00Z").unwrap())
            .with_event(Event::new("e4", "End", "2024-01-01T12:00:00Z").unwrap());

        EventLog::new("Parallel Process Log").with_trace(trace1)
    }

    /// Create a simple Petri net for testing.
    pub fn create_simple_petri_net() -> PetriNet {
        PetriNet::new()
            .with_name("Simple Approval Process")
            .with_place(Place::new("p_start").with_label("Start"))
            .with_place(Place::new("p_review").with_label("Under Review"))
            .with_place(Place::new("p_end").with_label("Complete"))
            .with_transition(Transition::new("t_submit").with_label("Submit"))
            .with_transition(Transition::new("t_approve").with_label("Approve"))
            .with_arc(PetriArc::new("p_start", "t_submit"))
            .with_arc(PetriArc::new("t_submit", "p_review"))
            .with_arc(PetriArc::new("p_review", "t_approve"))
            .with_arc(PetriArc::new("t_approve", "p_end"))
            .with_initial_marking(Marking::new().with_token("p_start", 1))
            .with_final_marking(Marking::new().with_token("p_end", 1))
    }

    /// Create a complex Petri net with parallel branches.
    pub fn create_parallel_petri_net() -> PetriNet {
        PetriNet::new()
            .with_name("Parallel Process")
            .with_place(Place::new("p_start").with_label("Start"))
            .with_place(Place::new("p_branch_a").with_label("Branch A"))
            .with_place(Place::new("p_branch_b").with_label("Branch B"))
            .with_place(Place::new("p_join").with_label("Join"))
            .with_place(Place::new("p_end").with_label("End"))
            .with_transition(Transition::new("t_start").with_label("Start"))
            .with_transition(Transition::new("t_task_a").with_label("TaskA"))
            .with_transition(Transition::new("t_task_b").with_label("TaskB"))
            .with_transition(Transition::new("t_end").with_label("End"))
            .with_arc(PetriArc::new("p_start", "t_start"))
            .with_arc(PetriArc::new("t_start", "p_branch_a"))
            .with_arc(PetriArc::new("t_start", "p_branch_b"))
            .with_arc(PetriArc::new("p_branch_a", "t_task_a"))
            .with_arc(PetriArc::new("p_branch_b", "t_task_b"))
            .with_arc(PetriArc::new("t_task_a", "p_join"))
            .with_arc(PetriArc::new("t_task_b", "p_join"))
            .with_arc(PetriArc::new("p_join", "t_end"))
            .with_arc(PetriArc::new("t_end", "p_end"))
            .with_initial_marking(Marking::new().with_token("p_start", 1))
            .with_final_marking(Marking::new().with_token("p_end", 1))
    }

    /// Create a sample YAWL XML for testing.
    pub fn create_sample_yawl() -> String {
        r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema">
  <name>Approval Process</name>
  <condition id="c_input">
    <name>Input</name>
  </condition>
  <condition id="c_review">
    <name>Review</name>
  </condition>
  <condition id="c_output">
    <name>Output</name>
  </condition>
  <task id="t_submit" splitType="and" joinType="and">
    <name>Submit</name>
  </task>
  <task id="t_approve" splitType="and" joinType="and">
    <name>Approve</name>
  </task>
  <flow source="c_input" target="t_submit"/>
  <flow source="t_submit" target="c_review"/>
  <flow source="c_review" target="t_approve"/>
  <flow source="t_approve" target="c_output"/>
</specification>"#
            .to_string()
    }

    /// Create a sample XES log for testing.
    pub fn create_sample_xes() -> String {
        r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Test Log" version="1.0">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <extension name="Organizational" prefix="org" uri="http://www.xes-standard.org/org.xesext"/>
  <global scope="trace">
    <string key="concept:name" value="case id"/>
  </global>
  <global scope="event">
    <string key="concept:name" value="activity name"/>
    <date key="time:timestamp" value="2024-01-01T10:00:00.000+00:00"/>
  </global>
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="Submit"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
      <string key="org:resource" value="Alice"/>
    </event>
    <event>
      <string key="concept:name" value="Review"/>
      <date key="time:timestamp" value="2024-01-01T11:00:00Z"/>
      <string key="org:resource" value="Bob"/>
    </event>
    <event>
      <string key="concept:name" value="Approve"/>
      <date key="time:timestamp" value="2024-01-01T12:00:00Z"/>
      <string key="org:resource" value="Carol"/>
    </event>
  </trace>
  <trace>
    <string key="concept:name" value="case2"/>
    <event>
      <string key="concept:name" value="Submit"/>
      <date key="time:timestamp" value="2024-01-02T10:00:00Z"/>
      <string key="org:resource" value="Dave"/>
    </event>
    <event>
      <string key="concept:name" value="Review"/>
      <date key="time:timestamp" value="2024-01-02T11:00:00Z"/>
      <string key="org:resource" value="Eve"/>
    </event>
    <event>
      <string key="concept:name" value="Approve"/>
      <date key="time:timestamp" value="2024-01-02T12:00:00Z"/>
      <string key="org:resource" value="Frank"/>
    </event>
  </trace>
</log>"#
            .to_string()
    }

    /// Create a sample OCEL JSON for testing.
    pub fn create_sample_ocel() -> String {
        r#"{
  "ocel-version": "2.0",
  "objectTypes": [
    {
      "name": "order",
      "attributes": [
        {"name": "price", "type": "float"},
        {"name": "status", "type": "string"}
      ]
    },
    {
      "name": "item",
      "attributes": [
        {"name": "name", "type": "string"},
        {"name": "quantity", "type": "int"}
      ]
    }
  ],
  "activities": ["Create Order", "Add Item", "Pay Order", "Ship Order"],
  "events": [
    {
      "id": "e1",
      "activity": "Create Order",
      "timestamp": "2024-01-01T10:00:00Z",
      "objects": [
        {"id": "order1", "qualifier": "creator"}
      ]
    },
    {
      "id": "e2",
      "activity": "Add Item",
      "timestamp": "2024-01-01T10:05:00Z",
      "objects": [
        {"id": "order1", "qualifier": "order"},
        {"id": "item1", "qualifier": "item"}
      ]
    },
    {
      "id": "e3",
      "activity": "Pay Order",
      "timestamp": "2024-01-01T11:00:00Z",
      "objects": [
        {"id": "order1", "qualifier": null}
      ]
    },
    {
      "id": "e4",
      "activity": "Ship Order",
      "timestamp": "2024-01-02T09:00:00Z",
      "objects": [
        {"id": "order1", "qualifier": null}
      ]
    }
  ],
  "objects": [
    {
      "id": "order1",
      "type": "order",
      "attributes": {
        "price": 99.99,
        "status": "shipped"
      }
    },
    {
      "id": "item1",
      "type": "item",
      "attributes": {
        "name": "Widget",
        "quantity": 2
      }
    }
  ]
}"#
        .to_string()
    }

    /// Verify Petri net structural properties.
    pub fn verify_petri_net_structure(
        net: &PetriNet, expected_places: usize, expected_transitions: usize, expected_arcs: usize,
    ) {
        assert_eq!(
            net.places.len(),
            expected_places,
            "Expected {} places, got {}",
            expected_places,
            net.places.len()
        );
        assert_eq!(
            net.transitions.len(),
            expected_transitions,
            "Expected {} transitions, got {}",
            expected_transitions,
            net.transitions.len()
        );
        assert_eq!(
            net.arcs.len(),
            expected_arcs,
            "Expected {} arcs, got {}",
            expected_arcs,
            net.arcs.len()
        );
        assert!(net.validate().is_ok(), "Petri net validation failed");
    }

    /// Verify workflow structure matches expected process pattern.
    pub fn verify_workflow_pattern(net: &PetriNet, pattern: &str) {
        match pattern {
            "sequential" => {
                // Sequential workflow should have a clear start and end
                assert!(
                    !net.initial_marking.tokens.is_empty(),
                    "Sequential workflow must have initial marking"
                );
                assert!(
                    !net.final_marking.tokens.is_empty(),
                    "Sequential workflow must have final marking"
                );
                assert!(
                    net.transitions.len() >= 2,
                    "Sequential workflow should have at least 2 transitions"
                );
            }
            "parallel" => {
                // Parallel workflow should have places with multiple outputs
                let has_parallel_split = net
                    .places
                    .iter()
                    .any(|p| net.output_transitions_for(&p.id).len() > 1);
                assert!(
                    has_parallel_split,
                    "Parallel workflow should have at least one parallel split"
                );
            }
            "loop" => {
                // Loop workflow should have cycles
                let transition_ids: Vec<_> = net.transitions.iter().map(|t| &t.id).collect();
                for tid in &transition_ids {
                    let outputs = net.output_places_for(tid);
                    for place in outputs {
                        let back_connections = net.input_transitions_for(&place.id);
                        if back_connections.iter().any(|t| t.id.as_str() == *tid) {
                            // Found a cycle back
                            return;
                        }
                    }
                }
                panic!("Loop workflow should have at least one cycle");
            }
            _ => {
                panic!("Unknown workflow pattern: {}", pattern);
            }
        }
    }
}
