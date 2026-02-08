//! PetriNet <-> YAWL Roundtrip Conversion Tests
//!
//! Tests for bidirectional conversion between Petri nets and YAWL workflows.
//! Verifies structural preservation and semantic equivalence.

use ggen_process_mining::{
    Marking, PetriNet, PetriNetToYawl, Place, Transition, YawlBridge, YawlToPetriNet,
};

// Import Arc from petri_net module with a different name to avoid conflict
use ggen_process_mining::petri_net::Arc as PetriArc;
use std::collections::HashMap;

#[cfg(test)]
mod roundtrip_tests {
    use super::super::helpers::*;
    use super::*;

    /// Test basic YAWL to Petri net conversion.
    #[test]
    fn test_yawl_to_petri_net_basic() {
        // Arrange
        let yawl_xml = create_sample_yawl();
        let bridge = YawlBridge::new();

        // Act
        let result = bridge.yawl_to_petri_net(&yawl_xml);

        // Assert
        assert!(result.is_ok(), "YAWL to PetriNet conversion should succeed");

        let net = result.unwrap();
        assert_eq!(net.name, Some("Approval Process".to_string()));
        verify_petri_net_structure(&net, 3, 2, 4);
    }

    /// Test Petri net to YAWL conversion.
    #[test]
    fn test_petri_net_to_yawl_basic() {
        // Arrange
        let net = create_simple_petri_net();
        let bridge = YawlBridge::new();

        // Act
        let result = bridge.petri_net_to_yawl(&net);

        // Assert
        assert!(result.is_ok(), "PetriNet to YAWL conversion should succeed");

        let yawl = result.unwrap();
        assert!(yawl.contains("<specification"));
        assert!(yawl.contains("Simple Approval Process"));
        assert!(yawl.contains("Submit"));
        assert!(yawl.contains("Approve"));
    }

    /// Test complete roundtrip: YAWL -> PetriNet -> YAWL.
    #[test]
    fn test_yawl_to_petri_net_to_yawl_roundtrip() {
        // Arrange
        let original_yawl = create_sample_yawl();
        let bridge = YawlBridge::new();

        // Act - First conversion: YAWL -> PetriNet
        let petri_net = bridge
            .yawl_to_petri_net(&original_yawl)
            .expect("YAWL to PetriNet conversion should succeed");

        // Act - Second conversion: PetriNet -> YAWL
        let roundtrip_yawl = bridge
            .petri_net_to_yawl(&petri_net)
            .expect("PetriNet to YAWL conversion should succeed");

        // Assert - Verify roundtrip preserves key elements
        assert!(roundtrip_yawl.contains("<specification"));
        assert!(roundtrip_yawl.contains("Approval Process"));

        // Extract task names from original
        let original_tasks = extract_task_names(&original_yawl);
        let roundtrip_tasks = extract_task_names(&roundtrip_yawl);

        // Same number of tasks
        assert_eq!(
            original_tasks.len(),
            roundtrip_tasks.len(),
            "Roundtrip should preserve task count"
        );

        // All original tasks present (may be in different order)
        for task in &original_tasks {
            assert!(
                roundtrip_tasks.contains(task),
                "Task '{}' should be preserved in roundtrip",
                task
            );
        }
    }

    /// Test complete roundtrip: PetriNet -> YAWL -> PetriNet.
    #[test]
    fn test_petri_net_to_yawl_to_petri_net_roundtrip() {
        // Arrange
        let original_net = create_simple_petri_net();
        let _original_place_count = original_net.places.len();
        let original_transition_count = original_net.transitions.len();
        let bridge = YawlBridge::new();

        // Act - First conversion: PetriNet -> YAWL
        let yawl = bridge
            .petri_net_to_yawl(&original_net)
            .expect("PetriNet to YAWL conversion should succeed");

        // Act - Second conversion: YAWL -> PetriNet
        let roundtrip_net = bridge
            .yawl_to_petri_net(&yawl)
            .expect("YAWL to PetriNet conversion should succeed");

        // Assert - Verify structural preservation
        assert_eq!(
            roundtrip_net.transitions.len(),
            original_transition_count,
            "Roundtrip should preserve transition count"
        );
        // Note: Place count may differ due to YAWL's condition mapping
        assert!(
            roundtrip_net.places.len() >= 2,
            "Roundtrip should preserve at least minimal place structure"
        );
    }

    /// Test YAWL with AND split/join types.
    #[test]
    fn test_yawl_and_split_join_conversion() {
        // Arrange
        let yawl_with_and = r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema">
  <name>Parallel Process</name>
  <condition id="c_start"><name>Start</name></condition>
  <condition id="c_a"><name>A</name></condition>
  <condition id="c_b"><name>B</name></condition>
  <condition id="c_join"><name>Join</name></condition>
  <condition id="c_end"><name>End</name></condition>
  <task id="t_split" splitType="and" joinType="and"><name>Split</name></task>
  <task id="t_a" splitType="and" joinType="and"><name>Task A</name></task>
  <task id="t_b" splitType="and" joinType="and"><name>Task B</name></task>
  <task id="t_join" splitType="and" joinType="and"><name>Join</name></task>
  <flow source="c_start" target="t_split"/>
  <flow source="t_split" target="c_a"/>
  <flow source="t_split" target="c_b"/>
  <flow source="c_a" target="t_a"/>
  <flow source="c_b" target="t_b"/>
  <flow source="t_a" target="c_join"/>
  <flow source="t_b" target="c_join"/>
  <flow source="c_join" target="t_join"/>
  <flow source="t_join" target="c_end"/>
</specification>"#;

        let bridge = YawlBridge::new();

        // Act
        let result = bridge.yawl_to_petri_net(yawl_with_and);

        // Assert
        assert!(result.is_ok());
        let net = result.unwrap();

        // Verify parallel structure is preserved
        verify_petri_net_structure(&net, 5, 4, 10);
        verify_workflow_pattern(&net, "parallel");
    }

    /// Test YAWL with XOR split/join types.
    #[test]
    fn test_yawl_xor_split_join_conversion() {
        // Arrange
        let yawl_with_xor = r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema">
  <name>Exclusive Choice</name>
  <condition id="c_start"><name>Start</name></condition>
  <condition id="c_a"><name>Path A</name></condition>
  <condition id="c_b"><name>Path B</name></condition>
  <condition id="c_end"><name>End</name></condition>
  <task id="t_choice" splitType="xor" joinType="xor"><name>Choice</name></task>
  <task id="t_a" splitType="and" joinType="and"><name>Task A</name></task>
  <task id="t_b" splitType="and" joinType="and"><name>Task B</name></task>
  <flow source="c_start" target="t_choice"/>
  <flow source="t_choice" target="c_a"/>
  <flow source="t_choice" target="c_b"/>
  <flow source="c_a" target="t_a"/>
  <flow source="c_b" target="t_b"/>
  <flow source="t_a" target="c_end"/>
  <flow source="t_b" target="c_end"/>
</specification>"#;

        let bridge = YawlBridge::new();

        // Act
        let result = bridge.yawl_to_petri_net(yawl_with_xor);

        // Assert
        assert!(result.is_ok());
        let net = result.unwrap();

        // XOR splits map to Petri net with choice structure
        assert!(net.transitions.len() >= 3);
        assert!(net.validate().is_ok());
    }

    /// Test complex YAWL workflow with multiple decomposition levels.
    #[test]
    fn test_yawl_composite_task_conversion() {
        // Arrange
        let yawl_with_composite = r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema">
  <name>Composite Task Process</name>
  <condition id="c_start"><name>Start</name></condition>
  <condition id="c_composite_input"><name>Composite Input</name></condition>
  <condition id="c_composite_output"><name>Composite Output</name></condition>
  <condition id="c_end"><name>End</name></condition>
  <task id="t_start" splitType="and" joinType="and"><name>Start</name></task>
  <task id="t_composite" splitType="and" joinType="and">
    <name>Composite Task</name>
    <decomposition id="d_composite"/>
  </task>
  <task id="t_end" splitType="and" joinType="and"><name>End</name></task>
  <flow source="c_start" target="t_start"/>
  <flow source="t_start" target="c_composite_input"/>
  <flow source="c_composite_input" target="t_composite"/>
  <flow source="t_composite" target="c_composite_output"/>
  <flow source="c_composite_output" target="t_end"/>
  <flow source="t_end" target="c_end"/>
</specification>"#;

        let bridge = YawlBridge::new().with_preserve_composites(true);

        // Act
        let result = bridge.yawl_to_petri_net(yawl_with_composite);

        // Assert
        assert!(result.is_ok());
        let net = result.unwrap();

        // Composite task should be preserved as a single transition
        assert!(net
            .transitions
            .iter()
            .any(|t| t.label.as_ref() == Some(&"Composite Task".to_string())));
    }

    /// Test trait-based conversion convenience methods.
    #[test]
    fn test_trait_based_conversion() {
        // Arrange
        let yawl_xml = create_sample_yawl();

        // Act - Using YawlToPetriNet trait on &str
        let petri_net = yawl_xml
            .to_petri_net()
            .expect("Trait-based YAWL to PetriNet should work");

        // Assert
        assert!(petri_net.validate().is_ok());
        assert_eq!(petri_net.name, Some("Approval Process".to_string()));

        // Act - Using PetriNetToYawl trait on PetriNet
        let yawl_output = petri_net
            .to_yawl()
            .expect("Trait-based PetriNet to YAWL should work");

        // Assert
        assert!(yawl_output.contains("<specification"));
    }

    /// Test invalid YAWL XML handling.
    #[test]
    fn test_invalid_yawl_xml() {
        // Arrange
        let invalid_xml = "This is not valid XML at all";
        let bridge = YawlBridge::new();

        // Act
        let result = bridge.yawl_to_petri_net(invalid_xml);

        // Assert
        assert!(result.is_err());
        if let Err(e) = result {
            assert!(
                e.to_string().contains("XML parse error") || e.to_string().contains("conversion")
            );
        }
    }

    /// Test YAWL XML missing required elements.
    #[test]
    fn test_yawl_missing_specification() {
        // Arrange
        let incomplete_yawl = r#"<?xml version="1.0" encoding="UTF-8"?>
<yawlschema>
  <name>Missing Specification Element</name>
</yawlschema>"#;

        let bridge = YawlBridge::new();

        // Act
        let result = bridge.yawl_to_petri_net(incomplete_yawl);

        // Assert
        assert!(result.is_err());
    }

    /// Test conversion preserves marking information.
    #[test]
    fn test_marking_preservation_in_conversion() {
        // Arrange
        let net = PetriNet::new()
            .with_name("Marking Test")
            .with_place(Place::new("p1"))
            .with_place(Place::new("p2"))
            .with_place(Place::new("p3"))
            .with_transition(Transition::new("t1"))
            .with_transition(Transition::new("t2"))
            .with_arc(PetriArc::new("p1", "t1"))
            .with_arc(PetriArc::new("t1", "p2"))
            .with_arc(PetriArc::new("p2", "t2"))
            .with_arc(PetriArc::new("t2", "p3"))
            .with_initial_marking(Marking::new().with_token("p1", 1))
            .with_final_marking(Marking::new().with_token("p3", 1));

        let bridge = YawlBridge::new();

        // Act
        let yawl = bridge.petri_net_to_yawl(&net).unwrap();
        let roundtrip_net = bridge.yawl_to_petri_net(&yawl).unwrap();

        // Assert - Markings should be preserved
        assert!(
            !roundtrip_net.initial_marking.tokens.is_empty(),
            "Initial marking should be preserved"
        );
        assert!(
            !roundtrip_net.final_marking.tokens.is_empty(),
            "Final marking should be preserved"
        );
    }
}

/// Helper function to extract task names from YAWL XML.
fn extract_task_names(yawl_xml: &str) -> Vec<String> {
    let mut tasks = Vec::new();

    // Simple XML parsing for task names
    for line in yawl_xml.lines() {
        if line.contains("<name>") && line.contains("</name>") {
            if let Some(start) = line.find("<name>") {
                if let Some(end) = line.find("</name>") {
                    let name = &line[start + 6..end];
                    if !name.trim().is_empty() && !line.contains("<specification") {
                        tasks.push(name.trim().to_string());
                    }
                }
            }
        }
    }

    tasks
}
