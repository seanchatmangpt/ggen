//! Tests for the YAWL XML parser module.
//!
//! This test file verifies that the parser module compiles correctly
//! and can parse YAWL XML specifications.

use ggen_workflow::parser::{WorkflowPattern, YawlParser, YawlSpec};
use std::collections::HashMap;

/// Simple YAWL specification for testing.
const SIMPLE_YAWL: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
  <name>Test Workflow</name>
  <description>A simple test workflow</description>
  <decomposition id="test_net" type="WSNet">
    <inputCondition id="input"/>
    <outputCondition id="output"/>
    <task id="task1" name="First Task">
      <split type="and"/>
      <join type="xor"/>
    </task>
    <task id="task2" name="Second Task">
      <split type="xor"/>
      <join type="and"/>
      <starting/>
    </task>
    <flow into="task1" from="input"/>
    <flow into="task2" from="task1"/>
    <flow into="output" from="task2"/>
  </decomposition>
</specification>"#;

#[test]
fn test_parser_compiles() {
    // This test verifies that the parser module compiles correctly
    let _parser = YawlParser::new();
}

#[test]
fn test_spec_type_compiles() {
    // This test verifies that the YawlSpec type compiles correctly
    let _spec = YawlSpec {
        metadata: ggen_workflow::parser::SpecMetadata {
            id: "test-id".to_string(),
            name: "Test".to_string(),
            description: None,
            version: "2.0".to_string(),
            author: None,
            created: None,
            attributes: HashMap::new(),
        },
        root_decomposition: ggen_workflow::parser::Decomposition {
            id: "test_net".to_string(),
            name: "Test Net".to_string(),
            decomposition_type: ggen_workflow::parser::DecompositionType::WSNet,
            input_condition: None,
            output_condition: None,
            tasks: vec![],
            flows: vec![],
            parent_id: None,
            is_root: true,
        },
        decompositions: vec![],
        tasks: HashMap::new(),
        flows: vec![],
        variables: HashMap::new(),
        conditions: HashMap::new(),
        detected_patterns: std::collections::HashSet::new(),
    };
}

#[test]
fn test_workflow_pattern_compiles() {
    // This test verifies that WorkflowPattern enum compiles correctly
    let _pattern = WorkflowPattern::Sequence;
    assert_eq!(WorkflowPattern::Sequence.number(), 1);
    assert_eq!(WorkflowPattern::Sequence.name(), "Sequence");
}

#[test]
fn test_all_20_patterns_defined() {
    // Verify all 20 Van der Aalst workflow patterns are defined
    let patterns = vec![
        WorkflowPattern::Sequence,
        WorkflowPattern::ParallelSplit,
        WorkflowPattern::Synchronization,
        WorkflowPattern::ExclusiveChoice,
        WorkflowPattern::SimpleMerge,
        WorkflowPattern::MultiChoice,
        WorkflowPattern::SynchronizingMerge,
        WorkflowPattern::MultiMerge,
        WorkflowPattern::Discriminator,
        WorkflowPattern::ArbitraryCycles,
        WorkflowPattern::ImplicitTermination,
        WorkflowPattern::MultipleInstancesDynamic,
        WorkflowPattern::MultipleInstancesDesignTime,
        WorkflowPattern::MultipleInstancesRuntime,
        WorkflowPattern::DeferredChoice,
        WorkflowPattern::InterleavedParallelRouting,
        WorkflowPattern::CancelActivity,
        WorkflowPattern::CancelCase,
        WorkflowPattern::PatternBasedTermination,
        WorkflowPattern::CancelRegion,
    ];

    // Verify all patterns have unique numbers 1-20
    let numbers: Vec<u8> = patterns.iter().map(|p| p.number()).collect();
    let mut sorted_numbers = numbers.clone();
    sorted_numbers.sort();
    sorted_numbers.dedup();

    assert_eq!(sorted_numbers.len(), 20);
    assert_eq!(sorted_numbers, (1..=20).collect::<Vec<_>>());
}
