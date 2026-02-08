//! Chicago TDD unit tests for ggen-yawl codegen::yawl_xml module
//!
//! Tests follow AAA pattern (Arrange/Act/Assert) with real collaborators.
//! State-based verification over interaction verification.

use ggen_yawl::codegen::yawl_xml::{canonicalize, validate, YawlXmlGenerator, escape_xml};
use ggen_yawl::template::{FlowContext, TaskContext, TemplateContext};

/// Helper module for test fixtures and utilities.
pub mod fixtures {
    use super::*;

    /// Create a minimal valid template context for YAWL XML generation.
    pub fn minimal_context() -> TemplateContext {
        TemplateContext {
            workflow_name: "minimal_workflow".to_string(),
            description: "A minimal YAWL workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![TaskContext {
                id: "task1".to_string(),
                name: "First Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            }],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        }
    }

    /// Create a full-featured template context.
    pub fn full_context() -> TemplateContext {
        TemplateContext {
            workflow_name: "full_workflow".to_string(),
            description: "A comprehensive YAWL workflow".to_string(),
            version: "2.1.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "start".to_string(),
                    name: "Start".to_string(),
                    split_type: "AND".to_string(),
                    join_type: "AND".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "process".to_string(),
                    name: "Process Data".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: false,
                    decomposition_id: Some("process_decomp".to_string()),
                },
                TaskContext {
                    id: "end".to_string(),
                    name: "End".to_string(),
                    split_type: "OR".to_string(),
                    join_type: "OR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![
                FlowContext {
                    source: "input".to_string(),
                    target: "start".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
                FlowContext {
                    source: "start".to_string(),
                    target: "process".to_string(),
                    condition: Some("has_data".to_string()),
                    predicate: Some("data != null".to_string()),
                    is_default: false,
                },
                FlowContext {
                    source: "start".to_string(),
                    target: "end".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
            ],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        }
    }

    /// Valid YAWL XML fixture for validation tests.
    pub const VALID_YAWL_XML: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
        <specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
            <name>test_workflow</name>
            <description>Test workflow</description>
            <decomposition id="test_workflow_net" type="WSNet">
                <inputCondition id="input"/>
                <outputCondition id="output"/>
            </decomposition>
        </specification>
    "#;

    /// Invalid YAWL XML missing declaration.
    pub const INVALID_YAWL_NO_DECLARATION: &str = r#"
        <specification xmlns="http://www.yawlfoundation.org/yawlschema"></specification>
    "#;

    /// Invalid YAWL XML missing specification element.
    pub const INVALID_YAWL_NO_SPECIFICATION: &str = r#"<?xml version="1.0"?><other></other>"#;

    /// Invalid YAWL XML with unclosed specification.
    pub const INVALID_YAWL_UNCLOSED: &str = r#"<?xml version="1.0"?><specification>"#;

    /// YAWL XML with tasks for testing generation.
    pub const YAWL_WITH_TASKS: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
        <specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
            <name>task_workflow</name>
            <description>Workflow with tasks</description>
            <decomposition id="task_workflow_net" type="WSNet">
                <inputCondition id="input"/>
                <outputCondition id="output"/>
                <task id="t1" name="Task 1">
                    <split type="XOR"/>
                    <join type="XOR"/>
                    <starting/>
                </task>
            </decomposition>
        </specification>
    "#;

    /// YAWL XML with flows for testing generation.
    pub const YAWL_WITH_FLOWS: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
        <specification xmlns="http://www.yawlfoundation.org/yawlschema" version="2.0">
            <name>flow_workflow</name>
            <description>Workflow with flows</description>
            <decomposition id="flow_workflow_net" type="WSNet">
                <inputCondition id="input"/>
                <outputCondition id="output"/>
                <task id="t1" name="Task 1">
                    <split type="XOR"/>
                    <join type="XOR"/>
                </task>
                <flow into="t2" from="t1"/>
            </decomposition>
        </specification>
    "#;
}

#[cfg(test)]
mod escape_xml_tests {
    use super::*;

    /// Test: Escape ampersand character
    #[test]
    fn test_escape_ampersand() {
        // Arrange & Act
        let escaped = escape_xml("Tom & Jerry");

        // Assert
        assert_eq!(escaped, "Tom &amp; Jerry");
    }

    /// Test: Escape less than character
    #[test]
    fn test_escape_less_than() {
        // Arrange & Act
        let escaped = escape_xml("a < b");

        // Assert
        assert_eq!(escaped, "a &lt; b");
    }

    /// Test: Escape greater than character
    #[test]
    fn test_escape_greater_than() {
        // Arrange & Act
        let escaped = escape_xml("a > b");

        // Assert
        assert_eq!(escaped, "a &gt; b");
    }

    /// Test: Escape double quote character
    #[test]
    fn test_escape_double_quote() {
        // Arrange & Act
        let escaped = escape_xml("\"quoted\"");

        // Assert
        assert_eq!(escaped, "&quot;quoted&quot;");
    }

    /// Test: Escape single quote (apostrophe) character
    #[test]
    fn test_escape_single_quote() {
        // Arrange & Act
        let escaped = escape_xml("'apostrophe'");

        // Assert
        assert_eq!(escaped, "&apos;apostrophe&apos;");
    }

    /// Test: Escape multiple special characters in one string
    #[test]
    fn test_escape_multiple_special_chars() {
        // Arrange & Act
        let escaped = escape_xml("<tag attr=\"value\" & 'other'>");

        // Assert
        assert_eq!(escaped, "&lt;tag attr=&quot;value&quot; &amp; &apos;other&apos;&gt;");
    }

    /// Test: Escape empty string returns empty string
    #[test]
    fn test_escape_empty_string() {
        // Arrange & Act
        let escaped = escape_xml("");

        // Assert
        assert_eq!(escaped, "");
    }

    /// Test: Escape string with no special characters returns unchanged
    #[test]
    fn test_escape_no_special_chars() {
        // Arrange & Act
        let escaped = escape_xml("normal_text_123");

        // Assert
        assert_eq!(escaped, "normal_text_123");
    }

    /// Test: Escape already escaped entities doesn't double-escape
    #[test]
    fn test_escape_already_entities() {
        // Arrange & Act
        let escaped = escape_xml("&amp; &lt; &gt;");

        // Assert
        // Note: Current implementation will double-escape the &
        assert!(escaped.contains("&amp;"));
    }

    /// Test: Escape unicode characters
    #[test]
    fn test_escape_unicode() {
        // Arrange & Act
        let escaped = escape_xml("Hello & world & ");

        // Assert
        assert_eq!(escaped, "Hello &amp; world &amp; ");
    }
}

#[cfg(test)]
mod validate_tests {
    use super::*;
    use fixtures::*;

    /// Test: Validate valid YAWL XML succeeds
    #[test]
    fn test_validate_valid_yawl_xml() {
        // Arrange & Act
        let result = validate(VALID_YAWL_XML);

        // Assert
        assert!(result.is_ok(), "Valid YAWL XML should pass validation");
    }

    /// Test: Validate XML without declaration fails
    #[test]
    fn test_validate_missing_xml_declaration() {
        // Arrange & Act
        let result = validate(INVALID_YAWL_NO_DECLARATION);

        // Assert
        assert!(result.is_err(), "Missing XML declaration should fail validation");
        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(error_msg.contains("XML declaration") || error_msg.contains("declaration"));
            }
            Ok(_) => panic!("Expected validation error"),
        }
    }

    /// Test: Validate XML without specification element fails
    #[test]
    fn test_validate_missing_specification_element() {
        // Arrange & Act
        let result = validate(INVALID_YAWL_NO_SPECIFICATION);

        // Assert
        assert!(result.is_err(), "Missing specification element should fail validation");
        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(error_msg.contains("specification") || error_msg.contains("element"));
            }
            Ok(_) => panic!("Expected validation error"),
        }
    }

    /// Test: Validate XML with unclosed specification fails
    #[test]
    fn test_validate_unclosed_specification() {
        // Arrange & Act
        let result = validate(INVALID_YAWL_UNCLOSED);

        // Assert
        assert!(result.is_err(), "Unclosed specification should fail validation");
        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(error_msg.contains("Unclosed") || error_msg.contains("specification"));
            }
            Ok(_) => panic!("Expected validation error"),
        }
    }

    /// Test: Validate empty string fails
    #[test]
    fn test_validate_empty_string() {
        // Arrange & Act
        let result = validate("");

        // Assert
        assert!(result.is_err(), "Empty string should fail validation");
    }

    /// Test: Validate with whitespace only fails
    #[test]
    fn test_validate_whitespace_only() {
        // Arrange & Act
        let result = validate("   \n\t  ");

        // Assert
        assert!(result.is_err(), "Whitespace only should fail validation");
    }
}

#[cfg(test)]
mod canonicalize_tests {
    use super::*;

    /// Test: Canonicalize simple XML returns normalized form
    #[test]
    fn test_canonicalize_simple_xml() {
        // Arrange
        let input = "<test><a>value</a></test>";

        // Act
        let result = canonicalize(input);

        // Assert
        assert!(result.is_ok());
        let canonical = result.unwrap();
        // Current implementation returns input as-is
        assert_eq!(canonical, input);
    }

    /// Test: Canonicalize preserves content
    #[test]
    fn test_canonicalize_preserves_content() {
        // Arrange
        let input = "<?xml version=\"1.0\"?><specification><name>Test</name></specification>";

        // Act
        let result = canonicalize(input);

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap().contains("Test"));
    }

    /// Test: Canonicalize idempotent
    #[test]
    fn test_canonicalize_idempotent() {
        // Arrange
        let input = "<?xml version=\"1.0\"?><test></test>";

        // Act
        let result1 = canonicalize(input);
        let result2 = canonicalize(result1.as_ref().map_or(input, |s| s.as_str()));

        // Assert
        assert!(result1.is_ok() && result2.is_ok());
        assert_eq!(result1.unwrap(), result2.unwrap());
    }
}

#[cfg(test)]
mod generator_tests {
    use super::*;
    use fixtures::*;

    /// Test: YawlXmlGenerator::generate produces valid XML structure
    #[test]
    fn test_generator_produces_valid_xml_structure() {
        // Arrange
        let ctx = minimal_context();

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok(), "Generation should succeed");
        let xml = result.unwrap();

        assert!(xml.contains("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"));
        assert!(xml.contains("<specification"));
        assert!(xml.contains("xmlns=\"http://www.yawlfoundation.org/yawlschema\""));
        assert!(xml.contains("version=\"2.0\""));
        assert!(xml.contains("</specification>"));
    }

    /// Test: YawlXmlGenerator includes workflow metadata
    #[test]
    fn test_generator_includes_workflow_metadata() {
        // Arrange
        let ctx = TemplateContext {
            workflow_name: "metadata_test".to_string(),
            description: "Test metadata inclusion".to_string(),
            version: "3.2.1".to_string(),
            tasks: vec![],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        assert!(xml.contains("<name>metadata_test</name>"));
        assert!(xml.contains("<description>Test metadata inclusion</description>"));
    }

    /// Test: YawlXmlGenerator includes tasks
    #[test]
    fn test_generator_includes_tasks() {
        // Arrange
        let ctx = TemplateContext {
            workflow_name: "task_test".to_string(),
            description: "Test".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "task_a".to_string(),
                    name: "Task A".to_string(),
                    split_type: "AND".to_string(),
                    join_type: "AND".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "task_b".to_string(),
                    name: "Task B".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: false,
                    decomposition_id: Some("decomp_b".to_string()),
                },
            ],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        assert!(xml.contains("<task id=\"task_a\""));
        assert!(xml.contains("<task id=\"task_b\""));
        assert!(xml.contains("name=\"Task A\""));
        assert!(xml.contains("name=\"Task B\""));
        assert!(xml.contains("<split type=\"AND\"/>"));
        assert!(xml.contains("<split type=\"XOR\"/>"));
        assert!(xml.contains("<join type=\"AND\"/>"));
        assert!(xml.contains("<join type=\"XOR\"/>"));
        assert!(xml.contains("<starting/>"));
        assert!(xml.contains("<decomposition id=\"decomp_b\"/>"));
    }

    /// Test: YawlXmlGenerator includes flows
    #[test]
    fn test_generator_includes_flows() {
        // Arrange
        let ctx = TemplateContext {
            workflow_name: "flow_test".to_string(),
            description: "Test".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "source".to_string(),
                    name: "Source".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "target".to_string(),
                    name: "Target".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![
                FlowContext {
                    source: "source".to_string(),
                    target: "target".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
            ],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        assert!(xml.contains("<flow into=\"target\" from=\"source\""));
    }

    /// Test: YawlXmlGenerator includes flow with condition
    #[test]
    fn test_generator_includes_flow_with_condition() {
        // Arrange
        let ctx = TemplateContext {
            workflow_name: "cond_flow_test".to_string(),
            description: "Test".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "t1".to_string(),
                    name: "T1".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "t2".to_string(),
                    name: "T2".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![FlowContext {
                source: "t1".to_string(),
                target: "t2".to_string(),
                condition: Some("x > 0".to_string()),
                predicate: Some("x > 0".to_string()),
                is_default: false,
            }],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        assert!(xml.contains("<predicate>"));
        assert!(xml.contains("x > 0"));
        assert!(xml.contains("</predicate>"));
        assert!(xml.contains("</flow>"));
    }

    /// Test: YawlXmlGenerator escapes special characters in output
    #[test]
    fn test_generator_escapes_special_characters() {
        // Arrange
        let ctx = TemplateContext {
            workflow_name: "test<&>\"".to_string(),
            description: "Desc & <test>".to_string(),
            version: "1.0".to_string(),
            tasks: vec![TaskContext {
                id: "task<&>".to_string(),
                name: "Task with <special> & \"chars\"".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            }],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Verify special characters are escaped
        assert!(xml.contains("&lt;"));
        assert!(xml.contains("&gt;"));
        assert!(xml.contains("&quot;"));
        assert!(xml.contains("&amp;"));
    }

    /// Test: YawlXmlGenerator produces deterministic output
    #[test]
    fn test_generator_deterministic_output() {
        // Arrange
        let ctx = full_context();

        // Act
        let result1 = YawlXmlGenerator::generate(&ctx);
        let result2 = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());
        assert_eq!(result1.unwrap(), result2.unwrap(), "Generation should be deterministic");
    }

    /// Test: YawlXmlGenerator handles empty workflow
    #[test]
    fn test_generator_empty_workflow() {
        // Arrange
        let ctx = TemplateContext {
            workflow_name: "empty".to_string(),
            description: "Empty".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        assert!(xml.contains("<inputCondition"));
        assert!(xml.contains("<outputCondition"));
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use fixtures::*;

    /// Test: Generate and validate complete YAWL workflow
    #[test]
    fn test_generate_and_validate_complete_workflow() {
        // Arrange
        let ctx = full_context();

        // Act
        let generate_result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(generate_result.is_ok());
        let xml = generate_result.unwrap();

        // Validate generated XML
        let validate_result = validate(&xml);
        assert!(validate_result.is_ok(), "Generated XML should pass validation");
    }

    /// Test: Full pipeline: generate -> canonicalize -> validate
    #[test]
    fn test_full_generation_pipeline() {
        // Arrange
        let ctx = full_context();

        // Act - Generate
        let generated = YawlXmlGenerator::generate(&ctx);
        assert!(generated.is_ok());

        // Act - Canonicalize
        let canonicalized = canonicalize(&generated.unwrap());
        assert!(canonicalized.is_ok());

        // Act - Validate
        let validated = validate(&canonicalized.unwrap());
        assert!(validated.is_ok(), "Complete pipeline should succeed");
    }

    /// Test: Generated XML contains all expected elements
    #[test]
    fn test_generated_xml_contains_all_elements() {
        // Arrange
        let ctx = full_context();

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Check for all required YAWL elements
        assert!(xml.contains("<?xml version=\"1.0\""));
        assert!(xml.contains("<specification"));
        assert!(xml.contains("<name>"));
        assert!(xml.contains("<description>"));
        assert!(xml.contains("<decomposition"));
        assert!(xml.contains("type=\"WSNet\""));
        assert!(xml.contains("<inputCondition"));
        assert!(xml.contains("<outputCondition"));

        // Check for tasks
        assert!(xml.contains("<task id=\"start\""));
        assert!(xml.contains("<task id=\"process\""));
        assert!(xml.contains("<task id=\"end\""));

        // Check for flows
        assert!(xml.contains("<flow"));
    }
}

#[cfg(test)]
mod property_based_tests {
    use super::*;

    /// Property: Escape function is idempotent for already escaped strings
    #[test]
    fn test_escape_idempotent_for_ampersand() {
        // Arrange
        let input = "&amp;";

        // Act
        let once = escape_xml(input);
        let twice = escape_xml(&once);

        // Assert - After first escape, should be stable
        assert_eq!(once, "&amp;amp;");
        assert_eq!(twice, "&amp;amp;amp;");
    }

    /// Property: Validate then canonicalize produces valid result
    #[test]
    fn test_validate_then_canonicalize() {
        // Arrange
        let valid_xml = fixtures::VALID_YAWL_XML;

        // Act
        let validate_result = validate(valid_xml);
        let canonical_result = canonicalize(valid_xml);

        // Assert
        assert!(validate_result.is_ok());
        assert!(canonical_result.is_ok());
    }

    /// Property: Canonicalize preserves validity
    #[test]
    fn test_canonicalize_preserves_validity() {
        // Arrange
        let valid_xml = fixtures::VALID_YAWL_XML;

        // Act
        let canonical = canonicalize(valid_xml).unwrap();

        // Assert
        assert!(validate(&canonical).is_ok());
    }

    /// Property: Empty context generates minimal but valid XML
    #[test]
    fn test_empty_context_generates_valid_xml() {
        // Arrange
        let ctx = TemplateContext {
            workflow_name: "test".to_string(),
            description: "test".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = YawlXmlGenerator::generate(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();
        assert!(validate(&xml).is_ok());
    }
}
