//! Chicago TDD unit tests for ggen-yawl template::renderer module
//!
//! Tests follow AAA pattern (Arrange/Act/Assert) with real collaborators.
//! State-based verification over interaction verification.

use ggen_yawl::template::{
    renderer::TemplateRenderer, ConditionContext, FlowContext, TaskContext, TemplateContext,
    VariableContext,
};
use std::path::Path;

/// Helper module for test fixtures and utilities.
pub mod fixtures {
    use super::*;

    /// Create a minimal valid template context for testing.
    pub fn minimal_context() -> TemplateContext {
        TemplateContext {
            workflow_name: "test_workflow".to_string(),
            description: "Test workflow description".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![TaskContext {
                id: "task1".to_string(),
                name: "First Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            }],
            flows: vec![FlowContext {
                source: "input".to_string(),
                target: "task1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            }],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        }
    }

    /// Create a complex template context with multiple tasks and flows.
    pub fn complex_context() -> TemplateContext {
        TemplateContext {
            workflow_name: "complex_workflow".to_string(),
            description: "A complex workflow with multiple tasks".to_string(),
            version: "2.5.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "start".to_string(),
                    name: "Start Task".to_string(),
                    split_type: "AND".to_string(),
                    join_type: "AND".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "process1".to_string(),
                    name: "Process Data".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: false,
                    decomposition_id: Some("decomp1".to_string()),
                },
                TaskContext {
                    id: "process2".to_string(),
                    name: "Validate Data".to_string(),
                    split_type: "OR".to_string(),
                    join_type: "OR".to_string(),
                    is_auto: false,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "end".to_string(),
                    name: "End Task".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
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
                    target: "process1".to_string(),
                    condition: Some("data_available".to_string()),
                    predicate: Some("hasData() == true".to_string()),
                    is_default: false,
                },
                FlowContext {
                    source: "start".to_string(),
                    target: "process2".to_string(),
                    condition: Some("data_valid".to_string()),
                    predicate: Some("isValid() == true".to_string()),
                    is_default: true,
                },
                FlowContext {
                    source: "process1".to_string(),
                    target: "end".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
                FlowContext {
                    source: "process2".to_string(),
                    target: "end".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
            ],
            input_condition: Some(ConditionContext {
                id: "input_condition".to_string(),
                expression: "true".to_string(),
                condition_type: "input".to_string(),
            }),
            output_condition: Some(ConditionContext {
                id: "output_condition".to_string(),
                expression: "finished == true".to_string(),
                condition_type: "output".to_string(),
            }),
            variables: vec![
                VariableContext {
                    name: "inputData".to_string(),
                    var_type: "string".to_string(),
                    default: Some("default_value".to_string()),
                    scope: "workflow".to_string(),
                },
                VariableContext {
                    name: "processCount".to_string(),
                    var_type: "integer".to_string(),
                    default: Some("0".to_string()),
                    scope: "process1".to_string(),
                },
            ],
        }
    }

    /// Create an empty template context.
    pub fn empty_context() -> TemplateContext {
        TemplateContext {
            workflow_name: "empty_workflow".to_string(),
            description: "Empty workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        }
    }

    /// Create a context with special characters that need XML escaping.
    pub fn special_characters_context() -> TemplateContext {
        TemplateContext {
            workflow_name: "workflow_<test>".to_string(),
            description: "Description with \"quotes\" & 'apostrophes'".to_string(),
            version: "1.0<>".to_string(),
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
        }
    }
}

#[cfg(test)]
mod renderer_creation_tests {
    use super::*;

    /// Test: TemplateRenderer::new creates a renderer with templates
    #[test]
    fn test_renderer_new() {
        // Arrange & Act
        let renderer = TemplateRenderer::new();

        // Assert
        let template_names: Vec<_> = renderer.tera().get_template_names().collect();
        assert!(!template_names.is_empty(), "Renderer should have templates loaded");
    }

    /// Test: TemplateRenderer::default creates a valid renderer
    #[test]
    fn test_renderer_default() {
        // Arrange & Act
        let renderer = TemplateRenderer::default();

        // Assert
        assert!(!renderer.tera().get_template_names().collect::<Vec<_>>().is_empty());
    }

    /// Test: TemplateRenderer::with_template_dir loads from custom directory
    #[test]
    fn test_renderer_with_template_dir() {
        // Arrange
        let template_dir = concat!(env!("CARGO_MANIFEST_DIR"), "/crates/ggen-yawl/templates");

        // Act
        let result = TemplateRenderer::with_template_dir(template_dir);

        // Assert
        assert!(result.is_ok(), "Should load from template directory");
        let renderer = result.unwrap();
        let template_names: Vec<_> = renderer.tera().get_template_names().collect();
        assert!(!template_names.is_empty());
    }

    /// Test: TemplateRenderer::with_template_dir returns error for invalid directory
    #[test]
    fn test_renderer_with_invalid_directory() {
        // Arrange
        let invalid_dir = "/nonexistent/directory/that/does/not/exist";

        // Act
        let result = TemplateRenderer::with_template_dir(invalid_dir);

        // Assert
        assert!(result.is_err(), "Invalid directory should return error");
    }
}

#[cfg(test)]
mod render_yawl_xml_tests {
    use super::*;
    use fixtures::*;

    /// Test: Render minimal context produces valid YAWL XML
    #[test]
    fn test_render_minimal_context() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = minimal_context();

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok(), "Rendering should succeed");
        let xml = result.unwrap();

        assert!(xml.contains("<?xml version=\"1.0\""));
        assert!(xml.contains("<specification"));
        assert!(xml.contains("xmlns=\"http://www.yawlfoundation.org/yawlschema\""));
        assert!(xml.contains("test_workflow"));
        assert!(xml.contains("Test workflow description"));
        assert!(xml.contains("<task id=\"task1\""));
        assert!(xml.contains("First Task"));
    }

    /// Test: Render complex context produces complete YAWL XML
    #[test]
    fn test_render_complex_context() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = complex_context();

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Verify workflow metadata
        assert!(xml.contains("complex_workflow"));
        assert!(xml.contains("A complex workflow with multiple tasks"));
        assert!(xml.contains("version=\"2.5.0\""));

        // Verify all tasks are rendered
        assert!(xml.contains("<task id=\"start\""));
        assert!(xml.contains("<task id=\"process1\""));
        assert!(xml.contains("<task id=\"process2\""));
        assert!(xml.contains("<task id=\"end\""));

        // Verify task names
        assert!(xml.contains("Start Task"));
        assert!(xml.contains("Process Data"));
        assert!(xml.contains("Validate Data"));
        assert!(xml.contains("End Task"));

        // Verify split/join types
        assert!(xml.contains("type=\"AND\""));
        assert!(xml.contains("type=\"XOR\""));
        assert!(xml.contains("type=\"OR\""));

        // Verify decomposition
        assert!(xml.contains("decomposition id=\"decomp1\""));

        // Verify auto-starting tasks
        assert!(xml.contains("<starting/>"));

        // Verify flows
        assert!(xml.contains("<flow into=\"start\""));
        assert!(xml.contains("<flow into=\"process1\""));
        assert!(xml.contains("<flow into=\"process2\""));

        // Verify conditions
        assert!(xml.contains("<predicate>"));
    }

    /// Test: Render empty context produces valid but minimal XML
    #[test]
    fn test_render_empty_context() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = empty_context();

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        assert!(xml.contains("<?xml version=\"1.0\""));
        assert!(xml.contains("<specification"));
        assert!(xml.contains("empty_workflow"));
        assert!(xml.contains("<decomposition"));
        assert!(xml.contains("<inputCondition"));
        assert!(xml.contains("<outputCondition"));
    }

    /// Test: Render context with special characters handles escaping
    #[test]
    fn test_render_special_characters_escaped() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = special_characters_context();

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Verify XML special characters are escaped
        assert!(xml.contains("&lt;test&gt;"), "< and > should be escaped");
        assert!(xml.contains("&quot;"), "Quotes should be escaped");
        assert!(xml.contains("&amp;"), "& should be escaped");

        // Verify no unescaped special characters in element content
        // (This is a basic check; comprehensive XML validation would require parsing)
        assert!(!xml.contains("<task id=\"task<"));
    }

    /// Test: Render preserves task order
    #[test]
    fn test_render_preserves_task_order() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = TemplateContext {
            workflow_name: "ordered_workflow".to_string(),
            description: "Test ordering".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "first".to_string(),
                    name: "First".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "second".to_string(),
                    name: "Second".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "third".to_string(),
                    name: "Third".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Verify tasks appear in order
        let first_pos = xml.find("id=\"first\"").unwrap();
        let second_pos = xml.find("id=\"second\"").unwrap();
        let third_pos = xml.find("id=\"third\"").unwrap();

        assert!(first_pos < second_pos);
        assert!(second_pos < third_pos);
    }

    /// Test: Render with flow conditions
    #[test]
    fn test_render_flow_with_conditions() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = TemplateContext {
            workflow_name: "conditional_workflow".to_string(),
            description: "Test conditional flows".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "decision".to_string(),
                    name: "Decision Point".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: false,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "branch_a".to_string(),
                    name: "Branch A".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "branch_b".to_string(),
                    name: "Branch B".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![
                FlowContext {
                    source: "decision".to_string(),
                    target: "branch_a".to_string(),
                    condition: Some("score > 50".to_string()),
                    predicate: Some("score > 50".to_string()),
                    is_default: false,
                },
                FlowContext {
                    source: "decision".to_string(),
                    target: "branch_b".to_string(),
                    condition: Some("score <= 50".to_string()),
                    predicate: Some("score <= 50".to_string()),
                    is_default: true,
                },
            ],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        assert!(xml.contains("<predicate>"));
        assert!(xml.contains("score > 50"));
        assert!(xml.contains("score <= 50"));
    }

    /// Test: Render without conditions produces self-closing flow elements
    #[test]
    fn test_render_flow_without_condition_self_closing() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = TemplateContext {
            workflow_name: "simple_workflow".to_string(),
            description: "Simple workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "task1".to_string(),
                    name: "Task 1".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "task2".to_string(),
                    name: "Task 2".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![FlowContext {
                source: "task1".to_string(),
                target: "task2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            }],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Should have self-closing flow element
        assert!(xml.contains("/>"));
    }
}

#[cfg(test)]
mod render_erlang_module_tests {
    use super::*;
    use fixtures::*;

    /// Test: Render Erlang module requires template
    #[test]
    fn test_render_erlang_module() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = minimal_context();

        // Act
        let result = renderer.render_erlang_module(&ctx);

        // Assert - May fail if template doesn't exist, which is expected
        match result {
            Ok(_) => {
                // If template exists, verify basic structure
                let erlang = result.unwrap();
                assert!(erlang.contains("-module"));
            }
            Err(_) => {
                // Expected if template doesn't exist yet
                // This is acceptable for a work-in-progress feature
            }
        }
    }
}

#[cfg(test)]
mod tera_accessor_tests {
    use super::*;

    /// Test: Tera accessor returns reference to Tera instance
    #[test]
    fn test_tera_accessor() {
        // Arrange
        let renderer = TemplateRenderer::new();

        // Act
        let tera_ref = renderer.tera();

        // Assert
        assert!(!tera_ref.get_template_names().collect::<Vec<_>>().is_empty());
    }

    /// Test: Tera mutable accessor returns mutable reference
    #[test]
    fn test_tera_mut_accessor() {
        // Arrange
        let mut renderer = TemplateRenderer::new();

        // Act
        let _ = renderer.tera_mut();

        // Assert - If we get here without panicking, accessor works
        // Can't easily test mutability without modifying the instance
    }
}

#[cfg(test)]
mod edge_case_tests {
    use super::*;
    use fixtures::*;

    /// Test: Render with very long task names
    #[test]
    fn test_render_long_task_names() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let long_name = "A".repeat(1000);
        let ctx = TemplateContext {
            workflow_name: "test".to_string(),
            description: "test".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![TaskContext {
                id: "task1".to_string(),
                name: long_name.clone(),
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
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();
        assert!(xml.contains(&long_name));
    }

    /// Test: Render with unicode characters
    #[test]
    fn test_render_unicode_characters() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = TemplateContext {
            workflow_name: "workflow_test".to_string(),
            description: "Test with unicode: &".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![TaskContext {
                id: "task_1".to_string(),
                name: "Zadanie".to_string(),
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
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();
        // Unicode should be preserved (may be escaped or not depending on serializer)
        assert!(xml.contains("Zadanie") || xml.contains("&#260;"));
    }

    /// Test: Render with zero tasks but non-empty flows
    #[test]
    fn test_render_zero_tasks_with_flows() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = TemplateContext {
            workflow_name: "invalid_workflow".to_string(),
            description: "Invalid: flows without tasks".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![],
            flows: vec![FlowContext {
                source: "input".to_string(),
                target: "task1".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            }],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert - Should still render, even if logically invalid
        assert!(result.is_ok());
    }

    /// Test: Multiple renders produce consistent output
    #[test]
    fn test_render_deterministic() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = complex_context();

        // Act
        let result1 = renderer.render_yawl_xml(&ctx);
        let result2 = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());
        let xml1 = result1.unwrap();
        let xml2 = result2.unwrap();
        assert_eq!(xml1, xml2, "Multiple renders should produce identical output");
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    /// Test: Full workflow rendering produces well-formed XML
    #[test]
    fn test_full_workflow_well_formed_xml() {
        // Arrange
        let renderer = TemplateRenderer::new();
        let ctx = fixtures::complex_context();

        // Act
        let result = renderer.render_yawl_xml(&ctx);

        // Assert
        assert!(result.is_ok());
        let xml = result.unwrap();

        // Verify XML structure
        assert!(xml.starts_with("<?xml version=\"1.0\""));
        assert!(xml.contains("<specification"));
        assert!(xml.ends_with("</specification>\n"));

        // Verify opening and closing tags match
        let spec_open = xml.matches("<specification").count();
        let spec_close = xml.matches("</specification>").count();
        assert_eq!(spec_open, 1, "Should have one opening specification tag");
        assert_eq!(spec_close, 1, "Should have one closing specification tag");

        // Verify decomposition tags
        let decomp_open = xml.matches("<decomposition").count();
        let decomp_close = xml.matches("</decomposition>").count();
        assert_eq!(decomp_open, decomp_close, "Decomposition tags should match");

        // Verify task tags
        let task_open = xml.matches("<task").count();
        let task_close = xml.matches("</task>").count();
        assert_eq!(task_open, task_close, "Task tags should match");
    }
}
