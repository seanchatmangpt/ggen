//! Template renderer for YAWL workflows.
//!
//! This module provides the [`TemplateRenderer`] type for rendering YAWL workflow
//! specifications from template contexts using the Tera templating engine.
//!
//! # Templates
//!
//! The renderer loads templates from the `templates/` directory:
//!
//! * `workflow.yawl.tera` - YAWL XML specification template
//! * `task.erl.tera` - Erlang task module template (optional)
//!
//! # Example
//!
//! ```rust,no_run
//! use ggen_yawl::TemplateRenderer;
//! use ggen_yawl::template::TemplateContext;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let renderer = TemplateRenderer::new();
//!
//! let context = TemplateContext {
//!     workflow_name: "MyWorkflow".to_string(),
//!     // ... other fields
//!     ..Default::default()
//! };
//!
//! let xml = renderer.render_yawl_xml(&context)?;
//! # Ok(())
//! # }
//! ```

use crate::{Error, Result};
use ggen_core::Graph;
use std::path::Path;
use tera::{Context as TeraContext, Tera};

/// Template directory for YAWL workflow templates.
const TEMPLATE_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/templates");

/// YAWL RDF namespace
const YAWL_NS: &str = "http://www.yawlfoundation.org/yawlschema";

/// Extract workflow metadata from YAWL RDF graph and convert to TemplateContext.
///
/// # Errors
///
/// Returns an error if:
/// - Required workflow properties are missing from the graph
/// - SPARQL queries fail
fn graph_to_template_context(graph: &Graph) -> Result<crate::template::TemplateContext> {
    // Query for workflow metadata
    let metadata_query = format!(
        r#"
        PREFIX yawl: <{YAWL_NS}>
        SELECT ?workflow_name ?description ?version WHERE {{
            OPTIONAL {{ ?workflow a yawl:Specification ; yawl:name ?workflow_name . }}
            OPTIONAL {{ ?workflow yawl:description ?description . }}
            OPTIONAL {{ ?workflow yawl:version ?version . }}
        }}
        LIMIT 1
        "#
    );

    let workflow_name = extract_string(graph, &metadata_query, 0)
        .unwrap_or_else(|_| "unnamed_workflow".to_string());
    let description = extract_string(graph, &metadata_query, 1)
        .unwrap_or_else(|_| "Generated workflow".to_string());
    let version = extract_string(graph, &metadata_query, 2).unwrap_or_else(|_| "1.0.0".to_string());

    // Query for tasks
    let tasks_query = format!(
        r#"
        PREFIX yawl: <{YAWL_NS}>
        SELECT ?task_id ?task_name ?split_type ?join_type ?is_auto ?decomp_id WHERE {{
            ?task a yawl:Task ;
                yawl:id ?task_id ;
                yawl:name ?task_name ;
                yawl:splitType ?split_type ;
                yawl:joinType ?join_type .
            OPTIONAL {{ ?task yawl:isAuto ?is_auto . }}
            OPTIONAL {{ ?task yawl:decompositionId ?decomp_id . }}
        }}
        "#
    );

    let tasks = extract_tasks(graph, &tasks_query)?;

    // Query for flows
    let flows_query = format!(
        r#"
        PREFIX yawl: <{YAWL_NS}>
        SELECT ?source ?target ?condition ?predicate ?is_default WHERE {{
            ?flow a yawl:Flow ;
                yawl:from ?source ;
                yawl:into ?target .
            OPTIONAL {{ ?flow yawl:condition ?condition . }}
            OPTIONAL {{ ?flow yawl:predicate ?predicate . }}
            OPTIONAL {{ ?flow yawl:isDefault ?is_default . }}
        }}
        "#
    );

    let flows = extract_flows(graph, &flows_query)?;

    Ok(crate::template::TemplateContext {
        workflow_name,
        description,
        version,
        tasks,
        flows,
        input_condition: None,
        output_condition: None,
        variables: vec![],
    })
}

/// Extract a string value from the first row of a SPARQL SELECT query at a given column index.
fn extract_string(graph: &Graph, query: &str, col_index: usize) -> Result<String> {
    let results = graph
        .query(query)
        .map_err(|e| Error::sparql(format!("Query failed: {}", e)))?;

    match results {
        oxigraph::sparql::QueryResults::Solutions(mut solutions) => {
            if let Some(solution) = solutions.next() {
                let solution =
                    solution.map_err(|e| Error::sparql(format!("Solution error: {}", e)))?;

                // Get the value at the specified column index
                let value = solution
                    .iter()
                    .nth(col_index)
                    .map(|(_, term)| term.to_string())
                    .unwrap_or_default();

                Ok(value)
            } else {
                Err(Error::sparql("No results".to_string()))
            }
        }
        _ => Err(Error::sparql("Expected SELECT results".to_string())),
    }
}

/// Extract tasks from SPARQL results.
fn extract_tasks(graph: &Graph, query: &str) -> Result<Vec<crate::template::TaskContext>> {
    use crate::template::TaskContext;

    let results = graph
        .query(query)
        .map_err(|e| Error::sparql(format!("Task query failed: {}", e)))?;

    let mut tasks = Vec::new();

    match results {
        oxigraph::sparql::QueryResults::Solutions(solutions) => {
            for solution_result in solutions {
                let solution =
                    solution_result.map_err(|e| Error::sparql(format!("Solution error: {}", e)))?;

                let id = solution
                    .get("?task_id")
                    .map(|v| v.to_string())
                    .unwrap_or_default();
                let name = solution
                    .get("?task_name")
                    .map(|v| v.to_string())
                    .unwrap_or_default();
                let split_type = solution
                    .get("?split_type")
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "XOR".to_string());
                let join_type = solution
                    .get("?join_type")
                    .map(|v| v.to_string())
                    .unwrap_or_else(|| "XOR".to_string());
                let is_auto = solution
                    .get("?is_auto")
                    .and_then(|v| {
                        // Extract boolean from Literal: "true" or "false" strings
                        use oxigraph::model::Term;
                        match v {
                            Term::Literal(lit) => {
                                let value = lit.value();
                                if value == "true" {
                                    Some(true)
                                } else if value == "false" {
                                    Some(false)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }
                    })
                    .unwrap_or(false);
                let decomposition_id = solution.get("?decomp_id").map(|v| v.to_string());

                tasks.push(TaskContext {
                    id,
                    name,
                    split_type,
                    join_type,
                    is_auto,
                    decomposition_id,
                });
            }
        }
        _ => {
            return Err(Error::sparql(
                "Expected SELECT results for tasks".to_string(),
            ))
        }
    }

    Ok(tasks)
}

/// Extract flows from SPARQL results.
fn extract_flows(graph: &Graph, query: &str) -> Result<Vec<crate::template::FlowContext>> {
    use crate::template::FlowContext;

    let results = graph
        .query(query)
        .map_err(|e| Error::sparql(format!("Flow query failed: {}", e)))?;

    let mut flows = Vec::new();

    match results {
        oxigraph::sparql::QueryResults::Solutions(solutions) => {
            for solution_result in solutions {
                let solution =
                    solution_result.map_err(|e| Error::sparql(format!("Solution error: {}", e)))?;

                let source = solution
                    .get("?source")
                    .map(|v| v.to_string())
                    .unwrap_or_default();
                let target = solution
                    .get("?target")
                    .map(|v| v.to_string())
                    .unwrap_or_default();
                let condition = solution.get("?condition").map(|v| v.to_string());
                let predicate = solution.get("?predicate").map(|v| v.to_string());
                let is_default = solution
                    .get("?is_default")
                    .and_then(|v| {
                        // Extract boolean from Literal: "true" or "false" strings
                        use oxigraph::model::Term;
                        match v {
                            Term::Literal(lit) => {
                                let value = lit.value();
                                if value == "true" {
                                    Some(true)
                                } else if value == "false" {
                                    Some(false)
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        }
                    })
                    .unwrap_or(true);

                flows.push(FlowContext {
                    source,
                    target,
                    condition,
                    predicate,
                    is_default,
                });
            }
        }
        _ => {
            return Err(Error::sparql(
                "Expected SELECT results for flows".to_string(),
            ))
        }
    }

    Ok(flows)
}

/// Renderer for YAWL workflow templates.
///
/// The [`TemplateRenderer`] uses Tera templates to generate YAWL XML and
/// optional Erlang code from workflow contexts.
///
/// # Example
///
/// ```rust,no_run
/// use ggen_yawl::TemplateRenderer;
///
/// let renderer = TemplateRenderer::new();
/// // Access underlying Tera instance for customization
/// let tera = renderer.tera();
/// ```
#[derive(Debug, Clone)]
pub struct TemplateRenderer {
    /// Tera template environment.
    tera: Tera,
}

impl Default for TemplateRenderer {
    fn default() -> Self {
        Self::new()
    }
}

impl TemplateRenderer {
    /// Create a new template renderer.
    ///
    /// This initializes Tera with the YAWL template directory and registers
    /// all necessary filters for code generation.
    ///
    /// # Panics
    ///
    /// Panics if the template directory cannot be read or no templates are found.
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::TemplateRenderer;
    ///
    /// let renderer = TemplateRenderer::new();
    /// assert!(renderer.tera().get_template_names().count() > 0);
    /// ```
    pub fn new() -> Self {
        let template_glob = format!("{}/{}", TEMPLATE_DIR, "*.tera");

        // Build Tera instance with template glob support
        let mut tera = Tera::new(&template_glob).unwrap_or_else(|e| {
            panic!(
                "Failed to initialize Tera with glob '{}': {}",
                template_glob, e
            )
        });

        // Disable auto-escaping for code generation (we want literal output)
        tera.autoescape_on(vec![]);

        Self { tera }
    }

    /// Create a new template renderer with a custom template directory.
    ///
    /// # Arguments
    ///
    /// * `dir` - Path to the directory containing `.tera` template files
    ///
    /// # Errors
    ///
    /// Returns an error if the template directory cannot be read or
    /// no templates are found.
    ///
    /// # Example
    ///
    /// ```rust,no_run
    /// use ggen_yawl::TemplateRenderer;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let renderer = TemplateRenderer::with_template_dir("./my_templates")?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_template_dir<P: AsRef<Path>>(dir: P) -> Result<Self> {
        let dir_ref = dir.as_ref();
        let template_glob = format!("{}/{}", dir_ref.display(), "*.tera");

        let mut tera = Tera::new(&template_glob).map_err(|e| {
            Error::template(format!(
                "Failed to load templates from '{}': {}",
                dir_ref.display(),
                e
            ))
        })?;

        // Disable auto-escaping for code generation
        tera.autoescape_on(vec![]);

        Ok(Self { tera })
    }

    /// Render YAWL XML from template context.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The template cannot be found
    /// - Template rendering fails
    /// - Context variables are missing
    pub fn render_yawl_xml(&self, ctx: &crate::template::TemplateContext) -> Result<String> {
        let template_name = "workflow.yawl.tera";

        // Build Tera context from template context
        let mut tera_ctx = TeraContext::new();
        tera_ctx.insert("workflow_name", &ctx.workflow_name);
        tera_ctx.insert("description", &ctx.description);
        tera_ctx.insert("version", &ctx.version);
        tera_ctx.insert("tasks", &ctx.tasks);
        tera_ctx.insert("flows", &ctx.flows);

        // Render the template
        self.tera
            .render(template_name, &tera_ctx)
            .map_err(|e| Error::template(format!("Failed to render '{}': {}", template_name, e)))
    }

    /// Render YAWL XML from a YAWL RDF graph.
    ///
    /// This method extracts workflow metadata, tasks, and flows from the RDF graph
    /// using SPARQL queries, then renders the YAWL XML template.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - SPARQL queries fail
    /// - The template cannot be found
    /// - Template rendering fails
    pub fn render_yawl_xml_from_graph(&self, graph: &Graph) -> Result<String> {
        let ctx = graph_to_template_context(graph)?;
        self.render_yawl_xml(&ctx)
    }

    /// Render Erlang module from template context.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The template cannot be found
    /// - Template rendering fails
    /// - Context variables are missing
    ///
    /// # Note
    ///
    /// This method currently requires a `task.erl.tera` template which
    /// may not exist yet. Returns a template error if the file is missing.
    pub fn render_erlang_module(&self, ctx: &crate::template::TemplateContext) -> Result<String> {
        let template_name = "task.erl.tera";

        // Build Tera context from template context
        let mut tera_ctx = TeraContext::new();
        tera_ctx.insert("workflow_name", &ctx.workflow_name);
        tera_ctx.insert("tasks", &ctx.tasks);

        // Render the template
        self.tera
            .render(template_name, &tera_ctx)
            .map_err(|e| Error::template(format!("Failed to render '{}': {}", template_name, e)))
    }

    /// Get the underlying Tera instance for custom rendering.
    pub fn tera(&self) -> &Tera {
        &self.tera
    }

    /// Get mutable reference to the underlying Tera instance.
    pub fn tera_mut(&mut self) -> &mut Tera {
        &mut self.tera
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::template::{FlowContext, TaskContext, TemplateContext};

    #[test]
    fn test_renderer_creation() {
        let renderer = TemplateRenderer::new();
        // Verify renderer has templates loaded
        assert!(renderer.tera().get_template_names().count() > 0);
    }

    #[test]
    fn test_renderer_default() {
        let renderer = TemplateRenderer::default();
        assert!(renderer.tera().get_template_names().count() > 0);
    }

    #[test]
    fn test_render_yawl_xml_basic() {
        let renderer = TemplateRenderer::new();

        let ctx = TemplateContext {
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
        };

        let result = renderer.render_yawl_xml(&ctx);
        assert!(
            result.is_ok(),
            "render_yawl_xml should succeed: {:?}",
            result.err()
        );

        let xml = result.unwrap();
        assert!(xml.contains("<?xml version=\"1.0\""));
        assert!(xml.contains("<specification"));
        assert!(xml.contains("test_workflow"));
        assert!(xml.contains("First Task"));
    }

    #[test]
    fn test_render_yawl_xml_multiple_tasks() {
        let renderer = TemplateRenderer::new();

        let ctx = TemplateContext {
            workflow_name: "multi_task_workflow".to_string(),
            description: "Multi-task workflow".to_string(),
            version: "2.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "task1".to_string(),
                    name: "Start".to_string(),
                    split_type: "AND".to_string(),
                    join_type: "AND".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "task2".to_string(),
                    name: "Process".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: false,
                    decomposition_id: Some("decomp1".to_string()),
                },
                TaskContext {
                    id: "task3".to_string(),
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
                    target: "task1".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
                FlowContext {
                    source: "task1".to_string(),
                    target: "task2".to_string(),
                    condition: Some("true".to_string()),
                    predicate: Some("x > 0".to_string()),
                    is_default: false,
                },
            ],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        let result = renderer.render_yawl_xml(&ctx);
        assert!(result.is_ok());

        let xml = result.unwrap();
        assert!(xml.contains("multi_task_workflow"));
        assert!(xml.contains("Start"));
        assert!(xml.contains("Process"));
        assert!(xml.contains("End"));
    }

    #[test]
    fn test_render_empty_workflow() {
        let renderer = TemplateRenderer::new();

        let ctx = TemplateContext {
            workflow_name: "empty_workflow".to_string(),
            description: "Empty workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![],
            flows: vec![],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        let result = renderer.render_yawl_xml(&ctx);
        assert!(result.is_ok());

        let xml = result.unwrap();
        assert!(xml.contains("empty_workflow"));
    }

    #[test]
    fn test_with_template_dir_valid() {
        let result = TemplateRenderer::with_template_dir(TEMPLATE_DIR);
        assert!(result.is_ok());
        let renderer = result.unwrap();
        assert!(renderer.tera().get_template_names().count() > 0);
    }

    #[test]
    fn test_tera_accessors() {
        let renderer = TemplateRenderer::new();

        // Test immutable accessor
        let tera_ref = renderer.tera();
        assert!(tera_ref.get_template_names().count() > 0);

        // Test mutable accessor
        let mut renderer_mut = TemplateRenderer::new();
        let _ = renderer_mut.tera_mut();
        // If we get here without panicking, the accessor works
    }

    #[test]
    fn test_render_with_conditions() {
        let renderer = TemplateRenderer::new();

        let ctx = TemplateContext {
            workflow_name: "conditional_workflow".to_string(),
            description: "Workflow with conditions".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![TaskContext {
                id: "task1".to_string(),
                name: "Conditional Task".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            }],
            flows: vec![
                FlowContext {
                    source: "input".to_string(),
                    target: "task1".to_string(),
                    condition: Some("approved".to_string()),
                    predicate: Some("status == 'approved'".to_string()),
                    is_default: false,
                },
                FlowContext {
                    source: "input".to_string(),
                    target: "task1".to_string(),
                    condition: None,
                    predicate: None,
                    is_default: true,
                },
            ],
            input_condition: None,
            output_condition: None,
            variables: vec![],
        };

        let result = renderer.render_yawl_xml(&ctx);
        assert!(result.is_ok());

        let xml = result.unwrap();
        assert!(xml.contains("conditional_workflow"));
    }
}
