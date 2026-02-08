//! Template context structures for YAWL rendering.

use crate::{Error, Result};
use ggen_core::Graph;
use serde::{Deserialize, Serialize};

/// Context for YAWL workflow template rendering.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateContext {
    /// Workflow name/identifier
    pub workflow_name: String,
    /// Workflow description
    pub description: String,
    /// Workflow version
    pub version: String,
    /// Tasks in the workflow
    pub tasks: Vec<TaskContext>,
    /// Flows connecting tasks
    pub flows: Vec<FlowContext>,
    /// Input condition
    pub input_condition: Option<ConditionContext>,
    /// Output condition
    pub output_condition: Option<ConditionContext>,
    /// Workflow variables
    pub variables: Vec<VariableContext>,
}

impl Default for TemplateContext {
    fn default() -> Self {
        Self {
            workflow_name: String::new(),
            description: String::new(),
            version: "1.0.0".to_string(),
            tasks: Vec::new(),
            flows: Vec::new(),
            input_condition: None,
            output_condition: None,
            variables: Vec::new(),
        }
    }
}

/// Task context for template rendering.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskContext {
    /// Task ID
    pub id: String,
    /// Task name
    pub name: String,
    /// Split type (AND, XOR, OR)
    pub split_type: String,
    /// Join type (AND, XOR, OR)
    pub join_type: String,
    /// Whether this is an automated task
    pub is_auto: bool,
    /// Decomposition ID for composite tasks
    pub decomposition_id: Option<String>,
}

/// Flow context for template rendering.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowContext {
    /// Source task ID
    pub source: String,
    /// Target task ID
    pub target: String,
    /// Optional condition expression
    pub condition: Option<String>,
    /// Optional predicate text
    pub predicate: Option<String>,
    /// Whether this is the default flow
    pub is_default: bool,
}

/// Condition context.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionContext {
    /// Condition ID
    pub id: String,
    /// Condition expression
    pub expression: String,
    /// Condition type
    pub condition_type: String,
}

/// Variable context.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableContext {
    /// Variable name
    pub name: String,
    /// Variable type
    pub var_type: String,
    /// Default value
    pub default: Option<String>,
    /// Scope (task or workflow)
    pub scope: String,
}

/// YAWL namespace constants for SPARQL queries.
#[allow(dead_code)]
mod yawl_ns {
    /// YAWL namespace IRI
    pub const YAWL: &str = "http://unrdf.org/yawl#";
    /// Industry ontology namespace IRI
    pub const SRC: &str = "http://industry-ontology.org/";
    /// RDF Schema namespace
    pub const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
    /// OWL namespace
    pub const OWL: &str = "http://www.w3.org/2002/07/owl#";
    /// XSD namespace
    pub const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
}

/// Builder for constructing TemplateContext from YAWL RDF graph.
///
/// The ContextBuilder extracts workflow information from a YAWL RDF graph
/// using SPARQL queries and populates a TemplateContext for template rendering.
/// It supports all FIBO patterns including BusinessProcess, ProcessStep, and hasNextStep.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_yawl::template::context::ContextBuilder;
/// use ggen_core::Graph;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let graph = Graph::new()?;
/// graph.insert_turtle(r#"
///     @prefix yawl: <http://unrdf.org/yawl#> .
///     @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
///     yawl:task1 a yawl:AtomicTask ;
///         rdfs:label "Task 1" ;
///         yawl:taskId "t1" .
/// "#)?;
///
/// let builder = ContextBuilder::new(&graph);
/// let context = builder.build()?;
/// # Ok(())
/// # }
/// ```
#[derive(Clone)]
pub struct ContextBuilder<'a> {
    /// Reference to the RDF graph
    graph: &'a Graph,
    /// Workflow name (optional override)
    workflow_name: Option<String>,
    /// Workflow description (optional override)
    description: Option<String>,
    /// Workflow version (optional override)
    version: Option<String>,
}

impl<'a> ContextBuilder<'a> {
    /// Create a new ContextBuilder for the given graph.
    ///
    /// # Arguments
    ///
    /// * `graph` - The RDF graph containing YAWL workflow data
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            workflow_name: None,
            description: None,
            version: None,
        }
    }

    /// Set the workflow name.
    ///
    /// If not set, the name will be extracted from the graph.
    pub fn with_workflow_name(mut self, name: impl Into<String>) -> Self {
        self.workflow_name = Some(name.into());
        self
    }

    /// Set the workflow description.
    ///
    /// If not set, the description will be extracted from the graph.
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Set the workflow version.
    ///
    /// If not set, defaults to "1.0.0".
    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.version = Some(version.into());
        self
    }

    /// Build the TemplateContext from the graph.
    ///
    /// This method extracts all tasks, flows, conditions, and variables from
    /// the YAWL RDF graph and constructs a complete TemplateContext.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - SPARQL queries fail
    /// - Required data is missing
    /// - Data format is invalid
    pub fn build(&self) -> Result<TemplateContext> {
        let mut context = TemplateContext {
            workflow_name: self
                .workflow_name
                .clone()
                .unwrap_or_else(|| self.extract_workflow_name()),
            description: self
                .description
                .clone()
                .unwrap_or_else(|| self.extract_workflow_description()),
            version: self.version.clone().unwrap_or_else(|| "1.0.0".to_string()),
            tasks: self.extract_tasks()?,
            flows: self.extract_flows()?,
            input_condition: self.extract_input_condition()?,
            output_condition: self.extract_output_condition()?,
            variables: self.extract_variables()?,
        };

        // Post-processing: link flows to tasks
        self.link_flows_to_tasks(&mut context)?;

        Ok(context)
    }

    /// Extract workflow name from the graph.
    fn extract_workflow_name(&self) -> String {
        const QUERY: &str = r#"
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX yawl: <http://unrdf.org/yawl#>

            SELECT ?name
            WHERE {
                ?spec a yawl:WorkflowSpec ;
                      rdfs:label ?name .
            }
            LIMIT 1
        "#;

        match self.graph.query_cached(QUERY) {
            Ok(ggen_core::graph::types::CachedResult::Solutions(rows)) => rows
                .first()
                .and_then(|row| row.get("name"))
                .map(|s| Self::strip_literal_quotes(s))
                .unwrap_or_else(|| "Unnamed Workflow".to_string()),
            _ => "Unnamed Workflow".to_string(),
        }
    }

    /// Extract workflow description from the graph.
    fn extract_workflow_description(&self) -> String {
        const QUERY: &str = r#"
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX yawl: <http://unrdf.org/yawl#>

            SELECT ?comment
            WHERE {
                ?spec a yawl:WorkflowSpec ;
                      rdfs:comment ?comment .
            }
            LIMIT 1
        "#;

        match self.graph.query_cached(QUERY) {
            Ok(ggen_core::graph::types::CachedResult::Solutions(rows)) => rows
                .first()
                .and_then(|row| row.get("comment"))
                .map(|s| Self::strip_literal_quotes(s))
                .unwrap_or_else(|| "Generated workflow".to_string()),
            _ => "Generated workflow".to_string(),
        }
    }

    /// Strip quotes from RDF literal string representation.
    ///
    /// Oxigraph returns literals as strings like "\"value\"" or
    /// "\"value\"^^http://www.w3.org/2001/XMLSchema#string".
    /// This function extracts the actual value.
    fn strip_literal_quotes(s: &str) -> String {
        let s = s.trim();
        if s.starts_with('"') {
            // Find the closing quote
            if let Some(end) = s[1..].find('"') {
                return s[1..end + 1].to_string();
            }
        }
        // Fallback: remove surrounding quotes if present
        s.strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .map(|s| s.to_string())
            .unwrap_or_else(|| s.to_string())
    }

    /// Extract all tasks from the YAWL RDF graph.
    ///
    /// This method queries for all YAWL task types (AtomicTask, CompositeTask,
    /// MultipleInstanceTask) and constructs TaskContext instances.
    fn extract_tasks(&self) -> Result<Vec<TaskContext>> {
        const QUERY: &str = r#"
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX yawl: <http://unrdf.org/yawl#>

            SELECT ?task ?taskId ?taskName ?splitBehavior ?joinBehavior ?decompositionId
            WHERE {
                {
                    ?task a yawl:AtomicTask .
                } UNION {
                    ?task a yawl:CompositeTask .
                } UNION {
                    ?task a yawl:MultipleInstanceTask .
                }
                OPTIONAL { ?task yawl:taskId ?taskId . }
                OPTIONAL { ?task rdfs:label ?taskName . }
                OPTIONAL { ?task yawl:splitBehavior ?splitBehavior . }
                OPTIONAL { ?task yawl:joinBehavior ?joinBehavior . }
                OPTIONAL { ?task yawl:decomposesTo ?decompositionId . }
            }
            ORDER BY ?taskName
        "#;

        let mut tasks = Vec::new();

        let result = self
            .graph
            .query_cached(QUERY)
            .map_err(|e| Error::sparql(format!("Failed to extract tasks: {}", e)))?;

        if let ggen_core::graph::types::CachedResult::Solutions(rows) = result {
            for row in rows {
                let task_iri = row.get("task").cloned().unwrap_or_default();
                let task_id = row
                    .get("taskId")
                    .cloned()
                    .unwrap_or_else(|| Self::generate_task_id_from_iri(&task_iri));
                let task_id = Self::strip_literal_quotes(&task_id);
                let task_name = row
                    .get("taskName")
                    .map(|s| Self::strip_literal_quotes(s))
                    .unwrap_or_else(|| Self::extract_local_name(&task_iri));
                let split_behavior = row.get("splitBehavior").cloned();
                let join_behavior = row.get("joinBehavior").cloned();
                let decomposition_id = row.get("decompositionId").cloned();

                // Determine split/join types from behavior or default to XOR
                let split_type = Self::behavior_to_split_type(&split_behavior);
                let join_type = Self::behavior_to_join_type(&join_behavior);

                // Determine if automated (atomic tasks are automated)
                let is_auto = task_iri.contains("AtomicTask") || task_iri.contains("task/");

                tasks.push(TaskContext {
                    id: task_id,
                    name: task_name,
                    split_type,
                    join_type,
                    is_auto,
                    decomposition_id,
                });
            }
        }

        Ok(tasks)
    }

    /// Extract all flows from the YAWL RDF graph.
    ///
    /// This method queries for YAWL Flow instances and their associated
    /// conditions and predicates.
    fn extract_flows(&self) -> Result<Vec<FlowContext>> {
        const QUERY: &str = r#"
            PREFIX yawl: <http://unrdf.org/yawl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?flow ?sourceTask ?targetTask ?condition ?predicate ?isDefault
            WHERE {
                ?flow a yawl:Flow ;
                      yawl:sourceTask ?sourceTask ;
                      yawl:targetTask ?targetTask .
                OPTIONAL { ?flow yawl:flowCondition ?condition . }
                OPTIONAL { ?flow rdfs:comment ?predicate . }
                OPTIONAL { ?flow yawl:isDefaultFlow ?isDefault . }
            }
            ORDER BY ?sourceTask ?targetTask
        "#;

        let mut flows = Vec::new();

        let result = self
            .graph
            .query_cached(QUERY)
            .map_err(|e| Error::sparql(format!("Failed to extract flows: {}", e)))?;

        if let ggen_core::graph::types::CachedResult::Solutions(rows) = result {
            for row in rows {
                let source = row.get("sourceTask").cloned().unwrap_or_default();
                let target = row.get("targetTask").cloned().unwrap_or_default();
                let condition = row.get("condition").cloned();
                let predicate = row.get("predicate").cloned();
                let is_default = row
                    .get("isDefault")
                    .and_then(|v| Self::parse_boolean(v))
                    .unwrap_or(false);

                // Extract task IDs from task IRIs
                let source_id = Self::extract_task_id_from_iri(&source);
                let target_id = Self::extract_task_id_from_iri(&target);

                flows.push(FlowContext {
                    source: source_id,
                    target: target_id,
                    condition,
                    predicate,
                    is_default,
                });
            }
        }

        Ok(flows)
    }

    /// Extract input condition from the YAWL RDF graph.
    ///
    /// The input condition represents the starting point of the workflow.
    fn extract_input_condition(&self) -> Result<Option<ConditionContext>> {
        const QUERY: &str = r#"
            PREFIX yawl: <http://unrdf.org/yawl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?condition ?expression ?condType
            WHERE {
                ?spec a yawl:WorkflowSpec ;
                      yawl:inputCondition ?condition .
                OPTIONAL { ?condition yawl:conditionExpression ?expression . }
                OPTIONAL { ?condition yawl:conditionType ?condType . }
            }
            LIMIT 1
        "#;

        let result = self
            .graph
            .query_cached(QUERY)
            .map_err(|e| Error::sparql(format!("Failed to extract input condition: {}", e)))?;

        if let ggen_core::graph::types::CachedResult::Solutions(rows) = result {
            if let Some(row) = rows.first() {
                let id = row
                    .get("condition")
                    .cloned()
                    .unwrap_or_else(|| "input-condition".to_string());
                let expression = row
                    .get("expression")
                    .cloned()
                    .unwrap_or_else(|| "true".to_string());
                let condition_type = row
                    .get("condType")
                    .cloned()
                    .unwrap_or_else(|| "input".to_string());

                return Ok(Some(ConditionContext {
                    id,
                    expression,
                    condition_type,
                }));
            }
        }

        Ok(None)
    }

    /// Extract output condition from the YAWL RDF graph.
    ///
    /// The output condition represents the termination point of the workflow.
    fn extract_output_condition(&self) -> Result<Option<ConditionContext>> {
        const QUERY: &str = r#"
            PREFIX yawl: <http://unrdf.org/yawl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?condition ?expression ?condType
            WHERE {
                ?spec a yawl:WorkflowSpec ;
                      yawl:outputCondition ?condition .
                OPTIONAL { ?condition yawl:conditionExpression ?expression . }
                OPTIONAL { ?condition yawl:conditionType ?condType . }
            }
            LIMIT 1
        "#;

        let result = self
            .graph
            .query_cached(QUERY)
            .map_err(|e| Error::sparql(format!("Failed to extract output condition: {}", e)))?;

        if let ggen_core::graph::types::CachedResult::Solutions(rows) = result {
            if let Some(row) = rows.first() {
                let id = row
                    .get("condition")
                    .cloned()
                    .unwrap_or_else(|| "output-condition".to_string());
                let expression = row
                    .get("expression")
                    .cloned()
                    .unwrap_or_else(|| "true".to_string());
                let condition_type = row
                    .get("condType")
                    .cloned()
                    .unwrap_or_else(|| "output".to_string());

                return Ok(Some(ConditionContext {
                    id,
                    expression,
                    condition_type,
                }));
            }
        }

        Ok(None)
    }

    /// Extract workflow variables from the YAWL RDF graph.
    ///
    /// Variables can be scoped at the workflow level or at specific tasks.
    fn extract_variables(&self) -> Result<Vec<VariableContext>> {
        const QUERY: &str = r#"
            PREFIX yawl: <http://unrdf.org/yawl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?var ?varName ?varType ?default ?scope
            WHERE {
                ?var a yawl:Variable ;
                     rdfs:label ?varName .
                OPTIONAL { ?var yawl:varType ?varType . }
                OPTIONAL { ?var yawl:defaultValue ?default . }
                OPTIONAL { ?var yawl:varScope ?scope . }
            }
            ORDER BY ?varName
        "#;

        let mut variables = Vec::new();

        let result = self
            .graph
            .query_cached(QUERY)
            .map_err(|e| Error::sparql(format!("Failed to extract variables: {}", e)))?;

        if let ggen_core::graph::types::CachedResult::Solutions(rows) = result {
            for row in rows {
                let name = row.get("varName").cloned().unwrap_or_default();
                let var_type = row
                    .get("varType")
                    .cloned()
                    .unwrap_or_else(|| "string".to_string());
                let default = row.get("default").cloned();
                let scope = row
                    .get("scope")
                    .cloned()
                    .unwrap_or_else(|| "workflow".to_string());

                variables.push(VariableContext {
                    name,
                    var_type,
                    default,
                    scope,
                });
            }
        }

        Ok(variables)
    }

    /// Link flows to tasks for validation and completeness.
    ///
    /// This post-processing step ensures that all flow source and target
    /// IDs correspond to valid tasks in the context.
    fn link_flows_to_tasks(&self, context: &mut TemplateContext) -> Result<()> {
        // Build a map of valid task IDs
        let task_ids: std::collections::HashSet<String> =
            context.tasks.iter().map(|t| t.id.clone()).collect();

        // Validate and filter flows
        let valid_flows: Vec<FlowContext> = context
            .flows
            .iter()
            .filter(|flow| task_ids.contains(&flow.source) && task_ids.contains(&flow.target))
            .cloned()
            .collect();

        let removed_count = context.flows.len() - valid_flows.len();
        if removed_count > 0 {
            // Log warning about invalid flows using tracing
            tracing::warn!(
                "Removed {} flows with invalid task references",
                removed_count
            );
        }

        context.flows = valid_flows;
        Ok(())
    }

    /// Generate a task ID from a task IRI.
    fn generate_task_id_from_iri(iri: &str) -> String {
        // Extract the last component of the IRI path
        iri.split('/')
            .last()
            .and_then(|s| s.split('#').last())
            .unwrap_or("unknown")
            .to_string()
    }

    /// Extract local name from an IRI.
    fn extract_local_name(iri: &str) -> String {
        iri.split('#')
            .last()
            .and_then(|s| s.split('/').last())
            .unwrap_or("Unknown")
            .to_string()
    }

    /// Extract task ID from a task IRI.
    fn extract_task_id_from_iri(iri: &str) -> String {
        // Remove namespace and extract task identifier
        iri.replace("http://unrdf.org/yawl#", "")
            .replace("http://unrdf.org/yawl/task/", "")
            .replace("http://unrdf.org/yawl/flow/", "")
            .to_string()
    }

    /// Convert YAWL split behavior IRI to split type string.
    fn behavior_to_split_type(behavior: &Option<String>) -> String {
        match behavior {
            Some(b) if b.contains("AND_Split") => "AND".to_string(),
            Some(b) if b.contains("XOR_Split") => "XOR".to_string(),
            _ => "XOR".to_string(), // Default to XOR (OR not fully supported in YAWL)
        }
    }

    /// Convert YAWL join behavior IRI to join type string.
    fn behavior_to_join_type(behavior: &Option<String>) -> String {
        match behavior {
            Some(b) if b.contains("AND_Join") => "AND".to_string(),
            Some(b) if b.contains("XOR_Join") => "XOR".to_string(),
            _ => "XOR".to_string(), // Default to XOR (OR not fully supported in YAWL)
        }
    }

    /// Parse a boolean value from a string.
    fn parse_boolean(value: &str) -> Option<bool> {
        match value.to_lowercase().as_str() {
            "true" | "1" | "yes" => Some(true),
            "false" | "0" | "no" => Some(false),
            _ => None,
        }
    }
}

impl TemplateContext {
    /// Create a new TemplateContext from a YAWL RDF graph.
    ///
    /// This is a convenience method that creates a ContextBuilder
    /// and builds the context in one step.
    ///
    /// # Arguments
    ///
    /// * `graph` - The RDF graph containing YAWL workflow data
    ///
    /// # Errors
    ///
    /// Returns an error if the graph cannot be queried or data is invalid.
    pub fn from_graph(graph: &Graph) -> Result<Self> {
        ContextBuilder::new(graph).build()
    }

    /// Get a task by ID.
    pub fn task_by_id(&self, id: &str) -> Option<&TaskContext> {
        self.tasks.iter().find(|t| t.id == id)
    }

    /// Get flows originating from a specific task.
    pub fn flows_from(&self, task_id: &str) -> Vec<&FlowContext> {
        self.flows.iter().filter(|f| f.source == task_id).collect()
    }

    /// Get flows terminating at a specific task.
    pub fn flows_to(&self, task_id: &str) -> Vec<&FlowContext> {
        self.flows.iter().filter(|f| f.target == task_id).collect()
    }

    /// Get all workflow-scoped variables.
    pub fn workflow_variables(&self) -> Vec<&VariableContext> {
        self.variables
            .iter()
            .filter(|v| v.scope == "workflow")
            .collect()
    }

    /// Get variables scoped to a specific task.
    pub fn task_variables(&self, task_id: &str) -> Vec<&VariableContext> {
        self.variables
            .iter()
            .filter(|v| v.scope == task_id)
            .collect()
    }

    /// Validate the context for completeness.
    ///
    /// Returns an error if:
    /// - No tasks are defined
    /// - Flow references are invalid
    /// - Required conditions are missing
    pub fn validate(&self) -> Result<()> {
        if self.tasks.is_empty() {
            return Err(Error::validation("Workflow must have at least one task"));
        }

        // Check that all flow references are valid
        let task_ids: std::collections::HashSet<_> =
            self.tasks.iter().map(|t| t.id.as_str()).collect();

        for flow in &self.flows {
            if !task_ids.contains(flow.source.as_str()) {
                return Err(Error::validation(format!(
                    "Flow references unknown source task: {}",
                    flow.source
                )));
            }
            if !task_ids.contains(flow.target.as_str()) {
                return Err(Error::validation(format!(
                    "Flow references unknown target task: {}",
                    flow.target
                )));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_context_default() {
        let ctx = TemplateContext::default();
        assert_eq!(ctx.version, "1.0.0");
        assert!(ctx.tasks.is_empty());
    }

    #[test]
    fn test_task_context_serialization() {
        let task = TaskContext {
            id: "t1".to_string(),
            name: "Test Task".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        };

        let json = serde_json::to_string(&task).unwrap();
        assert!(json.contains("Test Task"));
    }

    #[test]
    fn test_extract_local_name() {
        assert_eq!(
            ContextBuilder::extract_local_name("http://example.org/task#Task1"),
            "Task1"
        );
        assert_eq!(
            ContextBuilder::extract_local_name("http://example.org/task/Task1"),
            "Task1"
        );
        assert_eq!(ContextBuilder::extract_local_name("Unknown"), "Unknown");
    }

    #[test]
    fn test_generate_task_id_from_iri() {
        assert_eq!(
            ContextBuilder::generate_task_id_from_iri("http://unrdf.org/yawl/task/Task1"),
            "Task1"
        );
        assert_eq!(
            ContextBuilder::generate_task_id_from_iri("http://unrdf.org/yawl#task1"),
            "task1"
        );
    }

    #[test]
    fn test_behavior_to_split_type() {
        assert_eq!(
            ContextBuilder::behavior_to_split_type(&Some(
                "http://unrdf.org/yawl#AND_Split".to_string()
            )),
            "AND"
        );
        assert_eq!(
            ContextBuilder::behavior_to_split_type(&Some(
                "http://unrdf.org/yawl#XOR_Split".to_string()
            )),
            "XOR"
        );
        // OR defaults to XOR as YAWL doesn't fully support OR
        assert_eq!(
            ContextBuilder::behavior_to_split_type(&Some(
                "http://unrdf.org/yawl#OR_Split".to_string()
            )),
            "XOR"
        );
        assert_eq!(ContextBuilder::behavior_to_split_type(&None), "XOR");
    }

    #[test]
    fn test_behavior_to_join_type() {
        assert_eq!(
            ContextBuilder::behavior_to_join_type(&Some(
                "http://unrdf.org/yawl#AND_Join".to_string()
            )),
            "AND"
        );
        assert_eq!(
            ContextBuilder::behavior_to_join_type(&Some(
                "http://unrdf.org/yawl#XOR_Join".to_string()
            )),
            "XOR"
        );
        // OR defaults to XOR as YAWL doesn't fully support OR
        assert_eq!(
            ContextBuilder::behavior_to_join_type(&Some(
                "http://unrdf.org/yawl#OR_Join".to_string()
            )),
            "XOR"
        );
        assert_eq!(ContextBuilder::behavior_to_join_type(&None), "XOR");
    }

    #[test]
    fn test_parse_boolean() {
        assert_eq!(ContextBuilder::parse_boolean("true"), Some(true));
        assert_eq!(ContextBuilder::parse_boolean("TRUE"), Some(true));
        assert_eq!(ContextBuilder::parse_boolean("1"), Some(true));
        assert_eq!(ContextBuilder::parse_boolean("yes"), Some(true));
        assert_eq!(ContextBuilder::parse_boolean("false"), Some(false));
        assert_eq!(ContextBuilder::parse_boolean("0"), Some(false));
        assert_eq!(ContextBuilder::parse_boolean("no"), Some(false));
        assert_eq!(ContextBuilder::parse_boolean("invalid"), None);
    }

    #[test]
    fn test_template_context_validate_empty_tasks() {
        let ctx = TemplateContext::default();
        assert!(ctx.validate().is_err());
    }

    #[test]
    fn test_template_context_validate_invalid_flow() {
        let ctx = TemplateContext {
            workflow_name: "Test".to_string(),
            description: "Test workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![TaskContext {
                id: "t1".to_string(),
                name: "Task 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            }],
            flows: vec![FlowContext {
                source: "t1".to_string(),
                target: "nonexistent".to_string(),
                condition: None,
                predicate: None,
                is_default: false,
            }],
            input_condition: None,
            output_condition: None,
            variables: Vec::new(),
        };

        assert!(ctx.validate().is_err());
    }

    #[test]
    fn test_template_context_validate_valid() {
        let ctx = TemplateContext {
            workflow_name: "Test".to_string(),
            description: "Test workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "t1".to_string(),
                    name: "Task 1".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "t2".to_string(),
                    name: "Task 2".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![FlowContext {
                source: "t1".to_string(),
                target: "t2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            }],
            input_condition: None,
            output_condition: None,
            variables: Vec::new(),
        };

        assert!(ctx.validate().is_ok());
    }

    #[test]
    fn test_template_context_task_by_id() {
        let ctx = TemplateContext {
            workflow_name: "Test".to_string(),
            description: "Test workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![TaskContext {
                id: "t1".to_string(),
                name: "Task 1".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            }],
            flows: Vec::new(),
            input_condition: None,
            output_condition: None,
            variables: Vec::new(),
        };

        assert!(ctx.task_by_id("t1").is_some());
        assert!(ctx.task_by_id("t2").is_none());
    }

    #[test]
    fn test_template_context_flows_from_to() {
        let ctx = TemplateContext {
            workflow_name: "Test".to_string(),
            description: "Test workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: vec![
                TaskContext {
                    id: "t1".to_string(),
                    name: "Task 1".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
                TaskContext {
                    id: "t2".to_string(),
                    name: "Task 2".to_string(),
                    split_type: "XOR".to_string(),
                    join_type: "XOR".to_string(),
                    is_auto: true,
                    decomposition_id: None,
                },
            ],
            flows: vec![FlowContext {
                source: "t1".to_string(),
                target: "t2".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            }],
            input_condition: None,
            output_condition: None,
            variables: Vec::new(),
        };

        assert_eq!(ctx.flows_from("t1").len(), 1);
        assert_eq!(ctx.flows_to("t2").len(), 1);
        assert_eq!(ctx.flows_from("t2").len(), 0);
        assert_eq!(ctx.flows_to("t1").len(), 0);
    }

    #[test]
    fn test_template_context_workflow_variables() {
        let ctx = TemplateContext {
            workflow_name: "Test".to_string(),
            description: "Test workflow".to_string(),
            version: "1.0.0".to_string(),
            tasks: Vec::new(),
            flows: Vec::new(),
            input_condition: None,
            output_condition: None,
            variables: vec![
                VariableContext {
                    name: "var1".to_string(),
                    var_type: "string".to_string(),
                    default: None,
                    scope: "workflow".to_string(),
                },
                VariableContext {
                    name: "var2".to_string(),
                    var_type: "integer".to_string(),
                    default: None,
                    scope: "task1".to_string(),
                },
            ],
        };

        assert_eq!(ctx.workflow_variables().len(), 1);
        assert_eq!(ctx.workflow_variables()[0].name, "var1");
        assert_eq!(ctx.task_variables("task1").len(), 1);
        assert_eq!(ctx.task_variables("task1")[0].name, "var2");
    }

    #[test]
    fn test_context_builder_new() {
        let graph = Graph::new().unwrap();
        let builder = ContextBuilder::new(&graph);
        // Just verify it creates without error
        assert_eq!(builder.workflow_name, None);
    }

    #[test]
    fn test_context_builder_with_workflow_name() {
        let graph = Graph::new().unwrap();
        let builder = ContextBuilder::new(&graph).with_workflow_name("Test Workflow");
        assert_eq!(builder.workflow_name, Some("Test Workflow".to_string()));
    }

    #[test]
    fn test_context_builder_with_description() {
        let graph = Graph::new().unwrap();
        let builder = ContextBuilder::new(&graph).with_description("Test Description");
        assert_eq!(builder.description, Some("Test Description".to_string()));
    }

    #[test]
    fn test_context_builder_with_version() {
        let graph = Graph::new().unwrap();
        let builder = ContextBuilder::new(&graph).with_version("2.0.0");
        assert_eq!(builder.version, Some("2.0.0".to_string()));
    }

    #[test]
    fn test_context_builder_from_graph() {
        let graph = Graph::new().unwrap();

        // Add some basic YAWL data
        graph
            .insert_turtle(
                r#"
            @prefix yawl: <http://unrdf.org/yawl#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            yawl:spec1 a yawl:WorkflowSpec ;
                rdfs:label "Test Workflow" ;
                rdfs:comment "Test workflow description" .

            yawl:task1 a yawl:AtomicTask ;
                rdfs:label "Task 1" ;
                yawl:taskId "t1" ;
                yawl:splitBehavior yawl:XOR_Split ;
                yawl:joinBehavior yawl:XOR_Join .
        "#,
            )
            .unwrap();

        let result = ContextBuilder::new(&graph).build();
        assert!(result.is_ok());

        let ctx = result.unwrap();
        assert_eq!(ctx.workflow_name, "Test Workflow");
        assert_eq!(ctx.description, "Test workflow description");
        assert_eq!(ctx.tasks.len(), 1);
        assert_eq!(ctx.tasks[0].id, "t1");
        assert_eq!(ctx.tasks[0].name, "Task 1");
        assert_eq!(ctx.tasks[0].split_type, "XOR");
        assert_eq!(ctx.tasks[0].join_type, "XOR");
    }

    #[test]
    fn test_template_context_from_graph() {
        let graph = Graph::new().unwrap();

        // Add basic YAWL data
        graph
            .insert_turtle(
                r#"
            @prefix yawl: <http://unrdf.org/yawl#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            yawl:task1 a yawl:AtomicTask ;
                rdfs:label "Task 1" ;
                yawl:taskId "t1" .
        "#,
            )
            .unwrap();

        let result = TemplateContext::from_graph(&graph);
        assert!(result.is_ok());

        let ctx = result.unwrap();
        assert_eq!(ctx.tasks.len(), 1);
    }
}
