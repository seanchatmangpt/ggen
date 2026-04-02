//! Elixir/Ash code generator from RDF specifications
//!
//! Provides high-level generation orchestration for transforming
//! RDF ontologies into idiomatic Elixir code.

use crate::GenConfig;
use ggen_core::graph::Graph;
use ggen_core::template::TemplateContext;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use tera::{Tera, Value};

/// Elixir code generator configuration
#[derive(Debug, Clone)]
pub struct GenConfig {
    /// RDF ontology source file
    pub ontology: PathBuf,
    /// Output directory for generated code
    pub output_dir: PathBuf,
    /// Module prefix (e.g., "Craftplan")
    pub module_prefix: String,
    /// Whether to generate tests
    pub generate_tests: bool,
    /// Whether to generate Ash resources
    pub generate_ash_resources: bool,
    /// Whether to generate Ecto schemas
    pub generate_ecto_schemas: bool,
    /// Whether to generate agents
    pub generate_agents: bool,
}

impl GenConfig {
    /// Create new generator configuration
    pub fn new(ontology: PathBuf, output_dir: PathBuf) -> Self {
        Self {
            ontology,
            output_dir,
            module_prefix: "Craftplan".to_string(),
            generate_tests: true,
            generate_ash_resources: true,
            generate_ecto_schemas: false,
            generate_agents: false,
        }
    }

    /// Set module prefix
    pub fn with_module_prefix(mut self, prefix: String) -> Self {
        self.module_prefix = prefix;
        self
    }

    /// Enable/disable test generation
    pub fn with_tests(mut self, generate: bool) -> Self {
        self.generate_tests = generate;
        self
    }

    /// Enable/disable Ash resource generation
    pub fn with_ash_resources(mut self, generate: bool) -> Self {
        self.generate_ash_resources = generate;
        self
    }

    /// Enable/disable Ecto schema generation
    pub fn with_ecto_schemas(mut self, generate: bool) -> Self {
        self.generate_ecto_schemas = generate;
        self
    }

    /// Enable/disable agent generation
    pub fn with_agents(mut self, generate: bool) -> Self {
        self.generate_agents = generate;
        self
    }
}

/// Main Elixir code generator
pub struct ElixirGenerator {
    config: GenConfig,
    graph: Graph,
    tera: Tera,
}

impl ElixirGenerator {
    /// Create new generator
    pub fn new(config: GenConfig) -> Result<Self> {
        let graph = Graph::from_file(&config.ontology)?;

        // Initialize Tera with templates
        let mut tera = Tera::default();
        tera.add_raw_templates(vec![
            ("ash_resource.ex.tera", include_str!("../templates/elixir/ash_resource.ex.tera")),
            ("ecto_schema.ex.tera", include_str!("../templates/elixir/ecto_schema.ex.tera")),
            ("agent.ex.tera", include_str!("../templates/elixir/agent.ex.tera")),
            ("test.ex.tera", include_str!("../templates/elixir/test.ex.tera")),
            ("module.ex.tera", include_str!("../templates/elixir/module.ex.tera")),
        ])?;

        // Register custom filters
        tera.register_filter("elixir_module_name", elixir_module_name_filter);
        tera.register_filter("elixir_var_name", elixir_var_name_filter);
        tera.register_filter("to_snake", to_snake_filter);
        tera.register_filter("to_pascal", to_pascal_filter);

        Ok(Self { config, graph, tera })
    }

    /// Generate all code from ontology
    pub fn generate_all(&self) -> Result<Vec<PathBuf>> {
        let mut generated = Vec::new();

        // Extract all classes from ontology
        let classes = self.extract_classes()?;

        for class in &classes {
            // Generate Ash resource
            if self.config.generate_ash_resources {
                let path = self.generate_ash_resource(class)?;
                generated.push(path);
            }

            // Generate Ecto schema
            if self.config.generate_ecto_schemas {
                let path = self.generate_ecto_schema(class)?;
                generated.push(path);
            }

            // Generate test
            if self.config.generate_tests {
                let path = self.generate_test(class)?;
                generated.push(path);
            }
        }

        // Generate agents for domains
        if self.config.generate_agents {
            let domains = self.extract_domains()?;
            for domain in &domains {
                let path = self.generate_agent(domain)?;
                generated.push(path);
            }
        }

        Ok(generated)
    }

    /// Generate Ash resource for a class
    fn generate_ash_resource(&self, class: &RdfClass) -> Result<PathBuf> {
        let ctx = self.build_context(class)?;
        let output = self.tera.render("ash_resource.ex.tera", &ctx)?;

        let file_name = format!("{}.ex", to_snake_case(&class.name));
        let path = self.config.output_dir.join(&class.domain).join(&file_name);

        std::fs::create_dir_all(path.parent().unwrap())?;
        std::fs::write(&path, output)?;

        Ok(path)
    }

    /// Generate Ecto schema for a class
    fn generate_ecto_schema(&self, class: &RdfClass) -> Result<PathBuf> {
        let ctx = self.build_context(class)?;
        let output = self.tera.render("ecto_schema.ex.tera", &ctx)?;

        let file_name = format!("{}.ex", to_snake_case(&class.name));
        let path = self.config.output_dir.join(&class.domain).join(&file_name);

        std::fs::create_dir_all(path.parent().unwrap())?;
        std::fs::write(&path, output)?;

        Ok(path)
    }

    /// Generate agent for a domain
    fn generate_agent(&self, domain: &Domain) -> Result<PathBuf> {
        let mut ctx = BTreeMap::new();
        ctx.insert("domain_name".to_string(), Value::String(domain.name.clone()));
        ctx.insert("module_prefix".to_string(), Value::String(self.config.module_prefix.clone()));
        ctx.insert("capabilities".to_string(), serde_json::to_value(&domain.capabilities)?);

        let output = self.tera.render("agent.ex.tera", &ctx)?;

        let file_name = format!("{}_agent.ex", to_snake_case(&domain.name));
        let path = self.config.output_dir.join("agents").join(&file_name);

        std::fs::create_dir_all(path.parent().unwrap())?;
        std::fs::write(&path, output)?;

        Ok(path)
    }

    /// Generate test for a class
    fn generate_test(&self, class: &RdfClass) -> Result<PathBuf> {
        let ctx = self.build_context(class)?;
        let output = self.tera.render("test.ex.tera", &ctx)?;

        let file_name = format!("{}_test.exs", to_snake_case(&class.name));
        let path = self.config.output_dir.join("../test").join(&class.domain).join(&file_name);

        std::fs::create_dir_all(path.parent().unwrap())?;
        std::fs::write(&path, output)?;

        Ok(path)
    }

    /// Build template context from RDF class
    fn build_context(&self, class: &RdfClass) -> Result<TemplateContext> {
        let mut ctx = BTreeMap::new();

        ctx.insert("entity_name".to_string(), Value::String(class.name.clone()));
        ctx.insert("module_path".to_string(), Value::String(class.module_path()));
        ctx.insert("table_name".to_string(), Value::String(class.table_name()));
        ctx.insert("domain".to_string(), Value::String(class.domain.clone()));
        ctx.insert("doc_comments".to_string(), Value::String(class.doc.clone()));
        ctx.insert("attributes".to_string(), serde_json::to_value(&class.attributes)?);
        ctx.insert("relationships".to_string(), serde_json::to_value(&class.relationships)?);
        ctx.insert("constraints".to_string(), serde_json::to_value(&class.constraints)?);

        Ok(ctx)
    }

    /// Extract all classes from RDF graph
    fn extract_classes(&self) -> Result<Vec<RdfClass>> {
        // SPARQL query to extract classes
        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX sh: <http://www.w3.org/ns/shacl#>

            SELECT ?class ?label ?comment ?domain WHERE {
                ?class a rdfs:Class .
                OPTIONAL { ?class rdfs:label ?label }
                OPTIONAL { ?class rdfs:comment ?comment }
                OPTIONAL { ?class rdfs:subClassOf ?domain }
            }
            ORDER BY ?class
        "#;

        let results = self.graph.query(query)?;
        let mut classes = Vec::new();

        for row in results {
            // Parse and build RdfClass from SPARQL results
            // This is simplified - actual implementation would parse properly
        }

        Ok(classes)
    }

    /// Extract domains from RDF graph
    fn extract_domains(&self) -> Result<Vec<Domain>> {
        // Similar to extract_classes but for domain/grouping
        Ok(vec![])
    }
}

/// RDF class representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfClass {
    pub name: String,
    pub domain: String,
    pub doc: String,
    pub attributes: Vec<Attribute>,
    pub relationships: Vec<Relationship>,
    pub constraints: Vec<Constraint>,
}

impl RdfClass {
    pub fn module_path(&self) -> String {
        format!("{}.{}", capitalize(&self.domain), &self.name)
    }

    pub fn table_name(&self) -> String {
        format!("{}_{}", to_snake_case(&self.domain), to_snake_case(&self.name))
    }
}

/// Attribute representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Attribute {
    pub name: String,
    pub type_: String,
    pub allow_nil: bool,
    pub default: Option<String>,
    pub constraints: BTreeMap<String, Value>,
    pub public: bool,
    pub description: String,
}

/// Relationship representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Relationship {
    pub name: String,
    pub kind: String, // "belongs_to", "has_many", "has_one", "many_to_many"
    pub related: String,
    pub destination: Option<String>,
    pub through: Option<String>,
}

/// Constraint representation (from SHACL)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Constraint {
    pub property: String,
    pub kind: String, // "min_length", "max_length", "pattern", "min", "max", "one_of"
    pub value: Value,
}

/// Domain representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Domain {
    pub name: String,
    pub capabilities: Vec<String>,
}

// Tera filters

fn elixir_module_name_filter(value: &Value, _args: &std::collections::HashMap<String, Value>) -> tera::Result<Value> {
    let s = value.as_str().unwrap_or("");
    Ok(Value::String(elixir_module_name(s)))
}

fn elixir_var_name_filter(value: &Value, _args: &std::collections::HashMap<String, Value>) -> tera::Result<Value> {
    let s = value.as_str().unwrap_or("");
    Ok(Value::String(to_snake_case(s)))
}

fn to_snake_filter(value: &Value, _args: &std::collections::HashMap<String, Value>) -> tera::Result<Value> {
    let s = value.as_str().unwrap_or("");
    Ok(Value::String(to_snake_case(s)))
}

fn to_pascal_filter(value: &Value, _args: &std::collections::HashMap<String, Value>) -> tera::Result<Value> {
    let s = value.as_str().unwrap_or("");
    Ok(Value::String(capitalize(s)))
}

// Helper functions

fn elixir_module_name(s: &str) -> String {
    s.split('_')
        .map(|part| capitalize(part))
        .collect::<Vec<_>>()
        .join(".")
}

fn to_snake_case(s: &str) -> String {
    s.chars()
        .enumerate()
        .map(|(i, c)| {
            if c.is_uppercase() {
                if i > 0 { format!("_{}", c.to_lowercase().collect::<String>()) } else { c.to_lowercase().to_string() }
            } else {
                c.to_string()
            }
        })
        .collect::<String>()
}

fn capitalize(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
    }
}

pub type TemplateContext = BTreeMap<String, Value>;
