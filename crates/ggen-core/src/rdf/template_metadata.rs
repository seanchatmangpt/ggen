//! Template metadata management using Oxigraph
//!
//! This module provides RDF-based metadata storage and querying for templates.
//! It extracts metadata from template frontmatter, stores it in an Oxigraph store,
//! and provides SPARQL-based querying capabilities.

use chrono::{DateTime, Utc};
use ggen_utils::error::{Error, Result};
use oxigraph::io::RdfFormat;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::Path;
use std::sync::{Arc, Mutex};

use crate::graph::Graph;

/// Template variable metadata
///
/// Represents a variable that can be used in template rendering.
///
/// # Examples
///
/// ```rust
/// use ggen_core::rdf::template_metadata::TemplateVariable;
///
/// # fn main() {
/// let variable = TemplateVariable {
///     name: "project_name".to_string(),
///     var_type: "string".to_string(),
///     default_value: Some("MyProject".to_string()),
///     description: Some("Name of the project".to_string()),
///     required: true,
/// };
/// assert_eq!(variable.name, "project_name");
/// assert!(variable.required);
/// # }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TemplateVariable {
    pub name: String,
    pub var_type: String,
    pub default_value: Option<String>,
    pub description: Option<String>,
    pub required: bool,
}

/// Template metadata extracted from frontmatter
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateMetadata {
    pub id: String,
    pub name: String,
    pub version: Option<String>,
    pub description: Option<String>,
    pub author: Option<String>,
    pub created_at: Option<DateTime<Utc>>,
    pub updated_at: Option<DateTime<Utc>>,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub variables: Vec<TemplateVariable>,
    pub generated_files: Vec<String>,
    pub generated_directories: Vec<String>,
    pub dependencies: Vec<String>,
    pub stability: Option<String>,
    pub test_coverage: Option<f64>,
    pub usage_count: Option<i64>,
}

impl TemplateMetadata {
    /// Create new template metadata with required fields
    ///
    /// # Arguments
    ///
    /// * `id` - Unique identifier for the template
    /// * `name` - Human-readable name of the template
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::template_metadata::TemplateMetadata;
    ///
    /// # fn main() {
    /// let metadata = TemplateMetadata::new(
    ///     "template://example/rust-cli".to_string(),
    ///     "Rust CLI Template".to_string(),
    /// );
    /// assert_eq!(metadata.id, "template://example/rust-cli");
    /// assert_eq!(metadata.name, "Rust CLI Template");
    /// assert!(metadata.created_at.is_some());
    /// # }
    /// ```
    pub fn new(id: String, name: String) -> Self {
        Self {
            id,
            name,
            version: None,
            description: None,
            author: None,
            created_at: Some(Utc::now()),
            updated_at: Some(Utc::now()),
            category: None,
            tags: Vec::new(),
            variables: Vec::new(),
            generated_files: Vec::new(),
            generated_directories: Vec::new(),
            dependencies: Vec::new(),
            stability: Some("stable".to_string()),
            test_coverage: None,
            usage_count: Some(0),
        }
    }

    /// Generate Turtle RDF representation
    pub fn to_turtle(&self) -> Result<String> {
        let mut turtle = String::new();
        turtle.push_str("@prefix ggen: <http://ggen.dev/ontology#> .\n");
        turtle.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
        turtle.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n");

        // Template declaration
        turtle.push_str(&format!("<{}> a ggen:Template ;\n", self.id));
        turtle.push_str(&format!(
            "  ggen:templateName \"{}\" ;\n",
            escape_literal(&self.name)
        ));

        // Optional metadata
        if let Some(version) = &self.version {
            turtle.push_str(&format!(
                "  ggen:templateVersion \"{}\" ;\n",
                escape_literal(version)
            ));
        }
        if let Some(desc) = &self.description {
            turtle.push_str(&format!(
                "  ggen:templateDescription \"{}\" ;\n",
                escape_literal(desc)
            ));
        }
        if let Some(author) = &self.author {
            turtle.push_str(&format!(
                "  ggen:templateAuthor \"{}\" ;\n",
                escape_literal(author)
            ));
        }
        if let Some(created) = &self.created_at {
            turtle.push_str(&format!(
                "  ggen:createdAt \"{}\"^^xsd:dateTime ;\n",
                created.to_rfc3339()
            ));
        }
        if let Some(updated) = &self.updated_at {
            turtle.push_str(&format!(
                "  ggen:updatedAt \"{}\"^^xsd:dateTime ;\n",
                updated.to_rfc3339()
            ));
        }
        if let Some(category) = &self.category {
            turtle.push_str(&format!(
                "  ggen:category \"{}\" ;\n",
                escape_literal(category)
            ));
        }
        if let Some(stability) = &self.stability {
            turtle.push_str(&format!(
                "  ggen:stability \"{}\" ;\n",
                escape_literal(stability)
            ));
        }
        if let Some(coverage) = self.test_coverage {
            turtle.push_str(&format!(
                "  ggen:testCoverage \"{}\"^^xsd:decimal ;\n",
                coverage
            ));
        }
        if let Some(usage) = self.usage_count {
            turtle.push_str(&format!("  ggen:usageCount \"{}\"^^xsd:integer ;\n", usage));
        }

        // Tags
        for tag in &self.tags {
            turtle.push_str(&format!("  ggen:tag \"{}\" ;\n", escape_literal(tag)));
        }

        // Variables
        for (i, _var) in self.variables.iter().enumerate() {
            let var_id = format!("{}#var_{}", self.id, i);
            turtle.push_str(&format!("  ggen:hasVariable <{}> ;\n", var_id));
        }

        // Generated files
        for file in &self.generated_files {
            turtle.push_str(&format!(
                "  ggen:generatesFile \"{}\" ;\n",
                escape_literal(file)
            ));
        }

        // Dependencies
        for dep in &self.dependencies {
            turtle.push_str(&format!("  ggen:dependsOn <{}> ;\n", dep));
        }

        // Remove trailing semicolon and add period
        if turtle.ends_with(" ;\n") {
            turtle.truncate(turtle.len() - 3);
            turtle.push_str(" .\n\n");
        }

        // Variable definitions
        for (i, var) in self.variables.iter().enumerate() {
            let var_id = format!("{}#var_{}", self.id, i);
            turtle.push_str(&format!("<{}> a ggen:Variable ;\n", var_id));
            turtle.push_str(&format!(
                "  ggen:variableName \"{}\" ;\n",
                escape_literal(&var.name)
            ));
            turtle.push_str(&format!(
                "  ggen:variableType \"{}\" ;\n",
                escape_literal(&var.var_type)
            ));
            turtle.push_str(&format!(
                "  ggen:isRequired \"{}\"^^xsd:boolean",
                var.required
            ));

            if let Some(default) = &var.default_value {
                turtle.push_str(&format!(
                    " ;\n  ggen:variableDefault \"{}\"",
                    escape_literal(default)
                ));
            }
            if let Some(desc) = &var.description {
                turtle.push_str(&format!(
                    " ;\n  ggen:variableDescription \"{}\"",
                    escape_literal(desc)
                ));
            }
            turtle.push_str(" .\n\n");
        }

        Ok(turtle)
    }

    /// Parse template metadata from Turtle RDF
    pub fn from_turtle(turtle: &str, template_id: &str) -> Result<Self> {
        let graph = Graph::new()?;
        graph.insert_turtle(turtle)?;

        // Query for template metadata
        let query = format!(
            r#"
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT ?name ?version ?description ?author ?created ?updated ?category ?stability ?coverage ?usage
            WHERE {{
                <{template_id}> a ggen:Template ;
                    ggen:templateName ?name .
                OPTIONAL {{ <{template_id}> ggen:templateVersion ?version }}
                OPTIONAL {{ <{template_id}> ggen:templateDescription ?description }}
                OPTIONAL {{ <{template_id}> ggen:templateAuthor ?author }}
                OPTIONAL {{ <{template_id}> ggen:createdAt ?created }}
                OPTIONAL {{ <{template_id}> ggen:updatedAt ?updated }}
                OPTIONAL {{ <{template_id}> ggen:category ?category }}
                OPTIONAL {{ <{template_id}> ggen:stability ?stability }}
                OPTIONAL {{ <{template_id}> ggen:testCoverage ?coverage }}
                OPTIONAL {{ <{template_id}> ggen:usageCount ?usage }}
            }}
            "#,
            template_id = template_id
        );

        let results = graph.query_cached(&query)?;
        let mut metadata = TemplateMetadata::new(template_id.to_string(), String::new());

        if let crate::graph::CachedResult::Solutions(rows) = results {
            if let Some(row) = rows.first() {
                metadata.name = row
                    .get("name")
                    .map(|s| s.trim_matches('"').to_string())
                    .unwrap_or_default();
                metadata.version = row.get("version").map(|s| s.trim_matches('"').to_string());
                metadata.description = row
                    .get("description")
                    .map(|s| s.trim_matches('"').to_string());
                metadata.author = row.get("author").map(|s| s.trim_matches('"').to_string());
                metadata.category = row.get("category").map(|s| s.trim_matches('"').to_string());
                metadata.stability = row
                    .get("stability")
                    .map(|s| s.trim_matches('"').to_string());

                if let Some(coverage_str) = row.get("coverage") {
                    metadata.test_coverage = coverage_str.trim_matches('"').parse().ok();
                }
                if let Some(usage_str) = row.get("usage") {
                    metadata.usage_count = usage_str.trim_matches('"').parse().ok();
                }
            }
        }

        Ok(metadata)
    }
}

/// Relationship between templates
///
/// Template relationship types
///
/// Represents different types of relationships between templates.
///
/// # Examples
///
/// ```rust
/// use ggen_core::rdf::template_metadata::TemplateRelationship;
///
/// # fn main() {
/// let relationship = TemplateRelationship::DependsOn;
/// match relationship {
///     TemplateRelationship::DependsOn => assert!(true),
///     TemplateRelationship::Extends => assert!(true),
///     TemplateRelationship::Includes => assert!(true),
///     TemplateRelationship::Overrides => assert!(true),
/// }
/// # }
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplateRelationship {
    /// Template depends on another template
    DependsOn,
    /// Template extends another template
    Extends,
    /// Template includes another template
    Includes,
    /// Template overrides another template
    Overrides,
}

/// RDF metadata store for templates using Oxigraph
pub struct TemplateMetadataStore {
    store: Arc<Mutex<Store>>,
    metadata_cache: Arc<Mutex<HashMap<String, TemplateMetadata>>>,
}

impl TemplateMetadataStore {
    /// Create new in-memory metadata store
    pub fn new() -> Result<Self> {
        let store = Store::new()
            .map_err(|e| Error::with_context("Failed to create Oxigraph store", &e.to_string()))?;
        Ok(Self {
            store: Arc::new(Mutex::new(store)),
            metadata_cache: Arc::new(Mutex::new(HashMap::new())),
        })
    }

    /// Create persistent metadata store at path
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
        let store = Store::open(path.as_ref())
            .map_err(|e| Error::with_context("Failed to open Oxigraph store", &e.to_string()))?;
        Ok(Self {
            store: Arc::new(Mutex::new(store)),
            metadata_cache: Arc::new(Mutex::new(HashMap::new())),
        })
    }

    /// Load Ggen schema into store
    pub fn load_schema(&self) -> Result<()> {
        let schema = super::schema::load_schema()?;
        let store = self.store.lock().map_err(|_| Error::new("Lock poisoned"))?;
        store
            .load_from_reader(RdfFormat::Turtle, schema.as_bytes())
            .map_err(|e| Error::with_context("Failed to load schema", &e.to_string()))?;
        Ok(())
    }

    /// Store template metadata as RDF triples
    pub fn store_metadata(&self, metadata: &TemplateMetadata) -> Result<()> {
        let turtle = metadata.to_turtle()?;
        let store = self.store.lock().map_err(|_| Error::new("Lock poisoned"))?;
        store
            .load_from_reader(RdfFormat::Turtle, turtle.as_bytes())
            .map_err(|e| Error::with_context("Failed to store metadata", &e.to_string()))?;

        // Update cache
        let mut cache = self
            .metadata_cache
            .lock()
            .map_err(|_| Error::new("Lock poisoned"))?;
        cache.insert(metadata.id.clone(), metadata.clone());

        Ok(())
    }

    /// Retrieve template metadata by ID
    pub fn get_metadata(&self, template_id: &str) -> Result<Option<TemplateMetadata>> {
        // Check cache first
        {
            let cache = self
                .metadata_cache
                .lock()
                .map_err(|_| Error::new("Lock poisoned"))?;
            if let Some(metadata) = cache.get(template_id) {
                return Ok(Some(metadata.clone()));
            }
        }

        // Query from store
        let query = format!(
            r#"
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT ?name
            WHERE {{
                <{template_id}> a ggen:Template ;
                    ggen:templateName ?name .
            }}
            "#,
            template_id = template_id
        );

        let store = self.store.lock().map_err(|_| Error::new("Lock poisoned"))?;
        #[allow(deprecated)]
        let results = store.query(&query)?;

        if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
            if solutions.next().is_some() {
                // Template exists, construct metadata from queries
                // Note: dump_all() not available in current Oxigraph API
                // Instead, query all fields individually
                drop(store); // Release lock before querying

                let metadata = self.query_full_metadata(template_id)?;

                // Update cache
                let mut cache = self
                    .metadata_cache
                    .lock()
                    .map_err(|_| Error::new("Lock poisoned"))?;
                cache.insert(template_id.to_string(), metadata.clone());

                return Ok(Some(metadata));
            }
        }

        Ok(None)
    }

    /// Query templates using SPARQL
    pub fn query(&self, sparql: &str) -> Result<Vec<BTreeMap<String, String>>> {
        let store = self.store.lock().map_err(|_| Error::new("Lock poisoned"))?;
        #[allow(deprecated)]
        let results = store.query(sparql)?;

        let mut rows = Vec::new();
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                let solution = solution?;
                let mut row = BTreeMap::new();
                for (var, term) in solution.iter() {
                    row.insert(var.as_str().to_string(), term.to_string());
                }
                rows.push(row);
            }
        }

        Ok(rows)
    }

    /// Find templates by category
    pub fn find_by_category(&self, category: &str) -> Result<Vec<String>> {
        let query = format!(
            r#"
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT ?template ?name
            WHERE {{
                ?template a ggen:Template ;
                    ggen:category "{}" ;
                    ggen:templateName ?name .
            }}
            "#,
            escape_literal(category)
        );

        let results = self.query(&query)?;
        Ok(results
            .iter()
            .filter_map(|row| {
                row.get("template")
                    .map(|s| s.trim_matches('<').trim_matches('>').to_string())
            })
            .collect())
    }

    /// Find templates by tag
    pub fn find_by_tag(&self, tag: &str) -> Result<Vec<String>> {
        let query = format!(
            r#"
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT ?template
            WHERE {{
                ?template a ggen:Template ;
                    ggen:tag "{}" .
            }}
            "#,
            escape_literal(tag)
        );

        let results = self.query(&query)?;
        Ok(results
            .iter()
            .filter_map(|row| {
                row.get("template")
                    .map(|s| s.trim_matches('<').trim_matches('>').to_string())
            })
            .collect())
    }

    /// Get all dependencies for a template
    pub fn get_dependencies(&self, template_id: &str) -> Result<Vec<String>> {
        let query = format!(
            r#"
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT ?dependency
            WHERE {{
                <{template_id}> ggen:dependsOn ?dependency .
            }}
            "#,
            template_id = template_id
        );

        let results = self.query(&query)?;
        Ok(results
            .iter()
            .filter_map(|row| {
                row.get("dependency")
                    .map(|s| s.trim_matches('<').trim_matches('>').to_string())
            })
            .collect())
    }

    /// Export all metadata as Turtle
    pub fn export_turtle(&self) -> Result<String> {
        // Query all templates and reconstruct Turtle
        let query = r#"
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT DISTINCT ?template
            WHERE {
                ?template a ggen:Template .
            }
        "#;

        let templates = self.query(query)?;
        let mut turtle = String::new();

        // Add prefixes
        turtle.push_str("@prefix ggen: <http://ggen.dev/ontology#> .\n");
        turtle.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
        turtle.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n");

        // Export each template
        for row in templates {
            if let Some(template_id) = row.get("template") {
                let id = template_id.trim_matches('<').trim_matches('>');
                if let Some(metadata) = self.get_metadata(id)? {
                    turtle.push_str(&metadata.to_turtle()?);
                    turtle.push('\n');
                }
            }
        }

        Ok(turtle)
    }

    /// Clear all metadata
    pub fn clear(&self) -> Result<()> {
        let store = self.store.lock().map_err(|_| Error::new("Lock poisoned"))?;
        store
            .clear()
            .map_err(|e| Error::with_context("Failed to clear store", &e.to_string()))?;

        let mut cache = self
            .metadata_cache
            .lock()
            .map_err(|_| Error::new("Lock poisoned"))?;
        cache.clear();

        Ok(())
    }
}

impl Default for TemplateMetadataStore {
    fn default() -> Self {
        // The in-memory store should never fail to initialize.
        // If it does, we create an empty store as fallback.
        match Self::new() {
            Ok(store) => store,
            Err(e) => {
                log::warn!(
                    "Failed to create metadata store with in-memory backend: {}",
                    e
                );
                // Create empty store with Arc-wrapped Mutex
                Self {
                    store: Arc::new(Mutex::new(
                        Store::new().unwrap_or_else(|_| Store::default()),
                    )),
                }
            }
        }
    }
}

/// Escape special characters in RDF literals
fn escape_literal(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_metadata_creation() {
        let metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test Template".to_string(),
        );

        assert_eq!(metadata.id, "http://example.org/template1");
        assert_eq!(metadata.name, "Test Template");
        assert!(metadata.created_at.is_some());
        assert_eq!(metadata.stability, Some("stable".to_string()));
        assert_eq!(metadata.usage_count, Some(0));
    }

    #[test]
    fn test_template_to_turtle() {
        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test Template".to_string(),
        );
        metadata.description = Some("A test template".to_string());
        metadata.category = Some("testing".to_string());

        let turtle = metadata.to_turtle().unwrap();

        assert!(turtle.contains("@prefix ggen:"));
        assert!(turtle.contains("ggen:Template"));
        assert!(turtle.contains("ggen:templateName \"Test Template\""));
        assert!(turtle.contains("ggen:templateDescription \"A test template\""));
        assert!(turtle.contains("ggen:category \"testing\""));
    }

    #[test]
    fn test_metadata_store_operations() {
        let store = TemplateMetadataStore::new().unwrap();
        store.load_schema().unwrap();

        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test Template".to_string(),
        );
        metadata.category = Some("testing".to_string());
        metadata.tags = vec!["rust".to_string(), "test".to_string()];

        // Store metadata
        store.store_metadata(&metadata).unwrap();

        // Retrieve metadata
        let retrieved = store.get_metadata("http://example.org/template1").unwrap();
        assert!(retrieved.is_some());
        let retrieved = retrieved.unwrap();
        assert_eq!(retrieved.name, "Test Template");
    }

    #[test]
    fn test_find_by_category() {
        let store = TemplateMetadataStore::new().unwrap();
        store.load_schema().unwrap();

        let mut metadata1 = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Template 1".to_string(),
        );
        metadata1.category = Some("web".to_string());

        let mut metadata2 = TemplateMetadata::new(
            "http://example.org/template2".to_string(),
            "Template 2".to_string(),
        );
        metadata2.category = Some("web".to_string());

        store.store_metadata(&metadata1).unwrap();
        store.store_metadata(&metadata2).unwrap();

        let found = store.find_by_category("web").unwrap();
        assert_eq!(found.len(), 2);
    }

    #[test]
    fn test_escape_literal() {
        assert_eq!(escape_literal("hello"), "hello");
        assert_eq!(escape_literal("hello\"world"), "hello\\\"world");
        assert_eq!(escape_literal("line1\nline2"), "line1\\nline2");
        assert_eq!(escape_literal("tab\there"), "tab\\there");
    }

    #[test]
    fn test_template_variables() -> Result<()> {
        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test Template".to_string(),
        );

        metadata.variables.push(TemplateVariable {
            name: "project_name".to_string(),
            var_type: "string".to_string(),
            default_value: Some("my-project".to_string()),
            description: Some("Name of the project".to_string()),
            required: true,
        });

        let turtle = metadata.to_turtle()?;
        assert!(turtle.contains("ggen:Variable"));
        assert!(turtle.contains("ggen:variableName \"project_name\""));
        assert!(turtle.contains("ggen:variableType \"string\""));
        assert!(turtle.contains("ggen:isRequired \"true\""));

        Ok(())
    }
}
