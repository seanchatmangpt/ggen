//! Code graph operations
//!
//! Converts SPARQL query results into strongly-typed code graph entities
//! that can be rendered by Tera templates.

use ggen_utils::error::{Error, Result};
use serde::Serialize;
use std::collections::BTreeMap;

/// Builder for code graph entities from SPARQL results
pub struct CodeGraphBuilder {
    /// Parsed structs
    structs: Vec<CodeStruct>,
    /// Parsed traits
    traits: Vec<CodeTrait>,
    /// Parsed impls
    impls: Vec<CodeImpl>,
    /// Parsed enums
    enums: Vec<CodeEnum>,
}

impl CodeGraphBuilder {
    /// Create a new code graph builder
    pub fn new() -> Self {
        Self {
            structs: Vec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            enums: Vec::new(),
        }
    }

    /// Parse SPARQL SELECT results into CodeStruct objects
    ///
    /// # Arguments
    /// * `results` - SPARQL query results as maps of variable bindings
    ///
    /// # Returns
    /// * `Ok(Vec<CodeStruct>)` - Parsed structs
    /// * `Err(Error)` - Parse error
    pub fn from_sparql_results(results: &[BTreeMap<String, String>]) -> Result<Vec<CodeStruct>> {
        let mut structs = Vec::new();

        for row in results {
            let name = row
                .get("name")
                .ok_or_else(|| Error::new("SPARQL result missing 'name' binding"))?;

            let iri = row.get("iri").cloned().unwrap_or_default();

            let struct_def = CodeStruct {
                iri,
                name: name.clone(),
                visibility: row
                    .get("visibility")
                    .cloned()
                    .unwrap_or_else(|| "pub".to_string()),
                derives: Self::parse_derives(row.get("derives")),
                generics: row.get("generics").cloned(),
                fields: Vec::new(), // Populated by separate query
                docstring: row.get("docstring").cloned(),
                attributes: Vec::new(),
                source_iri: row.get("source_iri").cloned(),
            };

            structs.push(struct_def);
        }

        Ok(structs)
    }

    /// Parse comma-separated derives into Vec
    fn parse_derives(derives: Option<&String>) -> Vec<String> {
        derives
            .map(|d| d.split(',').map(|s| s.trim().to_string()).collect())
            .unwrap_or_default()
    }

    /// Convert code graph entities to Tera context
    pub fn to_tera_context(&self) -> tera::Context {
        let mut ctx = tera::Context::new();
        ctx.insert("structs", &self.structs);
        ctx.insert("traits", &self.traits);
        ctx.insert("impls", &self.impls);
        ctx.insert("enums", &self.enums);
        ctx
    }

    /// Add a struct to the code graph
    pub fn add_struct(&mut self, s: CodeStruct) {
        self.structs.push(s);
    }

    /// Add a trait to the code graph
    pub fn add_trait(&mut self, t: CodeTrait) {
        self.traits.push(t);
    }

    /// Add an impl to the code graph
    pub fn add_impl(&mut self, i: CodeImpl) {
        self.impls.push(i);
    }

    /// Build an impl block from a relationship definition
    ///
    /// # Arguments
    /// * `source` - Source entity name (e.g., "User")
    /// * `rel_type` - Relationship type (e.g., "has_many")
    /// * `target` - Target entity name (e.g., "Order")
    pub fn build_impl_from_relationship(source: &str, rel_type: &str, target: &str) -> CodeImpl {
        let method_name = match rel_type {
            "has_many" => format!("get_{}s", target.to_lowercase()),
            "has_one" | "belongs_to" => format!("get_{}", target.to_lowercase()),
            _ => format!("get_{}", target.to_lowercase()),
        };

        let return_type = match rel_type {
            "has_many" => format!("Vec<{}>", target),
            _ => target.to_string(),
        };

        CodeImpl {
            iri: String::new(),
            for_type: source.to_string(),
            trait_name: None,
            generics: None,
            methods: vec![CodeMethod {
                iri: String::new(),
                name: method_name,
                visibility: "pub".to_string(),
                is_async: false,
                self_param: Some("&self".to_string()),
                params: Vec::new(),
                return_type: Some(return_type),
                body: Some("todo!()".to_string()),
                docstring: Some(format!(
                    "Get {} {}(s)",
                    rel_type.replace('_', " "),
                    target.to_lowercase()
                )),
            }],
        }
    }
}

impl Default for CodeGraphBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// Code Graph Entity Types
// ============================================================================

/// Represents a Rust module
#[derive(Debug, Clone, Serialize)]
pub struct CodeModule {
    /// IRI identifying this module
    pub iri: String,
    /// Module name (e.g., "models")
    pub name: String,
    /// Visibility ("pub", "pub(crate)", "")
    #[serde(default)]
    pub visibility: String,
    /// Use statements
    #[serde(default)]
    pub imports: Vec<CodeImport>,
    /// Module items (structs, traits, impls)
    #[serde(default)]
    pub items: Vec<CodeItem>,
    /// Module-level attributes
    #[serde(default)]
    pub attributes: Vec<String>,
}

/// A code item (struct, trait, impl, or enum)
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type")]
pub enum CodeItem {
    /// Rust struct
    Struct(CodeStruct),
    /// Rust trait
    Trait(CodeTrait),
    /// Rust impl block
    Impl(CodeImpl),
    /// Rust enum
    Enum(CodeEnum),
}

/// Represents a Rust struct
#[derive(Debug, Clone, Serialize)]
pub struct CodeStruct {
    /// IRI from code graph
    pub iri: String,
    /// Struct name (PascalCase)
    pub name: String,
    /// Visibility
    #[serde(default)]
    pub visibility: String,
    /// Derive macros
    #[serde(default)]
    pub derives: Vec<String>,
    /// Generic parameters
    #[serde(default)]
    pub generics: Option<String>,
    /// Struct fields (ordered)
    #[serde(default)]
    pub fields: Vec<CodeField>,
    /// Documentation string
    #[serde(default)]
    pub docstring: Option<String>,
    /// Additional attributes
    #[serde(default)]
    pub attributes: Vec<String>,
    /// Source traceability
    #[serde(default)]
    pub source_iri: Option<String>,
}

/// Represents a struct field
#[derive(Debug, Clone, Serialize)]
pub struct CodeField {
    /// IRI from code graph
    pub iri: String,
    /// Field name (snake_case)
    pub name: String,
    /// Rust type
    pub field_type: String,
    /// Visibility
    #[serde(default)]
    pub visibility: String,
    /// Documentation
    #[serde(default)]
    pub docstring: Option<String>,
    /// Field attributes
    #[serde(default)]
    pub attributes: Vec<String>,
    /// Default value expression
    #[serde(default)]
    pub default: Option<String>,
}

/// Represents a Rust trait
#[derive(Debug, Clone, Serialize)]
pub struct CodeTrait {
    /// IRI from code graph
    pub iri: String,
    /// Trait name
    pub name: String,
    /// Visibility
    #[serde(default)]
    pub visibility: String,
    /// Trait bounds
    #[serde(default)]
    pub bounds: Option<String>,
    /// Trait methods
    #[serde(default)]
    pub methods: Vec<CodeMethod>,
    /// Whether trait has async methods
    #[serde(default)]
    pub is_async: bool,
    /// Documentation
    #[serde(default)]
    pub docstring: Option<String>,
}

/// Represents a method signature or implementation
#[derive(Debug, Clone, Serialize)]
pub struct CodeMethod {
    /// IRI from code graph
    pub iri: String,
    /// Method name
    pub name: String,
    /// Visibility
    #[serde(default)]
    pub visibility: String,
    /// Whether method is async
    #[serde(default)]
    pub is_async: bool,
    /// Self parameter ("&self", "&mut self", "self", None)
    #[serde(default)]
    pub self_param: Option<String>,
    /// Method parameters
    #[serde(default)]
    pub params: Vec<CodeParam>,
    /// Return type
    #[serde(default)]
    pub return_type: Option<String>,
    /// Method body (for impls)
    #[serde(default)]
    pub body: Option<String>,
    /// Documentation
    #[serde(default)]
    pub docstring: Option<String>,
}

/// Represents a method parameter
#[derive(Debug, Clone, Serialize)]
pub struct CodeParam {
    /// Parameter name
    pub name: String,
    /// Parameter type
    pub param_type: String,
}

/// Represents a Rust impl block
#[derive(Debug, Clone, Serialize)]
pub struct CodeImpl {
    /// IRI from code graph
    pub iri: String,
    /// Type being implemented for
    pub for_type: String,
    /// Trait being implemented (if any)
    #[serde(default)]
    pub trait_name: Option<String>,
    /// Generic parameters
    #[serde(default)]
    pub generics: Option<String>,
    /// Implemented methods
    #[serde(default)]
    pub methods: Vec<CodeMethod>,
}

/// Represents a Rust enum
#[derive(Debug, Clone, Serialize)]
pub struct CodeEnum {
    /// IRI from code graph
    pub iri: String,
    /// Enum name
    pub name: String,
    /// Visibility
    #[serde(default)]
    pub visibility: String,
    /// Derive macros
    #[serde(default)]
    pub derives: Vec<String>,
    /// Enum variants
    #[serde(default)]
    pub variants: Vec<CodeVariant>,
    /// Documentation
    #[serde(default)]
    pub docstring: Option<String>,
}

/// Represents an enum variant
#[derive(Debug, Clone, Serialize)]
pub struct CodeVariant {
    /// Variant name
    pub name: String,
    /// Variant fields (for tuple/struct variants)
    #[serde(default)]
    pub fields: Vec<CodeField>,
    /// Documentation
    #[serde(default)]
    pub docstring: Option<String>,
}

/// Represents a use/import statement
#[derive(Debug, Clone, Serialize)]
pub struct CodeImport {
    /// Import path
    pub path: String,
    /// Optional alias
    #[serde(default)]
    pub alias: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_derives() {
        let derives = Some("Debug, Clone, Serialize".to_string());
        let result = CodeGraphBuilder::parse_derives(derives.as_ref());
        assert_eq!(result, vec!["Debug", "Clone", "Serialize"]);
    }

    #[test]
    fn test_build_impl_has_many() {
        let impl_block =
            CodeGraphBuilder::build_impl_from_relationship("User", "has_many", "Order");
        assert_eq!(impl_block.for_type, "User");
        assert_eq!(impl_block.methods.len(), 1);
        assert_eq!(impl_block.methods[0].name, "get_orders");
        assert_eq!(
            impl_block.methods[0].return_type,
            Some("Vec<Order>".to_string())
        );
    }

    #[test]
    fn test_build_impl_belongs_to() {
        let impl_block =
            CodeGraphBuilder::build_impl_from_relationship("Order", "belongs_to", "User");
        assert_eq!(impl_block.methods[0].name, "get_user");
        assert_eq!(impl_block.methods[0].return_type, Some("User".to_string()));
    }

    #[test]
    fn test_from_sparql_results() {
        let mut row = BTreeMap::new();
        row.insert("name".to_string(), "User".to_string());
        row.insert("iri".to_string(), "http://example.org/User".to_string());
        row.insert("derives".to_string(), "Debug, Clone".to_string());
        row.insert("docstring".to_string(), "A user entity".to_string());

        let results = CodeGraphBuilder::from_sparql_results(&[row]).unwrap();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].name, "User");
        assert_eq!(results[0].derives, vec!["Debug", "Clone"]);
    }
}
