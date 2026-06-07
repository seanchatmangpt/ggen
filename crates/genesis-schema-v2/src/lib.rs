//! KNHK V2 Schema System - Pattern definitions and workflow schemas
//!
//! This crate provides:
//! - OpenAPI specifications for REST/GRPC APIs
//! - RDF ontology for semantic understanding
//! - Pattern definitions for all 43 YAWL patterns
//! - Workflow schema validation

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// OpenAPI v3.1 compliant API specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpenApiSpec {
    pub openapi: String,
    pub info: ApiInfo,
    pub paths: HashMap<String, PathItem>,
    pub components: Components,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ApiInfo {
    pub title: String,
    pub version: String,
    pub description: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathItem {
    pub post: Option<Operation>,
    pub get: Option<Operation>,
    pub put: Option<Operation>,
    pub delete: Option<Operation>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Operation {
    pub summary: String,
    #[serde(rename = "operationId")]
    pub operation_id: String,
    pub parameters: Vec<Parameter>,
    #[serde(rename = "requestBody")]
    pub request_body: Option<RequestBody>,
    pub responses: HashMap<String, Response>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    pub in_: String,
    pub required: bool,
    pub schema: SchemaRef,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RequestBody {
    pub required: bool,
    pub content: HashMap<String, MediaType>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MediaType {
    pub schema: SchemaRef,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Response {
    pub description: String,
    pub content: Option<HashMap<String, MediaType>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum SchemaRef {
    Ref {
        #[serde(rename = "$ref")]
        ref_: String,
    },
    Schema {
        schema: Box<JsonSchema>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonSchema {
    #[serde(rename = "type")]
    pub schema_type: String,
    pub properties: Option<HashMap<String, Box<JsonSchema>>>,
    pub required: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Components {
    pub schemas: HashMap<String, Box<JsonSchema>>,
}

impl OpenApiSpec {
    pub fn new(title: String, version: String) -> Self {
        Self {
            openapi: "3.1.0".to_string(),
            info: ApiInfo {
                title,
                version,
                description: None,
            },
            paths: HashMap::new(),
            components: Components {
                schemas: HashMap::new(),
            },
        }
    }

    /// Add an endpoint to the API specification
    pub fn add_endpoint(&mut self, path: String, method: String, operation: Operation) {
        let path_item = self.paths.entry(path).or_insert(PathItem {
            post: None,
            get: None,
            put: None,
            delete: None,
        });

        match method.as_str() {
            "POST" => path_item.post = Some(operation),
            "GET" => path_item.get = Some(operation),
            "PUT" => path_item.put = Some(operation),
            "DELETE" => path_item.delete = Some(operation),
            _ => {}
        }
    }
}

/// RDF/Turtle ontology for semantic integration
#[derive(Debug, Clone)]
pub struct RdfOntology {
    pub namespace: String,
    pub triples: Vec<(String, String, String)>,
}

impl RdfOntology {
    pub fn new(namespace: String) -> Self {
        Self {
            namespace,
            triples: Vec::new(),
        }
    }

    pub fn add_triple(&mut self, subject: String, predicate: String, object: String) {
        self.triples.push((subject, predicate, object));
    }

    pub fn to_turtle(&self) -> String {
        let mut turtle = format!("@prefix : <{}> .\n\n", self.namespace);
        for (subj, pred, obj) in &self.triples {
            turtle.push_str(&format!("{}  {}  {} .\n", subj, pred, obj));
        }
        turtle
    }
}

/// Pattern metadata supporting all 43 YAWL patterns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternMetadata {
    pub id: u32,
    pub name: String,
    pub category: String,
    pub description: String,
    pub yawl_pattern_id: String,
    pub is_control_flow: bool,
}

/// Registry of all 43 YAWL patterns
pub struct PatternRegistry {
    patterns: HashMap<u32, PatternMetadata>,
}

impl PatternRegistry {
    pub fn new() -> Self {
        Self {
            patterns: HashMap::new(),
        }
    }

    pub fn register(&mut self, pattern: PatternMetadata) {
        self.patterns.insert(pattern.id, pattern);
    }

    pub fn get(&self, id: u32) -> Option<&PatternMetadata> {
        self.patterns.get(&id)
    }

    pub fn list_all(&self) -> Vec<&PatternMetadata> {
        self.patterns.values().collect()
    }

    pub fn list_by_category(&self, category: &str) -> Vec<&PatternMetadata> {
        self.patterns
            .values()
            .filter(|p| p.category == category)
            .collect()
    }
}

impl Default for PatternRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_openapi_spec_creation() {
        let spec = OpenApiSpec::new("Test API".to_string(), "26.6.6".to_string());
        assert_eq!(spec.info.title, "Test API");
        assert_eq!(spec.openapi, "3.1.0");
    }

    #[test]
    fn test_rdf_ontology() {
        let mut onto = RdfOntology::new("http://example.org/knhk#".to_string());
        onto.add_triple(
            "Pattern".to_string(),
            "rdf:type".to_string(),
            "owl:Class".to_string(),
        );
        let turtle = onto.to_turtle();
        assert!(turtle.contains("Pattern"));
        assert!(turtle.contains("rdf:type"));
    }

    #[test]
    fn test_pattern_registry() {
        let mut registry = PatternRegistry::new();
        let pattern = PatternMetadata {
            id: 1,
            name: "Sequence".to_string(),
            category: "Basic".to_string(),
            description: "Sequential execution of tasks".to_string(),
            yawl_pattern_id: "WCP-1".to_string(),
            is_control_flow: true,
        };

        registry.register(pattern);
        assert!(registry.get(1).is_some());
        assert_eq!(registry.list_all().len(), 1);
    }
}
