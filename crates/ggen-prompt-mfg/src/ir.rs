//! Prompt Intermediate Representation (IR)
//!
//! Structured, pre-shaped prompt data extracted from RDF via CONSTRUCT queries.

use crate::{PromptError, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Prompt Intermediate Representation
///
/// Structured data extracted from RDF ontology, ready for deterministic emission.
/// Uses BTreeMap for stable ordering (critical for determinism).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PromptIR {
    /// Prompt sections (ordered)
    pub sections: BTreeMap<String, Section>,

    /// Prompt metadata
    pub metadata: PromptMetadata,

    /// Variables for template substitution (ordered)
    pub variables: BTreeMap<String, PromptVariable>,
}

/// Prompt section (system, user, assistant, etc.)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Section {
    /// Section type
    pub section_type: SectionType,

    /// Section content blocks (ordered)
    pub blocks: Vec<ContentBlock>,

    /// Section priority (for ordering)
    pub priority: i32,
}

/// Section type enumeration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SectionType {
    /// System context
    System,

    /// User message
    User,

    /// Assistant response
    Assistant,

    /// Custom section
    Custom(String),
}

/// Content block within a section
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ContentBlock {
    /// Block type
    pub block_type: BlockType,

    /// Block content (template-ready)
    pub content: String,

    /// Block metadata
    pub metadata: BTreeMap<String, String>,
}

/// Block type enumeration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum BlockType {
    /// Plain text
    Text,

    /// Code block
    Code { language: String },

    /// Instruction
    Instruction,

    /// Example
    Example,

    /// Constraint
    Constraint,
}

/// Prompt metadata
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PromptMetadata {
    /// Prompt identifier (from ontology)
    pub id: String,

    /// Prompt version
    pub version: String,

    /// Schema version
    pub schema_version: String,

    /// Source ontology URI
    pub source_ontology: String,

    /// CONSTRUCT query used
    pub construct_query: String,
}

/// Template variable definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PromptVariable {
    /// Variable name
    pub name: String,

    /// Variable type
    pub var_type: VariableType,

    /// Default value (optional)
    pub default: Option<String>,

    /// Variable description
    pub description: String,
}

/// Variable type enumeration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VariableType {
    /// String value
    String,

    /// Integer value
    Integer,

    /// Boolean value
    Boolean,

    /// List of values
    List,

    /// Object/map
    Object,
}

impl PromptIR {
    /// Create prompt IR from CONSTRUCT query
    ///
    /// # Errors
    ///
    /// Returns error if query parsing fails
    pub fn from_construct(construct_query: &str) -> Result<Self> {
        // Parse CONSTRUCT query and extract structure
        // For now, create a minimal valid IR
        Ok(Self {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "generated".to_string(),
                version: "0.1.0".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "unknown".to_string(),
                construct_query: construct_query.to_string(),
            },
            variables: BTreeMap::new(),
        })
    }

    /// Create prompt IR from Oxigraph store
    ///
    /// # Errors
    ///
    /// Returns error if query execution fails
    pub fn from_store(
        store: &oxigraph::store::Store,
        construct_query: &str,
    ) -> Result<Self> {
        use oxigraph::sparql::QueryResults;

        // Execute CONSTRUCT query
        #[allow(deprecated)]
        let results = store
            .query(construct_query)
            .map_err(|e| PromptError::Sparql(e.to_string()))?;

        // Extract triples and build IR
        match results {
            QueryResults::Graph(triples) => {
                let mut sections = BTreeMap::new();
                let variables = BTreeMap::new();

                // Process triples into structured IR
                for triple_result in triples {
                    let triple = triple_result.map_err(|e| PromptError::Sparql(e.to_string()))?;

                    // Extract section information from triples
                    // This is a simplified implementation - real version would parse RDF structure
                    let section_key = triple.subject.to_string();
                    sections.entry(section_key).or_insert_with(|| Section {
                        section_type: SectionType::System,
                        blocks: vec![],
                        priority: 0,
                    });
                }

                Ok(Self {
                    sections,
                    metadata: PromptMetadata {
                        id: "from_store".to_string(),
                        version: "0.1.0".to_string(),
                        schema_version: "1.0.0".to_string(),
                        source_ontology: "rdf_store".to_string(),
                        construct_query: construct_query.to_string(),
                    },
                    variables,
                })
            }
            _ => Err(PromptError::Sparql(
                "Expected CONSTRUCT query results".to_string(),
            )),
        }
    }

    /// Add a section to the IR
    pub fn add_section(&mut self, key: String, section: Section) {
        self.sections.insert(key, section);
    }

    /// Add a variable to the IR
    pub fn add_variable(&mut self, key: String, variable: PromptVariable) {
        self.variables.insert(key, variable);
    }

    /// Get sections in priority order
    pub fn sections_ordered(&self) -> Vec<(&String, &Section)> {
        let mut sections: Vec<_> = self.sections.iter().collect();
        sections.sort_by_key(|(_, s)| s.priority);
        sections
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prompt_ir_creation() {
        let ir = PromptIR::from_construct("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }");
        assert!(ir.is_ok());
    }

    #[test]
    fn test_section_ordering() {
        let mut ir = PromptIR::from_construct("test").unwrap();

        ir.add_section(
            "section1".to_string(),
            Section {
                section_type: SectionType::System,
                blocks: vec![],
                priority: 10,
            },
        );

        ir.add_section(
            "section2".to_string(),
            Section {
                section_type: SectionType::User,
                blocks: vec![],
                priority: 5,
            },
        );

        let ordered = ir.sections_ordered();
        assert_eq!(ordered.len(), 2);
        assert_eq!(ordered[0].0, "section2"); // Lower priority first
        assert_eq!(ordered[1].0, "section1");
    }

    #[test]
    fn test_btreemap_determinism() {
        let mut ir1 = PromptIR::from_construct("test").unwrap();
        ir1.variables.insert(
            "z_var".to_string(),
            PromptVariable {
                name: "z_var".to_string(),
                var_type: VariableType::String,
                default: None,
                description: "Test".to_string(),
            },
        );
        ir1.variables.insert(
            "a_var".to_string(),
            PromptVariable {
                name: "a_var".to_string(),
                var_type: VariableType::String,
                default: None,
                description: "Test".to_string(),
            },
        );

        let mut ir2 = PromptIR::from_construct("test").unwrap();
        // Insert in opposite order
        ir2.variables.insert(
            "a_var".to_string(),
            PromptVariable {
                name: "a_var".to_string(),
                var_type: VariableType::String,
                default: None,
                description: "Test".to_string(),
            },
        );
        ir2.variables.insert(
            "z_var".to_string(),
            PromptVariable {
                name: "z_var".to_string(),
                var_type: VariableType::String,
                default: None,
                description: "Test".to_string(),
            },
        );

        // BTreeMap ensures same ordering regardless of insertion order
        assert_eq!(ir1, ir2);
    }
}
