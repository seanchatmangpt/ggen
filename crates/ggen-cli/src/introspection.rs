//! Autonomic Capability Introspection - v5.3.0 Feature
//!
//! Provides CLI introspection capabilities for AI agents to discover and understand
//! available verbs, their signatures, and command graph structure.
//!
//! # Examples
//!
//! ```bash
//! # List all available verbs for a specific noun
//! ggen --capabilities template generate
//!
//! # Show detailed verb metadata (types, validation, description)
//! ggen --introspect template generate
//!
//! # Export complete command graph for workflow planning
//! ggen --graph
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Verb metadata for introspection and discovery
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct VerbMetadata {
    pub noun: String,
    pub verb: String,
    pub description: String,
    pub arguments: Vec<ArgumentMetadata>,
    pub return_type: String,
    pub supports_json_output: bool,
}

/// Argument metadata for introspection
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct ArgumentMetadata {
    pub name: String,
    pub argument_type: String,
    pub optional: bool,
    pub description: String,
    pub default_value: Option<String>,
}

/// Complete command graph for workflow planning
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CommandGraph {
    pub version: String,
    pub nouns: HashMap<String, NounDescriptor>,
    pub total_verbs: usize,
}

/// Noun descriptor in command graph
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NounDescriptor {
    pub description: String,
    pub verbs: Vec<VerbInfo>,
}

/// Verb information in command graph
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct VerbInfo {
    pub name: String,
    pub description: String,
    pub argument_count: usize,
    pub required_arguments: usize,
}

/// Registry of all available verbs (seeded with v5.3.0 metadata)
pub fn get_verb_registry() -> HashMap<String, VerbMetadata> {
    let mut registry = HashMap::new();

    // AI module verbs
    registry.insert(
        "ai::generate-ontology".to_string(),
        VerbMetadata {
            noun: "ai".to_string(),
            verb: "generate-ontology".to_string(),
            description: "Generate RDF ontology from natural language prompt".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "prompt".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Natural language prompt to generate ontology from".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output file path for generated ontology (default: ontology.ttl)"
                        .to_string(),
                    default_value: Some("ontology.ttl".to_string()),
                },
            ],
            return_type: "GenerateOutput".to_string(),
            supports_json_output: true,
        },
    );

    // Template module verbs (sample)
    registry.insert(
        "template::generate".to_string(),
        VerbMetadata {
            noun: "template".to_string(),
            verb: "generate".to_string(),
            description: "Generate code from template with variable substitution".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Path to template file".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output file path (default: generated.rs)".to_string(),
                    default_value: Some("generated.rs".to_string()),
                },
                ArgumentMetadata {
                    name: "force".to_string(),
                    argument_type: "bool".to_string(),
                    optional: true,
                    description: "Overwrite existing output file".to_string(),
                    default_value: Some("false".to_string()),
                },
            ],
            return_type: "GenerateOutput".to_string(),
            supports_json_output: true,
        },
    );

    // Graph module verbs (sample)
    registry.insert(
        "graph::load".to_string(),
        VerbMetadata {
            noun: "graph".to_string(),
            verb: "load".to_string(),
            description: "Load RDF graph from file or URL".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "source".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "File path or URL to load graph from".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "RDF format (turtle, jsonld, rdfxml)".to_string(),
                    default_value: Some("turtle".to_string()),
                },
            ],
            return_type: "LoadOutput".to_string(),
            supports_json_output: true,
        },
    );

    // CI module verbs (sample)
    registry.insert(
        "ci::workflow".to_string(),
        VerbMetadata {
            noun: "ci".to_string(),
            verb: "workflow".to_string(),
            description: "Generate GitHub Actions workflow configuration".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "name".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Workflow name".to_string(),
                    default_value: Some("build".to_string()),
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output path for workflow file".to_string(),
                    default_value: None,
                },
            ],
            return_type: "WorkflowOutput".to_string(),
            supports_json_output: true,
        },
    );

    // FMEA module verbs (sample)
    registry.insert(
        "fmea::report".to_string(),
        VerbMetadata {
            noun: "fmea".to_string(),
            verb: "report".to_string(),
            description: "Generate FMEA report with failure mode analysis".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output format (text, json)".to_string(),
                    default_value: Some("text".to_string()),
                },
                ArgumentMetadata {
                    name: "risk".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Filter by risk level (CRITICAL, HIGH, MEDIUM, LOW)".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "top".to_string(),
                    argument_type: "Option<usize>".to_string(),
                    optional: true,
                    description: "Show top N failure modes".to_string(),
                    default_value: Some("20".to_string()),
                },
            ],
            return_type: "()".to_string(),
            supports_json_output: true,
        },
    );

    registry
}

/// Get metadata for a specific verb
///
/// # Arguments
/// * `noun` - The noun (e.g., "template")
/// * `verb` - The verb name (e.g., "generate")
///
/// # Returns
/// * `Some(VerbMetadata)` if verb exists
/// * `None` if verb not found
pub fn get_verb_metadata(noun: &str, verb: &str) -> Option<VerbMetadata> {
    let registry = get_verb_registry();
    let key = format!("{}::{}", noun, verb);
    registry.get(&key).cloned()
}

/// List all verbs for a given noun
///
/// # Arguments
/// * `noun` - The noun to query (e.g., "template")
///
/// # Returns
/// Vector of verbs available for that noun
pub fn list_verbs_for_noun(noun: &str) -> Vec<VerbMetadata> {
    let registry = get_verb_registry();
    registry
        .values()
        .filter(|v| v.noun == noun)
        .cloned()
        .collect()
}

/// Build complete command graph for all verbs
pub fn build_command_graph() -> CommandGraph {
    let registry = get_verb_registry();
    let mut nouns: HashMap<String, NounDescriptor> = HashMap::new();
    let mut total_verbs = 0;

    for metadata in registry.values() {
        let noun_entry = nouns
            .entry(metadata.noun.clone())
            .or_insert_with(|| NounDescriptor {
                description: format!("{} commands", metadata.noun),
                verbs: Vec::new(),
            });

        noun_entry.verbs.push(VerbInfo {
            name: metadata.verb.clone(),
            description: metadata.description.clone(),
            argument_count: metadata.arguments.len(),
            required_arguments: metadata.arguments.iter().filter(|a| !a.optional).count(),
        });

        total_verbs += 1;
    }

    CommandGraph {
        version: "5.3.0".to_string(),
        nouns,
        total_verbs,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_verb_registry_not_empty() {
        let registry = get_verb_registry();
        assert!(!registry.is_empty(), "Verb registry should not be empty");
    }

    #[test]
    fn test_get_verb_metadata_existing() {
        let metadata = get_verb_metadata("template", "generate");
        assert!(metadata.is_some(), "template::generate should exist");
        let m = metadata.unwrap();
        assert_eq!(m.noun, "template");
        assert_eq!(m.verb, "generate");
    }

    #[test]
    fn test_get_verb_metadata_nonexisting() {
        let metadata = get_verb_metadata("nonexistent", "verb");
        assert!(metadata.is_none(), "nonexistent verb should not exist");
    }

    #[test]
    fn test_list_verbs_for_noun_template() {
        let verbs = list_verbs_for_noun("template");
        assert!(!verbs.is_empty(), "template noun should have verbs");
        assert!(
            verbs.iter().any(|v| v.verb == "generate"),
            "template should have generate verb"
        );
    }

    #[test]
    fn test_list_verbs_for_noun_nonexisting() {
        let verbs = list_verbs_for_noun("nonexistent_noun");
        assert!(verbs.is_empty(), "nonexistent noun should have no verbs");
    }

    #[test]
    fn test_command_graph_structure() {
        let graph = build_command_graph();
        assert!(!graph.nouns.is_empty(), "Command graph should have nouns");
        assert!(graph.total_verbs > 0, "Command graph should have verbs");
        assert_eq!(graph.version, "5.3.0");
    }

    #[test]
    fn test_command_graph_ai_noun() {
        let graph = build_command_graph();
        assert!(
            graph.nouns.contains_key("ai"),
            "Graph should contain ai noun"
        );
    }

    #[test]
    fn test_argument_metadata_optional() {
        let metadata = get_verb_metadata("template", "generate").unwrap();
        let template_arg = metadata
            .arguments
            .iter()
            .find(|a| a.name == "template")
            .unwrap();
        assert!(template_arg.optional, "template arg should be optional");
    }

    #[test]
    fn test_argument_metadata_required() {
        let metadata = get_verb_metadata("ai", "generate-ontology").unwrap();
        let prompt_arg = metadata
            .arguments
            .iter()
            .find(|a| a.name == "prompt")
            .unwrap();
        assert!(!prompt_arg.optional, "prompt arg should be required");
    }

    #[test]
    fn test_verb_metadata_serialization() {
        let metadata = get_verb_metadata("template", "generate").unwrap();
        let json = serde_json::to_string(&metadata).unwrap();
        assert!(json.contains("\"noun\":\"template\""));
        assert!(json.contains("\"verb\":\"generate\""));
    }

    #[test]
    fn test_command_graph_json_export() {
        let graph = build_command_graph();
        let json = serde_json::to_string(&graph).unwrap();
        assert!(json.contains("\"version\":\"5.3.0\""));
        assert!(json.contains("\"total_verbs\":"));
    }

    #[test]
    fn test_all_verbs_have_descriptions() {
        let registry = get_verb_registry();
        for metadata in registry.values() {
            assert!(
                !metadata.description.is_empty(),
                "Verb {}/{} should have description",
                metadata.noun,
                metadata.verb
            );
        }
    }

    #[test]
    fn test_all_arguments_have_descriptions() {
        let registry = get_verb_registry();
        for metadata in registry.values() {
            for arg in &metadata.arguments {
                assert!(
                    !arg.description.is_empty(),
                    "Argument {}/{}/{} should have description",
                    metadata.noun,
                    metadata.verb,
                    arg.name
                );
            }
        }
    }

    #[test]
    fn test_required_arguments_count() {
        let metadata = get_verb_metadata("ai", "generate-ontology").unwrap();
        let required = metadata.arguments.iter().filter(|a| !a.optional).count();
        assert_eq!(
            required, 1,
            "generate-ontology should have 1 required argument"
        );
    }

    #[test]
    fn test_optional_arguments_count() {
        let metadata = get_verb_metadata("template", "generate").unwrap();
        let optional = metadata.arguments.iter().filter(|a| a.optional).count();
        assert!(optional > 0, "generate should have optional arguments");
    }
}
