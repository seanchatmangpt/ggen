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

#![cfg(feature = "autonomic")]

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

/// Registry of all available verbs (complete v4.0.0 - 47 verbs across 9 modules)
pub fn get_verb_registry() -> HashMap<String, VerbMetadata> {
    let mut registry = HashMap::new();

    // ========================================
    // AI MODULE (6 verbs)
    // ========================================

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
                    description: "Natural language description of ontology".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output file path (default: ontology.ttl)".to_string(),
                    default_value: Some("ontology.ttl".to_string()),
                },
            ],
            return_type: "GenerateOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ai::analyze-model".to_string(),
        VerbMetadata {
            noun: "ai".to_string(),
            verb: "analyze-model".to_string(),
            description: "Analyze existing code/models and extract structure".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "input".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Source file to analyze".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "language".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Language hint (default: auto-detect)".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output analysis file".to_string(),
                    default_value: None,
                },
            ],
            return_type: "AnalysisOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ai::generate-schema".to_string(),
        VerbMetadata {
            noun: "ai".to_string(),
            verb: "generate-schema".to_string(),
            description: "Generate JSON Schema from RDF ontology".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output schema file".to_string(),
                    default_value: None,
                },
            ],
            return_type: "SchemaOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ai::validate-model".to_string(),
        VerbMetadata {
            noun: "ai".to_string(),
            verb: "validate-model".to_string(),
            description: "Validate model against semantic rules".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file to validate".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "strict".to_string(),
                    argument_type: "bool".to_string(),
                    optional: true,
                    description: "Enable strict validation".to_string(),
                    default_value: Some("false".to_string()),
                },
            ],
            return_type: "ValidationOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ai::export-types".to_string(),
        VerbMetadata {
            noun: "ai".to_string(),
            verb: "export-types".to_string(),
            description: "Export type definitions for a model".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "language".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Target language (rust|typescript|python)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ExportOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ai::infer-relationships".to_string(),
        VerbMetadata {
            noun: "ai".to_string(),
            verb: "infer-relationships".to_string(),
            description: "Infer implicit relationships in ontology".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output file for inferred relationships".to_string(),
                    default_value: None,
                },
            ],
            return_type: "InferOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // TEMPLATE MODULE (7 verbs)
    // ========================================

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

    registry.insert(
        "template::validate".to_string(),
        VerbMetadata {
            noun: "template".to_string(),
            verb: "validate".to_string(),
            description: "Validate template syntax and variables".to_string(),
            arguments: vec![ArgumentMetadata {
                name: "template".to_string(),
                argument_type: "String".to_string(),
                optional: false,
                description: "Template file to validate".to_string(),
                default_value: None,
            }],
            return_type: "ValidationOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "template::list".to_string(),
        VerbMetadata {
            noun: "template".to_string(),
            verb: "list".to_string(),
            description: "List all available templates".to_string(),
            arguments: vec![ArgumentMetadata {
                name: "category".to_string(),
                argument_type: "Option<String>".to_string(),
                optional: true,
                description: "Filter by template category".to_string(),
                default_value: None,
            }],
            return_type: "ListOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "template::preview".to_string(),
        VerbMetadata {
            noun: "template".to_string(),
            verb: "preview".to_string(),
            description: "Preview template output without writing".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Template file to preview".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "variables".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Variables file (JSON)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "PreviewOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "template::export".to_string(),
        VerbMetadata {
            noun: "template".to_string(),
            verb: "export".to_string(),
            description: "Export template in different format".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Template file to export".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Export format (liquid|jinja2|tera)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ExportOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "template::import".to_string(),
        VerbMetadata {
            noun: "template".to_string(),
            verb: "import".to_string(),
            description: "Import template from external source".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "source".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Source URL or path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "name".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Template name".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ImportOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "template::generate-rdf".to_string(),
        VerbMetadata {
            noun: "template".to_string(),
            verb: "generate-rdf".to_string(),
            description: "Generate from ontology using RDF-based templates".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Template name".to_string(),
                    default_value: None,
                },
            ],
            return_type: "GenerateOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // GRAPH MODULE (5 verbs)
    // ========================================

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
                    description: "File path or URL to load".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "RDF format (turtle|jsonld|rdfxml)".to_string(),
                    default_value: Some("turtle".to_string()),
                },
            ],
            return_type: "LoadOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "graph::query".to_string(),
        VerbMetadata {
            noun: "graph".to_string(),
            verb: "query".to_string(),
            description: "Execute SPARQL query on loaded graph".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "graph".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Graph file to query".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "query".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "SPARQL query string".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output format".to_string(),
                    default_value: Some("turtle".to_string()),
                },
            ],
            return_type: "QueryOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "graph::export".to_string(),
        VerbMetadata {
            noun: "graph".to_string(),
            verb: "export".to_string(),
            description: "Export graph in different RDF format".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "source".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Source graph file".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Export format".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output file path".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ExportOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "graph::validate".to_string(),
        VerbMetadata {
            noun: "graph".to_string(),
            verb: "validate".to_string(),
            description: "Validate RDF graph structure".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "source".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Graph file to validate".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "strict".to_string(),
                    argument_type: "bool".to_string(),
                    optional: true,
                    description: "Enable strict validation".to_string(),
                    default_value: Some("false".to_string()),
                },
            ],
            return_type: "ValidationOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "graph::merge".to_string(),
        VerbMetadata {
            noun: "graph".to_string(),
            verb: "merge".to_string(),
            description: "Merge multiple RDF graphs".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "graphs".to_string(),
                    argument_type: "Vec<String>".to_string(),
                    optional: false,
                    description: "Graph files to merge".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output merged graph file".to_string(),
                    default_value: None,
                },
            ],
            return_type: "MergeOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // ONTOLOGY MODULE (4 verbs)
    // ========================================

    registry.insert(
        "ontology::extract".to_string(),
        VerbMetadata {
            noun: "ontology".to_string(),
            verb: "extract".to_string(),
            description: "Extract ontology from code/documentation".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "source".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Source file to extract from".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "language".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Source language hint".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output ontology file".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ExtractOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ontology::generate".to_string(),
        VerbMetadata {
            noun: "ontology".to_string(),
            verb: "generate".to_string(),
            description: "Generate ontology from specification".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "spec".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Specification file (YAML)".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output ontology file".to_string(),
                    default_value: None,
                },
            ],
            return_type: "GenerateOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ontology::validate".to_string(),
        VerbMetadata {
            noun: "ontology".to_string(),
            verb: "validate".to_string(),
            description: "Validate ontology structure and semantics".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file to validate".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "strict".to_string(),
                    argument_type: "bool".to_string(),
                    optional: true,
                    description: "Enable strict validation".to_string(),
                    default_value: Some("false".to_string()),
                },
            ],
            return_type: "ValidationOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ontology::init".to_string(),
        VerbMetadata {
            noun: "ontology".to_string(),
            verb: "init".to_string(),
            description: "Initialize new ontology project".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "name".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Project name".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Project template (starter|advanced|minimal)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "InitOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // PROJECT MODULE (4 verbs)
    // ========================================

    registry.insert(
        "project::new".to_string(),
        VerbMetadata {
            noun: "project".to_string(),
            verb: "new".to_string(),
            description: "Create new project from template".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "name".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Project name".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Project template (rust|typescript|python)".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output directory".to_string(),
                    default_value: None,
                },
            ],
            return_type: "NewProjectOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "project::generate".to_string(),
        VerbMetadata {
            noun: "project".to_string(),
            verb: "generate".to_string(),
            description: "Generate project structure from ontology".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Project template".to_string(),
                    default_value: None,
                },
            ],
            return_type: "GenerateOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "project::plan".to_string(),
        VerbMetadata {
            noun: "project".to_string(),
            verb: "plan".to_string(),
            description: "Plan code generation strategy".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output plan file (JSON)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "PlanOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "project::validate".to_string(),
        VerbMetadata {
            noun: "project".to_string(),
            verb: "validate".to_string(),
            description: "Validate project structure".to_string(),
            arguments: vec![ArgumentMetadata {
                name: "output".to_string(),
                argument_type: "Option<String>".to_string(),
                optional: true,
                description: "Output validation report (JSON)".to_string(),
                default_value: None,
            }],
            return_type: "ValidationOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // PAPER MODULE (3 verbs)
    // ========================================

    registry.insert(
        "paper::export".to_string(),
        VerbMetadata {
            noun: "paper".to_string(),
            verb: "export".to_string(),
            description: "Export documentation as academic paper".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "source".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Source documentation directory".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output format (pdf|html|markdown)".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output file path".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ExportOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "paper::compile".to_string(),
        VerbMetadata {
            noun: "paper".to_string(),
            verb: "compile".to_string(),
            description: "Compile paper from LaTeX/Markdown sources".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "source".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Source paper file".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output file path".to_string(),
                    default_value: None,
                },
            ],
            return_type: "CompileOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "paper::init-bibliography".to_string(),
        VerbMetadata {
            noun: "paper".to_string(),
            verb: "init-bibliography".to_string(),
            description: "Initialize bibliography from ontology".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "format".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Bibliography format (bibtex|csl)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "BibliographyOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // CI MODULE (4 verbs)
    // ========================================

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
                    description: "Output workflow file".to_string(),
                    default_value: None,
                },
            ],
            return_type: "WorkflowOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ci::action".to_string(),
        VerbMetadata {
            noun: "ci".to_string(),
            verb: "action".to_string(),
            description: "Generate reusable CI action".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "name".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Action name".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "language".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Action language (rust|typescript)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ActionOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ci::pipeline".to_string(),
        VerbMetadata {
            noun: "ci".to_string(),
            verb: "pipeline".to_string(),
            description: "Generate complete CI/CD pipeline".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "template".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Pipeline template (standard|advanced)".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output directory".to_string(),
                    default_value: None,
                },
            ],
            return_type: "PipelineOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "ci::validate".to_string(),
        VerbMetadata {
            noun: "ci".to_string(),
            verb: "validate".to_string(),
            description: "Validate CI configuration".to_string(),
            arguments: vec![ArgumentMetadata {
                name: "workflow".to_string(),
                argument_type: "String".to_string(),
                optional: false,
                description: "Workflow file to validate".to_string(),
                default_value: None,
            }],
            return_type: "ValidationOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // WORKFLOW MODULE (2 verbs)
    // ========================================

    registry.insert(
        "workflow::generate".to_string(),
        VerbMetadata {
            noun: "workflow".to_string(),
            verb: "generate".to_string(),
            description: "Generate workflow specification".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "ontology".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Ontology file path".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "output".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Output workflow file (YAML)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "GenerateOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "workflow::execute".to_string(),
        VerbMetadata {
            noun: "workflow".to_string(),
            verb: "execute".to_string(),
            description: "Execute workflow definition".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "workflow".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Workflow file to execute".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "dry-run".to_string(),
                    argument_type: "bool".to_string(),
                    optional: true,
                    description: "Dry run mode (no actual execution)".to_string(),
                    default_value: Some("false".to_string()),
                },
            ],
            return_type: "ExecuteOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // FMEA MODULE (utils fmea - 5 verbs)
    // ========================================

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
                    description: "Output format (text|json)".to_string(),
                    default_value: Some("text".to_string()),
                },
                ArgumentMetadata {
                    name: "risk".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Filter by risk level (CRITICAL|HIGH|MEDIUM|LOW)".to_string(),
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

    registry.insert(
        "fmea::pareto".to_string(),
        VerbMetadata {
            noun: "fmea".to_string(),
            verb: "pareto".to_string(),
            description: "Generate Pareto analysis (80/20 rule) for failure modes".to_string(),
            arguments: vec![],
            return_type: "()".to_string(),
            supports_json_output: false,
        },
    );

    registry.insert(
        "fmea::list".to_string(),
        VerbMetadata {
            noun: "fmea".to_string(),
            verb: "list".to_string(),
            description: "List failure modes with filters".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "category".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Filter by category".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "sort".to_string(),
                    argument_type: "Option<String>".to_string(),
                    optional: true,
                    description: "Sort by (rpn|severity|id)".to_string(),
                    default_value: None,
                },
            ],
            return_type: "ListOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "fmea::show".to_string(),
        VerbMetadata {
            noun: "fmea".to_string(),
            verb: "show".to_string(),
            description: "Show specific failure mode details".to_string(),
            arguments: vec![
                ArgumentMetadata {
                    name: "mode-id".to_string(),
                    argument_type: "String".to_string(),
                    optional: false,
                    description: "Failure mode ID".to_string(),
                    default_value: None,
                },
                ArgumentMetadata {
                    name: "events".to_string(),
                    argument_type: "bool".to_string(),
                    optional: true,
                    description: "Include event history".to_string(),
                    default_value: Some("false".to_string()),
                },
            ],
            return_type: "ShowOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "fmea::export".to_string(),
        VerbMetadata {
            noun: "fmea".to_string(),
            verb: "export".to_string(),
            description: "Export FMEA data to JSON".to_string(),
            arguments: vec![ArgumentMetadata {
                name: "output".to_string(),
                argument_type: "Option<String>".to_string(),
                optional: true,
                description: "Output file path".to_string(),
                default_value: None,
            }],
            return_type: "ExportOutput".to_string(),
            supports_json_output: true,
        },
    );

    // ========================================
    // UTILS MODULE (2 additional verbs - doctor and env)
    // ========================================

    registry.insert(
        "utils::doctor".to_string(),
        VerbMetadata {
            noun: "utils".to_string(),
            verb: "doctor".to_string(),
            description: "System health check".to_string(),
            arguments: vec![ArgumentMetadata {
                name: "detailed".to_string(),
                argument_type: "bool".to_string(),
                optional: true,
                description: "Show detailed health information".to_string(),
                default_value: Some("false".to_string()),
            }],
            return_type: "DoctorOutput".to_string(),
            supports_json_output: true,
        },
    );

    registry.insert(
        "utils::env".to_string(),
        VerbMetadata {
            noun: "utils".to_string(),
            verb: "env".to_string(),
            description: "Show environment information".to_string(),
            arguments: vec![ArgumentMetadata {
                name: "json".to_string(),
                argument_type: "bool".to_string(),
                optional: true,
                description: "Output as JSON".to_string(),
                default_value: Some("false".to_string()),
            }],
            return_type: "EnvOutput".to_string(),
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
    fn test_verb_registry_complete() {
        let registry = get_verb_registry();
        assert_eq!(
            registry.len(),
            47,
            "Registry should contain exactly 47 verbs"
        );
    }

    #[test]
    fn test_command_graph_total_verbs() {
        let graph = build_command_graph();
        assert_eq!(
            graph.total_verbs, 47,
            "Command graph should report 47 total verbs"
        );
    }

    #[test]
    fn test_all_modules_present() {
        let graph = build_command_graph();
        let expected_nouns = vec![
            "ai", "template", "graph", "ontology", "project", "paper", "ci", "workflow", "fmea",
            "utils",
        ];
        for noun in expected_nouns {
            assert!(
                graph.nouns.contains_key(noun),
                "Graph should contain {} noun",
                noun
            );
        }
    }

    #[test]
    fn test_ai_module_verbs() {
        let verbs = list_verbs_for_noun("ai");
        assert_eq!(verbs.len(), 6, "AI module should have 6 verbs");
        let verb_names: Vec<String> = verbs.iter().map(|v| v.verb.clone()).collect();
        assert!(verb_names.contains(&"generate-ontology".to_string()));
        assert!(verb_names.contains(&"analyze-model".to_string()));
        assert!(verb_names.contains(&"generate-schema".to_string()));
        assert!(verb_names.contains(&"validate-model".to_string()));
        assert!(verb_names.contains(&"export-types".to_string()));
        assert!(verb_names.contains(&"infer-relationships".to_string()));
    }

    #[test]
    fn test_template_module_verbs() {
        let verbs = list_verbs_for_noun("template");
        assert_eq!(verbs.len(), 7, "Template module should have 7 verbs");
    }

    #[test]
    fn test_graph_module_verbs() {
        let verbs = list_verbs_for_noun("graph");
        assert_eq!(verbs.len(), 5, "Graph module should have 5 verbs");
    }

    #[test]
    fn test_ontology_module_verbs() {
        let verbs = list_verbs_for_noun("ontology");
        assert_eq!(verbs.len(), 4, "Ontology module should have 4 verbs");
    }

    #[test]
    fn test_project_module_verbs() {
        let verbs = list_verbs_for_noun("project");
        assert_eq!(verbs.len(), 4, "Project module should have 4 verbs");
    }

    #[test]
    fn test_paper_module_verbs() {
        let verbs = list_verbs_for_noun("paper");
        assert_eq!(verbs.len(), 3, "Paper module should have 3 verbs");
    }

    #[test]
    fn test_ci_module_verbs() {
        let verbs = list_verbs_for_noun("ci");
        assert_eq!(verbs.len(), 4, "CI module should have 4 verbs");
    }

    #[test]
    fn test_workflow_module_verbs() {
        let verbs = list_verbs_for_noun("workflow");
        assert_eq!(verbs.len(), 2, "Workflow module should have 2 verbs");
    }

    #[test]
    fn test_fmea_module_verbs() {
        let verbs = list_verbs_for_noun("fmea");
        assert_eq!(verbs.len(), 5, "FMEA module should have 5 verbs");
    }

    #[test]
    fn test_utils_module_verbs() {
        let verbs = list_verbs_for_noun("utils");
        assert_eq!(verbs.len(), 2, "Utils module should have 2 verbs");
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
    fn test_command_graph_structure() {
        let graph = build_command_graph();
        assert!(!graph.nouns.is_empty(), "Command graph should have nouns");
        assert!(graph.total_verbs > 0, "Command graph should have verbs");
        assert_eq!(graph.version, "5.3.0");
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
        assert!(json.contains("\"total_verbs\":47"));
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
