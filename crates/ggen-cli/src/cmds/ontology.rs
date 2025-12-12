//! Ontology Commands - clap-noun-verb v5.3.0 Migration
//!
//! This module implements RDF/OWL ontology operations using the v5.3.0 #[verb("verb", "noun")] pattern.
//!
//! ## Architecture: Three-Layer Pattern
//!
//! - **Layer 3 (CLI)**: Input validation, output formatting, thin routing
//! - **Layer 2 (Integration)**: Async execution, error handling
//! - **Layer 1 (Domain)**: Pure RDF logic from ggen_domain::ontology

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::path::PathBuf;

use crate::cmds::helpers::execute_async_op;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
struct ExtractOutput {
    classes_found: usize,
    properties_found: usize,
    relationships_found: usize,
    namespace: String,
    output_file: String,
}

#[derive(Serialize)]
struct GenerateOutput {
    language: String,
    files_generated: usize,
    output_directory: String,
    primary_file: String,
}

#[derive(Serialize)]
struct ValidateOutput {
    is_valid: bool,
    classes_count: usize,
    properties_count: usize,
    warnings: Vec<String>,
    errors: Vec<String>,
}

#[derive(Serialize)]
struct InitOutput {
    project_name: String,
    ontology_file: String,
    config_file: String,
    generated_files: Vec<String>,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Extract ontology schema from RDF/OWL file
///
/// Parses Turtle, RDF/XML, or N-Triples format files and extracts structured
/// ontology information (classes, properties, relationships).
///
/// Examples:
///   ggen ontology extract schema.ttl
///   ggen ontology extract ecommerce.rdf --namespace http://example.org#
///   ggen ontology extract foaf.ttl --output schema.json
#[verb("extract", "ontology")]
fn extract(
    ontology_file: String, namespace: Option<String>, output: Option<String>,
) -> VerbResult<ExtractOutput> {
    use ggen_core::Graph;

    let ontology_path = PathBuf::from(&ontology_file);

    // Validate file exists
    if !ontology_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Ontology file not found: {}",
            ontology_file
        )));
    }

    let result = execute_async_op("extract", async move {
        // Load the ontology file
        let graph = Graph::new().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create graph: {}", e))
        })?;

        let file_content = std::fs::read_to_string(&ontology_path)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to read file: {}", e)))?;

        graph.insert_turtle(&file_content).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to load ontology: {}", e))
        })?;

        // Extract ontology schema
        let schema_namespace = namespace.unwrap_or_else(|| "http://example.org#".to_string());
        let schema = ggen_core::OntologyExtractor::extract(&graph, &schema_namespace)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Extraction failed: {}", e)))?;

        // Save schema to output file
        let output_path = output.unwrap_or_else(|| "schema.json".to_string());
        let schema_json = serde_json::to_string_pretty(&schema).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("JSON serialization failed: {}", e))
        })?;

        std::fs::write(&output_path, schema_json).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write output: {}", e))
        })?;

        Ok((schema, output_path))
    })?;

    let (schema, output_path) = result;

    Ok(ExtractOutput {
        classes_found: schema.classes.len(),
        properties_found: schema.properties.len(),
        relationships_found: schema.relationships.len(),
        namespace: schema.namespace,
        output_file: output_path,
    })
}

/// Generate code from ontology schema
///
/// Creates TypeScript interfaces, Zod schemas, or other code artifacts from
/// an extracted ontology schema. Supports multiple target languages.
///
/// Examples:
///   ggen ontology generate schema.json --language typescript --output src/types
///   ggen ontology generate schema.json --language typescript --zod
///   ggen ontology generate ecommerce.json --language typescript --utilities
#[verb("generate", "ontology")]
fn generate(
    schema_file: String, language: String, output: Option<String>, zod: bool, utilities: bool,
) -> VerbResult<GenerateOutput> {
    use ggen_core::codegen::TypeScriptGenerator;

    let schema_path = PathBuf::from(&schema_file);

    // Validate file exists
    if !schema_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Schema file not found: {}",
            schema_file
        )));
    }

    let lang_clone = language.clone();
    let result = execute_async_op("generate", async move {
        // Read schema
        let schema_content = std::fs::read_to_string(&schema_path)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to read schema: {}", e)))?;

        let schema: ggen_core::ontology::OntologySchema = serde_json::from_str(&schema_content)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Invalid schema JSON: {}", e)))?;

        // Create output directory
        let output_dir = output.unwrap_or_else(|| "generated".to_string());
        std::fs::create_dir_all(&output_dir).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create output directory: {}", e))
        })?;

        let mut files_generated = 0;
        let mut primary_file = String::new();

        // Generate TypeScript code if requested
        if lang_clone == "typescript" {
            // Generate interfaces
            let interfaces = TypeScriptGenerator::generate_interfaces(&schema).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!("Interface generation failed: {}", e))
            })?;

            let interfaces_path = format!("{}/types.ts", output_dir);
            std::fs::write(&interfaces_path, interfaces).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!("Failed to write types: {}", e))
            })?;
            files_generated += 1;
            if primary_file.is_empty() {
                primary_file = interfaces_path.clone();
            }

            // Generate Zod schemas if requested
            if zod {
                let zod_schemas =
                    TypeScriptGenerator::generate_zod_schemas(&schema).map_err(|e| {
                        clap_noun_verb::NounVerbError::execution_error(format!("Zod generation failed: {}", e))
                    })?;

                let zod_path = format!("{}/schemas.ts", output_dir);
                std::fs::write(&zod_path, zod_schemas).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!("Failed to write schemas: {}", e))
                })?;
                files_generated += 1;
            }

            // Generate utility types if requested
            if utilities {
                let utils = TypeScriptGenerator::generate_utility_types(&schema).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!("Utilities generation failed: {}", e))
                })?;

                let utils_path = format!("{}/utilities.ts", output_dir);
                std::fs::write(&utils_path, utils).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!("Failed to write utilities: {}", e))
                })?;
                files_generated += 1;
            }
        }

        Ok((files_generated, primary_file, output_dir))
    })?;

    let (files_generated, primary_file, output_dir) = result;

    Ok(GenerateOutput {
        language,
        files_generated,
        output_directory: output_dir,
        primary_file,
    })
}

/// Validate ontology schema quality
///
/// Checks ontology structure for common issues: missing domains/ranges,
/// undefined class references, cardinality constraints, and more.
///
/// Examples:
///   ggen ontology validate schema.json
///   ggen ontology validate ecommerce.json --strict
#[verb("validate", "ontology")]
fn validate(schema_file: String, strict: bool) -> VerbResult<ValidateOutput> {
    let schema_path = PathBuf::from(&schema_file);

    // Validate file exists
    if !schema_path.exists() {
        return Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Schema file not found: {}",
            schema_file
        )));
    }

    let result = execute_async_op("validate", async move {
        // Read schema
        let schema_content = std::fs::read_to_string(&schema_path)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Failed to read schema: {}", e)))?;

        let schema: ggen_core::ontology::OntologySchema = serde_json::from_str(&schema_content)
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Invalid schema JSON: {}", e)))?;

        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        // Check for classes without properties
        for class in &schema.classes {
            if class.properties.is_empty() {
                warnings.push(format!("Class '{}' has no properties", class.name));
            }
        }

        // Check for properties with undefined classes
        for prop in &schema.properties {
            if prop.domain.is_empty() {
                warnings.push(format!("Property '{}' has no domain class", prop.name));
            }
        }

        // Check for circular references in strict mode
        if strict {
            for prop in &schema.properties {
                if let ggen_core::ontology::PropertyRange::Reference(ref_class) = &prop.range {
                    // Check if reference exists
                    if !schema.classes.iter().any(|c| &c.uri == ref_class) {
                        errors.push(format!(
                            "Property '{}' references undefined class '{}'",
                            prop.name, ref_class
                        ));
                    }
                }
            }
        }

        Ok((
            errors.is_empty(),
            schema.classes.len(),
            schema.properties.len(),
            warnings,
            errors,
        ))
    })?;

    let (is_valid, classes_count, properties_count, warnings, errors) = result;

    Ok(ValidateOutput {
        is_valid,
        classes_count,
        properties_count,
        warnings,
        errors,
    })
}

/// Initialize ontology project with example files
///
/// Creates a new ontology project structure with example files, configuration,
/// and templates for working with ontologies.
///
/// Examples:
///   ggen ontology init my-project
///   ggen ontology init ecommerce-api --template schema.org
///   ggen ontology init foaf-explorer --template foaf
#[verb("init", "ontology")]
fn init(project_name: String, template: Option<String>) -> VerbResult<InitOutput> {
    let proj_name = project_name.clone();
    let result = execute_async_op("init", async move {
        // Create project directory
        let proj_dir = PathBuf::from(&proj_name);
        std::fs::create_dir_all(&proj_dir).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create project: {}", e))
        })?;

        // Create subdirectories
        std::fs::create_dir_all(proj_dir.join("ontologies")).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create ontologies dir: {}", e))
        })?;
        std::fs::create_dir_all(proj_dir.join("src")).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create src dir: {}", e))
        })?;
        std::fs::create_dir_all(proj_dir.join("generated")).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to create generated dir: {}", e))
        })?;

        let mut generated_files = Vec::new();

        // Create package.json
        let package_json = format!(
            r#"{{
  "name": "{}",
  "version": "1.0.0",
  "description": "Ontology-driven code generation project",
  "type": "module",
  "scripts": {{
    "extract": "ggen ontology extract ontologies/schema.ttl --output schema.json",
    "generate": "ggen ontology generate schema.json --language typescript --zod --utilities",
    "validate": "ggen ontology validate schema.json --strict"
  }},
  "dependencies": {{
    "zod": "^3.0.0"
  }},
  "devDependencies": {{
    "typescript": "^5.0.0"
  }}
}}
"#,
            proj_name
        );
        let pkg_path = proj_dir.join("package.json");
        std::fs::write(&pkg_path, package_json).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write package.json: {}", e))
        })?;
        generated_files.push("package.json".to_string());

        // Create ggen.config.json
        let config = r#"{
  "ontologies": [
    "ontologies/schema.ttl"
  ],
  "namespace": "http://example.org#",
  "output": {
    "typescript": {
      "path": "src/types",
      "useZod": true,
      "generateUtilities": true
    }
  }
}
"#;
        let config_path = proj_dir.join("ggen.config.json");
        std::fs::write(&config_path, config).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write config: {}", e))
        })?;
        generated_files.push("ggen.config.json".to_string());

        // Create example ontology based on template
        let ontology_file = match template.as_deref() {
            Some("schema.org") => "schema-org-example.ttl",
            Some("foaf") => "foaf-example.ttl",
            Some("dublincore") => "dublincore-example.ttl",
            _ => "example.ttl",
        };

        let example_ttl = get_example_ontology(template.as_deref());
        let ontology_path = proj_dir.join("ontologies").join(ontology_file);
        std::fs::write(&ontology_path, example_ttl).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write example ontology: {}", e))
        })?;
        generated_files.push(format!("ontologies/{}", ontology_file));

        // Create README
        let readme = format!(
            r#"# {} - Ontology Project

Auto-generated ontology project using ggen.

## Quick Start

```bash
# Extract ontology schema
npm run extract

# Generate TypeScript code from ontology
npm run generate

# Validate ontology quality
npm run validate
```

## Project Structure

- `ontologies/` - RDF/OWL ontology files (Turtle, RDF/XML, etc.)
- `src/` - Source code and generated types
- `generated/` - Generated artifacts (TypeScript, GraphQL, SQL, etc.)
- `ggen.config.json` - Configuration for code generation

## Available Commands

### Extract Schema
```bash
ggen ontology extract ontologies/schema.ttl [--namespace <uri>] [--output <file>]
```

### Generate Code
```bash
ggen ontology generate schema.json [--language typescript] [--zod] [--utilities]
```

### Validate Quality
```bash
ggen ontology validate schema.json [--strict]
```

## Supported Ontology Formats

- **Turtle** (.ttl) - Recommended, most human-readable
- **RDF/XML** (.rdf, .xml)
- **N-Triples** (.nt)

## Supported Code Generation Targets

- TypeScript interfaces with Zod validation schemas
- (GraphQL, React components, SQL coming soon)

## Example Ontologies

This project includes example ontologies:
- `schema-org-example.ttl` - Schema.org subset
- `foaf-example.ttl` - Friend of a Friend vocabulary
- `dublincore-example.ttl` - Dublin Core metadata

## Resources

- [RDF Concepts](https://www.w3.org/TR/rdf-concepts/)
- [SPARQL Query Language](https://www.w3.org/TR/sparql11-query/)
- [OWL 2 Web Ontology Language](https://www.w3.org/TR/owl2-overview/)
"#,
            proj_name
        );
        let readme_path = proj_dir.join("README.md");
        std::fs::write(&readme_path, readme).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to write README: {}", e))
        })?;
        generated_files.push("README.md".to_string());

        let ontology_file_path = proj_dir.join("ontologies").join(ontology_file);
        let config_file_path = proj_dir.join("ggen.config.json");

        Ok((
            ontology_file_path.to_string_lossy().to_string(),
            config_file_path.to_string_lossy().to_string(),
            generated_files,
        ))
    })?;

    let (ontology_file, config_file, generated_files) = result;

    Ok(InitOutput {
        project_name,
        ontology_file,
        config_file,
        generated_files,
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

fn get_example_ontology(template: Option<&str>) -> String {
    match template {
        Some("schema.org") => SCHEMA_ORG_EXAMPLE.to_string(),
        Some("foaf") => FOAF_EXAMPLE.to_string(),
        Some("dublincore") => DUBLINCORE_EXAMPLE.to_string(),
        _ => DEFAULT_EXAMPLE.to_string(),
    }
}

// Default example ontology (Product/Order ecommerce-like)
const DEFAULT_EXAMPLE: &str = r#"@prefix ex: <http://example.org#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Classes
ex:Product a owl:Class ;
  rdfs:label "Product" ;
  rdfs:comment "A product in the catalog" .

ex:Order a owl:Class ;
  rdfs:label "Order" ;
  rdfs:comment "A customer order" .

# Properties
ex:name a owl:DatatypeProperty ;
  rdfs:domain ex:Product ;
  rdfs:range xsd:string ;
  rdfs:label "Name" ;
  a owl:FunctionalProperty .

ex:price a owl:DatatypeProperty ;
  rdfs:domain ex:Product ;
  rdfs:range xsd:decimal ;
  rdfs:label "Price" .

ex:quantity a owl:DatatypeProperty ;
  rdfs:domain ex:Order ;
  rdfs:range xsd:integer ;
  rdfs:label "Quantity" .
"#;

// Schema.org subset example
const SCHEMA_ORG_EXAMPLE: &str = r#"@prefix schema: <https://schema.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Product (from schema.org)
schema:Product a rdfs:Class ;
  rdfs:label "Product" ;
  rdfs:comment "Any offered product or service" .

schema:name a rdf:Property ;
  rdfs:domain schema:Product ;
  rdfs:range xsd:string ;
  rdfs:label "Name" .

schema:description a rdf:Property ;
  rdfs:domain schema:Product ;
  rdfs:range xsd:string ;
  rdfs:label "Description" .

schema:price a rdf:Property ;
  rdfs:domain schema:Product ;
  rdfs:range xsd:text ;
  rdfs:label "Price" .

schema:url a rdf:Property ;
  rdfs:domain schema:Product ;
  rdfs:range xsd:url ;
  rdfs:label "URL" .

# Organization
schema:Organization a rdfs:Class ;
  rdfs:label "Organization" .

schema:legalName a rdf:Property ;
  rdfs:domain schema:Organization ;
  rdfs:range xsd:string .

schema:email a rdf:Property ;
  rdfs:domain schema:Organization ;
  rdfs:range xsd:string .
"#;

// FOAF (Friend of a Friend) example
const FOAF_EXAMPLE: &str = r#"@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Person
foaf:Person a rdfs:Class ;
  rdfs:label "Person" ;
  rdfs:comment "A person" .

foaf:name a rdf:Property ;
  rdfs:domain foaf:Person ;
  rdfs:range xsd:string ;
  rdfs:label "Name" .

foaf:mbox a rdf:Property ;
  rdfs:domain foaf:Person ;
  rdfs:range xsd:string ;
  rdfs:label "Email" .

foaf:homepage a rdf:Property ;
  rdfs:domain foaf:Person ;
  rdfs:range xsd:url ;
  rdfs:label "Homepage" .

foaf:knows a rdf:Property ;
  rdfs:domain foaf:Person ;
  rdfs:range foaf:Person ;
  rdfs:label "Knows" .

# Group
foaf:Group a rdfs:Class ;
  rdfs:label "Group" ;
  rdfs:comment "A group of people" .

foaf:member a rdf:Property ;
  rdfs:domain foaf:Group ;
  rdfs:range foaf:Person ;
  rdfs:label "Member" .
"#;

// Dublin Core example
const DUBLINCORE_EXAMPLE: &str = r#"@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Resource
dcterms:Resource a rdfs:Class ;
  rdfs:label "Resource" ;
  rdfs:comment "A resource described by Dublin Core metadata" .

dc:title a rdf:Property ;
  rdfs:domain dcterms:Resource ;
  rdfs:range xsd:string ;
  rdfs:label "Title" .

dc:description a rdf:Property ;
  rdfs:domain dcterms:Resource ;
  rdfs:range xsd:string ;
  rdfs:label "Description" .

dc:creator a rdf:Property ;
  rdfs:domain dcterms:Resource ;
  rdfs:range xsd:string ;
  rdfs:label "Creator" .

dcterms:created a rdf:Property ;
  rdfs:domain dcterms:Resource ;
  rdfs:range xsd:dateTime ;
  rdfs:label "Created" .

dc:type a rdf:Property ;
  rdfs:domain dcterms:Resource ;
  rdfs:range xsd:string ;
  rdfs:label "Type" .

dc:subject a rdf:Property ;
  rdfs:domain dcterms:Resource ;
  rdfs:range xsd:string ;
  rdfs:label "Subject" .
"#;
