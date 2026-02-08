//! Ontology domain logic
//!
//! This module provides domain operations for working with ontologies,
//! separating business logic from CLI concerns.

use ggen_core::ontology::OntologySchema;
use ggen_utils::error::Error;
use std::path::PathBuf;

/// Extract schema from ontology file
pub async fn extract_ontology_schema(
    ontology_file: &PathBuf,
    namespace: &str,
) -> Result<OntologySchema, Error> {
    use ggen_core::Graph;

    let graph = Graph::new().map_err(|e| {
        Error::new(&format!("Failed to create graph: {}", e))
    })?;

    let file_content = std::fs::read_to_string(ontology_file)
        .map_err(|e| Error::new(&format!("Failed to read file: {}", e)))?;

    graph.insert_turtle(&file_content).map_err(|e| {
        Error::new(&format!("Failed to load ontology: {}", e))
    })?;

    let schema = ggen_core::OntologyExtractor::extract(&graph, namespace)
        .map_err(|e| Error::new(&format!("Extraction failed: {}", e)))?;

    Ok(schema)
}

/// Generate code from ontology schema
pub async fn generate_code_from_ontology(
    schema: &OntologySchema,
    language: &str,
    output_dir: &PathBuf,
    zod: bool,
    utilities: bool,
) -> Result<(usize, String), Error> {
    use ggen_core::codegen::TypeScriptGenerator;

    std::fs::create_dir_all(output_dir).map_err(|e| {
        Error::new(&format!("Failed to create output directory: {}", e))
    })?;

    let mut files_generated = 0;
    let mut primary_file = String::new();

    if language == "typescript" {
        // Generate interfaces
        let interfaces = TypeScriptGenerator::generate_interfaces(schema).map_err(|e| {
            Error::new(&format!("Interface generation failed: {}", e))
        })?;

        let interfaces_path = output_dir.join("types.ts");
        std::fs::write(&interfaces_path, interfaces).map_err(|e| {
            Error::new(&format!("Failed to write types: {}", e))
        })?;
        files_generated += 1;
        primary_file = interfaces_path.to_string_lossy().to_string();

        // Generate Zod schemas if requested
        if zod {
            let zod_schemas = TypeScriptGenerator::generate_zod_schemas(schema).map_err(|e| {
                Error::new(&format!("Zod generation failed: {}", e))
            })?;

            let zod_path = output_dir.join("schemas.ts");
            std::fs::write(&zod_path, zod_schemas).map_err(|e| {
                Error::new(&format!("Failed to write schemas: {}", e))
            })?;
            files_generated += 1;
        }

        // Generate utility types if requested
        if utilities {
            let utils = TypeScriptGenerator::generate_utility_types(schema).map_err(|e| {
                Error::new(&format!("Utilities generation failed: {}", e))
            })?;

            let utils_path = output_dir.join("utilities.ts");
            std::fs::write(&utils_path, utils).map_err(|e| {
                Error::new(&format!("Failed to write utilities: {}", e))
            })?;
            files_generated += 1;
        }
    }

    Ok((files_generated, primary_file))
}

/// Validate ontology schema quality
pub async fn validate_ontology_schema(
    schema: &OntologySchema,
    strict: bool,
) -> Result<(bool, Vec<String>, Vec<String>), Error> {
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

    Ok((errors.is_empty(), warnings, errors))
}

/// Initialize ontology project structure
pub async fn initialize_ontology_project(
    project_name: &str,
    template: Option<&str>,
) -> Result<(String, String, Vec<String>), Error> {
    let proj_dir = PathBuf::from(project_name);

    // Create project directory
    std::fs::create_dir_all(&proj_dir).map_err(|e| {
        Error::new(&format!("Failed to create project: {}", e))
    })?;

    // Create subdirectories
    std::fs::create_dir_all(proj_dir.join("ontologies")).map_err(|e| {
        Error::new(&format!("Failed to create ontologies dir: {}", e))
    })?;
    std::fs::create_dir_all(proj_dir.join("src")).map_err(|e| {
        Error::new(&format!("Failed to create src dir: {}", e))
    })?;
    std::fs::create_dir_all(proj_dir.join("generated")).map_err(|e| {
        Error::new(&format!("Failed to create generated dir: {}", e))
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
        project_name
    );
    let pkg_path = proj_dir.join("package.json");
    std::fs::write(&pkg_path, package_json).map_err(|e| {
        Error::new(&format!("Failed to write package.json: {}", e))
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
        Error::new(&format!("Failed to write config: {}", e))
    })?;
    generated_files.push("ggen.config.json".to_string());

    // Create example ontology based on template
    let ontology_file = match template {
        Some("schema.org") => "schema-org-example.ttl",
        Some("foaf") => "foaf-example.ttl",
        Some("dublincore") => "dublincore-example.ttl",
        _ => "example.ttl",
    };

    let example_ttl = get_example_ontology(template);
    let ontology_path = proj_dir.join("ontologies").join(ontology_file);
    std::fs::write(&ontology_path, example_ttl).map_err(|e| {
        Error::new(&format!("Failed to write example ontology: {}", e))
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
        project_name
    );
    let readme_path = proj_dir.join("README.md");
    std::fs::write(&readme_path, readme).map_err(|e| {
        Error::new(&format!("Failed to write README: {}", e))
    })?;
    generated_files.push("README.md".to_string());

    let ontology_file_path = proj_dir.join("ontologies").join(ontology_file);
    let config_file_path = proj_dir.join("ggen.config.json");

    Ok((
        ontology_file_path.to_string_lossy().to_string(),
        config_file_path.to_string_lossy().to_string(),
        generated_files,
    ))
}

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