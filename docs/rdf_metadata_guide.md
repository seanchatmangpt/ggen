# RDF Metadata Management Guide

## Overview

Ggen provides comprehensive RDF metadata management for templates using **Oxigraph**, a graph database that implements the SPARQL standard. This enables:

- **Machine-readable metadata** - Templates are described in RDF triples
- **SPARQL querying** - Powerful queries for template discovery
- **SHACL validation** - Ensure metadata quality and consistency
- **Relationship tracking** - Dependencies, extensions, overrides
- **Export with projects** - Metadata travels with generated code

## Quick Start

```rust
use ggen_core::rdf::{TemplateMetadata, TemplateMetadataStore};

// Create metadata store
let store = TemplateMetadataStore::new()?;
store.load_schema()?;

// Create template metadata
let mut metadata = TemplateMetadata::new(
    "http://ggen.dev/templates/rust-web".to_string(),
    "Rust Web Service".to_string(),
);

metadata.version = Some("1.0.0".to_string());
metadata.category = Some("web".to_string());
metadata.tags = vec!["rust".to_string(), "web".to_string()];

// Store in graph
store.store_metadata(&metadata)?;

// Query templates
let web_templates = store.find_by_category("web")?;
```

## Architecture

### Components

1. **Schema (`schema.ttl`)** - RDF ontology defining classes and properties
2. **TemplateMetadata** - Rust struct for template metadata
3. **TemplateMetadataStore** - Oxigraph wrapper with SPARQL queries
4. **Validator** - SHACL-based validation rules

### RDF Schema

The Ggen ontology defines:

**Core Classes:**
- `ggen:Template` - A code generation template
- `ggen:File` - A generated file artifact
- `ggen:Variable` - A template variable
- `ggen:Directory` - A generated directory structure

**Properties:**
- `ggen:generatesFile` - Links template to generated files
- `ggen:hasVariable` - Associates variables with templates
- `ggen:dependsOn` - Template dependency relationships
- `ggen:templateName` - Template identifier
- `ggen:category` - Template category for organization

See [schema.ttl](../ggen-core/src/rdf/schema.ttl) for complete ontology.

## Template Metadata

### Creating Metadata

```rust
use ggen_core::rdf::{TemplateMetadata, TemplateVariable};
use chrono::Utc;

let mut metadata = TemplateMetadata::new(
    "http://ggen.dev/templates/my-template".to_string(),
    "My Template".to_string(),
);

// Set metadata fields
metadata.version = Some("2.1.0".to_string());
metadata.description = Some("A production-ready template".to_string());
metadata.author = Some("Your Name".to_string());
metadata.category = Some("web".to_string());
metadata.tags = vec!["rust".to_string(), "api".to_string()];
metadata.stability = Some("stable".to_string());
metadata.test_coverage = Some(92.5);

// Add variables
metadata.variables.push(TemplateVariable {
    name: "service_name".to_string(),
    var_type: "string".to_string(),
    default_value: Some("my-service".to_string()),
    description: Some("Name of the service".to_string()),
    required: true,
});

// Specify generated artifacts
metadata.generated_files = vec![
    "src/main.rs".to_string(),
    "Cargo.toml".to_string(),
];

metadata.generated_directories = vec![
    "src".to_string(),
    "tests".to_string(),
];
```

### Validation

```rust
use ggen_core::rdf::Validator;

let validator = Validator::new();
let report = validator.validate(&metadata)?;

if report.is_valid() {
    println!("✓ Validation passed");
} else {
    println!("✗ Errors:");
    for error in &report.errors {
        println!("  - {}: {}", error.path, error.message);
    }
}

// Check warnings and suggestions
for warning in &report.warnings {
    println!("⚠ {}: {}", warning.path, warning.message);
}
```

### RDF Serialization

```rust
// Convert to Turtle RDF
let turtle = metadata.to_turtle()?;
println!("{}", turtle);

// Parse from Turtle
let parsed = TemplateMetadata::from_turtle(
    &turtle,
    "http://ggen.dev/templates/my-template"
)?;
```

## Metadata Store Operations

### Initialization

```rust
use ggen_core::rdf::TemplateMetadataStore;

// In-memory store
let store = TemplateMetadataStore::new()?;

// Persistent store
let store = TemplateMetadataStore::open("path/to/store")?;

// Load Ggen schema
store.load_schema()?;
```

### Storing Metadata

```rust
// Store template metadata
store.store_metadata(&metadata)?;

// Retrieve by ID
let retrieved = store.get_metadata("http://ggen.dev/templates/my-template")?;

if let Some(meta) = retrieved {
    println!("Found: {}", meta.name);
}
```

### Querying Templates

#### By Category

```rust
let web_templates = store.find_by_category("web")?;
println!("Found {} web templates", web_templates.len());

for template_id in web_templates {
    if let Some(meta) = store.get_metadata(&template_id)? {
        println!("  - {} (v{})", meta.name, meta.version.unwrap_or_default());
    }
}
```

#### By Tag

```rust
let rust_templates = store.find_by_tag("rust")?;
```

#### Custom SPARQL Queries

```rust
let query = r#"
    PREFIX ggen: <http://ggen.dev/ontology#>
    SELECT ?template ?name ?version ?stability
    WHERE {
        ?template a ggen:Template ;
            ggen:templateName ?name ;
            ggen:stability "stable" .
        OPTIONAL { ?template ggen:templateVersion ?version }
    }
    ORDER BY ?name
"#;

let results = store.query(query)?;

for row in results {
    let name = row.get("name").map(|s| s.trim_matches('"')).unwrap_or("?");
    let version = row.get("version").map(|s| s.trim_matches('"')).unwrap_or("N/A");
    println!("{}: v{}", name, version);
}
```

### Template Relationships

```rust
// Get dependencies
let deps = store.get_dependencies("http://ggen.dev/templates/my-template")?;

for dep_id in deps {
    println!("Depends on: {}", dep_id);
}
```

### Export & Import

```rust
// Export all metadata as Turtle
let turtle = store.export_turtle()?;
std::fs::write("templates.ttl", turtle)?;

// Clear store
store.clear()?;
```

## SPARQL Query Examples

### Find Templates by Test Coverage

```sparql
PREFIX ggen: <http://ggen.dev/ontology#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?template ?name ?coverage
WHERE {
    ?template a ggen:Template ;
        ggen:templateName ?name ;
        ggen:testCoverage ?coverage .
    FILTER (?coverage > 80.0)
}
ORDER BY DESC(?coverage)
```

### Find Most Used Templates

```sparql
PREFIX ggen: <http://ggen.dev/ontology#>

SELECT ?template ?name ?usage
WHERE {
    ?template a ggen:Template ;
        ggen:templateName ?name ;
        ggen:usageCount ?usage .
}
ORDER BY DESC(?usage)
LIMIT 10
```

### Find Templates with Required Variables

```sparql
PREFIX ggen: <http://ggen.dev/ontology#>

SELECT ?template ?templateName ?varName
WHERE {
    ?template a ggen:Template ;
        ggen:templateName ?templateName ;
        ggen:hasVariable ?var .
    ?var ggen:variableName ?varName ;
         ggen:isRequired "true"^^xsd:boolean .
}
```

### Template Dependency Graph

```sparql
PREFIX ggen: <http://ggen.dev/ontology#>

SELECT ?template ?name ?dependency ?depName
WHERE {
    ?template a ggen:Template ;
        ggen:templateName ?name ;
        ggen:dependsOn ?dependency .
    ?dependency ggen:templateName ?depName .
}
```

## Integration with Template Engine

### Extract Metadata from Frontmatter

```rust
use ggen_core::Template;

let template_source = r#"---
to: src/main.rs
vars:
  service_name: my-service
  port: 3000
metadata:
  version: "1.0.0"
  category: "web"
  tags: ["rust", "web"]
---
fn main() {
    println!("{{service_name}}");
}
"#;

let template = Template::parse(template_source)?;

// Extract metadata
let mut metadata = TemplateMetadata::new(
    template_id,
    template_name,
);

// Populate from frontmatter vars
for (key, value) in &template.frontmatter.vars {
    metadata.variables.push(TemplateVariable {
        name: key.clone(),
        var_type: infer_type(value),
        default_value: Some(value.to_string()),
        description: None,
        required: false,
    });
}

store.store_metadata(&metadata)?;
```

### Query During Generation

```rust
// Find templates by category
let templates = store.find_by_category("web")?;

// Check dependencies
let deps = store.get_dependencies(&template_id)?;

// Validate before generation
let report = validator.validate(&metadata)?;
if !report.is_valid() {
    bail!("Template validation failed");
}
```

### Export with Generated Code

```rust
// After code generation, export metadata
let turtle = store.export_turtle()?;

// Save alongside generated code
std::fs::write("output/.ggen/metadata.ttl", turtle)?;
```

## Validation Rules

The validator enforces:

1. **Required Fields**:
   - Template name must be present and non-empty
   - Variable names must be valid identifiers

2. **Format Constraints**:
   - Version should follow semantic versioning (x.y.z)
   - Stability must be: experimental, stable, or deprecated
   - Variable types: string, number, boolean, array, object

3. **Value Ranges**:
   - Test coverage: 0-100%
   - Usage count: non-negative integer

4. **Best Practices** (warnings/info):
   - Templates should have descriptions
   - Templates should have categories
   - Required variables should have descriptions

## Best Practices

1. **Use Semantic Versioning**: Follow x.y.z format for versions
2. **Add Descriptions**: Help users understand template purpose
3. **Categorize Templates**: Use consistent category names
4. **Tag Thoroughly**: Enable better discovery
5. **Document Variables**: Describe required variables clearly
6. **Track Dependencies**: Record template relationships
7. **Validate Before Storing**: Catch issues early
8. **Export Metadata**: Save with generated projects

## Performance

- **Caching**: Metadata is cached after first retrieval
- **SPARQL**: Uses Oxigraph's optimized query engine
- **Storage**: Choose in-memory for speed, persistent for durability
- **Batch Operations**: Store multiple templates before querying

## Examples

See:
- [examples/rdf_metadata_example.rs](../examples/rdf_metadata_example.rs) - Basic usage
- [examples/rdf_template_integration.rs](../examples/rdf_template_integration.rs) - Template engine integration

## API Reference

### TemplateMetadata

```rust
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
```

### TemplateMetadataStore

```rust
impl TemplateMetadataStore {
    pub fn new() -> Result<Self>
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self>
    pub fn load_schema(&self) -> Result<()>
    pub fn store_metadata(&self, metadata: &TemplateMetadata) -> Result<()>
    pub fn get_metadata(&self, template_id: &str) -> Result<Option<TemplateMetadata>>
    pub fn query(&self, sparql: &str) -> Result<Vec<BTreeMap<String, String>>>
    pub fn find_by_category(&self, category: &str) -> Result<Vec<String>>
    pub fn find_by_tag(&self, tag: &str) -> Result<Vec<String>>
    pub fn get_dependencies(&self, template_id: &str) -> Result<Vec<String>>
    pub fn export_turtle(&self) -> Result<String>
    pub fn clear(&self) -> Result<()>
}
```

### Validator

```rust
impl Validator {
    pub fn new() -> Self
    pub fn validate(&self, metadata: &TemplateMetadata) -> Result<ValidationReport>
    pub fn validate_turtle(&self, turtle: &str, template_id: &str) -> Result<ValidationReport>
}
```

## Troubleshooting

### Store Initialization Fails

```rust
// Use in-memory store
let store = TemplateMetadataStore::new()?;

// Or check path permissions
let path = PathBuf::from("metadata_store");
std::fs::create_dir_all(&path)?;
let store = TemplateMetadataStore::open(path)?;
```

### SPARQL Query Returns Empty

- Verify schema is loaded: `store.load_schema()?`
- Check template IDs match exactly
- Use `store.export_turtle()` to inspect stored data
- Validate SPARQL syntax with PREFIX declarations

### Validation Errors

```rust
let report = validator.validate(&metadata)?;

// Fix errors
for error in &report.errors {
    match error.path.as_str() {
        "templateName" => metadata.name = "valid-name".to_string(),
        "stability" => metadata.stability = Some("stable".to_string()),
        _ => {}
    }
}
```

## Future Enhancements

- RDFS inference for template hierarchies
- OWL reasoning for complex relationships
- Named graphs for versioning
- SPARQL Update for metadata modifications
- Template recommendation engine
- Automated documentation generation
- GraphQL interface for queries
