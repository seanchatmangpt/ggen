//! Example: RDF metadata management for templates
//!
//! This example demonstrates:
//! - Creating template metadata
//! - Storing metadata in Oxigraph
//! - Querying metadata with SPARQL
//! - SHACL validation
//! - Template discovery and relationships

use anyhow::Result;
use ggen_core::rdf::{TemplateMetadata, TemplateMetadataStore, TemplateVariable, Validator};

fn main() -> Result<()> {
    println!("=== Ggen RDF Metadata Example ===\n");

    // 1. Create metadata store
    println!("1. Creating metadata store...");
    let store = TemplateMetadataStore::new()?;
    store.load_schema()?;
    println!("   ✓ Store initialized with Ggen schema\n");

    // 2. Create template metadata
    println!("2. Creating template metadata...");
    let mut rust_template = TemplateMetadata::new(
        "http://ggen.dev/templates/rust-web-service".to_string(),
        "Rust Web Service".to_string(),
    );

    rust_template.version = Some("1.0.0".to_string());
    rust_template.description = Some("Production-ready Rust web service with Axum".to_string());
    rust_template.author = Some("Ggen Team".to_string());
    rust_template.category = Some("web".to_string());
    rust_template.tags = vec![
        "rust".to_string(),
        "web".to_string(),
        "axum".to_string(),
        "api".to_string(),
    ];

    // Add variables
    rust_template.variables.push(TemplateVariable {
        name: "service_name".to_string(),
        var_type: "string".to_string(),
        default_value: Some("my-service".to_string()),
        description: Some("Name of the web service".to_string()),
        required: true,
    });

    rust_template.variables.push(TemplateVariable {
        name: "port".to_string(),
        var_type: "number".to_string(),
        default_value: Some("3000".to_string()),
        description: Some("HTTP port to listen on".to_string()),
        required: false,
    });

    rust_template.variables.push(TemplateVariable {
        name: "enable_telemetry".to_string(),
        var_type: "boolean".to_string(),
        default_value: Some("true".to_string()),
        description: Some("Enable OpenTelemetry instrumentation".to_string()),
        required: false,
    });

    // Add generated files
    rust_template.generated_files = vec![
        "src/main.rs".to_string(),
        "src/routes.rs".to_string(),
        "src/handlers.rs".to_string(),
        "Cargo.toml".to_string(),
        "README.md".to_string(),
    ];

    rust_template.generated_directories = vec!["src".to_string(), "tests".to_string()];

    rust_template.stability = Some("stable".to_string());
    rust_template.test_coverage = Some(85.5);
    rust_template.usage_count = Some(42);

    println!("   ✓ Created metadata for: {}", rust_template.name);
    println!("   - Version: {}", rust_template.version.as_ref().unwrap());
    println!("   - Variables: {}", rust_template.variables.len());
    println!(
        "   - Generated files: {}",
        rust_template.generated_files.len()
    );
    println!();

    // 3. Validate metadata
    println!("3. Validating template metadata...");
    let validator = Validator::new();
    let report = validator.validate(&rust_template)?;

    if report.is_valid() {
        println!("   ✓ Validation passed!");
    } else {
        println!("   ✗ Validation failed:");
        for error in &report.errors {
            println!("     - {}: {}", error.path, error.message);
        }
    }

    if !report.warnings.is_empty() {
        println!("   Warnings:");
        for warning in &report.warnings {
            println!("     - {}: {}", warning.path, warning.message);
        }
    }

    if !report.info.is_empty() {
        println!("   Suggestions:");
        for info in &report.info {
            println!("     - {}: {}", info.path, info.message);
        }
    }
    println!();

    // 4. Store metadata
    println!("4. Storing metadata in RDF store...");
    store.store_metadata(&rust_template)?;
    println!("   ✓ Metadata stored\n");

    // 5. Create more templates
    println!("5. Creating additional templates...");
    let mut python_template = TemplateMetadata::new(
        "http://ggen.dev/templates/python-fastapi".to_string(),
        "Python FastAPI Service".to_string(),
    );
    python_template.version = Some("2.1.0".to_string());
    python_template.description = Some("FastAPI REST service with async support".to_string());
    python_template.category = Some("web".to_string());
    python_template.tags = vec![
        "python".to_string(),
        "fastapi".to_string(),
        "api".to_string(),
    ];
    python_template.stability = Some("stable".to_string());

    let mut typescript_template = TemplateMetadata::new(
        "http://ggen.dev/templates/typescript-express".to_string(),
        "TypeScript Express API".to_string(),
    );
    typescript_template.version = Some("1.5.0".to_string());
    typescript_template.description = Some("Express.js API with TypeScript".to_string());
    typescript_template.category = Some("web".to_string());
    typescript_template.tags = vec![
        "typescript".to_string(),
        "nodejs".to_string(),
        "express".to_string(),
    ];
    typescript_template.stability = Some("stable".to_string());

    let mut experimental_template = TemplateMetadata::new(
        "http://ggen.dev/templates/rust-wasm".to_string(),
        "Rust WASM Module".to_string(),
    );
    experimental_template.version = Some("0.1.0".to_string());
    experimental_template.category = Some("wasm".to_string());
    experimental_template.tags = vec!["rust".to_string(), "wasm".to_string()];
    experimental_template.stability = Some("experimental".to_string());

    store.store_metadata(&python_template)?;
    store.store_metadata(&typescript_template)?;
    store.store_metadata(&experimental_template)?;
    println!("   ✓ Stored 3 additional templates\n");

    // 6. Query templates by category
    println!("6. Querying templates by category 'web'...");
    let web_templates = store.find_by_category("web")?;
    println!("   Found {} web templates:", web_templates.len());
    for template_id in &web_templates {
        if let Some(metadata) = store.get_metadata(template_id)? {
            println!(
                "     - {} (v{})",
                metadata.name,
                metadata.version.unwrap_or_default()
            );
        }
    }
    println!();

    // 7. Query templates by tag
    println!("7. Querying templates by tag 'rust'...");
    let rust_templates = store.find_by_tag("rust")?;
    println!("   Found {} Rust templates:", rust_templates.len());
    for template_id in &rust_templates {
        if let Some(metadata) = store.get_metadata(template_id)? {
            println!(
                "     - {} ({})",
                metadata.name,
                metadata.stability.unwrap_or_default()
            );
        }
    }
    println!();

    // 8. Custom SPARQL queries
    println!("8. Running custom SPARQL queries...");

    // Query all stable templates
    let stable_query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?template ?name ?version
        WHERE {
            ?template a ggen:Template ;
                ggen:templateName ?name ;
                ggen:stability "stable" .
            OPTIONAL { ?template ggen:templateVersion ?version }
        }
        ORDER BY ?name
    "#;

    println!("   Query: All stable templates");
    let results = store.query(stable_query)?;
    for row in &results {
        let name = row
            .get("name")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("Unknown");
        let version = row
            .get("version")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("N/A");
        println!("     - {}: v{}", name, version);
    }
    println!();

    // Query templates with test coverage > 80%
    let coverage_query = r#"
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
    "#;

    println!("   Query: Templates with >80% test coverage");
    let results = store.query(coverage_query)?;
    for row in &results {
        let name = row
            .get("name")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("Unknown");
        let coverage = row
            .get("coverage")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("N/A");
        println!("     - {}: {}%", name, coverage);
    }
    println!();

    // 9. Export metadata as Turtle
    println!("9. Exporting metadata as Turtle...");
    let turtle = store.export_turtle()?;
    let lines = turtle.lines().count();
    println!("   ✓ Exported {} lines of RDF Turtle", lines);
    println!("   First few triples:");
    for line in turtle.lines().take(10) {
        if !line.is_empty() {
            println!("     {}", line);
        }
    }
    println!();

    // 10. Demonstrate RDF round-trip
    println!("10. Testing RDF round-trip (to_turtle → from_turtle)...");
    let original_turtle = rust_template.to_turtle()?;
    let parsed = TemplateMetadata::from_turtle(
        &original_turtle,
        "http://ggen.dev/templates/rust-web-service",
    )?;
    println!("   ✓ Successfully parsed metadata from Turtle");
    println!("   - Original name: {}", rust_template.name);
    println!("   - Parsed name: {}", parsed.name);
    println!("   - Match: {}", rust_template.name == parsed.name);
    println!();

    println!("=== Example Complete ===");

    Ok(())
}
