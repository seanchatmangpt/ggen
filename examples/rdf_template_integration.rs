//! Example: Integrating RDF metadata with template engine
//!
//! This example shows how to:
//! - Extract metadata from template frontmatter
//! - Store in RDF graph
//! - Query during generation
//! - Export with generated projects

use anyhow::Result;
use ggen_core::{
    rdf::{TemplateMetadata, TemplateMetadataStore, TemplateVariable},
    Template,
};
use std::collections::BTreeMap;

fn main() -> Result<()> {
    println!("=== Template Engine + RDF Integration ===\n");

    // 1. Template with metadata in frontmatter
    let template_source = r#"---
to: src/{{service_name}}/main.rs
vars:
  service_name: my-service
  port: 3000
  enable_telemetry: true
metadata:
  version: "1.0.0"
  description: "Rust web service with Axum"
  author: "Ggen Team"
  category: "web"
  tags:
    - rust
    - web
    - axum
  stability: stable
---
// {{service_name}} - Production Web Service
use axum::{Router, routing::get};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let app = Router::new()
        .route("/", get(|| async { "Hello, World!" }));

    let addr = "0.0.0.0:{{port}}";
    println!("Listening on {}", addr);

    axum::Server::bind(&addr.parse()?)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}
"#;

    println!("1. Template Source:");
    println!("{}", template_source);
    println!();

    // 2. Parse template
    println!("2. Parsing template...");
    let template = Template::parse(template_source)?;
    println!("   ✓ Template parsed successfully");
    println!("   - Output file: {:?}", template.frontmatter.to);
    println!(
        "   - Variables: {:?}",
        template.frontmatter.vars.keys().collect::<Vec<_>>()
    );
    println!();

    // 3. Extract metadata from frontmatter
    println!("3. Extracting RDF metadata from frontmatter...");
    let template_id = "http://ggen.dev/templates/rust-axum-service".to_string();

    let mut metadata = TemplateMetadata::new(template_id.clone(), "Rust Axum Service".to_string());

    // Extract from template frontmatter vars
    if let Some(service_name) = template.frontmatter.vars.get("service_name") {
        metadata.variables.push(TemplateVariable {
            name: "service_name".to_string(),
            var_type: "string".to_string(),
            default_value: Some(service_name.as_str().unwrap_or("").to_string()),
            description: Some("Name of the service".to_string()),
            required: true,
        });
    }

    if let Some(port) = template.frontmatter.vars.get("port") {
        metadata.variables.push(TemplateVariable {
            name: "port".to_string(),
            var_type: "number".to_string(),
            default_value: Some(port.as_i64().unwrap_or(3000).to_string()),
            description: Some("HTTP port".to_string()),
            required: false,
        });
    }

    metadata.version = Some("1.0.0".to_string());
    metadata.description = Some("Rust web service with Axum".to_string());
    metadata.category = Some("web".to_string());
    metadata.tags = vec!["rust".to_string(), "web".to_string(), "axum".to_string()];
    metadata.stability = Some("stable".to_string());
    metadata.generated_files = vec![template.frontmatter.to.clone().unwrap_or_default()];

    println!("   ✓ Extracted metadata:");
    println!("     - Version: {}", metadata.version.as_ref().unwrap());
    println!("     - Category: {}", metadata.category.as_ref().unwrap());
    println!("     - Variables: {}", metadata.variables.len());
    println!();

    // 4. Store in RDF graph
    println!("4. Storing in RDF metadata store...");
    let store = TemplateMetadataStore::new()?;
    store.load_schema()?;
    store.store_metadata(&metadata)?;
    println!("   ✓ Metadata stored in Oxigraph");
    println!();

    // 5. Query during generation
    println!("5. Querying metadata for generation...");

    let query = format!(
        r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?varName ?varType ?required ?default
        WHERE {{
            <{}> ggen:hasVariable ?var .
            ?var ggen:variableName ?varName ;
                 ggen:variableType ?varType ;
                 ggen:isRequired ?required .
            OPTIONAL {{ ?var ggen:variableDefault ?default }}
        }}
        ORDER BY ?varName
        "#,
        template_id
    );

    let results = store.query(&query)?;
    println!("   Found {} template variables:", results.len());
    for row in &results {
        let name = row
            .get("varName")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("?");
        let var_type = row
            .get("varType")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("?");
        let required = row
            .get("required")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("false");
        let default = row
            .get("default")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("N/A");
        println!(
            "     - {}: {} (required: {}, default: {})",
            name, var_type, required, default
        );
    }
    println!();

    // 6. Generate Turtle for export
    println!("6. Generating Turtle RDF for export...");
    let turtle = metadata.to_turtle()?;
    println!("   Generated {} bytes of Turtle RDF", turtle.len());
    println!("   Preview:");
    for line in turtle.lines().take(20) {
        println!("     {}", line);
    }
    println!();

    // 7. Query for template discovery
    println!("7. Template discovery via SPARQL...");

    let discovery_query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?template ?name ?description ?category
        WHERE {
            ?template a ggen:Template ;
                ggen:templateName ?name ;
                ggen:category ?category .
            OPTIONAL { ?template ggen:templateDescription ?description }
        }
    "#;

    let results = store.query(discovery_query)?;
    println!("   Discovered {} template(s):", results.len());
    for row in &results {
        let name = row
            .get("name")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("Unknown");
        let category = row
            .get("category")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("N/A");
        let desc = row
            .get("description")
            .map(|s| s.trim_matches('"'))
            .unwrap_or("No description");
        println!("     - {}", name);
        println!("       Category: {}", category);
        println!("       Description: {}", desc);
    }
    println!();

    // 8. Export complete metadata
    println!("8. Exporting complete project metadata...");
    let exported = store.export_turtle()?;
    println!("   ✓ Exported {} bytes of RDF", exported.len());
    println!("   ✓ Can be saved alongside generated code");
    println!("   ✓ Enables project introspection and documentation");
    println!();

    println!("=== Integration Complete ===");
    println!("\nKey Benefits:");
    println!("  • Template metadata is machine-readable (RDF)");
    println!("  • Can query templates using SPARQL");
    println!("  • Metadata travels with generated code");
    println!("  • Enables automated documentation");
    println!("  • Supports template discovery and recommendations");

    Ok(())
}
