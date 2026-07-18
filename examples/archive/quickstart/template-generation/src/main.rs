//! Template Generation Quickstart Example
//!
//! This example demonstrates generating code from a simple ontology
//! using variable substitution and template rendering.
//!
//! Run: cargo run --bin template_example

use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

/// Simple template definition
#[derive(Debug, Clone, Serialize, Deserialize)]
struct Template {
    name: String,
    description: String,
    variables: Vec<TemplateVariable>,
    files: Vec<TemplateFile>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TemplateVariable {
    name: String,
    description: String,
    default_value: Option<String>,
    required: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TemplateFile {
    path: String,
    content: String,
}

/// Simple template engine
struct TemplateEngine {
    templates: Vec<Template>,
}

impl TemplateEngine {
    fn new() -> Self {
        TemplateEngine {
            templates: Vec::new(),
        }
    }

    /// Load template from string (simulating RDF ontology loading)
    fn load_template(&mut self, template: Template) {
        println!("📖 Loading template: {}", template.name);
        self.templates.push(template);
    }

    /// Generate code from template with variable substitution
    fn generate(
        &self,
        template_name: &str,
        variables: &HashMap<String, String>,
    ) -> Result<Vec<(String, String)>, String> {
        // Find template
        let template = self
            .templates
            .iter()
            .find(|t| t.name == template_name)
            .ok_or_else(|| format!("Template not found: {}", template_name))?;

        println!("\n🔨 Generating from template: {}", template_name);
        println!("📝 Variables:");
        for (key, value) in variables {
            println!("  {} = {}", key, value);
        }
        println!();

        // Generate files with variable substitution
        let mut generated_files = Vec::new();

        for file in &template.files {
            let content = self.substitute_variables(&file.content, variables)?;
            generated_files.push((file.path.clone(), content));
        }

        Ok(generated_files)
    }

    /// Substitute variables in template content
    fn substitute_variables(
        &self,
        content: &str,
        variables: &HashMap<String, String>,
    ) -> Result<String, String> {
        let mut result = content.to_string();

        for (key, value) in variables {
            let placeholder = format!("{{{{{}}}}}", key);
            result = result.replace(&placeholder, value);
        }

        Ok(result)
    }

    /// Write generated files to disk
    fn write_files(&self, files: &[(String, String)], output_dir: &PathBuf) -> Result<(), String> {
        println!("📁 Writing files to: {}\n", output_dir.display());

        for (path, content) in files {
            let full_path = output_dir.join(path);

            // Create parent directories
            if let Some(parent) = full_path.parent() {
                fs::create_dir_all(parent)
                    .map_err(|e| format!("Failed to create directory {}: {}", parent.display(), e))?;
            }

            // Write file
            fs::write(&full_path, content)
                .map_err(|e| format!("Failed to write file {}: {}", full_path.display(), e))?;

            println!("  ✓ {}", path);
        }

        Ok(())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("📦 Template Generation Quickstart Example");
    println!("=========================================\n");

    // Create template engine
    let mut engine = TemplateEngine::new();

    // Define a simple Rust microservice template
    // (In real ggen, this would be loaded from RDF ontology)
    let microservice_template = Template {
        name: "rust-microservice".to_string(),
        description: "Simple Rust microservice with Axum".to_string(),
        variables: vec![
            TemplateVariable {
                name: "service_name".to_string(),
                description: "Name of the service".to_string(),
                default_value: Some("my-service".to_string()),
                required: true,
            },
            TemplateVariable {
                name: "version".to_string(),
                description: "Version of the service".to_string(),
                default_value: Some("0.1.0".to_string()),
                required: true,
            },
            TemplateVariable {
                name: "port".to_string(),
                description: "Port to listen on".to_string(),
                default_value: Some("3000".to_string()),
                required: false,
            },
        ],
        files: vec![
            TemplateFile {
                path: "Cargo.toml".to_string(),
                content: r#"[package]
name = "{{service_name}}"
version = "{{version}}"
edition = "2021"

[dependencies]
axum = "0.7"
tokio = { version = "1", features = ["full"] }
serde = { version = "1", features = ["derive"] }
"#.to_string(),
            },
            TemplateFile {
                path: "src/main.rs".to_string(),
                content: r#"use axum::{
    response::Json,
    routing::get,
    Router,
};
use serde::Serialize;

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", get(index));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:{{port}}")
        .await
        .unwrap();

    println!("🚀 {{service_name}} v{{version}} listening on :{{port}}");

    axum::serve(listener, app).await.unwrap();
}

#[derive(Serialize)]
struct IndexResponse {
    service: String,
    version: String,
    message: String,
}

async fn index() -> Json<IndexResponse> {
    Json(IndexResponse {
        service: "{{service_name}}".to_string(),
        version: "{{version}}".to_string(),
        message: "Hello, World!".to_string(),
    })
}
"#.to_string(),
            },
        ],
    };

    // Load template
    engine.load_template(microservice_template);
    println!("✅ Template loaded\n");

    // Set variable values
    let mut variables = HashMap::new();
    variables.insert("service_name".to_string(), "hello-service".to_string());
    variables.insert("version".to_string(), "1.0.0".to_string());
    variables.insert("port".to_string(), "8080".to_string());

    // Generate files
    let generated_files = engine.generate("rust-microservice", &variables)?;

    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("GENERATED FILES");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

    for (path, content) in &generated_files {
        println!("📄 {}", path);
        println!("{}\n", "─".repeat(50));
        println!("{}", content);
        println!();
    }

    // Write files to temporary directory
    let output_dir = std::env::current_dir()?.join("target").join("quickstart-output");
    engine.write_files(&generated_files, &output_dir)?;

    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
    println!("✅ Example complete!");
    println!("\n💡 Generated files written to:");
    println!("   {}", output_dir.display());
    println!("\n💡 To run the generated service:");
    println!("   cd {}", output_dir.display());
    println!("   cargo run");

    Ok(())
}
