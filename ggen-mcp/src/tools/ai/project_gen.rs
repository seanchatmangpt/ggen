//! Project scaffolding and code generation helpers
//!
//! This module provides functions to generate complete project structures
//! for various languages and frameworks.

use serde_json::{json, Value};
use crate::error::{Result, get_string_param, get_optional_string_param};
use super::client::get_ai_client;

/// Generate complete project structure from description
pub async fn generate_project(params: Value) -> Result<Value> {
    let description = get_string_param(&params, "description")?;
    let name = get_string_param(&params, "name")?;
    let provider = get_optional_string_param(&params, "provider").unwrap_or_else(|| "ollama".to_string());
    let language = get_optional_string_param(&params, "language").unwrap_or_else(|| "rust".to_string());
    let framework = get_optional_string_param(&params, "framework");
    let output_dir = get_optional_string_param(&params, "output_dir");

    tracing::info!("AI project generation: {} -> {}", description, name);

    let _client = get_ai_client(&provider).await?;

    let mut project_files = std::collections::HashMap::new();

    // Generate project structure based on language and framework
    match language.as_str() {
        "rust" => {
            if framework.as_deref() == Some("axum") {
                let cargo_toml = generate_cargo_toml(&name, &["axum", "tokio", "serde"])?;
                let main_rs = generate_axum_main(&name)?;
                let lib_rs = generate_axum_lib(&name)?;

                project_files.insert("Cargo.toml".to_string(), cargo_toml);
                project_files.insert("src/main.rs".to_string(), main_rs);
                project_files.insert("src/lib.rs".to_string(), lib_rs);
            } else {
                let cargo_toml = generate_cargo_toml(&name, &["tokio", "serde"])?;
                let main_rs = generate_basic_main(&name)?;

                project_files.insert("Cargo.toml".to_string(), cargo_toml);
                project_files.insert("src/main.rs".to_string(), main_rs);
            }
        },
        "python" => {
            let pyproject_toml = generate_pyproject_toml(&name, &["fastapi", "uvicorn"])?;
            let main_py = generate_fastapi_main(&name)?;

            project_files.insert("pyproject.toml".to_string(), pyproject_toml);
            project_files.insert("main.py".to_string(), main_py);
        },
        _ => {
            return Err(crate::error::GgenMcpError::Configuration(
                format!("Unsupported language: {}", language)
            ));
        }
    }

    // Create output directory if specified
    if let Some(dir) = &output_dir {
        std::fs::create_dir_all(dir)
            .map_err(crate::error::GgenMcpError::Io)?;

        // Write all files
        for (file_path, content) in &project_files {
            let full_path = std::path::Path::new(dir).join(file_path);
            if let Some(parent) = full_path.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(crate::error::GgenMcpError::Io)?;
            }
            std::fs::write(&full_path, content)
                .map_err(crate::error::GgenMcpError::Io)?;
        }
    }

    Ok(crate::error::success_response(json!({
        "project_name": name,
        "language": language,
        "framework": framework,
        "files_generated": project_files.len(),
        "output_directory": output_dir,
        "files": project_files.keys().cloned().collect::<Vec<_>>()
    })))
}

// Rust project helpers

fn generate_cargo_toml(name: &str, dependencies: &[&str]) -> Result<String> {
    Ok(format!(r#"[package]
name = "{}"
version = "0.1.0"
edition = "2021"

[dependencies]
{}
"#,
    name,
    dependencies.iter().map(|dep| format!("{} = \"1.0\"", dep)).collect::<Vec<_>>().join("\n")
    ))
}

fn generate_axum_main(name: &str) -> Result<String> {
    Ok(format!(r#"use axum::{{
    extract::Path,
    http::StatusCode,
    response::IntoResponse,
    routing::get,
    Router,
}};
use std::net::SocketAddr;

#[tokio::main]
async fn main() {{
    // build our application with a single route
    let app = Router::new()
        .route("/", get(root_handler))
        .route("/health", get(health_check));

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    println!("🚀 {{}} is listening on {{:?}}", "{}", listener.local_addr().unwrap());

    axum::serve(listener, app).await.unwrap();
}}

async fn root_handler() -> impl IntoResponse {{
    (StatusCode::OK, "Hello from {}!")
}}

async fn health_check() -> impl IntoResponse {{
    (StatusCode::OK, "OK")
}}
"#, name, name))
}

fn generate_axum_lib(name: &str) -> Result<String> {
    Ok(format!(r#"//! {} library

pub mod models;
pub mod handlers;
pub mod middleware;

// Re-export commonly used types
pub use models::*;
pub use handlers::*;
"#,
    name))
}

fn generate_basic_main(name: &str) -> Result<String> {
    Ok(format!(r#"use tokio;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {{
    println!("Hello, {}!", "{}");

    // Your application logic here

    Ok(())
}}
"#, name, name))
}

// Python project helpers

fn generate_pyproject_toml(name: &str, dependencies: &[&str]) -> Result<String> {
    Ok(format!(r#"[project]
name = "{}"
version = "0.1.0"
description = "Generated Python project"
dependencies = [
{}
]

[build-system]
requires = ["hatchling"]
build-backend = "hatchling.build"
"#,
    name,
    dependencies.iter().map(|dep| format!("    \"{}\"", dep)).collect::<Vec<_>>().join(",\n")
    ))
}

fn generate_fastapi_main(name: &str) -> Result<String> {
    Ok(format!(r#"from fastapi import FastAPI

app = FastAPI(title="{}")

@app.get("/")
async def root():
    return {{"message": "Hello, {}!"}}  # type: ignore

@app.get("/health")
async def health_check():
    return {{"status": "healthy"}}  # type: ignore

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
"#, name, name))
}
