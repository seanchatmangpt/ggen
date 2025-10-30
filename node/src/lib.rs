//! Production-grade Node.js N-API bindings for ggen CLI
//!
//! This module provides high-performance, type-safe bindings to the ggen CLI
//! for use in Node.js applications. All functions use proper error handling
//! without .expect() or .unwrap() to ensure production readiness.

use napi::bindgen_prelude::*;
use napi_derive::napi;

/// Result of a CLI command execution
#[napi(object)]
pub struct RunResult {
    /// Exit code (0 = success, non-zero = error)
    pub code: i32,
    /// Standard output captured from the command
    pub stdout: String,
    /// Standard error captured from the command
    pub stderr: String,
}

/// Get the ggen version
///
/// # Returns
/// Version string matching the Cargo package version
///
/// # Example
/// ```typescript
/// import { version } from '@ggen/node';
/// console.log('ggen version:', version());
/// ```
#[napi]
pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

/// Execute ggen CLI with provided arguments
///
/// This is the low-level API that all other functions use internally.
/// Prefer using the high-level wrapper functions for better ergonomics.
///
/// # Arguments
/// * `args` - Command-line arguments (without the 'ggen' binary name)
///
/// # Returns
/// RunResult containing exit code and captured stdout/stderr
///
/// # Example
/// ```typescript
/// const result = await run(['--version']);
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn run(args: Vec<String>) -> Result<RunResult> {
    let result = ggen_cli_lib::run_for_node(args)
        .await
        .map_err(|e| Error::from_reason(format!("CLI execution failed: {}", e)))?;
    Ok(RunResult {
        code: result.code,
        stdout: result.stdout,
        stderr: result.stderr,
    })
}

// ═══════════════════════════════════════════════════════════════════════════════
// Marketplace Commands
// ═══════════════════════════════════════════════════════════════════════════════

/// Search marketplace packages by query
///
/// # Arguments
/// * `query` - Search terms (e.g., "rust web service")
///
/// # Example
/// ```typescript
/// const result = await marketSearch('rust web');
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn market_search(query: String) -> Result<RunResult> {
    let args = vec!["market".to_string(), "search".to_string(), query];
    run(args).await
}

/// Add a marketplace package to the current project
///
/// # Arguments
/// * `package_id` - Package identifier (e.g., "io.ggen.rust.axum-service")
///
/// # Example
/// ```typescript
/// await marketAdd('io.ggen.rust.axum-service');
/// ```
#[napi]
pub async fn market_add(package_id: String) -> Result<RunResult> {
    let args = vec!["market".to_string(), "add".to_string(), package_id];
    run(args).await
}

/// List all installed marketplace packages
///
/// # Example
/// ```typescript
/// const result = await marketList();
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn market_list() -> Result<RunResult> {
    let args = vec!["market".to_string(), "list".to_string()];
    run(args).await
}

/// List available marketplace categories
///
/// # Example
/// ```typescript
/// const result = await marketCategories();
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn market_categories() -> Result<RunResult> {
    let args = vec!["market".to_string(), "categories".to_string()];
    run(args).await
}

/// Remove a marketplace package from the current project
///
/// # Arguments
/// * `package_id` - Package identifier to remove
///
/// # Example
/// ```typescript
/// await marketRemove('io.ggen.rust.axum-service');
/// ```
#[napi]
pub async fn market_remove(package_id: String) -> Result<RunResult> {
    let args = vec!["market".to_string(), "remove".to_string(), package_id];
    run(args).await
}

// ═══════════════════════════════════════════════════════════════════════════════
// Lifecycle Commands
// ═══════════════════════════════════════════════════════════════════════════════

/// Initialize a new ggen project
///
/// # Example
/// ```typescript
/// await lifecycleInit();
/// ```
#[napi]
pub async fn lifecycle_init() -> Result<RunResult> {
    let args = vec!["lifecycle".to_string(), "run".to_string(), "init".to_string()];
    run(args).await
}

/// Run tests for the current project
///
/// # Example
/// ```typescript
/// const result = await lifecycleTest();
/// if (result.code !== 0) {
///   console.error('Tests failed:', result.stderr);
/// }
/// ```
#[napi]
pub async fn lifecycle_test() -> Result<RunResult> {
    let args = vec!["lifecycle".to_string(), "run".to_string(), "test".to_string()];
    run(args).await
}

/// Build the current project
///
/// # Example
/// ```typescript
/// await lifecycleBuild();
/// ```
#[napi]
pub async fn lifecycle_build() -> Result<RunResult> {
    let args = vec![
        "lifecycle".to_string(),
        "run".to_string(),
        "build".to_string(),
    ];
    run(args).await
}

/// Deploy the project to a specified environment
///
/// # Arguments
/// * `env` - Target environment (e.g., "staging", "production")
///
/// # Example
/// ```typescript
/// await lifecycleDeploy('production');
/// ```
#[napi]
pub async fn lifecycle_deploy(env: Option<String>) -> Result<RunResult> {
    let mut args = vec![
        "lifecycle".to_string(),
        "run".to_string(),
        "deploy".to_string(),
    ];
    if let Some(environment) = env {
        args.push("--env".to_string());
        args.push(environment);
    }
    run(args).await
}

/// Validate deployment readiness for a specified environment
///
/// # Arguments
/// * `env` - Target environment to validate (e.g., "production")
///
/// # Example
/// ```typescript
/// const result = await lifecycleValidate('production');
/// if (result.code === 0) {
///   console.log('Ready for production deployment');
/// }
/// ```
#[napi]
pub async fn lifecycle_validate(env: Option<String>) -> Result<RunResult> {
    let mut args = vec!["lifecycle".to_string(), "validate".to_string()];
    if let Some(environment) = env {
        args.push("--env".to_string());
        args.push(environment);
    }
    run(args).await
}

/// Check production readiness status
///
/// # Example
/// ```typescript
/// const result = await lifecycleReadiness();
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn lifecycle_readiness() -> Result<RunResult> {
    let args = vec!["lifecycle".to_string(), "readiness".to_string()];
    run(args).await
}

/// Update readiness status for a specific requirement
///
/// # Arguments
/// * `requirement_id` - Requirement identifier
/// * `status` - New status value (e.g., "complete", "in-progress")
///
/// # Example
/// ```typescript
/// await lifecycleReadinessUpdate('auth-basic', 'complete');
/// ```
#[napi]
pub async fn lifecycle_readiness_update(requirement_id: String, status: String) -> Result<RunResult> {
    let args = vec![
        "lifecycle".to_string(),
        "readiness-update".to_string(),
        requirement_id,
        status,
    ];
    run(args).await
}

/// List available lifecycle phases
///
/// # Example
/// ```typescript
/// const result = await lifecycleList();
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn lifecycle_list() -> Result<RunResult> {
    let args = vec!["lifecycle".to_string(), "list".to_string()];
    run(args).await
}

// ═══════════════════════════════════════════════════════════════════════════════
// Template Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

/// Generate code from a template
///
/// # Arguments
/// * `template_path` - Path to template file
/// * `vars` - Optional variable map for template rendering (as JSON string)
/// * `manifest_path` - Optional path to ggen.toml manifest
///
/// # Example
/// ```typescript
/// await templateGenerate('service.tmpl', JSON.stringify({ name: 'api' }));
/// ```
#[napi]
pub async fn template_generate(
    template_path: String,
    vars: Option<String>,
    manifest_path: Option<String>,
) -> Result<RunResult> {
    let mut args = vec!["gen".to_string(), template_path];

    // Add variables if provided
    if let Some(vars_str) = vars {
        if let Ok(vars_obj) = serde_json::from_str::<serde_json::Value>(&vars_str) {
            if let Some(obj) = vars_obj.as_object() {
                args.push("--vars".to_string());
                for (key, value) in obj {
                    if let Some(val_str) = value.as_str() {
                        args.push(format!("{}={}", key, val_str));
                    }
                }
            }
        }
    }

    // Add manifest path if provided
    if let Some(manifest) = manifest_path {
        args.push("--manifest-path".to_string());
        args.push(manifest);
    }

    run(args).await
}

/// List available templates
///
/// # Example
/// ```typescript
/// const result = await templateList();
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn template_list() -> Result<RunResult> {
    let args = vec!["list".to_string()];
    run(args).await
}

// ═══════════════════════════════════════════════════════════════════════════════
// AI Generation Commands
// ═══════════════════════════════════════════════════════════════════════════════

/// Generate a complete project using AI from a description
///
/// # Arguments
/// * `description` - Natural language project description
/// * `name` - Optional project name
/// * `language` - Optional target language (rust, typescript, python, go)
///
/// # Example
/// ```typescript
/// await aiProject('REST API with authentication', 'my-api', 'rust');
/// ```
#[napi]
pub async fn ai_project(
    description: String,
    name: Option<String>,
    language: Option<String>,
) -> Result<RunResult> {
    let mut args = vec!["ai".to_string(), "project".to_string(), description];

    if let Some(project_name) = name {
        args.push("--name".to_string());
        args.push(project_name);
    }

    if let Some(lang) = language {
        args.push(format!("--{}", lang));
    }

    run(args).await
}

/// Generate a template file using AI from a description
///
/// # Arguments
/// * `description` - What the template should do
/// * `output_path` - Where to save the generated template
///
/// # Example
/// ```typescript
/// await aiGenerate('Database repository for users', 'user-repo.tmpl');
/// ```
#[napi]
pub async fn ai_generate(description: String, output_path: String) -> Result<RunResult> {
    let args = vec![
        "ai".to_string(),
        "generate".to_string(),
        "-d".to_string(),
        description,
        "-o".to_string(),
        output_path,
    ];
    run(args).await
}

/// Generate an RDF ontology using AI from a description
///
/// # Arguments
/// * `description` - What the ontology should represent
/// * `output_path` - Where to save the generated ontology (.ttl file)
///
/// # Example
/// ```typescript
/// await aiGraph('User management ontology', 'users.ttl');
/// ```
#[napi]
pub async fn ai_graph(description: String, output_path: String) -> Result<RunResult> {
    let args = vec![
        "ai".to_string(),
        "graph".to_string(),
        "-d".to_string(),
        description,
        "-o".to_string(),
        output_path,
    ];
    run(args).await
}

/// Generate a SPARQL query using AI from a description
///
/// # Arguments
/// * `description` - What the query should find
/// * `graph_path` - Optional path to RDF graph file for context
///
/// # Example
/// ```typescript
/// await aiSparql('Find all active users', 'users.ttl');
/// ```
#[napi]
pub async fn ai_sparql(description: String, graph_path: Option<String>) -> Result<RunResult> {
    let mut args = vec![
        "ai".to_string(),
        "sparql".to_string(),
        "-d".to_string(),
        description,
    ];

    if let Some(graph) = graph_path {
        args.push("-g".to_string());
        args.push(graph);
    }

    run(args).await
}

// ═══════════════════════════════════════════════════════════════════════════════
// Utility Commands
// ═══════════════════════════════════════════════════════════════════════════════

/// Run environment diagnostics
///
/// # Example
/// ```typescript
/// const result = await doctor();
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn doctor() -> Result<RunResult> {
    let args = vec!["doctor".to_string()];
    run(args).await
}

/// Get help text for a specific command
///
/// # Arguments
/// * `command` - Optional command name (empty for general help)
///
/// # Example
/// ```typescript
/// const result = await help('market');
/// console.log(result.stdout);
/// ```
#[napi]
pub async fn help(command: Option<String>) -> Result<RunResult> {
    let mut args = vec![];
    if let Some(cmd) = command {
        args.push(cmd);
    }
    args.push("--help".to_string());
    run(args).await
}
