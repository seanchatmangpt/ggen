//! Framework Bridge Commands - clap-noun-verb v4.0.2 Integration
//!
//! This module implements framework bridge commands for generating adapters
//! to connect ggen-generated components with external frameworks like LangChain.
//!
//! Typical usage:
//! ```bash
//! ggen framework bridge langchain extract_claims
//! ```

use clap_noun_verb::Result as NounVerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct BridgeLangChainOutput {
    tool_name: String,
    framework: String,
    output_path: String,
    status: String,
    message: String,
    python_syntax_valid: bool,
}

// ============================================================================
// Template Context
// ============================================================================

#[derive(Clone, Debug)]
struct TemplateContext {
    tool_name: String,
    description: String,
    parameters: String,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Bridge to LangChain Python framework
///
/// Generates a LangChain BaseTool adapter for a ggen-generated component.
/// The adapter can be used in LangChain agents and chains.
///
/// # Arguments
/// * `name` - The name of the component (e.g., "extract_claims")
///
/// # Example
/// ```bash
/// ggen framework bridge langchain extract_claims
/// ```
#[verb]
fn bridge_langchain(name: String) -> NounVerbResult<BridgeLangChainOutput> {
    // Step 1: Validate input
    validate_component_name(&name)?;

    // Step 2: Create output directory
    let output_dir = PathBuf::from("output/langchain");
    fs::create_dir_all(&output_dir).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create output directory: {}",
            e
        ))
    })?;

    // Step 3: Build template context
    let context = TemplateContext {
        tool_name: name.clone(),
        description: format!("LangChain adapter for {}", name),
        parameters: "input: str".to_string(),
    };

    // Step 4: Render template
    let adapter_code = render_langchain_template(&context)?;

    // Step 5: Verify Python syntax
    let syntax_valid = verify_python_syntax(&adapter_code).unwrap_or(false);
    if !syntax_valid {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Generated Python code has invalid syntax".to_string(),
        ));
    }

    // Step 6: Write output file
    let output_file = output_dir.join(format!("{}.tool.py", name));
    fs::write(&output_file, &adapter_code).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to write output file: {}",
            e
        ))
    })?;

    // Step 7: Return success
    let output_path = output_file.to_string_lossy().to_string();
    Ok(BridgeLangChainOutput {
        tool_name: name.clone(),
        framework: "langchain".to_string(),
        output_path,
        status: "generated".to_string(),
        message: format!(
            "LangChain adapter for '{}' generated successfully. Location: {}",
            name,
            output_dir.display()
        ),
        python_syntax_valid: syntax_valid,
    })
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Validate component name
fn validate_component_name(name: &str) -> NounVerbResult<()> {
    if name.trim().is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Component name must not be empty",
        ));
    }
    let valid = name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '_' || c == '-');
    if !valid {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Component name contains invalid characters. Use alphanumeric, underscores, hyphens only.",
        ));
    }
    Ok(())
}

/// Render the LangChain template using Tera
fn render_langchain_template(context: &TemplateContext) -> NounVerbResult<String> {
    // Try to load and render the Tera template
    let template_path = "templates/langchain.tool.py.tera";

    if Path::new(template_path).exists() {
        // Use Tera template if it exists
        use tera::Tera;

        // Tera::new expects a glob pattern, so we use a wildcard pattern
        let tera = Tera::new("templates/*.tera").map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to load templates: {}",
                e
            ))
        })?;

        let mut ctx = tera::Context::new();
        ctx.insert("tool_name", &context.tool_name);
        ctx.insert("description", &context.description);
        ctx.insert("parameters", &context.parameters);

        tera.render("langchain.tool.py.tera", &ctx).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to render template: {}",
                e
            ))
        })
    } else {
        // Fallback: generate template inline
        generate_langchain_fallback(context)
    }
}

/// Generate LangChain adapter code without a template (fallback)
fn generate_langchain_fallback(context: &TemplateContext) -> NounVerbResult<String> {
    let pascal_name = pascal_case(&context.tool_name);

    let code = format!(
        r#"from langchain.tools import BaseTool
from typing import Optional

class {}Tool(BaseTool):
    """{}"""

    name = "{}"
    description = "{}"

    def _run(self, {}) -> str:
        """Execute the tool."""
        # Call wrapped component
        return "{} executed"

    async def _arun(self, {}) -> str:
        """Execute the tool asynchronously."""
        return self._run({})
"#,
        pascal_name,
        context.description,
        context.tool_name,
        context.description,
        context.parameters,
        context.tool_name,
        context.parameters,
        extract_param_names(&context.parameters)
    );

    Ok(code)
}

/// Convert snake_case to PascalCase
fn pascal_case(input: &str) -> String {
    input
        .split('_')
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
            }
        })
        .collect()
}

/// Extract parameter names from "param1: type, param2: type" format
fn extract_param_names(params: &str) -> String {
    params
        .split(',')
        .filter_map(|p| {
            p.trim()
                .split(':')
                .next()
                .map(|name| name.trim().to_string())
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Verify Python syntax by attempting to compile
fn verify_python_syntax(code: &str) -> Result<bool, String> {
    // Create a temporary file with the code
    let temp_file = std::env::temp_dir().join("ggen_verify_syntax.py");

    fs::write(&temp_file, code).map_err(|e| format!("Failed to write temp file: {}", e))?;

    // Use Python to verify syntax
    let temp_path_str = temp_file.to_string_lossy().to_string();
    let output = Command::new("python3")
        .arg("-m")
        .arg("py_compile")
        .arg(&temp_path_str)
        .output()
        .or_else(|_| {
            // Fallback to python if python3 not found
            Command::new("python")
                .arg("-m")
                .arg("py_compile")
                .arg(&temp_path_str)
                .output()
        })
        .map_err(|e| format!("Failed to run Python: {}", e))?;

    // Clean up temp file
    let _ = fs::remove_file(&temp_file);

    Ok(output.status.success())
}
