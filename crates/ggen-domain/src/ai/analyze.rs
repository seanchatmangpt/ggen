//! AI-powered code analysis
//!
//! This module provides AI-powered analysis capabilities for code and project
//! structures. It can analyze code snippets and entire project directories to
//! provide insights and recommendations.
//!
//! ## Features
//!
//! - **Code Analysis**: Analyze code snippets for patterns and issues
//! - **Project Analysis**: Analyze entire project structures
//! - **Insights Generation**: Generate actionable insights from analysis
//!
//! ## Examples
//!
//! ### Analyzing Code
//!
//! ```rust,no_run
//! use ggen_domain::ai::analyze::analyze_code;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let code = r#"
//! fn main() {
//!     println!("Hello, world!");
//! }
//! "#;
//! let analysis = analyze_code(code).await?;
//! println!("{}", analysis);
//! # Ok(())
//! # }
//! ```
//!
//! ### Analyzing a Project
//!
//! ```rust,no_run
//! use ggen_domain::ai::analyze::analyze_project;
//! use std::path::Path;
//!
//! # async fn example() -> anyhow::Result<()> {
//! let analysis = analyze_project(Path::new("my-project")).await?;
//! println!("{}", analysis);
//! # Ok(())
//! # }
//! ```

use ggen_ai::{AiConfig, GenAiClient, LlmClient};
use ggen_utils::{bail, error::Error, error::Result};

/// Analyze code and provide insights (LLM-backed)
pub async fn analyze_code(code: &str) -> Result<String> {
    if code.is_empty() {
        bail!("Code cannot be empty");
    }

    let config = AiConfig::from_env().unwrap_or_else(|_| AiConfig::new()).llm;
    let client = GenAiClient::new(config)
        .map_err(|e| Error::new(&format!("Failed to create LLM client: {e}")))?;

    let prompt = format!(
        "You are a senior Rust engineer. Analyze the following code and provide concise findings (risks, improvements, tests).\n\nCode:\n```rust\n{}\n```",
        code
    );

    let resp = client
        .complete(&prompt)
        .await
        .map_err(|e| Error::new(&format!("LLM analysis failed: {e}")))?;
    Ok(resp.content)
}

/// Analyze project structure (summarized)
pub async fn analyze_project(path: &std::path::Path) -> Result<String> {
    if !path.exists() {
        bail!("Path does not exist: {}", path.display());
    }

    let config = AiConfig::from_env().unwrap_or_else(|_| AiConfig::new()).llm;
    let client = GenAiClient::new(config)
        .map_err(|e| Error::new(&format!("Failed to create LLM client: {e}")))?;

    let prompt = format!(
        "You are a senior Rust engineer. Provide a quick review and risk assessment for the project at path: {}. Focus on likely hotspots to inspect (testing gaps, performance, security). Respond succinctly.",
        path.display()
    );

    let resp = client
        .complete(&prompt)
        .await
        .map_err(|e| Error::new(&format!("LLM project analysis failed: {e}")))?;
    Ok(resp.content)
}
