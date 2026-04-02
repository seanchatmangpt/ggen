//! AI commands - CLI layer delegating to ggen-ai LLM client
//!
//! Uses ggen_ai::GenAiClient for real LLM calls with OTEL span verification.

use clap_noun_verb_macros::verb;
use serde::Serialize;

use crate::runtime::block_on;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct GenerateOutput {
    content: String,
    model: String,
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
}

#[derive(Serialize)]
struct ChatOutput {
    content: String,
    model: String,
    prompt_tokens: u32,
    completion_tokens: u32,
}

#[derive(Serialize)]
struct AnalyzeOutput {
    content: String,
    model: String,
    prompt_tokens: u32,
    completion_tokens: u32,
}

// ============================================================================
// Verb Functions
// ============================================================================

/// Generate code with AI assistance
#[allow(clippy::too_many_arguments)]
#[verb]
fn generate(
    prompt: String, code: Option<String>, model: Option<String>, max_tokens: Option<i64>,
    temperature: Option<f64>, _suggestions: bool, _language: Option<String>,
    _api_key: Option<String>,
) -> crate::Result<GenerateOutput> {
    run_generate(prompt, code, model, max_tokens, temperature)
}

fn run_generate(
    prompt: String, code: Option<String>, model: Option<String>, max_tokens: Option<i64>,
    temperature: Option<f64>,
) -> crate::Result<GenerateOutput> {
    let span = tracing::info_span!(
        "cli.ai.generate",
        "operation.name" = "cli.ai.generate",
        "operation.type" = "cli",
        "cli.command" = "ai.generate",
        "llm.model" = model.as_deref().unwrap_or("default"),
    );
    let _guard = span.enter();

    let full_prompt = build_generate_prompt(&prompt, code.as_ref());
    let result = call_llm(&full_prompt, model, max_tokens, temperature)?;
    println!("{}", result.content);
    Ok(GenerateOutput {
        model: result.model,
        prompt_tokens: result.prompt_tokens,
        completion_tokens: result.completion_tokens,
        total_tokens: result.total_tokens,
        content: result.content,
    })
}

/// Interactive AI chat session
#[verb]
fn chat(
    message: Option<String>, model: Option<String>, _interactive: bool, _stream: bool,
    max_tokens: Option<i64>, temperature: Option<f64>,
) -> crate::Result<ChatOutput> {
    run_chat(message, model, max_tokens, temperature)
}

fn run_chat(
    message: Option<String>, model: Option<String>, max_tokens: Option<i64>,
    temperature: Option<f64>,
) -> crate::Result<ChatOutput> {
    let span = tracing::info_span!(
        "cli.ai.chat",
        "operation.name" = "cli.ai.chat",
        "operation.type" = "cli",
        "cli.command" = "ai.chat",
    );
    let _guard = span.enter();

    let prompt = message.unwrap_or_else(|| "Hello".to_string());
    let result = call_llm(&prompt, model, max_tokens, temperature)?;
    println!("{}", result.content);
    Ok(ChatOutput {
        content: result.content.clone(),
        model: result.model,
        prompt_tokens: result.prompt_tokens,
        completion_tokens: result.completion_tokens,
    })
}

/// Analyze code and provide insights
#[verb]
fn analyze(
    file: Option<std::path::PathBuf>, code: Option<String>, model: Option<String>,
) -> crate::Result<AnalyzeOutput> {
    run_analyze(file, code, model)
}

fn run_analyze(
    file: Option<std::path::PathBuf>, code: Option<String>, model: Option<String>,
) -> crate::Result<AnalyzeOutput> {
    let span = tracing::info_span!(
        "cli.ai.analyze",
        "operation.name" = "cli.ai.analyze",
        "operation.type" = "cli",
        "cli.command" = "ai.analyze",
    );
    let _guard = span.enter();

    let source = if let Some(ref code_content) = code {
        code_content.clone()
    } else if let Some(ref path) = file {
        std::fs::read_to_string(path)
            .map_err(|e| GgenError::file_error(path.to_string_lossy().as_ref(), &e.to_string()))?
    } else {
        return Err(GgenError::InvalidInput(
            "Either --file or --code must be provided".to_string(),
        ));
    };

    let prompt = format!(
        "Analyze the following code for issues, improvements, and best practices:\n\n```\n{}\n```\n\nProvide specific, actionable feedback.",
        source
    );
    let result = call_llm(&prompt, model, None, None)?;
    println!("{}", result.content);
    Ok(AnalyzeOutput {
        content: result.content.clone(),
        model: result.model,
        prompt_tokens: result.prompt_tokens,
        completion_tokens: result.completion_tokens,
    })
}

// ============================================================================
// Helpers
// ============================================================================

struct LlmResult {
    content: String,
    model: String,
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
}

fn call_llm(
    prompt: &str, model: Option<String>, max_tokens: Option<i64>, temperature: Option<f64>,
) -> crate::Result<LlmResult> {
    use ggen_ai::LlmClient;

    let mut config = ggen_ai::LlmConfig::default();
    if let Some(m) = model {
        config.model = m;
    }
    if let Some(mt) = max_tokens {
        config.max_tokens = Some(mt as u32);
    }
    if let Some(t) = temperature {
        config.temperature = Some(t as f32);
    }

    let client = ggen_ai::GenAiClient::new(config)
        .map_err(|e| GgenError::ExternalServiceError(format!("Failed to create LLM client: {}", e)))?;

    let response = block_on(client.complete(prompt))
        .map_err(|e| GgenError::ExternalServiceError(format!("Runtime error: {}", e)))?
        .map_err(|e| GgenError::ExternalServiceError(format!("LLM request failed: {}", e)))?;

    let model_used = response.model.clone();
    let usage = response.usage.unwrap_or(ggen_ai::UsageStats {
        prompt_tokens: 0,
        completion_tokens: 0,
        total_tokens: 0,
    });

    Ok(LlmResult {
        content: response.content,
        model: model_used,
        prompt_tokens: usage.prompt_tokens,
        completion_tokens: usage.completion_tokens,
        total_tokens: usage.total_tokens,
    })
}

fn build_generate_prompt(prompt: &str, code: Option<&String>) -> String {
    match code {
        Some(c) => format!(
            "Generate code based on this description:\n{}\n\nExisting code context:\n```\n{}\n```\n\nProvide complete, production-ready code.",
            prompt, c
        ),
        None => format!(
            "Generate code based on this description:\n{}\n\nProvide complete, production-ready code.",
            prompt
        ),
    }
}
