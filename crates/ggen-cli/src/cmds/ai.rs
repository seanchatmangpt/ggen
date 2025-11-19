//! AI commands - clap-noun-verb v3.4.0 migration
//!
//! This module implements AI operations using the #[verb] macro pattern for
//! automatic discovery and JSON output support.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use futures::StreamExt;
use ggen_ai::config::get_global_config;
use serde::Serialize;
use std::path::PathBuf;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
pub struct GenerateOutput {
    generated_code: String,
    language: Option<String>,
    tokens_used: Option<usize>,
    model: String,
    finish_reason: Option<String>,
}

#[derive(Serialize)]
pub struct ChatMessage {
    role: String,
    content: String,
}

#[derive(Serialize)]
pub struct ChatOutput {
    messages: Vec<ChatMessage>,
    session_id: String,
    model: String,
    tokens_used: Option<usize>,
}

#[derive(Serialize)]
pub struct AnalyzeOutput {
    file_path: Option<String>,
    insights: Vec<String>,
    suggestions: Vec<String>,
    complexity_score: Option<f64>,
    model: String,
    tokens_used: Option<usize>,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Generate code with AI assistance
///
/// # Examples
///
/// Basic generation:
/// ```bash
/// ggen ai generate "Create a Rust function that calculates fibonacci numbers"
/// ```
///
/// With existing code context:
/// ```bash
/// ggen ai generate "Add error handling" --code "fn main() { println!(\"hello\"); }"
/// ```
///
/// With specific model:
/// ```bash
/// ggen ai generate "Generate REST API" --model gpt-4 --api-key $OPENAI_API_KEY
/// ```
#[allow(clippy::too_many_arguments)] // CLI command with many options
#[verb]
fn generate(
    prompt: String, code: Option<String>, model: Option<String>, _api_key: Option<String>,
    suggestions: bool, language: Option<String>, max_tokens: i64, temperature: f64,
) -> Result<GenerateOutput> {
    // Validate input parameters
    if prompt.is_empty() {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Prompt cannot be empty".to_string(),
        ));
    }

    if max_tokens <= 0 || max_tokens > 4_000_000 {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "max_tokens must be between 1 and 4,000,000".to_string(),
        ));
    }

    if temperature < 0.0 || temperature > 2.0 {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "temperature must be between 0.0 and 2.0".to_string(),
        ));
    }

    crate::runtime::block_on(async move {
        let mut global_config = get_global_config().clone();

        // Override model if provided
        if let Some(model_name) = &model {
            global_config.settings.default_model = Some(model_name.clone());
            if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider)
            {
                provider_config.model = model_name.clone();
            }
        }

        // Override max_tokens and temperature
        global_config.settings.default_max_tokens = Some(max_tokens as u32);
        global_config.settings.default_temperature = Some(temperature as f32);
        if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider) {
            provider_config.max_tokens = Some(max_tokens as u32);
            provider_config.temperature = Some(temperature as f32);
        }

        // Create client using global config
        let client = global_config.create_contextual_client().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to create AI client: {}",
                e
            ))
        })?;

        // Build prompt
        let mut full_prompt = prompt.clone();

        if let Some(lang) = &language {
            full_prompt.push_str(&format!("\nTarget language: {}", lang));
        }

        if let Some(code) = &code {
            full_prompt.push_str(&format!("\n\nExisting code:\n```\n{}\n```", code));
        }

        if suggestions {
            full_prompt.push_str("\n\nInclude suggestions for improvements and best practices.");
        }

        // Generate response
        let response = client.complete(&full_prompt).await.map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("AI generation failed: {}", e))
        })?;

        Ok(GenerateOutput {
            generated_code: response.content,
            language: language.clone(),
            tokens_used: response.usage.map(|u| u.total_tokens as usize),
            model: client.get_config().model.clone(),
            finish_reason: response.finish_reason,
        })
    })
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Interactive AI chat session
///
/// # Examples
///
/// Single message:
/// ```bash
/// ggen ai chat "Explain Rust ownership"
/// ```
///
/// Interactive mode:
/// ```bash
/// ggen ai chat --interactive --model claude-3-sonnet-20240229
/// ```
///
/// Stream response:
/// ```bash
/// ggen ai chat "Write a web server" --stream --api-key $OPENAI_API_KEY
/// ```
#[verb]
fn chat(
    message: Option<String>, model: Option<String>, _api_key: Option<String>, interactive: bool,
    stream: bool, max_tokens: i64, temperature: f64,
) -> Result<ChatOutput> {
    use std::io::Write;

    // Validate input parameters
    if max_tokens <= 0 || max_tokens > 4_000_000 {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "max_tokens must be between 1 and 4,000,000".to_string(),
        ));
    }

    if temperature < 0.0 || temperature > 2.0 {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "temperature must be between 0.0 and 2.0".to_string(),
        ));
    }

    crate::runtime::block_on(async move {
        let mut global_config = get_global_config().clone();

        // Override model if provided
        if let Some(model_name) = &model {
            global_config.settings.default_model = Some(model_name.clone());
            if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider)
            {
                provider_config.model = model_name.clone();
            }
        }

        // Override max_tokens and temperature
        global_config.settings.default_max_tokens = Some(max_tokens as u32);
        global_config.settings.default_temperature = Some(temperature as f32);
        if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider) {
            provider_config.max_tokens = Some(max_tokens as u32);
            provider_config.temperature = Some(temperature as f32);
        }

        // Create client using global config
        let client = global_config.create_contextual_client().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to create AI client: {}",
                e
            ))
        })?;

        let session_id = uuid::Uuid::new_v4().to_string();
        let mut messages: Vec<ChatMessage> = Vec::new();
        let mut total_tokens: Option<usize> = None;
        let model_name = client.get_config().model.clone();

        if interactive {
            // Interactive mode with multiple turns
            ggen_utils::alert_info!("🤖 AI Chat - Interactive Mode");
            ggen_utils::alert_info!("Model: {}", model_name);
            ggen_utils::alert_info!("Type 'exit' or 'quit' to end session\n");

            loop {
                eprint!("> ");
                std::io::stderr().flush().map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to flush stderr: {}",
                        e
                    ))
                })?;

                let mut input = String::new();
                std::io::stdin().read_line(&mut input).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to read input: {}",
                        e
                    ))
                })?;

                let input = input.trim();
                if input.is_empty() {
                    continue;
                }

                if input == "exit" || input == "quit" {
                    break;
                }

                messages.push(ChatMessage {
                    role: "user".to_string(),
                    content: input.to_string(),
                });

                if stream {
                    // Stream response
                    let mut stream = client.complete_stream(input).await.map_err(|e| {
                        clap_noun_verb::NounVerbError::execution_error(format!(
                            "Streaming failed: {}",
                            e
                        ))
                    })?;

                    let mut full_response = String::new();
                    eprint!("🤖: ");
                    while let Some(chunk) = stream.next().await {
                        eprint!("{}", chunk.content);
                        std::io::stderr().flush().map_err(|e| {
                            clap_noun_verb::NounVerbError::execution_error(format!(
                                "Failed to flush stderr: {}",
                                e
                            ))
                        })?;
                        full_response.push_str(&chunk.content);

                        if let Some(usage) = chunk.usage {
                            total_tokens = Some(usage.total_tokens as usize);
                        }
                    }
                    ggen_utils::alert_info!("\n");

                    messages.push(ChatMessage {
                        role: "assistant".to_string(),
                        content: full_response,
                    });
                } else {
                    // Non-streaming response
                    let response = client.complete(input).await.map_err(|e| {
                        clap_noun_verb::NounVerbError::execution_error(format!(
                            "Chat failed: {}",
                            e
                        ))
                    })?;

                    ggen_utils::alert_info!("🤖: {}\n", response.content);

                    if let Some(usage) = response.usage {
                        total_tokens = Some(usage.total_tokens as usize);
                    }

                    messages.push(ChatMessage {
                        role: "assistant".to_string(),
                        content: response.content,
                    });
                }
            }
        } else if let Some(msg) = message {
            // Single message mode
            messages.push(ChatMessage {
                role: "user".to_string(),
                content: msg.clone(),
            });

            if stream {
                // Stream response
                let mut stream = client.complete_stream(&msg).await.map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Streaming failed: {}",
                        e
                    ))
                })?;

                let mut full_response = String::new();
                while let Some(chunk) = stream.next().await {
                    eprint!("{}", chunk.content);
                    std::io::stderr().flush().map_err(|e| {
                        clap_noun_verb::NounVerbError::execution_error(format!(
                            "Failed to flush stderr: {}",
                            e
                        ))
                    })?;
                    full_response.push_str(&chunk.content);

                    if let Some(usage) = chunk.usage {
                        total_tokens = Some(usage.total_tokens as usize);
                    }
                }

                messages.push(ChatMessage {
                    role: "assistant".to_string(),
                    content: full_response,
                });
            } else {
                // Non-streaming response
                let response = client.complete(&msg).await.map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!("Chat failed: {}", e))
                })?;

                if let Some(usage) = response.usage {
                    total_tokens = Some(usage.total_tokens as usize);
                }

                messages.push(ChatMessage {
                    role: "assistant".to_string(),
                    content: response.content,
                });
            }
        } else {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "Provide a message or use --interactive for chat session",
            ));
        }

        Ok(ChatOutput {
            messages,
            session_id,
            model: model_name,
            tokens_used: total_tokens,
        })
    })
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

/// Analyze code with AI insights
///
/// # Examples
///
/// Analyze code string:
/// ```bash
/// ggen ai analyze "fn main() { println!(\"hello\"); }"
/// ```
///
/// Analyze from file:
/// ```bash
/// ggen ai analyze --file src/main.rs --api-key $OPENAI_API_KEY
/// ```
///
/// Analyze project directory:
/// ```bash
/// ggen ai analyze --project ./my-crate --model gpt-4
/// ```
#[allow(clippy::too_many_arguments)] // CLI command with many options
#[verb]
fn analyze(
    code: Option<String>, file: Option<PathBuf>, project: Option<PathBuf>, model: Option<String>,
    api_key: Option<String>, complexity: bool, security: bool, performance: bool, max_tokens: i64,
) -> Result<AnalyzeOutput> {
    // Validate input parameters
    if max_tokens <= 0 || max_tokens > 4_000_000 {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "max_tokens must be between 1 and 4,000,000".to_string(),
        ));
    }

    crate::runtime::block_on(async move {
        // Determine what to analyze
        let (code_content, file_path) = if let Some(code_str) = code {
            (code_str, None)
        } else if let Some(file_path) = &file {
            let content = std::fs::read_to_string(file_path).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to read file: {}",
                    e
                ))
            })?;
            (content, Some(file_path.display().to_string()))
        } else if let Some(project_path) = &project {
            // For project analysis, we'll provide a summary prompt
            return analyze_project(project_path, model, api_key, max_tokens as u32).await;
        } else {
            return Err(clap_noun_verb::NounVerbError::execution_error(
                "Provide code, --file, or --project to analyze",
            ));
        };

        let mut global_config = get_global_config().clone();

        // Override model if provided
        if let Some(model_name) = &model {
            global_config.settings.default_model = Some(model_name.clone());
            if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider)
            {
                provider_config.model = model_name.clone();
            }
        }

        // Override max_tokens and temperature (lower for analysis)
        global_config.settings.default_max_tokens = Some(max_tokens as u32);
        global_config.settings.default_temperature = Some(0.3);
        if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider) {
            provider_config.max_tokens = Some(max_tokens as u32);
            provider_config.temperature = Some(0.3);
        }

        // Create client using global config
        let client = global_config.create_contextual_client().map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to create AI client: {}",
                e
            ))
        })?;

        // Build analysis prompt
        let mut prompt = format!(
            "Analyze the following code and provide insights:\n\n```\n{}\n```\n\n",
            code_content
        );

        prompt.push_str("Provide:\n");
        prompt.push_str("1. Key insights about the code structure and design\n");
        prompt.push_str("2. Suggestions for improvements\n");

        if complexity {
            prompt.push_str("3. Complexity analysis (cyclomatic, cognitive)\n");
        }
        if security {
            prompt.push_str("4. Security considerations and potential vulnerabilities\n");
        }
        if performance {
            prompt.push_str("5. Performance optimization opportunities\n");
        }

        prompt.push_str("\nFormat your response with clear sections.");

        // Generate analysis
        let response = client.complete(&prompt).await.map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Analysis failed: {}", e))
        })?;

        // Parse response into structured output
        let (insights, suggestions) = parse_analysis_response(&response.content);

        // Calculate complexity score if requested (simplified placeholder)
        let complexity_score = if complexity {
            Some(estimate_complexity(&code_content))
        } else {
            None
        };

        Ok(AnalyzeOutput {
            file_path,
            insights,
            suggestions,
            complexity_score,
            model: client.get_config().model.clone(),
            tokens_used: response.usage.map(|u| u.total_tokens as usize),
        })
    })
    .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Analyze a project directory
async fn analyze_project(
    project_path: &PathBuf, model: Option<String>, _api_key: Option<String>, max_tokens: u32,
) -> Result<AnalyzeOutput> {
    use walkdir::WalkDir;

    // Collect source files
    let mut source_files = Vec::new();
    for entry in WalkDir::new(project_path)
        .max_depth(5)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.is_file() {
            if let Some(ext) = path.extension() {
                if matches!(ext.to_str(), Some("rs") | Some("toml") | Some("md")) {
                    source_files.push(path.to_path_buf());
                }
            }
        }
    }

    // Use global config system
    let mut global_config = get_global_config().clone();

    // Override model if provided
    if let Some(model_name) = &model {
        global_config.settings.default_model = Some(model_name.clone());
        if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider) {
            provider_config.model = model_name.clone();
        }
    }

    // Override max_tokens and temperature
    global_config.settings.default_max_tokens = Some(max_tokens);
    global_config.settings.default_temperature = Some(0.3);
    if let Some(provider_config) = global_config.providers.get_mut(&global_config.provider) {
        provider_config.max_tokens = Some(max_tokens);
        provider_config.temperature = Some(0.3);
    }

    // Create client using global config
    let client = global_config.create_contextual_client().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to create AI client: {}", e))
    })?;

    // Build project summary
    let file_list: Vec<String> = source_files
        .iter()
        .map(|p| p.display().to_string())
        .collect();

    let prompt = format!(
        "Analyze this project structure:\n\nProject: {}\n\nFiles:\n{}\n\n\
        Provide insights about:\n\
        1. Project architecture and organization\n\
        2. Code quality and design patterns\n\
        3. Suggested improvements\n\
        4. Potential issues or technical debt",
        project_path.display(),
        file_list.join("\n")
    );

    // Generate analysis
    let response = client.complete(&prompt).await.map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Project analysis failed: {}", e))
    })?;

    // Parse response
    let (insights, suggestions) = parse_analysis_response(&response.content);

    Ok(AnalyzeOutput {
        file_path: Some(project_path.display().to_string()),
        insights,
        suggestions,
        complexity_score: None,
        model: client.get_config().model.clone(),
        tokens_used: response.usage.map(|u| u.total_tokens as usize),
    })
}

/// Parse analysis response into structured insights and suggestions
fn parse_analysis_response(response: &str) -> (Vec<String>, Vec<String>) {
    let mut insights = Vec::new();
    let mut suggestions = Vec::new();

    let mut current_section = "";
    for line in response.lines() {
        let line = line.trim();

        // Detect section headers
        if line.to_lowercase().contains("insight") {
            current_section = "insights";
            continue;
        } else if line.to_lowercase().contains("suggestion")
            || line.to_lowercase().contains("improvement")
        {
            current_section = "suggestions";
            continue;
        }

        // Add to appropriate section
        if !line.is_empty() && line.starts_with(|c: char| c.is_numeric() || c == '-' || c == '*') {
            let cleaned = line
                .trim_start_matches(|c: char| c.is_numeric() || c == '.' || c == '-' || c == '*')
                .trim()
                .to_string();

            match current_section {
                "insights" => insights.push(cleaned),
                "suggestions" => suggestions.push(cleaned),
                _ => {
                    // Default to insights if unclear
                    insights.push(cleaned);
                }
            }
        }
    }

    // If parsing failed, put entire response in insights
    if insights.is_empty() && suggestions.is_empty() {
        insights.push(response.to_string());
    }

    (insights, suggestions)
}

/// Estimate code complexity (simplified metric)
fn estimate_complexity(code: &str) -> f64 {
    let mut complexity = 1.0;

    // Count control flow keywords
    let control_flow = ["if", "else", "match", "for", "while", "loop"];
    for keyword in &control_flow {
        complexity += code.matches(keyword).count() as f64;
    }

    // Count nested blocks (simplified)
    let nesting_level = code.matches('{').count().max(1) as f64;
    complexity *= nesting_level.log10().max(1.0);

    // Count function definitions
    complexity += code.matches("fn ").count() as f64 * 0.5;

    // Normalize to 0-100 scale
    (complexity.min(100.0) * 10.0).round() / 10.0
}

#[derive(Serialize)]
pub struct EvolveOutput {
    cycles_completed: usize,
    schema_modifications: usize,
    cumulative_reward: f64,
    converged: bool,
    language_quality_scores: HashMap<String, f64>,
    evolved_ontology: Option<String>,
}

/// Evolve RDF ontologies using AI with RL feedback loops and quantum-inspired algorithms
///
/// # Examples
///
/// Basic evolution:
/// ```bash
/// ggen ai evolve --cycles 10
/// ```
///
/// With custom parameters:
/// ```bash
/// ggen ai evolve --cycles 20 --learning-rate 0.05 --quantum-temperature 2.0
/// ```
///
/// Export results:
/// ```bash
/// ggen ai evolve --cycles 15 --output ontology.ttl --metrics metrics.json
/// ```
#[allow(clippy::too_many_arguments)]
#[verb]
fn evolve(
    cycles: u32,
    learning_rate: f64,
    discount_factor: f64,
    exploration_rate: f64,
    quantum_temperature: f64,
    quantum_states: u32,
    target_languages: String,
    output: Option<PathBuf>,
    metrics_output: Option<PathBuf>,
) -> Result<EvolveOutput> {
    use ggen_ai::{EvolutionConfig, OntologyEvolutionCoordinator};

    crate::runtime::block_on(async move {
        // Parse target languages
        let target_langs: Vec<String> = target_languages
            .split(',')
            .map(|s| s.trim().to_string())
            .collect();

        // Create evolution configuration
        let config = EvolutionConfig {
            learning_rate,
            discount_factor,
            exploration_rate,
            quantum_temperature,
            quantum_states: quantum_states as usize,
            max_iterations: cycles as usize,
            target_languages: target_langs,
            ..Default::default()
        };

        ggen_utils::alert_info!("🧬 AI-Native Ontology Evolution Engine\n");
        ggen_utils::alert_info!("Initializing coordinator...\n");

        let coordinator = OntologyEvolutionCoordinator::new(config);

        ggen_utils::alert_info!("Running {} evolution cycles...\n", cycles);

        let evolution_cycles = coordinator
            .evolve_n_cycles(cycles as usize)
            .await
            .map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Evolution failed: {}",
                    e
                ))
            })?;

        // Get final metrics
        let metrics = coordinator.get_metrics().await;

        ggen_utils::alert_success!("✅ Evolution Complete!\n");
        ggen_utils::alert_info!("Cycles: {}\n", metrics.iterations_completed);
        ggen_utils::alert_info!("Schema Modifications: {}\n", metrics.schema_modifications);
        ggen_utils::alert_info!("Cumulative Reward: {:.4}\n", metrics.cumulative_reward);
        ggen_utils::alert_info!("Converged: {}\n", metrics.converged);

        // Export evolved ontology if requested
        let evolved_ontology = if output.is_some() {
            let ontology_rdf = coordinator.export_ontology().await.map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to export ontology: {}",
                    e
                ))
            })?;

            if let Some(path) = &output {
                std::fs::write(path, &ontology_rdf).map_err(|e| {
                    clap_noun_verb::NounVerbError::execution_error(format!(
                        "Failed to write ontology file: {}",
                        e
                    ))
                })?;
                ggen_utils::alert_success!("💾 Ontology exported to: {:?}\n", path);
            }

            Some(ontology_rdf)
        } else {
            None
        };

        // Export metrics if requested
        if let Some(metrics_path) = &metrics_output {
            let metrics_json = serde_json::to_string_pretty(&metrics).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to serialize metrics: {}",
                    e
                ))
            })?;

            std::fs::write(metrics_path, metrics_json).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to write metrics file: {}",
                    e
                ))
            })?;

            ggen_utils::alert_success!("📈 Metrics exported to: {:?}\n", metrics_path);
        }

        Ok(EvolveOutput {
            cycles_completed: metrics.iterations_completed,
            schema_modifications: metrics.schema_modifications,
            cumulative_reward: metrics.cumulative_reward,
            converged: metrics.converged,
            language_quality_scores: metrics.language_quality_scores,
            evolved_ontology,
        })
    })
}

// ============================================================================
// Usage Notes
// ============================================================================

// To use this in your CLI:
// 1. Replace the existing ai.rs with this file (or integrate gradually)
// 2. Update main.rs to use: clap_noun_verb::run()
// 3. Ensure ggen-ai is properly configured with API keys
// 4. Test with: cargo run -- ai --help
// 5. JSON output: cargo run -- ai generate "hello world" --format json
