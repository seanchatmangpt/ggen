//! Generate frontmatter using AI
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This CLI command should generate blog/documentation frontmatter (YAML metadata) using AI,
//! providing SEO-optimized metadata for Jekyll, Hugo, or custom static site generators.
//! It should make creating properly-structured frontmatter effortless for content creators.
//!
//! ## RESPONSIBILITIES
//! 1. **Parse Arguments**: Should accept description, examples, and output options from CLI
//! 2. **Provider Selection**: Should auto-detect LLM provider or use user-specified configuration
//! 3. **Generate Frontmatter**: Should call TemplateGenerator with appropriate prompts
//! 4. **Format Output**: Should serialize frontmatter as valid YAML (not Rust Debug format)
//! 5. **Save or Display**: Should write to file or print to stdout based on user preference
//! 6. **Mock Mode**: Should support testing without real LLM API calls
//! 7. **Configuration**: Should respect global config and allow per-command overrides
//!
//! ## CONSTRAINTS
//! - Must output valid YAML that Jekyll/Hugo can parse
//! - Must use serde_yaml for serialization (not Debug trait)
//! - Must support all major LLM providers (OpenAI, Anthropic, Ollama)
//! - Must work offline with --mock flag
//! - Must handle missing API keys gracefully
//! - Must complete within reasonable time (<30s for most prompts)
//!
//! ## DEPENDENCIES
//! - `TemplateGenerator`: Should generate frontmatter via LLM
//! - `GlobalConfig`: Should provide provider auto-detection
//! - `LlmClient`: Should be provider-agnostic
//! - `serde_yaml`: Should serialize frontmatter correctly
//!
//! ## INVARIANTS
//! - Output must be valid YAML (parseable by static site generators)
//! - Frontmatter must include `---` delimiters
//! - Template body must be preserved exactly as generated
//! - File operations must be atomic (write + sync, no partial writes)
//!
//! ## DATA FLOW
//! ```text
//! User Command: ggen ai frontmatter "Blog about AI ethics"
//!   ↓
//! Parse CLI Arguments
//!   ├─ description: "Blog about AI ethics"
//!   ├─ examples: ["Include SEO keywords", "Add author"]
//!   ├─ output: Some("post.md") or None
//!   └─ model/temperature/max_tokens overrides
//!   ↓
//! Provider Selection
//!   ├─ Check --mock flag → Use MockClient
//!   ├─ Check --model flag → Use GenAiClient with custom config
//!   └─ Else → Use GlobalConfig::create_contextual_client()
//!   ↓
//! TemplateGenerator::generate_template()
//!   ├─ Build prompt from description + examples
//!   ├─ Call LLM provider
//!   └─ Parse response into Template
//!   ↓
//! Format Output
//!   ├─ Serialize template.front with serde_yaml
//!   ├─ Add YAML delimiters (---)
//!   └─ Append template body
//!   ↓
//! Output
//!   ├─ If --output specified → Write to file
//!   └─ Else → Print to stdout
//! ```
//!
//! ## ERROR HANDLING STRATEGY
//! - Missing API key → Clear error: "OPENAI_API_KEY not set. See: docs/setup.md"
//! - LLM error → Show provider error, suggest retry or different provider
//! - Invalid YAML → Show generated content, suggest prompt refinement
//! - File write error → Show full path, check permissions
//! - Network timeout → Suggest --mock flag or retry
//!
//! ## COMMAND-LINE INTERFACE CONTRACT
//!
//! ### Required Arguments
//! - `--description, -d`: Description of content needing frontmatter
//!
//! ### Optional Arguments
//! - `--examples, -e`: Additional requirements or examples (repeatable)
//! - `--output, -o`: Output file path (stdout if not specified)
//! - `--mock`: Use mock client for testing (no API calls)
//! - `--model`: Override default model (e.g., "gpt-4", "claude-3-opus")
//! - `--temperature`: Generation temperature (0.0-1.0)
//! - `--max-tokens`: Maximum tokens to generate
//!
//! ### Examples
//! ```bash
//! # Generate and display
//! ggen ai frontmatter --description "Blog post about Rust async"
//!
//! # Generate with examples and save
//! ggen ai frontmatter \
//!   --description "Tutorial on neural networks" \
//!   --examples "Include difficulty level" \
//!   --examples "Add estimated reading time" \
//!   --output tutorial.md
//!
//! # Test without API calls
//! ggen ai frontmatter --description "Test post" --mock
//!
//! # Use specific model
//! ggen ai frontmatter \
//!   --description "Advanced Rust patterns" \
//!   --model "claude-3-opus" \
//!   --temperature 0.7
//! ```
//!
//! ## OUTPUT FORMAT CONTRACT
//!
//! ### CORRECT Format (Valid YAML)
//! ```yaml
//! title: "Understanding AI Ethics"
//! description: "Exploring ethical considerations in artificial intelligence development"
//! date: 2025-10-10
//! author: "Content Creator"
//! tags:
//!   - ai
//!   - ethics
//!   - technology
//! keywords: "AI, ethics, responsible AI, technology"
//! image: "/images/ai-ethics.jpg"
//! ---
//!
//! Blog post content starts here...
//! ```
//!
//! ### WRONG Format (Rust Debug - Bug from earlier)
//! ```text
//! Frontmatter { to: Some("file.md"), vars: {"title": String("...")} }
//! ---
//! Content
//! ```
//!
//! **FIX APPLIED**: Now uses `serde_yaml::to_string(&template.front)`
//!
//! ## TESTING STRATEGY
//! - Test with --mock flag (no API calls)
//! - Test output to file vs stdout
//! - Test with all provider types (OpenAI, Anthropic, Ollama)
//! - Test model/temperature/max_tokens overrides
//! - Test error handling (missing keys, network errors)
//! - Validate output is valid YAML (parse with serde_yaml)
//! - Property test: Output should always be Jekyll/Hugo compatible
//!
//! ## REFACTORING PRIORITIES
//! - [P0] DONE: Fix YAML output (was using Debug format)
//! - [P0] Add YAML validation before output
//! - [P1] Support frontmatter-only mode (no body generation)
//! - [P1] Add template library for common frontmatter types
//! - [P2] Support JSON output format (alternative to YAML)
//! - [P2] Add interactive mode (prompt for missing fields)
//! - [P3] Support frontmatter profiles (blog, docs, landing pages)
//!
//! ## CORE TEAM BEST PRACTICES APPLIED
//! 1. **Clear Errors**: Actionable error messages with fix suggestions
//! 2. **Progressive Enhancement**: Works with minimal args, supports rich configuration
//! 3. **Provider Agnostic**: Works with any LLM via abstraction
//! 4. **Testability**: --mock flag enables testing without APIs
//! 5. **Sensible Defaults**: Auto-detects provider, uses reasonable settings
//! 6. **Output Flexibility**: File or stdout, user's choice
//!
//! ## INTEGRATION NOTES
//! - **TemplateGenerator Integration**: Uses for all generation logic
//! - **GlobalConfig Integration**: Provides provider auto-detection
//! - **Static Site Generators**: Output compatible with Jekyll, Hugo, Eleventy
//! - **SEO Tools**: Generated frontmatter suitable for SEO optimization
//!
//! ## KNOWN ISSUES
//! - [FIXED] Previously output Rust Debug format instead of YAML
//! - [WONTFIX] Cannot generate frontmatter for binary files
//!
//! ## COMPATIBILITY NOTES
//! - **Jekyll**: Fully compatible (YAML frontmatter standard)
//! - **Hugo**: Fully compatible (supports YAML/TOML/JSON)
//! - **Eleventy**: Compatible with YAML frontmatter
//! - **Custom Generators**: Standard YAML format works universally

use anyhow;
use clap::Args;
use ggen_ai::TemplateGenerator;
use ggen_utils::error::Result;
use std::fs;

#[derive(Debug, Args)]
pub struct FrontmatterArgs {
    /// Description of the frontmatter to generate
    #[arg(short, long)]
    pub description: String,

    /// Examples or requirements
    #[arg(short, long)]
    pub examples: Vec<String>,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,

    /// Use mock client for testing
    #[arg(long)]
    pub mock: bool,

    /// Model name to use
    #[arg(long)]
    pub model: Option<String>,

    /// Temperature for generation
    #[arg(long)]
    pub temperature: Option<f32>,

    /// Maximum tokens to generate
    #[arg(long)]
    pub max_tokens: Option<u32>,
}

pub async fn run(args: &FrontmatterArgs) -> Result<()> {
    // ============================================================================
    // RIG + MCP INTEGRATION FOR FRONTMATTER GENERATION
    // ============================================================================
    //
    // This file's LLM usage has been commented out for Rig+MCP integration.
    // Frontmatter generation would benefit from MCP tools for:
    // - YAML/TOML validation
    // - SEO optimization tools
    // - Metadata extraction from content
    // - Static site generator integration (Jekyll, Hugo)
    //
    // See generate.rs for full Rig+MCP integration pattern.
    // ============================================================================

    println!("📋 Generating frontmatter with AI...");
    println!("Description: {}", args.description);

    // ⚠️  COMMENTED OUT FOR RIG INTEGRATION
    // let global_config = ggen_ai::get_global_config();

    use ggen_ai::client::{GenAiClient, LlmClient};
    use ggen_ai::{LlmConfig, MockClient};
    use std::sync::Arc;

    // ⚠️  COMMENTED OUT FOR RIG INTEGRATION - LLM client creation
    /*
    let client: Arc<dyn LlmClient> = if args.mock {
        println!("ℹ️  Using mock client for testing");
        Arc::new(MockClient::with_response("Generated frontmatter content"))
    } else {
        println!("ℹ️  Using {} provider", global_config.provider_name());

        // Create client with proper configuration
        if let Some(model) = &args.model {
            // Use custom model if specified
            let llm_config = LlmConfig {
                model: model.clone(),
                max_tokens: args.max_tokens,
                temperature: args.temperature,
                top_p: Some(0.9),
                stop: None,
                extra: std::collections::HashMap::new(),
            };
            Arc::new(
                GenAiClient::new(llm_config)
                    .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?,
            )
        } else {
            // Use contextual client with auto-detection
            global_config
                .create_contextual_client()
                .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?
        }
    };

    let generator = TemplateGenerator::with_client(client);

    // Generate template with frontmatter
    let template = generator
        .generate_template(
            &args.description,
            args.examples.iter().map(|s| s.as_str()).collect(),
        )
        .await
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;
    */

    // TODO: Replace with Rig+MCP frontmatter generation
    return Err(ggen_utils::error::Error::new(
        "LLM integration disabled for Rig migration. See comments in frontmatter.rs",
    ));

    // ⚠️  COMMENTED OUT - Frontmatter output (unreachable after early return)
    /*
    println!("✅ Frontmatter generated successfully!");

    if let Some(output_path) = &args.output {
        // Write frontmatter in YAML format
        let frontmatter_yaml = serde_yaml::to_string(&template.front).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to serialize frontmatter: {}", e))
        })?;

        fs::write(
            output_path,
            format!("{}\n---\n{}", frontmatter_yaml, template.body),
        )?;
        println!("📁 Saved to: {}", output_path);
    } else {
        println!("📄 Generated frontmatter:");
        let frontmatter_yaml = serde_yaml::to_string(&template.front).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to serialize frontmatter: {}", e))
        })?;
        println!("{}", frontmatter_yaml);
        println!("---");
        println!("{}", template.body);
    }

    Ok(())
    */
}
