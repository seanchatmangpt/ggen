//! Natural language search functionality for the marketplace using AI.
//!
//! This module provides AI-powered natural language search capabilities for the ggen marketplace,
//! allowing users to search for packages using conversational queries. It uses the configured
//! LLM provider to understand user intent and convert natural language queries into
//! structured search parameters.
//!
//! # Examples
//!
//! ```bash
//! ggen market natural "I need a user authentication system"
//! ggen market natural "Find me web frameworks for APIs" --detailed
//! ggen market natural "What packages help with database operations?" --json
//! ```

use clap::Args;
use ggen_ai::config::GlobalLlmConfig;
use ggen_ai::generators::NaturalSearchGenerator;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct NaturalArgs {
    /// Natural language search query
    pub query: String,

    /// Show detailed output with AI reasoning
    #[arg(long)]
    pub detailed: bool,

    /// Output as JSON
    #[arg(long)]
    pub json: bool,

    /// Maximum number of results
    #[arg(long, default_value = "10")]
    pub limit: usize,

    /// Show AI's interpretation of the query
    #[arg(long)]
    pub explain: bool,

    /// Use specific AI model (defaults to environment or provider default)
    #[arg(long)]
    pub model: Option<String>,
}


pub async fn run(args: &NaturalArgs) -> Result<()> {
    // Get global LLM configuration
    let global_config = GlobalLlmConfig::new();

    let model_display = args.model.clone().unwrap_or_else(|| {
        global_config
            .get_default_config()
            .map(|c| c.model.clone())
            .unwrap_or_else(|| "default".to_string())
    });

    println!(
        "🤖 Natural language search using AI model: {}",
        model_display
    );
    println!("Query: \"{}\"", args.query);
    println!();

    // Validate input
    if args.query.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Natural language query cannot be empty",
        ));
    }

    if args.query.len() > 500 {
        return Err(ggen_utils::error::Error::new(
            "Query too long (max 500 characters)",
        ));
    }

    // Create LLM client using global config (not async)
    let client = global_config.create_client().map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create LLM client: {}", e))
    })?;

    // Initialize natural search generator with real LLM client
    let mut generator = NaturalSearchGenerator::new(client).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to initialize search generator: {}", e))
    })?;

    // Run search using real LLM
    let result = generator.search(&args.query).await.map_err(|e| {
        ggen_utils::error::Error::new(&format!("Search failed: {}", e))
    })?;

    if args.json {
        // Output as JSON
        let json_output = serde_json::to_string_pretty(&result)?;
        println!("{}", json_output);
    } else {
        // Output formatted results
        println!("🎯 AI Interpretation:");
        println!("{}", result.interpretation);
        println!();

        if args.explain {
            println!("🔍 Search Parameters:");
            for (key, value) in &result.search_params {
                println!("  {}: {}", key, value);
            }
            println!();
        }

        println!(
            "📦 Found {} relevant packages (confidence: {:.1}%)",
            result.results.len(),
            result.confidence * 100.0
        );
        println!();

        for (i, package) in result.results.iter().enumerate() {
            println!(
                "{}. 📦 {} (⭐ {:.0}%)",
                i + 1,
                package.name,
                package.relevance_score * 100.0
            );
            println!("   ID: {}", package.id);
            println!("   Description: {}", package.description);

            if let Some(category) = &package.category {
                println!("   Category: {}", category);
            }

            if !package.tags.is_empty() {
                println!("   Tags: {}", package.tags.join(", "));
            }

            if args.detailed && package.ai_reasoning.is_some() {
                println!(
                    "   AI Reasoning: {}",
                    package.ai_reasoning.as_ref().unwrap()
                );
            }

            println!();
        }

        if !result.suggestions.is_empty() {
            println!("💡 Try these related searches:");
            for suggestion in &result.suggestions {
                println!("   • {}", suggestion);
            }
            println!();
        }

        println!("🔧 To use a package:");
        if !result.results.is_empty() {
            println!("   ggen market add \"{}\"", result.results[0].id);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_ai::providers::adapter::MockClient;

    #[tokio::test]
    async fn test_natural_search_with_mock() {
        // Create mock response matching expected format
        let mock_response = r#"```json
{
  "interpretation": "User is looking for authentication packages",
  "keywords": ["authentication", "auth", "user"],
  "category": "security",
  "confidence": 0.9,
  "packages": [
    {
      "id": "io.ggen.auth.user",
      "name": "User Authentication System",
      "description": "Complete user authentication with JWT",
      "category": "security",
      "tags": ["authentication", "jwt", "user"],
      "relevance_score": 0.95,
      "ai_reasoning": "Perfect match for authentication needs"
    }
  ],
  "suggestions": ["Try: 'OAuth2 authentication'"]
}
```"#;

        let client = MockClient::with_response(mock_response);
        let mut generator = NaturalSearchGenerator::new(std::sync::Arc::new(client)).unwrap();

        let result = generator.search("I need user authentication").await.unwrap();

        assert_eq!(result.query, "I need user authentication");
        assert!(result.interpretation.contains("authentication"));
        assert!(!result.results.is_empty());
        assert!(result.confidence > 0.0);
    }
}
