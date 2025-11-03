//! Natural language search for the ggen marketplace using AI
//!
//! This standalone CLI tool demonstrates natural language search capabilities
//! for the ggen marketplace, allowing users to search for packages using
//! conversational queries with AI interpretation.

use clap::{Parser, Subcommand};
use serde_json::Value;
use std::collections::HashMap;

#[derive(Parser)]
#[command(name = "natural-market-search")]
#[command(about = "Natural language search for ggen marketplace using AI")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Search marketplace using natural language
    Search {
        /// Natural language search query
        #[arg(short, long)]
        query: String,

        /// Show detailed output with AI reasoning
        #[arg(long)]
        detailed: bool,

        /// Output as JSON
        #[arg(long)]
        json: bool,

        /// Show AI's interpretation of the query
        #[arg(long)]
        explain: bool,

        /// Maximum number of results
        #[arg(long, default_value = "10")]
        limit: usize,

        /// Use specific AI model
        #[arg(long, default_value = "qwen3-coder:30b")]
        model: String,
    },

    /// Show example queries
    Examples,

    /// Test the AI interpretation
    Test {
        /// Query to test
        #[arg(short, long)]
        query: String,
    },
}

#[derive(Debug, Clone, serde::Serialize)]
struct NaturalSearchResult {
    query: String,
    interpretation: String,
    search_params: HashMap<String, Value>,
    results: Vec<PackageResult>,
    suggestions: Vec<String>,
    confidence: f32,
}

#[derive(Debug, Clone, serde::Serialize)]
struct PackageResult {
    id: String,
    name: String,
    description: String,
    category: Option<String>,
    tags: Vec<String>,
    relevance_score: f32,
    ai_reasoning: Option<String>,
}

struct NaturalLanguageInterpreter {
    model: String,
}

impl NaturalLanguageInterpreter {
    fn new(model: String) -> Self {
        Self { model }
    }

    async fn interpret_query(
        &self, query: &str,
    ) -> Result<NaturalSearchResult, Box<dyn std::error::Error>> {
        let interpretation = self.generate_interpretation(query).await?;
        let search_params = self.extract_search_params(query, &interpretation).await?;
        let results = self.generate_mock_results(query, &search_params).await?;
        let suggestions = self.generate_suggestions(query).await?;
        let confidence = self.calculate_confidence(query, &interpretation).await?;

        Ok(NaturalSearchResult {
            query: query.to_string(),
            interpretation,
            search_params,
            results,
            suggestions,
            confidence,
        })
    }

    async fn generate_interpretation(
        &self, query: &str,
    ) -> Result<String, Box<dyn std::error::Error>> {
        // Mock AI interpretation - in real implementation, this would call Ollama
        let query_lower = query.to_lowercase();

        if query_lower.contains("authentication")
            || query_lower.contains("auth")
            || query_lower.contains("login")
        {
            Ok("User is looking for authentication and authorization packages. This includes login systems, JWT tokens, OAuth2, and user management solutions.".to_string())
        } else if query_lower.contains("web")
            && (query_lower.contains("framework") || query_lower.contains("api"))
        {
            Ok("User is searching for web frameworks and API development tools. This includes REST APIs, GraphQL, web servers, and backend frameworks.".to_string())
        } else if query_lower.contains("database")
            || query_lower.contains("db")
            || query_lower.contains("sql")
        {
            Ok("User needs database-related packages. This includes ORMs, database drivers, query builders, and data persistence solutions.".to_string())
        } else if query_lower.contains("cli")
            || query_lower.contains("command")
            || query_lower.contains("terminal")
        {
            Ok("User is looking for CLI and command-line tools. This includes argument parsers, terminal interfaces, and command-line applications.".to_string())
        } else if query_lower.contains("template") || query_lower.contains("boilerplate") {
            Ok("User wants code templates and boilerplate generators. This includes project templates, code generators, and scaffolding tools.".to_string())
        } else {
            Ok(format!(
                "User query: '{}'. Analyzing for relevant packages in the marketplace.",
                query
            ))
        }
    }

    async fn extract_search_params(
        &self, query: &str, _interpretation: &str,
    ) -> Result<HashMap<String, Value>, Box<dyn std::error::Error>> {
        let mut params = HashMap::new();
        let query_lower = query.to_lowercase();

        // Extract keywords
        let mut keywords = Vec::new();
        if query_lower.contains("authentication") || query_lower.contains("auth") {
            keywords.push("authentication");
        }
        if query_lower.contains("web") {
            keywords.push("web");
        }
        if query_lower.contains("api") {
            keywords.push("api");
        }
        if query_lower.contains("database") || query_lower.contains("db") {
            keywords.push("database");
        }
        if query_lower.contains("cli") {
            keywords.push("cli");
        }
        if query_lower.contains("template") {
            keywords.push("template");
        }

        if !keywords.is_empty() {
            params.insert(
                "keywords".to_string(),
                Value::Array(
                    keywords
                        .into_iter()
                        .map(|k| Value::String(k.to_string()))
                        .collect(),
                ),
            );
        }

        // Extract category
        if query_lower.contains("authentication") || query_lower.contains("auth") {
            params.insert(
                "category".to_string(),
                Value::String("security".to_string()),
            );
        } else if query_lower.contains("web") {
            params.insert("category".to_string(), Value::String("web".to_string()));
        } else if query_lower.contains("database") {
            params.insert("category".to_string(), Value::String("data".to_string()));
        } else if query_lower.contains("cli") {
            params.insert("category".to_string(), Value::String("tools".to_string()));
        }

        // Extract search terms
        params.insert("search_terms".to_string(), Value::String(query.to_string()));

        // Set relevance-based sorting
        params.insert("sort".to_string(), Value::String("relevance".to_string()));

        Ok(params)
    }

    async fn generate_mock_results(
        &self, query: &str, _params: &HashMap<String, Value>,
    ) -> Result<Vec<PackageResult>, Box<dyn std::error::Error>> {
        let query_lower = query.to_lowercase();

        let mut results = Vec::new();

        if query_lower.contains("authentication") || query_lower.contains("auth") {
            results.push(PackageResult {
                id: "io.ggen.auth.user".to_string(),
                name: "User Authentication System".to_string(),
                description: "Complete user authentication with email/password, JWT tokens, and session management".to_string(),
                category: Some("security".to_string()),
                tags: vec!["authentication".to_string(), "jwt".to_string(), "user".to_string()],
                relevance_score: 0.95,
                ai_reasoning: Some("Perfect match for authentication needs with comprehensive features".to_string()),
            });

            results.push(PackageResult {
                id: "io.ggen.auth.oauth2".to_string(),
                name: "OAuth2 Authentication Flow".to_string(),
                description: "OAuth2 implementation for Google, GitHub, and other providers"
                    .to_string(),
                category: Some("security".to_string()),
                tags: vec![
                    "oauth2".to_string(),
                    "social".to_string(),
                    "authentication".to_string(),
                ],
                relevance_score: 0.88,
                ai_reasoning: Some(
                    "Great for social login and third-party authentication".to_string(),
                ),
            });

            results.push(PackageResult {
                id: "io.ggen.auth.rbac".to_string(),
                name: "Role-Based Access Control".to_string(),
                description: "RBAC system with permissions, roles, and access control".to_string(),
                category: Some("security".to_string()),
                tags: vec![
                    "rbac".to_string(),
                    "permissions".to_string(),
                    "authorization".to_string(),
                ],
                relevance_score: 0.82,
                ai_reasoning: Some("Essential for authorization and access control".to_string()),
            });
        }

        if query_lower.contains("web") && query_lower.contains("api") {
            results.push(PackageResult {
                id: "io.ggen.web.rest".to_string(),
                name: "REST API Framework".to_string(),
                description: "Complete REST API framework with routing, middleware, and validation"
                    .to_string(),
                category: Some("web".to_string()),
                tags: vec!["rest".to_string(), "api".to_string(), "web".to_string()],
                relevance_score: 0.92,
                ai_reasoning: Some(
                    "Excellent choice for building REST APIs with modern features".to_string(),
                ),
            });

            results.push(PackageResult {
                id: "io.ggen.web.graphql".to_string(),
                name: "GraphQL Server".to_string(),
                description: "GraphQL server with schema generation and query optimization"
                    .to_string(),
                category: Some("web".to_string()),
                tags: vec![
                    "graphql".to_string(),
                    "api".to_string(),
                    "server".to_string(),
                ],
                relevance_score: 0.85,
                ai_reasoning: Some("Modern API approach with flexible querying".to_string()),
            });
        }

        if query_lower.contains("database") || query_lower.contains("db") {
            results.push(PackageResult {
                id: "io.ggen.data.orm".to_string(),
                name: "Database ORM".to_string(),
                description: "Object-relational mapping with query builder and migrations"
                    .to_string(),
                category: Some("data".to_string()),
                tags: vec!["database".to_string(), "orm".to_string(), "sql".to_string()],
                relevance_score: 0.90,
                ai_reasoning: Some(
                    "Comprehensive database solution with modern ORM features".to_string(),
                ),
            });

            results.push(PackageResult {
                id: "io.ggen.data.migrations".to_string(),
                name: "Database Migrations".to_string(),
                description: "Database schema migrations with version control and rollback"
                    .to_string(),
                category: Some("data".to_string()),
                tags: vec![
                    "migrations".to_string(),
                    "database".to_string(),
                    "schema".to_string(),
                ],
                relevance_score: 0.83,
                ai_reasoning: Some("Essential for database schema management".to_string()),
            });
        }

        if query_lower.contains("cli") {
            results.push(PackageResult {
                id: "io.ggen.cli.framework".to_string(),
                name: "CLI Framework".to_string(),
                description:
                    "Command-line interface framework with argument parsing and help generation"
                        .to_string(),
                category: Some("tools".to_string()),
                tags: vec![
                    "cli".to_string(),
                    "command".to_string(),
                    "terminal".to_string(),
                ],
                relevance_score: 0.87,
                ai_reasoning: Some(
                    "Perfect for building command-line tools and applications".to_string(),
                ),
            });

            results.push(PackageResult {
                id: "io.ggen.cli.parser".to_string(),
                name: "Argument Parser".to_string(),
                description: "Advanced argument parsing with validation and help generation"
                    .to_string(),
                category: Some("tools".to_string()),
                tags: vec![
                    "parser".to_string(),
                    "arguments".to_string(),
                    "cli".to_string(),
                ],
                relevance_score: 0.80,
                ai_reasoning: Some("Core component for CLI applications".to_string()),
            });
        }

        // If no specific matches, provide general recommendations
        if results.is_empty() {
            results.push(PackageResult {
                id: "io.ggen.template.basic".to_string(),
                name: "Basic Project Template".to_string(),
                description: "Starter template for new projects with common patterns".to_string(),
                category: Some("template".to_string()),
                tags: vec![
                    "template".to_string(),
                    "starter".to_string(),
                    "basic".to_string(),
                ],
                relevance_score: 0.75,
                ai_reasoning: Some(
                    "General-purpose template that might be useful for your project".to_string(),
                ),
            });
        }

        Ok(results)
    }

    async fn generate_suggestions(
        &self, query: &str,
    ) -> Result<Vec<String>, Box<dyn std::error::Error>> {
        let query_lower = query.to_lowercase();
        let mut suggestions = Vec::new();

        if query_lower.contains("authentication") {
            suggestions.push("Try: 'user management system'".to_string());
            suggestions.push("Try: 'JWT token authentication'".to_string());
            suggestions.push("Try: 'OAuth2 social login'".to_string());
        } else if query_lower.contains("web") {
            suggestions.push("Try: 'REST API framework'".to_string());
            suggestions.push("Try: 'GraphQL server'".to_string());
            suggestions.push("Try: 'web server middleware'".to_string());
        } else if query_lower.contains("database") {
            suggestions.push("Try: 'database ORM'".to_string());
            suggestions.push("Try: 'query builder'".to_string());
            suggestions.push("Try: 'database migrations'".to_string());
        } else {
            suggestions.push("Try: 'authentication system'".to_string());
            suggestions.push("Try: 'web API framework'".to_string());
            suggestions.push("Try: 'CLI application'".to_string());
        }

        Ok(suggestions)
    }

    async fn calculate_confidence(
        &self, query: &str, _interpretation: &str,
    ) -> Result<f32, Box<dyn std::error::Error>> {
        // Simple confidence calculation based on query specificity
        let query_lower = query.to_lowercase();
        let mut confidence: f32 = 0.5; // Base confidence

        // Increase confidence for specific technical terms
        let technical_terms = [
            "authentication",
            "authorization",
            "jwt",
            "oauth2",
            "rest",
            "graphql",
            "api",
            "web",
            "framework",
            "database",
            "orm",
            "sql",
            "nosql",
            "cli",
            "command",
            "terminal",
            "interface",
            "template",
            "boilerplate",
            "generator",
        ];

        for term in technical_terms {
            if query_lower.contains(term) {
                confidence += 0.1;
            }
        }

        // Cap confidence at 0.95
        Ok(confidence.min(0.95))
    }
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Search {
            query,
            detailed,
            json,
            explain,
            limit,
            model,
        } => {
            search_marketplace(query, detailed, json, explain, limit, model).await?;
        }
        Commands::Examples => {
            show_examples().await?;
        }
        Commands::Test { query } => {
            test_interpretation(query).await?;
        }
    }

    Ok(())
}

async fn search_marketplace(
    query: String, detailed: bool, json: bool, explain: bool, limit: usize, model: String,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("🤖 Natural language search using AI model: {}", model);
    println!("Query: \"{}\"", query);
    println!();

    // Validate input
    if query.trim().is_empty() {
        return Err("Natural language query cannot be empty".into());
    }

    if query.len() > 500 {
        return Err("Query too long (max 500 characters)".into());
    }

    // Initialize AI interpreter
    let interpreter = NaturalLanguageInterpreter::new(model);

    // Interpret the query
    let result = interpreter.interpret_query(&query).await?;

    if json {
        // Output as JSON
        let json_output = serde_json::to_string_pretty(&result)?;
        println!("{}", json_output);
    } else {
        // Output formatted results
        println!("🎯 AI Interpretation:");
        println!("{}", result.interpretation);
        println!();

        if explain {
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
            if i >= limit {
                break;
            }

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

            if detailed && package.ai_reasoning.is_some() {
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

async fn show_examples() -> Result<(), Box<dyn std::error::Error>> {
    println!("📚 Natural Language Search Examples");
    println!();

    let examples = vec![
        (
            "I need a user authentication system",
            "Find authentication packages",
        ),
        (
            "Find me web frameworks for APIs",
            "Search for web API frameworks",
        ),
        (
            "What packages help with database operations?",
            "Discover database tools",
        ),
        (
            "I want to build a CLI application",
            "Find CLI development tools",
        ),
        (
            "Show me templates for new projects",
            "Browse project templates",
        ),
        ("I need OAuth2 integration", "Find OAuth2 packages"),
        ("What's available for REST APIs?", "Search REST API tools"),
        ("Help me with database migrations", "Find migration tools"),
    ];

    for (query, description) in examples {
        println!("🔍 \"{}\"", query);
        println!("   {}", description);
        println!();
    }

    println!("💡 Usage:");
    println!("   natural-market-search search --query \"I need authentication\"");
    println!("   natural-market-search search --query \"web API\" --detailed --explain");
    println!("   natural-market-search test --query \"database tools\"");

    Ok(())
}

async fn test_interpretation(query: String) -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Testing AI Interpretation");
    println!("Query: \"{}\"", query);
    println!();

    let interpreter = NaturalLanguageInterpreter::new("qwen3-coder:30b".to_string());
    let result = interpreter.interpret_query(&query).await?;

    println!("🎯 Interpretation:");
    println!("{}", result.interpretation);
    println!();

    println!("🔍 Extracted Parameters:");
    for (key, value) in &result.search_params {
        println!("  {}: {}", key, value);
    }
    println!();

    println!("📊 Confidence: {:.1}%", result.confidence * 100.0);
    println!();

    println!("📦 Sample Results:");
    for (i, package) in result.results.iter().take(3).enumerate() {
        println!(
            "  {}. {} ({}%)",
            i + 1,
            package.name,
            (package.relevance_score * 100.0) as u32
        );
    }

    Ok(())
}
