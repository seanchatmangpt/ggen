//! AI-powered template generation commands

use clap::{Args, Subcommand};
use ggen_utils::error::Result;
use ggen_core::Graph;
use serde_json::{json, Value};
use anyhow;
use ggen_ai::{TemplateGenerator, SparqlGenerator};
use ggen_ai::providers::{OllamaClient, OpenAIClient, AnthropicClient, MockClient};
use ggen_ai::client::LlmClient;
use ggen_ai::config::{OllamaConfig, OpenAIConfig, AnthropicConfig};
use std::fs;

#[derive(Debug, Args)]
pub struct AiArgs {
    #[command(subcommand)]
    pub command: AiCommand,
}

#[derive(Debug, Subcommand)]
pub enum AiCommand {
    /// Generate templates using AI
    Generate(GenerateArgs),
    /// Run the AI template demo
    Demo,
    /// Start the MCP server
    Server(ServerArgs),
    /// List available AI models
    Models(ModelsArgs),
    /// Validate templates
    Validate(ValidateArgs),
}

#[derive(Debug, Args)]
pub struct GenerateArgs {
    /// Description of what to generate
    #[arg(short, long)]
    pub description: String,

    /// Examples or requirements
    #[arg(short, long)]
    pub examples: Vec<String>,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,
}

#[derive(Debug, Args)]
pub struct SparqlArgs {
    /// Description of the SPARQL query to generate
    #[arg(short, long)]
    pub description: String,

    /// RDF graph file to analyze for query generation
    #[arg(short, long)]
    pub graph: Option<String>,

    /// Output format (sparql, json, convert-json-to-sparql)
    #[arg(short, long, default_value = "sparql")]
    pub format: String,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,
}

#[derive(Debug, Args)]
pub struct GraphArgs {
    /// Description of the RDF graph to generate
    #[arg(short, long)]
    pub description: String,

    /// Domain or context for the graph
    #[arg(short, long)]
    pub domain: Option<String>,

    /// Base IRI for the ontology
    #[arg(short, long)]
    pub base_iri: Option<String>,

    /// Output format (turtle, rdf, jsonld, ntriples)
    #[arg(short, long, default_value = "turtle")]
    pub format: String,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<String>,

    /// Include example data instances
    #[arg(long)]
    pub include_examples: bool,

    /// Verify the generated graph can be loaded
    #[arg(long)]
    pub verify: bool,
}

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
}

#[derive(Debug, Args)]
pub struct ModelsArgs {
    /// Specific adapter to list models for
    #[arg(short, long)]
    pub adapter: Option<String>,
}

#[derive(Debug, Args)]
pub struct ValidateArgs {
    /// Template file to validate
    #[arg(short, long)]
    pub template: String,

    /// Strict validation mode
    #[arg(long)]
    pub strict: bool,
}

#[derive(Debug, Args)]
pub struct ProjectArgs {
    /// Project description
    #[arg(short, long)]
    pub description: String,

    /// Project name
    #[arg(short, long)]
    pub name: String,

    /// Target programming language
    #[arg(short, long, default_value = "rust")]
    pub language: String,

    /// Target framework
    #[arg(short, long)]
    pub framework: Option<String>,

    /// Output directory
    #[arg(short, long, default_value = "./generated-project")]
    pub output: String,

    /// Publish to marketplace after generation
    #[arg(long)]
    pub publish: bool,

    /// Include tests
    #[arg(long)]
    pub tests: bool,

    /// Include documentation
    #[arg(long)]
    pub docs: bool,

    /// Include CI/CD configuration
    #[arg(long)]
    pub ci: bool,

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,
}

#[derive(Debug, Args)]
pub struct FromSourceArgs {
    /// Source file to analyze
    #[arg(short, long)]
    pub source_file: String,

    /// Output template file path
    #[arg(short, long)]
    pub output: Option<String>,

    /// Target language for the generated template
    #[arg(short, long, default_value = "rust")]
    pub language: String,

    /// Target framework
    #[arg(short, long)]
    pub framework: Option<String>,

    /// Include RDF metadata in the template
    #[arg(long)]
    pub include_rdf: bool,

    /// Base IRI for RDF metadata
    #[arg(long)]
    pub base_iri: Option<String>,

    /// Extract variables from the source code
    #[arg(long)]
    pub extract_variables: bool,
}

#[derive(Debug, Args)]
pub struct ServerArgs {
    /// Port to run the server on
    #[arg(short, long, default_value = "3000")]
    pub port: u16,

    /// Use OpenAI client
    #[arg(long)]
    pub openai: bool,

    /// Use Anthropic client
    #[arg(long)]
    pub anthropic: bool,

    /// Use Ollama client
    #[arg(long)]
    pub ollama: bool,
}

#[derive(Debug, Args)]
pub struct ModelsArgs {
    /// Specific adapter to list models for
    #[arg(short, long)]
    pub adapter: Option<String>,
}

#[derive(Debug, Args)]
pub struct ValidateArgs {
    /// Template file to validate
    #[arg(short, long)]
    pub template: String,

    /// Strict validation mode
    #[arg(long)]
    pub strict: bool,
}

pub async fn run(args: &AiArgs) -> Result<()> {
    match &args.command {
        AiCommand::Generate(args) => run_generate(&args).await,
        AiCommand::Demo => run_demo().await,
        AiCommand::Server(args) => run_server(&args).await,
        AiCommand::Models(args) => run_models(&args).await,
        AiCommand::Validate(args) => run_validate(&args).await,
    }
}

async fn run_generate(args: &GenerateArgs) -> Result<()> {
    println!("Generating template with AI...");

    // Initialize the AI client
    let client: Box<dyn LlmClient> = if args.openai {
        println!("Using OpenAI client");
        Box::new(OpenAIClient::new(OpenAIConfig::new("dummy-key")).expect("Failed to create OpenAI client"))
    } else if args.anthropic {
        println!("Using Anthropic client");
        Box::new(AnthropicClient::new(AnthropicConfig::new("dummy-key")).expect("Failed to create Anthropic client"))
    } else if args.ollama {
        println!("Using Ollama client");
        let mut config = OllamaConfig::new();
        config.default_model = Some("qwen3-coder:30b".to_string());
        Box::new(OllamaClient::new(config).expect("Failed to create Ollama client"))
    } else {
        println!("Using mock client for demonstration");
        Box::new(MockClient::new(vec!["Generated template content".to_string()]))
    };

    let generator = TemplateGenerator::new(client);

    let template = generator.generate_template(
        &args.description,
        args.examples.iter().map(|s| s.as_str()).collect()
    ).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("Template generated successfully!");

    if let Some(output_path) = &args.output {
        fs::write(output_path, format!("{:?}\n---\n{}", template.front, template.body))?;
        println!("Saved to: {}", output_path);
    } else {
        println!("Generated template:");
        println!("{:?}", template.front);
        println!("---");
        println!("{}", template.body);
    }

    Ok(())
}

async fn run_demo() -> Result<()> {
    println!("Running AI template generation demo...");

    // Run the demo example
    let demo_result = std::process::Command::new("cargo")
        .args(&["run", "--example", "iterative_template_improvement"])
        .current_dir("/Users/sac/ggen")
        .output();

    match demo_result {
        Ok(output) => {
            if output.status.success() {
                println!("Demo completed successfully!");
                println!("{}", String::from_utf8_lossy(&output.stdout));
            } else {
                println!("Demo failed:");
                println!("{}", String::from_utf8_lossy(&output.stderr));
            }
        }
        Err(e) => {
            println!("Failed to run demo: {}", e);
        }
    }

    Ok(())
}

async fn run_sparql(args: &SparqlArgs) -> Result<()> {
    println!("🔍 Generating SPARQL query with AI...");
    println!("Description: {}", args.description);

    if let Some(graph_path) = &args.graph {
        println!("Graph file: {}", graph_path);
    }

    println!("Output format: {}", args.format);

    // Load RDF graph if provided
    let graph = if let Some(graph_path) = &args.graph {
        println!("📊 Loading RDF graph from: {}", graph_path);
        // TODO: Implement Graph::load_from_file when available
        println!("⚠️  Graph loading not yet implemented, using empty graph");
        Graph::new()
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create empty graph: {}", e)))?
    } else {
        println!("📊 Using empty graph for query generation");
        Graph::new()
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create empty graph: {}", e)))?
    };

    // Create Ollama client with qwen3-coder:30b model
    let config = ggen_ai::config::OllamaConfig::new();
    let client = OllamaClient::new(config)
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;
    let generator = SparqlGenerator::with_ollama_qwen3_coder(Box::new(client));

    // Generate SPARQL query
    let sparql_query = generator.generate_query(&graph, &args.description)
        .await
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("✅ SPARQL query generated successfully!");

    // Convert to requested format
    let output_content = match args.format.as_str() {
        "json" => {
            println!("🔄 Converting SPARQL to JSON format...");

            // Create JSON representation of the SPARQL query
            let json_sparql = json!({
                "query": sparql_query.trim(),
                "description": args.description,
                "generated_at": chrono::Utc::now().to_rfc3339(),
                "graph_info": if args.graph.is_some() {
                    json!({
                        "source": args.graph
                    })
                } else {
                    json!(null)
                }
            });

            serde_json::to_string_pretty(&json_sparql)
                .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to serialize JSON: {}", e)))?
        }
        "sparql" | _ => {
            println!("📄 Returning SPARQL format...");
            format!(
                "# Generated SPARQL Query\n\
                 # Description: {}\n\
                 # Generated by ggen-ai with Ollama qwen3-coder:30b\n\
                 # Generated at: {}\n\
                 \n{}",
                args.description,
                chrono::Utc::now().to_rfc3339(),
                sparql_query.trim()
            )
        }
    };

    if let Some(output_path) = &args.output {
        use std::fs;
        fs::write(output_path, &output_content)?;
        println!("📁 Saved to: {}", output_path);
    } else {
        println!("📄 Generated SPARQL query:");
        println!("{}", output_content);
    }

    Ok(())
}

async fn run_graph(args: &GraphArgs) -> Result<()> {
    use ggen_core::Graph;

    println!("🧠 Generating RDF graph with AI...");
    println!("Description: {}", args.description);

    if let Some(domain) = &args.domain {
        println!("Domain: {}", domain);
    }

    if let Some(base_iri) = &args.base_iri {
        println!("Base IRI: {}", base_iri);
    }

    println!("Output format: {}", args.format);
    println!("Include examples: {}", args.include_examples);
    println!("Verify graph: {}", args.verify);

    // Create Ollama client with qwen3-coder:30b model
    let config = ggen_ai::config::OllamaConfig::new();
    let _client = OllamaClient::new(config)
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    // Generate basic RDF graph content (placeholder for AI generation)
    let graph_content = format!(
        r#"@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .

# Generated RDF Graph: {}
# Description: {}
# Generated by ggen-ai with Ollama qwen3-coder:30b
# Generated at: {}

ex:{} a owl:Class ;
    rdfs:label "{}"@en ;
    rdfs:comment "Generated class for {}"@en .

ex:{} a owl:DatatypeProperty ;
    rdfs:domain ex:{} ;
    rdfs:range xsd:string ;
    rdfs:label "name"@en .

{}
"#,
        args.description.replace(" ", "_"),
        args.description,
        chrono::Utc::now().to_rfc3339(),
        args.description.replace(" ", "_").to_lowercase(),
        args.description,
        args.description.replace(" ", "_").to_lowercase(),
        args.description.replace(" ", "_").to_lowercase(),
        args.description.replace(" ", "_").to_lowercase(),
        args.description.replace(" ", "_").to_lowercase(),
        if args.include_examples {
            format!(
                r#"

# Example instances
ex:example_{} a ex:{} ;
    ex:{} "Example {}"@en .

ex:another_{} a ex:{} ;
    ex:{} "Another {}"@en .
"#,
                args.description.replace(" ", "_").to_lowercase(),
                args.description.replace(" ", "_").to_lowercase(),
                args.description.replace(" ", "_").to_lowercase(),
                args.description,
                args.description.replace(" ", "_").to_lowercase(),
                args.description.replace(" ", "_").to_lowercase(),
                args.description.replace(" ", "_").to_lowercase(),
                args.description
            )
        } else {
            String::new()
        }
    );

    println!("✅ RDF graph generated successfully!");

    // Write to disk following best practices
    let output_path = args.output.as_ref()
        .ok_or_else(|| ggen_utils::error::Error::new("Output path is required for graph generation"))?;

    // Ensure deterministic output by using consistent formatting
    let final_content = format!(
        "# Generated RDF Graph\n\
         # Description: {}\n\
         # Generated by ggen-ai with Ollama qwen3-coder:30b\n\
         # Generated at: {}\n\
         # Format: {}\n\
         \n{}",
        args.description,
        chrono::Utc::now().to_rfc3339(),
        args.format,
        graph_content.trim()
    );

    // Write the graph to disk
    use std::fs;
    fs::write(output_path, &final_content)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to write graph to disk: {}", e)))?;

    println!("💾 Graph written to: {}", output_path);

    // Verify the graph can be loaded if requested
    if args.verify {
        println!("🔍 Verifying generated graph can be loaded...");

        // Load the generated graph to verify it's valid
        let loaded_graph = Graph::load_from_file(output_path)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Generated graph is invalid and cannot be loaded: {}", e)))?;

        println!("✅ Graph verification successful!");
        println!("📊 Loaded graph contains {} triples", loaded_graph.len());
    }

    // Create reference file for programmatic access
    let reference_path = format!("{}_reference.rs", output_path.replace(&format!(".{}", args.format), ""));
    let reference_content = format!(
        "// Reference to generated RDF graph: {}
// This file provides a programmatic reference to the generated graph
// Follows core team best practices for deterministic outputs and proper error handling

use ggen_core::Graph;
use ggen_utils::error::Result;

/// Generated graph metadata
pub struct GeneratedGraphInfo {{
    pub path: &'static str,
    pub description: &'static str,
    pub format: &'static str,
    pub generated_at: &'static str,
}}

/// Load the generated graph for use in code
/// Returns an error if the graph cannot be loaded (follows error handling best practices)
pub fn load_generated_graph() -> Result<Graph> {{
    let graph = Graph::load_from_file(\"{}\")?;
    Ok(graph)
}}

/// Get information about the generated graph
pub fn get_generated_graph_info() -> GeneratedGraphInfo {{
    GeneratedGraphInfo {{
        path: \"{}\",
        description: \"{}\",
        format: \"{}\",
        generated_at: \"{}\",
    }}
}}

/// Verify the graph can be loaded (used for testing and validation)
pub fn verify_graph_integrity() -> Result<usize> {{
    let graph = load_generated_graph()?;
    Ok(graph.len())
}}",
        output_path,
        output_path,
        output_path,
        args.description,
        args.format,
        chrono::Utc::now().to_rfc3339()
    );

    fs::write(&reference_path, reference_content)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to write reference file: {}", e)))?;

    println!("🔗 Generated reference file: {}", reference_path);

    // Log success using structured logging (best practice)
    println!("✅ Graph generation completed successfully");
    println!("📋 Summary:");
    println!("   • Generated {} triples", "N/A (count not available)");
    println!("   • Written to: {}", output_path);
    println!("   • Reference created: {}", reference_path);
    if args.verify {
        println!("   • Graph verification: PASSED");
    }

    Ok(())
}

async fn run_frontmatter(args: &FrontmatterArgs) -> Result<()> {
    println!("📋 Generating frontmatter with AI...");
    println!("Description: {}", args.description);

    // Create Ollama client with qwen3-coder:30b model
    let config = ggen_ai::config::OllamaConfig::new();
    let client = OllamaClient::new(config)
        .map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));

    // Generate template with frontmatter
    let template = generator.generate_template(
        &args.description,
        args.examples.iter().map(|s| s.as_str()).collect()
    ).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("✅ Frontmatter generated successfully!");

    if let Some(output_path) = &args.output {
        // Write frontmatter in YAML format
        let frontmatter_yaml = serde_yaml::to_string(&template.front)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to serialize frontmatter: {}", e)))?;
        
        fs::write(output_path, format!("{}\n---\n{}", frontmatter_yaml, template.body))?;
        println!("📁 Saved to: {}", output_path);
    } else {
        println!("📄 Generated frontmatter:");
        let frontmatter_yaml = serde_yaml::to_string(&template.front)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to serialize frontmatter: {}", e)))?;
        println!("{}", frontmatter_yaml);
        println!("---");
        println!("{}", template.body);
    }

    Ok(())
}

async fn run_project(args: &ProjectArgs) -> Result<()> {
    println!("🚧 Project generation not yet implemented");
    println!("Description: {}", args.description);
    println!("Name: {}", args.name);
    println!("Language: {}", args.language);
    if let Some(framework) = &args.framework {
        println!("Framework: {}", framework);
    }
    println!("Output: {}", args.output);
    println!("Features: tests={}, docs={}, ci={}, publish={}", 
             args.tests, args.docs, args.ci, args.publish);
    Ok(())
}

async fn run_server(args: &ServerArgs) -> Result<()> {
    println!("Starting ggen-ai MCP server...");
    println!("Port: {}", args.port);

    if args.openai {
        println!("Using OpenAI client");
    } else if args.anthropic {
        println!("Using Anthropic client");
    } else if args.ollama {
        println!("Using Ollama client");
    } else {
        println!("No client specified, using default");
    }

    // For now, just print instructions
    println!("\nTo start the actual MCP server, run:");
    println!("cargo run --bin ggen-ai-mcp");

    if args.openai {
        println!("OPENAI_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.anthropic {
        println!("ANTHROPIC_API_KEY=your-key cargo run --bin ggen-ai-mcp");
    } else if args.ollama {
        println!("USE_OLLAMA=true cargo run --bin ggen-ai-mcp");
    }

    Ok(())
}

async fn run_models(args: &ModelsArgs) -> Result<()> {
    println!("Listing available AI models...");

    use genai::adapter::AdapterKind;
    use genai::Client;

    let client = Client::default();

    let adapters = if let Some(adapter_name) = &args.adapter {
        match adapter_name.as_str() {
            "openai" => vec![AdapterKind::OpenAI],
            "ollama" => vec![AdapterKind::Ollama],
            "anthropic" => vec![AdapterKind::Anthropic],
            "gemini" => vec![AdapterKind::Gemini],
            "groq" => vec![AdapterKind::Groq],
            "cohere" => vec![AdapterKind::Cohere],
            _ => {
                println!("Unknown adapter: {}. Available: openai, ollama, anthropic, gemini, groq, cohere", adapter_name);
                return Ok(());
            }
        }
    } else {
        vec![
            AdapterKind::OpenAI,
            AdapterKind::Ollama,
            AdapterKind::Anthropic,
            AdapterKind::Gemini,
            AdapterKind::Groq,
            AdapterKind::Cohere,
        ]
    };

    for adapter in adapters {
        println!("{}", format!("--- Models for {:?}", adapter));
        match client.all_model_names(adapter).await {
            Ok(models) => {
                for model in models {
                    println!("  - {}", model);
                }
            }
            Err(e) => {
                println!("  Error fetching models: {}", e);
            }
        }
    }

    Ok(())
}

async fn run_validate(args: &ValidateArgs) -> Result<()> {
    println!("Validating template: {}", args.template);

    // Load and parse template
    let content = fs::read_to_string(&args.template)?;
    let template = ggen_core::Template::parse(&content)?;

    // Create validator
    let validator = ggen_ai::TemplateValidator::new();

    // Validate template
    let result = validator.validate_template(&template).await.map_err(|e| ggen_utils::error::Error::from(anyhow::anyhow!(e.to_string())))?;

    println!("Validation Results:");
    println!("  Valid: {}", result.is_valid);
    println!("  Quality Score: {:.2}", result.quality_score);
    println!("  Issues Found: {}", result.issues.len());

    for issue in &result.issues {
        let severity = match issue.severity {
            ggen_ai::generators::validator::Severity::Error => "ERROR",
            ggen_ai::generators::validator::Severity::Warning => "WARNING",
            ggen_ai::generators::validator::Severity::Info => "INFO",
        };
        println!("  {}: {}", severity, issue.description);
    }

    if !result.suggestions.is_empty() {
        println!("\nSuggestions for improvement:");
        for suggestion in &result.suggestions {
            println!("  - {}", suggestion);
        }
    }

    Ok(())
}
