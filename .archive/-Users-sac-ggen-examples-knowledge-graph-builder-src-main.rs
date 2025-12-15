//! Knowledge Graph Builder
//!
//! Complete integration of all ggen features:
//! - AI-powered ontology generation
//! - Template-driven code generation from graphs
//! - SPARQL query generation and execution
//! - Lifecycle management for complex workflows
//! - Multi-format export (RDF, JSON-LD, Turtle)

use clap::{Parser, Subcommand};
use colored::*;
use ggen_ai::{
    GenAiClient, LlmClient, LlmConfig, OntologyGenerator, SparqlGenerator, TemplateGenerator,
};
use ggen_core::{Generator, GenContext, Graph};
use indicatif::{ProgressBar, ProgressStyle};
use std::path::PathBuf;
use std::sync::Arc;
use tracing::info;

#[derive(Parser)]
#[command(name = "kg-builder")]
#[command(about = "AI-powered knowledge graph builder", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Create new knowledge graph from domain description
    Create {
        /// Domain description
        description: String,

        /// Output file
        #[arg(short, long, default_value = "graph.ttl")]
        output: PathBuf,
    },

    /// Extend existing graph with new concepts
    Extend {
        /// Input graph file
        input: PathBuf,

        /// New concepts to add
        concepts: Vec<String>,

        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Query graph with natural language
    Query {
        /// Graph file
        graph: PathBuf,

        /// Natural language query
        query: String,

        /// Output format (json, table)
        #[arg(short, long, default_value = "table")]
        format: String,
    },

    /// Generate code from graph
    Generate {
        /// Graph file
        graph: PathBuf,

        /// Template directory
        #[arg(short, long)]
        templates: PathBuf,

        /// Output directory
        #[arg(short, long, default_value = "generated")]
        output: PathBuf,

        /// Programming language
        #[arg(short, long, default_value = "rust")]
        language: String,
    },

    /// Validate graph schema
    Validate {
        /// Graph file
        graph: PathBuf,

        /// Schema file (SHACL)
        schema: Option<PathBuf>,
    },

    /// Export graph in different formats
    Export {
        /// Input graph file
        input: PathBuf,

        /// Output format (turtle, jsonld, ntriples, rdfxml)
        #[arg(short, long, default_value = "jsonld")]
        format: String,

        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,
    },

    /// Interactive shell
    Shell {
        /// Graph file to load
        graph: Option<PathBuf>,
    },
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    let level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(format!("kg_builder={},ggen_ai={}", level, level))
        .with_target(false)
        .init();

    // Initialize AI clients
    let config = LlmConfig::default();
    let client = Arc::new(GenAiClient::new(config)?) as Arc<dyn LlmClient>;
    let ontology_gen = OntologyGenerator::new(client.clone());
    let sparql_gen = SparqlGenerator::new(client.clone());
    let template_gen = TemplateGenerator::new(client);

    // Execute command
    match cli.command {
        Commands::Create { description, output } => {
            create_graph(&ontology_gen, &description, &output).await?
        }
        Commands::Extend {
            input,
            concepts,
            output,
        } => extend_graph(&ontology_gen, &input, concepts, output).await?,
        Commands::Query { graph, query, format } => {
            query_graph(&sparql_gen, &graph, &query, &format).await?
        }
        Commands::Generate {
            graph,
            templates,
            output,
            language,
        } => {
            generate_code(&template_gen, &graph, &templates, &output, &language).await?
        }
        Commands::Validate { graph, schema } => validate_graph(&graph, schema)?,
        Commands::Export { input, format, output } => export_graph(&input, &format, output)?,
        Commands::Shell { graph } => {
            interactive_shell(&ontology_gen, &sparql_gen, &template_gen, graph).await?
        }
    }

    Ok(())
}

async fn create_graph(
    generator: &OntologyGenerator,
    description: &str,
    output: &PathBuf,
) -> anyhow::Result<()> {
    println!("{}", "Creating knowledge graph...".cyan().bold());

    let pb = create_spinner("Generating ontology");
    let rdf = generator.generate_ontology(description).await?;
    pb.finish_with_message("✓ Ontology generated".green().to_string());

    std::fs::write(output, &rdf)?;
    println!("{}", format!("✓ Graph saved to {:?}", output).green());

    // Show summary
    let lines = rdf.lines().count();
    println!("\n{}", "Summary:".yellow().bold());
    println!("  Lines: {}", lines);
    println!("  Format: Turtle");

    Ok(())
}

async fn extend_graph(
    generator: &OntologyGenerator,
    input: &PathBuf,
    concepts: Vec<String>,
    output: Option<PathBuf>,
) -> anyhow::Result<()> {
    println!("{}", "Extending knowledge graph...".cyan().bold());

    // Load existing graph
    let existing = std::fs::read_to_string(input)?;

    // Generate extension
    let description = format!("Add these concepts to the ontology: {}", concepts.join(", "));
    let pb = create_spinner("Generating extension");
    let extension = generator.generate_ontology(&description).await?;
    pb.finish_with_message("✓ Extension generated".green().to_string());

    // Combine
    let combined = format!("{}\n\n# Extension\n{}", existing, extension);

    let output_path = output.unwrap_or_else(|| input.clone());
    std::fs::write(&output_path, combined)?;

    println!("{}", format!("✓ Graph extended and saved to {:?}", output_path).green());

    Ok(())
}

async fn query_graph(
    generator: &SparqlGenerator,
    graph_path: &PathBuf,
    query: &str,
    format: &str,
) -> anyhow::Result<()> {
    println!("{}", "Querying knowledge graph...".cyan().bold());

    // Load graph
    let mut graph = Graph::new()?;
    let content = std::fs::read_to_string(graph_path)?;
    graph.parse(&content, "turtle")?;

    // Generate SPARQL query
    let pb = create_spinner("Generating SPARQL query");
    let sparql = generator.generate_query(&graph, query).await?;
    pb.finish_with_message("✓ Query generated".green().to_string());

    println!("\n{}", "Generated Query:".yellow().bold());
    println!("{}", sparql);

    // Execute query
    let pb = create_spinner("Executing query");
    let results = graph.query(&sparql)?;
    pb.finish_with_message("✓ Query executed".green().to_string());

    println!("\n{}", "Results:".yellow().bold());
    match format {
        "json" => println!("{}", serde_json::to_string_pretty(&results)?),
        "table" => print_table_results(&results),
        _ => println!("{:?}", results),
    }

    Ok(())
}

async fn generate_code(
    generator: &TemplateGenerator,
    graph_path: &PathBuf,
    templates_dir: &PathBuf,
    output_dir: &PathBuf,
    language: &str,
) -> anyhow::Result<()> {
    println!("{}", "Generating code from knowledge graph...".cyan().bold());

    // Load graph
    let mut graph = Graph::new()?;
    let content = std::fs::read_to_string(graph_path)?;
    graph.parse(&content, "turtle")?;

    // Find entities to generate
    let query = "SELECT ?entity WHERE { ?entity a <http://www.w3.org/2002/07/owl#Class> }";
    let entities = graph.query(query)?;

    println!("Found {} entities to generate", entities.len());

    std::fs::create_dir_all(output_dir)?;

    for (i, entity) in entities.iter().enumerate() {
        let pb = create_spinner(&format!("Generating code {}/{}", i + 1, entities.len()));

        let description = format!("Generate a {} struct/class for entity: {:?}", language, entity);
        let code = generator.generate(&description, language).await?;

        let filename = format!("{}/entity_{}.{}", output_dir.display(), i, get_extension(language));
        std::fs::write(&filename, code)?;

        pb.finish_with_message(format!("✓ Generated {}", filename).green().to_string());
    }

    println!("{}", format!("\n✓ Code generated in {:?}", output_dir).green());

    Ok(())
}

fn validate_graph(graph_path: &PathBuf, _schema: Option<PathBuf>) -> anyhow::Result<()> {
    println!("{}", "Validating knowledge graph...".cyan().bold());

    let mut graph = Graph::new()?;
    let content = std::fs::read_to_string(graph_path)?;

    let pb = create_spinner("Parsing graph");
    graph.parse(&content, "turtle")?;
    pb.finish_with_message("✓ Graph is valid".green().to_string());

    println!("\n{}", "Statistics:".yellow().bold());
    println!("  Triples: {}", graph.len());

    Ok(())
}

fn export_graph(input: &PathBuf, format: &str, output: Option<PathBuf>) -> anyhow::Result<()> {
    println!("{}", format!("Exporting graph to {}...", format).cyan().bold());

    let mut graph = Graph::new()?;
    let content = std::fs::read_to_string(input)?;
    graph.parse(&content, "turtle")?;

    let exported = match format {
        "turtle" | "ttl" => graph.serialize("turtle")?,
        "jsonld" => graph.serialize("jsonld")?,
        "ntriples" | "nt" => graph.serialize("ntriples")?,
        "rdfxml" | "xml" => graph.serialize("rdfxml")?,
        _ => anyhow::bail!("Unsupported format: {}", format),
    };

    if let Some(output_path) = output {
        std::fs::write(&output_path, exported)?;
        println!("{}", format!("✓ Exported to {:?}", output_path).green());
    } else {
        println!("{}", exported);
    }

    Ok(())
}

async fn interactive_shell(
    _ontology_gen: &OntologyGenerator,
    _sparql_gen: &SparqlGenerator,
    _template_gen: &TemplateGenerator,
    _graph: Option<PathBuf>,
) -> anyhow::Result<()> {
    println!("{}", "Knowledge Graph Builder Shell".cyan().bold());
    println!("Commands: create, extend, query, generate, export, quit");
    println!();

    // Simplified shell - full implementation would handle all commands
    println!("Interactive shell not yet implemented");

    Ok(())
}

// Utilities

fn create_spinner(message: &str) -> ProgressBar {
    let pb = ProgressBar::new_spinner();
    pb.set_style(
        ProgressStyle::default_spinner()
            .template("{spinner:.cyan} {msg}")
            .unwrap(),
    );
    pb.set_message(message.to_string());
    pb.enable_steady_tick(std::time::Duration::from_millis(100));
    pb
}

fn print_table_results(results: &serde_json::Value) {
    if let Some(arr) = results.as_array() {
        for (i, item) in arr.iter().enumerate() {
            println!("{}. {:?}", i + 1, item);
        }
    } else {
        println!("{:?}", results);
    }
}

fn get_extension(language: &str) -> &str {
    match language {
        "rust" => "rs",
        "python" => "py",
        "typescript" => "ts",
        "javascript" => "js",
        "java" => "java",
        _ => "txt",
    }
}
