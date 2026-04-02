//! AI-Powered SPARQL Query Engine
//!
//! Demonstrates:
//! - Natural language to SPARQL conversion
//! - JSON to SPARQL conversion
//! - Query streaming
//! - Knowledge graph integration
//! - Multiple query types (SELECT, CONSTRUCT, ASK, DESCRIBE)

use clap::{Parser, Subcommand};
use ggen_ai::{GenAiClient, LlmClient, LlmConfig, SparqlGenerator};
use ggen_core::Graph;
use serde_json::{json, Value};
use std::path::PathBuf;
use std::sync::Arc;
use tracing::{info, error};

#[derive(Parser)]
#[command(name = "sparql-engine")]
#[command(about = "AI-powered SPARQL query engine", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,

    /// Graph file to query (Turtle format)
    #[arg(short, long)]
    graph: Option<PathBuf>,

    /// Enable verbose output
    #[arg(short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate SPARQL query from natural language
    Generate {
        /// Natural language description
        intent: String,

        /// Enable streaming output
        #[arg(short, long)]
        stream: bool,
    },

    /// Convert JSON to SPARQL query
    Json {
        /// JSON query specification
        json: String,

        /// Add common prefixes
        #[arg(short, long)]
        prefixes: bool,
    },

    /// Execute a SPARQL query
    Execute {
        /// SPARQL query string or file
        query: String,

        /// Output format (json, turtle, ntriples)
        #[arg(short, long, default_value = "json")]
        format: String,
    },

    /// Interactive REPL mode
    Repl,

    /// Load example data into graph
    LoadExamples,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Initialize logging
    let level = if cli.verbose { "debug" } else { "info" };
    tracing_subscriber::fmt()
        .with_env_filter(format!("sparql_engine={}", level))
        .init();

    // Load or create graph
    let mut graph = if let Some(path) = &cli.graph {
        info!("Loading graph from {:?}", path);
        load_graph(path)?
    } else {
        info!("Creating new empty graph");
        Graph::new()?
    };

    // Initialize AI client
    let config = LlmConfig::default();
    let client = Arc::new(GenAiClient::new(config)?) as Arc<dyn LlmClient>;
    let sparql_gen = SparqlGenerator::new(client);

    // Execute command
    match cli.command {
        Commands::Generate { intent, stream } => {
            if stream {
                generate_stream(&sparql_gen, &graph, &intent).await?;
            } else {
                generate_query(&sparql_gen, &graph, &intent).await?;
            }
        }
        Commands::Json { json, prefixes } => {
            convert_json(&json, prefixes)?;
        }
        Commands::Execute { query, format } => {
            execute_query(&graph, &query, &format)?;
        }
        Commands::Repl => {
            repl(&sparql_gen, &mut graph).await?;
        }
        Commands::LoadExamples => {
            load_examples(&mut graph)?;
        }
    }

    Ok(())
}

async fn generate_query(
    generator: &SparqlGenerator,
    graph: &Graph,
    intent: &str,
) -> anyhow::Result<()> {
    info!("Generating SPARQL query from: {}", intent);

    let query = generator.generate_query(graph, intent).await?;

    println!("Generated SPARQL Query:");
    println!("═══════════════════════");
    println!("{}", query);
    println!();

    Ok(())
}

async fn generate_stream(
    generator: &SparqlGenerator,
    graph: &Graph,
    intent: &str,
) -> anyhow::Result<()> {
    use futures_util::StreamExt;

    info!("Streaming SPARQL generation...");

    let prefixes = vec![
        ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
        ("foaf", "http://xmlns.com/foaf/0.1/"),
    ];

    let mut stream = generator.stream_sparql(graph, intent, &prefixes).await?;

    print!("Generated SPARQL Query: ");
    while let Some(chunk) = stream.next().await {
        match chunk {
            Ok(content) => print!("{}", content),
            Err(e) => error!("Stream error: {}", e),
        }
    }
    println!("\n");

    Ok(())
}

fn convert_json(json_str: &str, with_prefixes: bool) -> anyhow::Result<()> {
    let json: Value = serde_json::from_str(json_str)?;

    let prefixes = if with_prefixes {
        vec![
            ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
            ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
            ("foaf", "http://xmlns.com/foaf/0.1/"),
            ("schema", "http://schema.org/"),
        ]
    } else {
        vec![]
    };

    let query = SparqlGenerator::json_to_sparql(&json, &prefixes)?;

    println!("Converted SPARQL Query:");
    println!("════════════════════════");
    println!("{}", query);
    println!();

    Ok(())
}

fn execute_query(graph: &Graph, query_str: &str, format: &str) -> anyhow::Result<()> {
    info!("Executing SPARQL query");

    // Read query from file if it's a path
    let query = if std::path::Path::new(query_str).exists() {
        std::fs::read_to_string(query_str)?
    } else {
        query_str.to_string()
    };

    let results = graph.query(&query)?;

    match format {
        "json" => println!("{}", serde_json::to_string_pretty(&results)?),
        "turtle" | "ttl" => println!("{}", results), // Simplified
        _ => println!("{:?}", results),
    }

    Ok(())
}

async fn repl(generator: &SparqlGenerator, graph: &mut Graph) -> anyhow::Result<()> {
    use std::io::{self, Write};

    println!("SPARQL Engine REPL");
    println!("Commands: :query <intent>, :json <json>, :execute <query>, :load <file>, :quit");
    println!();

    loop {
        print!("> ");
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let input = input.trim();

        if input.is_empty() {
            continue;
        }

        match input.split_once(' ') {
            Some((":query", intent)) => {
                match generator.generate_query(graph, intent).await {
                    Ok(query) => println!("{}\n", query),
                    Err(e) => eprintln!("Error: {}\n", e),
                }
            }
            Some((":json", json)) => {
                match SparqlGenerator::json_to_sparql(&serde_json::from_str(json)?, &[]) {
                    Ok(query) => println!("{}\n", query),
                    Err(e) => eprintln!("Error: {}\n", e),
                }
            }
            Some((":execute", query)) => {
                match execute_query(graph, query, "json") {
                    Ok(_) => println!(),
                    Err(e) => eprintln!("Error: {}\n", e),
                }
            }
            Some((":load", path)) => {
                match load_graph(&PathBuf::from(path)) {
                    Ok(new_graph) => {
                        *graph = new_graph;
                        println!("Graph loaded\n");
                    }
                    Err(e) => eprintln!("Error: {}\n", e),
                }
            }
            Some((":quit", _)) | Some((":exit", _)) => break,
            _ => eprintln!("Unknown command\n"),
        }
    }

    Ok(())
}

fn load_graph(path: &PathBuf) -> anyhow::Result<Graph> {
    let content = std::fs::read_to_string(path)?;
    let mut graph = Graph::new()?;
    graph.parse(&content, "turtle")?;
    Ok(graph)
}

fn load_examples(graph: &mut Graph) -> anyhow::Result<()> {
    info!("Loading example data");

    let example_data = r#"
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix ex: <http://example.org/> .

        ex:alice a foaf:Person ;
            foaf:name "Alice Smith" ;
            foaf:age 30 ;
            foaf:knows ex:bob .

        ex:bob a foaf:Person ;
            foaf:name "Bob Jones" ;
            foaf:age 25 .
    "#;

    graph.parse(example_data, "turtle")?;
    println!("Example data loaded successfully\n");

    Ok(())
}
