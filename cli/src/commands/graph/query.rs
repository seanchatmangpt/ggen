//! SPARQL query CLI command
//!
//! Delegates to domain layer for business logic

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct QueryArgs {
    /// SPARQL query to execute
    pub query: String,

    /// Output format (json, csv, table)
    #[arg(long, default_value = "table")]
    pub format: String,

    /// RDF graph file to query
    #[arg(long)]
    pub graph: Option<String>,
}

pub async fn run(args: &QueryArgs) -> Result<()> {
    println!("🔍 Executing SPARQL query...");

    // Delegate to domain layer
    let options = crate::domain::graph::QueryOptions {
        query: args.query.clone(),
        graph_file: args.graph.clone(),
        output_format: args.format.clone(),
    };

    let result = crate::domain::graph::execute_sparql(options)?;

    // Format and display results
    match args.format.as_str() {
        "json" => {
            let json = serde_json::to_string_pretty(&result.bindings)
                .map_err(|e| ggen_utils::error::Error::new(&format!("JSON serialization failed: {}", e)))?;
            println!("{}", json);
        }
        "csv" => {
            // Print CSV header
            println!("{}", result.variables.join(","));
            // Print rows
            for binding in &result.bindings {
                let row: Vec<String> = result
                    .variables
                    .iter()
                    .map(|var| binding.get(var).cloned().unwrap_or_default())
                    .collect();
                println!("{}", row.join(","));
            }
        }
        "table" => {
            // Print table header
            println!("{}", result.variables.join(" | "));
            println!("{}", "-".repeat(result.variables.len() * 20));
            // Print rows
            for binding in &result.bindings {
                let row: Vec<String> = result
                    .variables
                    .iter()
                    .map(|var| binding.get(var).cloned().unwrap_or_default())
                    .collect();
                println!("{}", row.join(" | "));
            }
        }
        _ => {
            return Err(ggen_utils::error::Error::new(&format!(
                "Unsupported output format: {}. Supported formats: json, csv, table",
                args.format
            )));
        }
    }

    println!("\n📊 {} results", result.result_count);
    Ok(())
}
