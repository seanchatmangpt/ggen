//! Graph export CLI command
//!
//! Delegates to domain layer for business logic

use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct ExportArgs {
    /// Output file path
    pub output: String,

    /// RDF format (turtle, ntriples, rdfxml, jsonld, n3)
    #[arg(long, default_value = "turtle")]
    pub format: String,

    /// Pretty print output
    #[arg(long)]
    pub pretty: bool,
}

pub async fn run(args: &ExportArgs) -> Result<()> {
    println!("🔍 Exporting graph...");

    // Parse export format
    let format = crate::domain::graph::ExportFormat::from_str(&args.format)?;

    // Delegate to domain layer
    let options = crate::domain::graph::ExportOptions {
        output_path: args.output.clone(),
        format,
        pretty: args.pretty,
        graph: None, // Use default/global graph
    };

    let content = crate::domain::graph::export_graph(options)?;

    // Get file size
    let file_size = content.len();

    println!(
        "✅ Exported graph to {} ({} bytes)",
        args.output,
        file_size
    );

    Ok(())
}
