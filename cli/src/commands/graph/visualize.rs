//! Sync CLI wrapper for graph visualize command
//!
//! This module provides the synchronous CLI interface that bridges to the async
//! domain logic following the v2.0 architecture pattern.

use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;
use std::path::PathBuf;

/// Arguments for the graph visualize command
#[derive(Args, Debug, Clone)]
pub struct VisualizeArgs {
    /// RDF graph file to visualize
    pub graph_file: PathBuf,

    /// Output format (dot, svg, png, json)
    #[arg(short, long, default_value = "dot")]
    pub format: String,

    /// Output file path
    #[arg(short, long)]
    pub output: Option<PathBuf>,

    /// Include labels in visualization
    #[arg(short, long)]
    pub labels: bool,

    /// Maximum depth for graph traversal
    #[arg(short, long)]
    pub depth: Option<usize>,

    /// Filter by subject pattern
    #[arg(short, long)]
    pub subject: Option<String>,
}

/// Execute the graph visualize command (sync wrapper)
pub fn run(args: &VisualizeArgs) -> Result<()> {
    crate::runtime::execute(async {
        // Load graph
        println!("📊 Visualizing graph from {}...", args.graph_file.display());

        // Parse format
        let format = crate::domain::graph::VisualizeFormat::from_str(&args.format)?;

        // Build options
        let mut options = crate::domain::graph::VisualizeOptions::new()
            .with_format(format);

        if let Some(ref output) = args.output {
            options = options.with_output(output.clone());
        }

        if args.labels {
            options = options.with_labels();
        }

        if let Some(depth) = args.depth {
            options = options.with_max_depth(depth);
        }

        if let Some(ref subject) = args.subject {
            options = options.with_subject_filter(subject);
        }

        // Visualize graph
        let stats = crate::domain::graph::visualize_graph(&args.graph_file, &options).await?;

        println!("✅ Graph visualization complete");
        println!("   Nodes: {}", stats.nodes_rendered);
        println!("   Edges: {}", stats.edges_rendered);
        if let Some(output) = stats.output_path {
            println!("   Output: {}", output.display());
        }

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_visualize_args_defaults() {
        let args = VisualizeArgs {
            graph_file: PathBuf::from("graph.ttl"),
            format: "dot".to_string(),
            output: None,
            labels: false,
            depth: None,
            subject: None,
        };
        assert_eq!(args.format, "dot");
        assert!(!args.labels);
        assert!(args.output.is_none());
    }

    #[test]
    fn test_visualize_args_with_options() {
        let args = VisualizeArgs {
            graph_file: PathBuf::from("data.rdf"),
            format: "svg".to_string(),
            output: Some(PathBuf::from("output.svg")),
            labels: true,
            depth: Some(3),
            subject: Some("http://example.com/*".to_string()),
        };
        assert_eq!(args.format, "svg");
        assert!(args.labels);
        assert_eq!(args.depth, Some(3));
    }
}
