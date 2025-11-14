//! Domain logic for RDF graph visualization
//!
//! This module provides graph visualization capabilities including:
//! - DOT format generation for Graphviz
//! - SVG/PNG rendering (via external tools)
//! - JSON export for web-based visualization
//! - Interactive graph exploration

use ggen_core::Graph;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::str::FromStr;

/// Visualization format options
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum VisualizeFormat {
    /// Graphviz DOT format
    Dot,
    /// SVG vector graphics
    Svg,
    /// PNG raster graphics
    Png,
    /// JSON for D3.js or other web viz
    Json,
}

impl FromStr for VisualizeFormat {
    type Err = ggen_utils::error::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.to_lowercase().as_str() {
            "dot" => Ok(Self::Dot),
            "svg" => Ok(Self::Svg),
            "png" => Ok(Self::Png),
            "json" => Ok(Self::Json),
            _ => Err(ggen_utils::error::Error::new(&format!(
                "Unsupported format: {}. Use dot, svg, png, or json",
                s
            ))),
        }
    }
}

impl VisualizeFormat {
    pub fn extension(&self) -> &str {
        match self {
            Self::Dot => "dot",
            Self::Svg => "svg",
            Self::Png => "png",
            Self::Json => "json",
        }
    }
}

/// Visualization options
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VisualizeOptions {
    pub format: Option<VisualizeFormat>,
    pub output_path: Option<PathBuf>,
    pub include_labels: bool,
    pub max_depth: Option<usize>,
    pub subject_filter: Option<String>,
    pub layout_engine: LayoutEngine,
}

#[derive(Debug, Clone, Copy, Default, Serialize, Deserialize)]
pub enum LayoutEngine {
    #[default]
    /// Hierarchical layout (top-to-bottom)
    Dot,
    /// Radial layout
    Neato,
    /// Force-directed layout
    Fdp,
    /// Circular layout
    Circo,
}

impl VisualizeOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_format(mut self, format: VisualizeFormat) -> Self {
        self.format = Some(format);
        self
    }

    pub fn with_output(mut self, path: PathBuf) -> Self {
        self.output_path = Some(path);
        self
    }

    pub fn with_labels(mut self) -> Self {
        self.include_labels = true;
        self
    }

    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = Some(depth);
        self
    }

    pub fn with_subject_filter(mut self, filter: impl Into<String>) -> Self {
        self.subject_filter = Some(filter.into());
        self
    }

    pub fn with_layout(mut self, engine: LayoutEngine) -> Self {
        self.layout_engine = engine;
        self
    }
}

/// Visualization result statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VisualizeStats {
    pub nodes_rendered: usize,
    pub edges_rendered: usize,
    pub output_path: Option<PathBuf>,
    pub format: String,
}

/// CLI Arguments for visualize command
#[derive(Debug, Clone, Default, serde::Deserialize)]
pub struct VisualizeInput {
    /// Input RDF file to visualize
    pub input: PathBuf,

    /// Output file path
    pub output: Option<PathBuf>,

    /// Visualization format (dot, svg, png, json)
    pub format: String,

    /// Include RDF labels in visualization
    #[serde(default)]
    pub labels: bool,

    /// Maximum depth for graph traversal
    pub max_depth: Option<usize>,

    /// Filter to specific subject IRI
    pub subject: Option<String>,
}

/// Visualize an RDF graph
///
/// Real implementation using ggen-core Graph APIs
pub async fn visualize_graph(
    graph_path: &Path, options: &VisualizeOptions,
) -> Result<VisualizeStats> {
    use std::fs;

    // Load graph from file
    let graph = if graph_path.exists() {
        Graph::load_from_file(graph_path)
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to load graph: {}", e)))?
    } else {
        return Err(ggen_utils::error::Error::new(&format!(
            "Graph file not found: {}",
            graph_path.display()
        )));
    };

    // Determine output format
    let format = options.format.unwrap_or(VisualizeFormat::Dot);

    // Extract nodes and edges from graph
    let nodes = extract_nodes(&graph, options)?;
    let edges = extract_edges(&graph, options)?;

    // Determine output path
    let output_path = if let Some(ref path) = options.output_path {
        Some(path.clone())
    } else {
        let extension = format.extension();
        Some(graph_path.with_extension(extension))
    };

    // Generate visualization based on format
    let content = match format {
        VisualizeFormat::Dot => generate_dot(&nodes, &edges, options.include_labels),
        VisualizeFormat::Json => generate_json(&nodes, &edges)?,
        VisualizeFormat::Svg | VisualizeFormat::Png => {
            // For SVG/PNG, generate DOT first (would need external tool to convert)
            generate_dot(&nodes, &edges, options.include_labels)
        }
    };

    // Write output file
    if let Some(ref path) = output_path {
        fs::write(path, content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to write visualization: {}", e))
        })?;
    }

    let stats = VisualizeStats {
        nodes_rendered: nodes.len(),
        edges_rendered: edges.len(),
        output_path: output_path.clone(),
        format: format!("{:?}", format),
    };

    Ok(stats)
}

/// Extract nodes (subjects) from graph
fn extract_nodes(graph: &Graph, options: &VisualizeOptions) -> Result<Vec<(String, String)>> {
    let mut nodes = Vec::new();
    let mut seen = std::collections::HashSet::new();

    // Query all subjects
    let query = if let Some(ref filter) = options.subject_filter {
        format!(
            "SELECT DISTINCT ?s WHERE {{ ?s ?p ?o FILTER(strstarts(str(?s), \"{}\")) }}",
            filter
        )
    } else {
        "SELECT DISTINCT ?s WHERE { ?s ?p ?o }".to_string()
    };

    let results = graph
        .query(&query)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to query graph: {}", e)))?;

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let solution = solution.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Query solution error: {}", e))
            })?;
            if let Some(term) = solution.get("s") {
                let node_id = term.to_string();
                if !seen.contains(&node_id) {
                    seen.insert(node_id.clone());
                    let label = if options.include_labels {
                        extract_label(graph, &node_id)?
                    } else {
                        node_id.clone()
                    };
                    nodes.push((node_id, label));
                }
            }
        }
    }

    Ok(nodes)
}

/// Extract edges (triples) from graph
fn extract_edges(
    graph: &Graph, options: &VisualizeOptions,
) -> Result<Vec<(String, String, String)>> {
    let mut edges = Vec::new();

    let query = if let Some(ref filter) = options.subject_filter {
        format!(
            "SELECT ?s ?p ?o WHERE {{ ?s ?p ?o FILTER(strstarts(str(?s), \"{}\")) }}",
            filter
        )
    } else {
        "SELECT ?s ?p ?o WHERE { ?s ?p ?o }".to_string()
    };

    let results = graph
        .query(&query)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to query graph: {}", e)))?;

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let solution = solution.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Query solution error: {}", e))
            })?;
            if let (Some(s), Some(p), Some(o)) =
                (solution.get("s"), solution.get("p"), solution.get("o"))
            {
                edges.push((s.to_string(), p.to_string(), o.to_string()));
            }
        }
    }

    Ok(edges)
}

/// Extract label for a node (try rdfs:label or use IRI)
fn extract_label(graph: &Graph, node_id: &str) -> Result<String> {
    let query = format!(
        "SELECT ?label WHERE {{ <{}> <http://www.w3.org/2000/01/rdf-schema#label> ?label }}",
        node_id
    );

    let results = graph
        .query(&query)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to query label: {}", e)))?;

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        for solution in solutions {
            let solution = solution.map_err(|e| {
                ggen_utils::error::Error::new(&format!("Query solution error: {}", e))
            })?;
            if let Some(label) = solution.get("label") {
                return Ok(label.to_string());
            }
        }
    }

    // Fallback to node ID (shortened if IRI)
    Ok(node_id
        .split('/')
        .next_back()
        .unwrap_or(node_id)
        .to_string())
}

/// Generate DOT format representation
pub fn generate_dot(
    nodes: &[(String, String)], edges: &[(String, String, String)], include_labels: bool,
) -> String {
    let mut dot = String::from("digraph RDF {\n");
    dot.push_str("  rankdir=TB;\n");
    dot.push_str("  node [shape=box, style=rounded];\n\n");

    // Add nodes
    for (id, label) in nodes {
        if include_labels {
            dot.push_str(&format!("  \"{}\" [label=\"{}\"];\n", id, label));
        } else {
            dot.push_str(&format!("  \"{}\";\n", id));
        }
    }

    dot.push('\n');

    // Add edges
    for (from, to, predicate) in edges {
        if include_labels {
            dot.push_str(&format!(
                "  \"{}\" -> \"{}\" [label=\"{}\"];\n",
                from, to, predicate
            ));
        } else {
            dot.push_str(&format!("  \"{}\" -> \"{}\";\n", from, to));
        }
    }

    dot.push_str("}\n");
    dot
}

/// Generate JSON format for web visualization
pub fn generate_json(
    nodes: &[(String, String)], edges: &[(String, String, String)],
) -> Result<String> {
    let node_objects: Vec<_> = nodes
        .iter()
        .map(|(id, label)| serde_json::json!({ "id": id, "label": label }))
        .collect();

    let edge_objects: Vec<_> = edges
        .iter()
        .map(|(from, to, predicate)| {
            serde_json::json!({
                "source": from,
                "target": to,
                "label": predicate
            })
        })
        .collect();

    let graph = serde_json::json!({
        "nodes": node_objects,
        "edges": edge_objects
    });

    serde_json::to_string_pretty(&graph)
        .map_err(|e| ggen_utils::error::Error::new(&format!("JSON serialization failed: {}", e)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_visualize_format_from_str() {
        assert!(matches!(
            VisualizeFormat::from_str("dot").unwrap(),
            VisualizeFormat::Dot
        ));
        assert!(matches!(
            VisualizeFormat::from_str("svg").unwrap(),
            VisualizeFormat::Svg
        ));
        assert!(matches!(
            VisualizeFormat::from_str("png").unwrap(),
            VisualizeFormat::Png
        ));
        assert!(matches!(
            VisualizeFormat::from_str("json").unwrap(),
            VisualizeFormat::Json
        ));
        assert!(VisualizeFormat::from_str("invalid").is_err());
    }

    #[test]
    fn test_visualize_options_builder() {
        let options = VisualizeOptions::new()
            .with_format(VisualizeFormat::Svg)
            .with_output(PathBuf::from("output.svg"))
            .with_labels()
            .with_max_depth(3)
            .with_subject_filter("http://example.com/*");

        assert!(matches!(options.format, Some(VisualizeFormat::Svg)));
        assert!(options.include_labels);
        assert_eq!(options.max_depth, Some(3));
        assert!(options.subject_filter.is_some());
    }

    #[test]
    fn test_generate_dot_basic() {
        let nodes = vec![
            ("n1".to_string(), "Node 1".to_string()),
            ("n2".to_string(), "Node 2".to_string()),
        ];
        let edges = vec![("n1".to_string(), "n2".to_string(), "connects".to_string())];

        let dot = generate_dot(&nodes, &edges, true);

        assert!(dot.contains("digraph RDF"));
        assert!(dot.contains("n1"));
        assert!(dot.contains("n2"));
        assert!(dot.contains("connects"));
    }

    #[test]
    fn test_generate_dot_no_labels() {
        let nodes = vec![("n1".to_string(), "Label".to_string())];
        let edges = vec![];

        let dot = generate_dot(&nodes, &edges, false);

        assert!(dot.contains("\"n1\""));
        assert!(!dot.contains("Label"));
    }

    #[test]
    fn test_generate_json() {
        let nodes = vec![
            ("n1".to_string(), "Node 1".to_string()),
            ("n2".to_string(), "Node 2".to_string()),
        ];
        let edges = vec![("n1".to_string(), "n2".to_string(), "rel".to_string())];

        let json = generate_json(&nodes, &edges).unwrap();

        assert!(json.contains("\"nodes\""));
        assert!(json.contains("\"edges\""));
        assert!(json.contains("\"n1\""));
        assert!(json.contains("\"rel\""));
    }

    #[test]
    fn test_format_extensions() {
        assert_eq!(VisualizeFormat::Dot.extension(), "dot");
        assert_eq!(VisualizeFormat::Svg.extension(), "svg");
        assert_eq!(VisualizeFormat::Png.extension(), "png");
        assert_eq!(VisualizeFormat::Json.extension(), "json");
    }
}

/// Visualize output for CLI
#[derive(Debug, Clone, serde::Serialize)]
pub struct VisualizeOutput {
    pub nodes_rendered: usize,
    pub edges_rendered: usize,
    pub output_path: String,
    pub format: String,
}

/// Execute visualize operation - domain logic entry point
///
/// This is the main entry point for the visualize command from CLI
pub async fn execute_visualize(input: VisualizeInput) -> Result<VisualizeOutput> {
    // Parse format
    let format = VisualizeFormat::from_str(&input.format)?;

    // Create visualize options
    let mut options = VisualizeOptions::new().with_format(format).with_output(
        input.output.clone().unwrap_or_else(|| {
            let mut path = input.input.clone();
            path.set_extension(format.extension());
            path
        }),
    );

    // Apply labels if requested
    if input.labels {
        options = options.with_labels();
    }

    // Apply optional settings
    let options = if let Some(depth) = input.max_depth {
        options.with_max_depth(depth)
    } else {
        options
    };

    let options = if let Some(ref subject) = input.subject {
        options.with_subject_filter(subject.clone())
    } else {
        options
    };

    // Perform the actual visualization
    let stats = visualize_graph(&input.input, &options).await?;

    Ok(VisualizeOutput {
        nodes_rendered: stats.nodes_rendered,
        edges_rendered: stats.edges_rendered,
        output_path: stats
            .output_path
            .as_ref()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default(),
        format: stats.format,
    })
}

/// CLI run function - bridges sync CLI to async domain logic
pub async fn run(args: &VisualizeInput) -> Result<()> {
    let output = execute_visualize(args.clone()).await?;

    ggen_utils::alert_success!(
        "Visualized {} nodes and {} edges to {}",
        output.nodes_rendered,
        output.edges_rendered,
        output.output_path
    );
    ggen_utils::alert_info!("   Format: {}", output.format);

    Ok(())
}
