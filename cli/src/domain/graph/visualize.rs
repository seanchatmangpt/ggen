//! Domain logic for RDF graph visualization
//!
//! This module provides graph visualization capabilities including:
//! - DOT format generation for Graphviz
//! - SVG/PNG rendering (via external tools)
//! - JSON export for web-based visualization
//! - Interactive graph exploration

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

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

impl VisualizeFormat {
    pub fn from_str(s: &str) -> Result<Self> {
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

/// Visualize an RDF graph
///
/// This is a placeholder implementation for Phase 1.
/// Phase 2 will implement:
/// - Real Graphviz integration via dot command
/// - SVG/PNG rendering pipeline
/// - Interactive D3.js JSON export
/// - Graph layout algorithms
pub async fn visualize_graph(
    graph_path: &Path,
    options: &VisualizeOptions,
) -> Result<VisualizeStats> {
    // Placeholder: Validate inputs
    if !graph_path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "Graph file not found: {}",
            graph_path.display()
        )));
    }

    // Determine output format
    let format = options
        .format
        .unwrap_or(VisualizeFormat::Dot);

    // Generate visualization (placeholder)
    let output_path = if let Some(ref path) = options.output_path {
        Some(path.clone())
    } else {
        let extension = format.extension();
        Some(PathBuf::from(format!(
            "{}.{}",
            graph_path.file_stem().unwrap().to_str().unwrap(),
            extension
        )))
    };

    // Placeholder stats
    let stats = VisualizeStats {
        nodes_rendered: 42, // Placeholder
        edges_rendered: 73, // Placeholder
        output_path,
        format: format!("{:?}", format),
    };

    Ok(stats)
}

/// Generate DOT format representation
pub fn generate_dot(
    nodes: &[(String, String)],
    edges: &[(String, String, String)],
    include_labels: bool,
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

    dot.push_str("\n");

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
    nodes: &[(String, String)],
    edges: &[(String, String, String)],
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

    serde_json::to_string_pretty(&graph).map_err(|e| {
        ggen_utils::error::Error::new(&format!("JSON serialization failed: {}", e))
    })
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
