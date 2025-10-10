use clap::Args;
use ggen_utils::error::Result;
use std::collections::HashMap;

#[derive(Args, Debug)]
pub struct StatsArgs {
    /// RDF graph file (uses current graph if not specified)
    #[arg(long)]
    pub graph: Option<String>,

    /// Show detailed statistics
    #[arg(long)]
    pub detailed: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait GraphAnalyzer {
    fn analyze(&self, graph: Option<String>) -> Result<GraphStats>;
}

#[derive(Debug, Clone)]
pub struct GraphStats {
    pub total_triples: usize,
    pub unique_subjects: usize,
    pub unique_predicates: usize,
    pub unique_objects: usize,
    pub namespaces: Vec<String>,
    pub predicate_counts: HashMap<String, usize>,
}

pub async fn run(args: &StatsArgs) -> Result<()> {
    println!("🚧 Placeholder: graph stats");
    println!("  Graph: {:?}", args.graph);
    println!("  Detailed: {}", args.detailed);
    Ok(())
}

pub async fn run_with_deps(args: &StatsArgs, analyzer: &dyn GraphAnalyzer) -> Result<()> {
    let stats = analyzer.analyze(args.graph.clone())?;

    println!("Graph Statistics:");
    println!("  Total triples: {}", stats.total_triples);
    println!("  Unique subjects: {}", stats.unique_subjects);
    println!("  Unique predicates: {}", stats.unique_predicates);
    println!("  Unique objects: {}", stats.unique_objects);

    if !stats.namespaces.is_empty() {
        println!("\nNamespaces:");
        for ns in &stats.namespaces {
            println!("  - {}", ns);
        }
    }

    if args.detailed && !stats.predicate_counts.is_empty() {
        println!("\nPredicate usage:");
        let mut counts: Vec<_> = stats.predicate_counts.iter().collect();
        counts.sort_by(|a, b| b.1.cmp(a.1));
        for (predicate, count) in counts {
            println!("  {} ({})", predicate, count);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_stats_displays_basic_info() {
        let mut mock_analyzer = MockGraphAnalyzer::new();
        mock_analyzer.expect_analyze().times(1).returning(|_| {
            Ok(GraphStats {
                total_triples: 100,
                unique_subjects: 25,
                unique_predicates: 10,
                unique_objects: 50,
                namespaces: vec![
                    "http://example.org/".to_string(),
                    "http://xmlns.com/foaf/0.1/".to_string(),
                ],
                predicate_counts: HashMap::new(),
            })
        });

        let args = StatsArgs {
            graph: None,
            detailed: false,
        };

        let result = run_with_deps(&args, &mock_analyzer).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_stats_detailed_mode() {
        let mut mock_analyzer = MockGraphAnalyzer::new();
        mock_analyzer.expect_analyze().times(1).returning(|_| {
            let mut predicate_counts = HashMap::new();
            predicate_counts.insert("rdf:type".to_string(), 25);
            predicate_counts.insert("foaf:name".to_string(), 20);
            predicate_counts.insert("ex:hasValue".to_string(), 15);

            Ok(GraphStats {
                total_triples: 60,
                unique_subjects: 20,
                unique_predicates: 3,
                unique_objects: 40,
                namespaces: vec!["http://example.org/".to_string()],
                predicate_counts,
            })
        });

        let args = StatsArgs {
            graph: Some("data.ttl".to_string()),
            detailed: true,
        };

        let result = run_with_deps(&args, &mock_analyzer).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_stats_empty_graph() {
        let mut mock_analyzer = MockGraphAnalyzer::new();
        mock_analyzer.expect_analyze().times(1).returning(|_| {
            Ok(GraphStats {
                total_triples: 0,
                unique_subjects: 0,
                unique_predicates: 0,
                unique_objects: 0,
                namespaces: vec![],
                predicate_counts: HashMap::new(),
            })
        });

        let args = StatsArgs {
            graph: None,
            detailed: false,
        };

        let result = run_with_deps(&args, &mock_analyzer).await;
        assert!(result.is_ok());
    }
}
