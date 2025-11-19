//! SPARQL query path risk analysis
//!
//! Analyzes SPARQL queries for:
//! - Query complexity and performance risks
//! - Dependency on schema elements
//! - Potential failure points
//! - SOD scoring for query execution paths

use super::scoring::{
    ChangeFrequency, DetectionMetrics, ImpactMetrics, OccurrenceMetrics, SodScorer, UserImpact,
};
use super::types::{
    FailureMode, FailureModeCategory, ImpactTarget, MitigationCost, MitigationStatus,
    MitigationStrategy,
};
use crate::graph::Graph;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// SPARQL query path analyzer
pub struct QueryPathAnalyzer {
    scorer: SodScorer,
}

impl QueryPathAnalyzer {
    /// Create a new query path analyzer
    pub fn new() -> Self {
        Self {
            scorer: SodScorer::new(),
        }
    }

    /// Analyze a SPARQL query for risks
    pub fn analyze_query(&self, query: &str, graph: &Graph) -> Result<QueryRisk> {
        let path = self.parse_query_path(query)?;
        let dependencies = self.extract_dependencies(&path);
        let complexity = self.calculate_complexity(&path);

        // Calculate SOD scores
        let impact = ImpactMetrics {
            breaking_change: false,
            affected_systems: 1,
            user_impact: if complexity > 0.7 {
                UserImpact::Significant
            } else if complexity > 0.4 {
                UserImpact::Moderate
            } else {
                UserImpact::Minimal
            },
            ..Default::default()
        };

        let occurrence = OccurrenceMetrics {
            historical_failures: 0,
            complexity_score: complexity,
            change_frequency: ChangeFrequency::Occasional,
        };

        let detection = DetectionMetrics {
            test_coverage: 0.5,
            has_static_analysis: true,
            has_runtime_monitoring: true,
            requires_manual_review: complexity > 0.7,
        };

        let sod = self.scorer.calculate_sod(&impact, &occurrence, &detection);

        Ok(QueryRisk {
            query: query.to_string(),
            path,
            dependencies,
            complexity_score: complexity,
            severity: sod.severity,
            occurrence: sod.occurrence,
            detection: sod.detection,
            rpn: sod.rpn,
            risks: self.identify_risks(&path, complexity),
        })
    }

    /// Parse a SPARQL query into a query path
    fn parse_query_path(&self, query: &str) -> Result<QueryPath> {
        // Simplified parsing - in production, use a proper SPARQL parser
        let mut path = QueryPath {
            query_type: self.detect_query_type(query),
            triple_patterns: Vec::new(),
            filters: Vec::new(),
            optional_patterns: Vec::new(),
            union_branches: 0,
            subqueries: 0,
            aggregations: Vec::new(),
        };

        // Count OPTIONAL clauses
        path.optional_patterns = query.matches("OPTIONAL").count();

        // Count UNION clauses
        path.union_branches = query.matches("UNION").count();

        // Count subqueries
        path.subqueries = query.matches("SELECT").count().saturating_sub(1);

        // Detect aggregations
        for agg in &["COUNT", "SUM", "AVG", "MIN", "MAX", "GROUP_CONCAT"] {
            if query.contains(agg) {
                path.aggregations.push(agg.to_string());
            }
        }

        // Extract filters (simplified)
        path.filters = query.matches("FILTER").count();

        Ok(path)
    }

    /// Detect query type
    fn detect_query_type(&self, query: &str) -> QueryType {
        if query.contains("SELECT") {
            QueryType::Select
        } else if query.contains("CONSTRUCT") {
            QueryType::Construct
        } else if query.contains("ASK") {
            QueryType::Ask
        } else if query.contains("DESCRIBE") {
            QueryType::Describe
        } else {
            QueryType::Unknown
        }
    }

    /// Extract schema dependencies from query path
    fn extract_dependencies(&self, path: &QueryPath) -> Vec<QueryDependency> {
        let mut deps = Vec::new();

        // Add dependencies based on patterns
        deps.push(QueryDependency {
            dependency_type: DependencyType::Schema,
            element: "schema_element".to_string(),
            criticality: if path.optional_patterns > 0 {
                DependencyCriticality::Low
            } else {
                DependencyCriticality::High
            },
        });

        deps
    }

    /// Calculate query complexity score (0.0-1.0)
    fn calculate_complexity(&self, path: &QueryPath) -> f64 {
        let mut score = 0.0;

        // Base complexity from query type
        score += match path.query_type {
            QueryType::Select => 0.1,
            QueryType::Construct => 0.2,
            QueryType::Ask => 0.05,
            QueryType::Describe => 0.15,
            QueryType::Unknown => 0.3,
        };

        // Optional patterns add complexity
        score += (path.optional_patterns as f64) * 0.1;

        // Union branches add significant complexity
        score += (path.union_branches as f64) * 0.15;

        // Subqueries are expensive
        score += (path.subqueries as f64) * 0.2;

        // Aggregations add moderate complexity
        score += (path.aggregations.len() as f64) * 0.1;

        // Filters add minor complexity
        score += (path.filters as f64) * 0.05;

        // Cap at 1.0
        score.min(1.0)
    }

    /// Identify specific risks in the query
    fn identify_risks(&self, path: &QueryPath, complexity: f64) -> Vec<String> {
        let mut risks = Vec::new();

        if complexity > 0.7 {
            risks.push("High query complexity may impact performance".to_string());
        }

        if path.subqueries > 2 {
            risks.push("Multiple subqueries may cause timeout".to_string());
        }

        if path.union_branches > 3 {
            risks.push("Excessive UNION branches increase execution time".to_string());
        }

        if path.optional_patterns > 5 {
            risks.push("Many OPTIONAL clauses can lead to cartesian products".to_string());
        }

        if path.aggregations.len() > 3 {
            risks.push("Multiple aggregations may be inefficient".to_string());
        }

        risks
    }

    /// Convert query risks to failure modes
    pub fn query_risk_to_failure_mode(&self, risk: &QueryRisk) -> FailureMode {
        let mut fm = FailureMode::new(
            format!("QUERY-{}", risk.query.chars().take(10).collect::<String>()),
            FailureModeCategory::QueryExecution,
            format!("Query execution risk (complexity: {:.2})", risk.complexity_score),
            risk.severity,
            risk.occurrence,
            risk.detection,
        );

        // Add identified risks as effects
        for r in &risk.risks {
            fm = fm.add_effect(r.clone());
        }

        // Add mitigations
        if risk.complexity_score > 0.7 {
            fm = fm.add_mitigation(MitigationStrategy {
                description: "Optimize query by reducing subqueries and UNION clauses".to_string(),
                impact_on: ImpactTarget::Severity,
                reduction: 3,
                status: MitigationStatus::Planned,
                cost: MitigationCost::Medium,
            });
        }

        fm = fm.add_mitigation(MitigationStrategy {
            description: "Add query timeout and resource limits".to_string(),
            impact_on: ImpactTarget::Detection,
            reduction: 2,
            status: MitigationStatus::Planned,
            cost: MitigationCost::Low,
        });

        fm = fm.add_mitigation(MitigationStrategy {
            description: "Implement query result caching".to_string(),
            impact_on: ImpactTarget::Occurrence,
            reduction: 4,
            status: MitigationStatus::Planned,
            cost: MitigationCost::Medium,
        });

        fm
    }

    /// Analyze query dependencies on schema
    pub fn analyze_schema_dependencies(
        &self,
        query: &str,
        _graph: &Graph,
    ) -> Result<Vec<QueryDependency>> {
        let mut dependencies = Vec::new();

        // Extract prefixes
        for line in query.lines() {
            if line.trim().starts_with("PREFIX") {
                if let Some(prefix_name) = line.split_whitespace().nth(1) {
                    dependencies.push(QueryDependency {
                        dependency_type: DependencyType::Namespace,
                        element: prefix_name.trim_end_matches(':').to_string(),
                        criticality: DependencyCriticality::High,
                    });
                }
            }
        }

        Ok(dependencies)
    }
}

impl Default for QueryPathAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Query path representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryPath {
    /// Type of query
    pub query_type: QueryType,

    /// Triple patterns in the query
    pub triple_patterns: Vec<String>,

    /// Number of FILTER clauses
    pub filters: usize,

    /// Number of OPTIONAL patterns
    pub optional_patterns: usize,

    /// Number of UNION branches
    pub union_branches: usize,

    /// Number of subqueries
    pub subqueries: usize,

    /// Aggregation functions used
    pub aggregations: Vec<String>,
}

/// SPARQL query type
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum QueryType {
    Select,
    Construct,
    Ask,
    Describe,
    Unknown,
}

/// Query risk assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryRisk {
    /// Original query
    pub query: String,

    /// Parsed query path
    pub path: QueryPath,

    /// Dependencies
    pub dependencies: Vec<QueryDependency>,

    /// Complexity score (0.0-1.0)
    pub complexity_score: f64,

    /// SOD scores
    pub severity: super::types::Severity,
    pub occurrence: super::types::Occurrence,
    pub detection: super::types::Detection,
    pub rpn: super::types::RiskPriorityNumber,

    /// Identified risks
    pub risks: Vec<String>,
}

/// Query dependency on schema elements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryDependency {
    /// Type of dependency
    pub dependency_type: DependencyType,

    /// Schema element (class, property, namespace)
    pub element: String,

    /// How critical is this dependency
    pub criticality: DependencyCriticality,
}

/// Type of query dependency
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DependencyType {
    /// Depends on a class
    Class,

    /// Depends on a property
    Property,

    /// Depends on a namespace/prefix
    Namespace,

    /// Depends on schema structure
    Schema,
}

/// Dependency criticality
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DependencyCriticality {
    /// Optional dependency
    Low,

    /// Recommended dependency
    Medium,

    /// Required dependency
    High,

    /// Critical dependency
    Critical,
}

/// Query performance analyzer
pub struct QueryPerformanceAnalyzer {
    /// Historical query execution times
    execution_times: HashMap<String, Vec<f64>>,
}

impl QueryPerformanceAnalyzer {
    /// Create a new performance analyzer
    pub fn new() -> Self {
        Self {
            execution_times: HashMap::new(),
        }
    }

    /// Record query execution time
    pub fn record_execution(&mut self, query_id: &str, duration_ms: f64) {
        self.execution_times
            .entry(query_id.to_string())
            .or_default()
            .push(duration_ms);
    }

    /// Get average execution time
    pub fn average_execution_time(&self, query_id: &str) -> Option<f64> {
        self.execution_times.get(query_id).map(|times| {
            let sum: f64 = times.iter().sum();
            sum / times.len() as f64
        })
    }

    /// Identify slow queries (> 1000ms average)
    pub fn identify_slow_queries(&self) -> Vec<String> {
        self.execution_times
            .iter()
            .filter_map(|(id, times)| {
                let avg = times.iter().sum::<f64>() / times.len() as f64;
                if avg > 1000.0 {
                    Some(id.clone())
                } else {
                    None
                }
            })
            .collect()
    }
}

impl Default for QueryPerformanceAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;

    #[test]
    fn test_query_type_detection() {
        let analyzer = QueryPathAnalyzer::new();

        assert_eq!(
            analyzer.detect_query_type("SELECT * WHERE { ?s ?p ?o }"),
            QueryType::Select
        );
        assert_eq!(
            analyzer.detect_query_type("CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }"),
            QueryType::Construct
        );
        assert_eq!(
            analyzer.detect_query_type("ASK { ?s ?p ?o }"),
            QueryType::Ask
        );
    }

    #[test]
    fn test_complexity_calculation() {
        let analyzer = QueryPathAnalyzer::new();

        let simple_path = QueryPath {
            query_type: QueryType::Select,
            triple_patterns: vec!["?s ?p ?o".to_string()],
            filters: 0,
            optional_patterns: 0,
            union_branches: 0,
            subqueries: 0,
            aggregations: Vec::new(),
        };

        let complexity = analyzer.calculate_complexity(&simple_path);
        assert!(complexity < 0.3);

        let complex_path = QueryPath {
            query_type: QueryType::Construct,
            triple_patterns: Vec::new(),
            filters: 5,
            optional_patterns: 3,
            union_branches: 2,
            subqueries: 2,
            aggregations: vec!["COUNT".to_string(), "AVG".to_string()],
        };

        let high_complexity = analyzer.calculate_complexity(&complex_path);
        assert!(high_complexity > 0.7);
    }

    #[test]
    fn test_query_parsing() {
        let analyzer = QueryPathAnalyzer::new();
        let graph = Graph::new().unwrap();

        let query = r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            SELECT ?s ?p ?o
            WHERE {
                ?s ?p ?o .
                OPTIONAL { ?s rdf:type ?type }
                FILTER (?o > 10)
            }
        "#;

        let path = analyzer.parse_query_path(query).unwrap();
        assert_eq!(path.query_type, QueryType::Select);
        assert_eq!(path.optional_patterns, 1);
        assert_eq!(path.filters, 1);
    }

    #[test]
    fn test_performance_analyzer() {
        let mut analyzer = QueryPerformanceAnalyzer::new();

        analyzer.record_execution("query1", 500.0);
        analyzer.record_execution("query1", 600.0);
        analyzer.record_execution("query2", 1500.0);

        let avg = analyzer.average_execution_time("query1");
        assert_eq!(avg, Some(550.0));

        let slow = analyzer.identify_slow_queries();
        assert!(slow.contains(&"query2".to_string()));
    }
}
