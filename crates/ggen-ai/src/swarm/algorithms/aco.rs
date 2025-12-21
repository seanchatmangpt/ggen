//! Ant Colony Optimization for SPARQL Query Path Finding
//!
//! This module implements ACO to optimize SPARQL query execution paths by:
//! - Modeling triple patterns as graph nodes
//! - Using pheromone trails to find efficient query paths
//! - Adapting to query performance feedback
//! - Discovering optimal join orders

use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info};

/// Ant Colony Optimization configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AcoConfig {
    /// Number of ants in the colony
    pub num_ants: usize,
    /// Pheromone evaporation rate (0.0 to 1.0)
    pub evaporation_rate: f64,
    /// Pheromone deposit weight
    pub pheromone_weight: f64,
    /// Heuristic information weight (alpha)
    pub alpha: f64,
    /// Pheromone weight (beta)
    pub beta: f64,
    /// Maximum iterations
    pub max_iterations: usize,
    /// Elite ant count (best paths get extra pheromone)
    pub elite_ant_count: usize,
}

impl Default for AcoConfig {
    fn default() -> Self {
        Self {
            num_ants: 20,
            evaporation_rate: 0.1,
            pheromone_weight: 1.0,
            alpha: 1.0,
            beta: 2.0,
            max_iterations: 100,
            elite_ant_count: 5,
        }
    }
}

/// SPARQL query graph node representing a triple pattern
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct QueryNode {
    /// Subject pattern
    pub subject: String,
    /// Predicate pattern
    pub predicate: String,
    /// Object pattern
    pub object: String,
    /// Estimated selectivity (0.0 to 1.0)
    pub selectivity: f64,
}

/// Edge between query nodes with pheromone trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryEdge {
    /// Source node
    pub from: QueryNode,
    /// Target node
    pub to: QueryNode,
    /// Pheromone level
    pub pheromone: f64,
    /// Heuristic value (based on selectivity and cardinality)
    pub heuristic: f64,
    /// Join cost estimate
    pub cost: f64,
}

/// Ant path through query graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AntPath {
    /// Sequence of nodes visited
    pub nodes: Vec<QueryNode>,
    /// Total path cost
    pub cost: f64,
    /// Path quality score (inverse of cost)
    pub quality: f64,
}

/// Ant Colony Optimizer for SPARQL queries
#[derive(Debug)]
pub struct SparqlAcoOptimizer {
    /// Configuration
    config: AcoConfig,
    /// Query graph nodes
    nodes: Vec<QueryNode>,
    /// Pheromone matrix
    pheromones: Arc<RwLock<HashMap<(QueryNode, QueryNode), f64>>>,
    /// Heuristic matrix (static information)
    heuristics: HashMap<(QueryNode, QueryNode), f64>,
    /// Best path found so far
    best_path: Arc<RwLock<Option<AntPath>>>,
    /// Performance history for adaptive tuning
    performance_history: Arc<RwLock<Vec<PathPerformance>>>,
}

/// Path performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathPerformance {
    /// Path taken
    pub path: AntPath,
    /// Actual execution time (ms)
    pub execution_time_ms: u64,
    /// Number of results returned
    pub result_count: usize,
    /// Timestamp
    pub timestamp: String,
}

impl SparqlAcoOptimizer {
    /// Create a new SPARQL ACO optimizer
    pub fn new(config: AcoConfig, nodes: Vec<QueryNode>) -> Self {
        let mut heuristics = HashMap::new();
        let mut initial_pheromones = HashMap::new();

        // Initialize heuristics and pheromones
        for i in 0..nodes.len() {
            for j in 0..nodes.len() {
                if i != j {
                    let from = nodes[i].clone();
                    let to = nodes[j].clone();

                    // Heuristic based on selectivity (more selective = better)
                    let heuristic = 1.0 / (from.selectivity + 0.01).max(to.selectivity + 0.01);
                    heuristics.insert((from.clone(), to.clone()), heuristic);

                    // Initialize pheromone to small value
                    initial_pheromones.insert((from, to), 1.0);
                }
            }
        }

        Self {
            config,
            nodes,
            pheromones: Arc::new(RwLock::new(initial_pheromones)),
            heuristics,
            best_path: Arc::new(RwLock::new(None)),
            performance_history: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Optimize SPARQL query path using ACO
    pub async fn optimize(&self) -> Result<AntPath> {
        let mut iteration_best: Option<AntPath> = None;

        for iteration in 0..self.config.max_iterations {
            let mut iteration_paths = Vec::new();

            // Send out ants to explore paths
            for ant_id in 0..self.config.num_ants {
                let path = self.construct_path(ant_id).await?;
                iteration_paths.push(path);
            }

            // Find best path in this iteration
            if let Some(best) = iteration_paths.iter()
                .min_by(|a, b| a.cost.partial_cmp(&b.cost).unwrap_or(std::cmp::Ordering::Equal))
            {
                if iteration_best.as_ref().map_or(true, |current| best.cost < current.cost) {
                    iteration_best = Some(best.clone());
                }
            }

            // Update pheromones
            self.update_pheromones(&iteration_paths).await?;

            // Update global best
            if let Some(iter_best) = &iteration_best {
                let mut best_path = self.best_path.write().await;
                if best_path.as_ref().map_or(true, |current| iter_best.cost < current.cost) {
                    *best_path = Some(iter_best.clone());
                    info!("ACO iteration {}: new best path with cost {}", iteration, iter_best.cost);
                }
            }
        }

        let best = self.best_path.read().await;
        best.clone().ok_or_else(|| GgenAiError::internal("No valid path found"))
    }

    /// Construct a path for a single ant
    async fn construct_path(&self, _ant_id: usize) -> Result<AntPath> {
        let mut visited = Vec::new();
        let mut current_nodes: Vec<QueryNode> = self.nodes.clone();
        let mut total_cost = 0.0;

        // Start with a random node
        if current_nodes.is_empty() {
            return Err(GgenAiError::internal("No nodes to explore"));
        }

        let start_idx = fastrand::usize(0..current_nodes.len());
        let start_node = current_nodes.remove(start_idx);
        visited.push(start_node.clone());

        // Build path by selecting next nodes probabilistically
        while !current_nodes.is_empty() {
            let current = visited.last()
                .ok_or_else(|| GgenAiError::internal("Path construction failed: no nodes visited"))?;
            let next_node = self.select_next_node(current, &current_nodes).await?;

            // Calculate cost of this edge
            let edge_cost = self.calculate_edge_cost(current, &next_node);
            total_cost += edge_cost;

            // Add to path
            visited.push(next_node.clone());
            current_nodes.retain(|n| n != &next_node);
        }

        Ok(AntPath {
            nodes: visited,
            cost: total_cost,
            quality: 1.0 / (total_cost + 1.0),
        })
    }

    /// Select next node based on pheromone and heuristic
    async fn select_next_node(
        &self,
        current: &QueryNode,
        available: &[QueryNode],
    ) -> Result<QueryNode> {
        let pheromones = self.pheromones.read().await;
        let mut probabilities: Vec<(QueryNode, f64)> = Vec::new();
        let mut total_probability = 0.0;

        for node in available {
            let key = (current.clone(), node.clone());
            let pheromone = pheromones.get(&key).copied().unwrap_or(1.0);
            let heuristic = self.heuristics.get(&key).copied().unwrap_or(1.0);

            // Probability = (pheromone^alpha) * (heuristic^beta)
            let prob = pheromone.powf(self.config.alpha) * heuristic.powf(self.config.beta);
            probabilities.push((node.clone(), prob));
            total_probability += prob;
        }

        // Normalize probabilities
        if total_probability == 0.0 {
            // Fallback to random selection
            return Ok(available[fastrand::usize(0..available.len())].clone());
        }

        // Roulette wheel selection
        let mut roulette = fastrand::f64() * total_probability;
        for (node, prob) in probabilities {
            roulette -= prob;
            if roulette <= 0.0 {
                return Ok(node);
            }
        }

        // Fallback to last node
        available.last()
            .cloned()
            .ok_or_else(|| GgenAiError::internal("No available nodes for selection"))
    }

    /// Calculate edge cost between two nodes
    fn calculate_edge_cost(&self, from: &QueryNode, to: &QueryNode) -> f64 {
        // Cost based on selectivity (higher selectivity = lower cost)
        // and estimated cardinality
        let selectivity_cost = from.selectivity * to.selectivity;
        let join_cost = 1.0 / (selectivity_cost + 0.01);
        join_cost
    }

    /// Update pheromone trails
    async fn update_pheromones(&self, paths: &[AntPath]) -> Result<()> {
        let mut pheromones = self.pheromones.write().await;

        // Evaporation
        for (_edge, pheromone) in pheromones.iter_mut() {
            *pheromone *= 1.0 - self.config.evaporation_rate;
        }

        // Deposit pheromones from all ants
        for path in paths {
            let deposit = self.config.pheromone_weight * path.quality;

            for i in 0..path.nodes.len() - 1 {
                let edge = (path.nodes[i].clone(), path.nodes[i + 1].clone());
                *pheromones.entry(edge).or_insert(1.0) += deposit;
            }
        }

        // Elite ants get extra pheromone
        let mut sorted_paths = paths.to_vec();
        sorted_paths.sort_by(|a, b| b.quality.partial_cmp(&a.quality).unwrap_or(std::cmp::Ordering::Equal));

        for path in sorted_paths.iter().take(self.config.elite_ant_count) {
            let elite_deposit = self.config.pheromone_weight * path.quality * 2.0;

            for i in 0..path.nodes.len() - 1 {
                let edge = (path.nodes[i].clone(), path.nodes[i + 1].clone());
                *pheromones.entry(edge).or_insert(1.0) += elite_deposit;
            }
        }

        Ok(())
    }

    /// Record actual performance for adaptive learning
    pub async fn record_performance(&self, path: AntPath, execution_time_ms: u64, result_count: usize) -> Result<()> {
        let mut history = self.performance_history.write().await;

        history.push(PathPerformance {
            path,
            execution_time_ms,
            result_count,
            timestamp: chrono::Utc::now().to_rfc3339(),
        });

        // Adapt pheromones based on actual performance
        self.adapt_from_performance().await?;

        Ok(())
    }

    /// Adapt pheromone levels based on actual performance
    async fn adapt_from_performance(&self) -> Result<()> {
        let history = self.performance_history.read().await;
        if history.is_empty() {
            return Ok(());
        }

        // Find best performing paths
        let mut sorted_history = history.clone();
        sorted_history.sort_by(|a, b| a.execution_time_ms.cmp(&b.execution_time_ms));

        // Boost pheromones for well-performing paths
        let mut pheromones = self.pheromones.write().await;

        for perf in sorted_history.iter().take(5) {
            let boost = 1.0 / (perf.execution_time_ms as f64 + 1.0);

            for i in 0..perf.path.nodes.len() - 1 {
                let edge = (perf.path.nodes[i].clone(), perf.path.nodes[i + 1].clone());
                *pheromones.entry(edge).or_insert(1.0) += boost;
            }
        }

        debug!("Adapted pheromones based on {} performance records", history.len());

        Ok(())
    }

    /// Get current best path
    pub async fn get_best_path(&self) -> Option<AntPath> {
        self.best_path.read().await.clone()
    }
}

/// Convert SPARQL query to query nodes for ACO
pub fn parse_sparql_to_nodes(sparql: &str) -> Result<Vec<QueryNode>> {
    let mut nodes = Vec::new();

    // Simple pattern extraction (in practice, would use proper SPARQL parser)
    // This is a placeholder implementation
    let lines: Vec<&str> = sparql.lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .collect();

    for line in lines {
        if line.contains("?") {
            // Extract triple pattern
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 3 {
                let subject = parts[0].to_string();
                let predicate = parts[1].to_string();
                let object = parts[2].trim_end_matches(['.', ';'].as_ref()).to_string();

                // Estimate selectivity based on variable count
                let var_count = [&subject, &predicate, &object]
                    .iter()
                    .filter(|s| s.starts_with('?'))
                    .count();

                let selectivity = match var_count {
                    0 => 0.001, // Constant triple - very selective
                    1 => 0.1,   // One variable
                    2 => 0.5,   // Two variables
                    _ => 0.9,   // All variables - not selective
                };

                nodes.push(QueryNode {
                    subject,
                    predicate,
                    object,
                    selectivity,
                });
            }
        }
    }

    if nodes.is_empty() {
        return Err(GgenAiError::internal("No valid query patterns found"));
    }

    Ok(nodes)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_aco_optimizer_creation() {
        let nodes = vec![
            QueryNode {
                subject: "?person".to_string(),
                predicate: "rdf:type".to_string(),
                object: "foaf:Person".to_string(),
                selectivity: 0.1,
            },
            QueryNode {
                subject: "?person".to_string(),
                predicate: "foaf:name".to_string(),
                object: "?name".to_string(),
                selectivity: 0.5,
            },
        ];

        let config = AcoConfig::default();
        let optimizer = SparqlAcoOptimizer::new(config, nodes);

        assert_eq!(optimizer.nodes.len(), 2);
    }

    #[tokio::test]
    async fn test_path_construction() {
        let nodes = vec![
            QueryNode {
                subject: "?person".to_string(),
                predicate: "rdf:type".to_string(),
                object: "foaf:Person".to_string(),
                selectivity: 0.1,
            },
            QueryNode {
                subject: "?person".to_string(),
                predicate: "foaf:name".to_string(),
                object: "?name".to_string(),
                selectivity: 0.5,
            },
            QueryNode {
                subject: "?person".to_string(),
                predicate: "foaf:age".to_string(),
                object: "?age".to_string(),
                selectivity: 0.3,
            },
        ];

        let config = AcoConfig {
            num_ants: 5,
            max_iterations: 10,
            ..Default::default()
        };

        let optimizer = SparqlAcoOptimizer::new(config, nodes);
        let path = optimizer.optimize().await.unwrap();

        assert_eq!(path.nodes.len(), 3);
        assert!(path.cost > 0.0);
        assert!(path.quality > 0.0);
    }

    #[test]
    fn test_sparql_parsing() {
        let sparql = r#"
            SELECT ?person ?name WHERE {
                ?person rdf:type foaf:Person .
                ?person foaf:name ?name .
                ?person foaf:age ?age .
            }
        "#;

        let nodes = parse_sparql_to_nodes(sparql).unwrap();
        assert!(nodes.len() >= 3);
    }
}
