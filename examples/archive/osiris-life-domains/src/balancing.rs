//! Domain Balancing and Consensus Mechanism
//!
//! Handles:
//! - Consensus voting for resource allocation
//! - Imbalance detection
//! - Rebalancing strategy generation

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::info;

/// Domain balance tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DomainBalance {
    pub consensus_threshold: f64,
    pub imbalance_threshold: f64,
    pub allocations: HashMap<String, f64>,
}

impl DomainBalance {
    /// Create new domain balance tracker
    pub fn new() -> Self {
        Self {
            consensus_threshold: 0.67,
            imbalance_threshold: 0.65,
            allocations: HashMap::new(),
        }
    }

    /// Run consensus voting for resource allocation
    pub fn run_consensus_voting(&mut self, scores: &HashMap<String, f64>) -> anyhow::Result<()> {
        info!("Running consensus voting with {} domains", scores.len());

        // Calculate weighted allocations based on domain needs
        let total_score: f64 = scores.values().sum();

        for (domain_id, score) in scores {
            // Inverse allocation: domains with lower scores get more resources
            let gap = 1.0 - score;
            let allocation = gap / total_score;
            self.allocations.insert(domain_id.clone(), allocation);
        }
        
        Ok(())
    }

    /// Detect imbalanced domains
    pub fn detect_imbalances(&self, scores: &HashMap<String, f64>) -> anyhow::Result<Vec<String>> {
        let imbalanced: Vec<String> = scores
            .iter()
            .filter(|(_, score)| **score < self.imbalance_threshold)
            .map(|(id, _)| id.clone())
            .collect();
        
        if !imbalanced.is_empty() {
            info!("Detected imbalances in: {:?}", imbalanced);
        }
        
        Ok(imbalanced)
    }

    /// Propose rebalancing strategy
    pub fn propose_rebalancing(
        &self,
        imbalanced_domains: &[String],
    ) -> anyhow::Result<HashMap<String, f64>> {
        info!("Proposing rebalancing for {} domains", imbalanced_domains.len());
        
        let mut strategy = HashMap::new();
        let base_allocation = 1.0 / imbalanced_domains.len() as f64;
        
        for domain in imbalanced_domains {
            strategy.insert(domain.clone(), base_allocation * 1.5); // 50% boost
        }
        
        Ok(strategy)
    }

    /// Get allocation for a domain
    pub fn get_allocation(&self, domain_id: &str) -> f64 {
        self.allocations.get(domain_id).copied().unwrap_or(0.1)
    }
}

impl Default for DomainBalance {
    fn default() -> Self {
        Self::new()
    }
}

/// Consensus voting mechanism
pub struct ConsensusVoting {
    threshold: f64,
}

impl ConsensusVoting {
    /// Create new consensus voting
    pub fn new() -> Self {
        Self {
            threshold: 0.67,
        }
    }

    /// Vote on a proposal with weighted scores
    pub fn vote(&self, votes: &HashMap<String, f64>) -> anyhow::Result<bool> {
        if votes.is_empty() {
            return Ok(false);
        }
        
        let total_weight: f64 = votes.values().sum();
        let average_score = total_weight / votes.len() as f64;
        
        Ok(average_score >= self.threshold)
    }

    /// Calculate weighted consensus score
    pub fn calculate_consensus(&self, scores: &[f64]) -> f64 {
        if scores.is_empty() {
            return 0.0;
        }
        scores.iter().sum::<f64>() / scores.len() as f64
    }
}

impl Default for ConsensusVoting {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_balance_creation() {
        let balance = DomainBalance::new();
        assert_eq!(balance.consensus_threshold, 0.67);
    }

    #[test]
    fn test_detect_imbalances() {
        let balance = DomainBalance::new();
        let mut scores = HashMap::new();
        scores.insert("health".to_string(), 0.5);
        scores.insert("career".to_string(), 0.8);
        
        let imbalanced = balance.detect_imbalances(&scores).unwrap();
        assert_eq!(imbalanced.len(), 1);
        assert!(imbalanced.contains(&"health".to_string()));
    }

    #[test]
    fn test_consensus_voting() {
        let voting = ConsensusVoting::new();
        let mut votes = HashMap::new();
        votes.insert("health".to_string(), 0.8);
        votes.insert("career".to_string(), 0.6);
        
        let consensus = voting.vote(&votes).unwrap();
        assert!(consensus);
    }
}
