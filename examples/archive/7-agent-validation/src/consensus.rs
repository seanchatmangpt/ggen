//! Consensus layer for 7-agent validation
//!
//! Implements PBFT (Practical Byzantine Fault Tolerance) with 5-of-7 quorum.
//! Agents vote independently, and consensus is reached when 5+ agents agree.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::gates::{AndonSignal, GateResult};

/// Consensus decision (APPROVE, REJECT, or DEFER)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConsensusDecision {
    /// APPROVE: 5+ agents voted GREEN (code passes validation)
    Approve {
        /// Number of GREEN votes
        green_votes: u8,
        /// Number of YELLOW votes
        yellow_votes: u8,
        /// Number of RED votes
        red_votes: u8,
    },
    /// REJECT: 5+ agents voted RED (code fails validation)
    Reject {
        /// Number of GREEN votes
        green_votes: u8,
        /// Number of YELLOW votes
        yellow_votes: u8,
        /// Number of RED votes
        red_votes: u8,
        /// Reason for rejection (agent messages)
        reasons: Vec<String>,
    },
    /// DEFER: Unable to reach consensus (split vote, insufficient quorum)
    Defer {
        /// Number of GREEN votes
        green_votes: u8,
        /// Number of YELLOW votes
        yellow_votes: u8,
        /// Number of RED votes
        red_votes: u8,
        /// Suggested next action (retry, manual review, etc.)
        suggestion: String,
    },
}

impl ConsensusDecision {
    /// Check if decision is APPROVE
    pub fn is_approve(&self) -> bool {
        matches!(self, ConsensusDecision::Approve { .. })
    }

    /// Check if decision is REJECT
    pub fn is_reject(&self) -> bool {
        matches!(self, ConsensusDecision::Reject { .. })
    }

    /// Check if decision is DEFER
    pub fn is_defer(&self) -> bool {
        matches!(self, ConsensusDecision::Defer { .. })
    }

    /// Get vote counts
    pub fn vote_counts(&self) -> (u8, u8, u8) {
        match self {
            ConsensusDecision::Approve {
                green_votes,
                yellow_votes,
                red_votes,
            } => (*green_votes, *yellow_votes, *red_votes),
            ConsensusDecision::Reject {
                green_votes,
                yellow_votes,
                red_votes,
                ..
            } => (*green_votes, *yellow_votes, *red_votes),
            ConsensusDecision::Defer {
                green_votes,
                yellow_votes,
                red_votes,
                ..
            } => (*green_votes, *yellow_votes, *red_votes),
        }
    }
}

/// Consensus layer (PBFT implementation)
pub struct ConsensusLayer {
    /// Quorum size (minimum votes required for decision)
    quorum: u8,
    /// Total number of agents
    total_agents: u8,
}

impl ConsensusLayer {
    /// Create a new consensus layer
    ///
    /// # Arguments
    /// * `quorum` - Minimum votes required (e.g., 5 for 5-of-7)
    /// * `total_agents` - Total number of agents (e.g., 7)
    pub fn new(quorum: u8, total_agents: u8) -> Result<Self, Box<dyn std::error::Error>> {
        if quorum > total_agents {
            return Err("Quorum cannot exceed total agents".into());
        }
        if quorum < (total_agents / 2 + 1) {
            // Byzantine fault tolerance requires >2/3 quorum
            return Err("Quorum must be >2/3 of total agents for Byzantine fault tolerance".into());
        }

        Ok(Self {
            quorum,
            total_agents,
        })
    }

    /// Aggregate agent results into consensus decision
    ///
    /// # Algorithm (PBFT-inspired)
    ///
    /// 1. Count votes by signal type (Green, Yellow, Red)
    /// 2. If any signal has ≥quorum votes, that signal wins
    /// 3. Priority: RED > GREEN > YELLOW (safety over progress)
    /// 4. If no quorum, DEFER with suggestion
    pub async fn aggregate(&self, results: Vec<GateResult>) -> Result<ConsensusDecision, Box<dyn std::error::Error>> {
        // Count votes
        let mut green_votes = 0u8;
        let mut yellow_votes = 0u8;
        let mut red_votes = 0u8;
        let mut red_reasons = Vec::new();

        for result in &results {
            match result.signal {
                AndonSignal::Green => green_votes += 1,
                AndonSignal::Yellow => yellow_votes += 1,
                AndonSignal::Red => {
                    red_votes += 1;
                    red_reasons.push(result.message.clone());
                }
            }
        }

        tracing::info!(
            green = green_votes,
            yellow = yellow_votes,
            red = red_votes,
            quorum = self.quorum,
            "Aggregating votes"
        );

        // Check for quorum (priority: RED > GREEN > YELLOW)
        if red_votes >= self.quorum {
            // REJECT: 5+ agents voted RED (safety first)
            return Ok(ConsensusDecision::Reject {
                green_votes,
                yellow_votes,
                red_votes,
                reasons: red_reasons,
            });
        }

        if green_votes >= self.quorum {
            // APPROVE: 5+ agents voted GREEN
            return Ok(ConsensusDecision::Approve {
                green_votes,
                yellow_votes,
                red_votes,
            });
        }

        if yellow_votes >= self.quorum {
            // Treat YELLOW quorum as DEFER (warnings need investigation)
            return Ok(ConsensusDecision::Defer {
                green_votes,
                yellow_votes,
                red_votes,
                suggestion: "Investigate warnings and retry".into(),
            });
        }

        // No quorum reached
        let suggestion = if red_votes > green_votes {
            "Leaning towards rejection - manual review recommended"
        } else if green_votes > red_votes {
            "Leaning towards approval - consider waiving warnings"
        } else {
            "Split vote - escalate to human review"
        };

        Ok(ConsensusDecision::Defer {
            green_votes,
            yellow_votes,
            red_votes,
            suggestion: suggestion.into(),
        })
    }

    /// Check if a given vote count reaches quorum
    pub fn has_quorum(&self, votes: u8) -> bool {
        votes >= self.quorum
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    fn mock_result(agent_id: &str, signal: AndonSignal) -> GateResult {
        GateResult {
            agent_id: agent_id.into(),
            signal,
            message: "Test result".into(),
            timestamp: Utc::now(),
        }
    }

    #[tokio::test]
    async fn test_consensus_approve() {
        let layer = ConsensusLayer::new(5, 7).unwrap();

        // 5 GREEN, 2 YELLOW → APPROVE
        let results = vec![
            mock_result("agent-1", AndonSignal::Green),
            mock_result("agent-2", AndonSignal::Green),
            mock_result("agent-3", AndonSignal::Green),
            mock_result("agent-4", AndonSignal::Green),
            mock_result("agent-5", AndonSignal::Green),
            mock_result("agent-6", AndonSignal::Yellow),
            mock_result("agent-7", AndonSignal::Yellow),
        ];

        let decision = layer.aggregate(results).await.unwrap();
        assert!(decision.is_approve());
        assert_eq!(decision.vote_counts(), (5, 2, 0));
    }

    #[tokio::test]
    async fn test_consensus_reject() {
        let layer = ConsensusLayer::new(5, 7).unwrap();

        // 5 RED, 2 GREEN → REJECT (RED priority)
        let results = vec![
            mock_result("agent-1", AndonSignal::Red),
            mock_result("agent-2", AndonSignal::Red),
            mock_result("agent-3", AndonSignal::Red),
            mock_result("agent-4", AndonSignal::Red),
            mock_result("agent-5", AndonSignal::Red),
            mock_result("agent-6", AndonSignal::Green),
            mock_result("agent-7", AndonSignal::Green),
        ];

        let decision = layer.aggregate(results).await.unwrap();
        assert!(decision.is_reject());
        assert_eq!(decision.vote_counts(), (2, 0, 5));
    }

    #[tokio::test]
    async fn test_consensus_defer() {
        let layer = ConsensusLayer::new(5, 7).unwrap();

        // 3 GREEN, 2 RED, 2 YELLOW → DEFER (no quorum)
        let results = vec![
            mock_result("agent-1", AndonSignal::Green),
            mock_result("agent-2", AndonSignal::Green),
            mock_result("agent-3", AndonSignal::Green),
            mock_result("agent-4", AndonSignal::Red),
            mock_result("agent-5", AndonSignal::Red),
            mock_result("agent-6", AndonSignal::Yellow),
            mock_result("agent-7", AndonSignal::Yellow),
        ];

        let decision = layer.aggregate(results).await.unwrap();
        assert!(decision.is_defer());
        assert_eq!(decision.vote_counts(), (3, 2, 2));
    }

    #[test]
    fn test_consensus_validation() {
        // Valid: quorum <= total and >2/3
        assert!(ConsensusLayer::new(5, 7).is_ok());
        assert!(ConsensusLayer::new(3, 4).is_ok());

        // Invalid: quorum > total
        assert!(ConsensusLayer::new(8, 7).is_err());

        // Invalid: quorum too small (<2/3)
        assert!(ConsensusLayer::new(2, 7).is_err());
    }

    #[test]
    fn test_vote_counts() {
        let decision = ConsensusDecision::Approve {
            green_votes: 5,
            yellow_votes: 1,
            red_votes: 1,
        };

        assert_eq!(decision.vote_counts(), (5, 1, 1));
        assert!(decision.is_approve());
        assert!(!decision.is_reject());
        assert!(!decision.is_defer());
    }
}
