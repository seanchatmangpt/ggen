//! Quorum calculation for Byzantine consensus
//!
//! Implements quorum rules for PBFT:
//! - Quorum: 2f+1 replicas (majority of honest nodes)
//! - Total replicas: 3f+1 (tolerates f Byzantine faults)

use crate::{ConsensusError, Result};
use serde::{Deserialize, Serialize};

/// Configuration for quorum calculations
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct QuorumConfig {
    /// Total number of replicas (3f+1)
    pub total_replicas: usize,
    /// Maximum Byzantine faults tolerated (f)
    pub max_faults: usize,
}

impl QuorumConfig {
    /// Create a new quorum configuration
    ///
    /// # Arguments
    /// * `total_replicas` - Total number of replicas (must be >= 3f+1)
    /// * `max_faults` - Maximum Byzantine faults to tolerate (f)
    pub fn new(total_replicas: usize, max_faults: usize) -> Result<Self> {
        let min_replicas = 3 * max_faults + 1;

        if total_replicas < min_replicas {
            return Err(ConsensusError::InsufficientReplicas {
                got: total_replicas,
                needed: min_replicas,
            });
        }

        Ok(Self {
            total_replicas,
            max_faults,
        })
    }

    /// Create configuration from total replicas (calculates f automatically)
    pub fn from_total_replicas(total_replicas: usize) -> Result<Self> {
        if total_replicas < 4 {
            return Err(ConsensusError::InsufficientReplicas {
                got: total_replicas,
                needed: 4,
            });
        }

        // Calculate maximum f for 3f+1 constraint
        let max_faults = (total_replicas - 1) / 3;
        Self::new(total_replicas, max_faults)
    }
}

/// Quorum calculator for Byzantine consensus
#[derive(Debug, Clone)]
pub struct QuorumCalculator {
    config: QuorumConfig,
}

impl QuorumCalculator {
    /// Create a new quorum calculator
    pub fn new(config: QuorumConfig) -> Self {
        Self { config }
    }

    /// Get the quorum size (2f+1)
    ///
    /// This is the minimum number of votes needed for consensus.
    /// With 2f+1 votes, at most f can be Byzantine, ensuring at least
    /// f+1 honest replicas agree.
    pub fn quorum_size(&self) -> usize {
        2 * self.config.max_faults + 1
    }

    /// Get the prepare quorum size
    ///
    /// For PBFT prepare phase: 2f (excluding the primary's pre-prepare)
    pub fn prepare_quorum(&self) -> usize {
        2 * self.config.max_faults
    }

    /// Get the commit quorum size
    ///
    /// For PBFT commit phase: 2f+1 (including own commit)
    pub fn commit_quorum(&self) -> usize {
        2 * self.config.max_faults + 1
    }

    /// Get the view change quorum size
    ///
    /// For view change: 2f+1 replicas must agree to change view
    pub fn view_change_quorum(&self) -> usize {
        2 * self.config.max_faults + 1
    }

    /// Check if we have reached quorum
    pub fn has_quorum(&self, votes: usize) -> bool {
        votes >= self.quorum_size()
    }

    /// Check if we have reached prepare quorum
    pub fn has_prepare_quorum(&self, votes: usize) -> bool {
        votes >= self.prepare_quorum()
    }

    /// Check if we have reached commit quorum
    pub fn has_commit_quorum(&self, votes: usize) -> bool {
        votes >= self.commit_quorum()
    }

    /// Check if we have reached view change quorum
    pub fn has_view_change_quorum(&self, votes: usize) -> bool {
        votes >= self.view_change_quorum()
    }

    /// Get the maximum number of Byzantine faults tolerated
    pub fn max_faults(&self) -> usize {
        self.config.max_faults
    }

    /// Get total number of replicas
    pub fn total_replicas(&self) -> usize {
        self.config.total_replicas
    }

    /// Validate vote count doesn't exceed total replicas
    pub fn validate_vote_count(&self, votes: usize) -> Result<()> {
        if votes > self.config.total_replicas {
            return Err(ConsensusError::ByzantineFault {
                reason: format!(
                    "Vote count {} exceeds total replicas {}",
                    votes, self.config.total_replicas
                ),
            });
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quorum_config_valid() {
        // Arrange & Act
        let config = QuorumConfig::new(4, 1);

        // Assert
        assert!(config.is_ok());
        let config = config.unwrap();
        assert_eq!(config.total_replicas, 4);
        assert_eq!(config.max_faults, 1);
    }

    #[test]
    fn test_quorum_config_insufficient_replicas() {
        // Arrange & Act
        let result = QuorumConfig::new(3, 1);

        // Assert
        assert!(matches!(
            result,
            Err(ConsensusError::InsufficientReplicas { got: 3, needed: 4 })
        ));
    }

    #[test]
    fn test_quorum_config_from_total() {
        // Arrange & Act
        let config = QuorumConfig::from_total_replicas(7).unwrap();

        // Assert
        assert_eq!(config.total_replicas, 7);
        assert_eq!(config.max_faults, 2); // (7-1)/3 = 2
    }

    #[test]
    fn test_quorum_sizes() {
        // Arrange
        let config = QuorumConfig::new(4, 1).unwrap();
        let calculator = QuorumCalculator::new(config);

        // Act & Assert
        assert_eq!(calculator.quorum_size(), 3); // 2*1+1
        assert_eq!(calculator.prepare_quorum(), 2); // 2*1
        assert_eq!(calculator.commit_quorum(), 3); // 2*1+1
        assert_eq!(calculator.view_change_quorum(), 3); // 2*1+1
    }

    #[test]
    fn test_quorum_sizes_multiple_faults() {
        // Arrange
        let config = QuorumConfig::new(10, 3).unwrap();
        let calculator = QuorumCalculator::new(config);

        // Act & Assert
        assert_eq!(calculator.quorum_size(), 7); // 2*3+1
        assert_eq!(calculator.prepare_quorum(), 6); // 2*3
        assert_eq!(calculator.commit_quorum(), 7); // 2*3+1
        assert_eq!(calculator.max_faults(), 3);
        assert_eq!(calculator.total_replicas(), 10);
    }

    #[test]
    fn test_has_quorum() {
        // Arrange
        let config = QuorumConfig::new(4, 1).unwrap();
        let calculator = QuorumCalculator::new(config);

        // Act & Assert
        assert!(!calculator.has_quorum(2));
        assert!(calculator.has_quorum(3));
        assert!(calculator.has_quorum(4));
    }

    #[test]
    fn test_has_prepare_quorum() {
        // Arrange
        let config = QuorumConfig::new(4, 1).unwrap();
        let calculator = QuorumCalculator::new(config);

        // Act & Assert
        assert!(!calculator.has_prepare_quorum(1));
        assert!(calculator.has_prepare_quorum(2));
        assert!(calculator.has_prepare_quorum(3));
    }

    #[test]
    fn test_validate_vote_count() {
        // Arrange
        let config = QuorumConfig::new(4, 1).unwrap();
        let calculator = QuorumCalculator::new(config);

        // Act & Assert
        assert!(calculator.validate_vote_count(4).is_ok());
        assert!(calculator.validate_vote_count(5).is_err());
    }

    #[test]
    fn test_byzantine_tolerance_scenarios() {
        // Test various configurations
        let scenarios = vec![
            (4, 1, 3),   // 4 replicas, tolerate 1 fault, need 3 votes
            (7, 2, 5),   // 7 replicas, tolerate 2 faults, need 5 votes
            (10, 3, 7),  // 10 replicas, tolerate 3 faults, need 7 votes
            (13, 4, 9),  // 13 replicas, tolerate 4 faults, need 9 votes
            (100, 33, 67), // 100 replicas, tolerate 33 faults, need 67 votes
        ];

        for (total, faults, expected_quorum) in scenarios {
            // Arrange
            let config = QuorumConfig::new(total, faults).unwrap();
            let calculator = QuorumCalculator::new(config);

            // Act & Assert
            assert_eq!(calculator.quorum_size(), expected_quorum);
            assert_eq!(calculator.max_faults(), faults);

            // Verify we can tolerate f faults
            let honest_replicas = total - faults;
            assert!(honest_replicas >= expected_quorum);
        }
    }
}
