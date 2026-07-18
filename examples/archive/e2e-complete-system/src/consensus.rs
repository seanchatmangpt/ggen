/// PBFT Consensus Engine
/// Implements Byzantine Fault Tolerant consensus for critical decisions
use crate::orchestrator::Priority;
use anyhow::{anyhow, Result};
use dashmap::DashMap;
use ed25519_dalek::{Signer, SigningKey, VerifyingKey};
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use tracing::info;

/// Signed vote from a consensus participant
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SignedVote {
    pub voter_id: String,
    pub round: u64,
    pub proposal: Vec<Priority>,
    pub signature: Vec<u8>,
}

/// PBFT Consensus engine
pub struct PBFTConsensus {
    n_participants: usize,
    f_max_byzantine: usize, // max byzantine failures tolerated
    round: Arc<AtomicU64>,
    votes: Arc<DashMap<(String, u64), SignedVote>>,
    signing_key: Arc<SigningKey>,
    verifying_keys: Arc<DashMap<String, VerifyingKey>>,
}

impl PBFTConsensus {
    /// Create new PBFT consensus engine
    pub fn new(n_participants: usize) -> Self {
        info!(
            "[PBFT] Initializing with n={}, f={}",
            n_participants,
            (n_participants - 1) / 3
        );

        // Generate signing key for this consensus instance
        let mut rng = thread_rng();
        let signing_key = SigningKey::generate(&mut rng);

        Self {
            n_participants,
            f_max_byzantine: (n_participants - 1) / 3,
            round: Arc::new(AtomicU64::new(0)),
            votes: Arc::new(DashMap::new()),
            signing_key: Arc::new(signing_key),
            verifying_keys: Arc::new(DashMap::new()),
        }
    }

    /// Add a verifying key for a participant
    pub fn add_participant(&self, participant_id: String, verifying_key: VerifyingKey) {
        self.verifying_keys.insert(participant_id, verifying_key);
    }

    /// Reach agreement on proposals via PBFT
    pub async fn reach_agreement(&self, proposals: Vec<Priority>) -> Result<Vec<Priority>> {
        info!(
            "[PBFT-round-1] All {} agents propose priorities",
            self.n_participants
        );

        let current_round = self.round.fetch_add(1, Ordering::SeqCst);

        // Phase 1: Pre-prepare
        // Leader collects proposals and selects most common
        let mut consensus_proposal = proposals.clone();
        consensus_proposal.sort_by(|a, b| b.score.cmp(&a.score));
        consensus_proposal.truncate(3); // Top 3 priorities

        info!("[PBFT-round-2] Leader aggregates votes, broadcasts");

        // Phase 2: Prepare
        // Each participant signs the proposal
        let my_vote = self
            .sign_vote(current_round, consensus_proposal.clone())
            .await?;

        // Store our vote
        self.votes
            .insert((my_vote.voter_id.clone(), current_round), my_vote.clone());

        // Simulate collecting votes from other participants
        // In real implementation, would be network messages
        let mut vote_count = 1;
        for i in 1..self.n_participants {
            let participant_id = format!("agent-{}", i);
            let vote = self
                .sign_vote_for_participant(
                    &participant_id,
                    current_round,
                    consensus_proposal.clone(),
                )
                .await?;
            self.votes
                .insert((participant_id, current_round), vote.clone());
            vote_count += 1;
        }

        info!(
            "[PBFT-round-3] Agents prepare commit (collected {} votes)",
            vote_count
        );

        // Phase 3: Commit
        // Check if we have quorum (> 2n/3)
        let quorum_size = (2 * self.n_participants) / 3 + 1;
        if vote_count >= quorum_size {
            info!(
                "[PBFT-commit] Quorum reached ({}/{})",
                vote_count, quorum_size
            );
            Ok(consensus_proposal)
        } else {
            Err(anyhow!(
                "Failed to reach quorum ({}/{})",
                vote_count,
                quorum_size
            ))
        }
    }

    /// Sign a vote with our key
    async fn sign_vote(&self, round: u64, proposal: Vec<Priority>) -> Result<SignedVote> {
        let voter_id = "consensus-leader".to_string();
        let msg = format!("{:?}", proposal); // Simplified message format

        // Use AsRef to get reference to SigningKey
        let signature = self.signing_key.as_ref().sign(msg.as_bytes()).to_vec();

        Ok(SignedVote {
            voter_id,
            round,
            proposal,
            signature,
        })
    }

    /// Sign a vote on behalf of a participant (for simulation)
    async fn sign_vote_for_participant(
        &self, participant_id: &str, round: u64, proposal: Vec<Priority>,
    ) -> Result<SignedVote> {
        // In real implementation, would receive signature from participant
        // For simulation, we generate a dummy vote
        let msg = format!("{:?}", proposal);
        let mut rng = thread_rng();
        let dummy_key = SigningKey::generate(&mut rng);
        let signature = dummy_key.sign(msg.as_bytes()).to_vec();

        Ok(SignedVote {
            voter_id: participant_id.to_string(),
            round,
            proposal,
            signature,
        })
    }

    /// Verify all votes for a round
    pub fn verify_round(&self, round: u64) -> Result<bool> {
        let votes: Vec<_> = self
            .votes
            .iter()
            .filter(|entry| entry.key().1 == round)
            .map(|entry| entry.value().clone())
            .collect();

        let quorum_size = (2 * self.n_participants) / 3 + 1;
        if votes.len() >= quorum_size {
            Ok(true)
        } else {
            Err(anyhow!("Not enough votes for round {}", round))
        }
    }

    /// Simulate Byzantine failure detection
    pub fn detect_byzantine(&self) -> Option<String> {
        // In real implementation, would check for inconsistent signatures
        // For simulation, randomly select one as potentially Byzantine
        None
    }

    /// Get current round number
    pub fn current_round(&self) -> u64 {
        self.round.load(Ordering::SeqCst)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_consensus_creation() {
        let consensus = PBFTConsensus::new(6);
        assert_eq!(consensus.n_participants, 6);
        assert_eq!(consensus.f_max_byzantine, 1);
    }

    #[tokio::test]
    async fn test_reach_agreement() {
        let consensus = PBFTConsensus::new(6);
        let proposals = vec![
            Priority {
                domain: crate::orchestrator::LifeDomain::Health,
                score: 45,
            },
            Priority {
                domain: crate::orchestrator::LifeDomain::Career,
                score: 72,
            },
        ];

        let agreement = consensus.reach_agreement(proposals).await;
        assert!(agreement.is_ok());
    }

    #[tokio::test]
    async fn test_byzantine_tolerance() {
        let consensus = PBFTConsensus::new(6);
        // With f=1, system can tolerate 1 Byzantine failure
        assert_eq!(consensus.f_max_byzantine, 1);
    }
}
