use crate::canon::{CanonBasis, SCRIPTURE_1_COR_4_2};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use ggen_core::parts_execution::{ExecutionPacket, ExecutionStatus, PartExecutor, VectorClock};
use ggen_core::utils::error::Result;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// FollowUpObligationPart - Implementation of CSC-1 Follow-Up.
pub struct FollowUpObligationPart {
    pub part_id: String,
    pub owner: String,
}

impl CanonBasis for FollowUpObligationPart {
    fn canon_basis(&self) -> &'static [&'static str] {
        &[SCRIPTURE_1_COR_4_2]
    }
}

impl FollowUpObligationPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FollowUpInput {
    pub obligation_id: String,
    pub deadline: DateTime<Utc>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FollowUpOutput {
    pub status: String,
    pub terminal_state: String,
}

#[async_trait]
impl PartExecutor for FollowUpObligationPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: FollowUpInput = serde_json::from_slice(&input)?;

        let (status, terminal_state, execution_status) = if Utc::now() > input_json.deadline {
            (
                "Escalated".to_string(),
                "Overdue".to_string(),
                ExecutionStatus::Refused,
            )
        } else {
            (
                "Completed".to_string(),
                "SuccessfulIncorporate".to_string(),
                ExecutionStatus::Success,
            )
        };

        let output = FollowUpOutput {
            status,
            terminal_state,
        };
        let output_bytes = serde_json::to_vec(&output)?;
        let packet = ExecutionPacket::new(
            Uuid::new_v4().to_string(),
            self.part_id.clone(),
            blake3::hash(&input).into(),
            blake3::hash(&output_bytes).into(),
            VectorClock::new(self.part_id.clone()),
            execution_status,
            1,
        );
        Ok((output_bytes, packet))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_followup_overdue() {
        let part = FollowUpObligationPart::new("F-001".to_string(), "Cell-Alpha".to_string());
        // Deadline in the past
        let input = FollowUpInput {
            obligation_id: "o1".to_string(),
            deadline: Utc::now() - chrono::Duration::days(1),
        };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, packet) = part.execute(input_bytes).await.unwrap();
        let output: FollowUpOutput = serde_json::from_slice(&output_bytes).unwrap();
        assert_eq!(output.terminal_state, "Overdue");
        assert_eq!(packet.status, ExecutionStatus::Refused);
    }
}
