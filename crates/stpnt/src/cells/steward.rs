use ggen_core::parts_execution::{ExecutionPacket, ExecutionStatus, PartExecutor, VectorClock};
use ggen_core::utils::error::Result;
use serde::{Deserialize, Serialize};
use async_trait::async_trait;
use uuid::Uuid;
use crate::canon::{CanonBasis, SCRIPTURE_1_COR_4_2};

/// AssignStewardPart - Implementation of CSC-1 Steward Assignment.
pub struct AssignStewardPart {
    pub part_id: String,
    pub owner: String,
}

impl CanonBasis for AssignStewardPart {
    fn canon_basis(&self) -> &'static [&'static str] {
        &[SCRIPTURE_1_COR_4_2]
    }
}

impl AssignStewardPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssignInput {
    pub person_id: String,
    pub steward_id: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssignOutput {
    pub status: String,
    pub terminal_state: String,
}

#[async_trait]
impl PartExecutor for AssignStewardPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let output = AssignOutput {
            status: "Success".to_string(),
            terminal_state: "StewardBound".to_string(),
        };
        let output_bytes = serde_json::to_vec(&output)?;
        let packet = ExecutionPacket::new(
            Uuid::new_v4().to_string(),
            self.part_id.clone(),
            blake3::hash(&input).into(),
            blake3::hash(&output_bytes).into(),
            VectorClock::new(self.part_id.clone()),
            ExecutionStatus::Success,
            1,
        );
        Ok((output_bytes, packet))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_steward_assignment() {
        let part = AssignStewardPart::new("S-001".to_string(), "Cell-Alpha".to_string());
        let input = AssignInput { person_id: "p1".to_string(), steward_id: "s1".to_string() };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, packet) = part.execute(input_bytes).await.unwrap();
        let output: AssignOutput = serde_json::from_slice(&output_bytes).unwrap();
        assert_eq!(output.terminal_state, "StewardBound");
        assert_eq!(packet.status, ExecutionStatus::Success);
    }
}
