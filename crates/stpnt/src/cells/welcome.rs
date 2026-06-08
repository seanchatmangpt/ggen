use crate::canon::{CanonBasis, SCRIPTURE_ROMANS_15_7};
use async_trait::async_trait;
use blake3;
use ggen_core::parts_execution::{ExecutionPacket, ExecutionStatus, PartExecutor, VectorClock};
use ggen_core::utils::error::Result;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// WelcomeOneAnotherPart - Implementation of CSC-1 Welcome Trigger.
pub struct WelcomeOneAnotherPart {
    pub part_id: String,
    pub owner: String,
}

impl CanonBasis for WelcomeOneAnotherPart {
    fn canon_basis(&self) -> &'static [&'static str] {
        &[SCRIPTURE_ROMANS_15_7]
    }
}

impl WelcomeOneAnotherPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WelcomeInput {
    pub person_id: String,
    pub arrival_timestamp: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WelcomeOutput {
    pub status: String,
    pub terminal_state: String,
}

#[async_trait]
impl PartExecutor for WelcomeOneAnotherPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let _input_json: WelcomeInput = serde_json::from_slice(&input)?;
        let output = WelcomeOutput {
            status: "Success".to_string(),
            terminal_state: "ReceivedAndRemembered".to_string(),
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
#[allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_welcome_execution() {
        let part = WelcomeOneAnotherPart::new("W-001".to_string(), "Cell-Alpha".to_string());
        let input = WelcomeInput {
            person_id: "p1".to_string(),
            arrival_timestamp: "2025-05-20".to_string(),
        };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, packet) = part.execute(input_bytes).await.unwrap();
        let output: WelcomeOutput = serde_json::from_slice(&output_bytes).unwrap();
        assert_eq!(output.terminal_state, "ReceivedAndRemembered");
        assert_eq!(packet.status, ExecutionStatus::Success);
    }
}
