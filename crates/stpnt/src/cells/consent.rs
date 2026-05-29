use async_trait::async_trait;
use blake3;
use chrono::Utc;
use ggen_core::parts_execution::{ExecutionPacket, ExecutionStatus, PartExecutor, VectorClock};
use ggen_core::utils::error::Result;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// ConsentGatePart - Implementation of the Consent Cell Fabricator refusal law.
///
/// Scripture Basis: Galatians 6:2 - "Carry each other’s burdens, and in this way you will fulfill the law of Christ."
/// Refusal Law: No follow-up activity is admitted without an explicit stpnt:ConsentReceived event.
pub struct ConsentGatePart {
    pub part_id: String,
    pub owner: String,
}

impl ConsentGatePart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ConsentInput {
    pub person_id: String,
    pub follow_up_activity: String,
    pub has_consent_receipt: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ConsentOutput {
    pub status: String,
    pub canon_basis: String,
    pub refusal_evidence: Option<String>,
    pub terminal_state: String,
    pub owner: String,
    pub prov_receipt: ProvReceipt,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProvReceipt {
    pub activity_id: String,
    pub agent_id: String,
    pub entity_id: String,
    pub generated_at_time: String,
    pub scripture: String,
}

#[async_trait]
impl PartExecutor for ConsentGatePart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: ConsentInput = serde_json::from_slice(&input)?;

        let now = Utc::now();
        let activity_id = format!("consent-gate-{}", Uuid::new_v4());

        // GENESIS MACHINE-TO-MACHINE CLOSURE PROTOCOL
        // Refusal Law: No follow-up activity is admitted without an explicit stpnt:ConsentReceived event.
        // Link to Galatians 6:2 in canon_basis.
        let (status, terminal_state, refusal_evidence, execution_status) = if input_json
            .has_consent_receipt
        {
            (
                "Admitted".to_string(),
                "ConsentAdmitted".to_string(),
                None,
                ExecutionStatus::Success,
            )
        } else {
            (
                "Refused".to_string(), 
                "ConsentRefused".to_string(), 
                Some("Refusal: No stpnt:ConsentReceived found for follow-up attempt. Galatians 6:2 - Refusal of unburdened follow-up.".to_string()), 
                ExecutionStatus::Refused
            )
        };

        let output = ConsentOutput {
            status,
            canon_basis: "Galatians 6:2".to_string(),
            refusal_evidence,
            terminal_state,
            owner: self.owner.clone(),
            prov_receipt: ProvReceipt {
                activity_id,
                agent_id: self.owner.clone(),
                entity_id: input_json.person_id.clone(),
                generated_at_time: now.to_rfc3339(),
                scripture: "Galatians 6:2".to_string(),
            },
        };

        let output_bytes = serde_json::to_vec(&output)?;
        let input_hash = blake3::hash(&input).into();
        let output_hash = blake3::hash(&output_bytes).into();

        let mut clock = VectorClock::new(self.part_id.clone());
        clock.tick(&self.part_id);

        let packet = ExecutionPacket::new(
            Uuid::new_v4().to_string(),
            self.part_id.clone(),
            input_hash,
            output_hash,
            clock,
            execution_status,
            1, // duration placeholder
        );

        Ok((output_bytes, packet))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_consent_gate_refusal_law() {
        let part =
            ConsentGatePart::new("ConsentGate-001".to_string(), "StewardshipCell".to_string());

        // Test case 1: Refusal when no consent receipt is present
        let input_refused = ConsentInput {
            person_id: "person-123".to_string(),
            follow_up_activity: "pastoral-visit".to_string(),
            has_consent_receipt: false,
        };
        let input_bytes = serde_json::to_vec(&input_refused).unwrap();
        let (output_bytes, packet) = part.execute(input_bytes).await.unwrap();
        let output: ConsentOutput = serde_json::from_slice(&output_bytes).unwrap();

        assert_eq!(output.status, "Refused");
        assert_eq!(output.terminal_state, "ConsentRefused");
        assert_eq!(output.canon_basis, "Galatians 6:2");
        assert!(output.refusal_evidence.is_some());
        assert!(output.refusal_evidence.unwrap().contains("Galatians 6:2"));
        assert_eq!(packet.status, ExecutionStatus::Refused);

        // Test case 2: Admittance when consent receipt is present
        let input_admitted = ConsentInput {
            person_id: "person-123".to_string(),
            follow_up_activity: "pastoral-visit".to_string(),
            has_consent_receipt: true,
        };
        let input_bytes_admitted = serde_json::to_vec(&input_admitted).unwrap();
        let (output_bytes_admitted, packet_admitted) =
            part.execute(input_bytes_admitted).await.unwrap();
        let output_admitted: ConsentOutput =
            serde_json::from_slice(&output_bytes_admitted).unwrap();

        assert_eq!(output_admitted.status, "Admitted");
        assert_eq!(output_admitted.terminal_state, "ConsentAdmitted");
        assert!(output_admitted.refusal_evidence.is_none());
        assert_eq!(packet_admitted.status, ExecutionStatus::Success);
    }
}
