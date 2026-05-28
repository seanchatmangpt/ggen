use crate::parts_execution::{ExecutionPacket, ExecutionStatus, PartExecutor, VectorClock};
use crate::utils::error::Result;
use chrono::Utc;
use serde::{Deserialize, Serialize};
use async_trait::async_trait;
use blake3;
use uuid::Uuid;

/// Stewardship Part: WelcomeOneAnotherPart
/// 
/// Scripture: Romans 15:7 - "Welcome one another, therefore, as Christ has welcomed you, for the glory of God."
/// AA Structure: Agent(Steward), Activity(WelcomeActivity), Object(Visitor)
/// Owner: StewardshipCell
/// Timer: 24h
/// Terminal State: ReceivedAndRemembered
pub struct WelcomeOneAnotherPart {
    pub part_id: String,
    pub owner: String,
}

impl WelcomeOneAnotherPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }

    /// Event-to-obligation mapping for the initial "arrival" event.
    pub fn identifies_obligation(&self, event_type: &str) -> bool {
        event_type == "VisitorArrival"
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WelcomeInput {
    pub visitor_id: String,
    pub arrival_timestamp: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct WelcomeOutput {
    pub status: String,
    pub scripture_basis: String,
    pub terminal_state: String,
    pub owner: String,
    pub welcome_timestamp: String,
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
impl PartExecutor for WelcomeOneAnotherPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: WelcomeInput = serde_json::from_slice(&input)
            .map_err(|e| crate::utils::error::GgenError::TemplateError(format!("Invalid input JSON: {}", e)))?;
        
        let now = Utc::now();
        let activity_id = format!("welcome-{}", Uuid::new_v4());
        
        let prov_receipt = ProvReceipt {
            activity_id: activity_id.clone(),
            agent_id: self.owner.clone(), // The cell acts as the agent/steward
            entity_id: input_json.visitor_id.clone(),
            generated_at_time: now.to_rfc3339(),
            scripture: "Romans 15:7".to_string(),
        };

        let output = WelcomeOutput {
            status: "Success".to_string(),
            scripture_basis: "Romans 15:7".to_string(),
            terminal_state: "ReceivedAndRemembered".to_string(),
            owner: self.owner.clone(),
            welcome_timestamp: now.to_rfc3339(),
            prov_receipt,
        };
        
        let output_bytes = serde_json::to_vec(&output).unwrap();
        
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
            ExecutionStatus::Success,
            1, // duration placeholder
        );
        
        Ok((output_bytes, packet))
    }
}

/// Stewardship Part: ConsentGatePart
/// 
/// Scripture: Galatians 6:2 - "Carry each other’s burdens, and in this way you will fulfill the law of Christ."
/// Refusal Law: No follow-up activity is admitted without an explicit stpnt:ConsentReceived event.
/// Terminal State: ConsentAdmitted | ConsentRefused
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
    pub scripture_basis: String,
    pub refusal_evidence: Option<String>,
    pub terminal_state: String,
    pub owner: String,
    pub prov_receipt: ProvReceipt,
}

#[async_trait]
impl PartExecutor for ConsentGatePart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: ConsentInput = serde_json::from_slice(&input)
            .map_err(|e| crate::utils::error::GgenError::TemplateError(format!("Invalid input: {}", e)))?;
        
        let now = Utc::now();
        let activity_id = format!("consent-gate-{}", Uuid::new_v4());
        
        let (status, terminal_state, refusal_evidence, execution_status) = if input_json.has_consent_receipt {
            ("Admitted".to_string(), "ConsentAdmitted".to_string(), None, ExecutionStatus::Success)
        } else {
            ("Refused".to_string(), "ConsentRefused".to_string(), Some("Refusal: No stpnt:ConsentReceived found for follow-up attempt.".to_string()), ExecutionStatus::Refused)
        };

        let output = ConsentOutput {
            status,
            scripture_basis: "Galatians 6:2".to_string(),
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

        let output_bytes = serde_json::to_vec(&output).unwrap();
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
            1,
        );
        
        Ok((output_bytes, packet))
    }
}

/// Stewardship Part: AssignStewardPart
/// 
/// Scripture: 1 Corinthians 4:2 - "Moreover it is required in stewards, that a man be found faithful."
pub struct AssignStewardPart {
    pub part_id: String,
    pub owner: String,
}

impl AssignStewardPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssignInput {
    pub obligation_id: String,
    pub steward_uri: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AssignOutput {
    pub status: String,
    pub terminal_state: String,
    pub prov_receipt: ProvReceipt,
}

#[async_trait]
impl PartExecutor for AssignStewardPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: AssignInput = serde_json::from_slice(&input)
            .map_err(|e| crate::utils::error::GgenError::TemplateError(format!("Invalid input: {}", e)))?;
        
        let now = Utc::now();
        let output = AssignOutput {
            status: "Assigned".to_string(),
            terminal_state: "StewardBound".to_string(),
            prov_receipt: ProvReceipt {
                activity_id: format!("assign-{}", Uuid::new_v4()),
                agent_id: self.owner.clone(),
                entity_id: input_json.obligation_id.clone(),
                generated_at_time: now.to_rfc3339(),
                scripture: "1 Corinthians 4:2".to_string(),
            },
        };

        let output_bytes = serde_json::to_vec(&output).unwrap();
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

/// Stewardship Part: FollowUpObligationPart
/// 
/// Scripture: 1 Corinthians 4:2 - "Moreover it is required in stewards, that a man be found faithful."
pub struct FollowUpObligationPart {
    pub part_id: String,
    pub owner: String,
}

impl FollowUpObligationPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FollowUpInput {
    pub obligation_id: String,
    pub deadline: String, // ISO8601
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FollowUpOutput {
    pub status: String,
    pub terminal_state: String,
}

#[async_trait]
impl PartExecutor for FollowUpObligationPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: FollowUpInput = serde_json::from_slice(&input).unwrap();
        let deadline = chrono::DateTime::parse_from_rfc3339(&input_json.deadline).unwrap();
        
        let (status, terminal_state, execution_status) = if Utc::now() > deadline {
            ("Escalated".to_string(), "Overdue".to_string(), ExecutionStatus::Refused)
        } else {
            ("Pending".to_string(), "InProgress".to_string(), ExecutionStatus::Success)
        };

        let output = FollowUpOutput { status: status.to_string(), terminal_state: terminal_state.to_string() };
        let output_bytes = serde_json::to_vec(&output).unwrap();
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

/// Stewardship Part: PentecostInvitationPart
/// 
/// Scripture: 1 Corinthians 12 - Distributed Gifts
pub struct PentecostInvitationPart {
    pub part_id: String,
    pub owner: String,
}

impl PentecostInvitationPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct InvitationInput {
    pub person_id: String,
    pub is_capable_of_receiving_another: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct InvitationOutput {
    pub status: String,
    pub terminal_state: String,
}

#[async_trait]
impl PartExecutor for PentecostInvitationPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: InvitationInput = serde_json::from_slice(&input).unwrap();
        
        let (status, terminal_state) = if input_json.is_capable_of_receiving_another {
            ("Incorporated".to_string(), "IncorporatedAndServing".to_string())
        } else {
            ("Developing".to_string(), "FormationPending".to_string())
        };

        let output = InvitationOutput { status: status.to_string(), terminal_state: terminal_state.to_string() };
        let output_bytes = serde_json::to_vec(&output).unwrap();
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

/// Stewardship Part: ContinuityWatchPart
/// 
/// Scripture: Hebrews 13:17 - "keeping watch over your souls"
pub struct ContinuityWatchPart {
    pub part_id: String,
    pub owner: String,
}

impl ContinuityWatchPart {
    pub fn new(part_id: String, owner: String) -> Self {
        Self { part_id, owner }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ContinuityInput {
    pub last_activity: String, // ISO8601
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ContinuityOutput {
    pub alert: bool,
    pub message: String,
}

#[async_trait]
impl PartExecutor for ContinuityWatchPart {
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)> {
        let input_json: ContinuityInput = serde_json::from_slice(&input).unwrap();
        let last_act = chrono::DateTime::parse_from_rfc3339(&input_json.last_activity).unwrap();
        
        let alert = Utc::now() - last_act.with_timezone(&Utc) > chrono::Duration::days(7);
        let output = ContinuityOutput {
            alert,
            message: if alert { "Alert: Silent disappearance detected." } else { "Continuity verified." }.to_string(),
        };

        let output_bytes = serde_json::to_vec(&output).unwrap();
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
    async fn test_welcome_one_another_part_execution() {
        let part = WelcomeOneAnotherPart::new(
            "WelcomeOneAnotherPart-001".to_string(),
            "StewardshipCell-Alpha".to_string()
        );
        
        assert!(part.identifies_obligation("VisitorArrival"));

        let input = WelcomeInput {
            visitor_id: "visitor-123".to_string(),
            arrival_timestamp: Utc::now().to_rfc3339(),
        };
        
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, packet) = part.execute(input_bytes).await.expect("Execution failed");
        let output: WelcomeOutput = serde_json::from_slice(&output_bytes).expect("Invalid output JSON");
        
        assert_eq!(output. scripture_basis, "Romans 15:7");
        assert_eq!(output.terminal_state, "ReceivedAndRemembered");
        assert_eq!(packet.status, ExecutionStatus::Success);
    }

    #[tokio::test]
    async fn test_consent_gate_part_refusal() {
        let part = ConsentGatePart::new("ConsentGate-001".to_string(), "StewardshipCell".to_string());
        let input = ConsentInput {
            person_id: "person-456".to_string(),
            follow_up_activity: "phone-call".to_string(),
            has_consent_receipt: false,
        };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, packet) = part.execute(input_bytes).await.unwrap();
        let output: ConsentOutput = serde_json::from_slice(&output_bytes).unwrap();
        
        assert_eq!(output.status, "Refused");
        assert_eq!(packet.status, ExecutionStatus::Refused);
    }

    #[tokio::test]
    async fn test_assign_steward_part() {
        let part = AssignStewardPart::new("Assign-001".to_string(), "StewardshipCell".to_string());
        let input = AssignInput {
            obligation_id: "obl-789".to_string(),
            steward_uri: "canon:Paul".to_string(),
        };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, _packet) = part.execute(input_bytes).await.unwrap();
        let output: AssignOutput = serde_json::from_slice(&output_bytes).unwrap();
        
        assert_eq!(output.terminal_state, "StewardBound");
        assert_eq!(output.prov_receipt.scripture, "1 Corinthians 4:2");
    }

    #[tokio::test]
    async fn test_follow_up_escalation() {
        let part = FollowUpObligationPart::new("FollowUp-001".to_string(), "StewardshipCell".to_string());
        let input = FollowUpInput {
            obligation_id: "obl-789".to_string(),
            deadline: (Utc::now() - chrono::Duration::days(1)).to_rfc3339(),
        };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, packet) = part.execute(input_bytes).await.unwrap();
        let output: FollowUpOutput = serde_json::from_slice(&output_bytes).unwrap();
        
        assert_eq!(output.status, "Escalated");
        assert_eq!(packet.status, ExecutionStatus::Refused);
    }

    #[tokio::test]
    async fn test_pentecost_incorporation() {
        let part = PentecostInvitationPart::new("Pentecost-001".to_string(), "StewardshipCell".to_string());
        let input = InvitationInput {
            person_id: "person-123".to_string(),
            is_capable_of_receiving_another: true,
        };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, _packet) = part.execute(input_bytes).await.unwrap();
        let output: InvitationOutput = serde_json::from_slice(&output_bytes).unwrap();
        
        assert_eq!(output.status, "Incorporated");
        assert_eq!(output.terminal_state, "IncorporatedAndServing");
    }

    #[tokio::test]
    async fn test_continuity_watch_alert() {
        let part = ContinuityWatchPart::new("Watch-001".to_string(), "StewardshipCell".to_string());
        let input = ContinuityInput {
            last_activity: (Utc::now() - chrono::Duration::days(10)).to_rfc3339(),
        };
        let input_bytes = serde_json::to_vec(&input).unwrap();
        let (output_bytes, _packet) = part.execute(input_bytes).await.unwrap();
        let output: ContinuityOutput = serde_json::from_slice(&output_bytes).unwrap();
        
        assert!(output.alert);
        assert!(output.message.contains("Silent disappearance"));
    }
}
