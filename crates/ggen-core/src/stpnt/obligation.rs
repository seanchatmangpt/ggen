use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CanonSet {
    pub scripture: String,
    pub aa_structure: String,
    pub avatar: String,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum TerminalState {
    ReceivedAndRemembered,
    ConsentAdmitted,
    ConsentRefused,
    StewardBound,
    SuccessfulIncorporate,
    IncorporatedAndServing,
    Refusal,
    Retraction,
    Overdue,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum ObligationStatus {
    Pending,
    InProgress,
    Escalated,
    Completed,
    Failed,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct StewardshipObligation {
    pub id: Uuid,
    pub canon: CanonSet,
    pub steward_id: Option<String>,
    pub person_id: String,
    pub status: ObligationStatus,
    pub terminal_state: Option<TerminalState>,
    pub created_at: DateTime<Utc>,
    pub deadline: Option<DateTime<Utc>>,
}

impl StewardshipObligation {
    pub fn new(person_id: String, canon: CanonSet) -> Self {
        Self {
            id: Uuid::new_v4(),
            canon,
            steward_id: None,
            person_id,
            status: ObligationStatus::Pending,
            terminal_state: None,
            created_at: Utc::now(),
            deadline: None,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum StewardshipEvent {
    VisitorArrival,
    FollowUpAttempt,
    ConsentReceived,
    MemberIncorporation,
    ContinuityCheck,
}
