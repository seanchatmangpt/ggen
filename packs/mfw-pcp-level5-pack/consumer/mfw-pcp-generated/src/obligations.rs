//! Generated theorem-target ledger. A declaration is not proof standing.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ObligationStatus { Proved, Blocked, Unknown, Unsupported }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProofObligation { pub id: &'static str, pub statement: &'static str, pub status: ObligationStatus }

pub const ALL: &[ProofObligation] = &[
    ProofObligation { id: "MFW-KERNEL-001", statement: "ker(tau_Pi) equals admitted Mazurkiewicz trace equivalence", status: ObligationStatus::Blocked },
    ProofObligation { id: "MFW-PCP-LIVE-001", statement: "the generated consumer rebuild is byte-identical and passes the live verifier ladder", status: ObligationStatus::Unknown },
];

pub fn all_proved() -> bool { ALL.iter().all(|item| item.status == ObligationStatus::Proved) }
