use serde::{Serialize, Deserialize};

pub type TermId = u64;
pub type PredicateId = u32;
pub type RuleId = u32;
pub type SourceId = u32;
pub type PlanId = u32;
pub type EpochId = u64;
pub type Hash = [u8; 32];
pub type ProofNodeId = u32;
pub type SubstitutionId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProofMode {
    Positive,
    Negative,
    Full,
}

impl std::fmt::Display for ProofMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ProofMode::Positive => "Positive",
            ProofMode::Negative => "Negative",
            ProofMode::Full => "Full",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProofKind {
    Fact,
    Rule,
    Builtin,
    Negation,
    AntiJoin,
    Aggregate,
    Foreign,
    MissingFact,
    FailedJoin,
    BlockedAlternative,
}

impl std::fmt::Display for ProofKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            ProofKind::Fact => "Fact",
            ProofKind::Rule => "Rule",
            ProofKind::Builtin => "Builtin",
            ProofKind::Negation => "Negation",
            ProofKind::AntiJoin => "AntiJoin",
            ProofKind::Aggregate => "Aggregate",
            ProofKind::Foreign => "Foreign",
            ProofKind::MissingFact => "MissingFact",
            ProofKind::FailedJoin => "FailedJoin",
            ProofKind::BlockedAlternative => "BlockedAlternative",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DecisionKind {
    Allow,
    Deny,
    Escalate,
    Unknown,
    Invalid,
}

impl std::fmt::Display for DecisionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            DecisionKind::Allow => "Allow",
            DecisionKind::Deny => "Deny",
            DecisionKind::Escalate => "Escalate",
            DecisionKind::Unknown => "Unknown",
            DecisionKind::Invalid => "Invalid",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Atom8 {
    pub pred_id: PredicateId,
    pub arity: u8,
    pub binding_mask: u8,
    pub args: [TermId; 8],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Rule8 {
    pub rule_id: RuleId,
    pub head: Atom8,
    pub body: [Atom8; 8],
    pub body_len: u8,
    pub body_mask: u8,
    pub negation_mask: u8,
    pub builtin_mask: u8,
    pub var_count: u8,
    pub var_live_mask: u8,
    pub feature_mask: u8,
    pub proof_mask: u8,
    pub plan_id: PlanId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct QueryAtom8 {
    pub atom: Atom8,
    pub output_mask: u8,
    pub proof_mode: ProofMode,
    pub epoch: EpochId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct FactRow8 {
    pub pred_id: PredicateId,
    pub arity: u8,
    pub args: [TermId; 8],
    pub source_id: SourceId,
    pub fact_hash: Hash,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FactBlock8 {
    pub pred_id: PredicateId,
    pub arity: u8,
    pub arg_order: [u8; 8],
    pub row_count: u32,
    pub rows: Vec<FactRow8>,
    pub block_hash: Hash,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProofNode {
    pub node_id: ProofNodeId,
    pub kind: ProofKind,
    pub pred_id: PredicateId,
    pub rule_id: Option<RuleId>,
    pub fact_hash: Option<Hash>,
    pub children: [ProofNodeId; 8],
    pub child_count: u8,
    pub substitution_id: SubstitutionId,
    pub node_hash: Hash,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Receipt {
    pub engine_version: String,
    pub catalog_root: Hash,
    pub rule_root: Hash,
    pub fact_root: Hash,
    pub input_root: Hash,
    pub proof_root: Hash,
    pub output_root: Hash,
    pub decision: DecisionKind,
    pub epoch: EpochId,
    pub receipt_hash: Hash,
}
