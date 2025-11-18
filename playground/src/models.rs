//! Core data models for HTF

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Δ-Shard: Atomic unit of thesis work
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeltaShard {
    pub id: String,
    pub name: String,
    pub family: ShardFamily,
    pub content: String,
    pub status: ShardStatus,
    pub dependencies: Vec<String>, // IDs of parent shards
}

/// Family of Δ-Shards (research categories)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ShardFamily {
    // IMRaD
    Intro,
    Method,
    Result,
    Discussion,

    // Thesis-by-Papers
    Paper,
    Synthesis,

    // Argument
    Claim,
    Ground,
    Proof,
    Objection,
    Reply,

    // Contribution
    Gap,
    Design,
    Evaluation,
    Impact,

    // Monograph
    Context,
    Canon,
    Analysis,
    Conclusion,

    // DSR
    Problem,
    Artifact,
    Theory,

    // Narrative
    Field,
    Voice,
    Pattern,
    Insight,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ShardStatus {
    Draft,
    InProgress,
    Review,
    Final,
}

/// Λ-Ordering: Total order across all families
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LambdaOrder {
    pub shards: Vec<String>, // Ordered list of shard IDs
}

/// Π-Profile: How research shards map to HTF categories
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PiProfile {
    pub thesis_id: String,
    pub shards: HashMap<String, DeltaShard>,
    pub coverage: HashMap<ShardFamily, f32>, // Percentage coverage per family
    pub total_words: usize,
}

/// Q-Invariants: Constraints that must hold
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Invariant {
    pub name: String,
    pub description: String,
    pub constraint_type: ConstraintType,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ConstraintType {
    AllFamiliesCovered,
    NoCyclicDependencies,
    TotalOrderPreserved,
    ContentNotEmpty,
    StatusConsistent,
}

/// Γ-Check Result: Validation against Q-invariants
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GammaCheckResult {
    pub is_valid: bool,
    pub invariants_passed: Vec<String>,
    pub invariants_failed: Vec<String>,
    pub drift_detected: Vec<String>,
    pub recommendations: Vec<String>,
}

/// Chapter outline from Λ-scheduling
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChapterPlan {
    pub thesis_id: String,
    pub chapters: Vec<Chapter>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Chapter {
    pub number: usize,
    pub title: String,
    pub shards: Vec<String>, // IDs of shards in this chapter
    pub estimated_words: usize,
    pub families: Vec<ShardFamily>,
}
