//! Receipt and Replay: The deterministic proof of consequence

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// A canonical capture of a semantic operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt<L, C, A, E> {
    pub law_id: String,
    pub law_version: String,
    pub field_raw: u8,
    pub selected_condition: C,
    pub admitted_consequence: A,
    pub exit: E,
    pub sequence: u64,
    pub timestamp: DateTime<Utc>,
    pub digest: String,
    _marker: std::marker::PhantomData<L>,
}

/// The result of a replay operation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Replay<R> {
    Ok(R),
    Mismatch(String),
    LawMismatch { expected: String, actual: String },
}
