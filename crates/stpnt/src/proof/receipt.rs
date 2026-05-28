use serde::{Deserialize, Serialize};
use chrono::{Utc, DateTime};
use blake3;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct StewardshipReceipt {
    pub input_state_hash: [u8; 32],
    pub canon_hash: [u8; 32],
    pub route_hash: [u8; 32],
    pub obligation_hash: [u8; 32],
    pub prior_receipt_hash: Option<[u8; 32]>,
    pub actor_hash: [u8; 32],
    pub output_state_hash: [u8; 32],
    pub timestamp: DateTime<Utc>,
    pub signature: Vec<u8>,
}

impl StewardshipReceipt {
    pub fn new(
        input: &[u8],
        canon: &[&str],
        prior_hash: Option<[u8; 32]>,
        actor: &str,
        output: &[u8],
    ) -> Self {
        let mut canon_hasher = blake3::Hasher::new();
        for c in canon {
            canon_hasher.update(c.as_bytes());
        }
        
        Self {
            input_state_hash: blake3::hash(input).into(),
            canon_hash: canon_hasher.finalize().into(),
            route_hash: [0u8; 32], // placeholder
            obligation_hash: [0u8; 32], // placeholder
            prior_receipt_hash: prior_hash,
            actor_hash: blake3::hash(actor.as_bytes()).into(),
            output_state_hash: blake3::hash(output).into(),
            timestamp: Utc::now(),
            signature: vec![], // placeholder for real signature
        }
    }
}
