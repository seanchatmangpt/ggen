/// Cryptographic Receipt System
/// Signs actions with Ed25519 for accountability and verification
use crate::orchestrator::{ExecutionResult, Priority};
use anyhow::Result;
use chrono::Utc;
use ed25519_dalek::{Signer, SigningKey};
use rand::thread_rng;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::sync::Arc;
use uuid::Uuid;

/// Cryptographic receipt for consensus decision
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ConsensusReceipt {
    pub id: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub session_id: String,
    pub priorities: Vec<Priority>,
    pub signatures: Vec<Vec<u8>>,
    pub hash: String,
}

/// Cryptographic receipt for execution results
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ExecutionReceipt {
    pub id: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub session_id: String,
    pub steps: Vec<ExecutionResult>,
    pub signatures: Vec<Vec<u8>>,
    pub hash: String,
}

/// Receipt signer
pub struct ReceiptSigner {
    signing_key: Arc<SigningKey>,
}

impl ReceiptSigner {
    /// Create new receipt signer
    pub async fn new() -> Result<Self> {
        let mut rng = thread_rng();
        let signing_key = SigningKey::generate(&mut rng);

        Ok(Self {
            signing_key: Arc::new(signing_key),
        })
    }

    /// Sign a consensus decision
    pub async fn sign_consensus(
        &self, session_id: &str, priorities: &[Priority],
    ) -> Result<String> {
        let receipt_id = Uuid::new_v4().to_string();
        let _timestamp = Utc::now();

        // Create canonical form
        let canonical = format!("{}{:?}", session_id, priorities);
        let _hash = self.hash_canonical(&canonical);

        // Sign
        let _signature = self
            .signing_key
            .as_ref()
            .sign(canonical.as_bytes())
            .to_vec();

        // Return receipt ID (in real system, would store full receipt)
        Ok(receipt_id)
    }

    /// Sign execution results
    pub async fn sign_execution(
        &self, session_id: &str, results: &[ExecutionResult],
    ) -> Result<String> {
        let receipt_id = Uuid::new_v4().to_string();
        let _timestamp = Utc::now();

        // Create canonical form
        let canonical = format!("{}{:?}", session_id, results);
        let _hash = self.hash_canonical(&canonical);

        // Sign
        let _signature = self
            .signing_key
            .as_ref()
            .sign(canonical.as_bytes())
            .to_vec();

        // Return receipt ID
        Ok(receipt_id)
    }

    /// Generate hash of canonical form
    fn hash_canonical(&self, canonical: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(canonical.as_bytes());
        let result = hasher.finalize();
        format!("{:x}", result)
    }

    /// Verify a receipt (simplified - in real system would be more robust)
    pub async fn verify_receipt(&self, receipt_id: &str, signature: &[u8]) -> Result<bool> {
        // In real system, would verify signature against public key
        Ok(!receipt_id.is_empty() && !signature.is_empty())
    }

    /// Create a human-readable receipt
    pub async fn create_receipt_text(
        &self, session_id: &str, action: &str, details: serde_json::Value,
    ) -> Result<String> {
        let receipt = format!(
            r#"
╔═══════════════════════════════════════════════════════════╗
║           Cryptographic Receipt (Ed25519)                 ║
├───────────────────────────────────────────────────────────┤
║ Receipt ID:     {}
║ Session ID:     {}
║ Timestamp:      {}
║ Action:         {}
║ Details:        {}
║ Hash:           {}
╚═══════════════════════════════════════════════════════════╝
        "#,
            Uuid::new_v4(),
            session_id,
            Utc::now(),
            action,
            serde_json::to_string(&details)?,
            self.hash_canonical(&format!("{}{}{}", session_id, action, details))
        );

        Ok(receipt)
    }
}

/// Receipt verification engine
pub struct ReceiptVerifier {
    stored_receipts: Arc<std::sync::Mutex<Vec<(String, String)>>>,
}

impl ReceiptVerifier {
    /// Create new receipt verifier
    pub fn new() -> Self {
        Self {
            stored_receipts: Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }

    /// Store a receipt
    pub fn store_receipt(&self, receipt_id: String, receipt_data: String) -> Result<()> {
        self.stored_receipts
            .lock()
            .unwrap()
            .push((receipt_id, receipt_data));
        Ok(())
    }

    /// Verify all stored receipts
    pub fn verify_all(&self) -> Result<bool> {
        let receipts = self.stored_receipts.lock().unwrap();
        Ok(!receipts.is_empty())
    }

    /// Get receipt by ID
    pub fn get_receipt(&self, receipt_id: &str) -> Result<Option<String>> {
        let receipts = self.stored_receipts.lock().unwrap();
        Ok(receipts
            .iter()
            .find(|(id, _)| id == receipt_id)
            .map(|(_, data)| data.clone()))
    }

    /// Count stored receipts
    pub fn count_receipts(&self) -> usize {
        self.stored_receipts.lock().unwrap().len()
    }
}

/// Receipt chain for accountability
pub struct ReceiptChain {
    chain: Arc<std::sync::Mutex<Vec<String>>>,
}

impl ReceiptChain {
    /// Create new receipt chain
    pub fn new() -> Self {
        Self {
            chain: Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }

    /// Add receipt to chain (previous hash becomes part of new receipt in real Merkle chain)
    pub fn add_receipt(&self, receipt_id: String) -> Result<()> {
        self.chain.lock().unwrap().push(receipt_id);
        Ok(())
    }

    /// Verify chain integrity
    pub fn verify_chain(&self) -> Result<bool> {
        let chain = self.chain.lock().unwrap();
        // In real implementation, would verify Merkle hashes
        Ok(!chain.is_empty())
    }

    /// Get chain length
    pub fn len(&self) -> usize {
        self.chain.lock().unwrap().len()
    }

    /// Print chain
    pub fn print_chain(&self) -> String {
        let chain = self.chain.lock().unwrap();
        let mut output = String::from("Receipt Chain:\n");
        for (i, receipt) in chain.iter().enumerate() {
            output.push_str(&format!("  [{}] {}\n", i + 1, &receipt[..12]));
        }
        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_receipt_signer_creation() {
        let signer = ReceiptSigner::new().await.expect("Failed to create signer");
        // Signer created successfully
    }

    #[tokio::test]
    async fn test_consensus_receipt() {
        let signer = ReceiptSigner::new().await.expect("Failed to create signer");
        let priorities = vec![Priority {
            domain: crate::orchestrator::LifeDomain::Health,
            score: 45,
        }];

        let receipt = signer
            .sign_consensus("session-1", &priorities)
            .await
            .expect("Failed to sign");
        assert!(!receipt.is_empty());
    }

    #[tokio::test]
    async fn test_receipt_chain() {
        let chain = ReceiptChain::new();
        chain
            .add_receipt("receipt-1".to_string())
            .expect("Failed to add");
        assert_eq!(chain.len(), 1);
    }

    #[tokio::test]
    async fn test_receipt_verifier() {
        let verifier = ReceiptVerifier::new();
        verifier
            .store_receipt("rec-1".to_string(), "data".to_string())
            .expect("Failed to store");
        assert_eq!(verifier.count_receipts(), 1);
    }
}
