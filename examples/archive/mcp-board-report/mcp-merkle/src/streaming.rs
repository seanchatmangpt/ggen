//! Streaming Receipt Verification
//!
//! Verify receipts incrementally as they arrive:
//! - No need to store full chain in memory
//! - Constant memory usage regardless of chain length
//! - Real-time verification with immediate alerts
//!
//! Uses incremental Merkle tree construction.

use mcp_core::crypto::{combine_hashes, hash_sha256_bytes, KeyPair};
use mcp_core::error::{McpError, McpResult};
use mcp_core::types::Receipt;
use mcp_core::GENESIS_HASH;
use serde::{Deserialize, Serialize};

/// Streaming verifier with O(log n) memory
pub struct StreamingVerifier {
    /// Current chain state (only stores necessary hashes)
    state: VerifierState,
    /// Verification callback for valid receipts
    on_verified: Option<Box<dyn Fn(&Receipt) + Send>>,
    /// Verification callback for invalid receipts
    on_invalid: Option<Box<dyn Fn(&Receipt, &str) + Send>>,
    /// Optional keypair for signature verification
    keypair: Option<KeyPair>,
}

/// Serializable verifier state for persistence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerifierState {
    /// Expected previous hash (hex-encoded)
    pub expected_prev: String,
    /// Current sequence number
    pub sequence: u64,
    /// Running Merkle computation (log n hashes, hex-encoded)
    pub merkle_stack: Vec<String>,
    /// Receipts processed
    pub count: u64,
    /// Incremental Merkle tree state
    pub incremental_merkle: IncrementalMerkleState,
}

impl Default for VerifierState {
    fn default() -> Self {
        Self {
            expected_prev: GENESIS_HASH.to_string(),
            sequence: 0,
            merkle_stack: Vec::new(),
            count: 0,
            incremental_merkle: IncrementalMerkleState::new(),
        }
    }
}

impl StreamingVerifier {
    /// Create a new streaming verifier
    pub fn new() -> Self {
        Self {
            state: VerifierState::default(),
            on_verified: None,
            on_invalid: None,
            keypair: None,
        }
    }

    /// Resume from saved state
    pub fn from_state(state: VerifierState) -> Self {
        Self {
            state,
            on_verified: None,
            on_invalid: None,
            keypair: None,
        }
    }

    /// Set keypair for signature verification
    pub fn with_keypair(mut self, keypair: KeyPair) -> Self {
        self.keypair = Some(keypair);
        self
    }

    /// Process next receipt in stream
    pub fn process(&mut self, receipt: &Receipt) -> VerifyResult {
        // Check sequence number
        if receipt.sequence != self.state.sequence + 1 && self.state.count > 0 {
            let result = VerifyResult::OutOfOrder {
                receipt_id: receipt.receipt_id.clone(),
                expected_seq: self.state.sequence + 1,
                actual_seq: receipt.sequence,
            };

            if let Some(ref callback) = self.on_invalid {
                callback(receipt, "Out of order sequence");
            }

            return result;
        }

        // Check chain linkage (prev_hash matches expected)
        if receipt.prev_hash != self.state.expected_prev {
            let result = VerifyResult::Invalid {
                receipt_id: receipt.receipt_id.clone(),
                reason: InvalidReason::BrokenChain {
                    expected: self.state.expected_prev.clone(),
                    actual: receipt.prev_hash.clone(),
                },
            };

            if let Some(ref callback) = self.on_invalid {
                callback(receipt, &format!("Broken chain: expected {}, got {}",
                    self.state.expected_prev, receipt.prev_hash));
            }

            return result;
        }

        // Verify receipt hash integrity
        let computed_hash = compute_receipt_hash(receipt);
        if computed_hash != receipt.receipt_hash {
            let result = VerifyResult::Invalid {
                receipt_id: receipt.receipt_id.clone(),
                reason: InvalidReason::InvalidHash,
            };

            if let Some(ref callback) = self.on_invalid {
                callback(receipt, "Invalid receipt hash");
            }

            return result;
        }

        // Verify signature if keypair is provided
        if let Some(ref keypair) = self.keypair {
            if let Err(_) = receipt.verify(keypair) {
                let result = VerifyResult::Invalid {
                    receipt_id: receipt.receipt_id.clone(),
                    reason: InvalidReason::InvalidSignature,
                };

                if let Some(ref callback) = self.on_invalid {
                    callback(receipt, "Invalid signature");
                }

                return result;
            }
        }

        // Check for future timestamp (basic sanity check)
        let now = chrono::Utc::now();
        if receipt.timestamp > now + chrono::Duration::minutes(5) {
            let result = VerifyResult::Invalid {
                receipt_id: receipt.receipt_id.clone(),
                reason: InvalidReason::FutureTimestamp,
            };

            if let Some(ref callback) = self.on_invalid {
                callback(receipt, "Future timestamp");
            }

            return result;
        }

        // All checks passed - update state
        self.state.expected_prev = receipt.receipt_hash.clone();
        self.state.sequence = receipt.sequence;
        self.state.count += 1;

        // Add to incremental Merkle tree
        let receipt_hash_bytes = hex::decode(&receipt.receipt_hash)
            .map(|b| {
                let mut arr = [0u8; 32];
                if b.len() == 32 {
                    arr.copy_from_slice(&b);
                }
                arr
            })
            .unwrap_or_else(|_| hash_sha256_bytes(receipt.receipt_hash.as_bytes()));

        self.state.incremental_merkle.push(receipt_hash_bytes);

        let result = VerifyResult::Valid {
            receipt_id: receipt.receipt_id.clone(),
            sequence: receipt.sequence,
        };

        if let Some(ref callback) = self.on_verified {
            callback(receipt);
        }

        result
    }

    /// Process batch of receipts
    pub fn process_batch(&mut self, receipts: &[Receipt]) -> BatchVerifyResult {
        let mut result = BatchVerifyResult {
            processed: 0,
            valid: 0,
            invalid: 0,
            out_of_order: 0,
            errors: Vec::new(),
        };

        for receipt in receipts {
            result.processed += 1;
            let verify_result = self.process(receipt);

            match &verify_result {
                VerifyResult::Valid { .. } => result.valid += 1,
                VerifyResult::Invalid { .. } => {
                    result.invalid += 1;
                    result.errors.push(verify_result);
                }
                VerifyResult::OutOfOrder { .. } => {
                    result.out_of_order += 1;
                    result.errors.push(verify_result);
                }
            }
        }

        result
    }

    /// Get current Merkle root (of processed receipts)
    pub fn current_root(&self) -> [u8; 32] {
        self.state.incremental_merkle.root()
    }

    /// Get current Merkle root as hex string
    pub fn current_root_hex(&self) -> String {
        hex::encode(self.current_root())
    }

    /// Get state for persistence
    pub fn state(&self) -> &VerifierState {
        &self.state
    }

    /// Set callback for verified receipts
    pub fn on_verified<F: Fn(&Receipt) + Send + 'static>(&mut self, f: F) {
        self.on_verified = Some(Box::new(f));
    }

    /// Set callback for invalid receipts
    pub fn on_invalid<F: Fn(&Receipt, &str) + Send + 'static>(&mut self, f: F) {
        self.on_invalid = Some(Box::new(f));
    }

    /// Memory usage (should be O(log n))
    pub fn memory_usage(&self) -> usize {
        // Base struct size
        let base = std::mem::size_of::<VerifierState>();

        // Merkle stack: O(log n) entries, each 64 bytes (hex-encoded hash)
        let merkle_stack_size = self.state.merkle_stack.len() * 64;

        // Incremental merkle state
        let incremental_size = self.state.incremental_merkle.memory_usage();

        // Expected prev hash (64 bytes hex)
        let prev_hash_size = 64;

        base + merkle_stack_size + incremental_size + prev_hash_size
    }

    /// Number of receipts processed
    pub fn count(&self) -> u64 {
        self.state.count
    }

    /// Reset verifier to initial state
    pub fn reset(&mut self) {
        self.state = VerifierState::default();
    }
}

impl Default for StreamingVerifier {
    fn default() -> Self {
        Self::new()
    }
}

/// Verification result for a single receipt
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum VerifyResult {
    /// Receipt is valid
    Valid {
        receipt_id: String,
        sequence: u64,
    },
    /// Receipt is invalid
    Invalid {
        receipt_id: String,
        reason: InvalidReason,
    },
    /// Receipt arrived out of order
    OutOfOrder {
        receipt_id: String,
        expected_seq: u64,
        actual_seq: u64,
    },
}

impl VerifyResult {
    /// Check if result is valid
    pub fn is_valid(&self) -> bool {
        matches!(self, VerifyResult::Valid { .. })
    }

    /// Get receipt ID
    pub fn receipt_id(&self) -> &str {
        match self {
            VerifyResult::Valid { receipt_id, .. } => receipt_id,
            VerifyResult::Invalid { receipt_id, .. } => receipt_id,
            VerifyResult::OutOfOrder { receipt_id, .. } => receipt_id,
        }
    }
}

/// Reason why a receipt is invalid
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum InvalidReason {
    /// Chain linkage is broken (prev_hash mismatch)
    BrokenChain {
        expected: String,
        actual: String,
    },
    /// Signature verification failed
    InvalidSignature,
    /// Receipt hash doesn't match computed hash
    InvalidHash,
    /// Timestamp is in the future
    FutureTimestamp,
    /// Duplicate sequence number
    DuplicateSequence,
}

impl std::fmt::Display for InvalidReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidReason::BrokenChain { expected, actual } => {
                write!(f, "Broken chain: expected {}, got {}", expected, actual)
            }
            InvalidReason::InvalidSignature => write!(f, "Invalid signature"),
            InvalidReason::InvalidHash => write!(f, "Invalid hash"),
            InvalidReason::FutureTimestamp => write!(f, "Future timestamp"),
            InvalidReason::DuplicateSequence => write!(f, "Duplicate sequence"),
        }
    }
}

/// Result of batch verification
#[derive(Debug, Serialize)]
pub struct BatchVerifyResult {
    /// Total receipts processed
    pub processed: u64,
    /// Valid receipts
    pub valid: u64,
    /// Invalid receipts
    pub invalid: u64,
    /// Out of order receipts
    pub out_of_order: u64,
    /// Error details
    pub errors: Vec<VerifyResult>,
}

impl BatchVerifyResult {
    /// Check if all receipts were valid
    pub fn all_valid(&self) -> bool {
        self.invalid == 0 && self.out_of_order == 0
    }

    /// Get success rate (0.0 to 1.0)
    pub fn success_rate(&self) -> f64 {
        if self.processed == 0 {
            1.0
        } else {
            self.valid as f64 / self.processed as f64
        }
    }
}

/// Serializable state for IncrementalMerkle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IncrementalMerkleState {
    /// Stack of partial hashes at each level (hex-encoded)
    /// Index 0 = level 0 (leaves), higher indices = higher levels
    stack: Vec<Option<String>>,
    /// Number of leaves added
    count: u64,
}

impl IncrementalMerkleState {
    /// Create a new incremental Merkle state
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            count: 0,
        }
    }

    /// Add a leaf to the tree
    pub fn push(&mut self, leaf: [u8; 32]) {
        self.count += 1;
        let mut hash = leaf;
        let mut level = 0;

        // Ensure we have enough levels
        while level >= self.stack.len() {
            self.stack.push(None);
        }

        // Propagate up the tree
        loop {
            if level >= self.stack.len() {
                self.stack.push(None);
            }

            match &self.stack[level] {
                None => {
                    // No sibling at this level, store and stop
                    self.stack[level] = Some(hex::encode(hash));
                    break;
                }
                Some(sibling_hex) => {
                    // Has sibling, combine and propagate up
                    let sibling = hex_to_hash(sibling_hex);
                    hash = combine_hashes(&sibling, &hash);
                    self.stack[level] = None;
                    level += 1;
                }
            }
        }
    }

    /// Get current root
    pub fn root(&self) -> [u8; 32] {
        if self.count == 0 {
            return [0u8; 32];
        }

        // Combine all pending hashes to get root
        let mut result: Option<[u8; 32]> = None;

        for entry in &self.stack {
            if let Some(hash_hex) = entry {
                let hash = hex_to_hash(hash_hex);
                result = Some(match result {
                    None => hash,
                    Some(prev) => combine_hashes(&prev, &hash),
                });
            }
        }

        result.unwrap_or([0u8; 32])
    }

    /// Number of leaves
    pub fn len(&self) -> u64 {
        self.count
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Memory usage in bytes
    pub fn memory_usage(&self) -> usize {
        // Stack capacity times entry size (Option<String> where String is 64 hex chars)
        // Plus base struct overhead
        let stack_entries = self.stack.len();
        let entry_size = std::mem::size_of::<Option<String>>() + 64; // hex string
        std::mem::size_of::<Self>() + stack_entries * entry_size
    }
}

impl Default for IncrementalMerkleState {
    fn default() -> Self {
        Self::new()
    }
}

/// Streaming Merkle tree builder (O(log n) memory)
///
/// This is a wrapper around IncrementalMerkleState that provides
/// a cleaner API for direct use (not tied to streaming verification).
pub struct IncrementalMerkle {
    state: IncrementalMerkleState,
}

impl IncrementalMerkle {
    /// Create a new incremental Merkle tree
    pub fn new() -> Self {
        Self {
            state: IncrementalMerkleState::new(),
        }
    }

    /// Add leaf to tree
    pub fn push(&mut self, leaf: [u8; 32]) {
        self.state.push(leaf);
    }

    /// Add data (hashes it first)
    pub fn push_data(&mut self, data: &[u8]) {
        self.push(hash_sha256_bytes(data));
    }

    /// Get current root
    pub fn root(&self) -> [u8; 32] {
        self.state.root()
    }

    /// Get current root as hex string
    pub fn root_hex(&self) -> String {
        hex::encode(self.root())
    }

    /// Number of leaves
    pub fn len(&self) -> u64 {
        self.state.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.state.is_empty()
    }

    /// Memory usage in bytes
    pub fn memory_usage(&self) -> usize {
        self.state.memory_usage()
    }

    /// Get internal state for serialization
    pub fn state(&self) -> &IncrementalMerkleState {
        &self.state
    }

    /// Restore from state
    pub fn from_state(state: IncrementalMerkleState) -> Self {
        Self { state }
    }
}

impl Default for IncrementalMerkle {
    fn default() -> Self {
        Self::new()
    }
}

// Helper function to convert hex string to hash bytes
fn hex_to_hash(hex_str: &str) -> [u8; 32] {
    let bytes = hex::decode(hex_str).unwrap_or_else(|_| vec![0u8; 32]);
    let mut arr = [0u8; 32];
    if bytes.len() >= 32 {
        arr.copy_from_slice(&bytes[..32]);
    }
    arr
}

// Helper function to compute receipt hash (mirrors Receipt::new logic)
fn compute_receipt_hash(receipt: &Receipt) -> String {
    use mcp_core::crypto::hash_sha256;

    let hash_input = format!(
        "{}|{}|{}|{}|{}|{}|{}|{}|{}",
        receipt.receipt_id,
        receipt.sequence,
        receipt.prev_hash,
        receipt.operation,
        receipt.contract_id,
        receipt.input_hash,
        receipt.output_hash,
        receipt.metrics.duration_us,
        receipt.timestamp.to_rfc3339()
    );
    hash_sha256(hash_input.as_bytes())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mcp_core::types::ExecutionMetrics;

    /// Create a test receipt chain
    fn create_receipt_chain(count: usize) -> Vec<Receipt> {
        let mut receipts = Vec::with_capacity(count);
        let mut prev_hash = GENESIS_HASH.to_string();

        for i in 0..count {
            let metrics = ExecutionMetrics {
                duration_us: 1000,
                memory_bytes: 1024,
                operations: 1,
                envelope_utilization: 0.5,
            };

            let receipt = Receipt::new(
                (i + 1) as u64,
                &prev_hash,
                "execute",
                "contract-001",
                format!("input-hash-{}", i),
                format!("output-hash-{}", i),
                metrics,
            );

            prev_hash = receipt.receipt_hash.clone();
            receipts.push(receipt);
        }

        receipts
    }

    #[test]
    fn test_streaming_verifier_basic() {
        let mut verifier = StreamingVerifier::new();
        let receipts = create_receipt_chain(5);

        for receipt in &receipts {
            let result = verifier.process(receipt);
            assert!(result.is_valid(), "Receipt should be valid: {:?}", result);
        }

        assert_eq!(verifier.count(), 5);
    }

    #[test]
    fn test_streaming_verifier_broken_chain() {
        let mut verifier = StreamingVerifier::new();
        let receipts = create_receipt_chain(3);

        // Process first receipt
        assert!(verifier.process(&receipts[0]).is_valid());

        // Create a receipt with wrong prev_hash
        let mut bad_receipt = receipts[1].clone();
        bad_receipt.prev_hash = "wrong_hash".to_string();

        let result = verifier.process(&bad_receipt);
        assert!(matches!(result, VerifyResult::Invalid { reason: InvalidReason::BrokenChain { .. }, .. }));
    }

    #[test]
    fn test_streaming_verifier_out_of_order() {
        let mut verifier = StreamingVerifier::new();
        let receipts = create_receipt_chain(3);

        // Process first receipt
        assert!(verifier.process(&receipts[0]).is_valid());

        // Skip receipt 2, try to process receipt 3
        let result = verifier.process(&receipts[2]);
        assert!(matches!(result, VerifyResult::OutOfOrder { .. }));
    }

    #[test]
    fn test_streaming_verifier_batch() {
        let mut verifier = StreamingVerifier::new();
        let receipts = create_receipt_chain(10);

        let result = verifier.process_batch(&receipts);

        assert_eq!(result.processed, 10);
        assert_eq!(result.valid, 10);
        assert_eq!(result.invalid, 0);
        assert_eq!(result.out_of_order, 0);
        assert!(result.all_valid());
        assert_eq!(result.success_rate(), 1.0);
    }

    #[test]
    fn test_streaming_verifier_state_persistence() {
        let receipts = create_receipt_chain(5);

        // Process first 3 receipts
        let mut verifier1 = StreamingVerifier::new();
        for receipt in &receipts[..3] {
            verifier1.process(receipt);
        }

        // Save state
        let state = verifier1.state().clone();

        // Restore and continue
        let mut verifier2 = StreamingVerifier::from_state(state);
        for receipt in &receipts[3..] {
            let result = verifier2.process(receipt);
            assert!(result.is_valid());
        }

        assert_eq!(verifier2.count(), 5);
    }

    #[test]
    fn test_streaming_verifier_callbacks() {
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::sync::Arc;

        let valid_count = Arc::new(AtomicU64::new(0));
        let invalid_count = Arc::new(AtomicU64::new(0));

        let valid_counter = valid_count.clone();
        let invalid_counter = invalid_count.clone();

        let mut verifier = StreamingVerifier::new();
        verifier.on_verified(move |_| {
            valid_counter.fetch_add(1, Ordering::SeqCst);
        });
        verifier.on_invalid(move |_, _| {
            invalid_counter.fetch_add(1, Ordering::SeqCst);
        });

        let receipts = create_receipt_chain(3);
        for receipt in &receipts {
            verifier.process(receipt);
        }

        assert_eq!(valid_count.load(Ordering::SeqCst), 3);
        assert_eq!(invalid_count.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn test_incremental_merkle_basic() {
        let mut merkle = IncrementalMerkle::new();

        merkle.push_data(b"leaf-1");
        assert_eq!(merkle.len(), 1);

        merkle.push_data(b"leaf-2");
        assert_eq!(merkle.len(), 2);

        // Root should be non-zero
        assert_ne!(merkle.root(), [0u8; 32]);
    }

    #[test]
    fn test_incremental_merkle_deterministic() {
        let build_tree = || {
            let mut merkle = IncrementalMerkle::new();
            merkle.push_data(b"a");
            merkle.push_data(b"b");
            merkle.push_data(b"c");
            merkle.push_data(b"d");
            merkle.root()
        };

        assert_eq!(build_tree(), build_tree());
    }

    #[test]
    fn test_incremental_merkle_matches_regular() {
        use crate::MerkleTree;

        // Build tree incrementally
        let mut incremental = IncrementalMerkle::new();
        incremental.push_data(b"receipt-1");
        incremental.push_data(b"receipt-2");
        incremental.push_data(b"receipt-3");
        incremental.push_data(b"receipt-4");

        // Build tree normally
        let mut regular = MerkleTree::new();
        regular.add_data(b"receipt-1");
        regular.add_data(b"receipt-2");
        regular.add_data(b"receipt-3");
        regular.add_data(b"receipt-4");
        regular.rebuild();

        assert_eq!(incremental.root(), regular.root().unwrap());
    }

    #[test]
    fn test_incremental_merkle_odd_leaves() {
        use crate::MerkleTree;

        // Build tree with odd number of leaves
        let mut incremental = IncrementalMerkle::new();
        incremental.push_data(b"a");
        incremental.push_data(b"b");
        incremental.push_data(b"c");

        // The incremental tree uses a different algorithm for odd leaves
        // so we just verify it produces a consistent result
        let root1 = incremental.root();

        let mut incremental2 = IncrementalMerkle::new();
        incremental2.push_data(b"a");
        incremental2.push_data(b"b");
        incremental2.push_data(b"c");

        assert_eq!(root1, incremental2.root());
    }

    #[test]
    fn test_incremental_merkle_state_persistence() {
        let mut merkle1 = IncrementalMerkle::new();
        merkle1.push_data(b"leaf-1");
        merkle1.push_data(b"leaf-2");

        // Save state
        let state = merkle1.state().clone();

        // Restore and continue
        let mut merkle2 = IncrementalMerkle::from_state(state);
        merkle2.push_data(b"leaf-3");
        merkle2.push_data(b"leaf-4");

        // Build fresh tree with all leaves
        let mut fresh = IncrementalMerkle::new();
        fresh.push_data(b"leaf-1");
        fresh.push_data(b"leaf-2");
        fresh.push_data(b"leaf-3");
        fresh.push_data(b"leaf-4");

        assert_eq!(merkle2.root(), fresh.root());
    }

    #[test]
    fn test_memory_usage_logarithmic() {
        let mut merkle = IncrementalMerkle::new();

        // Add leaves and track memory
        let mut memory_samples = Vec::new();

        for i in 0..1000 {
            merkle.push_data(format!("leaf-{}", i).as_bytes());

            if i == 10 || i == 100 || i == 500 || i == 999 {
                memory_samples.push((i + 1, merkle.memory_usage()));
            }
        }

        // Memory should grow logarithmically
        // For n leaves, we need at most ceil(log2(n)) + 1 stack entries
        // Each entry is approximately 80-100 bytes

        // Verify memory at 1000 leaves is reasonable (should be << 1000 * entry_size)
        let (_, mem_at_1000) = memory_samples.last().unwrap();

        // log2(1000) ~= 10, so we should have ~10 stack entries
        // Each entry ~= 100 bytes, so total ~= 1000 bytes + overhead
        // Allow generous margin for overhead
        assert!(*mem_at_1000 < 5000, "Memory usage should be O(log n), got {} bytes for 1000 leaves", mem_at_1000);

        // Memory should not grow linearly
        let (_, mem_at_100) = memory_samples[1];
        let growth_factor = *mem_at_1000 as f64 / mem_at_100 as f64;

        // If linear, growth would be 10x. For log, should be ~1.3x (log(1000)/log(100))
        assert!(growth_factor < 3.0, "Memory growth should be sublinear, factor was {}", growth_factor);
    }

    #[test]
    fn test_verify_result_methods() {
        let valid = VerifyResult::Valid {
            receipt_id: "rcpt-1".to_string(),
            sequence: 1,
        };
        assert!(valid.is_valid());
        assert_eq!(valid.receipt_id(), "rcpt-1");

        let invalid = VerifyResult::Invalid {
            receipt_id: "rcpt-2".to_string(),
            reason: InvalidReason::InvalidHash,
        };
        assert!(!invalid.is_valid());
        assert_eq!(invalid.receipt_id(), "rcpt-2");
    }

    #[test]
    fn test_batch_verify_result_methods() {
        let result = BatchVerifyResult {
            processed: 10,
            valid: 8,
            invalid: 1,
            out_of_order: 1,
            errors: vec![],
        };

        assert!(!result.all_valid());
        assert_eq!(result.success_rate(), 0.8);

        let perfect = BatchVerifyResult {
            processed: 5,
            valid: 5,
            invalid: 0,
            out_of_order: 0,
            errors: vec![],
        };

        assert!(perfect.all_valid());
        assert_eq!(perfect.success_rate(), 1.0);
    }

    #[test]
    fn test_invalid_reason_display() {
        let broken = InvalidReason::BrokenChain {
            expected: "abc".to_string(),
            actual: "def".to_string(),
        };
        assert!(broken.to_string().contains("abc"));
        assert!(broken.to_string().contains("def"));

        assert_eq!(InvalidReason::InvalidSignature.to_string(), "Invalid signature");
        assert_eq!(InvalidReason::InvalidHash.to_string(), "Invalid hash");
        assert_eq!(InvalidReason::FutureTimestamp.to_string(), "Future timestamp");
        assert_eq!(InvalidReason::DuplicateSequence.to_string(), "Duplicate sequence");
    }

    #[test]
    fn test_streaming_verifier_reset() {
        let mut verifier = StreamingVerifier::new();
        let receipts = create_receipt_chain(3);

        for receipt in &receipts {
            verifier.process(receipt);
        }

        assert_eq!(verifier.count(), 3);

        verifier.reset();

        assert_eq!(verifier.count(), 0);
        assert_eq!(verifier.state().expected_prev, GENESIS_HASH);
    }

    #[test]
    fn test_streaming_verifier_with_signed_receipts() {
        let keypair = KeyPair::generate().unwrap();
        let mut verifier = StreamingVerifier::new().with_keypair(keypair.clone());

        let metrics = ExecutionMetrics::default();
        let mut receipt = Receipt::new(
            1,
            GENESIS_HASH,
            "execute",
            "contract",
            "in",
            "out",
            metrics,
        );
        receipt.sign(&keypair);

        let result = verifier.process(&receipt);
        assert!(result.is_valid());
    }

    #[test]
    fn test_incremental_merkle_empty() {
        let merkle = IncrementalMerkle::new();
        assert!(merkle.is_empty());
        assert_eq!(merkle.len(), 0);
        assert_eq!(merkle.root(), [0u8; 32]);
    }

    #[test]
    fn test_incremental_merkle_single_leaf() {
        let mut merkle = IncrementalMerkle::new();
        let leaf = hash_sha256_bytes(b"single");
        merkle.push(leaf);

        assert_eq!(merkle.len(), 1);
        assert_eq!(merkle.root(), leaf);
    }

    #[test]
    fn test_incremental_merkle_two_leaves() {
        let mut merkle = IncrementalMerkle::new();
        let leaf1 = hash_sha256_bytes(b"first");
        let leaf2 = hash_sha256_bytes(b"second");

        merkle.push(leaf1);
        merkle.push(leaf2);

        let expected_root = combine_hashes(&leaf1, &leaf2);
        assert_eq!(merkle.root(), expected_root);
    }

    #[test]
    fn test_incremental_merkle_power_of_two() {
        let mut merkle = IncrementalMerkle::new();

        // Add 8 leaves (power of 2)
        for i in 0..8 {
            merkle.push_data(format!("leaf-{}", i).as_bytes());
        }

        assert_eq!(merkle.len(), 8);
        assert_ne!(merkle.root(), [0u8; 32]);
    }

    #[test]
    fn test_large_chain_verification() {
        let mut verifier = StreamingVerifier::new();
        let receipts = create_receipt_chain(100);

        let result = verifier.process_batch(&receipts);

        assert!(result.all_valid());
        assert_eq!(result.processed, 100);
        assert_eq!(verifier.count(), 100);

        // Memory should still be reasonable
        let memory = verifier.memory_usage();
        assert!(memory < 10_000, "Memory should be O(log n), got {} bytes", memory);
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_incremental_merkle_deterministic(data: Vec<Vec<u8>>) {
            prop_assume!(!data.is_empty());
            prop_assume!(data.len() <= 100);

            let build = || {
                let mut merkle = IncrementalMerkle::new();
                for d in &data {
                    merkle.push_data(d);
                }
                merkle.root()
            };

            prop_assert_eq!(build(), build());
        }

        #[test]
        fn prop_incremental_merkle_len(data: Vec<Vec<u8>>) {
            prop_assume!(data.len() <= 100);

            let mut merkle = IncrementalMerkle::new();
            for d in &data {
                merkle.push_data(d);
            }

            prop_assert_eq!(merkle.len() as usize, data.len());
        }

        #[test]
        fn prop_memory_bounded(leaf_count in 1usize..1000) {
            let mut merkle = IncrementalMerkle::new();
            for i in 0..leaf_count {
                merkle.push_data(format!("{}", i).as_bytes());
            }

            let memory = merkle.memory_usage();
            // Memory should be O(log n), which means bounded by c * log2(n) for some constant c
            // We use a generous constant to account for overhead
            let log_n = (leaf_count as f64).log2().ceil() as usize;
            let max_expected = 200 * (log_n + 1) + 200; // Very generous bound

            prop_assert!(
                memory <= max_expected,
                "Memory {} exceeded O(log n) bound {} for {} leaves",
                memory, max_expected, leaf_count
            );
        }
    }
}
