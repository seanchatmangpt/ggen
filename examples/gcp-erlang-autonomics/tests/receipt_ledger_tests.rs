//! Receipt ledger and hash chain verification tests
//!
//! Chicago TDD: Tests verify hash chain integrity, tamper detection, and audit trail.
//! Uses real hash computation and cryptographic verification.

use std::collections::HashMap;

/// A receipt entry in the immutable ledger
#[derive(Debug, Clone)]
pub struct LedgerEntry {
    /// Entry ID (unique, monotonic)
    pub id: u64,
    /// Receipt ID
    pub receipt_id: String,
    /// Tenant ID for isolation
    pub tenant_id: String,
    /// Action executed
    pub action: String,
    /// Previous hash in chain
    pub previous_hash: String,
    /// This entry's hash (proving integrity)
    pub entry_hash: String,
    /// Timestamp in ISO 8601
    pub timestamp: String,
    /// Full payload for audit
    pub payload: serde_json::Value,
}

impl LedgerEntry {
    pub fn new(
        id: u64,
        receipt_id: impl Into<String>,
        tenant_id: impl Into<String>,
        action: impl Into<String>,
        previous_hash: impl Into<String>,
        payload: serde_json::Value,
    ) -> Self {
        let entry = Self {
            id,
            receipt_id: receipt_id.into(),
            tenant_id: tenant_id.into(),
            action: action.into(),
            previous_hash: previous_hash.into(),
            entry_hash: String::new(),
            timestamp: chrono::Utc::now().to_rfc3339(),
            payload,
        };

        // Compute hash based on content
        let mut entry_mut = entry.clone();
        entry_mut.compute_hash();
        entry_mut
    }

    /// Compute SHA-256 hash of entry
    fn compute_hash(&mut self) {
        use sha2::Digest;
        let mut hasher = sha2::Sha256::new();

        // Hash components in order
        hasher.update(self.id.to_string());
        hasher.update(&self.receipt_id);
        hasher.update(&self.tenant_id);
        hasher.update(&self.action);
        hasher.update(&self.previous_hash);
        hasher.update(&self.timestamp);
        hasher.update(self.payload.to_string());

        self.entry_hash = hex::encode(hasher.finalize());
    }

    /// Verify entry hash (detects tampering)
    pub fn verify_hash(&self) -> bool {
        let mut copy = self.clone();
        copy.compute_hash();
        self.entry_hash == copy.entry_hash
    }

    /// Verify this entry's previous hash matches parent
    pub fn verify_chain(&self, parent: &LedgerEntry) -> bool {
        self.previous_hash == parent.entry_hash
    }
}

/// Immutable ledger storing all receipts (audit trail)
pub struct ReceiptLedger {
    /// All entries, indexed by ID (immutable, append-only)
    entries: Vec<LedgerEntry>,
    /// Index by receipt ID for fast lookup
    by_receipt_id: HashMap<String, u64>,
    /// Index by tenant for isolation
    by_tenant: HashMap<String, Vec<u64>>,
    /// Index by action type
    by_action: HashMap<String, Vec<u64>>,
}

impl ReceiptLedger {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            by_receipt_id: HashMap::new(),
            by_tenant: HashMap::new(),
            by_action: HashMap::new(),
        }
    }

    /// Append an entry to ledger (immutable once added)
    pub fn append(
        &mut self,
        receipt_id: impl Into<String>,
        tenant_id: impl Into<String>,
        action: impl Into<String>,
        payload: serde_json::Value,
    ) -> Result<u64, String> {
        let next_id = (self.entries.len() as u64) + 1;

        // Get previous hash (last entry's hash, or "0" for genesis)
        let previous_hash = self
            .entries
            .last()
            .map(|e| e.entry_hash.clone())
            .unwrap_or_else(|| "0".to_string());

        let receipt_id_str = receipt_id.into();
        let tenant_id_str = tenant_id.into();
        let action_str = action.into();

        let mut entry = LedgerEntry::new(
            next_id,
            &receipt_id_str,
            &tenant_id_str,
            &action_str,
            previous_hash,
            payload,
        );

        // Verify entry hash is valid
        if !entry.verify_hash() {
            return Err("Entry hash verification failed".to_string());
        }

        // Index by various keys
        self.by_receipt_id.insert(receipt_id_str, next_id);
        self.by_tenant
            .entry(tenant_id_str)
            .or_insert_with(Vec::new)
            .push(next_id);
        self.by_action.entry(action_str).or_insert_with(Vec::new).push(next_id);

        self.entries.push(entry);
        Ok(next_id)
    }

    /// Get entry by ID
    pub fn get(&self, id: u64) -> Option<&LedgerEntry> {
        self.entries.get((id - 1) as usize)
    }

    /// Get entry by receipt ID
    pub fn get_by_receipt(&self, receipt_id: &str) -> Option<&LedgerEntry> {
        self.by_receipt_id
            .get(receipt_id)
            .and_then(|id| self.get(*id))
    }

    /// Get entries for tenant
    pub fn get_by_tenant(&self, tenant_id: &str) -> Vec<&LedgerEntry> {
        self.by_tenant
            .get(tenant_id)
            .map(|ids| ids.iter().filter_map(|id| self.get(*id)).collect())
            .unwrap_or_default()
    }

    /// Get entries by action type
    pub fn get_by_action(&self, action: &str) -> Vec<&LedgerEntry> {
        self.by_action
            .get(action)
            .map(|ids| ids.iter().filter_map(|id| self.get(*id)).collect())
            .unwrap_or_default()
    }

    /// Get all entries
    pub fn all(&self) -> &[LedgerEntry] {
        &self.entries
    }

    /// Get entry count
    pub fn count(&self) -> usize {
        self.entries.len()
    }

    /// Get count for tenant
    pub fn count_for_tenant(&self, tenant_id: &str) -> usize {
        self.by_tenant.get(tenant_id).map(|ids| ids.len()).unwrap_or(0)
    }

    /// Verify entire chain integrity
    pub fn verify_chain_integrity(&self) -> bool {
        for i in 1..self.entries.len() {
            let current = &self.entries[i];
            let parent = &self.entries[i - 1];

            // Verify current entry's hash
            if !current.verify_hash() {
                return false;
            }

            // Verify chain link
            if current.previous_hash != parent.entry_hash {
                return false;
            }
        }

        // Verify first entry
        if !self.entries.is_empty() && !self.entries[0].verify_hash() {
            return false;
        }

        true
    }

    /// Verify all entries are tamper-free
    pub fn verify_all_entries(&self) -> bool {
        self.entries.iter().all(|e| e.verify_hash())
    }

    /// Get entries for date range (timestamp filtering)
    pub fn get_in_range(
        &self,
        start: chrono::DateTime<chrono::Utc>,
        end: chrono::DateTime<chrono::Utc>,
    ) -> Vec<&LedgerEntry> {
        self.entries
            .iter()
            .filter(|e| {
                if let Ok(ts) = chrono::DateTime::parse_from_rfc3339(&e.timestamp) {
                    let ts = ts.with_timezone(&chrono::Utc);
                    ts >= start && ts <= end
                } else {
                    false
                }
            })
            .collect()
    }
}

impl Default for ReceiptLedger {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    // Test 1: Append entry to ledger
    #[test]
    fn test_append_entry() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        let payload = json!({"cost": 100.0, "forecast": 120.0});

        // Act
        let id = ledger
            .append("receipt-1", "tenant-1", "throttle", payload)
            .unwrap();

        // Assert
        assert_eq!(id, 1);
        assert_eq!(ledger.count(), 1);

        let entry = ledger.get(id).unwrap();
        assert_eq!(entry.receipt_id, "receipt-1");
        assert_eq!(entry.tenant_id, "tenant-1");
        assert_eq!(entry.action, "throttle");
    }

    // Test 2: Hash is computed
    #[test]
    fn test_entry_hash_computed() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        let payload = json!({"value": 42});

        // Act
        let id = ledger.append("receipt-1", "tenant-1", "action", payload).unwrap();

        // Assert
        let entry = ledger.get(id).unwrap();
        assert!(!entry.entry_hash.is_empty());
        assert_eq!(entry.entry_hash.len(), 64); // SHA-256 hex length
    }

    // Test 3: Hash verification passes for valid entry
    #[test]
    fn test_hash_verification_valid() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        let payload = json!({"data": "test"});

        // Act
        let id = ledger.append("receipt-1", "tenant-1", "action", payload).unwrap();

        // Assert
        let entry = ledger.get(id).unwrap();
        assert!(entry.verify_hash());
    }

    // Test 4: Hash chain verification
    #[test]
    fn test_hash_chain_verification() {
        // Arrange
        let mut ledger = ReceiptLedger::new();

        // Act: Append multiple entries
        ledger
            .append("receipt-1", "tenant-1", "action-1", json!({"n": 1}))
            .unwrap();
        ledger
            .append("receipt-2", "tenant-1", "action-2", json!({"n": 2}))
            .unwrap();
        ledger
            .append("receipt-3", "tenant-1", "action-3", json!({"n": 3}))
            .unwrap();

        // Assert: Chain is valid
        assert!(ledger.verify_chain_integrity());
    }

    // Test 5: Tamper detection - modified payload
    #[test]
    fn test_tamper_detection_payload_modification() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        let id = ledger
            .append("receipt-1", "tenant-1", "action", json!({"value": 100}))
            .unwrap();

        // Act: Tamper with entry (simulated)
        let entry = &mut ledger.entries[0];
        entry.payload = json!({"value": 999}); // Tamper!

        // Assert: Hash verification fails
        assert!(!entry.verify_hash());
    }

    // Test 6: Tamper detection - modified previous hash
    #[test]
    fn test_tamper_detection_chain_break() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger
            .append("receipt-1", "tenant-1", "action-1", json!({"n": 1}))
            .unwrap();
        ledger
            .append("receipt-2", "tenant-1", "action-2", json!({"n": 2}))
            .unwrap();

        // Act: Tamper with chain
        ledger.entries[1].previous_hash = "deadbeef".to_string();

        // Assert: Chain integrity fails
        assert!(!ledger.verify_chain_integrity());
    }

    // Test 7: Retrieve by receipt ID
    #[test]
    fn test_get_by_receipt_id() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger
            .append("receipt-abc", "tenant-1", "action", json!({}))
            .unwrap();

        // Act
        let entry = ledger.get_by_receipt("receipt-abc");

        // Assert
        assert!(entry.is_some());
        assert_eq!(entry.unwrap().receipt_id, "receipt-abc");
    }

    // Test 8: Retrieve by tenant ID
    #[test]
    fn test_get_by_tenant() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger.append("r1", "tenant-1", "action-1", json!({})).unwrap();
        ledger.append("r2", "tenant-1", "action-2", json!({})).unwrap();
        ledger.append("r3", "tenant-2", "action-3", json!({})).unwrap();

        // Act
        let entries = ledger.get_by_tenant("tenant-1");

        // Assert
        assert_eq!(entries.len(), 2);
        assert!(entries.iter().all(|e| e.tenant_id == "tenant-1"));
    }

    // Test 9: Retrieve by action type
    #[test]
    fn test_get_by_action() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger.append("r1", "tenant-1", "throttle", json!({})).unwrap();
        ledger.append("r2", "tenant-1", "degrade", json!({})).unwrap();
        ledger.append("r3", "tenant-1", "throttle", json!({})).unwrap();

        // Act
        let entries = ledger.get_by_action("throttle");

        // Assert
        assert_eq!(entries.len(), 2);
        assert!(entries.iter().all(|e| e.action == "throttle"));
    }

    // Test 10: Multi-tenant isolation
    #[test]
    fn test_multi_tenant_isolation() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger.append("r1", "tenant-1", "action", json!({})).unwrap();
        ledger.append("r2", "tenant-2", "action", json!({})).unwrap();
        ledger.append("r3", "tenant-3", "action", json!({})).unwrap();

        // Act
        let t1 = ledger.get_by_tenant("tenant-1");
        let t2 = ledger.get_by_tenant("tenant-2");
        let t3 = ledger.get_by_tenant("tenant-3");

        // Assert: Each tenant sees only their own
        assert_eq!(t1.len(), 1);
        assert_eq!(t2.len(), 1);
        assert_eq!(t3.len(), 1);
        assert_eq!(t1[0].tenant_id, "tenant-1");
        assert_eq!(t2[0].tenant_id, "tenant-2");
        assert_eq!(t3[0].tenant_id, "tenant-3");
    }

    // Test 11: Immutability (append-only)
    #[test]
    fn test_immutability_append_only() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        let id1 = ledger.append("r1", "tenant-1", "action-1", json!({})).unwrap();

        // Act
        let id2 = ledger.append("r2", "tenant-1", "action-2", json!({})).unwrap();

        // Assert: IDs are monotonically increasing
        assert_eq!(id1, 1);
        assert_eq!(id2, 2);
        assert_eq!(ledger.count(), 2);

        // First entry is unchanged
        assert_eq!(ledger.get(id1).unwrap().receipt_id, "r1");
    }

    // Test 12: Ledger is empty initially
    #[test]
    fn test_initial_empty() {
        // Arrange & Act
        let ledger = ReceiptLedger::new();

        // Assert
        assert_eq!(ledger.count(), 0);
        assert_eq!(ledger.all().len(), 0);
    }

    // Test 13: Count by tenant
    #[test]
    fn test_count_by_tenant() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger.append("r1", "tenant-1", "action", json!({})).unwrap();
        ledger.append("r2", "tenant-1", "action", json!({})).unwrap();
        ledger.append("r3", "tenant-1", "action", json!({})).unwrap();
        ledger.append("r4", "tenant-2", "action", json!({})).unwrap();

        // Act & Assert
        assert_eq!(ledger.count_for_tenant("tenant-1"), 3);
        assert_eq!(ledger.count_for_tenant("tenant-2"), 1);
        assert_eq!(ledger.count_for_tenant("unknown"), 0);
    }

    // Test 14: Verify all entries
    #[test]
    fn test_verify_all_entries() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger.append("r1", "tenant-1", "action", json!({})).unwrap();
        ledger.append("r2", "tenant-1", "action", json!({})).unwrap();
        ledger.append("r3", "tenant-1", "action", json!({})).unwrap();

        // Act & Assert
        assert!(ledger.verify_all_entries());
    }

    // Test 15: Previous hash links entries
    #[test]
    fn test_previous_hash_links() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        let id1 = ledger.append("r1", "tenant-1", "action", json!({})).unwrap();
        let id2 = ledger.append("r2", "tenant-1", "action", json!({})).unwrap();
        let id3 = ledger.append("r3", "tenant-1", "action", json!({})).unwrap();

        // Act
        let entry1 = ledger.get(id1).unwrap();
        let entry2 = ledger.get(id2).unwrap();
        let entry3 = ledger.get(id3).unwrap();

        // Assert: Chain links
        assert_eq!(entry1.previous_hash, "0"); // Genesis
        assert_eq!(entry2.previous_hash, entry1.entry_hash);
        assert_eq!(entry3.previous_hash, entry2.entry_hash);
    }

    // Test 16: Large audit trail
    #[test]
    fn test_large_audit_trail() {
        // Arrange
        let mut ledger = ReceiptLedger::new();

        // Act: Append many entries
        for i in 0..100 {
            ledger
                .append(
                    format!("receipt-{}", i),
                    "tenant-1",
                    "action",
                    json!({"index": i}),
                )
                .unwrap();
        }

        // Assert
        assert_eq!(ledger.count(), 100);
        assert!(ledger.verify_chain_integrity());

        let entries = ledger.get_by_tenant("tenant-1");
        assert_eq!(entries.len(), 100);
    }

    // Test 17: Timestamp ordering
    #[test]
    fn test_timestamp_ordering() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        ledger.append("r1", "tenant-1", "action", json!({})).unwrap();

        // Small delay
        std::thread::sleep(std::time::Duration::from_millis(10));

        ledger.append("r2", "tenant-1", "action", json!({})).unwrap();

        // Act
        let entries = ledger.all();

        // Assert: Timestamps are ordered
        let ts1 = chrono::DateTime::parse_from_rfc3339(&entries[0].timestamp).unwrap();
        let ts2 = chrono::DateTime::parse_from_rfc3339(&entries[1].timestamp).unwrap();
        assert!(ts1 <= ts2);
    }

    // Test 18: Payload flexibility
    #[test]
    fn test_flexible_payload() {
        // Arrange
        let mut ledger = ReceiptLedger::new();

        // Act: Different payload structures
        ledger
            .append("r1", "tenant-1", "action", json!({"simple": "value"}))
            .unwrap();
        ledger
            .append(
                "r2",
                "tenant-1",
                "action",
                json!({"complex": {"nested": {"structure": [1, 2, 3]}}})
            )
            .unwrap();
        ledger
            .append("r3", "tenant-1", "action", json!({"array": [1, 2, 3, 4, 5]}))
            .unwrap();

        // Assert: All stored correctly
        assert_eq!(ledger.count(), 3);
        assert!(ledger.verify_chain_integrity());
    }

    // Test 19: Query by date range
    #[test]
    fn test_get_in_range() {
        // Arrange
        let mut ledger = ReceiptLedger::new();
        let now = chrono::Utc::now();

        ledger.append("r1", "tenant-1", "action", json!({})).unwrap();

        // Act
        let entries = ledger.get_in_range(now - chrono::Duration::seconds(5), now + chrono::Duration::seconds(5));

        // Assert
        assert_eq!(entries.len(), 1);
    }

    // Test 20: Duplicate receipt ID handling
    #[test]
    fn test_duplicate_receipt_id_in_different_tenants() {
        // Arrange
        let mut ledger = ReceiptLedger::new();

        // Act: Same receipt ID for different tenants (allowed)
        ledger.append("receipt-abc", "tenant-1", "action", json!({})).unwrap();
        ledger.append("receipt-abc", "tenant-2", "action", json!({})).unwrap();

        // Assert: Both stored, indexed separately per receipt
        let entries = ledger.all();
        assert_eq!(entries.len(), 2);

        // But get_by_receipt returns only one (indexed as unique)
        let entry = ledger.get_by_receipt("receipt-abc");
        assert!(entry.is_some());
    }
}
