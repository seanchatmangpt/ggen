use chrono::{DateTime, Utc};
use dashmap::DashMap;
use std::sync::atomic::{AtomicU32, Ordering};
use thiserror::Error;

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum PacketError {
    #[error("Packet triple limit exceeded: holds at most 8 active triples")]
    TripleLimitExceeded,
}

#[repr(C)]
#[derive(Debug, Clone, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Construct8Packet {
    pub epoch: u64,
    pub law_ref: u64,

    pub subjects: [u32; 8],
    pub predicates: [u32; 8],
    pub objects: [u32; 8],

    pub kind_mask: u8,
    pub valid_mask: u8,
    pub emit_mask: u8,
    pub block_mask: u8,

    pub order: u32,
    pub receipt_seed: [u8; 32],
}

impl Construct8Packet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, subject: u32, predicate: u32, object: u32) -> Result<(), PacketError> {
        if self.len() >= 8 {
            return Err(PacketError::TripleLimitExceeded);
        }

        // Find the first free lane (unset bit in valid_mask)
        let mut free_lane = None;
        for i in 0..8 {
            if (self.valid_mask & (1 << i)) == 0 {
                free_lane = Some(i);
                break;
            }
        }

        if let Some(i) = free_lane {
            self.subjects[i] = subject;
            self.predicates[i] = predicate;
            self.objects[i] = object;
            self.valid_mask |= 1 << i;
            self.emit_mask |= 1 << i;
            Ok(())
        } else {
            Err(PacketError::TripleLimitExceeded)
        }
    }

    pub fn len(&self) -> usize {
        self.valid_mask.count_ones() as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn clear(&mut self) {
        self.subjects = [0; 8];
        self.predicates = [0; 8];
        self.objects = [0; 8];
        self.valid_mask = 0;
        self.emit_mask = 0;
        self.kind_mask = 0;
        self.block_mask = 0;
    }
}

/// A concurrent and thread-safe symbol table.
pub struct SymbolTable {
    str_to_id: DashMap<String, u32>,
    id_to_str: DashMap<u32, String>,
    next_id: AtomicU32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            str_to_id: DashMap::new(),
            id_to_str: DashMap::new(),
            next_id: AtomicU32::new(1),
        }
    }

    pub fn get_or_insert(&self, s: &str) -> u32 {
        if let Some(id) = self.str_to_id.get(s) {
            *id
        } else {
            use dashmap::mapref::entry::Entry;
            match self.str_to_id.entry(s.to_string()) {
                Entry::Occupied(entry) => *entry.get(),
                Entry::Vacant(entry) => {
                    let id = self.next_id.fetch_add(1, Ordering::SeqCst);
                    // CRITICAL MITIGATION: Write mapping to id_to_str FIRST before inserting the entry in str_to_id.
                    // This guarantees that any concurrent thread waiting for the str_to_id entry lock
                    // will find the reverse lookup fully populated upon release.
                    self.id_to_str.insert(id, s.to_string());
                    entry.insert(id);
                    id
                }
            }
        }
    }

    pub fn lookup(&self, id: u32) -> Option<String> {
        self.id_to_str.get(&id).map(|r| r.value().clone())
    }

    pub fn get_max_id(&self) -> u32 {
        self.next_id.load(std::sync::atomic::Ordering::SeqCst)
    }

    pub fn get_all_symbols(&self) -> std::collections::HashMap<u32, String> {
        let mut map = std::collections::HashMap::new();
        let max_id = self.next_id.load(std::sync::atomic::Ordering::SeqCst);
        for id in 1..max_id {
            if let Some(s) = self.lookup(id) {
                map.insert(id, s);
            }
        }
        map
    }

    pub fn insert_custom(&self, id: u32, s: &str) {
        self.str_to_id.insert(s.to_string(), id);
        self.id_to_str.insert(id, s.to_string());
        let mut current = self.next_id.load(std::sync::atomic::Ordering::SeqCst);
        while id >= current {
            match self.next_id.compare_exchange_weak(
                current,
                id + 1,
                std::sync::atomic::Ordering::SeqCst,
                std::sync::atomic::Ordering::SeqCst,
            ) {
                Ok(_) => break,
                Err(actual) => current = actual,
            }
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct BlockArtifact {
    pub reason: String,
    pub input_digest: String,
    pub raw_input: Vec<u8>,
    pub timestamp: DateTime<Utc>,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ReplayBundle {
    pub symbols: std::collections::HashMap<u32, String>,
    pub packets: Vec<Construct8Packet>,
}

pub fn materialize_block(reason: &str, raw_input: &[u8]) -> BlockArtifact {
    let hash = blake3::hash(raw_input);
    BlockArtifact {
        reason: reason.to_string(),
        input_digest: hash.to_hex().to_string(),
        raw_input: raw_input.to_vec(),
        timestamp: Utc::now(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn test_construct8_packet_push_limit_clear() {
        let mut packet = Construct8Packet::new();
        assert!(packet.is_empty());
        assert_eq!(packet.len(), 0);

        for i in 0..8 {
            let res = packet.push(i + 1, (i + 1) * 10, (i + 1) * 100);
            assert!(res.is_ok());
            assert_eq!(packet.len(), (i + 1) as usize);
            assert!(!packet.is_empty());
        }

        // 9th push should fail
        let res = packet.push(9, 90, 900);
        assert_eq!(res, Err(PacketError::TripleLimitExceeded));
        assert_eq!(packet.len(), 8);

        // Verify elements match
        for i in 0..8 {
            assert_eq!(packet.subjects[i], (i + 1) as u32);
            assert_eq!(packet.predicates[i], ((i + 1) * 10) as u32);
            assert_eq!(packet.objects[i], ((i + 1) * 100) as u32);
            assert_ne!(packet.valid_mask & (1 << i), 0);
            assert_ne!(packet.emit_mask & (1 << i), 0);
        }

        // Clear and verify
        packet.clear();
        assert!(packet.is_empty());
        assert_eq!(packet.len(), 0);
        assert_eq!(packet.valid_mask, 0);
        assert_eq!(packet.emit_mask, 0);
        assert_eq!(packet.subjects, [0; 8]);
        assert_eq!(packet.predicates, [0; 8]);
        assert_eq!(packet.objects, [0; 8]);
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn test_symbol_table_thread_safety() {
        let table = Arc::new(SymbolTable::new());
        let num_threads = 8;
        let inserts_per_thread = 100;
        let mut handles = vec![];

        for t in 0..num_threads {
            let table_clone = Arc::clone(&table);
            let handle = tokio::spawn(async move {
                for i in 0..inserts_per_thread {
                    let key_shared = format!("shared_key_{}", i);
                    let key_unique = format!("thread_{}_key_{}", t, i);

                    let id1 = table_clone.get_or_insert(&key_shared);
                    let id2 = table_clone.get_or_insert(&key_unique);

                    let lookup1 = table_clone.lookup(id1);
                    assert_eq!(lookup1, Some(key_shared));

                    let lookup2 = table_clone.lookup(id2);
                    assert_eq!(lookup2, Some(key_unique));
                }
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.await.expect("thread execution failed");
        }

        for i in 0..inserts_per_thread {
            let key = format!("shared_key_{}", i);
            let id = table.get_or_insert(&key);
            assert_eq!(table.lookup(id), Some(key));
        }
    }

    #[test]
    fn test_materialize_block_hashing_and_serialization() {
        let reason = "Invalid signature detected";
        let raw_input = b"malformed_packet_payload_12345";
        let artifact = materialize_block(reason, raw_input);

        assert_eq!(artifact.reason, reason);
        assert_eq!(artifact.raw_input, raw_input);

        let expected_digest = blake3::hash(raw_input).to_hex().to_string();
        assert_eq!(artifact.input_digest, expected_digest);

        let serialized = serde_json::to_string(&artifact).expect("failed serialization");
        let deserialized: BlockArtifact =
            serde_json::from_str(&serialized).expect("failed deserialization");

        assert_eq!(deserialized.reason, artifact.reason);
        assert_eq!(deserialized.input_digest, artifact.input_digest);
        assert_eq!(deserialized.raw_input, artifact.raw_input);
        assert_eq!(deserialized.timestamp, artifact.timestamp);
    }
}
