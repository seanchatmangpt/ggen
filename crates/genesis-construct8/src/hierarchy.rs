//! Segment/Shard/Corpus receipt hierarchy for Vision 2030 A = μ(O) construction
//! Implements three-level lawful data hierarchy with deterministic merkle proofs

use crate::models::Construct8Packet;
use blake3::Hasher;
use genesis_lockchain::merkle::MerkleTree;
use std::fmt;

/// A Segment is the lowest level of the hierarchy
/// Contains 1..N packets within a single law_ref and epoch window
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct Segment {
    pub law_ref: u64,
    pub epoch_start: u64,
    pub epoch_end: u64,
    pub packet_count: u64,
    pub triple_count: u64,
    pub merkle_root: [u8; 32],
}

/// Receipt for a Segment, deterministically produced by SegmentBuilder::finish()
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct SegmentReceipt {
    pub segment_hash: String,
    pub merkle_root: [u8; 32],
    pub packet_count: u64,
    pub triple_count: u64,
    pub law_ref: u64,
    pub epoch_start: u64,
    pub epoch_end: u64,
}

impl fmt::Display for SegmentReceipt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "SegmentReceipt {{ hash: {}, packets: {}, triples: {}, law_ref: {}, epoch_start: {}, epoch_end: {} }}",
            self.segment_hash, self.packet_count, self.triple_count, self.law_ref, self.epoch_start, self.epoch_end
        )
    }
}

/// A Shard is the middle level: aggregates multiple Segments
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct Shard {
    pub shard_index: u64,
    pub segment_count: u64,
    pub packet_count: u64,
    pub triple_count: u64,
    pub merkle_root: [u8; 32],
}

/// Receipt for a Shard, produced by ShardBuilder::finish()
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct ShardReceipt {
    pub shard_hash: String,
    pub merkle_root: [u8; 32],
    pub shard_index: u64,
    pub segment_count: u64,
    pub packet_count: u64,
    pub triple_count: u64,
}

impl fmt::Display for ShardReceipt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ShardReceipt {{ hash: {}, shard: {}, segments: {}, packets: {} }}",
            self.shard_hash, self.shard_index, self.segment_count, self.packet_count
        )
    }
}

/// Corpus is the top level: aggregates multiple Shards
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct CorpusManifest {
    pub shard_count: u64,
    pub segment_count: u64,
    pub packet_count: u64,
    pub triple_count: u64,
    pub merkle_root: [u8; 32],
    pub manifest_hash: [u8; 32],
}

/// Receipt for a Corpus, produced by CorpusBuilder::finish()
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct CorpusReceipt {
    pub corpus_hash: String,
    pub merkle_root: String,
    pub shard_count: u64,
    pub segment_count: u64,
    pub packet_count: u64,
    pub triple_count: u64,
}

impl fmt::Display for CorpusReceipt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "CorpusReceipt {{ hash: {}, shards: {}, segments: {}, packets: {} }}",
            self.corpus_hash, self.shard_count, self.segment_count, self.packet_count
        )
    }
}

/// Refusal to admit a layer due to structural or legal violation
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct HierarchyRefusal {
    pub level: &'static str,
    pub reason: String,
    pub input_digest: String,
}

impl fmt::Display for HierarchyRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "HierarchyRefusal {{ level: {}, reason: {}, digest: {} }}",
            self.level, self.reason, self.input_digest
        )
    }
}

/// Admission verdict for packet/receipt admission gates
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AdmissionVerdict {
    Admit,
    Refuse(String),
}

/// SegmentBuilder: streams packets, produces SegmentReceipt
/// Holds ≤N packet hashes in memory; streaming, not loading all into memory
#[derive(Debug)]
pub struct SegmentBuilder {
    law_ref: u64,
    epoch_start: u64,
    epoch_end: u64,
    merkle_tree: MerkleTree,
    packet_count: u64,
    triple_count: u64,
}

impl SegmentBuilder {
    pub fn new(law_ref: u64, epoch_start: u64, epoch_end: u64) -> Self {
        Self {
            law_ref,
            epoch_start,
            epoch_end,
            merkle_tree: MerkleTree::new(),
            packet_count: 0,
            triple_count: 0,
        }
    }

    /// Feed a packet into the segment builder
    /// Hash packet, add to merkle tree, track counts
    pub fn feed(&mut self, packet: &Construct8Packet) -> Result<(), String> {
        if packet.law_ref != self.law_ref {
            return Err(format!(
                "law_ref mismatch: expected {}, got {}",
                self.law_ref, packet.law_ref
            ));
        }
        if packet.epoch < self.epoch_start || packet.epoch > self.epoch_end {
            return Err(format!(
                "epoch {} out of segment range [{}, {}]",
                packet.epoch, self.epoch_start, self.epoch_end
            ));
        }

        let packet_hash = hash_construct8_packet(packet);
        self.merkle_tree.add_raw_leaf(packet_hash);
        self.packet_count += 1;
        self.triple_count += packet.len() as u64;

        Ok(())
    }

    /// Finish segment and produce receipt
    /// Computes merkle root, creates deterministic segment hash
    pub fn finish(mut self) -> SegmentReceipt {
        let merkle_root = self.merkle_tree.compute_root();

        // Segment hash is hash of (law_ref || epoch_start || epoch_end || packet_count || triple_count || merkle_root)
        let mut hasher = Hasher::new();
        hasher.update(&self.law_ref.to_le_bytes());
        hasher.update(&self.epoch_start.to_le_bytes());
        hasher.update(&self.epoch_end.to_le_bytes());
        hasher.update(&self.packet_count.to_le_bytes());
        hasher.update(&self.triple_count.to_le_bytes());
        hasher.update(&merkle_root);

        let segment_hash = hasher.finalize().to_hex().to_string();

        SegmentReceipt {
            segment_hash,
            merkle_root,
            packet_count: self.packet_count,
            triple_count: self.triple_count,
            law_ref: self.law_ref,
            epoch_start: self.epoch_start,
            epoch_end: self.epoch_end,
        }
    }
}

/// ShardBuilder: streams SegmentReceipts, produces ShardReceipt
#[derive(Debug)]
pub struct ShardBuilder {
    shard_index: u64,
    merkle_tree: MerkleTree,
    segment_count: u64,
    packet_count: u64,
    triple_count: u64,
}

impl ShardBuilder {
    pub fn new(shard_index: u64) -> Self {
        Self {
            shard_index,
            merkle_tree: MerkleTree::new(),
            segment_count: 0,
            packet_count: 0,
            triple_count: 0,
        }
    }

    /// Feed a SegmentReceipt into the shard builder
    pub fn feed(&mut self, segment: &SegmentReceipt) -> Result<(), String> {
        self.merkle_tree.add_raw_leaf(segment.merkle_root);
        self.segment_count += 1;
        self.packet_count += segment.packet_count;
        self.triple_count += segment.triple_count;
        Ok(())
    }

    /// Finish shard and produce receipt
    pub fn finish(mut self) -> ShardReceipt {
        let merkle_root = self.merkle_tree.compute_root();

        let mut hasher = Hasher::new();
        hasher.update(&self.shard_index.to_le_bytes());
        hasher.update(&self.segment_count.to_le_bytes());
        hasher.update(&self.packet_count.to_le_bytes());
        hasher.update(&self.triple_count.to_le_bytes());
        hasher.update(&merkle_root);

        let shard_hash = hasher.finalize().to_hex().to_string();

        ShardReceipt {
            shard_hash,
            merkle_root,
            shard_index: self.shard_index,
            segment_count: self.segment_count,
            packet_count: self.packet_count,
            triple_count: self.triple_count,
        }
    }
}

/// CorpusBuilder: streams ShardReceipts, produces CorpusReceipt
#[derive(Debug)]
pub struct CorpusBuilder {
    merkle_tree: MerkleTree,
    shard_count: u64,
    segment_count: u64,
    packet_count: u64,
    triple_count: u64,
}

impl CorpusBuilder {
    pub fn new() -> Self {
        Self {
            merkle_tree: MerkleTree::new(),
            shard_count: 0,
            segment_count: 0,
            packet_count: 0,
            triple_count: 0,
        }
    }

    /// Feed a ShardReceipt into the corpus builder
    pub fn feed(&mut self, shard: &ShardReceipt) -> Result<(), String> {
        self.merkle_tree.add_raw_leaf(shard.merkle_root);
        self.shard_count += 1;
        self.segment_count += shard.segment_count;
        self.packet_count += shard.packet_count;
        self.triple_count += shard.triple_count;
        Ok(())
    }

    /// Finish corpus and produce receipt
    pub fn finish(mut self) -> CorpusReceipt {
        let merkle_root = self.merkle_tree.compute_root();

        let mut hasher = Hasher::new();
        hasher.update(&self.shard_count.to_le_bytes());
        hasher.update(&self.segment_count.to_le_bytes());
        hasher.update(&self.packet_count.to_le_bytes());
        hasher.update(&self.triple_count.to_le_bytes());
        hasher.update(&merkle_root);

        let corpus_hash = hasher.finalize().to_hex().to_string();
        let merkle_root_hex = merkle_root
            .iter()
            .map(|b| format!("{:02x}", b))
            .collect::<String>();

        CorpusReceipt {
            corpus_hash,
            merkle_root: merkle_root_hex,
            shard_count: self.shard_count,
            segment_count: self.segment_count,
            packet_count: self.packet_count,
            triple_count: self.triple_count,
        }
    }
}

impl Default for CorpusBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Hash a Construct8Packet deterministically
/// Uses raw #[repr(C)] struct fields, not forge_canonical
fn hash_construct8_packet(packet: &Construct8Packet) -> [u8; 32] {
    let mut hasher = Hasher::new();

    hasher.update(&packet.epoch.to_le_bytes());
    hasher.update(&packet.law_ref.to_le_bytes());

    // Hash subject/predicate/object arrays
    for i in 0..8 {
        hasher.update(&packet.subjects[i].to_le_bytes());
        hasher.update(&packet.predicates[i].to_le_bytes());
        hasher.update(&packet.objects[i].to_le_bytes());
    }

    // Hash masks
    hasher.update(&packet.kind_mask.to_le_bytes());
    hasher.update(&packet.valid_mask.to_le_bytes());
    hasher.update(&packet.emit_mask.to_le_bytes());
    hasher.update(&packet.block_mask.to_le_bytes());

    // Hash order and seed
    hasher.update(&packet.order.to_le_bytes());
    hasher.update(&packet.receipt_seed);

    let hash = hasher.finalize();
    let mut result = [0u8; 32];
    result.copy_from_slice(hash.as_bytes());
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_segment_builder_deterministic() {
        let packet1 = Construct8Packet {
            epoch: 100,
            law_ref: 1,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        let mut builder1 = SegmentBuilder::new(1, 100, 200);
        builder1.feed(&packet1).unwrap();
        let receipt1 = builder1.finish();

        let mut builder2 = SegmentBuilder::new(1, 100, 200);
        builder2.feed(&packet1).unwrap();
        let receipt2 = builder2.finish();

        assert_eq!(
            receipt1, receipt2,
            "Same packet should produce same receipt"
        );
        assert_eq!(receipt1.segment_hash, receipt2.segment_hash);
    }

    #[test]
    fn test_shard_builder() {
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 1,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        let mut segment_builder = SegmentBuilder::new(1, 100, 200);
        segment_builder.feed(&packet).unwrap();
        let segment = segment_builder.finish();

        let mut shard_builder = ShardBuilder::new(0);
        shard_builder.feed(&segment).unwrap();
        let shard = shard_builder.finish();

        assert_eq!(shard.shard_index, 0);
        assert_eq!(shard.segment_count, 1);
        assert_eq!(shard.packet_count, 1);
    }

    #[test]
    fn test_corpus_builder() {
        let packet = Construct8Packet {
            epoch: 100,
            law_ref: 1,
            subjects: [1, 0, 0, 0, 0, 0, 0, 0],
            predicates: [2, 0, 0, 0, 0, 0, 0, 0],
            objects: [3, 0, 0, 0, 0, 0, 0, 0],
            kind_mask: 0,
            valid_mask: 1,
            emit_mask: 1,
            block_mask: 0,
            order: 0,
            receipt_seed: [0u8; 32],
        };

        let mut segment_builder = SegmentBuilder::new(1, 100, 200);
        segment_builder.feed(&packet).unwrap();
        let segment = segment_builder.finish();

        let mut shard_builder = ShardBuilder::new(0);
        shard_builder.feed(&segment).unwrap();
        let shard = shard_builder.finish();

        let mut corpus_builder = CorpusBuilder::new();
        corpus_builder.feed(&shard).unwrap();
        let corpus = corpus_builder.finish();

        assert_eq!(corpus.shard_count, 1);
        assert_eq!(corpus.segment_count, 1);
        assert_eq!(corpus.packet_count, 1);
    }
}
