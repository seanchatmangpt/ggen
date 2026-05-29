#![no_std]

//! Zero-allocation, no-std compatible Genesis Core primitives.
//!
//! These structures define the pure mathematical foundation for data construction
//! in the interchangeable parts of Genesis:
//! - `Pair2`: Left byte + right byte under assumed middle relation matter.
//! - `RelationPage`: Predicate-fixed binary relation context.
//! - `Construct8Packet`: Bounded lane packing representing up to eight relation pairs per construction act.
//! - Receipts, replay cursors, and refusal structures.

/// Bounded left/right byte pair under assumed middle relation matter.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Pair2 {
    pub left: u8,
    pub right: u8,
}

impl Pair2 {
    /// Create a new pair of left and right bytes.
    pub const fn new(left: u8, right: u8) -> Self {
        Self { left, right }
    }
}

/// Reasons why a construction or validation act was refused.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum RefusalReason {
    /// The destination RelationPage is full.
    PageFull = 1,
    /// Bounded packing lane capacity exceeded.
    ConstructFull = 2,
    /// Out of sequence epoch detected during replay.
    OutofOrderEpoch = 3,
    /// Receipt cryptographic validation failed.
    ReceiptMismatch = 4,
    /// Assumed relations violated constraints.
    ConstraintViolation = 5,
    /// Duplicate relation forbidden.
    DuplicateRelation = 6,
}

/// Refusal structure representing a rejected construction act or constraint block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Refusal {
    pub epoch: u64,
    pub reason: RefusalReason,
    pub failed_pair: Pair2,
}

impl Refusal {
    /// Create a new Refusal record.
    pub const fn new(epoch: u64, reason: RefusalReason, failed_pair: Pair2) -> Self {
        Self {
            epoch,
            reason,
            failed_pair,
        }
    }
}

/// Predicate-fixed binary relation context.
/// Zero-allocation, no-std compatible using const generics.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct RelationPage<const CAP: usize = 256> {
    pub predicate: u32,
    pub len: u32,
    pub pairs: [Pair2; CAP],
}

impl<const CAP: usize> RelationPage<CAP> {
    /// Create a new RelationPage for a given predicate.
    pub const fn new(predicate: u32) -> Self {
        Self {
            predicate,
            pairs: [Pair2::new(0, 0); CAP],
            len: 0,
        }
    }

    /// Check if a pair exists in the relation page.
    pub fn contains(&self, pair: Pair2) -> bool {
        let mut found = false;
        let mut i = 0;
        let limit = if (self.len as usize) > CAP {
            CAP
        } else {
            self.len as usize
        };
        while i < limit {
            if self.pairs[i].left == pair.left && self.pairs[i].right == pair.right {
                found = true;
                break;
            }
            i += 1;
        }
        found
    }

    /// Insert a pair into the relation page. Returns a `Refusal` if full.
    pub fn insert(&mut self, pair: Pair2, epoch: u64) -> Result<(), Refusal> {
        let current_len = self.len as usize;
        if current_len >= CAP {
            return Err(Refusal::new(epoch, RefusalReason::PageFull, pair));
        }
        if self.contains(pair) {
            return Err(Refusal::new(epoch, RefusalReason::DuplicateRelation, pair));
        }
        self.pairs[current_len] = pair;
        self.len += 1;
        Ok(())
    }

    /// Get the valid/inserted pairs as a slice.
    pub fn as_slice(&self) -> &[Pair2] {
        let limit = if (self.len as usize) > CAP {
            CAP
        } else {
            self.len as usize
        };
        &self.pairs[..limit]
    }
}

/// Bounded lane packing representing up to eight relation pairs per construction act.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct Construct8Packet {
    pub epoch: u64,
    pub relation_id: u32,
    pub lanes: [Pair2; 8],
    pub valid_mask: u8,
}

impl Construct8Packet {
    /// Create a new empty Construct8Packet.
    pub const fn new(epoch: u64, relation_id: u32) -> Self {
        Self {
            epoch,
            relation_id,
            lanes: [Pair2::new(0, 0); 8],
            valid_mask: 0,
        }
    }

    /// Push a pair into the next available lane.
    pub fn push(&mut self, pair: Pair2) -> Result<usize, Refusal> {
        let count = self.len();
        if count >= 8 {
            return Err(Refusal::new(self.epoch, RefusalReason::ConstructFull, pair));
        }

        let mut i = 0;
        while i < 8 {
            if (self.valid_mask & (1 << i)) == 0 {
                self.lanes[i] = pair;
                self.valid_mask |= 1 << i;
                return Ok(i);
            }
            i += 1;
        }

        Err(Refusal::new(self.epoch, RefusalReason::ConstructFull, pair))
    }

    /// Get the number of valid pairs packed.
    pub const fn len(&self) -> usize {
        self.valid_mask.count_ones() as usize
    }

    /// Check if the pack is empty.
    pub const fn is_empty(&self) -> bool {
        self.valid_mask == 0
    }

    /// Get the pair at a specific lane if valid.
    pub fn get(&self, lane: usize) -> Option<Pair2> {
        if lane < 8 && (self.valid_mask & (1 << lane)) != 0 {
            Some(self.lanes[lane])
        } else {
            None
        }
    }
}

/// Cryptographic proof of a Construct8 act.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Receipt {
    pub signature: [u8; 32],
}

impl Receipt {
    /// Generate a BLAKE3 receipt deterministically for a Construct8 act and a previous receipt state.
    pub fn new(previous_receipt: &[u8; 32], act: &Construct8Packet) -> Self {
        let mut hasher = blake3::Hasher::new();
        hasher.update(previous_receipt);
        hasher.update(&act.epoch.to_le_bytes());
        hasher.update(&act.relation_id.to_le_bytes());
        for i in 0..8 {
            if (act.valid_mask & (1 << i)) != 0 {
                hasher.update(&[act.lanes[i].left, act.lanes[i].right]);
            }
        }
        Self {
            signature: *hasher.finalize().as_bytes(),
        }
    }
}

/// A cursor allowing deterministic replay of a construction act.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct ReplayCursor {
    pub seed: u64,
    pub offset: u32,
    pub expected_receipt: [u8; 32],
}

impl ReplayCursor {
    pub const fn new(seed: u64, offset: u32, expected_receipt: [u8; 32]) -> Self {
        Self {
            seed,
            offset,
            expected_receipt,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_core_primitives_compile_and_instantiate() {
        let pair = Pair2::new(1, 2);
        assert_eq!(pair.left, 1);
        assert_eq!(pair.right, 2);

        let mut page: RelationPage<256> = RelationPage::new(42);
        assert!(page.insert(pair, 100).is_ok());
        assert_eq!(page.len, 1);

        let mut packet = Construct8Packet::new(100, 42);
        assert!(packet.push(pair).is_ok());
        assert_eq!(packet.len(), 1);

        let prev = [0u8; 32];
        let receipt = Receipt::new(&prev, &packet);
        assert_ne!(receipt.signature, [0u8; 32]);
    }
}
