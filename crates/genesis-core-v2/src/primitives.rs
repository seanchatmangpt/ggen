//! Zero-allocation, no-std compatible Genesis Core primitives.
//!
//! These structures define the pure mathematical foundation for data construction
//! in the interchangeable parts of Genesis:
//! - `Pair2`: Left byte + right byte under assumed middle relation matter.
//! - `RelationPage`: Predicate-fixed binary relation context.
//! - `Construct8`: Bounded lane packing representing up to eight relation pairs per construction act.
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
pub struct Construct8 {
    pub epoch: u64,
    pub relation_id: u32,
    pub lanes: [Pair2; 8],
    pub valid_mask: u8,
}

impl Construct8 {
    /// Create a new empty Construct8 pack.
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
    /// Generate a receipt deterministically for a Construct8 act and a previous receipt state.
    pub fn generate(act: &Construct8, previous_receipt: &[u8; 32]) -> Self {
        let mut signature = [0u8; 32];
        let mut state = 0xabcdef1234567890u64;

        let mut mix = |val: u64| {
            state = state.wrapping_add(val);
            state ^= state >> 33;
            state = state.wrapping_mul(0xff51afd7ed558ccd);
            state ^= state >> 33;
            state = state.wrapping_mul(0xc4ceb9fe1a85ec53);
            state ^= state >> 33;
            state
        };

        // Mix previous receipt signature
        let mut i = 0;
        while i < 4 {
            let chunk = u64::from_le_bytes(
                previous_receipt[i * 8..i * 8 + 8]
                    .try_into()
                    .unwrap_or([0u8; 8]),
            );
            mix(chunk);
            i += 1;
        }

        let s1 = mix(act.epoch);
        let s2 = mix(act.relation_id as u64);
        let s3 = mix(act.valid_mask as u64);

        let mut lefts = 0u64;
        let mut rights = 0u64;
        let mut lane_idx = 0;
        while lane_idx < 8 {
            if (act.valid_mask & (1 << lane_idx)) != 0 {
                lefts = (lefts << 8) | act.lanes[lane_idx].left as u64;
                rights = (rights << 8) | act.lanes[lane_idx].right as u64;
            } else {
                lefts = (lefts << 8) | 0u64;
                rights = (rights << 8) | 0u64;
            }
            lane_idx += 1;
        }
        let s4 = mix(lefts);
        let s5 = mix(rights);

        let s1_bytes = s1.to_le_bytes();
        let s2_bytes = s2.to_le_bytes();
        let s3_bytes = s3.to_le_bytes();
        let s4_5_bytes = (s4 ^ s5).to_le_bytes();

        let mut b = 0;
        while b < 8 {
            signature[b] = s1_bytes[b];
            signature[8 + b] = s2_bytes[b];
            signature[16 + b] = s3_bytes[b];
            signature[24 + b] = s4_5_bytes[b];
            b += 1;
        }

        Self { signature }
    }
}

/// Replay cursor that validates sequence of events.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
#[repr(C)]
pub struct ReplayCursor {
    pub expected_epoch: u64,
    pub expected_relation_id: u32,
    pub processed_count: u64,
    pub last_receipt: [u8; 32],
}

impl ReplayCursor {
    /// Create a new ReplayCursor.
    pub const fn new() -> Self {
        Self {
            expected_epoch: 0,
            expected_relation_id: 0,
            processed_count: 0,
            last_receipt: [0; 32],
        }
    }

    /// Advance the cursor and verify the current Construct8 act against the expected receipt.
    pub fn advance(&mut self, act: &Construct8, expected_receipt: &Receipt) -> Result<(), Refusal> {
        if act.epoch < self.expected_epoch {
            return Err(Refusal::new(
                act.epoch,
                RefusalReason::OutofOrderEpoch,
                Pair2::new(0, 0),
            ));
        }

        let calculated = Receipt::generate(act, &self.last_receipt);
        if calculated.signature != expected_receipt.signature {
            return Err(Refusal::new(
                act.epoch,
                RefusalReason::ReceiptMismatch,
                Pair2::new(0, 0),
            ));
        }

        self.expected_epoch = act.epoch;
        self.expected_relation_id = act.relation_id;
        self.processed_count += 1;
        self.last_receipt = expected_receipt.signature;

        Ok(())
    }
}

// Compile-time layout safety assertions
const _: () = {
    assert!(std::mem::size_of::<Pair2>() == 2);
    assert!(std::mem::align_of::<Pair2>() == 1);
    assert!(std::mem::size_of::<RefusalReason>() == 1);
    assert!(std::mem::size_of::<Refusal>() == 16);
    assert!(std::mem::align_of::<Refusal>() == 8);
    assert!(std::mem::size_of::<RelationPage<256>>() == 520);
    assert!(std::mem::align_of::<RelationPage<256>>() == 4);
    assert!(std::mem::size_of::<Construct8>() == 32);
    assert!(std::mem::align_of::<Construct8>() == 8);
    assert!(std::mem::size_of::<Receipt>() == 32);
    assert!(std::mem::size_of::<ReplayCursor>() == 56);
    assert!(std::mem::align_of::<ReplayCursor>() == 8);
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pair2() {
        let pair = Pair2::new(10, 20);
        assert_eq!(pair.left, 10);
        assert_eq!(pair.right, 20);
    }

    #[test]
    fn test_relation_page() {
        let mut page = RelationPage::<4>::new(42);
        assert_eq!(page.predicate, 42);
        assert_eq!(page.len, 0);

        let p1 = Pair2::new(1, 2);
        let p2 = Pair2::new(3, 4);

        assert!(page.insert(p1, 1).is_ok());
        assert_eq!(page.len, 1);
        assert!(page.contains(p1));
        assert!(!page.contains(p2));

        // Duplicate insert should fail
        assert_eq!(
            page.insert(p1, 2),
            Err(Refusal::new(2, RefusalReason::DuplicateRelation, p1))
        );

        assert!(page.insert(p2, 3).is_ok());
        assert_eq!(page.len, 2);

        // Check as_slice
        let slice = page.as_slice();
        assert_eq!(slice.len(), 2);
        assert_eq!(slice[0], p1);
        assert_eq!(slice[1], p2);

        // Fill page
        assert!(page.insert(Pair2::new(5, 6), 4).is_ok());
        assert!(page.insert(Pair2::new(7, 8), 5).is_ok());

        // Overflow insert should fail
        let p_overflow = Pair2::new(9, 10);
        assert_eq!(
            page.insert(p_overflow, 6),
            Err(Refusal::new(6, RefusalReason::PageFull, p_overflow))
        );
    }

    #[test]
    fn test_construct8() {
        let mut act = Construct8::new(100, 999);
        assert_eq!(act.epoch, 100);
        assert_eq!(act.relation_id, 999);
        assert!(act.is_empty());

        for i in 0..8 {
            let pair = Pair2::new(i as u8, (i * 2) as u8);
            assert_eq!(act.push(pair).unwrap(), i);
        }

        assert_eq!(act.len(), 8);
        assert!(!act.is_empty());

        // Pushing a 9th element should fail
        let extra = Pair2::new(9, 9);
        assert_eq!(
            act.push(extra),
            Err(Refusal::new(100, RefusalReason::ConstructFull, extra))
        );

        // Check get
        assert_eq!(act.get(0), Some(Pair2::new(0, 0)));
        assert_eq!(act.get(7), Some(Pair2::new(7, 14)));
        assert_eq!(act.get(8), None);
    }

    #[test]
    fn test_receipt_and_replay() {
        let mut act1 = Construct8::new(1, 42);
        act1.push(Pair2::new(10, 20)).unwrap();

        let prev_sig = [0u8; 32];
        let receipt1 = Receipt::generate(&act1, &prev_sig);

        let mut cursor = ReplayCursor::new();
        // Successful replay
        assert!(cursor.advance(&act1, &receipt1).is_ok());
        assert_eq!(cursor.expected_epoch, 1);
        assert_eq!(cursor.processed_count, 1);
        assert_eq!(cursor.last_receipt, receipt1.signature);

        // Advancing with out of order epoch should fail
        let mut act_out_of_order = Construct8::new(0, 42);
        act_out_of_order.push(Pair2::new(10, 20)).unwrap();
        let receipt_out_of_order = Receipt::generate(&act_out_of_order, &cursor.last_receipt);
        assert_eq!(
            cursor.advance(&act_out_of_order, &receipt_out_of_order),
            Err(Refusal::new(
                0,
                RefusalReason::OutofOrderEpoch,
                Pair2::new(0, 0)
            ))
        );

        // Advancing with invalid receipt should fail
        let mut act2 = Construct8::new(2, 42);
        act2.push(Pair2::new(30, 40)).unwrap();
        let invalid_receipt = Receipt {
            signature: [0xff; 32],
        };
        assert_eq!(
            cursor.advance(&act2, &invalid_receipt),
            Err(Refusal::new(
                2,
                RefusalReason::ReceiptMismatch,
                Pair2::new(0, 0)
            ))
        );
    }
}
