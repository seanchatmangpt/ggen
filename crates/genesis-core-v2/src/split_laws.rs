//! Need9 and Need257 Split Laws for RelationPage.
//!
//! The Genesis kernel enforces strict memory boundaries via split laws:
//!
//! - **Need9 Law**: A RelationPage at capacity-9 MUST be split when the 9th pair is confirmed.
//!   This prevents fragmentation in small semantic domains (e.g., role enumerations).
//!
//! - **Need257 Law**: A RelationPage at capacity-257 MUST be split when the 257th element
//!   is about to be inserted. This prevents 8-bit index overflow in the byte-addressable model.
//!
//! Both split laws return:
//! - Two half-page `RelationPage` instances with the pairs distributed left/right.
//! - Two `Receipt` values (one per output page) proving the split was lawful.
//! - If split is illegal (page not full), a `Refusal` is returned instead.

use crate::primitives::{Construct8, Pair2, Receipt, Refusal, RefusalReason, RelationPage};

/// Output of a lawful split operation.
#[derive(Debug)]
pub struct SplitResult<const HALF: usize> {
    /// The left half of the split (first HALF pairs).
    pub left_page: RelationPage<HALF>,
    /// The right half of the split (remaining pairs).
    pub right_page: RelationPage<HALF>,
    /// BLAKE3 receipt committing the left half construction act.
    pub left_receipt: Receipt,
    /// BLAKE3 receipt committing the right half construction act (chains from left_receipt).
    pub right_receipt: Receipt,
}

/// Need9 Split: Split a fully saturated RelationPage<9> into two RelationPage<5> halves.
///
/// # Precondition
/// `page.len` must equal 9. If not, returns `Err(Refusal)`.
///
/// # Returns
/// `Ok(SplitResult<5>)` with two half-pages and their BLAKE3 receipts.
pub fn need9_split(
    page: RelationPage<9>,
    epoch: u64,
    previous_receipt: &[u8; 32],
) -> Result<SplitResult<5>, Refusal> {
    if page.len != 9 {
        return Err(Refusal::new(
            epoch,
            RefusalReason::ConstraintViolation,
            Pair2::new(page.len as u8, 9),
        ));
    }

    let predicate = page.predicate;
    let pairs = page.as_slice();

    // Build left half (first 5 pairs: indices 0..4)
    let mut left_page = RelationPage::<5>::new(predicate);
    for &pair in &pairs[..5] {
        left_page
            .insert(pair, epoch)
            .expect("Need9 left half: page capacity must fit 5 pairs");
    }

    // Build right half (last 4 pairs: indices 5..9)
    let mut right_page = RelationPage::<5>::new(predicate);
    for &pair in &pairs[5..9] {
        right_page
            .insert(pair, epoch)
            .expect("Need9 right half: page capacity must fit 4 pairs");
    }

    // Emit receipts: construct synthetic Construct8 acts representing each half-page
    let mut left_act = Construct8::new(epoch, predicate);
    for &pair in left_page.as_slice() {
        left_act.push(pair).ok(); // max 5 pairs, well within 8-lane capacity
    }
    let left_receipt = Receipt::generate(&left_act, previous_receipt);

    let mut right_act = Construct8::new(epoch, predicate);
    for &pair in right_page.as_slice() {
        right_act.push(pair).ok();
    }
    let right_receipt = Receipt::generate(&right_act, &left_receipt.signature);

    Ok(SplitResult {
        left_page,
        right_page,
        left_receipt,
        right_receipt,
    })
}

/// Need257 Split: Split a fully saturated RelationPage<257> into two RelationPage<129> halves.
///
/// # Precondition
/// `page.len` must equal 257. If not, returns `Err(Refusal)`.
///
/// # Returns
/// `Ok(SplitResult<129>)` with two half-pages and their BLAKE3 receipts.
pub fn need257_split(
    page: RelationPage<257>,
    epoch: u64,
    previous_receipt: &[u8; 32],
) -> Result<SplitResult<129>, Refusal> {
    if page.len != 257 {
        return Err(Refusal::new(
            epoch,
            RefusalReason::ConstraintViolation,
            Pair2::new((page.len >> 8) as u8, page.len as u8),
        ));
    }

    let predicate = page.predicate;
    let pairs = page.as_slice();

    // Build left half (first 129 pairs: indices 0..128)
    let mut left_page = RelationPage::<129>::new(predicate);
    for &pair in &pairs[..129] {
        left_page
            .insert(pair, epoch)
            .expect("Need257 left half: page capacity must fit 129 pairs");
    }

    // Build right half (last 128 pairs: indices 129..257)
    let mut right_page = RelationPage::<129>::new(predicate);
    for &pair in &pairs[129..257] {
        right_page
            .insert(pair, epoch)
            .expect("Need257 right half: page capacity must fit 128 pairs");
    }

    // Emit receipts using Construct8 acts (8 lanes per act; chain acts for larger pages)
    // Left page receipt: hash ALL left pairs via a keyed BLAKE3 derivation over the act chain
    let left_receipt = receipt_over_page(&left_page, epoch, previous_receipt);
    let right_receipt = receipt_over_page(&right_page, epoch, &left_receipt.signature);

    Ok(SplitResult {
        left_page,
        right_page,
        left_receipt,
        right_receipt,
    })
}

/// Compute a BLAKE3 receipt over an entire RelationPage by chaining Construct8 acts
/// (8 lanes each) across all pairs in the page.
///
/// This allows pages larger than 8 pairs to produce a single, unforgeable receipt
/// that commits to every pair in the page without truncation.
fn receipt_over_page<const CAP: usize>(
    page: &RelationPage<CAP>,
    epoch: u64,
    initial_previous: &[u8; 32],
) -> Receipt {
    let pairs = page.as_slice();
    let mut previous: [u8; 32] = *initial_previous;

    let mut chunk_start = 0;
    while chunk_start < pairs.len() {
        let chunk_end = (chunk_start + 8).min(pairs.len());
        let chunk = &pairs[chunk_start..chunk_end];

        let mut act = Construct8::new(epoch, page.predicate);
        for &pair in chunk {
            act.push(pair).ok(); // fits within 8-lane capacity
        }
        let receipt = Receipt::generate(&act, &previous);
        previous = receipt.signature;

        chunk_start = chunk_end;
    }

    Receipt { signature: previous }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::{Pair2, RefusalReason, RelationPage};

    #[test]
    fn test_need9_split_requires_full_page() {
        let page = RelationPage::<9>::new(42);
        // page.len == 0, not 9 — must refuse
        let prev = [0u8; 32];
        let err = need9_split(page, 1, &prev).unwrap_err();
        assert_eq!(err.reason, RefusalReason::ConstraintViolation);
        assert_eq!(err.epoch, 1);
    }

    #[test]
    fn test_need9_split_on_full_page() {
        let mut page = RelationPage::<9>::new(99);
        let prev = [0u8; 32];

        // Fill to exactly 9 (unique pairs to avoid DuplicateRelation)
        for i in 0u8..9 {
            page.insert(Pair2::new(i, i + 100), 1).unwrap();
        }
        assert_eq!(page.len, 9);

        let result = need9_split(page, 2, &prev).unwrap();

        // Left has 5, right has 4
        assert_eq!(result.left_page.len, 5);
        assert_eq!(result.right_page.len, 4);

        // Both pages share the same predicate
        assert_eq!(result.left_page.predicate, 99);
        assert_eq!(result.right_page.predicate, 99);

        // Receipts are real BLAKE3 — non-zero, non-equal, and input-sensitive
        assert_ne!(result.left_receipt.signature, [0u8; 32]);
        assert_ne!(result.right_receipt.signature, [0u8; 32]);
        assert_ne!(result.left_receipt.signature, result.right_receipt.signature);

        // Anti-cheating: verify receipt changes when a pair changes.
        // Build page2 with a different FIRST pair so the left-half receipt differs.
        let mut page2 = RelationPage::<9>::new(99);
        // Replace first pair with (50, 150) instead of (0, 100)
        page2.insert(Pair2::new(50, 150), 1).unwrap();
        for i in 1u8..9 {
            page2.insert(Pair2::new(i, i + 100), 1).unwrap();
        }
        let result2 = need9_split(page2, 2, &prev).unwrap();
        assert_ne!(
            result.left_receipt.signature,
            result2.left_receipt.signature,
            "Different first-half content must produce different left receipts"
        );
        // Also verify right halves differ when the right-half pair differs
        assert_ne!(
            result.right_receipt.signature,
            result2.right_receipt.signature,
            "Different right-half content must produce different right receipts (chained from different left receipt)"
        );
    }

    #[test]
    fn test_need257_split_requires_full_page() {
        let page = RelationPage::<257>::new(7);
        let prev = [0u8; 32];
        let err = need257_split(page, 5, &prev).unwrap_err();
        assert_eq!(err.reason, RefusalReason::ConstraintViolation);
    }

    #[test]
    fn test_need257_split_on_full_page() {
        let mut page = RelationPage::<257>::new(77);
        let prev = [0u8; 32];

        // Build 257 unique pairs using all 256 byte values + overflow into 16-bit space
        // We need left != right to ensure uniqueness within the byte-pair domain.
        // Strategy: left = (i / 256) as u8, right = (i % 256) as u8 but with i ≠ j pairs
        // Simpler: use pairs (i as u8, (i.wrapping_add(128)) as u8) for i in 0..257
        // but we need to avoid duplicates in a (u8, u8) space.
        // Since CAP=257 > 256 unique (0,x) pairs, we mix leading bits.
        let mut inserted = 0usize;
        'outer: for left in 0u8..=255 {
            for right in 0u8..=255 {
                if left == right {
                    continue; // skip trivial identity pairs
                }
                if inserted >= 257 {
                    break 'outer;
                }
                // Some pairs will conflict — skip them
                if page.insert(Pair2::new(left, right), 1).is_ok() {
                    inserted += 1;
                }
            }
        }
        assert_eq!(page.len, 257, "Must fill exactly 257 pairs");

        let result = need257_split(page, 3, &prev).unwrap();
        assert_eq!(result.left_page.len, 129);
        assert_eq!(result.right_page.len, 128);
        assert_eq!(result.left_page.predicate, 77);
        assert_eq!(result.right_page.predicate, 77);
        assert_ne!(result.left_receipt.signature, [0u8; 32]);
        assert_ne!(result.right_receipt.signature, [0u8; 32]);
    }
}
