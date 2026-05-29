//! E2E Receipt Chain Test — WP5
//!
//! Verifies the complete causal chain:
//!   act1 → receipt1 → act2(receipt1) → receipt2 → act3(receipt2) → receipt3
//!
//! Anti-cheating requirements:
//! 1. All receipts must be real BLAKE3 outputs (non-zero, input-sensitive)
//! 2. Tampering with any receipt in the chain must cause `ReceiptMismatch` refusal
//! 3. The `ReplayCursor` must correctly validate the full chain in order
//! 4. Out-of-order replay must emit `OutofOrderEpoch` refusal
//! 5. A forged all-zero receipt must fail verification

use genesis_core_v2::primitives::{
    Construct8, Pair2, Receipt, Refusal, RefusalReason, ReplayCursor,
};

/// Build a 3-act causal chain and return (act1, receipt1, act2, receipt2, act3, receipt3).
fn build_chain() -> (
    Construct8,
    Receipt,
    Construct8,
    Receipt,
    Construct8,
    Receipt,
) {
    let genesis_prev = [0u8; 32];

    // Act 1: epoch=1, relation=10, one pair
    let mut act1 = Construct8::new(1, 10);
    act1.push(Pair2::new(11, 22)).unwrap();
    let receipt1 = Receipt::generate(&act1, &genesis_prev);

    // Act 2: epoch=2, relation=10, two pairs — chained to receipt1
    let mut act2 = Construct8::new(2, 10);
    act2.push(Pair2::new(33, 44)).unwrap();
    act2.push(Pair2::new(55, 66)).unwrap();
    let receipt2 = Receipt::generate(&act2, &receipt1.signature);

    // Act 3: epoch=3, relation=10, three pairs — chained to receipt2
    let mut act3 = Construct8::new(3, 10);
    act3.push(Pair2::new(77, 88)).unwrap();
    act3.push(Pair2::new(99, 111)).unwrap();
    act3.push(Pair2::new(112, 113)).unwrap();
    let receipt3 = Receipt::generate(&act3, &receipt2.signature);

    (act1, receipt1, act2, receipt2, act3, receipt3)
}

#[test]
fn test_receipt_chain_all_non_zero() {
    let (_, r1, _, r2, _, r3) = build_chain();

    assert_ne!(r1.signature, [0u8; 32], "receipt1 must be non-zero BLAKE3");
    assert_ne!(r2.signature, [0u8; 32], "receipt2 must be non-zero BLAKE3");
    assert_ne!(r3.signature, [0u8; 32], "receipt3 must be non-zero BLAKE3");
}

#[test]
fn test_receipt_chain_all_distinct() {
    let (_, r1, _, r2, _, r3) = build_chain();

    assert_ne!(r1.signature, r2.signature, "receipts must be distinct");
    assert_ne!(r2.signature, r3.signature, "receipts must be distinct");
    assert_ne!(r1.signature, r3.signature, "receipts must be distinct");
}

#[test]
fn test_replay_cursor_validates_full_chain() {
    let (act1, receipt1, act2, receipt2, act3, receipt3) = build_chain();

    let mut cursor = ReplayCursor::new();

    assert!(
        cursor.advance(&act1, &receipt1).is_ok(),
        "act1 must advance cleanly"
    );
    assert_eq!(cursor.processed_count, 1);
    assert_eq!(cursor.last_receipt, receipt1.signature);

    assert!(
        cursor.advance(&act2, &receipt2).is_ok(),
        "act2 must advance cleanly"
    );
    assert_eq!(cursor.processed_count, 2);
    assert_eq!(cursor.last_receipt, receipt2.signature);

    assert!(
        cursor.advance(&act3, &receipt3).is_ok(),
        "act3 must advance cleanly"
    );
    assert_eq!(cursor.processed_count, 3);
    assert_eq!(cursor.last_receipt, receipt3.signature);
}

#[test]
fn test_tampered_receipt_causes_mismatch_refusal() {
    let (act1, receipt1, _act2, receipt2, act3, receipt3) = build_chain();

    // Advance to act2 legitimately
    let mut cursor = ReplayCursor::new();
    cursor.advance(&act1, &receipt1).unwrap();

    // Tamper: flip one bit in receipt2
    let mut tampered_receipt2 = receipt2;
    tampered_receipt2.signature[0] ^= 0x01;

    // Advancing act3 with tampered receipt2 as last known state should fail
    // First, inject the tampered receipt2 into the cursor state
    let mut cursor_tampered = ReplayCursor::new();
    cursor_tampered.advance(&act1, &receipt1).unwrap();
    // Force the cursor to believe it has receipt2 (tampered) as last receipt
    cursor_tampered.last_receipt = tampered_receipt2.signature;
    cursor_tampered.expected_epoch = 2;

    // Now act3's receipt was computed from the REAL receipt2 — it won't match tampered state
    let err: Refusal = cursor_tampered.advance(&act3, &receipt3).unwrap_err();
    assert_eq!(
        err.reason,
        RefusalReason::ReceiptMismatch,
        "Tampered chain must emit ReceiptMismatch refusal"
    );
}

#[test]
fn test_out_of_order_epoch_refusal() {
    let (act1, receipt1, act2, receipt2, act3, receipt3) = build_chain();

    let mut cursor = ReplayCursor::new();
    cursor.advance(&act1, &receipt1).unwrap();
    cursor.advance(&act2, &receipt2).unwrap();

    // Re-play act1 (epoch=1) after cursor is at epoch=2 — must fail
    let err: Refusal = cursor.advance(&act1, &receipt1).unwrap_err();
    assert_eq!(
        err.reason,
        RefusalReason::OutofOrderEpoch,
        "Replaying past epoch must emit OutofOrderEpoch refusal"
    );

    // Ensure act3 still works correctly after the failed attempt (cursor unchanged)
    assert!(
        cursor.advance(&act3, &receipt3).is_ok(),
        "Cursor must remain functional after rejected replay"
    );
    assert_eq!(cursor.processed_count, 3);
}

#[test]
fn test_forged_all_zero_receipt_fails() {
    let (act1, _receipt1, _act2, _receipt2, _act3, _receipt3) = build_chain();

    let forged_zero = Receipt {
        signature: [0u8; 32],
    };
    let mut cursor = ReplayCursor::new();

    // The real receipt1 won't match a zero signature
    let err: Refusal = cursor.advance(&act1, &forged_zero).unwrap_err();
    assert_eq!(
        err.reason,
        RefusalReason::ReceiptMismatch,
        "Zero-signature receipt must be rejected as ReceiptMismatch"
    );
}

#[test]
fn test_receipt_input_sensitivity() {
    // Prove that changing a single pair bit changes the receipt (no hash collision tolerance)
    let prev = [0u8; 32];

    let mut act_a = Construct8::new(1, 42);
    act_a.push(Pair2::new(10, 20)).unwrap();
    let receipt_a = Receipt::generate(&act_a, &prev);

    let mut act_b = Construct8::new(1, 42);
    act_b.push(Pair2::new(10, 21)).unwrap(); // right byte differs by 1
    let receipt_b = Receipt::generate(&act_b, &prev);

    assert_ne!(
        receipt_a.signature, receipt_b.signature,
        "One-bit change in pair must produce a different BLAKE3 receipt"
    );
}

#[test]
fn test_previous_receipt_sensitivity() {
    // Prove that the previous receipt is actually bound into the new receipt
    let prev_zero = [0u8; 32];
    let prev_one = [1u8; 32];

    let mut act = Construct8::new(1, 42);
    act.push(Pair2::new(10, 20)).unwrap();

    let receipt_zero = Receipt::generate(&act, &prev_zero);
    let receipt_one = Receipt::generate(&act, &prev_one);

    assert_ne!(
        receipt_zero.signature, receipt_one.signature,
        "Different previous receipts must produce different child receipts"
    );
}
