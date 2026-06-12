#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! Genesis determinism verification tests.
//! Ensures Pair2 serialization, Construct8 hashing, and PageSplit are deterministic.
//! Chicago TDD: Real serialization, hash verification, idempotence checks.

use ggen_core::genesis::{
    Admission8, Construct8, Graph8, Mask8, Multiplicity, Node8, Object8, PageSplit, Pair2,
    Predicate8, Provenance8, Receipt, ReceiptHint8, RefusalCode, RelationPage, SymbolDomain,
    HASH_SIZE,
};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[test]
fn test_pair2_to_bytes_deterministic_basic() {
    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let object = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);
    let pair = Pair2::new(subject, object);

    let bytes1 = pair.to_bytes();
    let bytes2 = pair.to_bytes();
    let bytes3 = pair.to_bytes();

    assert_eq!(bytes1, bytes2, "First and second serialization must match");
    assert_eq!(bytes2, bytes3, "Second and third serialization must match");
    assert_eq!(
        bytes1.len(),
        32,
        "Serialized Pair2 must be exactly 32 bytes"
    );
}

#[test]
fn test_pair2_to_bytes_with_timestamp_deterministic() {
    let subject = Node8::from_bytes([10, 20, 30, 40, 50, 60, 70, 80]);
    let object = Node8::from_bytes([80, 70, 60, 50, 40, 30, 20, 10]);
    let timestamp = 1234567890u64;
    let pair = Pair2::with_timestamp(subject, object, timestamp);

    let bytes1 = pair.to_bytes();
    let bytes2 = pair.to_bytes();

    // Verify timestamp is encoded in bytes 16-24 as little-endian
    let ts_bytes = &bytes1[16..24];
    let ts_from_bytes = u64::from_le_bytes([
        ts_bytes[0],
        ts_bytes[1],
        ts_bytes[2],
        ts_bytes[3],
        ts_bytes[4],
        ts_bytes[5],
        ts_bytes[6],
        ts_bytes[7],
    ]);
    assert_eq!(
        ts_from_bytes, timestamp,
        "Timestamp should be encoded in little-endian at bytes 16-24"
    );

    assert_eq!(bytes1, bytes2, "Multiple serializations must be identical");
}

#[test]
fn test_pair2_to_bytes_with_event_id_deterministic() {
    let subject = Node8::from_bytes([11, 22, 33, 44, 55, 66, 77, 88]);
    let object = Node8::from_bytes([99, 88, 77, 66, 55, 44, 33, 22]);
    let event_id = [100u8, 101, 102, 103, 104, 105, 106, 107];
    let pair = Pair2::with_event_id(subject, object, event_id);

    let bytes1 = pair.to_bytes();
    let bytes2 = pair.to_bytes();

    // Verify event_id is encoded in bytes 24-32
    assert_eq!(
        &bytes1[24..32],
        &event_id,
        "Event ID should be at bytes 24-32"
    );
    assert_eq!(bytes1, bytes2, "Multiple serializations must be identical");
}

#[test]
fn test_pair2_determinism_across_1000_iterations() {
    let subject = Node8::from_bytes([42u8; 8]);
    let object = Node8::from_bytes([99u8; 8]);
    let pair = Pair2::new(subject, object);

    let first_bytes = pair.to_bytes();
    for _ in 0..1000 {
        let bytes = pair.to_bytes();
        assert_eq!(
            bytes, first_bytes,
            "Every serialization must match the first"
        );
    }
}

#[test]
fn test_pair2_hash_determinism() {
    let subject = Node8::from_bytes([5, 10, 15, 20, 25, 30, 35, 40]);
    let object = Node8::from_bytes([40, 35, 30, 25, 20, 15, 10, 5]);
    let pair = Pair2::new(subject, object);

    // Use the serialized bytes to compute a hash
    let bytes = pair.to_bytes();

    let mut hasher1 = DefaultHasher::new();
    bytes.hash(&mut hasher1);
    let hash1 = hasher1.finish();

    let mut hasher2 = DefaultHasher::new();
    bytes.hash(&mut hasher2);
    let hash2 = hasher2.finish();

    assert_eq!(hash1, hash2, "Hash of same bytes must be identical");
}

#[test]
fn test_construct8_serialization_deterministic() {
    let subject = Node8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]);
    let predicate = Predicate8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]);
    let object = Object8::from_bytes([3, 3, 3, 3, 3, 3, 3, 3]);
    let graph = Graph8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]);
    let mask = Mask8::from_bytes([255, 255, 255, 255, 255, 255, 255, 255]);
    let provenance = Provenance8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]);
    let admission = Admission8::from_bytes([6, 6, 6, 6, 6, 6, 6, 6]);
    let hint = ReceiptHint8::from_bytes([7, 7, 7, 7, 7, 7, 7, 7]);

    let construct = Construct8::new(
        subject, predicate, object, graph, mask, provenance, admission, hint,
    );

    let bytes1 = construct.to_bytes();
    let bytes2 = construct.to_bytes();
    let bytes3 = construct.to_bytes();

    assert_eq!(bytes1, bytes2, "First and second serialization must match");
    assert_eq!(bytes2, bytes3, "Second and third serialization must match");
    assert_eq!(
        bytes1.len(),
        64,
        "Construct8 serialization must be 64 bytes"
    );
}

#[test]
fn test_page_split_determinism_same_input() {
    let page1 = RelationPage::new();
    let page2 = RelationPage::new();

    let source_hash = [42u8; HASH_SIZE];
    let time = 9876543210u64;

    let split1 = PageSplit::split(&page1, source_hash, time).unwrap();
    let split2 = PageSplit::split(&page2, source_hash, time).unwrap();

    assert_eq!(
        split1.source_page_hash, split2.source_page_hash,
        "Source hashes must match"
    );
    assert_eq!(
        split1.left_page_hash, split2.left_page_hash,
        "Left page hashes must match"
    );
    assert_eq!(
        split1.right_page_hash, split2.right_page_hash,
        "Right page hashes must match"
    );
    assert_eq!(split1.timestamp, split2.timestamp, "Timestamps must match");
}

#[test]
fn test_page_split_determinism_multiple_calls() {
    let page = RelationPage::new();
    let source_hash = [123u8; HASH_SIZE];
    let time = 555u64;

    let mut previous_split = PageSplit::split(&page, source_hash, time).unwrap();

    for _ in 0..10 {
        let split = PageSplit::split(&page, source_hash, time).unwrap();
        assert_eq!(
            split.source_page_hash, previous_split.source_page_hash,
            "Source hash must remain consistent"
        );
        assert_eq!(
            split.left_page_hash, previous_split.left_page_hash,
            "Left hash must remain consistent"
        );
        assert_eq!(
            split.right_page_hash, previous_split.right_page_hash,
            "Right hash must remain consistent"
        );
        previous_split = split;
    }
}

#[test]
fn test_page_split_different_inputs_produce_different_outputs() {
    let page = RelationPage::new();

    let hash1 = [1u8; HASH_SIZE];
    let hash2 = [2u8; HASH_SIZE];
    let time = 1000u64;

    let split1 = PageSplit::split(&page, hash1, time).unwrap();
    let split2 = PageSplit::split(&page, hash2, time).unwrap();

    // Different inputs should produce different outputs
    assert_ne!(
        split1.left_page_hash, split2.left_page_hash,
        "Different source hashes should produce different left hashes"
    );
    assert_ne!(
        split1.right_page_hash, split2.right_page_hash,
        "Different source hashes should produce different right hashes"
    );
}

#[test]
fn test_pair2_different_values_produce_different_bytes() {
    let pair1 = Pair2::new(
        Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]),
        Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]),
    );

    let pair2 = Pair2::new(
        Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 9]), // Different
        Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]),
    );

    let bytes1 = pair1.to_bytes();
    let bytes2 = pair2.to_bytes();

    assert_ne!(bytes1, bytes2, "Different pairs must serialize differently");
}

#[test]
fn test_pair2_metadata_variations_deterministic() {
    let subject = Node8::from_bytes([50, 50, 50, 50, 50, 50, 50, 50]);
    let object = Node8::from_bytes([60, 60, 60, 60, 60, 60, 60, 60]);

    let pair_base = Pair2::new(subject, object);
    let pair_ts = Pair2::with_timestamp(subject, object, 1111u64);
    let pair_ev = Pair2::with_event_id(subject, object, [2u8; 8]);

    // Each variant is deterministic within itself
    let base_bytes_a = pair_base.to_bytes();
    let base_bytes_b = pair_base.to_bytes();
    assert_eq!(base_bytes_a, base_bytes_b);

    let ts_bytes_a = pair_ts.to_bytes();
    let ts_bytes_b = pair_ts.to_bytes();
    assert_eq!(ts_bytes_a, ts_bytes_b);

    let ev_bytes_a = pair_ev.to_bytes();
    let ev_bytes_b = pair_ev.to_bytes();
    assert_eq!(ev_bytes_a, ev_bytes_b);

    // But they produce different serializations
    assert_ne!(
        base_bytes_a, ts_bytes_a,
        "Timestamp variant should produce different bytes"
    );
    assert_ne!(
        base_bytes_a, ev_bytes_a,
        "Event ID variant should produce different bytes"
    );
}

#[test]
fn test_symbol_domain_iteration_deterministic() {
    let mut domain = SymbolDomain::new();

    let symbols = vec![[10u8; 8], [20u8; 8], [30u8; 8], [40u8; 8], [50u8; 8]];

    for sym in &symbols {
        let _ = domain.insert(*sym);
    }

    // Check that contains() returns consistent results across calls
    for sym in &symbols {
        assert!(domain.contains(sym), "Symbol should be found");
        assert!(domain.contains(sym), "Symbol should still be found");
    }

    let not_inserted = [99u8; 8];
    assert!(
        !domain.contains(&not_inserted),
        "Non-inserted symbol should not be found"
    );
    assert!(
        !domain.contains(&not_inserted),
        "Non-inserted symbol should still not be found"
    );
}

#[test]
fn test_pair2_equality_semantics_independent_of_creation() {
    let subj = Node8::from_bytes([77, 77, 77, 77, 77, 77, 77, 77]);
    let obj = Node8::from_bytes([88, 88, 88, 88, 88, 88, 88, 88]);

    let pair1 = Pair2::new(subj, obj);
    let pair2 = Pair2::new(subj, obj);

    // Two Pair2s created with same values should serialize identically
    assert_eq!(pair1.to_bytes(), pair2.to_bytes());
}
