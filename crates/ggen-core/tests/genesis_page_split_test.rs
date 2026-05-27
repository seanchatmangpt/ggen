//! Page split law verification tests for Genesis core primitives.
//! Testing domain saturation, multiplicity semantics, and split mechanics.
//! Chicago TDD: Real state changes, no mocks, deterministic replay.

use ggen_core::genesis::{
    Admission8, Construct8, Graph8, Mask8, Multiplicity, Node8, Object8, Pair2, PageSplit,
    Predicate8, Receipt, ReceiptHint8, RefusalCode, RelationPage, SymbolDomain, Provenance8,
    HASH_SIZE,
};

#[test]
fn test_symbol_domain_255_insert_succeeds() {
    let mut domain = SymbolDomain::new();

    // Insert 255 distinct symbols
    for i in 0..255 {
        let symbol = [(i & 0xFF) as u8, 0, 0, 0, 0, 0, 0, 0];
        let result = domain.insert(symbol);
        assert!(result.is_ok(), "Insert at {} failed", i);
        assert!(result.unwrap(), "Should be new insertion at {}", i);
    }

    assert_eq!(domain.count(), 255, "Domain should contain 255 symbols");
}

#[test]
fn test_symbol_domain_256_insert_fails() {
    let mut domain = SymbolDomain::new();

    // Insert 256 symbols
    for i in 0..256 {
        let symbol = [(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0];
        let _ = domain.insert(symbol);
    }

    // 257th insert should fail with PageSplitRequired
    let symbol_257 = [255u8, 255u8, 0, 0, 0, 0, 0, 0];
    let result = domain.insert(symbol_257);
    assert_eq!(result, Err(RefusalCode::PageSplitRequired));
    assert_eq!(domain.count(), 256);
}

#[test]
fn test_symbol_domain_contains_check() {
    let mut domain = SymbolDomain::new();
    let symbol = [42u8, 99, 88, 77, 66, 55, 44, 33];

    assert!(!domain.contains(&symbol));

    let _ = domain.insert(symbol);
    assert!(domain.contains(&symbol));
}

#[test]
fn test_relation_page_left_domain_saturation() {
    let mut page = RelationPage::new();

    // Insert pairs with 256 distinct subjects
    for i in 0..256 {
        let subject = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let pair = Pair2::new(subject, object);

        let inserted = page.insert(pair);
        assert!(inserted, "Insert {} into left domain should succeed", i);
        assert!(page.remove(&pair));
    }

    assert_eq!(page.left_domain.count(), 256);

    // 257th insert with new subject should fail
    let subject_257 = Node8::from_bytes([255u8, 255u8, 0, 0, 0, 0, 0, 0]);
    let object_257 = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 255]);
    let pair_257 = Pair2::new(subject_257, object_257);

    // This will fail because left domain is saturated
    let result = page.insert(pair_257);
    assert!(!result, "Insert should fail at left domain saturation");
}

#[test]
fn test_relation_page_right_domain_saturation() {
    let mut page = RelationPage::new();

    // Insert pairs with 256 distinct objects
    for i in 0..256 {
        let subject = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let object = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let pair = Pair2::new(subject, object);

        let inserted = page.insert(pair);
        assert!(inserted, "Insert {} into right domain should succeed", i);
        assert!(page.remove(&pair));
    }

    assert_eq!(page.right_domain.count(), 256);

    // 257th insert with new object should fail
    let subject_257 = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 255]);
    let object_257 = Node8::from_bytes([255u8, 255u8, 0, 0, 0, 0, 0, 0]);
    let pair_257 = Pair2::new(subject_257, object_257);

    let result = page.insert(pair_257);
    assert!(!result, "Insert should fail at right domain saturation");
}

#[test]
fn test_page_split_determinism() {
    let page = RelationPage::new();
    let hash = [1u8; HASH_SIZE];
    let time = 1234567890u64;

    let split1 = PageSplit::split(&page, hash, time).unwrap();
    let split2 = PageSplit::split(&page, hash, time).unwrap();

    assert_eq!(split1.source_page_hash, split2.source_page_hash);
    assert_eq!(split1.left_page_hash, split2.left_page_hash);
    assert_eq!(split1.right_page_hash, split2.right_page_hash);
    assert_eq!(split1.timestamp, split2.timestamp);
}

#[test]
fn test_multiplicity_set_rejects_duplicates() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Set);

    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let object = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);
    let pair = Pair2::new(subject, object);

    // First insert succeeds
    assert!(page.insert(pair));
    assert_eq!(page.length, 1);

    // Duplicate insert fails
    assert!(!page.insert(pair));
    assert_eq!(page.length, 1, "Set multiplicity should reject duplicates");
}

#[test]
fn test_multiplicity_bag_accepts_duplicates() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Bag);

    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let object = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);
    let pair = Pair2::new(subject, object);

    // First insert succeeds
    assert!(page.insert(pair));
    assert_eq!(page.length, 1);
    assert_eq!(page.pair_counts[0], 1);

    // Duplicate insert succeeds and increments count
    assert!(page.insert(pair));
    assert_eq!(page.length, 1, "Bag multiplicity keeps single pair entry");
    assert_eq!(page.pair_counts[0], 2, "Bag should track count");
}

#[test]
fn test_stream_multiplicity_timestamp_addressed() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Stream);

    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let object = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);

    let pair_ts1 = Pair2::with_timestamp(subject, object, 1000u64);
    let pair_ts2 = Pair2::with_timestamp(subject, object, 2000u64);

    // Both should insert successfully (Stream allows duplicates)
    assert!(page.insert(pair_ts1));
    assert!(page.insert(pair_ts2));
    assert_eq!(page.length, 1);
    assert_eq!(page.pair_counts[0], 2);
}

#[test]
fn test_event_addressed_multiplicity() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::EventAddressed);

    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let object = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);

    let event_id_1 = [1u8; 8];
    let event_id_2 = [2u8; 8];

    let pair_ev1 = Pair2::with_event_id(subject, object, event_id_1);
    let pair_ev2 = Pair2::with_event_id(subject, object, event_id_2);

    // Both should insert successfully (EventAddressed allows duplicates)
    assert!(page.insert(pair_ev1));
    assert!(page.insert(pair_ev2));
    assert_eq!(page.length, 1);
    assert_eq!(page.pair_counts[0], 2);
}

#[test]
fn test_pair2_serialization_idempotent() {
    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let object = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);
    let pair = Pair2::with_timestamp(subject, object, 1234567890u64);

    let bytes1 = pair.to_bytes();
    let bytes2 = pair.to_bytes();

    assert_eq!(bytes1, bytes2, "Pair2 serialization must be idempotent");
}

#[test]
fn test_pair2_ignore_metadata_comparison() {
    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let object = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);

    let pair_no_ts = Pair2::new(subject, object);
    let pair_with_ts = Pair2::with_timestamp(subject, object, 5000u64);

    assert!(
        pair_no_ts.pairs_equal_ignoring_metadata(&pair_with_ts),
        "Pairs should be equal ignoring metadata"
    );
}

#[test]
fn test_multiplicity_allows_duplicates() {
    assert!(!Multiplicity::Set.allows_duplicates());
    assert!(Multiplicity::Bag.allows_duplicates());
    assert!(Multiplicity::Stream.allows_duplicates());
    assert!(Multiplicity::EventAddressed.allows_duplicates());
}

#[test]
fn test_page_split_hash_deterministic() {
    let page1 = RelationPage::new();
    let page2 = RelationPage::new();

    let hash = [42u8; HASH_SIZE];
    let time = 9999u64;

    let split1 = PageSplit::split(&page1, hash, time).unwrap();
    let split2 = PageSplit::split(&page2, hash, time).unwrap();

    // Same input produces same output
    assert_eq!(split1.left_page_hash, split2.left_page_hash);
    assert_eq!(split1.right_page_hash, split2.right_page_hash);
}

#[test]
fn test_both_domains_near_saturation() {
    let mut page = RelationPage::new();

    // Fill both domains to near capacity
    for i in 0..255 {
        let subject = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let pair = Pair2::new(subject, object);

        assert!(page.insert(pair), "Insert {} should succeed", i);
        assert!(page.remove(&pair));
    }

    assert_eq!(page.left_domain.count(), 255);
    assert_eq!(page.right_domain.count(), 255);

    // One more pair with new symbols on both sides should succeed
    let subject_new = Node8::from_bytes([255u8, 255u8, 0, 0, 0, 0, 0, 0]);
    let object_new = Node8::from_bytes([0, 0, 0, 0, 0, 0, 255u8, 255u8]);
    let pair_new = Pair2::new(subject_new, object_new);

    assert!(page.insert(pair_new), "Should fit both domains at 256");
    assert_eq!(page.left_domain.count(), 256);
    assert_eq!(page.right_domain.count(), 256);
}
