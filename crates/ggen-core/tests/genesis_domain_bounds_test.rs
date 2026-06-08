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

//! Domain bounds enforcement and saturation testing for Genesis primitives.
//! Tests that left and right domains enforce 256-symbol limits and proper refusal.
//! Chicago TDD: Real state changes, deterministic bounds checking, no mocks.

use ggen_core::genesis::{
    Multiplicity, Node8, Object8, Pair2, RefusalCode, RelationPage, SymbolDomain, HASH_SIZE,
};

#[test]
fn test_symbol_domain_zero_initialization() {
    let domain = SymbolDomain::new();
    assert_eq!(domain.count(), 0);
    assert!(!domain.contains(&[0u8; 8]));
}

#[test]
fn test_symbol_domain_single_insert_and_retrieval() {
    let mut domain = SymbolDomain::new();
    let symbol = [42u8, 99, 88, 77, 66, 55, 44, 33];

    let result = domain.insert(symbol);
    assert!(result.is_ok());
    assert!(result.unwrap());
    assert_eq!(domain.count(), 1);
    assert!(domain.contains(&symbol));
}

#[test]
fn test_symbol_domain_duplicate_insert_returns_false() {
    let mut domain = SymbolDomain::new();
    let symbol = [123u8, 45, 67, 89, 10, 11, 12, 13];

    // First insert succeeds and returns true (new)
    let first = domain.insert(symbol).unwrap();
    assert!(first, "First insert should return true for new symbol");

    // Second insert succeeds but returns false (already present)
    let second = domain.insert(symbol).unwrap();
    assert!(!second, "Second insert of same symbol should return false");
    assert_eq!(domain.count(), 1, "Count should remain 1");
}

#[test]
fn test_symbol_domain_255_distinct_symbols() {
    let mut domain = SymbolDomain::new();

    // Insert 255 distinct symbols
    for i in 0..255 {
        let symbol = [(i & 0xFF) as u8, 0, 0, 0, 0, 0, 0, 0];
        let result = domain.insert(symbol);
        assert!(result.is_ok(), "Insert {} should succeed", i);
        assert!(result.unwrap(), "Insert {} should be new", i);
        assert_eq!(
            domain.count(),
            i + 1,
            "Count at iteration {} should be {}",
            i,
            i + 1
        );
    }

    assert_eq!(domain.count(), 255);
}

#[test]
fn test_symbol_domain_256th_insert_fails_with_refusal() {
    let mut domain = SymbolDomain::new();

    // Fill to 256
    for i in 0..256 {
        let symbol = [(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0];
        let _ = domain.insert(symbol);
    }

    // 257th insert should fail
    let symbol_257 = [255u8, 255u8, 0, 0, 0, 0, 0, 0];
    let result = domain.insert(symbol_257);
    assert_eq!(
        result,
        Err(RefusalCode::PageSplitRequired),
        "257th insert must return PageSplitRequired"
    );
    assert_eq!(domain.count(), 256, "Count must remain 256");
}

#[test]
fn test_relation_page_left_domain_saturation_blocks_insertion() {
    let mut page = RelationPage::new();

    // Insert pairs with 256 distinct subjects (left domain)
    for i in 0..256 {
        let subject = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let pair = Pair2::new(subject, object);

        assert!(
            page.insert(pair),
            "Insert pair {} into left domain should succeed",
            i
        );
        assert!(page.remove(&pair));
    }

    // Verify left domain is at capacity
    assert_eq!(
        page.left_domain.count(),
        256,
        "Left domain should have 256 symbols"
    );

    // 257th insert with new subject should fail
    let subject_257 = Node8::from_bytes([255u8, 255u8, 0, 0, 0, 0, 0, 0]);
    let object_257 = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 255]);
    let pair_257 = Pair2::new(subject_257, object_257);

    assert!(
        !page.insert(pair_257),
        "Insert at left domain saturation must return false"
    );
    assert_eq!(
        page.left_domain.count(),
        256,
        "Left domain count must remain 256"
    );
}

#[test]
fn test_relation_page_right_domain_saturation_blocks_insertion() {
    let mut page = RelationPage::new();

    // Insert pairs with 256 distinct objects (right domain)
    for i in 0..256 {
        let subject = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let object = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let pair = Pair2::new(subject, object);

        assert!(
            page.insert(pair),
            "Insert pair {} into right domain should succeed",
            i
        );
        assert!(page.remove(&pair));
    }

    // Verify right domain is at capacity
    assert_eq!(
        page.right_domain.count(),
        256,
        "Right domain should have 256 symbols"
    );

    // 257th insert with new object should fail
    let subject_257 = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 255]);
    let object_257 = Node8::from_bytes([255u8, 255u8, 0, 0, 0, 0, 0, 0]);
    let pair_257 = Pair2::new(subject_257, object_257);

    assert!(
        !page.insert(pair_257),
        "Insert at right domain saturation must return false"
    );
    assert_eq!(
        page.right_domain.count(),
        256,
        "Right domain count must remain 256"
    );
}

#[test]
fn test_both_domains_can_each_reach_255() {
    let mut page = RelationPage::new();

    // Interleave inserts: i subject with i object (distinct)
    for i in 0..256 {
        let subject = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let pair = Pair2::new(subject, object);

        assert!(
            page.insert(pair),
            "Pair {} should insert when both domains have space",
            i
        );
        assert!(page.remove(&pair));
    }

    assert_eq!(page.left_domain.count(), 256);
    assert_eq!(page.right_domain.count(), 256);
}

#[test]
fn test_multiplicity_set_respects_domain_bounds() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Set);

    // Fill both domains to near capacity (255)
    for i in 0..255 {
        let subject = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let pair = Pair2::new(subject, object);
        assert!(page.insert(pair));
        assert!(page.remove(&pair));
    }

    assert_eq!(page.left_domain.count(), 255);
    assert_eq!(page.right_domain.count(), 255);

    // One more pair with both new symbols succeeds (reaches 256)
    let subj_256 = Node8::from_bytes([222u8; 8]);
    let obj_256 = Node8::from_bytes([111u8; 8]);
    let pair_256 = Pair2::new(subj_256, obj_256);
    assert!(page.insert(pair_256));
    assert!(page.remove(&pair_256));

    assert_eq!(page.left_domain.count(), 256);
    assert_eq!(page.right_domain.count(), 256);

    // Next insert with any new symbol must fail
    let next_subj = Node8::from_bytes([255u8, 255u8, 1u8, 0, 0, 0, 0, 0]);
    let next_obj = Node8::from_bytes([0, 0, 0, 0, 0, 0, 1u8, 255u8]);
    let next_pair = Pair2::new(next_subj, next_obj);
    assert!(!page.insert(next_pair), "Insert at saturation must fail");
}

#[test]
fn test_multiplicity_bag_respects_domain_bounds() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Bag);

    // Fill both domains to 255
    for i in 0..255 {
        let subject = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let pair = Pair2::new(subject, object);
        assert!(page.insert(pair));
        assert!(page.remove(&pair));
    }

    // One more pair succeeds (reaches 256)
    let subj_256 = Node8::from_bytes([222u8; 8]);
    let obj_256 = Node8::from_bytes([111u8; 8]);
    let pair = Pair2::new(subj_256, obj_256);
    assert!(page.insert(pair));

    // Duplicates of this pair should still succeed and increment count in Bag mode
    let dup = Pair2::new(subj_256, obj_256);
    assert!(
        page.insert(dup),
        "Duplicate with existing symbols should increment count in Bag mode"
    );

    // New pair with new symbols must fail (domains are full)
    let new_subj = Node8::from_bytes([255u8, 255u8, 1u8, 0, 0, 0, 0, 0]);
    let new_obj = Node8::from_bytes([0, 0, 0, 0, 0, 0, 1u8, 255u8]);
    let new_pair = Pair2::new(new_subj, new_obj);
    assert!(
        !page.insert(new_pair),
        "New pair at domain saturation must fail even in Bag mode"
    );
}

#[test]
fn test_multiplicity_stream_respects_domain_bounds() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Stream);

    // Fill domains
    for i in 0..256 {
        let subject = Node8::from_bytes([(i & 0xFF) as u8, (i >> 8) as u8, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (i & 0xFF) as u8]);
        let pair = Pair2::new(subject, object);
        assert!(page.insert(pair));
        assert!(page.remove(&pair));
    }

    // Trying to add new symbols should fail (domain saturation overrides Stream semantics)
    let new_subj = Node8::from_bytes([255u8, 255u8, 0, 0, 0, 0, 0, 0]);
    let new_obj = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 255]);
    let new_pair = Pair2::with_timestamp(new_subj, new_obj, 1000u64);
    assert!(
        !page.insert(new_pair),
        "New pair at Stream domain saturation must fail"
    );
}

#[test]
fn test_symbol_domain_deterministic_lookup() {
    let mut domain1 = SymbolDomain::new();
    let mut domain2 = SymbolDomain::new();

    let symbols = vec![[10u8; 8], [20u8; 8], [30u8; 8], [40u8; 8]];

    // Insert into both in same order
    for sym in &symbols {
        let _ = domain1.insert(*sym);
        let _ = domain2.insert(*sym);
    }

    // Both should have same count
    assert_eq!(domain1.count(), domain2.count());

    // Both should contain same symbols
    for sym in &symbols {
        assert!(domain1.contains(sym));
        assert!(domain2.contains(sym));
    }
}

#[test]
fn test_page_domain_tracking_independent() {
    let mut page = RelationPage::new();

    // Insert pairs where subjects and objects are different ranges
    for i in 0..100 {
        let subject = Node8::from_bytes([i as u8, 0, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, (100 + i) as u8]);
        let pair = Pair2::new(subject, object);
        assert!(page.insert(pair));
        assert!(page.remove(&pair));
    }

    // Left domain should have 100 symbols (0-99)
    assert_eq!(page.left_domain.count(), 100);
    // Right domain should have 100 symbols (100-199)
    assert_eq!(page.right_domain.count(), 100);
}
