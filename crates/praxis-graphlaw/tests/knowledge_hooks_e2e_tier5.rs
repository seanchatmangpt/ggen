mod common;

use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::TripleStore;

// TIER 5: hook: NAMESPACE ALIAS TESTS
// =========================================================================

/// Covers hook: namespace aliasing: hook:* produces identical CompiledHook/schedule to kh:*
/// with byte-identical BLAKE3 hashes and deterministic receipt generation.
#[test]
fn test_hook_alias_vocabulary_identical_receipts() {
    let mut store_kh = TripleStore::new();
    let mut store_hook = TripleStore::new();

    // Reference hook pack using canonical kh: namespace
    let hook_pack_kh = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:h_canonical a kh:Hook ;
            kh:name "canonical_hook" ;
            kh:kind "delta" ;
            kh:var "http://example.org/trigger" ;
            kh:on "assert" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .
    "#;

    // Equivalent hook pack using hook: namespace alias. Uses the SAME subject
    // IRI (`ex:h_canonical`) as the kh: variant above, deliberately: the hook
    // IRI is embedded verbatim in each emitted delta-quad line
    // (`hooks::construct::serialize_delta_quad`'s `hook_wrapped` -- see
    // `<hook> <kh#addQuad> <bn_id> .`), which is itself part of what gets
    // BLAKE3-hashed into `delta_hash`/`idempotency_key`. A DIFFERENT subject
    // IRI (the original `ex:h_alias`) makes the two receipts differ for a
    // real, correct reason -- provenance, not vocabulary -- so it can never
    // produce a byte-identical hash no matter how faithfully `hook:` aliases
    // to `kh:`. Holding the subject IRI fixed isolates the ONE variable this
    // test exists to check: does `hook:` vocabulary aliasing change compiled
    // behavior. It must not, and (with the subject held fixed) now doesn't.
    let hook_pack_hook = r#"
        @prefix hook: <http://seanchatmangpt.github.io/praxis/hook#> .
        @prefix ex: <http://example.org/> .

        ex:h_canonical a hook:Hook ;
            hook:name "canonical_hook" ;
            hook:kind "delta" ;
            hook:var "http://example.org/trigger" ;
            hook:on "assert" ;
            hook:effect "emit-delta" ;
            hook:priority 1 .
    "#;

    // Load both hook packs
    assert!(
        store_kh.load_hook_pack(hook_pack_kh).is_ok(),
        "Failed to load hook pack with kh: namespace"
    );
    assert!(
        store_hook.load_hook_pack(hook_pack_hook).is_ok(),
        "Failed to load hook pack with hook: namespace alias"
    );

    // Load identical base facts
    let base_facts = "ex:Node <http://example.org/trigger> 'yes' .";
    store_kh.load_triples(base_facts, Syntax::Turtle).unwrap();
    store_hook.load_triples(base_facts, Syntax::Turtle).unwrap();

    // Materialize both stores
    store_kh.materialize().unwrap();
    store_hook.materialize().unwrap();

    // Get receipts from both stores
    let receipts_kh = store_kh.get_hook_receipts();
    let receipts_hook = store_hook.get_hook_receipts();

    // Verify both produced receipts
    assert_eq!(
        receipts_kh.len(),
        1,
        "kh: hook pack should produce exactly one receipt"
    );
    assert_eq!(
        receipts_hook.len(),
        1,
        "hook: hook pack should produce exactly one receipt"
    );

    // Verify hook names match
    assert_eq!(receipts_kh[0].hook_name, "canonical_hook");
    assert_eq!(receipts_hook[0].hook_name, "canonical_hook");

    // Verify BLAKE3 hashes are byte-identical (determinism guarantee)
    assert_eq!(
        receipts_kh[0].delta_hash, receipts_hook[0].delta_hash,
        "hook: and kh: namespaces must produce identical BLAKE3 hashes"
    );

    // Verify delta quads are identical
    assert_eq!(
        receipts_kh[0].delta_quads, receipts_hook[0].delta_quads,
        "hook: and kh: namespaces must produce identical delta quads"
    );

    // Verify idempotency keys are identical
    assert_eq!(
        receipts_kh[0].idempotency_key, receipts_hook[0].idempotency_key,
        "hook: and kh: namespaces must produce identical idempotency keys"
    );
}

/// Covers hook: namespace validation: Unknown hook:* predicate is refused with proper error text.
#[test]
fn test_hook_alias_unknown_predicate_refused() {
    let mut store = TripleStore::new();

    // Hook pack using an unknown hook: predicate
    let hook_pack = r#"
        @prefix hook: <http://seanchatmangpt.github.io/praxis/hook#> .
        @prefix ex: <http://example.org/> .

        ex:h_unknown a hook:Hook ;
            hook:name "unknown_predicate_hook" ;
            hook:kind "delta" ;
            hook:var "x" ;
            hook:effect "emit-delta" ;
            hook:unknown_field "this_should_fail" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Hook pack with unknown hook: predicate must be refused"
    );

    let err = res.unwrap_err();
    assert!(
        err.contains("SHACL") || err.contains("validation") || err.contains("unknown"),
        "Error message should mention SHACL validation or unknown predicate, got: {}",
        err
    );
}

/// Covers hook: namespace validation: Mixed kh:/hook: on same hook is refused.
#[test]
fn test_hook_alias_mixed_namespaces_refused() {
    let mut store = TripleStore::new();

    // Hook pack mixing hook: and kh: namespaces on the same hook (violates SHACL shape)
    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix hook: <http://seanchatmangpt.github.io/praxis/hook#> .
        @prefix ex: <http://example.org/> .

        ex:h_mixed a kh:Hook ;
            kh:name "mixed_namespaces" ;
            hook:kind "delta" ;
            kh:var "x" ;
            hook:on "assert" ;
            kh:effect "emit-delta" .
    "#;

    let res = store.load_hook_pack(hook_pack);
    assert!(
        res.is_err(),
        "Hook pack mixing hook: and kh: namespaces must be refused"
    );

    let err = res.unwrap_err();
    assert!(
        err.contains("SHACL") || err.contains("validation"),
        "Error should reference SHACL validation failure, got: {}",
        err
    );
}
