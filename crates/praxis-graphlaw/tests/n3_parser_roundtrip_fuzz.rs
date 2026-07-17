//! Parser round-trip fuzzing (targets a specifically named low-confidence
//! N3 gap: this crate's own N3/Turtle parser -- not the reasoning logic --
//! is newer, pest-grammar-based code; a parser data-loss bug would corrupt
//! input silently, independent of anything the reasoner does). Generates
//! random valid N3 documents via `proptest`, parses them with ZERO rules
//! (so `materialize()` derives nothing new), and confirms the stored fact
//! set exactly matches what was written -- no triples dropped, duplicated,
//! or corrupted on the way in.

use praxis_graphlaw::TripleStore;
use proptest::prelude::*;
use std::collections::HashSet;

fn decode_all_stored_facts(store: &TripleStore) -> HashSet<String> {
    (0..store.len())
        .filter_map(|i| store.triple_index.get(i))
        .map(TripleStore::decode_triple)
        .collect()
}

/// Targeted (non-proptest) coverage for string-escape decoding: parse a
/// literal containing each supported escape sequence, decode the stored
/// fact, and compare against the exact expected decoded string. Covers all
/// four N3/Turtle string-quote forms ("...", '...', """...""", '''...''').
#[test]
fn escape_sequences_decode_to_expected_strings() {
    let cases: Vec<(&str, &str)> = vec![
        (r#""line1\nline2""#, "line1\nline2"),
        (r#""a\tb""#, "a\tb"),
        (r#""cr\rlf""#, "cr\rlf"),
        (r#""bell\bfeed""#, "bell\u{0008}feed"),
        (r#""form\ffeed""#, "form\u{000C}feed"),
        (r#""quote\"inside""#, "quote\"inside"),
        (r#""back\\slash""#, "back\\slash"),
        (r#""unicodeé""#, "unicode\u{00e9}"),
        (r#""astral\U0001F600""#, "astral\u{1F600}"),
        (r#"'single\'quote'"#, "single'quote"),
        (r#"'''triple\nsingle'''"#, "triple\nsingle"),
        (
            r#""""triple\ndouble\twith\ttabs""""#,
            "triple\ndouble\twith\ttabs",
        ),
    ];

    for (idx, (literal, expected_decoded)) in cases.iter().enumerate() {
        let doc = format!("@prefix : <http://example.org/> .\n:s{idx} :p{idx} {literal} .\n");
        let mut store = TripleStore::from(&doc);
        let _inferred = store.materialize().unwrap();
        let actual = decode_all_stored_facts(&store);

        let expected_fact = format!(
            "<http://example.org/s{idx}> <http://example.org/p{idx}> \"{}\"^^<http://www.w3.org/2001/XMLSchema#string>",
            expected_decoded
        );
        assert!(
            actual.contains(&expected_fact),
            "escape-sequence round-trip mismatch for literal {literal}.\nDocument:\n{doc}\nExpected fact: {expected_fact}\nActual facts: {actual:?}"
        );
    }
}

proptest! {
    #[test]
    fn prop_parser_roundtrip_preserves_exact_fact_set(
        // Each entry: (subject index, predicate index, object choice)
        triples in prop::collection::vec(
            (0usize..6, 0usize..4, prop::sample::select(vec![0u8, 1, 2])),
            0..12,
        ),
        int_vals in prop::collection::vec(-100i64..100, 12),
        str_vals in prop::collection::vec("[a-zA-Z]{1,8}", 12),
    ) {
        let mut doc = String::from("@prefix : <http://example.org/> .\n");
        let mut expected: HashSet<String> = HashSet::new();

        for (idx, &(subj, pred, obj_kind)) in triples.iter().enumerate() {
            let s_str = format!(":s{subj}");
            let p_str = format!(":p{pred}");
            let (obj_str, expected_o) = match obj_kind {
                0 => {
                    let v = int_vals[idx % int_vals.len()];
                    (v.to_string(), format!("\"{v}\"^^<http://www.w3.org/2001/XMLSchema#integer>"))
                }
                1 => {
                    // Per RDF 1.1, a plain string literal is xsd:string-typed;
                    // this crate's decoder makes that explicit (verified by
                    // running this test), not left implicit as bare `"v"`.
                    let v = &str_vals[idx % str_vals.len()];
                    (format!("\"{v}\""), format!("\"{v}\"^^<http://www.w3.org/2001/XMLSchema#string>"))
                }
                _ => {
                    let other_subj = (subj + 1) % 6;
                    (format!(":s{other_subj}"), format!("<http://example.org/s{other_subj}>"))
                }
            };
            doc.push_str(&format!("{s_str} {p_str} {obj_str} .\n"));
            expected.insert(format!(
                "<http://example.org/s{subj}> <http://example.org/p{pred}> {expected_o}"
            ));
        }

        let mut store = TripleStore::from(&doc);
        let _inferred = store.materialize().unwrap(); // zero rules in doc -> derives nothing new
        let actual = decode_all_stored_facts(&store);

        prop_assert_eq!(
            &actual, &expected,
            "parser round-trip mismatch.\nDocument:\n{}\nExpected facts: {:?}\nActual facts: {:?}",
            doc, expected, actual
        );
    }
}
