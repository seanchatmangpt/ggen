#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Lane V16 (v26.10.20 dialect slice 1): ground-data-tier streaming receipt
//! verification. Chicago-style: real byte streams in, assert on real verdicts.

use ggen_engine::receipt_dialect::verify_stream;

const BUDGET: u64 = 1 << 20;

#[test]
fn valid_nt_stream_passes_with_triple_count() {
    let input = b"\
# receipt log
<urn:r:evt1> <urn:r:type> \"commit\" .
<urn:r:evt2> <urn:r:parent> <urn:r:evt1> .

<urn:r:evt3> <urn:r:sha> \"abc123\"^^<urn:x:hex> .\n";
    let v = verify_stream(input, BUDGET).expect("valid stream must verify");
    assert_eq!(v.triples, 3);
    assert_eq!(v.quads, 0);
    assert_eq!(v.comments, 1);
    assert_eq!(v.blank_lines, 1);
    assert!(v.digest.is_some(), "facts present => chain digest present");
}

#[test]
fn nquads_graph_label_counted_as_quad() {
    let input = b"<urn:s> <urn:p> \"o\" <urn:g> .\n";
    let v = verify_stream(input, BUDGET).expect("quad line is in-subset");
    assert_eq!(v.triples, 0);
    assert_eq!(v.quads, 1);
}

#[test]
fn blank_node_refused_with_line_number_and_ground_code() {
    let input = b"<urn:a> <urn:b> <urn:c> .\n_:x <urn:p> \"v\" .\n";
    let err = verify_stream(input, BUDGET).expect_err("blank node must refuse");
    assert_eq!(err.code(), "FD-DIALECT_FEATURE_DISALLOWED");
    assert_eq!(err.line(), Some(2));
    assert!(
        err.detail().contains("ground"),
        "detail names the tier rule"
    );
}

#[test]
fn malformed_line_refused_with_line_number() {
    let input = b"<urn:a> <urn:b> <urn:c> .\n<urn:d> <urn:e>\n";
    let err = verify_stream(input, BUDGET).expect_err("missing terminator must refuse");
    assert_eq!(err.code(), "FD-MALFORMED_SYNTAX");
    assert_eq!(err.line(), Some(2));
}

#[test]
fn unterminated_literal_refused() {
    let err = verify_stream(b"<urn:a> <urn:b> \"oops .\n", BUDGET)
        .expect_err("unterminated literal must refuse");
    assert_eq!(err.code(), "FD-MALFORMED_SYNTAX");
    assert_eq!(err.line(), Some(1));
}

#[test]
fn two_term_line_refused() {
    let err =
        verify_stream(b"<urn:a> <urn:b> .\n", BUDGET).expect_err("2 terms is not a statement");
    assert_eq!(err.code(), "FD-MALFORMED_SYNTAX");
    assert_eq!(err.line(), Some(1));
}

#[test]
fn empty_input_is_empty_verdict_no_digest() {
    let v = verify_stream(b"", BUDGET).expect("empty input verifies trivially");
    assert_eq!(
        v,
        ggen_engine::receipt_dialect::StreamVerdict {
            triples: 0,
            quads: 0,
            comments: 0,
            blank_lines: 0,
            digest: None,
        }
    );
}

#[test]
fn budget_refusal_is_typed_limit() {
    let input = b"<urn:a> <urn:b> <urn:c> .\n";
    let err = verify_stream(input, 4).expect_err("over-budget input must refuse");
    assert_eq!(err.code(), "LIMIT_EXCEEDED");
    assert!(err.line().is_none());
    let l = err.limit().expect("typed limit payload present");
    assert_eq!(l.name, "receipt_stream_bytes");
    assert_eq!(l.observed, input.len() as u64);
    assert_eq!(l.max, 4);
    assert!(err.emit().starts_with("REFUSED:LIMIT_EXCEEDED"));
}

#[test]
fn digest_is_stable_across_line_reordering() {
    // Digest is over the SORTED canonicalized fact-line multiset, so the
    // same facts in any order produce the same chain digest (documented
    // decision in receipt_dialect.rs module docs: digest attests the fact
    // SET, not serialization order).
    let a = b"\
<urn:s1> <urn:p> \"one\" .
<urn:s2> <urn:p> \"two\" .
<urn:s3> <urn:p> \"three\" .\n";
    let mut reordered: Vec<u8> = Vec::new();
    reordered.extend_from_slice(b"<urn:s3> <urn:p> \"three\" .\n");
    reordered.extend_from_slice(b"<urn:s1> <urn:p> \"one\" .\n");
    reordered.extend_from_slice(b"<urn:s2> <urn:p> \"two\" .\n");

    let va = verify_stream(a, BUDGET).expect("stream A verifies");
    let vb = verify_stream(&reordered, BUDGET).expect("stream B verifies");
    assert_eq!(va.digest, vb.digest, "reorder must not change the digest");

    // Falsifier of vacuity: changing one fact MUST change the digest.
    let mut mutated = reordered.clone();
    mutated.extend_from_slice(b"<urn:s4> <urn:p> \"four\" .\n");
    let vm = verify_stream(&mutated, BUDGET).expect("mutated stream verifies");
    assert_ne!(
        va.digest, vm.digest,
        "content change must change the digest"
    );
}

#[test]
fn canonicalization_absorbs_whitespace_and_trailing_comments() {
    let tight = b"<urn:a> <urn:b> \"c\" .\n";
    let loose = b"<urn:a>   <urn:b>   \"c\"   . # trailing\n";
    let vt = verify_stream(tight, BUDGET).expect("tight verifies");
    let vl = verify_stream(loose, BUDGET).expect("loose verifies");
    assert_eq!(
        vt.digest, vl.digest,
        "whitespace/comment-only diffs must not move the digest"
    );
    assert_eq!(
        vl.comments, 0,
        "trailing comment is stripped, not a comment line"
    );
}
