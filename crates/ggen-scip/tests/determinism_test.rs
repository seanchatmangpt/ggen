//! Determinism: two emits over the same surface are byte-identical.

use ggen_scip::emit::{build_index, emit};
use std::path::Path;

const ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../..");

#[test]
fn two_emits_are_byte_identical() {
    let root = Path::new(ROOT);
    let a = ggen_scip::canonical_json(&build_index(root).unwrap()).unwrap();
    let b = ggen_scip::canonical_json(&build_index(root).unwrap()).unwrap();
    assert_eq!(a, b, "canonical emit must be byte-deterministic");
}

#[test]
fn emit_writes_scip_json() {
    let root = Path::new(ROOT);
    let index = emit(root).expect("emit");
    let on_disk = std::fs::read_to_string(root.join(".ggen/scip.json")).unwrap();
    let round: ggen_scip::Index = serde_json::from_str(&on_disk).unwrap();
    assert_eq!(round.documents.len(), index.documents.len());
}
