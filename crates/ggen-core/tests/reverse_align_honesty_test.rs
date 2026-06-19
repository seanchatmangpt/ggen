//! Honesty test for the public-ontology alignment gate (Phase 2).
//!
//! Chicago TDD: real temp filesystem, real oxigraph graphs. Proves the gate
//! admits a domain graph that composes only REAL (defined) public terms, and
//! REFUSES one that references a fabricated public term — the "no fabrication"
//! discipline applied to ontology composition.

use std::path::PathBuf;

use ggen_core::reverse::check_alignment;

fn temp_dir() -> PathBuf {
    let root = std::env::temp_dir().join(format!("ggen_reverse_align_{}", uuid::Uuid::new_v4()));
    std::fs::create_dir_all(&root).expect("create temp dir");
    root
}

/// Write a minimal real PROV-O subset into `dir/public/prov.ttl`.
fn vendor_prov(dir: &std::path::Path) -> PathBuf {
    let public = dir.join("public");
    std::fs::create_dir_all(&public).expect("mk public");
    std::fs::write(
        public.join("prov.ttl"),
        "@prefix prov: <http://www.w3.org/ns/prov#> .\n\
         @prefix owl: <http://www.w3.org/2002/07/owl#> .\n\
         prov:Entity a owl:Class .\n\
         prov:Activity a owl:Class .\n\
         prov:wasGeneratedBy a owl:ObjectProperty .\n",
    )
    .expect("write prov");
    public
}

#[test]
fn aligned_domain_is_admitted() {
    let dir = temp_dir();
    let public = vendor_prov(&dir);
    let domain = dir.join("domain.ttl");
    std::fs::write(
        &domain,
        "@prefix prov: <http://www.w3.org/ns/prov#> .\n\
         @prefix ex: <https://example.org/eden#> .\n\
         ex:Receipt a prov:Entity ; prov:wasGeneratedBy ex:SyncRun .\n\
         ex:SyncRun a prov:Activity .\n",
    )
    .expect("write domain");

    let report = check_alignment(&domain, &public).expect("alignment runs");
    assert!(
        report.is_aligned(),
        "domain composing only defined PROV terms must be admitted; unaligned={:?}",
        report.unaligned
    );
    assert!(report
        .admitted
        .iter()
        .any(|t| t == "http://www.w3.org/ns/prov#Entity"));
    assert!(report
        .admitted
        .iter()
        .any(|t| t == "http://www.w3.org/ns/prov#wasGeneratedBy"));

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn fabricated_public_term_is_refused() {
    let dir = temp_dir();
    let public = vendor_prov(&dir);
    let domain = dir.join("domain.ttl");
    // `prov:conjuredFrom` is NOT a real PROV term and is not vendored.
    std::fs::write(
        &domain,
        "@prefix prov: <http://www.w3.org/ns/prov#> .\n\
         @prefix ex: <https://example.org/eden#> .\n\
         ex:Receipt a prov:Entity ; prov:conjuredFrom ex:Nothing .\n",
    )
    .expect("write domain");

    let report = check_alignment(&domain, &public).expect("alignment runs");
    assert!(
        !report.is_aligned(),
        "a fabricated public term must refuse admission"
    );
    assert!(
        report
            .unaligned
            .iter()
            .any(|t| t == "http://www.w3.org/ns/prov#conjuredFrom"),
        "the fabricated term must be named in `unaligned`, got {:?}",
        report.unaligned
    );
    // The real term it also used is still admitted (precise, not all-or-nothing).
    assert!(report
        .admitted
        .iter()
        .any(|t| t == "http://www.w3.org/ns/prov#Entity"));

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn missing_domain_graph_fails_loudly() {
    let dir = temp_dir();
    let public = vendor_prov(&dir);
    let result = check_alignment(&dir.join("nope.ttl"), &public);
    assert!(result.is_err(), "missing domain graph must error");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn empty_public_dir_fails_loudly() {
    let dir = temp_dir();
    let empty_public = dir.join("public");
    std::fs::create_dir_all(&empty_public).expect("mk empty public");
    let domain = dir.join("domain.ttl");
    std::fs::write(&domain, "@prefix ex: <https://example.org/e#> .\nex:A a ex:B .\n")
        .expect("write domain");
    let result = check_alignment(&domain, &empty_public);
    assert!(
        result.is_err(),
        "no vendored ontologies means nothing to admit against — must error"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
