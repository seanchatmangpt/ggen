//! L5 condition 13 (義務数が憲法判定後の未解決義務から計算される -- "obligation count is
//! computed from unresolved obligations post-constitutional-judgment"): proves that
//! `praxis_core::receipt_epoch::ObligationCount` on a real `ggen sync run` receipt is now a
//! live query over `ccn:Law` individuals in the loaded graph, not merely a query over
//! gate/sync admission outcomes as it was before (see `crates/ggen-engine/src/sync.rs`'s
//! `write_receipt` constitution-law admission-items block).
//!
//! Real `TempDir`, real `sync()`, real BLAKE3-chained receipt read back from disk -- no mocks.
//! `ccn:mechanized` is a STATIC, HUMAN-AUTHORED boolean in the ontology (see
//! `packs/ggen-constitution-pack/ontology.ttl`'s own doc comment); these tests prove the
//! obligation count tracks THAT flag live, not that the pipeline independently re-verifies
//! whether each law is actually enforced -- an unmechanized law is an honestly-recorded open
//! obligation, never a claim of a machine-checked judgment.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions, SyncReceipt, RECEIPT_REL_PATH};
use praxis_core::receipt_epoch::{read_receipt_epoch, ObligationCount};

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

const CCN_PREFIX: &str =
    "@prefix ccn: <http://seanchatmangpt.github.io/packs/ggen-constitution#> .\n";

/// A synthetic mini-constitution: 3 unmechanized laws (A, B, C), 2 mechanized
/// laws (D, E) -- deliberately using the real `ccn:` namespace/predicates
/// (`a ccn:Law`, `ccn:mechanized`) so this test exercises the exact same
/// query `write_receipt` runs against a real constitution pack, without
/// depending on `packs/ggen-constitution-pack/ontology.ttl`'s own evolving
/// content.
const CONSTITUTION_3_FALSE_2_TRUE: &str = r#"
ccn:LawA a ccn:Law ; ccn:mechanized false .
ccn:LawB a ccn:Law ; ccn:mechanized false .
ccn:LawC a ccn:Law ; ccn:mechanized false .
ccn:LawD a ccn:Law ; ccn:mechanized true .
ccn:LawE a ccn:Law ; ccn:mechanized true .
"#;

/// Same 5 laws, but `LawC` flipped from `false` to `true` (2 unmechanized, 3
/// mechanized) -- the sabotage variant proving the count tracks live graph
/// state rather than a number computed once and cached.
const CONSTITUTION_2_FALSE_3_TRUE: &str = r#"
ccn:LawA a ccn:Law ; ccn:mechanized false .
ccn:LawB a ccn:Law ; ccn:mechanized false .
ccn:LawC a ccn:Law ; ccn:mechanized true .
ccn:LawD a ccn:Law ; ccn:mechanized true .
ccn:LawE a ccn:Law ; ccn:mechanized true .
"#;

fn scaffold(root: &Path, constitution: &str) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    ttl.push_str("ex:alice ex:name \"alice\" .\n");
    ttl.push_str(CCN_PREFIX);
    ttl.push_str(constitution);
    std::fs::write(root.join("ontology.ttl"), ttl).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/one.tmpl"), TEMPLATE).expect("write template");
}

fn read_obligation_count(root: &Path) -> ObligationCount {
    let raw = std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt");
    let receipt: SyncReceipt = serde_json::from_str(&raw).expect("parse receipt");
    let epoch = read_receipt_epoch(&receipt.record).expect("receipt is a readable v2 epoch");
    epoch.obligation_count
}

// ---------------------------------------------------------------------------
// (a) obligation_count.required is non-zero and matches the real count of
//     ccn:mechanized=false laws in the loaded ontology
// ---------------------------------------------------------------------------

#[test]
fn obligation_count_required_matches_real_count_of_unmechanized_laws() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    scaffold(dir.path(), CONSTITUTION_3_FALSE_2_TRUE);

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("real sync with a loaded ccn:Law ontology");

    let obligation_count = read_obligation_count(dir.path());
    match obligation_count {
        ObligationCount::Tracked {
            required,
            discharged,
        } => {
            assert_eq!(
                required, 3,
                "required must equal the real count of ccn:mechanized=false laws (LawA/B/C)"
            );
            assert_ne!(
                required, 0,
                "3 unmechanized laws must not report a zero obligation count"
            );
            assert_eq!(
                discharged, 2,
                "discharged must equal the real count of ccn:mechanized=true laws (LawD/E)"
            );
        }
        ObligationCount::Unknown => panic!("a real v2 sync receipt must never report Unknown"),
    }
}

// ---------------------------------------------------------------------------
// (b) sabotage: flipping one law's ccn:mechanized false -> true decreases the
//     obligation count by exactly 1, proving live graph tracking, not a
//     hardcoded/cached number
// ---------------------------------------------------------------------------

#[test]
fn flipping_one_law_to_mechanized_decreases_obligation_count_by_exactly_one() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    scaffold(dir.path(), CONSTITUTION_3_FALSE_2_TRUE);

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("first real sync");
    let before = match read_obligation_count(dir.path()) {
        ObligationCount::Tracked { required, .. } => required,
        ObligationCount::Unknown => panic!("must be Tracked"),
    };

    // Sabotage: overwrite the ontology with LawC flipped false -> true (a
    // real edit to a real file in the temp project, not an in-memory patch).
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    ttl.push_str("ex:alice ex:name \"alice\" .\n");
    ttl.push_str(CCN_PREFIX);
    ttl.push_str(CONSTITUTION_2_FALSE_3_TRUE);
    std::fs::write(dir.path().join("ontology.ttl"), ttl).expect("rewrite ontology (sabotage)");

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("second real sync over the sabotaged ontology");
    let after = match read_obligation_count(dir.path()) {
        ObligationCount::Tracked { required, .. } => required,
        ObligationCount::Unknown => panic!("must be Tracked"),
    };

    assert_eq!(
        before, 3,
        "precondition: 3 unmechanized laws before the flip"
    );
    assert_eq!(
        after, 2,
        "exactly one law's obligation must resolve after the flip"
    );
    assert_eq!(
        before - after,
        1,
        "flipping exactly one law's ccn:mechanized must decrease required by exactly 1, \
         proving the count tracks live graph state rather than a cached/hardcoded number"
    );
}

// ---------------------------------------------------------------------------
// (c) a project with no ccn:Law individuals loaded (no constitution pack) is
//     entirely unaffected -- zero rows back, obligation_count stays the
//     pre-existing all-zero Tracked value, never Unknown and never non-zero
//     from nothing
// ---------------------------------------------------------------------------

#[test]
fn project_without_any_ccn_law_individuals_reports_zero_required() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    // scaffold() with an empty constitution body: no ccn:Law individuals at
    // all, matching every pre-existing fixture in this test suite that
    // predates this feature.
    scaffold(dir.path(), "");

    sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("real sync with no ccn:Law individuals loaded");

    match read_obligation_count(dir.path()) {
        ObligationCount::Tracked {
            required,
            discharged,
        } => {
            assert_eq!(required, 0);
            assert_eq!(discharged, 0);
        }
        ObligationCount::Unknown => panic!("a real v2 sync receipt must never report Unknown"),
    }
}
