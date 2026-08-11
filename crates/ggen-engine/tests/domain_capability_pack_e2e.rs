//! Chicago-TDD e2e proof for `packs/domain-capability-pack` — real filesystem, real
//! `sync()`, real assertions on real file content, no mocks. Closes the automation
//! gap the pack itself opened: `scripts/ci/guard-pack-e2e-coverage.sh` only counts a
//! pack "covered" if its directory name appears literally in a file under this
//! directory — the pack's own manual scratch-consumer verification (real, but run
//! outside `cargo test`) does not count.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;
use std::path::{Path, PathBuf};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

#[test]
fn domain_capability_pack_generates_and_is_idempotent() {
    // 1. SCAFFOLD: copy the real pack + a minimal consumer into a TempDir. The
    //    pack's own ontology.ttl already carries the real, transcribed 14-capability
    //    / 1-allowlist worked instance -- no extra consumer facts are needed.
    let (_dir, project) = scaffold_pack(&packs_dir().join("domain-capability-pack"));

    // 2. GENERATE + ASSERT REAL CONTENT.
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    let doc = read(&project, "docs/domain-capability/sregym-capabilities.md");

    // Exactly 14 real capability rows (order 1..14, one table row each) -- excludes
    // the header row ("| # | Slug | ...") and the "|---|" separator row.
    let row_count = doc
        .lines()
        .filter(|l| l.starts_with("| ") && !l.starts_with("| #"))
        .count();
    assert_eq!(
        row_count, 14,
        "expected exactly 14 real SREGYM_CAPABILITIES rows, got {row_count}:\n{doc}"
    );

    // The 5 real allowlist entries render as admitted...
    for slug in [
        "observe_cluster_state",
        "run_kubectl",
        "get_benchmark_status",
        "submit_diagnosis",
        "submit_mitigation",
    ] {
        let line = doc
            .lines()
            .find(|l| l.contains(&format!("`{slug}`")))
            .unwrap_or_else(|| panic!("row for {slug} not found:\n{doc}"));
        assert!(
            line.contains("| true |"),
            "{slug} must be admitted=true (it's in the real 5-entry autofde-lab allowlist): {line}"
        );
    }

    // ...and a real, non-admitted capability (the drift gymact grew past the stale
    // autofde-lab allowlist comment) renders as NOT admitted, proving the join is
    // real, not a constant.
    let jaeger_line = doc
        .lines()
        .find(|l| l.contains("`jaeger_get_services`"))
        .expect("jaeger_get_services row not found");
    assert!(
        jaeger_line.contains("| false |"),
        "jaeger_get_services is real in gymact but NOT in autofde-lab's allowlist: {jaeger_line}"
    );

    // 3. IDEMPOTENCY.
    assert_idempotent(&project);
}

#[test]
fn domain_capability_pack_gate_refuses_missing_required_property() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("domain-capability-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE (previously untested branch of 010_required.rq): a dcp:Capability
    // missing a required property (here, dcp:slug) must be refused, citing
    // 010_required by name -- this gate had zero real sabotage coverage before this
    // test, a real decorative-gate risk on the pack's most basic completeness check.
    assert_gate_refuses(
        &project,
        "@prefix dcp: <http://seanchatmangpt.github.io/packs/domain-capability#> .\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
         dcp:sabotage-incomplete-capability a dcp:Capability ;\n\
         \x20\x20\x20\x20dcp:order 1 ; rdfs:label \"Sabotage\" ; dcp:consequence \"DO\" ;\n\
         \x20\x20\x20\x20dcp:worldBinding \"sabotage\" ; dcp:sourceRepo \"nowhere\" ;\n\
         \x20\x20\x20\x20dcp:sourceFile \"nowhere.py\" .\n",
        "010_required",
    );
}

#[test]
fn domain_capability_pack_gate_refuses_out_of_enum_consequence() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("domain-capability-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE (the second, closed-enum branch of 010_required.rq): a
    // dcp:consequence value outside "READ"|"DO" must be refused, citing 010_required
    // by name.
    assert_gate_refuses(
        &project,
        "@prefix dcp: <http://seanchatmangpt.github.io/packs/domain-capability#> .\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
         dcp:sabotage-bad-consequence a dcp:Capability ;\n\
         \x20\x20\x20\x20dcp:order 1 ; dcp:slug \"sabotage\" ; rdfs:label \"Sabotage\" ;\n\
         \x20\x20\x20\x20dcp:consequence \"MAYBE\" ; dcp:worldBinding \"sabotage\" ;\n\
         \x20\x20\x20\x20dcp:sourceRepo \"nowhere\" ; dcp:sourceFile \"nowhere.py\" .\n",
        "010_required",
    );
}

#[test]
fn domain_capability_pack_gate_refuses_allowlist_referencing_bogus_capability() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("domain-capability-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // 4. GATE SABOTAGE: an allowlist admitting a capability that doesn't exist in
    //    the graph must be refused, citing 030_allowlist_subset by name.
    assert_gate_refuses(
        &project,
        "@prefix dcp: <http://seanchatmangpt.github.io/packs/domain-capability#> .\n\
         dcp:sabotage-allowlist a dcp:LabAllowlist ;\n\
         \x20\x20\x20\x20dcp:ownerRepo \"sabotage-repo\" ;\n\
         \x20\x20\x20\x20dcp:allows dcp:this-capability-does-not-exist .\n",
        "030_allowlist_subset",
    );
}

#[test]
fn domain_capability_pack_gate_refuses_capability_count_drift() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("domain-capability-pack"));
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // A manifest declaring a count that doesn't match the real, admitted capability
    // count for that (sourceRepo, sourceFile) pair must be refused, citing
    // 020_exact_count_per_source by name -- including the zero-real-capabilities
    // edge case the gate's OPTIONAL/COALESCE handling exists for.
    assert_gate_refuses(
        &project,
        "@prefix dcp: <http://seanchatmangpt.github.io/packs/domain-capability#> .\n\
         dcp:sabotage-manifest a dcp:CapabilitySourceManifest ;\n\
         \x20\x20\x20\x20dcp:manifestSourceRepo \"made-up-repo\" ;\n\
         \x20\x20\x20\x20dcp:manifestSourceFile \"does/not/exist.py\" ;\n\
         \x20\x20\x20\x20dcp:expectedCapabilityCount 99 .\n",
        "020_exact_count_per_source",
    );
}
