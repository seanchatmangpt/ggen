//! Chicago-TDD e2e proof for `packs/k8s-pack-DESIGN` — real filesystem, real
//! `sync()`, real assertions on real file content, no mocks. Closes step 3 of
//! the pack's own `NOTES.md` promotion checklist ("Add a
//! `crates/ggen-engine/tests/k8s_pack_e2e.rs` using the standard
//! `scaffold_pack`/`assert_idempotent`/`assert_gate_refuses` four-part
//! pattern"). Steps 1/2/4 of that checklist (real schema-to-ontology
//! generator, real-cluster verification, directory rename) are explicitly
//! out of scope here — this test only proves the sketch's existing shape
//! (`PodSpec` -> `Container` -> `ResourceRequirements`) round-trips through a
//! real sync and that its gate is real admission control, not decoration.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;
use std::path::{Path, PathBuf};
use support::{assert_gate_refuses, assert_idempotent, read, scaffold_pack_with_ontology};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// A real, conforming 2-container Pod fact set exercising all 3 nesting
/// levels (`PodSpec` -> Container -> `ResourceRequirements`), including the
/// OPTIONAL `hasResourceRequirements` branch on one container but not the
/// other (per `ontology.ttl`'s own comment: an unset resources block is
/// valid real k8s semantics, not itself gated).
const CONFORMING_ONTOLOGY: &str = "\
@prefix k8s: <http://seanchatmangpt.github.io/packs/k8s#> .

k8s:pod-web a k8s:PodSpec ;
    k8s:podName \"web\" ;
    k8s:hasContainer k8s:pod-web-c0, k8s:pod-web-c1 .

k8s:pod-web-c0 a k8s:Container ;
    k8s:containerIndex 0 ;
    k8s:containerName \"app\" ;
    k8s:image \"web-app:1.2.3\" ;
    k8s:hasResourceRequirements k8s:pod-web-c0-rr .

k8s:pod-web-c0-rr a k8s:ResourceRequirements ;
    k8s:cpuLimit \"500m\" ;
    k8s:memoryLimit \"256Mi\" .

k8s:pod-web-c1 a k8s:Container ;
    k8s:containerIndex 1 ;
    k8s:containerName \"sidecar\" ;
    k8s:image \"log-shipper:0.9\" .
";

#[test]
fn k8s_pack_generates_and_is_idempotent() {
    // 1. SCAFFOLD: copy the real pack + a minimal consumer, pre-populated
    //    with real 3-level-nested Pod/Container/ResourceRequirements facts
    //    (the pack itself ships zero individuals, by design — see
    //    ontology.ttl's "zero-individuals, vocabulary-only shipping" note).
    let (_dir, project) =
        scaffold_pack_with_ontology(&packs_dir().join("k8s-pack-DESIGN"), CONFORMING_ONTOLOGY);

    // 2. GENERATE + ASSERT REAL CONTENT.
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync");
    let doc = read(&project, "docs/k8s-design-sketch/pod-resource-summary.md");

    // Both containers of the one real PodSpec render, ordered by containerIndex.
    let c0_line = doc
        .lines()
        .find(|l| l.contains("container[0]"))
        .unwrap_or_else(|| panic!("container[0] row not found:\n{doc}"));
    assert!(
        c0_line.contains("**web**")
            && c0_line.contains("`app`")
            && c0_line.contains("`web-app:1.2.3`"),
        "container[0] row must echo pod/container/image literals verbatim: {c0_line}"
    );
    assert!(
        c0_line.contains("cpu limit `500m`") && c0_line.contains("memory limit `256Mi`"),
        "container[0] has ResourceRequirements — must render the 3rd nesting level's limits: {c0_line}"
    );

    let c1_line = doc
        .lines()
        .find(|l| l.contains("container[1]"))
        .unwrap_or_else(|| panic!("container[1] row not found:\n{doc}"));
    assert!(
        c1_line.contains("**web**")
            && c1_line.contains("`sidecar`")
            && c1_line.contains("`log-shipper:0.9`"),
        "container[1] row must echo pod/container/image literals verbatim: {c1_line}"
    );
    assert!(
        !c1_line.contains("cpu limit"),
        "container[1] has no ResourceRequirements (optional in real k8s) — must NOT render a \
         limits clause, proving the template's OPTIONAL join is real, not always-on: {c1_line}"
    );

    // Exactly 2 rows rendered (one per container of the single conforming
    // PodSpec) — proves the SPARQL rows query's joins are real and bounded,
    // not a fan-out over every individual in the graph.
    let row_count = doc.lines().filter(|l| l.starts_with("- **")).count();
    assert_eq!(row_count, 2, "expected exactly 2 container rows:\n{doc}");

    // 3. IDEMPOTENCY.
    assert_idempotent(&project);
}

#[test]
fn k8s_pack_gate_refuses_resource_requirements_missing_cpu_limit() {
    let (_dir, project) =
        scaffold_pack_with_ontology(&packs_dir().join("k8s-pack-DESIGN"), CONFORMING_ONTOLOGY);
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // GATE SABOTAGE: the exact sabotage fact NOTES.md/pack.toml already cite
    // as this pack's own verification precedent — a
    // k8s:ResourceRequirements individual missing the required
    // k8s:cpuLimit — must be refused, citing 010_admission by name.
    assert_gate_refuses(
        &project,
        "@prefix k8s: <http://seanchatmangpt.github.io/packs/k8s#> .\n\
         k8s:pod-sabotage a k8s:PodSpec ;\n\
         \x20\x20\x20\x20k8s:podName \"sabotage\" ;\n\
         \x20\x20\x20\x20k8s:hasContainer k8s:pod-sabotage-c0 .\n\
         k8s:pod-sabotage-c0 a k8s:Container ;\n\
         \x20\x20\x20\x20k8s:containerIndex 0 ;\n\
         \x20\x20\x20\x20k8s:containerName \"app\" ;\n\
         \x20\x20\x20\x20k8s:image \"broken:1.0\" ;\n\
         \x20\x20\x20\x20k8s:hasResourceRequirements k8s:pod-sabotage-c0-rr .\n\
         k8s:pod-sabotage-c0-rr a k8s:ResourceRequirements ;\n\
         \x20\x20\x20\x20k8s:memoryLimit \"128Mi\" .\n",
        "010_admission",
    );
}

#[test]
fn k8s_pack_gate_refuses_podspec_missing_container() {
    let (_dir, project) =
        scaffold_pack_with_ontology(&packs_dir().join("k8s-pack-DESIGN"), CONFORMING_ONTOLOGY);
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("baseline sync");

    // Second, distinct branch of the same VALUES-table gate: a PodSpec with
    // podName but zero k8s:hasContainer must also be refused, citing
    // 010_admission by name — proving the gate's VALUES-table consolidation
    // covers more than the one cpuLimit branch.
    assert_gate_refuses(
        &project,
        "@prefix k8s: <http://seanchatmangpt.github.io/packs/k8s#> .\n\
         k8s:pod-empty a k8s:PodSpec ;\n\
         \x20\x20\x20\x20k8s:podName \"empty\" .\n",
        "010_admission",
    );
}
