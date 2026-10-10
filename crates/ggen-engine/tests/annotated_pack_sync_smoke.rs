//! Chicago-TDD end-to-end smoke: real `ggen_engine::sync::sync` consuming
//! REAL `[capabilities]`-annotated packs from `/Users/sac/ggen-marketplace/packs`
//! via `[packs]` path entries.
//!
//! 1. Positive: three real annotated packs with empty `capabilities.requires`
//!    (aaif-vanilla-pack, aaif-profile-tailoring-pack, ai-chatbot-shadcn-pack)
//!    sync green and the project's generation rule renders real output.
//! 2. Negative: a 4th reference to a2a-hex-migration-pack — whose
//!    `capabilities.requires` names `urn:ggen:pack:ash-extension-pack`, which
//!    nothing in its declared dependency closure provides — refuses the sync
//!    with the typed `[FM-PACK-018]` error.
//!
//! Real filesystem (`tempfile::TempDir`), real pack resolution, real Tera
//! render, real write — no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const MARKETPLACE: &str = "/Users/sac/ggen-marketplace/packs";

/// Real annotated packs with `requires = []` — safe in any project.
const OK_PACKS: [&str; 3] = [
    "aaif-vanilla-pack",
    "aaif-profile-tailoring-pack",
    "ai-chatbot-shadcn-pack",
];

/// Real annotated pack whose `requires` cannot be satisfied in-project:
/// `requires = ["urn:ggen:pack:ash-extension-pack"]`, no `[dependencies]`.
const UNSATISFIED_PACK: &str = "a2a-hex-migration-pack";

fn write(root: &Path, rel: &str, content: &str) {
    let path = root.join(rel);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdir parent");
    }
    std::fs::write(path, content).expect("write file");
}

fn packs_table(extra: Option<&str>) -> String {
    let mut t = String::from("[packs]\n");
    for p in OK_PACKS {
        t.push_str(&format!("{p} = {{ path = \"{MARKETPLACE}/{p}\" }}\n"));
    }
    if let Some(p) = extra {
        t.push_str(&format!("{p} = {{ path = \"{MARKETPLACE}/{p}\" }}\n"));
    }
    t
}

/// Minimal project manifest: real annotated packs + a trivial generation
/// rule that renders a known marker, so "rendered output" is observable.
fn write_project(root: &Path, extra_pack: Option<&str>) {
    // Pure frontmatter schema (matching real consumers like
    // clap-noun-verb-zeroconfig-pack): [project] without version,
    // [ontology], [packs] path refs, [templates] dir. Mixing declarative
    // markers (project.version + [[generation.rules]]) with [packs] makes
    // the manifest schema-ambiguous (FM-CONFIG-101).
    write(
        root,
        "ggen.toml",
        &format!(
            "[project]\nname = \"annotated-pack-sync-smoke\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             {}\n\
             [templates]\ndir = \"templates\"\n",
            packs_table(extra_pack)
        ),
    );
    write(
        root,
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:one a ex:Thing .\n",
    );
    write(
        root,
        "templates/smoke.tmpl",
        "---\nto: out/smoke.txt\nsparql:\n  s: SELECT ?s WHERE { ?s a <http://example.org/Thing> } ORDER BY ?s\n---\n{% for row in s %}PACK_SYNC_SMOKE: {{ row.s }}\n{% endfor %}",
    );
}

/// THE positive proof: three real `[capabilities]`-annotated packs resolve,
/// the capability-closure admission passes, and the sync renders real output.
#[test]
fn real_annotated_packs_sync_green_and_render() {
    for p in OK_PACKS {
        let toml = std::fs::read_to_string(format!("{MARKETPLACE}/{p}/pack.toml"))
            .expect("pack.toml readable");
        assert!(
            toml.contains("[capabilities]"),
            "{p} must be a real [capabilities]-annotated pack"
        );
    }

    let dir = TempDir::new().expect("tempdir");
    write_project(dir.path(), None);

    let report = sync(dir.path(), SyncOptions::default())
        .expect("sync over 3 real annotated packs must succeed");

    // Rendered output is real: the generation rule's SELECT matched the
    // ontology triple and Tera rendered it to disk.
    let out = std::fs::read_to_string(dir.path().join("out/smoke.txt")).expect("rendered output");
    assert!(
        out.contains("PACK_SYNC_SMOKE: http://example.org/one"),
        "expected rendered marker, got: {out}"
    );
    assert!(
        !report.written.is_empty(),
        "sync must write files, got: {:?}",
        report.written
    );
    let lock = std::fs::read_to_string(dir.path().join("ggen.lock")).expect("ggen.lock written");
    for p in OK_PACKS {
        assert!(
            lock.contains(&format!("[packs.{p}]")),
            "lock missing [packs.{p}]"
        );
    }
}

/// Tier-1 positive: a pack whose URN-form `requires` IS satisfied in-project
/// because the consumer DECLARES the provider. cargo-cicd-pack requires
/// `urn:ggen:pack:clap-noun-verb-pack`; clap-noun-verb-pack provides it (and
/// itself requires praxis-core-pack + star-toml-pack, which provide those).
const CONSUMER_PACK: &str = "cargo-cicd-pack";
const PROVIDER_PACK: &str = "clap-noun-verb-pack";
const PROVIDER_DEPS: [&str; 2] = ["praxis-core-pack", "star-toml-pack"];

fn packs_table_multi(packs: &[&str]) -> String {
    let mut t = String::from("[packs]\n");
    for p in packs {
        t.push_str(&format!("{p} = {{ path = \"{MARKETPLACE}/{p}\" }}\n"));
    }
    t
}

fn write_project_packs(root: &Path, packs: &[&str]) {
    write(
        root,
        "ggen.toml",
        &format!(
            "[project]\nname = \"annotated-pack-sync-smoke\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             {}\n\
             [templates]\ndir = \"templates\"\n",
            packs_table_multi(packs)
        ),
    );
    write(
        root,
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:one a ex:Thing .\n",
    );
    write(
        root,
        "templates/smoke.tmpl",
        "---\nto: out/smoke.txt\nsparql:\n  s: SELECT ?s WHERE { ?s a <http://example.org/Thing> } ORDER BY ?s\n---\n{% for row in s %}PACK_SYNC_SMOKE: {{ row.s }}\n{% endfor %}",
    );
}

/// THE satisfied-requires proof: the consumer declares the provider in
/// `[packs]`, so the URN-form requirement is satisfied in-project and the
/// sync goes green with real rendered output.
#[test]
fn declared_provider_satisfies_urn_requires_and_syncs_green() {
    for p in [CONSUMER_PACK, PROVIDER_PACK] {
        let toml = std::fs::read_to_string(format!("{MARKETPLACE}/{p}/pack.toml"))
            .expect("pack.toml readable");
        assert!(
            toml.contains("[capabilities]"),
            "{p} must be a real [capabilities]-annotated pack"
        );
    }
    let consumer = std::fs::read_to_string(format!("{MARKETPLACE}/{CONSUMER_PACK}/pack.toml"))
        .expect("consumer pack.toml readable");
    assert!(
        consumer.contains(&format!("urn:ggen:pack:{PROVIDER_PACK}")),
        "fixture precondition: {CONSUMER_PACK} requires urn:ggen:pack:{PROVIDER_PACK}"
    );

    let mut packs: Vec<&str> = vec![CONSUMER_PACK, PROVIDER_PACK];
    packs.extend(PROVIDER_DEPS);
    let dir = TempDir::new().expect("tempdir");
    write_project_packs(dir.path(), &packs);

    let report = sync(dir.path(), SyncOptions::default())
        .expect("sync with declared provider must succeed (FM-PACK-018 must not fire)");

    let out = std::fs::read_to_string(dir.path().join("out/smoke.txt")).expect("rendered output");
    assert!(
        out.contains("PACK_SYNC_SMOKE: http://example.org/one"),
        "expected rendered marker, got: {out}"
    );
    assert!(
        !report.written.is_empty(),
        "sync must write files, got: {:?}",
        report.written
    );
}

/// THE contrast proof: the same project MINUS the provider declaration —
/// cargo-cicd-pack's URN requirement is now unsatisfied in-project and the
/// sync refuses with the typed FM-PACK-018 naming the provider.
#[test]
fn same_project_without_provider_declaration_refuses_with_fm_pack_018() {
    let mut packs: Vec<&str> = vec![CONSUMER_PACK];
    packs.extend(PROVIDER_DEPS);
    let dir = TempDir::new().expect("tempdir");
    write_project_packs(dir.path(), &packs);

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("missing provider declaration must refuse the sync");
    let msg = err.to_string();
    assert!(
        msg.contains("[FM-PACK-018]"),
        "expected typed FM-PACK-018 refusal, got: {msg}"
    );
    assert!(
        msg.contains(&format!("urn:ggen:pack:{PROVIDER_PACK}")),
        "refusal must name the missing provider capability, got: {msg}"
    );
}

/// Tier-2 (FM-PACK-018 adjudication H2): non-URN `requires` are admitted
/// only from the pack's own declared dependency closure. No shipped pack
/// carries a non-URN `capabilities.requires` (and the one real
/// `[dependencies]` pair, sa2a-* → praxis-core-pack "0.1.0", pins a version
/// the resolved 0.3.0 refuses), so the pair is authored as REAL on-disk
/// packs in the test's `TempDir` — real pack.toml parsing, real dependency
/// closure, real FM-PACK-018 adjudication. No mocks.
const NON_URN_CAPABILITY: &str = "example:transitive-capability";
const CONSUMER_V2_PACK: &str = "tier2-consumer-pack";
const PROVIDER_V2_PACK: &str = "tier2-provider-pack";

/// Write two real sibling packs: the consumer carries a non-URN
/// `capabilities.requires` plus a `[dependencies]` entry on the provider;
/// the provider provides (or, with `with_capability = false`, withholds)
/// the required capability.
fn write_tier2_packs(root: &Path, with_capability: bool) {
    let provides = if with_capability {
        format!(
            "[capabilities]\nprovides = [\"urn:ggen:pack:{PROVIDER_V2_PACK}\", \
             \"{NON_URN_CAPABILITY}\"]\n"
        )
    } else {
        format!("[capabilities]\nprovides = [\"urn:ggen:pack:{PROVIDER_V2_PACK}\"]\n")
    };
    write(
        root,
        &format!("packs-a/{PROVIDER_V2_PACK}/pack.toml"),
        &format!(
            "[pack]\nname = \"{PROVIDER_V2_PACK}\"\nversion = \"0.1.0\"\n\
             description = \"tier-2 closure provider\"\n\n{provides}"
        ),
    );
    write(
        root,
        &format!("packs-a/{PROVIDER_V2_PACK}/ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:p a ex:Thing .\n",
    );
    write(
        root,
        &format!("packs-a/{PROVIDER_V2_PACK}/templates/p.tmpl"),
        "---\nto: out/tier2-provider.txt\n---\nTIER2_PROVIDER\n",
    );
    write(
        root,
        &format!("packs-a/{CONSUMER_V2_PACK}/pack.toml"),
        &format!(
            "[pack]\nname = \"{CONSUMER_V2_PACK}\"\nversion = \"0.1.0\"\n\
             description = \"tier-2 non-URN requires consumer\"\n\n\
             [dependencies]\n\"{PROVIDER_V2_PACK}\" = \"0.1.0\"\n\n\
             [capabilities]\nprovides = [\"urn:ggen:pack:{CONSUMER_V2_PACK}\"]\n\
             requires = [\"{NON_URN_CAPABILITY}\"]\n"
        ),
    );
    write(
        root,
        &format!("packs-a/{CONSUMER_V2_PACK}/ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:c a ex:Thing .\n",
    );
    write(
        root,
        &format!("packs-a/{CONSUMER_V2_PACK}/templates/c.tmpl"),
        "---\nto: out/tier2-consumer.txt\n---\nTIER2_CONSUMER\n",
    );
}

/// Project manifest whose `[packs]` table declares both Tier-2 packs by
/// path, plus the same trivial rendering rule the Tier-1 tests use.
fn write_project_tier2(root: &Path) {
    write(
        root,
        "ggen.toml",
        &format!(
            "[project]\nname = \"annotated-pack-sync-smoke\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n\
             {CONSUMER_V2_PACK} = {{ path = \"packs-a/{CONSUMER_V2_PACK}\" }}\n\
             {PROVIDER_V2_PACK} = {{ path = \"packs-a/{PROVIDER_V2_PACK}\" }}\n\n\
             [templates]\ndir = \"templates\"\n"
        ),
    );
    write(
        root,
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:one a ex:Thing .\n",
    );
    write(
        root,
        "templates/smoke.tmpl",
        "---\nto: out/smoke.txt\nsparql:\n  s: SELECT ?s WHERE { ?s a <http://example.org/Thing> } ORDER BY ?s\n---\n{% for row in s %}PACK_SYNC_SMOKE: {{ row.s }}\n{% endfor %}",
    );
}

/// THE Tier-2 positive proof: the consumer's non-URN requirement is
/// satisfied through its DECLARED dependency closure — the provider is
/// declared in `[packs]`, named in the consumer's `[dependencies]`, and
/// provides the capability — so the sync goes green with real output.
#[test]
fn non_urn_requires_satisfied_via_declared_dependency_closure() {
    let dir = TempDir::new().expect("tempdir");
    write_tier2_packs(dir.path(), true);
    write_project_tier2(dir.path());

    let report = sync(dir.path(), SyncOptions::default())
        .expect("non-URN requires satisfied via declared closure must sync green");

    let out = std::fs::read_to_string(dir.path().join("out/smoke.txt")).expect("rendered output");
    assert!(
        out.contains("PACK_SYNC_SMOKE: http://example.org/one"),
        "expected rendered marker, got: {out}"
    );
    assert!(
        !report.written.is_empty(),
        "sync must write files, got: {:?}",
        report.written
    );
}

/// THE Tier-2 fail-closed proof: the identical project, except the declared
/// closure provider WITHHOLDS the capability — the non-URN requirement now
/// has no provider in the consumer's declared dependency closure and the
/// sync refuses with the typed FM-PACK-018 naming the missing capability.
#[test]
fn non_urn_requires_without_closure_provider_refuse_with_fm_pack_018() {
    let dir = TempDir::new().expect("tempdir");
    write_tier2_packs(dir.path(), false);
    write_project_tier2(dir.path());

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("non-URN requires with no closure provider must refuse the sync");
    let msg = err.to_string();
    assert!(
        msg.contains("[FM-PACK-018]"),
        "expected typed FM-PACK-018 refusal, got: {msg}"
    );
    assert!(
        msg.contains(NON_URN_CAPABILITY),
        "refusal must name the missing closure capability, got: {msg}"
    );
}

/// Tier-2 TRANSITIVE (`ScopeDepth::Transitive`): a 3-pack chain A [dependencies]
/// B, B [dependencies] C, where C (not B) provides the non-URN capability A's
/// `requires` names. No shipped corpus pack forms such a chain, so the trio is
/// authored as REAL on-disk packs in the test's `TempDir` — real pack.toml
/// parsing, real 2-level transitive closure, real FM-PACK-018 adjudication.
const TRANSITIVE_CAPABILITY: &str = "example:transitive-deep-capability";
const A_PACK: &str = "tier2-transitive-consumer-pack";
const B_PACK: &str = "tier2-transitive-middle-pack";
const C_PACK: &str = "tier2-transitive-root-pack";

/// Write three real sibling packs: A requires the non-URN capability and
/// depends on B; B depends on C; C provides (`with_capability`) or withholds
/// the capability. A does NOT depend on C directly.
fn write_transitive_packs(root: &Path, with_capability: bool) {
    let c_provides = if with_capability {
        format!(
            "[capabilities]\nprovides = [\"urn:ggen:pack:{C_PACK}\", \
             \"{TRANSITIVE_CAPABILITY}\"]\n"
        )
    } else {
        format!("[capabilities]\nprovides = [\"urn:ggen:pack:{C_PACK}\"]\n")
    };
    write(
        root,
        &format!("packs-t/{C_PACK}/pack.toml"),
        &format!(
            "[pack]\nname = \"{C_PACK}\"\nversion = \"0.1.0\"\n\
             description = \"tier-2 transitive closure root\"\n\n{c_provides}"
        ),
    );
    write(
        root,
        &format!("packs-t/{C_PACK}/ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:c a ex:Thing .\n",
    );
    write(
        root,
        &format!("packs-t/{C_PACK}/templates/c.tmpl"),
        "---\nto: out/t2-root.txt\n---\nT2_ROOT\n",
    );
    write(
        root,
        &format!("packs-t/{B_PACK}/pack.toml"),
        &format!(
            "[pack]\nname = \"{B_PACK}\"\nversion = \"0.1.0\"\n\
             description = \"tier-2 transitive closure middle\"\n\n\
             [dependencies]\n\"{C_PACK}\" = \"0.1.0\"\n\n\
             [capabilities]\nprovides = [\"urn:ggen:pack:{B_PACK}\"]\n"
        ),
    );
    write(
        root,
        &format!("packs-t/{B_PACK}/ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:b a ex:Thing .\n",
    );
    write(
        root,
        &format!("packs-t/{B_PACK}/templates/b.tmpl"),
        "---\nto: out/t2-middle.txt\n---\nT2_MIDDLE\n",
    );
    write(
        root,
        &format!("packs-t/{A_PACK}/pack.toml"),
        &format!(
            "[pack]\nname = \"{A_PACK}\"\nversion = \"0.1.0\"\n\
             description = \"tier-2 transitive closure consumer\"\n\n\
             [dependencies]\n\"{B_PACK}\" = \"0.1.0\"\n\n\
             [capabilities]\nprovides = [\"urn:ggen:pack:{A_PACK}\"]\n\
             requires = [\"{TRANSITIVE_CAPABILITY}\"]\n"
        ),
    );
    write(
        root,
        &format!("packs-t/{A_PACK}/ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:a a ex:Thing .\n",
    );
    write(
        root,
        &format!("packs-t/{A_PACK}/templates/a.tmpl"),
        "---\nto: out/t2-consumer.txt\n---\nT2_CONSUMER\n",
    );
}

/// Project manifest declaring A and B by path; C is declared only when
/// `with_root` — the broken-chain negative omits it.
fn write_project_transitive(root: &Path, with_root: bool) {
    let root_line = if with_root {
        format!("{C_PACK} = {{ path = \"packs-t/{C_PACK}\" }}\n")
    } else {
        String::new()
    };
    write(
        root,
        "ggen.toml",
        &format!(
            "[project]\nname = \"annotated-pack-sync-smoke\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n\
             {A_PACK} = {{ path = \"packs-t/{A_PACK}\" }}\n\
             {B_PACK} = {{ path = \"packs-t/{B_PACK}\" }}\n\
             {root_line}\n\
             [templates]\ndir = \"templates\"\n"
        ),
    );
    write(
        root,
        "ontology.ttl",
        "@prefix ex: <http://example.org/> .\nex:one a ex:Thing .\n",
    );
    write(
        root,
        "templates/smoke.tmpl",
        "---\nto: out/smoke.txt\nsparql:\n  s: SELECT ?s WHERE { ?s a <http://example.org/Thing> } ORDER BY ?s\n---\n{% for row in s %}PACK_SYNC_SMOKE: {{ row.s }}\n{% endfor %}",
    );
}

/// THE Tier-2 transitive positive proof: A's non-URN requirement is satisfied
/// two hops away through the declared dependency closure (A→B→C), which the
/// FM-PACK-018 adjudication walks at `ScopeDepth::Transitive` — the sync goes
/// green even though neither A's own deps nor the project's direct packs
/// other than C's declaration provide the capability.
#[test]
fn non_urn_requires_satisfied_via_two_level_transitive_closure() {
    let dir = TempDir::new().expect("tempdir");
    write_transitive_packs(dir.path(), true);
    write_project_transitive(dir.path(), true);

    let report = sync(dir.path(), SyncOptions::default())
        .expect("non-URN requires satisfied via 2-level transitive closure must sync green");

    let out = std::fs::read_to_string(dir.path().join("out/smoke.txt")).expect("rendered output");
    assert!(
        out.contains("PACK_SYNC_SMOKE: http://example.org/one"),
        "expected rendered marker, got: {out}"
    );
    assert!(
        !report.written.is_empty(),
        "sync must write files, got: {:?}",
        report.written
    );
}

/// THE Tier-2 transitive fail-closed proof: the identical 3-pack chain, all
/// three packs declared in [packs], but the root provider C WITHHOLDS the
/// capability — neither A's direct dep B nor anything else in the transitive
/// closure provides it, and the sync refuses with FM-PACK-018 naming the
/// capability. (Omitting C from [packs] entirely instead fires FM-PACK-014
/// at resolution, before capability adjudication.)
#[test]
fn broken_transitive_chain_refuses_with_fm_pack_018() {
    let dir = TempDir::new().expect("tempdir");
    write_transitive_packs(dir.path(), false);
    write_project_transitive(dir.path(), true);

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("transitive chain broken at the middle must refuse the sync");
    let msg = err.to_string();
    assert!(
        msg.contains("[FM-PACK-018]"),
        "expected typed FM-PACK-018 refusal, got: {msg}"
    );
    assert!(
        msg.contains(TRANSITIVE_CAPABILITY),
        "refusal must name the missing transitive capability, got: {msg}"
    );
}

/// THE negative proof: a 4th annotated pack whose `capabilities.requires`
/// names a capability no pack in its declared dependency closure provides
/// refuses the sync with the typed FM-PACK-018 — fail-closed, never a
/// warning.
#[test]
fn unsatisfied_capability_requires_refuse_with_fm_pack_018() {
    let toml = std::fs::read_to_string(format!("{MARKETPLACE}/{UNSATISFIED_PACK}/pack.toml"))
        .expect("pack.toml readable");
    assert!(
        toml.contains("urn:ggen:pack:ash-extension-pack"),
        "fixture precondition: {UNSATISFIED_PACK} requires ash-extension-pack"
    );

    let dir = TempDir::new().expect("tempdir");
    write_project(dir.path(), Some(UNSATISFIED_PACK));

    let err = sync(dir.path(), SyncOptions::default())
        .expect_err("unsatisfied capabilities.requires must refuse the sync");
    let msg = err.to_string();
    assert!(
        msg.contains("[FM-PACK-018]"),
        "expected typed FM-PACK-018 refusal, got: {msg}"
    );
    assert!(
        msg.contains("urn:ggen:pack:ash-extension-pack"),
        "refusal must name the missing capability, got: {msg}"
    );
}
