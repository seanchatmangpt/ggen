//! Chicago-TDD witnesses for FM-PACK-018 two-tier capability satisfaction
//! (`ggen_engine::pack::resolve` -> `validate_capability_requirements`).
//!
//! Real filesystem, real pack.toml parsing, real dependency admission — no
//! mocks. Every fixture is a `TempDir` project with real pack dirs.
//!
//! Tier semantics under test (adjudication H2, 2026-10-09):
//! - Tier 1: URN-form requires (`urn:ggen:pack:<name>`) are satisfied iff the
//!   consumer's declared `[packs]` universe contains `<name>`; otherwise
//!   resolve is REFUSED with FM-PACK-018 (fail-closed, not a soft report).
//! - Tier 2: non-URN requires are satisfied iff a provider exists inside the
//!   subject's transitive declared-dependency closure; ambient providers are
//!   never admitted (broken closure is REFUSED with FM-PACK-018).
//! - Capability ordering edges from URN requires never cause refusal: a cycle
//!   of mutual URN requires between declared packs composes (edges dropped
//!   deterministically by `admitted_capability_edges`).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::fmt::Write as _;
use std::path::{Path, PathBuf};

use ggen_engine::{config::GgenConfig, pack::resolve};
use tempfile::TempDir;

/// Write a real pack dir with `[capabilities]` and optional dependencies.
fn write_pack(
    root: &Path, name: &str, version: &str, dependencies: &[(&str, &str)], provides: &[&str],
    requires: &[&str],
) {
    let pack = root.join("packs").join(name);
    std::fs::create_dir_all(pack.join("templates")).expect("pack templates dir");

    let mut manifest = format!(
        "[pack]\nname = \"{name}\"\nversion = \"{version}\"\n\
         description = \"two-tier satisfaction fixture\"\n"
    );
    if !dependencies.is_empty() {
        manifest.push_str("\n[dependencies]\n");
        for (dependency, requirement) in dependencies {
            let _ = writeln!(manifest, "{dependency} = \"{requirement}\"");
        }
    }
    let p: Vec<String> = provides.iter().map(|c| format!("\"{c}\"")).collect();
    let r: Vec<String> = requires.iter().map(|c| format!("\"{c}\"")).collect();
    let _ = write!(
        manifest,
        "\n[capabilities]\nprovides = [{}]\nrequires = [{}]\n",
        p.join(", "),
        r.join(", ")
    );
    std::fs::write(pack.join("pack.toml"), manifest).expect("pack.toml written");

    let subject = name.replace('-', "_");
    std::fs::write(
        pack.join("ontology.ttl"),
        format!("@prefix ex: <http://example.com/two-tier#> .\nex:{subject} a ex:PackFixture .\n"),
    )
    .expect("ontology written");
    std::fs::write(
        pack.join("templates").join(format!("{subject}.rs.tmpl")),
        format!("---\nto: src/{subject}.rs\n---\n// generated from {name}\n"),
    )
    .expect("template written");
}

/// Write a real consumer project ggen.toml declaring `pack_names` by path.
fn write_project(root: &Path, pack_names: &[&str]) -> PathBuf {
    let project = root.join("project");
    std::fs::create_dir_all(project.join("templates")).expect("project templates dir");
    std::fs::write(project.join("ontology.ttl"), "").expect("project ontology written");

    let mut manifest = String::from(
        "[project]\nname = \"two-tier-satisfaction-fixture\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [templates]\ndir = \"templates\"\n",
    );
    for name in pack_names {
        let _ = write!(manifest, "\n[packs.{name}]\npath = \"../packs/{name}\"");
        let _ = writeln!(manifest);
    }
    std::fs::write(project.join("ggen.toml"), manifest).expect("ggen.toml written");
    project
}

fn resolved(config_root: &Path) -> Result<Vec<ggen_engine::pack::Pack>, String> {
    let config =
        GgenConfig::load(&config_root.join("ggen.toml")).map_err(|e| format!("load: {e}"))?;
    resolve(&config, config_root).map_err(|e| format!("{e}"))
}

/// (1) Tier 1 satisfied: consumer declares the URN-referenced provider pack
/// in [packs] -> resolve succeeds; the require never needs a dependency edge.
#[test]
fn tier1_urn_require_satisfied_by_consumer_declaration() {
    let dir = TempDir::new().expect("tempdir");
    // provider-b declares capability cap-one; requirer-a URN-requires it with
    // NO dependency edge between them.
    write_pack(
        dir.path(),
        "requirer-a",
        "1.0.0",
        &[],
        &[],
        &["urn:ggen:pack:provider-b"],
    );
    write_pack(dir.path(), "provider-b", "1.0.0", &[], &["cap-one"], &[]);
    let project = write_project(dir.path(), &["requirer-a", "provider-b"]);

    let packs = resolved(&project).expect("Tier 1 URN require satisfied by declaration");
    let mut names: Vec<&str> = packs.iter().map(|p| p.name.as_str()).collect();
    names.sort_unstable();
    assert_eq!(names, vec!["provider-b", "requirer-a"]);
}

/// (2) Tier 1 unsatisfied: URN require with no declaration anywhere ->
/// resolve REFUSES with FM-PACK-018 (fail-closed; satisfaction is not
/// reported as false, the transition is refused).
#[test]
fn tier1_urn_require_without_declaration_is_refused() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "requirer-a",
        "1.0.0",
        &[],
        &[],
        &["urn:ggen:pack:never-declared"],
    );
    let project = write_project(dir.path(), &["requirer-a"]);

    let err = resolved(&project).expect_err("undeclared URN require must refuse");
    assert!(
        err.contains("FM-PACK-018") || err.contains("does not declare the referenced pack"),
        "expected FM-PACK-018 consumer-declaration refusal, got: {err}"
    );
}

/// (3) Tier 2 satisfied transitively: requirer-a depends on b-mid which
/// depends on c-provider (provides cap-two); requirer-a requires cap-two.
/// The provider is inside the declared transitive closure -> resolve succeeds.
#[test]
fn tier2_non_urn_require_satisfied_through_transitive_closure() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "requirer-a",
        "1.0.0",
        &[("b-mid", "1.0.0")],
        &[],
        &["cap-two"],
    );
    write_pack(
        dir.path(),
        "b-mid",
        "1.0.0",
        &[("c-provider", "1.0.0")],
        &[],
        &[],
    );
    write_pack(dir.path(), "c-provider", "1.0.0", &[], &["cap-two"], &[]);
    let project = write_project(dir.path(), &["requirer-a", "b-mid", "c-provider"]);

    resolved(&project).expect("Tier 2 require satisfied via transitive closure");
}

/// (4) Tier 2 broken closure: provider exists in the declared universe but is
/// NOT in requirer-a's dependency closure -> resolve REFUSES with FM-PACK-018
/// (ambient providers are never admitted).
#[test]
fn tier2_broken_closure_is_refused_even_with_ambient_provider() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(dir.path(), "requirer-a", "1.0.0", &[], &[], &["cap-three"]);
    // Declared in [packs] but no dependency edge from requirer-a: ambient.
    write_pack(
        dir.path(),
        "ambient-provider",
        "1.0.0",
        &[],
        &["cap-three"],
        &[],
    );
    let project = write_project(dir.path(), &["requirer-a", "ambient-provider"]);

    let err = resolved(&project).expect_err("ambient provider must not satisfy Tier 2");
    assert!(
        err.contains("FM-PACK-018") || err.contains("no provider exists"),
        "expected FM-PACK-018 closure-scope refusal, got: {err}"
    );
}

/// (5) Mixed requires: one URN-form (Tier 1, declaration-satisfied) and one
/// non-URN (Tier 2, closure-satisfied) in the same pack -> both admitted,
/// each through its own tier path.
#[test]
fn mixed_requires_attribute_each_tier_independently() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "requirer-a",
        "1.0.0",
        &[("closure-provider", "1.0.0")],
        &[],
        &["urn:ggen:pack:declared-provider", "cap-four"],
    );
    write_pack(dir.path(), "declared-provider", "1.0.0", &[], &[], &[]);
    write_pack(
        dir.path(),
        "closure-provider",
        "1.0.0",
        &[],
        &["cap-four"],
        &[],
    );
    let project = write_project(
        dir.path(),
        &["requirer-a", "declared-provider", "closure-provider"],
    );

    resolved(&project).expect("both tiers satisfied through their own paths");

    // And the mixed attribution fails per-tier when only one tier breaks:
    // drop the Tier 1 declaration, keep the Tier 2 closure intact.
    let dir2 = TempDir::new().expect("tempdir");
    write_pack(
        dir2.path(),
        "requirer-a",
        "1.0.0",
        &[("closure-provider", "1.0.0")],
        &[],
        &["urn:ggen:pack:declared-provider", "cap-four"],
    );
    write_pack(
        dir2.path(),
        "closure-provider",
        "1.0.0",
        &[],
        &["cap-four"],
        &[],
    );
    let project2 = write_project(dir2.path(), &["requirer-a", "closure-provider"]);
    let err = resolved(&project2).expect_err("missing Tier 1 declaration must refuse");
    assert!(
        err.contains("urn:ggen:pack:declared-provider"),
        "refusal must name the unsatisfied URN require, got: {err}"
    );
}

/// (6) Ordering edges never refuse: mutual URN capability requires between
/// two declared packs (a capability cycle) compose. The dependency-only graph
/// stays acyclic; capability edges would form a cycle and are dropped
/// deterministically, never refused.
#[test]
fn capability_require_cycle_composes_without_refusal() {
    let dir = TempDir::new().expect("tempdir");
    // Mutual URN requires, no declared dependencies.
    write_pack(
        dir.path(),
        "self-monitoring",
        "1.0.0",
        &[],
        &[],
        &["urn:ggen:pack:dogfood-lifecycle"],
    );
    write_pack(
        dir.path(),
        "dogfood-lifecycle",
        "1.0.0",
        &[],
        &[],
        &["urn:ggen:pack:self-monitoring"],
    );
    let project = write_project(dir.path(), &["self-monitoring", "dogfood-lifecycle"]);

    let packs = resolved(&project).expect("mutual URN requires compose (H2 consumer-advice)");
    let mut names: Vec<&str> = packs.iter().map(|p| p.name.as_str()).collect();
    names.sort_unstable();
    assert_eq!(names, vec!["dogfood-lifecycle", "self-monitoring"]);
}
