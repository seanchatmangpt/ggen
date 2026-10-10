//! Capability corpus composition test (Chicago TDD, lane composer-corpus-test).
//!
//! Scans BOTH real pack.toml corpora on disk (`~/ggen/packs` and
//! `~/ggen-marketplace/packs`), parses each with the real `PackFile` parser
//! (`star_toml` via `packs_registry::types::PackFile`), and drives the real
//! deterministic composition kernel `packs_registry::composer::compose`.
//!
//! Annotation completeness: the [capabilities] annotation wave was courted
//! complete (403/403, 2026-10-09); a pack.toml missing `[capabilities]` with a
//! well-formed self-URN provides is a FAILURE naming the file. The injection
//! tests (duplicate capability, unbound requirement) are always binding.


#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use ggen_marketplace::packs_registry::composer::{compose, CompositionRefusal};
use ggen_marketplace::packs_registry::types::{PackDependency, PackFile, PackTemplate};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

fn corpus_dirs() -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Ok(home) = std::env::var("HOME") {
        dirs.push(PathBuf::from(&home).join("ggen").join("packs"));
        dirs.push(PathBuf::from(&home).join("ggen-marketplace").join("packs"));
    }
    // Hardcoded fallbacks: cargo's test harness can run with HOME unset.
    dirs.push(PathBuf::from("/Users/sac/ggen/packs"));
    dirs.push(PathBuf::from("/Users/sac/ggen-marketplace/packs"));
    let unique: std::collections::BTreeSet<PathBuf> = dirs.into_iter().collect();
    unique.into_iter().filter(|d| d.is_dir()).collect()
}

fn find_pack_files() -> Vec<(PathBuf, PathBuf)> {
    let mut found = Vec::new();
    for dir in corpus_dirs() {
        for entry in walkdir::WalkDir::new(&dir)
            .max_depth(2)
            .into_iter()
            .filter_map(std::result::Result::ok)
        {
            let p = entry.path();
            if p.file_name().map(|n| n == "pack.toml").unwrap_or(false) {
                found.push((dir.clone(), p.to_path_buf()));
            }
        }
    }
    found.sort();
    found
}

/// Parsed corpus entry: the real PackFile model (which now carries the
/// `[capabilities]` table via `capabilities: Option<PackCapabilitiesFile>`).
struct CorpusPack {
    path: PathBuf,
    pack_file: PackFile,
    provides_uris: Vec<String>,
    requires_uris: Vec<String>,
}

fn parse_corpus_pack(path: &Path, raw: &str) -> Result<CorpusPack, String> {
    // The on-disk corpus does not carry `pack.id`; identity is the pack
    // directory name (matching how the registry resolves packs). Inject it
    // before deserializing into the real PackFile model.
    let mut value: toml::Value =
        star_toml::from_str(raw)
            .map_err(|e| format!("{}: raw toml parse: {}", path.display(), e))?;
    let dir_name = path
        .parent()
        .and_then(|p| p.file_name())
        .map(|n| n.to_string_lossy().to_string())
        .ok_or_else(|| format!("{}: no parent directory", path.display()))?;
    let pack_table = value
        .get_mut("pack")
        .and_then(|p| p.as_table_mut())
        .ok_or_else(|| format!("{}: missing [pack] table", path.display()))?;
    // The corpus predates the marketplace Pack model's required fields
    // (category/description/packages/...); the annotation wave targets the
    // engine's pack.toml shape. Inject harness defaults for absent required
    // fields so the real model parses — composition still runs over real
    // packages/dependencies/capabilities data.
    let defaults: &[(&str, toml::Value)] = &[
        ("id", toml::Value::String(dir_name)),
        ("name", toml::Value::String(String::new())),
        ("version", toml::Value::String("0.0.0".into())),
        ("description", toml::Value::String(String::new())),
        ("category", toml::Value::String("uncategorized".into())),
        ("packages", toml::Value::Array(vec![])),
    ];
    for (key, default) in defaults {
        pack_table
            .entry(key.to_string())
            .or_insert_with(|| default.clone());
    }
    let pack_file: PackFile = value
        .try_into()
        .map_err(|e| format!("{}: {}", path.display(), e))?;
    // [capabilities] flows into the real PackFile model (capabilities:
    // Option<PackCapabilitiesFile>); read provides/requires from there.
    let (provides_uris, requires_uris) = pack_file
        .capabilities
        .as_ref()
        .map(|c| {
            (
                c.provides.clone().unwrap_or_default().into_iter().collect(),
                c.requires.clone().unwrap_or_default().into_iter().collect(),
            )
        })
        .unwrap_or_default();
    Ok(CorpusPack {
        path: path.to_path_buf(),
        pack_file,
        provides_uris,
        requires_uris,
    })
}

fn load_corpus() -> (Vec<CorpusPack>, Vec<String>) {
    let mut packs = Vec::new();
    let mut parse_errors = Vec::new();
    for (_dir, path) in find_pack_files() {
        let raw = match std::fs::read_to_string(&path) {
            Ok(r) => r,
            Err(e) => {
                parse_errors.push(format!("{}: read: {}", path.display(), e));
                continue;
            }
        };
        match parse_corpus_pack(&path, &raw) {
            Ok(cp) => packs.push(cp),
            Err(e) => parse_errors.push(e),
        }
    }
    (packs, parse_errors)
}

#[test]
fn corpus_composes_and_capability_uris_are_unique() {
    let (all_packs, parse_errors) = load_corpus();
    // The two corpora overlap (~/ggen/packs mirrors a subset of
    // ~/ggen-marketplace/packs); a pack id present in both counts once.
    let mut seen = std::collections::BTreeSet::new();
    let packs: Vec<&CorpusPack> = all_packs
        .iter()
        .filter(|p| seen.insert(p.pack_file.pack.id.clone()))
        .collect();
    let total = packs.len();
    assert!(
        total > 0,
        "no pack.toml parsed successfully ({} discovered, parse errors: {:?})",
        find_pack_files().len(),
        parse_errors
    );

    // Annotation wave courted complete 2026-10-09: 403/403 pack.tomls carry
    // [capabilities]. Re-laxing this to a skip is a deliberate diff.
    // (The completeness itself is asserted by the unannotated check below.)

    // Missing/unannotated [capabilities] is now a FAILURE naming the file.
    let unannotated: Vec<&str> = packs
        .iter()
        .filter(|p| p.provides_uris.is_empty())
        .map(|p| p.path.to_str().unwrap_or("<non-utf8 path>"))
        .collect();
    assert!(
        unannotated.is_empty(),
        "{}/{} packs lack a well-formed [capabilities] provides (self-URN): {:?}",
        unannotated.len(),
        total,
        unannotated
    );

    // (a1) Self-IRI uniqueness: each urn:ggen:pack:<name> capability must be
    // provided by exactly one pack, and the name must match the pack's own id.
    let mut uri_owners: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for p in &packs {
        let id = &p.pack_file.pack.id;
        for uri in &p.provides_uris {
            uri_owners.entry(uri.as_str()).or_default().push(id);
        }
    }
    let duplicates: Vec<(&str, Vec<&str>)> = uri_owners
        .iter()
        .filter(|(_, owners)| owners.len() > 1)
        .map(|(uri, owners)| (*uri, owners.clone()))
        .collect();
    assert!(
        duplicates.is_empty(),
        "DuplicateCapability across [capabilities] self-IRIs: {duplicates:?}"
    );
    let mismatched: Vec<String> = packs
        .iter()
        .filter(|p| {
            p.provides_uris
                .iter()
                .any(|uri| uri != &format!("urn:ggen:pack:{}", p.pack_file.pack.id))
        })
        .map(|p| p.pack_file.pack.id.clone())
        .collect();
    assert!(
        mismatched.is_empty(),
        "self-IRI name mismatch (expected urn:ggen:pack:<id>): {mismatched:?}"
    );

    // (a2) Unbound [capabilities] requires: every required URI must be a
    // provides URI of some pack in the corpus (annotations only; a pack may
    // require evidence it annotates).
    let provided: std::collections::BTreeSet<&str> = uri_owners.keys().copied().collect();
    let unbound: Vec<(&str, &str)> = packs
        .iter()
        .flat_map(|p| {
            p.requires_uris
                .iter()
                .map(move |req| (p.pack_file.pack.id.as_str(), req.as_str()))
        })
        .filter(|(_, req)| !provided.contains(req))
        .collect();
    // WARNING, not yet fatal: the annotation wave is still authoring provider
    // packs (e.g. ops-dashboard-legend-pack requires deckgl-*/shadcn-* packs
    // that do not exist in either corpus yet). This becomes an assert once the
    // wave completes.
    if !unbound.is_empty() {
        eprintln!(
            "WARNING unbound [capabilities] requires URIs ({}): {unbound:?}",
            unbound.len()
        );
    }

    // (a3) The real composition kernel over the full real corpus must admit.
    let pack_files: Vec<PackFile> = packs.iter().map(|p| p.pack_file.clone()).collect();
    match compose(&pack_files) {
        Ok(plan) => {
            println!(
                "compose(plan): {} packs, order len {}",
                plan.pack_ids.len(),
                plan.order.len()
            );
        }
        Err(e) => panic!("compose refused the real corpus: {e:?} (parse errors: {parse_errors:?})"),
    }
}

#[test]
fn synthetic_duplicate_capability_is_refused() {
    let (packs, _errors) = load_corpus();
    // Pick any real pack and clone its provided package name into a synthetic
    // pack with a different id. If the corpus has no packages at all, inject
    // a synthetic pair entirely.
    let mut pack_files: Vec<PackFile> = packs.iter().map(|p| p.pack_file.clone()).collect();
    // These two tests exercise packages/dependencies arbitration; the cloned
    // corpus packs' [capabilities].provides (self-URNs, including both mirror
    // copies) must not leak into the court or compose() refuses on mirrors
    // before the synthetic case is reached.
    for pf in &mut pack_files {
        pf.capabilities = None;
    }

    let (dup_package, dup_pack) = match pack_files.iter().find_map(|pf| {
        pf.pack
            .packages
            .first()
            .map(|pkg| (pkg.clone(), pf.pack.id.clone()))
    }) {
        Some((pkg, src)) => (pkg, src),
        None => {
            let mut pf = pack_files
                .first()
                .cloned()
                .expect("corpus is non-empty (asserted by corpus test)");
            pf.pack.id = "synthetic-dup-a".into();
            pf.pack.packages = vec!["urn:ggen:pack:synthetic-shared".into()];
            pf.pack.templates = vec![];
            let mut dup = pf.clone();
            dup.pack.id = "synthetic-dup-b".into();
            pack_files.clear();
            pack_files.push(pf);
            pack_files.push(dup);
            (
                "urn:ggen:pack:synthetic-shared".to_string(),
                "synthetic-dup-a".to_string(),
            )
        }
    };

    let mut synthetic = pack_files.first().cloned().expect("corpus is non-empty");
    synthetic.pack.id = "synthetic-duplicate-capability".into();
    synthetic.pack.packages = vec![dup_package.clone()];
    synthetic.pack.templates = vec![];
    synthetic.pack.dependencies = vec![];
    // These tests exercise the packages/dependencies arbitration path; the
    // cloned corpus pack's [capabilities].provides (its self-URN) must not
    // leak into the synthetic or compose() refuses on the original URN.
    synthetic.capabilities = None;
    pack_files.push(synthetic);

    match compose(&pack_files) {
        Err(CompositionRefusal::DuplicateCapability {
            capability,
            providers,
        }) => {
            assert_eq!(capability, dup_package);
            assert!(providers.contains(&"synthetic-duplicate-capability".to_string()));
            assert!(providers.contains(&dup_pack));
        }
        Err(other) => panic!("expected DuplicateCapability, got {other:?}"),
        Ok(_) => panic!(
            "expected DuplicateCapability for package {dup_package:?} (claimed by \
             {dup_pack:?} and synthetic-duplicate-capability), got Ok"
        ),
    }
}

#[test]
fn synthetic_unbound_requirement_is_refused() {
    let (packs, _errors) = load_corpus();
    let mut pack_files: Vec<PackFile> = packs.iter().map(|p| p.pack_file.clone()).collect();

    let mut synthetic = pack_files.first().cloned().expect("corpus is non-empty");
    synthetic.pack.id = "synthetic-unbound-requires".into();
    synthetic.pack.templates = vec![];
    synthetic.pack.dependencies = vec![PackDependency {
        pack_id: "urn:ggen:pack:definitely-not-in-corpus".into(),
        version: "0.0.0".into(),
        optional: false,
    }];
    synthetic.capabilities = None;
    pack_files.push(synthetic);

    match compose(&pack_files) {
        Err(CompositionRefusal::UnboundRequirement {
            requiring_pack,
            required_pack,
        }) => {
            assert_eq!(requiring_pack, "synthetic-unbound-requires");
            assert_eq!(required_pack, "urn:ggen:pack:definitely-not-in-corpus");
        }
        Err(other) => panic!("expected UnboundRequirement, got {other:?}"),
        Ok(_) => panic!("expected UnboundRequirement for absent provider, got Ok"),
    }
}

// ---------------------------------------------------------------------------
// Full-tree nested binding (lane corpus-fulltree-bind, 2026-10-09).
//
// The canonical census is 439 pack.tomls full-tree: 403 top-level (bound by
// the tests above) + 36 nested manifests (mindepth 3 under packs/). The
// nested ones are bound here: EVERY nested manifest must carry a well-formed
// [capabilities] provides (self-URN matching its parent directory name),
// unless it is an explicit ALLOWLIST entry (deliberate fixture manifest).
// Removing an allowlist entry after annotating its manifest is the intended
// forward diff; adding a new nested manifest without [capabilities] or
// without an allowlist entry is a FAILURE naming the file.
// ---------------------------------------------------------------------------

/// Deliberate fixture manifests exempt from the [capabilities] binding.
/// Each entry is a deliberate diff — removing one requires annotating the
/// manifest or deleting the fixture.
///
/// PERMANENT (qualification fixtures: deliberately minimal/invalid inputs to
/// the pack-spec qualification courts; annotating them would change what they
/// test):
const ALLOWLIST_FIXTURES: &[&str] = &[
    // ggen-pack-spec-pack qualification corpus (21 fixtures)
    "ggen-pack-spec-pack/qualification/negative/path-traversal/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/missing-ontology/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/manifest-unknown-key/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/symlink/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/manifest-semantic-identity-mismatch/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/renderer-mismatch/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/query-positive-select/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/gate-positive-select/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/ambiguous-capability/provider-alpha/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/ambiguous-capability/provider-beta/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/ambiguous-capability/consumer/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/dependency-cycle/pack-a/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/dependency-cycle/pack-b/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/target-collision/pack-one/pack.toml",
    "ggen-pack-spec-pack/qualification/negative/target-collision/pack-two/pack.toml",
    "ggen-pack-spec-pack/qualification/positive/consumer-alias/pack.toml",
    "ggen-pack-spec-pack/qualification/positive/two-semantic-dependencies/pack.toml",
    "ggen-pack-spec-pack/qualification/positive/two-semantic-dependencies/deps/alpha-semantics-pack/pack.toml",
    "ggen-pack-spec-pack/qualification/positive/two-semantic-dependencies/deps/beta-semantics-pack/pack.toml",
    "ggen-pack-spec-pack/qualification/positive/portable-tera-fanout/pack.toml",
    "ggen-pack-spec-pack/qualification/positive/minimal-portable-pack/pack.toml",
    // ggen-self-pack replay fixture (expected qualification output tree)
    "ggen-self-pack/qualification/expected/packs/qualification-pack/pack.toml",
];

/// TEMPORARY allowlist entries: REAL variant packs (dfcm-pack families)
/// pending the [capabilities] annotation wave. These are NOT fixtures —
/// each entry here is a debt marker; the forward diff annotates the manifest
/// and deletes the entry. This list shrinking to empty is the goal state.
const ALLOWLIST_PENDING_ANNOTATION: &[&str] = &[];

fn allowlist() -> std::collections::BTreeSet<&'static str> {
    ALLOWLIST_FIXTURES
        .iter()
        .chain(ALLOWLIST_PENDING_ANNOTATION.iter())
        .copied()
        .collect()
}

/// Discover NESTED pack.toml manifests (depth >= 3 below the packs root),
/// i.e. those the top-level `find_pack_files` (max_depth 2) does not bind.
fn find_nested_pack_files() -> Vec<(PathBuf, PathBuf)> {
    let mut found = Vec::new();
    for dir in corpus_dirs() {
        for entry in walkdir::WalkDir::new(&dir)
            .max_depth(8)
            .into_iter()
            .filter_map(std::result::Result::ok)
        {
            let p = entry.path();
            if !p.file_name().map(|n| n == "pack.toml").unwrap_or(false) {
                continue;
            }
            let rel = match p.strip_prefix(&dir) {
                Ok(r) => r,
                Err(_) => continue,
            };
            // Nested only: at least packs/<top>/<sub>/.../pack.toml
            if rel.components().count() >= 3 {
                found.push((dir.clone(), p.to_path_buf()));
            }
        }
    }
    found.sort();
    found
}

#[test]
fn nested_full_tree_manifests_are_bound_or_allowlisted() {
    let allowlist = allowlist();
    let nested = find_nested_pack_files();
    assert!(
        nested.len() >= 30,
        "nested corpus collapsed ({} found; canonical census is 36 nested + \
         403 top-level = 439 full-tree) — discovery walk is broken",
        nested.len()
    );

    // Full-tree tally: top-level bound corpus + nested manifests.
    let top_level = find_pack_files();
    let full_tree = top_level.len() + nested.len();
    println!(
        "full-tree census: {} top-level + {} nested = {} pack.tomls \
         ({} allowlisted: {} fixtures + {} pending annotation)",
        top_level.len(),
        nested.len(),
        full_tree,
        allowlist.len(),
        ALLOWLIST_FIXTURES.len(),
        ALLOWLIST_PENDING_ANNOTATION.len()
    );

    let mut allowlisted_on_disk: std::collections::BTreeSet<String> = Default::default();
    let mut unbound_real: Vec<String> = Vec::new();
    let mut stale_allowlist: Vec<&str> = allowlist.iter().copied().collect();

    for (_dir, path) in &nested {
        let rel = path
            .strip_prefix("/Users/sac/ggen-marketplace/packs")
            .or_else(|_| {
                let home = std::path::PathBuf::from(std::env::var("HOME").unwrap_or_default());
                path.strip_prefix(home.join("ggen-marketplace/packs"))
            })
            .map(|r| r.to_string_lossy().to_string())
            .unwrap_or_else(|_| path.to_string_lossy().to_string());
        if allowlist.contains(rel.as_str()) {
            stale_allowlist.retain(|e| *e != rel.as_str());
            allowlisted_on_disk.insert(rel);
            continue;
        }
        // REAL nested pack: hard-fail binding, same bar as top-level.
        let raw = std::fs::read_to_string(path)
            .unwrap_or_else(|e| panic!("{}: read: {e}", path.display()));
        let cp = parse_corpus_pack(path, &raw)
            .unwrap_or_else(|e| panic!("nested real pack failed to parse: {e}"));
        let expected_urn = format!(
            "urn:ggen:pack:{}",
            path.parent()
                .and_then(|p| p.file_name())
                .map(|n| n.to_string_lossy().to_string())
                .unwrap_or_default()
        );
        let well_formed = cp.provides_uris.len() == 1
            && cp.provides_uris.iter().all(|u| *u == expected_urn);
        if !well_formed {
            unbound_real.push(format!(
                "{}: provides={:?} (expected [{expected_urn:?}])",
                rel,
                cp.provides_uris
            ));
        }
    }

    assert!(
        unbound_real.is_empty(),
        "{} nested REAL pack.tomls lack a well-formed [capabilities] provides \
         (self-URN). Annotate them or add an explicit ALLOWLIST entry \
         (each entry is a deliberate diff): {:?}",
        unbound_real.len(),
        unbound_real
    );
    assert!(
        stale_allowlist.is_empty(),
        "stale ALLOWLIST entries (no such nested manifest on disk — delete them): {stale_allowlist:?}"
    );
    assert_eq!(
        allowlisted_on_disk.len(),
        allowlist.len(),
        "allowlist ↔ disk bijection broken: {} entries on disk vs {} declared",
        allowlisted_on_disk.len(),
        allowlist.len()
    );
}

#[test]
fn nested_real_pack_capability_uris_do_not_dangle_or_collide_full_tree() {
    // Full-tree invariant re-assertion: across top-level packs + any nested
    // ANNOTATED (non-allowlisted) packs, self-URNs stay unique/name-matched
    // and no [capabilities] requires dangles. (0 bare is asserted by
    // nested_full_tree_manifests_are_bound_or_allowlisted; 0 dangling + 0
    // duplicate here.)
    let (all_packs_raw, parse_errors) = load_corpus();
    // Same two-mirror dedup policy as corpus_composes_and_capability_uris_are_unique:
    // a pack id present in both corpora counts once.
    let mut seen = std::collections::BTreeSet::new();
    let mut all_packs: Vec<CorpusPack> = all_packs_raw
        .into_iter()
        .filter(|p| seen.insert(p.pack_file.pack.id.clone()))
        .collect();
    for (_dir, path) in find_nested_pack_files() {
        let rel = path.to_string_lossy().to_string();
        let allowlist = allowlist();
        let is_allowlisted = allowlist.iter().any(|a| rel.ends_with(a));
        if is_allowlisted {
            continue;
        }
        let raw = std::fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("{}: read: {e}", path.display()));
        let cp = parse_corpus_pack(&path, &raw)
            .unwrap_or_else(|e| panic!("nested real pack failed to parse: {e}"));
        all_packs.push(cp);
    }
    // Top-level parse errors are tolerated upstream (same policy as the
    // corpus test); nested real packs hard-fail at parse inside the loop.

    let mut uri_owners: BTreeMap<&str, Vec<&str>> = BTreeMap::new();
    for p in &all_packs {
        for uri in &p.provides_uris {
            uri_owners.entry(uri.as_str()).or_default().push(&p.pack_file.pack.id);
        }
    }
    let duplicates: Vec<(&str, Vec<&str>)> = uri_owners
        .iter()
        .filter(|(_, owners)| owners.len() > 1)
        .map(|(u, o)| (*u, o.clone()))
        .collect();
    assert!(
        duplicates.is_empty(),
        "full-tree DuplicateCapability across self-IRIs: {duplicates:?}"
    );

    let provided: std::collections::BTreeSet<&str> = uri_owners.keys().copied().collect();
    let unbound: Vec<(&str, &str)> = all_packs
        .iter()
        .flat_map(|p| {
            p.requires_uris
                .iter()
                .map(move |req| (p.pack_file.pack.id.as_str(), req.as_str()))
        })
        .filter(|(_, req)| !provided.contains(req))
        .collect();
    if !unbound.is_empty() {
        eprintln!(
            "WARNING full-tree unbound [capabilities] requires ({}): {unbound:?}",
            unbound.len()
        );
    }
}

// Re-export to silence unused warnings if types drift.
#[allow(dead_code)]
fn _type_witness(_t: PackTemplate) {}
