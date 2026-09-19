//! Chicago-TDD witnesses for the remaining DfCM pack-scope requirements.
//!
//! Real files, real pack.toml parsing, real portable digests, real canonical
//! graph loading. No mocks.
//!
//! The subject is candidate selection only. These tests deliberately prove
//! that scoping emits no admission, artifact, receipt, or DO authority.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{
    collections::BTreeSet,
    path::{Path, PathBuf},
};

use ggen_engine::{
    config::GgenConfig,
    pack::{resolve, Pack, ScopeDepth},
    pack_scope::{
        admission_success_rate, artifact_success_rate, benchmark_dfcm_scopes,
        mean_reciprocal_rank, receipt_success_rate, reverse_dependency_closure, topology_turtle,
        unknown_rate, CandidateAuthority, DependencyScopeResolver, ScopeDisposition,
        ScopeRequirement,
    },
    project_graph::load_for_query,
};
use oxigraph::sparql::QueryResults;
use tempfile::TempDir;

struct Fixture {
    _dir: TempDir,
    project: PathBuf,
}

fn safe_name(name: &str) -> String {
    name.replace('-', "_")
}

fn write_pack(
    root: &Path,
    name: &str,
    version: &str,
    dependencies: &[(&str, &str)],
    types: &[&str],
    provides: &[&str],
    requires: &[&str],
) {
    let pack = root.join("packs").join(name);
    std::fs::create_dir_all(pack.join("templates")).expect("pack templates");

    let mut manifest = format!(
        "[pack]\nname = \"{name}\"\nversion = \"{version}\"\n\
         description = \"DfCM fixture\"\n"
    );
    if !dependencies.is_empty() {
        manifest.push_str("\n[dependencies]\n");
        for (dependency, requirement) in dependencies {
            manifest.push_str(&format!("{dependency} = \"{requirement}\"\n"));
        }
    }
    if !types.is_empty() || !provides.is_empty() || !requires.is_empty() {
        manifest.push_str("\n[capabilities]\n");
        manifest.push_str(&format!("types = {}\n", toml_array(types)));
        manifest.push_str(&format!("provides = {}\n", toml_array(provides)));
        manifest.push_str(&format!("requires = {}\n", toml_array(requires)));
    }
    std::fs::write(pack.join("pack.toml"), manifest).expect("pack.toml");

    let local = safe_name(name);
    std::fs::write(
        pack.join("ontology.ttl"),
        format!(
            "@prefix ex: <http://example.com/dfcm#> .\n\
             ex:{local} a ex:FixturePack .\n"
        ),
    )
    .expect("ontology");

    std::fs::write(
        pack.join("templates").join(format!("{local}.md.tmpl")),
        format!(
            "---\nto: docs/{local}.md\n---\n\
# generated from {name}\n"
        ),
    )
    .expect("template");
}

fn toml_array(values: &[&str]) -> String {
    let body = values
        .iter()
        .map(|value| format!("\"{value}\""))
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{body}]")
}

fn write_project(root: &Path, pack_names: &[&str]) -> PathBuf {
    let project = root.join("project");
    std::fs::create_dir_all(project.join("templates")).expect("project templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("project ontology");

    let mut manifest = String::from(
        "[project]\nname = \"dfcm-fixture\"\n\n\
         [ontology]\nsource = \"ontology.ttl\"\n\n\
         [templates]\ndir = \"templates\"\n",
    );
    for name in pack_names {
        manifest.push_str(&format!(
            "\n[packs.{name}]\npath = \"../packs/{name}\"\n"
        ));
    }
    std::fs::write(project.join("ggen.toml"), manifest).expect("ggen.toml");
    project
}

fn standard_fixture() -> Fixture {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "a-root",
        "1.0.0",
        &[("b-direct", "1.0.0")],
        &["application"],
        &["cap.root"],
        &["cap.direct"],
    );
    write_pack(
        dir.path(),
        "b-direct",
        "1.0.0",
        &[("c-twohop", "1.0.0")],
        &["runtime"],
        &["cap.direct"],
        &[],
    );
    write_pack(
        dir.path(),
        "c-twohop",
        "1.0.0",
        &[],
        &["projection"],
        &["cap.twohop"],
        &[],
    );
    write_pack(
        dir.path(),
        "z-global",
        "1.0.0",
        &[],
        &["fallback"],
        &["cap.global"],
        &[],
    );

    let project = write_project(
        dir.path(),
        &["a-root", "b-direct", "c-twohop", "z-global"],
    );
    Fixture { _dir: dir, project }
}

fn resolved(fx: &Fixture) -> Vec<Pack> {
    let config = GgenConfig::load(&fx.project.join("ggen.toml")).expect("load config");
    resolve(&config, &fx.project).expect("resolve packs")
}

#[test]
fn resolver_escalates_local_direct_twohop_global_then_unknown_without_authority() {
    let fx = standard_fixture();
    let packs = resolved(&fx);
    let resolver = DependencyScopeResolver::new(&packs, "fixture-registry@v1");

    let direct = resolver
        .resolve("a-root", &ScopeRequirement::capability("cap.direct"))
        .expect("direct capability");
    assert_eq!(direct.disposition, ScopeDisposition::Candidates);
    assert_eq!(direct.depth, Some(ScopeDepth::Direct));
    assert_eq!(direct.candidate_names, vec!["b-direct"]);
    assert_eq!(direct.authority, CandidateAuthority::SelectOnly);
    assert_eq!(
        direct.searched_depths,
        vec![ScopeDepth::Local, ScopeDepth::Direct]
    );

    let twohop = resolver
        .resolve(
            "a-root",
            &ScopeRequirement::typed_capability("projection", "cap.twohop"),
        )
        .expect("two-hop typed capability");
    assert_eq!(twohop.depth, Some(ScopeDepth::TwoLevel));
    assert_eq!(twohop.candidate_names, vec!["c-twohop"]);

    let global = resolver
        .resolve("a-root", &ScopeRequirement::capability("cap.global"))
        .expect("global fallback");
    assert_eq!(global.depth, Some(ScopeDepth::Global));
    assert_eq!(global.candidate_names, vec!["z-global"]);

    let unknown = resolver
        .resolve("a-root", &ScopeRequirement::capability("cap.missing"))
        .expect("bounded exhaustion is UNKNOWN");
    assert_eq!(unknown.disposition, ScopeDisposition::Unknown);
    assert_eq!(unknown.depth, None);
    assert!(unknown.candidate_names.is_empty());
    assert_eq!(
        unknown.searched_depths,
        vec![
            ScopeDepth::Local,
            ScopeDepth::Direct,
            ScopeDepth::TwoLevel,
            ScopeDepth::Global,
        ]
    );
    assert_eq!(unknown.authority, CandidateAuthority::SelectOnly);
}

#[test]
fn exact_fingerprint_cache_hits_and_invalidates_on_ontology_or_registry_change() {
    let fx = standard_fixture();
    let packs = resolved(&fx);
    let requirement = ScopeRequirement::capability("cap.direct");
    let mut resolver = DependencyScopeResolver::new(&packs, "fixture-registry@v1");

    let first = resolver
        .resolve("a-root", &requirement)
        .expect("first resolve");
    assert!(!first.cache_hit);

    let second = resolver
        .resolve("a-root", &requirement)
        .expect("cached resolve");
    assert!(second.cache_hit);
    assert_eq!(first.cache_fingerprint, second.cache_fingerprint);

    let direct = packs
        .iter()
        .find(|pack| pack.name == "b-direct")
        .expect("direct pack");
    std::fs::write(
        &direct.ontology_path,
        "@prefix ex: <http://example.com/dfcm#> .\nex:b_direct a ex:ChangedFixturePack .\n",
    )
    .expect("mutate ontology");

    let changed = resolver
        .resolve("a-root", &requirement)
        .expect("source mutation re-resolve");
    assert!(!changed.cache_hit);
    assert_ne!(first.cache_fingerprint, changed.cache_fingerprint);

    resolver.set_registry_revision("fixture-registry@v2");
    let registry_changed = resolver
        .resolve("a-root", &requirement)
        .expect("registry revision re-resolve");
    assert!(!registry_changed.cache_hit);
    assert_ne!(changed.cache_fingerprint, registry_changed.cache_fingerprint);
}

#[test]
fn capability_requirement_refuses_ambient_global_provider() {
    let dir = TempDir::new().expect("tempdir");
    write_pack(
        dir.path(),
        "a-root",
        "1.0.0",
        &[("b-direct", "1.0.0")],
        &["application"],
        &[],
        &["cap.only-global"],
    );
    write_pack(
        dir.path(),
        "b-direct",
        "1.0.0",
        &[],
        &["runtime"],
        &[],
        &[],
    );
    write_pack(
        dir.path(),
        "z-global",
        "1.0.0",
        &[],
        &["fallback"],
        &["cap.only-global"],
        &[],
    );
    let project = write_project(dir.path(), &["a-root", "b-direct", "z-global"]);
    let config = GgenConfig::load(&project.join("ggen.toml")).expect("config");

    let err = resolve(&config, &project).expect_err("ambient provider must not satisfy requires");
    assert!(err.to_string().contains("FM-PACK-018"), "{err}");
    assert!(err.to_string().contains("cap.only-global"), "{err}");
}

#[test]
fn reverse_dependency_closure_bounds_incremental_regeneration_without_pruning_authority() {
    let fx = standard_fixture();
    let packs = resolved(&fx);

    assert_eq!(
        reverse_dependency_closure(&packs, "c-twohop").expect("reverse closure"),
        vec!["c-twohop", "b-direct", "a-root"]
    );
    assert_eq!(
        reverse_dependency_closure(&packs, "z-global").expect("isolated closure"),
        vec!["z-global"]
    );

    let err = reverse_dependency_closure(&packs, "missing")
        .expect_err("unknown changed pack must refuse");
    assert!(err.to_string().contains("FM-PACK-020"), "{err}");
}

#[test]
fn topology_is_public_predicate_rdf_and_is_visible_through_project_graph() {
    let fx = standard_fixture();
    let packs = resolved(&fx);
    let ttl = topology_turtle(&packs);

    assert!(ttl.contains("https://schema.org/potentialAction"), "{ttl}");
    assert!(ttl.contains("http://purl.org/dc/terms/requires"), "{ttl}");
    assert!(ttl.contains("https://schema.org/additionalType"), "{ttl}");
    assert!(!ttl.contains("seanchatmangpt.github.io"), "{ttl}");

    let graph = load_for_query(&fx.project).expect("project graph");
    let results = graph
        .query(
            "SELECT ?pack ?cap WHERE { \
             ?pack <https://schema.org/potentialAction> ?cap . \
             } ORDER BY ?pack ?cap",
        )
        .expect("query topology");
    let QueryResults::Solutions(solutions) = results else {
        panic!("SELECT must return solutions");
    };
    let count = solutions.count();
    assert_eq!(count, 4, "one provided capability per fixture pack");
}

#[test]
fn benchmark_records_mrr_topk_latency_capacity_coupling_and_zero_llm_or_authority() {
    let fx = standard_fixture();
    let packs = resolved(&fx);
    let relevant = BTreeSet::from(["c-twohop".to_string()]);
    let records =
        benchmark_dfcm_scopes(&packs, "a-root", &relevant, 3).expect("scope benchmark");

    assert_eq!(records.len(), 4);
    assert_eq!(records[0].depth, ScopeDepth::Local);
    assert_eq!(records[0].candidate_count, 1);
    assert!(records[0].unknown);
    assert_eq!(records[1].depth, ScopeDepth::Direct);
    assert_eq!(records[1].candidate_count, 2);
    assert!(records[1].unknown);
    assert_eq!(records[2].depth, ScopeDepth::TwoLevel);
    assert_eq!(records[2].candidate_count, 3);
    assert!(!records[2].unknown);
    assert_eq!(records[2].reciprocal_rank, 1.0 / 3.0);
    assert!(records[2].top_k_hit);
    assert_eq!(records[3].depth, ScopeDepth::Global);
    assert_eq!(records[3].candidate_count, 4);

    for record in &records {
        assert_eq!(record.llm_tokens, 0);
        assert_eq!(record.admission_events, 0);
        assert_eq!(record.artifact_count, 0);
        assert_eq!(record.receipt_count, 0);
        assert_eq!(record.admission_success, None);
        assert_eq!(record.artifact_success, None);
        assert_eq!(record.receipt_success, None);
        assert_eq!(record.authority, CandidateAuthority::SelectOnly);
    }
    assert!(records[2].semantic_capacity >= records[1].semantic_capacity);
    assert!(records[1].semantic_capacity < records[3].semantic_capacity);
    assert!(
        records[2].constraint_coupling_width
            >= records[1].constraint_coupling_width
    );
    assert!(mean_reciprocal_rank(&records) > 0.0);
    assert_eq!(unknown_rate(&records), 0.5);
    assert_eq!(admission_success_rate(&records), None);
    assert_eq!(artifact_success_rate(&records), None);
    assert_eq!(receipt_success_rate(&records), None);
}
