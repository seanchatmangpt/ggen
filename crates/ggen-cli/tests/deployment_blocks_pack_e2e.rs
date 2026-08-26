//! End-to-end checkpoint for `packs/fortune5-deployment-blocks-pack`.
//!
//! Unlike most `*_pack_e2e.rs` files in this workspace, this pack is NOT consumed
//! via a consumer project's `ggen.toml` `[[packs]]` + `templates/` wired into
//! `ggen sync run` -- it has no templates at all. It is consumed a different way:
//! `crates/ggen-cli/src/cmds/bblock.rs` embeds
//! `packs/fortune5-deployment-blocks-pack/catalog/fortune5-bblocks.json` at
//! compile time via `include_str!` and serves it through the real `ggen bblock
//! <verb>` CLI subcommand. `ontology.ttl` is a parallel RDF mirror of the same
//! facts, kept (per `pack.toml`/`README.md`) as the ontology-authoritative source
//! the retained JSON catalog is meant to be "independently verified against ...
//! in CI". So the real consumer scenario here is a project directory invoking
//! `ggen bblock <verb>` as a subprocess, not a `sync run` over templates -- these
//! tests follow that real shape instead of forcing the sync-pack pattern.
//!
//! Confirmed before writing this file: zero coverage anywhere in the workspace
//! actually invoked `ggen bblock <verb>` as a subprocess (only in-module
//! library-level `#[cfg(test)]` unit tests inside `bblock.rs` itself existed),
//! and nothing in the workspace ever executed `ontology.ttl` against its own
//! `gates/*.rq` files or cross-checked it against the retained JSON catalog --
//! the "independently verified ... in CI" claim in `pack.toml`/`README.md` was
//! unproven. This file closes both gaps.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use chicago_tdd_tools::cli_proof::{CliHarness, CliOutput};
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde_json::Value;
use tempfile::TempDir;

const GATE_NS: &str = "PREFIX bb: <http://seanchatmangpt.github.io/packs/fortune5-bblock#>\n";

fn pack_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs/fortune5-deployment-blocks-pack")
}

fn run_bblock(cwd: &Path, args: &[&str]) -> CliOutput {
    let mut full_args = vec!["bblock"];
    full_args.extend_from_slice(args);
    CliHarness::cargo_bin("ggen")
        .args(full_args)
        .current_dir(cwd)
        .run()
        .expect("spawn ggen bblock subprocess")
}

fn stdout_json(output: &CliOutput) -> Value {
    serde_json::from_str(&output.stdout).unwrap_or_else(|error| {
        panic!(
            "stdout was not valid JSON: {error}\nstdout:\n{}\nstderr:\n{}",
            output.stdout, output.stderr
        )
    })
}

fn read_json(path: &Path) -> Value {
    let bytes = std::fs::read_to_string(path)
        .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
    serde_json::from_str(&bytes).unwrap_or_else(|error| panic!("parse {}: {error}", path.display()))
}

/// Real consumer scenario #1: a project plans, then enables, a deployment-block
/// group. `plan` must only write control-surface artifacts; `enable` must
/// additionally materialize real `infrastructure/...` directories, extend
/// `.ggen/packs.lock` with real pack identities, and chain its receipts against
/// prior history rather than resetting to `GENESIS` on every run.
#[test]
fn plan_then_enable_materializes_real_directories_receipts_and_lockfile() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    let plan_out = run_bblock(
        root,
        &[
            "plan",
            "--group-id",
            "testing",
            "--provider",
            "aws",
            "--format",
            "json",
        ],
    );
    let _ = plan_out.assert_success();
    let plan_json = stdout_json(&plan_out);
    let plan_path = root.join(plan_json["plan_path"].as_str().expect("plan_path"));
    let plan_intent_path = root.join(
        plan_json["intent_receipt"]
            .as_str()
            .expect("intent_receipt"),
    );
    let plan_result_path = root.join(
        plan_json["result_receipt"]
            .as_str()
            .expect("result_receipt"),
    );
    assert!(plan_path.is_file(), "plan artifact missing on disk");
    assert!(
        plan_intent_path.is_file(),
        "plan intent receipt missing on disk"
    );
    assert!(
        plan_result_path.is_file(),
        "plan result receipt missing on disk"
    );
    assert!(
        !root.join("infrastructure/testing").exists(),
        "plan must be dry: it must not materialize infrastructure directories"
    );

    let enable_out = run_bblock(
        root,
        &[
            "enable",
            "--group-id",
            "testing",
            "--provider",
            "aws",
            "--format",
            "json",
        ],
    );
    let _ = enable_out.assert_success();
    let enable_json = stdout_json(&enable_out);
    assert_eq!(enable_json["status"], "enabled");

    let directories = enable_json["directories"]
        .as_array()
        .expect("directories array");
    assert!(
        !directories.is_empty(),
        "testing group must resolve at least one directory"
    );
    for directory in directories {
        let relative = directory.as_str().expect("directory entry is a string");
        assert!(
            root.join(relative).is_dir(),
            "enable must materialize directory {relative} on disk"
        );
    }
    assert!(root.join("infrastructure/testing").is_dir());
    assert!(
        root.join("infrastructure/network").is_dir(),
        "testing's transitive dependency (global-network) must also be materialized"
    );

    let lockfile_path = root.join(".ggen/packs.lock");
    assert!(
        lockfile_path.is_file(),
        "packs.lock must be written by enable"
    );
    let lockfile = read_json(&lockfile_path);
    let locked_packs = lockfile["packs"].as_object().expect("packs.lock packs map");
    for expected in ["aws-testing-boundary-pack", "fortune5-testing-bblock-pack"] {
        assert!(
            locked_packs.contains_key(expected),
            "packs.lock missing expected pack {expected}"
        );
    }
    for (pack_id, entry) in locked_packs {
        let integrity = entry["integrity"]
            .as_str()
            .unwrap_or_else(|| panic!("pack {pack_id} has no integrity value"));
        assert!(
            integrity.starts_with("blake3-"),
            "pack {pack_id} integrity {integrity} must be a blake3- digest"
        );
    }

    // Receipt chain must be real history, not a fixed placeholder: the first
    // enable's intent chains off GENESIS, its result chains off that intent's
    // own digest, and a second enable's intent chains off the first result --
    // never resetting.
    let enable_intent = read_json(&root.join(enable_json["intent_receipt"].as_str().unwrap()));
    let enable_result = read_json(&root.join(enable_json["result_receipt"].as_str().unwrap()));
    assert_eq!(enable_intent["previous_digest"], "GENESIS");
    assert_eq!(enable_result["previous_digest"], enable_intent["digest"]);

    let enable_out_2 = run_bblock(
        root,
        &[
            "enable",
            "--group-id",
            "testing",
            "--provider",
            "aws",
            "--format",
            "json",
        ],
    );
    let _ = enable_out_2.assert_success();
    let enable_json_2 = stdout_json(&enable_out_2);
    let enable_intent_2 = read_json(&root.join(enable_json_2["intent_receipt"].as_str().unwrap()));
    assert_eq!(
        enable_intent_2["previous_digest"], enable_result["digest"],
        "second enable's intent must chain off the first enable's result, not reset to GENESIS"
    );
}

/// Real consumer scenario #2: the compatibility alias `gpc` for `gcp` must
/// resolve to a byte-identical, deterministic, duplicate-free plan for the
/// entire `fortune5-complete` bundle across separate subprocess invocations --
/// and the read-only `inspect` verb must never write project state.
#[test]
fn gpc_alias_and_gcp_produce_identical_deterministic_plans_for_the_complete_bundle() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    let canonical = run_bblock(
        root,
        &[
            "inspect",
            "--group-id",
            "fortune5-complete",
            "--provider",
            "gcp",
            "--format",
            "json",
        ],
    );
    let _ = canonical.assert_success();
    let alias = run_bblock(
        root,
        &[
            "inspect",
            "--group-id",
            "fortune5-complete",
            "--provider",
            "gpc",
            "--format",
            "json",
        ],
    );
    let _ = alias.assert_success();
    let repeated = run_bblock(
        root,
        &[
            "inspect",
            "--group-id",
            "fortune5-complete",
            "--provider",
            "gcp",
            "--format",
            "json",
        ],
    );
    let _ = repeated.assert_success();

    let canonical_json = stdout_json(&canonical);
    let alias_json = stdout_json(&alias);
    let repeated_json = stdout_json(&repeated);
    assert_eq!(
        canonical_json, alias_json,
        "gpc alias must resolve identically to gcp"
    );
    assert_eq!(
        canonical_json, repeated_json,
        "resolution must be deterministic across subprocess runs"
    );

    let packs = canonical_json["packs"].as_array().expect("packs array");
    assert!(!packs.is_empty());
    let pack_strs: Vec<&str> = packs
        .iter()
        .map(|p| p.as_str().expect("pack id string"))
        .collect();
    let unique: BTreeSet<&str> = pack_strs.iter().copied().collect();
    assert_eq!(
        unique.len(),
        pack_strs.len(),
        "resolved packs must be duplicate-free"
    );
    assert!(
        pack_strs.iter().any(|p| p.starts_with("gcp-")),
        "gcp packs must be present"
    );
    assert!(
        !pack_strs
            .iter()
            .any(|p| p.starts_with("aws-") || p.starts_with("azure-")),
        "only the resolved provider's packs may appear, found a cross-provider pack"
    );

    let resolved_groups = canonical_json["resolved_groups"]
        .as_array()
        .expect("resolved_groups");
    assert_eq!(
        resolved_groups.last().unwrap(),
        "fortune5-complete",
        "the requested composite group must resolve last (dependency-first order)"
    );
    assert!(
        resolved_groups.iter().any(|g| g == "testing"),
        "the complete bundle must include the executable-verification group"
    );

    assert!(
        !root.join(".ggen").exists(),
        "read-only inspect must never write project state"
    );
}

/// Real consumer scenario #3: unknown providers and unknown groups are typed
/// refusals (non-zero exit, a specific message on stderr), not silent
/// fall-through -- and a refused call must leave no partial project state.
#[test]
fn refuses_unknown_provider_and_unknown_group_with_typed_nonzero_exits() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    let bad_provider = run_bblock(
        root,
        &[
            "inspect",
            "--group-id",
            "fortune5-complete",
            "--provider",
            "oracle",
        ],
    );
    let _ = bad_provider.assert_failure();
    assert!(
        bad_provider.stderr.contains("unsupported provider"),
        "refusal must name the bad provider, got stderr:\n{}",
        bad_provider.stderr
    );

    let bad_group = run_bblock(
        root,
        &[
            "inspect",
            "--group-id",
            "not-a-real-group",
            "--provider",
            "aws",
        ],
    );
    let _ = bad_group.assert_failure();
    assert!(
        bad_group.stderr.contains("unknown bblock group"),
        "refusal must name the unknown group, got stderr:\n{}",
        bad_group.stderr
    );

    assert!(
        !root.join(".ggen").exists(),
        "a refused inspect must not leave partial project state behind"
    );
}

fn term_to_local(term: &Term) -> String {
    match term {
        Term::NamedNode(node) => node
            .as_str()
            .rsplit(['#', '/'])
            .next()
            .unwrap_or_else(|| node.as_str())
            .to_string(),
        Term::Literal(literal) => literal.value().to_string(),
        other => panic!("unexpected RDF term kind in bblock ontology: {other:?}"),
    }
}

#[allow(deprecated)]
fn select(store: &Store, query: &str) -> Vec<oxigraph::sparql::QuerySolution> {
    match store
        .query(query)
        .unwrap_or_else(|error| panic!("query failed: {error}\n{query}"))
    {
        QueryResults::Solutions(solutions) => solutions
            .collect::<Result<Vec<_>, _>>()
            .expect("read solution rows"),
        QueryResults::Boolean(value) => {
            panic!("expected SELECT solutions, got ASK -> {value} for query:\n{query}")
        }
        QueryResults::Graph(_) => {
            panic!("expected SELECT solutions, got CONSTRUCT/DESCRIBE for query:\n{query}")
        }
    }
}

fn multi_valued_by_group(store: &Store, predicate: &str) -> BTreeMap<String, BTreeSet<String>> {
    let query = format!(
        "{GATE_NS}SELECT ?groupId ?value WHERE {{ ?g a bb:BlockGroup ; bb:groupId ?groupId ; {predicate} ?value . }}"
    );
    let mut map: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for row in select(store, &query) {
        let group_id = term_to_local(&row.get("groupId").expect("groupId").clone());
        let value = term_to_local(&row.get("value").expect("value").clone());
        map.entry(group_id).or_default().insert(value);
    }
    map
}

fn json_array_to_set(value: &Value) -> BTreeSet<String> {
    value
        .as_array()
        .map(|items| {
            items
                .iter()
                .map(|v| v.as_str().unwrap().to_string())
                .collect()
        })
        .unwrap_or_default()
}

/// The pack's own claim (`pack.toml`, `README.md`): the retained JSON catalog
/// consumed by `ggen bblock <verb>` is "independently verified against this
/// ontology" and "checked for equivalence in CI". Confirmed before writing this
/// test: nothing in the workspace actually performs that check. This test does
/// it for real -- deriving group directories/dependencies/pack projections and
/// provider aliases from `ontology.ttl` via real SPARQL execution (not a
/// re-parse of the JSON) and asserting set-equality against the embedded
/// catalog -- and additionally executes both of the pack's own `gates/*.rq`
/// law queries against that same ontology, proving the acyclicity/safety/
/// completeness invariants they encode actually hold.
#[test]
fn catalog_is_equivalent_to_the_pack_ontology_and_the_ontology_passes_its_own_law_gates() {
    let pack = pack_root();
    let ontology_ttl =
        std::fs::read_to_string(pack.join("ontology.ttl")).expect("read ontology.ttl");
    let catalog_json: Value = serde_json::from_str(
        &std::fs::read_to_string(pack.join("catalog/fortune5-bblocks.json"))
            .expect("read catalog json"),
    )
    .expect("parse catalog json");

    let store = Store::new().expect("create oxigraph store");
    store
        .load_from_reader(
            RdfParser::from_format(RdfFormat::Turtle),
            ontology_ttl.as_bytes(),
        )
        .expect("parse ontology.ttl into the store");

    let ontology_directories: BTreeMap<String, String> = select(
        &store,
        &format!(
            "{GATE_NS}SELECT ?groupId ?directory WHERE {{ ?g a bb:BlockGroup ; bb:groupId ?groupId ; bb:outputDirectory ?directory . }}"
        ),
    )
    .into_iter()
    .map(|row| {
        (
            term_to_local(&row.get("groupId").unwrap().clone()),
            term_to_local(&row.get("directory").unwrap().clone()),
        )
    })
    .collect();
    let ontology_deps = multi_valued_by_group(&store, "bb:dependsOn");
    let ontology_common = multi_valued_by_group(&store, "bb:commonPack");
    let ontology_aws = multi_valued_by_group(&store, "bb:awsPack");
    let ontology_azure = multi_valued_by_group(&store, "bb:azurePack");
    let ontology_gcp = multi_valued_by_group(&store, "bb:gcpPack");

    let catalog_groups = catalog_json["groups"]
        .as_array()
        .expect("catalog groups array");
    assert_eq!(
        catalog_groups.len(),
        ontology_directories.len(),
        "catalog group count must equal ontology bb:BlockGroup count"
    );

    for group in catalog_groups {
        let id = group["id"].as_str().expect("group id");
        assert_eq!(
            ontology_directories.get(id).map(String::as_str),
            group["directory"].as_str(),
            "outputDirectory drifted between ontology.ttl and the catalog for group {id}"
        );
        assert_eq!(
            ontology_deps.get(id).cloned().unwrap_or_default(),
            json_array_to_set(&group["dependencies"]),
            "dependsOn drifted between ontology.ttl and the catalog for group {id}"
        );
        assert_eq!(
            ontology_common.get(id).cloned().unwrap_or_default(),
            json_array_to_set(&group["common_packs"]),
            "commonPack drifted between ontology.ttl and the catalog for group {id}"
        );
        assert_eq!(
            ontology_aws.get(id).cloned().unwrap_or_default(),
            json_array_to_set(&group["provider_packs"]["aws"]),
            "awsPack drifted between ontology.ttl and the catalog for group {id}"
        );
        assert_eq!(
            ontology_azure.get(id).cloned().unwrap_or_default(),
            json_array_to_set(&group["provider_packs"]["azure"]),
            "azurePack drifted between ontology.ttl and the catalog for group {id}"
        );
        assert_eq!(
            ontology_gcp.get(id).cloned().unwrap_or_default(),
            json_array_to_set(&group["provider_packs"]["gcp"]),
            "gcpPack drifted between ontology.ttl and the catalog for group {id}"
        );
    }

    let ontology_provider_aliases: BTreeMap<String, BTreeSet<String>> = {
        let query = format!(
            "{GATE_NS}SELECT ?providerId ?alias WHERE {{ ?p a bb:Provider ; bb:providerId ?providerId ; bb:alias ?alias . }}"
        );
        let mut map: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for row in select(&store, &query) {
            let provider_id = term_to_local(&row.get("providerId").unwrap().clone());
            let alias = term_to_local(&row.get("alias").unwrap().clone());
            map.entry(provider_id).or_default().insert(alias);
        }
        map
    };
    for provider in catalog_json["providers"]
        .as_array()
        .expect("catalog providers array")
    {
        let id = provider["id"].as_str().expect("provider id");
        assert_eq!(
            ontology_provider_aliases
                .get(id)
                .cloned()
                .unwrap_or_default(),
            json_array_to_set(&provider["aliases"]),
            "provider aliases drifted between ontology.ttl and the catalog for provider {id}"
        );
    }

    // Both of the pack's own SPARQL law gates must return zero violation rows
    // against the real ontology -- proving, not merely asserting, that the
    // catalog is closed/provider-complete (010) and acyclic/safe/broker-only
    // with directActuation=false (020).
    for gate_file in [
        "gates/010_catalog_contract.rq",
        "gates/020_safe_acyclic_broker_only.rq",
    ] {
        let gate_query = std::fs::read_to_string(pack.join(gate_file))
            .unwrap_or_else(|error| panic!("read {gate_file}: {error}"));
        let violations = select(&store, &gate_query);
        assert!(
            violations.is_empty(),
            "gate {gate_file} reported {} violation row(s): {:?}",
            violations.len(),
            violations
                .iter()
                .map(|row| row
                    .iter()
                    .map(|(var, term)| format!("{var}={term}"))
                    .collect::<Vec<_>>()
                    .join(", "))
                .collect::<Vec<_>>()
        );
    }
}
