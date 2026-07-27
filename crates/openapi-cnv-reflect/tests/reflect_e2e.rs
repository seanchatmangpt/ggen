//! Chicago-TDD closure for the OpenAPI reflector: the actual "N endpoints ->
//! one reflection pass, not N hand-authored ontology blocks" claim made
//! checkable, not just asserted. Real temporary filesystem, the real `ggen`
//! binary, real `cargo build`/`cargo test` on the manufactured crate -- no
//! mocks.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};
use std::process::Command;

use chicago_tdd_tools::cli_proof::CliHarness;
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use tempfile::TempDir;

const CUSTOM_BEHAVIOR_PACKS: [&str; 6] = [
    "clap-noun-verb-schema-pack",
    "clap-noun-verb-crate-pack",
    "clap-noun-verb-routing-pack",
    "clap-noun-verb-behavior-pack",
    "clap-noun-verb-boundary-pack",
    "clap-noun-verb-verification-pack",
];

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn fixture_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/petstore-slice.json")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir destination");
    for entry in std::fs::read_dir(src).expect("read source directory") {
        let entry = entry.expect("directory entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy file");
        }
    }
}

fn count(store: &oxigraph::store::Store, query: &str) -> usize {
    let results = SparqlEvaluator::new()
        .parse_query(query)
        .expect("parse SPARQL query")
        .on_store(store)
        .execute()
        .expect("execute SPARQL query");
    match results {
        QueryResults::Solutions(solutions) => solutions.count(),
        QueryResults::Boolean(_) | QueryResults::Graph(_) => {
            panic!("expected a SELECT query's Solutions, got a different result kind")
        }
    }
}

fn assert_cli_success(project: &Path, args: &[&str]) {
    let output = CliHarness::cargo_bin("ggen")
        .args(args)
        .current_dir(project)
        .run()
        .expect("run ggen");
    output.assert_success();
}

fn run_cargo(project: &Path, args: &[&str]) -> std::process::Output {
    Command::new("cargo")
        .args(args)
        .current_dir(project)
        .output()
        .expect("run cargo")
}

/// The reflector itself: does it actually turn N OpenAPI operations into N
/// `cnv:Command` individuals (minus the one deliberately-out-of-slice POST),
/// grouped into the right number of nouns, with a warning for the skip --
/// not silently dropped?
#[test]
fn reflecting_the_fixture_produces_the_expected_graph_shape() {
    let raw = std::fs::read_to_string(fixture_path()).expect("read fixture");
    let spec: serde_json::Value = serde_json::from_str(&raw).expect("parse fixture JSON");

    let outcome = openapi_cnv_reflect::reflect(&spec).expect("reflect fixture");

    assert_eq!(
        outcome.warnings.len(),
        1,
        "exactly the POST /store/order operation must be skipped, with a reason, not silently \
         dropped and not miscounted: {:?}",
        outcome.warnings
    );
    assert!(
        outcome.warnings[0].reason.contains("GET"),
        "the warning must explain WHY it was skipped: {:?}",
        outcome.warnings[0]
    );

    let cnv = "PREFIX cnv: <https://clap-noun-verb.dev/ontology#>";
    assert_eq!(
        count(
            &outcome.store,
            &format!("{cnv} SELECT ?c WHERE {{ ?c a cnv:Command }}")
        ),
        4,
        "the 4 GET operations in the fixture must become exactly 4 cnv:Command individuals"
    );
    assert_eq!(
        count(
            &outcome.store,
            &format!("{cnv} SELECT ?n WHERE {{ ?n a cnv:Noun }}")
        ),
        2,
        "the fixture's two tags (pets, store) must become exactly 2 cnv:Noun individuals"
    );
    assert_eq!(
        count(
            &outcome.store,
            &format!("{cnv} SELECT ?b WHERE {{ ?b a cnv:CustomBehavior }}")
        ),
        4,
        "every reflected command must carry cnv:CustomBehavior -- a real HTTP call is never \
         one of the six closed primitives"
    );
    assert_eq!(
        count(
            &outcome.store,
            &format!("{cnv} SELECT ?a WHERE {{ ?a a cnv:Argument }}")
        ),
        4,
        "listPets(limit,tag) + getPetById(petId) + getOrderById(orderId) = 4 arguments \
         (getInventory has none)"
    );
}

/// The actual claim: reflect once, and the real zero-code compiler admits
/// the result, manufactures a complete crate, and that crate's own tests
/// (including the generated wiring-proof test) pass -- with zero
/// hand-authored ontology facts beyond the OpenAPI document itself.
#[test]
fn reflected_ontology_passes_through_the_real_zero_code_pipeline() {
    let raw = std::fs::read_to_string(fixture_path()).expect("read fixture");
    let spec: serde_json::Value = serde_json::from_str(&raw).expect("parse fixture JSON");
    let outcome = openapi_cnv_reflect::reflect(&spec).expect("reflect fixture");

    let directory = TempDir::new().expect("temporary directory");
    for pack in CUSTOM_BEHAVIOR_PACKS {
        copy_tree(&packs_dir().join(pack), &directory.path().join(pack));
    }

    let project = directory.path().join("consumer");
    std::fs::create_dir_all(&project).expect("consumer directory");
    openapi_cnv_reflect::write_ontology(&outcome.store, &project.join("ontology.ttl"))
        .expect("write reflected ontology");

    let packs_table: String = CUSTOM_BEHAVIOR_PACKS
        .iter()
        .map(|pack| format!("{pack} = {{ path = \"../{pack}\" }}\n"))
        .collect::<Vec<_>>()
        .join("");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"reflected-demo\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [ontology.prefixes]\ncnv = \"https://clap-noun-verb.dev/ontology#\"\n\n\
             [packs]\n{packs_table}\n\
             [templates]\ndir = \".\"\naggregate_modules = false\n"
        ),
    )
    .expect("consumer manifest");

    // The real gates admit it, and the real compiler manufactures a real crate.
    assert_cli_success(&project, &["sync", "run"]);

    for expected in [
        "src/generated_cli.rs",
        "src/custom_handlers.rs",
        "Cargo.toml",
    ] {
        assert!(
            project.join(expected).is_file(),
            "expected {expected} to be manufactured"
        );
    }

    let custom_handlers = std::fs::read_to_string(project.join("src/custom_handlers.rs"))
        .expect("scaffolded custom_handlers.rs");
    for expected_fn in [
        "pub fn pets_list_pets",
        "pub fn pets_get_pet_by_id",
        "pub fn store_get_inventory",
        "pub fn store_get_order_by_id",
    ] {
        assert!(
            custom_handlers.contains(expected_fn),
            "every reflected operation must get its own scaffolded stub: missing {expected_fn} \
             in:\n{custom_handlers}"
        );
    }

    // The generated crate -- including every_custom_command_routes_to_its_own_handler
    // for all four reflected commands -- must build and pass with zero hand edits
    // beyond the OpenAPI document itself.
    let build_output = run_cargo(&project, &["build", "--quiet"]);
    assert!(
        build_output.status.success(),
        "reflected crate failed to build\nstderr:\n{}",
        String::from_utf8_lossy(&build_output.stderr)
    );

    let test_output = run_cargo(&project, &["test", "--quiet"]);
    assert!(
        test_output.status.success(),
        "reflected crate's own tests failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&test_output.stdout),
        String::from_utf8_lossy(&test_output.stderr)
    );
}
