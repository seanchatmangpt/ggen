//! Chicago-TDD closure for `cnv:CustomBehavior`, the zero-code clap-noun-verb
//! compiler's sole, explicitly admitted handler-seam escape hatch.
//!
//! Real temporary filesystem, the real `ggen` binary, real `cargo build`/
//! `cargo test` on the manufactured crate. No pack loader, renderer, or
//! process collaborator is mocked.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};
use std::process::Command;

use chicago_tdd_tools::cli_proof::CliHarness;
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

/// One closed command (`system ping`) alongside one `cnv:CustomBehavior`
/// command (`price lookup`) that a real domain rule -- branching on the SKU
/// argument's value -- cannot express through any of the six closed
/// primitives.
const ONTOLOGY: &str = r#"@prefix cnv: <https://clap-noun-verb.dev/ontology#> .

cnv:DemoCli
    a cnv:Cli ;
    cnv:binaryName "customdemo" ;
    cnv:crateName "customdemo" ;
    cnv:version "0.1.0" ;
    cnv:edition "2024" ;
    cnv:rustVersion "1.85" ;
    cnv:about "Custom behavior regression fixture." ;
    cnv:hasNoun cnv:SystemNoun, cnv:PriceNoun .

cnv:SystemNoun
    a cnv:Noun ;
    cnv:name "system" ;
    cnv:about "Inspect the system." ;
    cnv:hasCommand cnv:SystemPing .

cnv:SystemPing
    a cnv:Command ;
    cnv:name "ping" ;
    cnv:about "Return liveness." ;
    cnv:belongsToNoun cnv:SystemNoun ;
    cnv:hasBehavior cnv:PingBehavior .

cnv:PingBehavior
    a cnv:StaticJsonBehavior ;
    cnv:jsonValue "{\"status\":\"alive\"}" .

cnv:PriceNoun
    a cnv:Noun ;
    cnv:name "price" ;
    cnv:about "Domain-specific pricing logic." ;
    cnv:hasCommand cnv:PriceLookup .

cnv:PriceLookup
    a cnv:Command ;
    cnv:name "lookup" ;
    cnv:about "Look up a price via custom domain logic." ;
    cnv:belongsToNoun cnv:PriceNoun ;
    cnv:hasArgument cnv:PriceSku ;
    cnv:hasBehavior cnv:PricingBehavior .

cnv:PriceSku
    a cnv:Argument ;
    cnv:name "sku" ;
    cnv:fieldName "sku" ;
    cnv:valueKind "string" ;
    cnv:required true ;
    cnv:position 1 ;
    cnv:testValue "SKU-1" ;
    cnv:about "Product SKU." .

cnv:PricingBehavior a cnv:CustomBehavior .
"#;

fn scaffold_consumer() -> (TempDir, PathBuf) {
    let directory = TempDir::new().expect("temporary directory");
    for pack in CUSTOM_BEHAVIOR_PACKS {
        copy_tree(&packs_dir().join(pack), &directory.path().join(pack));
    }

    let project = directory.path().join("consumer");
    std::fs::create_dir_all(&project).expect("consumer directory");
    std::fs::write(project.join("ontology.ttl"), ONTOLOGY).expect("consumer ontology");

    let packs_table: String = CUSTOM_BEHAVIOR_PACKS
        .iter()
        .map(|pack| format!("{pack} = {{ path = \"../{pack}\" }}\n"))
        .collect();
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"custom-behavior-demo\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [ontology.prefixes]\ncnv = \"https://clap-noun-verb.dev/ontology#\"\n\n\
             [packs]\n{packs_table}\n\
             [templates]\ndir = \".\"\naggregate_modules = false\n"
        ),
    )
    .expect("consumer manifest");

    (directory, project)
}

#[test]
fn custom_behavior_scaffolds_once_and_survives_hand_completion() {
    let (_directory, project) = scaffold_consumer();

    assert_cli_success(&project, &["sync", "run"]);

    let handler_path = project.join("src/custom_handlers.rs");
    let scaffolded = std::fs::read_to_string(&handler_path).expect("scaffolded custom_handlers.rs");
    assert!(
        scaffolded.contains("pub fn price_lookup"),
        "scaffold must declare a stub named after the command's own noun and verb"
    );
    assert!(
        scaffolded.contains("todo!"),
        "an unfinished custom handler must fail to compile via todo!(), not silently pass"
    );

    let generated_cli =
        std::fs::read_to_string(project.join("src/generated_cli.rs")).expect("generated_cli.rs");
    assert!(
        generated_cli.contains("crate::custom_handlers::price_lookup(inputs)"),
        "the routing layer must dispatch the custom command to its own handler"
    );

    // Hand-complete the scaffold, exactly as a consumer would.
    let hand_completed = scaffolded.replace(
        "todo!(\"implement price lookup\")",
        "Ok(serde_json::json!({ \"sku\": inputs.get(\"sku\").and_then(serde_json::Value::as_str).unwrap_or(\"\"), \"price_cents\": 1999 }))",
    );
    assert_ne!(
        hand_completed, scaffolded,
        "replacement must actually apply"
    );
    std::fs::write(&handler_path, &hand_completed).expect("hand-complete custom handler");

    // A second sync must never clobber hand-completed scaffolding.
    assert_cli_success(&project, &["sync", "run"]);
    let after_resync =
        std::fs::read_to_string(&handler_path).expect("custom_handlers.rs after resync");
    assert_eq!(
        after_resync, hand_completed,
        "unless_exists must leave a hand-completed custom_handlers.rs untouched"
    );

    // The generated crate -- including the generated wiring-proof test --
    // must build and pass with the hand-completed handler in place.
    let test_output = run_cargo(&project, &["test", "--quiet"]);
    assert!(
        test_output.status.success(),
        "generated crate tests failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&test_output.stdout),
        String::from_utf8_lossy(&test_output.stderr)
    );

    // Real dispatch through the compiled binary produces real, deterministic
    // output -- not a mock, not a stub.
    let build_output = run_cargo(&project, &["build", "--quiet"]);
    assert!(
        build_output.status.success(),
        "generated crate failed to build"
    );
    let binary = project.join("target/debug/customdemo");
    let run_output = Command::new(&binary)
        .args(["price", "lookup", "SKU-1"])
        .output()
        .expect("run manufactured binary");
    assert!(
        run_output.status.success(),
        "price lookup must succeed once hand-completed"
    );
    let stdout = String::from_utf8_lossy(&run_output.stdout);
    assert!(
        stdout.contains("\"sku\""),
        "real handler output must reach argv: {stdout}"
    );
    assert!(
        stdout.contains("1999"),
        "real handler output must reach argv: {stdout}"
    );
}

#[test]
fn custom_behavior_wiring_proof_test_catches_misdirected_routing() {
    let (_directory, project) = scaffold_consumer();
    assert_cli_success(&project, &["sync", "run"]);

    // Baseline: the generated wiring-proof test passes on correct output.
    let baseline = run_cargo(
        &project,
        &["test", "--quiet", "--lib", "generated_route_proofs"],
    );
    assert!(
        baseline.status.success(),
        "baseline generated_route_proofs must pass before sabotage\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&baseline.stdout),
        String::from_utf8_lossy(&baseline.stderr)
    );

    // Sabotage: give generated_cli.rs a second custom command routed to the
    // WRONG handler name, but define that handler too so the crate still
    // compiles -- this is exactly the class of bug a compile error alone
    // cannot catch, and the only class every_custom_command_routes_to_its_own_handler
    // exists to catch.
    let generated_cli_path = project.join("src/generated_cli.rs");
    let generated_cli = std::fs::read_to_string(&generated_cli_path).expect("generated_cli.rs");
    let sabotaged = generated_cli.replacen(
        "crate::custom_handlers::price_lookup(inputs)",
        "crate::custom_handlers::misdirected_stand_in(inputs)",
        1,
    );
    assert_ne!(
        sabotaged, generated_cli,
        "sabotage replacement must actually apply"
    );
    std::fs::write(&generated_cli_path, &sabotaged).expect("write sabotaged generated_cli.rs");

    let handler_path = project.join("src/custom_handlers.rs");
    let handlers = std::fs::read_to_string(&handler_path).expect("custom_handlers.rs");
    let handlers_with_decoy = format!(
        "{handlers}\npub fn misdirected_stand_in(inputs: serde_json::Map<String, serde_json::Value>) -> clap_noun_verb::Result<serde_json::Value> {{\n    let _ = inputs;\n    Ok(serde_json::Value::Null)\n}}\n"
    );
    std::fs::write(&handler_path, handlers_with_decoy).expect("write decoy handler");

    let sabotaged_result = run_cargo(
        &project,
        &["test", "--quiet", "--lib", "generated_route_proofs"],
    );
    assert!(
        !sabotaged_result.status.success(),
        "every_custom_command_routes_to_its_own_handler must fail when routing is misdirected \
         to a real, compiling, but wrong function -- a passing result here means the generated \
         proof is decorative, not load-bearing"
    );
    let combined = format!(
        "{}{}",
        String::from_utf8_lossy(&sabotaged_result.stdout),
        String::from_utf8_lossy(&sabotaged_result.stderr)
    );
    assert!(
        combined.contains("must route to its own custom handler"),
        "failure must come from the wiring-proof assertion, not an unrelated error: {combined}"
    );
}
