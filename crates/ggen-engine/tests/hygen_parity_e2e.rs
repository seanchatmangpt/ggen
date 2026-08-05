//! Hygen-parity checkpoint: composite, real end-to-end scenarios (via the
//! actual `ggen` binary subprocess, `chicago_tdd_tools::cli_proof::CliHarness`
//! — no mocks) proving ggen reproduces hygen's canonical generator workflow:
//! https://www.hygen.io/docs/quick-start (new-component-plus-barrel-injection)
//! and https://www.hygen.io/docs/templates (`to`, `inject`, `before`/`after`,
//! `unless_exists`, `skip_if`, `sh`).
//!
//! This file is deliberately NOT a duplicate of
//! `write_behaviors_cli_e2e.rs`'s exhaustive per-branch decision-table
//! proofs. Each test here assembles a realistic hygen-shaped generator
//! scenario end-to-end, the way a user would actually invoke it, and is the
//! gate: if hygen parity regresses, one of these fails.
//!
//! One real gap vs hygen, noted rather than hidden: hygen's variables come
//! from interactive CLI prompts / `--name` flags; ggen has no prompt layer,
//! so the equivalent here is a SPARQL-bound row variable (`{{ row.name }}`)
//! sourced from the project ontology. Same substitution role, different
//! source of truth (RDF fact instead of a terminal prompt) -- by design,
//! not a missing feature (see CLAUDE.md's "A = μ(O)" formula).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "hygen-parity-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

fn scaffold(root: &Path, ontology: &str) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ontology).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
}

fn write_template(root: &Path, name: &str, content: &str) {
    std::fs::write(root.join("templates").join(name), content).expect("write template");
}

fn run_sync(root: &Path) -> chicago_tdd_tools::cli_proof::CliOutput {
    CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(root)
        .run()
        .expect("spawn ggen sync run")
}

/// hygen quick-start's flagship example: `hygen component new --name Button`
/// both scaffolds `components/Button.tsx` AND injects an export line into
/// `components/index.ts`'s barrel, from one generator invocation. ggen's
/// equivalent: one `sync run` driving two templates off the same ontology
/// fact, one creating (dynamic `to:` from a SPARQL-bound row variable), one
/// injecting (`inject: true` + `after:`).
#[test]
fn component_scaffold_creates_file_and_injects_barrel_export_in_one_run() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        r#"
@prefix ex: <http://example.org/> .
ex:Button ex:componentName "Button" .
"#,
    );
    std::fs::write(
        dir.path().join("components_index.ts"),
        "// COMPONENT-EXPORTS\n",
    )
    .expect("pre-existing barrel file");

    write_template(
        dir.path(),
        "component.tmpl",
        "---\nto: \"components/{{ row.name }}.tsx\"\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:componentName ?name }\nfor_each: entities\n---\nexport function {{ row.name }}() {\n  return null;\n}\n",
    );
    write_template(
        dir.path(),
        "barrel.tmpl",
        "---\nto: components_index.ts\ninject: true\nafter: \"// COMPONENT-EXPORTS\"\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:componentName ?name }\nfor_each: entities\n---\nexport { {{ row.name }} } from \"./components/{{ row.name }}\";\n",
    );

    run_sync(dir.path()).assert_success();

    let component = std::fs::read_to_string(dir.path().join("components/Button.tsx"))
        .expect("component file must be created");
    assert_eq!(component, "export function Button() {\n  return null;\n}\n");

    let barrel =
        std::fs::read_to_string(dir.path().join("components_index.ts")).expect("barrel file");
    assert_eq!(
        barrel,
        "// COMPONENT-EXPORTS\nexport { Button } from \"./components/Button\";\n"
    );
}

/// hygen's `unless_exists: true`: re-running the generator over a
/// hand-edited scaffold must never clobber the human's edits.
#[test]
fn unless_exists_preserves_a_hand_edited_scaffold_across_reruns() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        r#"
@prefix ex: <http://example.org/> .
ex:Button ex:componentName "Button" .
"#,
    );
    std::fs::create_dir_all(dir.path().join("components")).expect("mkdir components");
    std::fs::write(
        dir.path().join("components/Button.tsx"),
        "// hand-customized, do not touch\nexport function Button() {\n  return <div>custom</div>;\n}\n",
    )
    .expect("pre-existing hand-edited file");

    write_template(
        dir.path(),
        "component.tmpl",
        "---\nto: \"components/{{ row.name }}.tsx\"\nunless_exists: true\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:componentName ?name }\nfor_each: entities\n---\nexport function {{ row.name }}() {\n  return null;\n}\n",
    );

    run_sync(dir.path()).assert_success();

    let after =
        std::fs::read_to_string(dir.path().join("components/Button.tsx")).expect("component file");
    assert_eq!(
        after,
        "// hand-customized, do not touch\nexport function Button() {\n  return <div>custom</div>;\n}\n",
        "unless_exists must never overwrite a pre-existing hand-edited scaffold"
    );
}

/// hygen's `skip_if:`: a generator marker already present in the target
/// means "already generated here, don't regenerate" -- e.g. skip re-adding
/// a route registration that's already wired up by hand.
#[test]
fn skip_if_prevents_regenerating_an_already_marked_file() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        r#"
@prefix ex: <http://example.org/> .
ex:route ex:routeName "users" .
"#,
    );
    std::fs::write(
        dir.path().join("routes.ts"),
        "// GENERATED-ROUTE:users\nrouter.get(\"/users\", customHandler);\n",
    )
    .expect("pre-existing marked file");

    write_template(
        dir.path(),
        "route.tmpl",
        "---\nto: routes.ts\nskip_if: \"GENERATED-ROUTE:{{ row.name }}\"\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:routeName ?name }\nfor_each: entities\n---\n// GENERATED-ROUTE:{{ row.name }}\nrouter.get(\"/{{ row.name }}\", defaultHandler);\n",
    );

    run_sync(dir.path()).assert_success();

    let after = std::fs::read_to_string(dir.path().join("routes.ts")).expect("routes.ts");
    assert_eq!(
        after, "// GENERATED-ROUTE:users\nrouter.get(\"/users\", customHandler);\n",
        "skip_if must leave the marked file untouched, not regenerate over the custom handler"
    );
}

/// hygen's `sh:` post-generation hook (its docs' own example: running a
/// formatter after scaffolding). Proves the hook actually executes, exactly
/// once, and only after a real `Written` outcome -- not on every sync
/// attempt regardless of result.
#[test]
fn sh_after_hook_runs_exactly_once_after_a_real_write() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(
        dir.path(),
        r#"
@prefix ex: <http://example.org/> .
ex:Button ex:componentName "Button" .
"#,
    );

    write_template(
        dir.path(),
        "component.tmpl",
        "---\nto: \"components/{{ row.name }}.tsx\"\nsh_after: \"echo formatted >> format.log\"\nsparql:\n  entities: |\n    PREFIX ex: <http://example.org/>\n    SELECT ?name WHERE { ?s ex:componentName ?name }\nfor_each: entities\n---\nexport function {{ row.name }}() {\n  return null;\n}\n",
    );

    run_sync(dir.path()).assert_success();

    let log = std::fs::read_to_string(dir.path().join("format.log")).expect("format.log");
    assert_eq!(
        log, "formatted\n",
        "sh_after must run exactly once after the real write, matching hygen's post-gen `sh:` hook"
    );

    // Re-run with the file now present and identical: no new write, so the
    // hook must not fire again (hygen's `sh:` only runs on real generation,
    // not on a no-op rerun).
    run_sync(dir.path()).assert_success();
    let log_after_rerun =
        std::fs::read_to_string(dir.path().join("format.log")).expect("format.log");
    assert_eq!(
        log_after_rerun, "formatted\n",
        "sh_after must not re-fire on a no-op rerun where content is unchanged"
    );
}
