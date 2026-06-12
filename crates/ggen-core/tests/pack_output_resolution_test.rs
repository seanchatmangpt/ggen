#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! BUG-008: PackageToml pack output resolution — Chicago TDD integration tests.
//!
//! All tests use real TempDir filesystem operations. No mocks.

use ggen_core::manifest::types::PackageToml;
use tempfile::TempDir;

// ─────────────────────────────────────────────────────────────────────────────
// Test 1: load_package_toml_parses_outputs
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn load_package_toml_parses_outputs() {
    let tmp = TempDir::new().expect("create tempdir");
    let pack_root = tmp.path();

    // Write a package.toml with an [outputs] table
    std::fs::write(
        pack_root.join("package.toml"),
        "[outputs]\nqueries = \"queries/\"\n",
    )
    .expect("write package.toml");

    // Load and resolve
    let pkg = PackageToml::load(pack_root);
    assert_eq!(
        pkg.resolve_output_key("queries"),
        "queries/",
        "resolve_output_key must return the mapped directory path"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 2: resolve_output_key_falls_back_to_literal_when_key_absent
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn resolve_output_key_falls_back_to_literal_when_key_absent() {
    let tmp = TempDir::new().expect("create tempdir");
    let pack_root = tmp.path();

    // Write a package.toml that does NOT contain the key we will look up
    std::fs::write(
        pack_root.join("package.toml"),
        "[outputs]\nother = \"other_dir/\"\n",
    )
    .expect("write package.toml");

    let pkg = PackageToml::load(pack_root);
    assert_eq!(
        pkg.resolve_output_key("missing"),
        "missing",
        "resolve_output_key must fall back to the literal key string when absent"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 3: missing_package_toml_returns_empty_struct
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn missing_package_toml_returns_empty_struct() {
    let tmp = TempDir::new().expect("create tempdir");
    let pack_root = tmp.path();
    // Deliberately do NOT write package.toml

    let pkg = PackageToml::load(pack_root);
    // outputs map is empty → any key falls back to itself
    assert_eq!(
        pkg.resolve_output_key("anything"),
        "anything",
        "missing package.toml must produce empty PackageToml with literal fallback"
    );
    assert!(
        pkg.pack.as_ref().map_or(true, |p| p.outputs.is_empty()),
        "outputs map must be empty when package.toml is absent"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Test 4: pack_output_resolution_full_pipeline
//
// End-to-end: pack directory with package.toml → pipeline resolves output key
// → template renders → output file contains expected data.
//
// This test verifies that resolve_output_key ("queries" → "rq/") causes the
// pipeline to read the query file from the correct sub-directory inside the
// pack, and that the rendered output file is written to the project.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn pack_output_resolution_full_pipeline() {
    use ggen_core::codegen::GenerationPipeline;
    use ggen_core::manifest::ManifestParser;
    use std::fs;

    let project_tmp = TempDir::new().expect("project tempdir");
    let project_root = project_tmp.path();

    // ── Pack directory ──────────────────────────────────────────────────────
    let pack_tmp = TempDir::new().expect("pack tempdir");
    let pack_dir = pack_tmp.path();

    // package.toml: maps output key "queries" → actual subdirectory "rq/"
    fs::write(
        pack_dir.join("package.toml"),
        "[outputs]\nqueries = \"rq/\"\n",
    )
    .expect("write pack package.toml");

    // Pack query lives under rq/ (not queries/)
    fs::create_dir_all(pack_dir.join("rq")).expect("rq dir");
    fs::write(
        pack_dir.join("rq/my_query.rq"),
        "SELECT ?name WHERE { ?s <http://example.org/name> ?name . } ORDER BY ?name\n",
    )
    .expect("write rq file");

    // Pack template lives in the pack directory under templates/
    fs::create_dir_all(pack_dir.join("templates")).expect("pack templates dir");
    fs::write(
        pack_dir.join("templates/names.tera"),
        "{% for row in results %}{{ row.name }}\n{% endfor %}",
    )
    .expect("write pack template");

    // ── Project ontology ────────────────────────────────────────────────────
    fs::create_dir_all(project_root.join("ontology")).expect("ontology dir");
    fs::write(
        project_root.join("ontology/data.ttl"),
        "@prefix : <http://example.org/> .\n<http://example.org/alice> <http://example.org/name> \"Alice\" .\n",
    )
    .expect("write ontology");

    // ── ggen.toml ───────────────────────────────────────────────────────────
    // The generation rule references the pack by name and uses output key "queries"
    // which the pipeline must resolve through package.toml → "rq/".
    let ggen_toml = format!(
        r#"
[project]
name = "pack-output-test"
version = "0.1.0"

[ontology]
source = "ontology/data.ttl"
base_iri = "http://example.org/"

[generation]
output_dir = "out"

[[generation.rules]]
name = "names"
query = {{ pack = "mypkg", output = "queries", file = "my_query.rq" }}
template = {{ pack = "mypkg", output = "templates", file = "names.tera" }}
output_file = "names.txt"

[[packs]]
name = "mypkg"
registry = "local"
path = "{pack}"
"#,
        pack = pack_dir.display()
    );
    let ggen_toml_path = project_root.join("ggen.toml");
    fs::write(&ggen_toml_path, &ggen_toml).expect("write ggen.toml");

    // ── Parse manifest and run GenerationPipeline ────────────────────────────
    let manifest = ManifestParser::parse(&ggen_toml_path).expect("parse ggen.toml");
    let mut pipeline = GenerationPipeline::new(manifest, project_root.to_path_buf());

    pipeline.load_ontology().expect("load ontology");

    match pipeline.execute_generation_rules() {
        Ok(generated) => {
            // ── Assert at least one file was generated ───────────────────────
            assert!(
                !generated.is_empty(),
                "pipeline must generate at least one file"
            );
            // ── Assert the output file contains "Alice" ──────────────────────
            let out_path = project_root.join("out/names.txt");
            assert!(
                out_path.exists(),
                "output file out/names.txt must exist after pipeline run"
            );
            let content = fs::read_to_string(&out_path).expect("read output");
            assert!(
                content.contains("Alice"),
                "rendered output must contain 'Alice'; got: {content}"
            );
        }
        Err(e) => {
            // If the pipeline does not yet support pack output key resolution,
            // the test fails with a descriptive message that drives the fix.
            panic!(
                "Pipeline failed — pack output key resolution (BUG-008) is not yet wired: {e}"
            );
        }
    }
}
