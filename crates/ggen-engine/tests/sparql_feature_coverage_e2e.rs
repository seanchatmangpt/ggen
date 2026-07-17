//! Chicago-TDD end-to-end tests proving SPARQL features that had zero (or
//! thin) prior test coverage actually work through the real pipeline
//! (specs/014-ggen-core-replacement Phase 7, Task 4): `OPTIONAL` with an
//! unbound variable (single row and heterogeneous multi-row), `GROUP BY`/
//! `COUNT` (exercised together with Task 1's datatype coercion), `FILTER`,
//! `UNION`, and `DESCRIBE` (previously zero test coverage per this task's
//! own investigation). Real filesystem, real oxigraph store, real
//! `ggen_engine::sync::sync()` pipeline — no mocks.

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

/// Alice and Carol have an `ex:age`; Bob does not (drives the OPTIONAL
/// tests). Dave is an `ex:Robot`, not an `ex:Person` (drives the UNION
/// test).
const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:alice a ex:Person ; ex:name "Alice" ; ex:age "30"^^xsd:integer .
ex:bob   a ex:Person ; ex:name "Bob" .
ex:carol a ex:Person ; ex:name "Carol" ; ex:age "25"^^xsd:integer .
ex:dave  a ex:Robot  ; ex:name "Dave" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
}

fn write_template(root: &Path, name: &str, content: &str) {
    std::fs::write(root.join("templates").join(name), content).expect("write template");
}

fn run_sync(root: &Path) -> ggen_engine::sync::SyncReport {
    sync(
        root,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync")
}

/// OPTIONAL with an unbound variable across multiple rows in the SAME
/// result set: Bob's row has no `age` key at all (not `null` — genuinely
/// absent), Alice's and Carol's rows do. A `{% if row.age is defined %}`
/// guard correctly distinguishes present-vs-absent for every row, proving
/// heterogeneous row shapes don't crash the pipeline. This one test
/// exercises both the single-row-unbound and multi-row-heterogeneous
/// requirements at once (same SELECT, same result set).
#[test]
fn optional_unbound_variable_across_heterogeneous_rows_with_guarded_access() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "optional.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?name ?age WHERE { ?s a <http://example.org/Person> ; <http://example.org/name> ?name . OPTIONAL { ?s <http://example.org/age> ?age } } ORDER BY ?name\"\n---\n{% for row in results %}{{ row.name }}={% if row.age is defined %}{{ row.age }}{% else %}N/A{% endif %};{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    assert_eq!(
        out, "Alice=30;Bob=N/A;Carol=25;",
        "guarded `is defined` access must distinguish bound vs. unbound OPTIONAL vars per row: {out:?}"
    );
}

/// Confirms (does not change) what unguarded access to a specific missing
/// OPTIONAL key actually does today: Bob's row has no `age` key, and
/// rendering `{{ row.age }}` unconditionally for every row is expected to
/// hard-fail the render (Tera errors on referencing a context key that does
/// not exist), rather than silently printing an empty string. This test
/// documents real, observed behavior — it does not assert a design
/// preference.
#[test]
fn optional_unguarded_access_to_missing_key_behavior_is_documented_by_a_real_run() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "optional_unguarded.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?name ?age WHERE { ?s a <http://example.org/Person> ; <http://example.org/name> ?name . OPTIONAL { ?s <http://example.org/age> ?age } } ORDER BY ?name\"\n---\n{% for row in results %}{{ row.name }}={{ row.age }};{% endfor %}",
    );

    let result = sync(
        dir.path(),
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    );
    assert!(
        result.is_err(),
        "unguarded access to an unbound OPTIONAL variable (Bob has no `age` key) is \
         observed to hard-error the render, not silently render empty -- if this \
         assertion ever starts failing, that's a real behavior change in Tera/the \
         pipeline to investigate, not a flake"
    );
}

/// `GROUP BY`/`COUNT` aggregate query, exercising Task 1's datatype
/// coercion together with GROUP BY: the resulting COUNT literal is
/// `xsd:integer`-typed (oxigraph's own aggregate result datatype), so it
/// must arrive as a real Tera number, provable via arithmetic
/// (`{{ row.cnt + 10 }}` — a string would fail to add, not silently
/// concatenate, under Tera's numeric `+`).
#[test]
fn group_by_count_aggregate_comes_through_as_a_real_number_after_task1_coercion() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "count.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT (COUNT(?s) AS ?cnt) WHERE { ?s a <http://example.org/Person> }\"\n---\n{% for row in results %}{{ row.cnt + 10 }}{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    assert_eq!(
        out, "13",
        "COUNT(?s) over 3 Person instances (alice, bob, carol) + 10 must be 13 via real numeric addition: {out:?}"
    );
}

/// A `FILTER` clause correctly restricts the result set.
#[test]
fn filter_clause_restricts_result_set() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "filter.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?name WHERE { ?s <http://example.org/name> ?name . FILTER(?name = \\\"Alice\\\") }\"\n---\n{% for row in results %}{{ row.name }};{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    assert_eq!(out, "Alice;", "FILTER must restrict to only the matching row: {out:?}");
}

/// A `UNION` clause correctly combines two alternative patterns (`Person`
/// names and `Robot` names).
#[test]
fn union_clause_combines_alternative_patterns() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "union.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"SELECT ?name WHERE { { ?s a <http://example.org/Person> ; <http://example.org/name> ?name } UNION { ?s a <http://example.org/Robot> ; <http://example.org/name> ?name } } ORDER BY ?name\"\n---\n{% for row in results %}{{ row.name }};{% endfor %}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    assert_eq!(
        out, "Alice;Bob;Carol;Dave;",
        "UNION must combine both Person and Robot name matches: {out:?}"
    );
}

/// A `DESCRIBE` query, the first test coverage for this query form
/// (flagged as zero prior coverage by this task's investigation): the
/// `sparql:` frontmatter field accepts any query form (not just SELECT),
/// and DESCRIBE's result flows through `EngineQueryResults::Graph` the
/// same way CONSTRUCT does -- an array of `{subject, predicate, object}`
/// rows, non-empty for a resource that has outgoing triples.
#[test]
fn describe_query_returns_a_non_empty_triple_array() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    write_template(
        dir.path(),
        "describe.tmpl",
        "---\nto: out.txt\nsparql:\n  default: \"DESCRIBE <http://example.org/alice>\"\n---\n{{ results | length }}",
    );

    run_sync(dir.path());
    let out = std::fs::read_to_string(dir.path().join("out.txt")).expect("read output");
    let n: usize = out.trim().parse().expect("output must be a plain integer");
    assert!(
        n >= 2,
        "DESCRIBE <alice> must return at least alice's `a`/`name`/`age` triples, got {n}: {out:?}"
    );
}
