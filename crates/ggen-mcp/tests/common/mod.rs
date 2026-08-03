//! Shared real-project fixtures. Chicago TDD: every fixture is a real
//! directory with a real `ggen.toml`, a real Turtle ontology, and real
//! template files on disk. No mocks, no in-memory graph doubles.
//!
//! `dead_code` is allowed because each integration-test binary compiles
//! this module independently, so any fixture not used by *that particular*
//! binary looks unused to it.
#![allow(dead_code)]

use std::path::Path;

/// Frontmatter-schema project (`[templates].dir`, per-file frontmatter).
pub const FRONTMATTER_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

/// `ex:hasName` is used; `ex:hasX` is used ZERO times anywhere. A mandatory
/// (non-OPTIONAL) pattern on `ex:hasX` is the exact shape of the verified
/// incident this crate was built to make loud.
pub const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:hasName "alice" .
ex:bob ex:hasName "bob" .
"#;

/// A template whose SELECT binds `?name` and whose body consumes it --
/// the well-formed baseline.
pub const GOOD_TEMPLATE: &str = "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/hasName> ?name } ORDER BY ?name\n---\n{% for row in people %}{{ row.name }}\n{% endfor %}";

/// Body iterates `{% for row in nosuchquery %}` -- a ROOT identifier the
/// frontmatter never projects. Note the engine's var-diff works on root
/// identifiers, not on field accesses of a loop-local (`row.missing` is
/// deliberately out of scope: `row` is a local, and its columns are not
/// statically known), so an unbound ROOT is the real, checkable case.
pub const UNBOUND_VAR_TEMPLATE: &str = "---\nto: out/bad.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/hasName> ?name } ORDER BY ?name\n---\n{% for row in nosuchquery %}{{ row.name }}\n{% endfor %}";

/// Jinja2 inline-ternary syntax, which Tera has no equivalent for -- the
/// second verified incident. Must be reported as a parse failure, not
/// silently accepted.
pub const JINJA_TERNARY_TEMPLATE: &str = "---\nto: out/ternary.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/hasName> ?name }\n---\n{{ \"yes\" if true else \"no\" }}\n";

/// Write a frontmatter-schema project into `root`.
pub fn write_frontmatter_project(root: &Path) {
    std::fs::write(root.join("ggen.toml"), FRONTMATTER_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology.ttl");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/names.tmpl"), GOOD_TEMPLATE).expect("write template");
}

/// Declarative-rules-schema project (`[[generation.rules]]`), including one
/// rule that uses the inert `template.pack` source so
/// `ggen_capability_status` has something real to detect.
pub fn write_declarative_project(root: &Path) {
    let toml = r#"
[project]
name = "declarative-demo"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[[generation.rules]]
name = "names"
query = { inline = "SELECT ?name WHERE { ?s <http://example.org/hasName> ?name } ORDER BY ?name" }
template = { inline = "{% for row in results %}{{ row.name }}\n{% endfor %}" }
output_file = "out/names.txt"

[[generation.rules]]
name = "from-pack"
query = { inline = "SELECT ?name WHERE { ?s <http://example.org/hasName> ?name }" }
template = { pack = "some-pack", file = "tpl.tera" }
output_file = "out/packed.txt"
"#;
    std::fs::write(root.join("ggen.toml"), toml).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology.ttl");
}
