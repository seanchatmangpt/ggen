//! E_DOD9 + metamodel-hygiene audit over the repository's real pack
//! ontologies (`packs/*/ontology.ttl`), plus non-vacuity falsifiers: the same
//! gate must refuse a document that carries a DoD#9 collision, a hygiene
//! violation, an unpinned authority IRI, or a namespace leak.
//!
//! Chicago discipline: the corpus is the real files on disk, parsed with the
//! real praxis-graphlaw parser — no fixtures, no doubles.

use praxis_graphlaw::ggen_law::{
    check_hygiene, scan_dod9, LawTriple, AUTHORITY_ROOT, PACK_NS_PREFIX,
};
use praxis_graphlaw::ggen_law::PackIngest;
use praxis_graphlaw::parser::Parser;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::term::VarOrTerm;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("manifest dir has two ancestors to repo root")
        .to_path_buf()
}

/// Parse a real Turtle document into law triples (plain strings). Parse
/// failures are audit failures (fail-closed), never skipped documents.
fn parse_law_triples(src: &str) -> Vec<LawTriple> {
    let triples = Parser::parse_triples(src, Syntax::Turtle)
        .expect("real pack ontology must parse");
    triples
        .iter()
        .filter_map(|t| {
            let term_str = |v: &VarOrTerm| match v {
                VarOrTerm::Term(t) => Some(t.to_string()),
                VarOrTerm::Var(_) => None,
            };
            Some((
                term_str(&t.s)?,
                term_str(&t.p)?,
                term_str(&t.o)?,
            ))
        })
        .collect()
}

fn pack_ontologies() -> Vec<PathBuf> {
    let mut out = Vec::new();
    let packs_dir = repo_root().join("packs");
    let mut entries: Vec<PathBuf> = std::fs::read_dir(&packs_dir)
        .expect("packs/ directory exists")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.is_dir())
        .collect();
    entries.sort();
    for dir in entries {
        let ont = dir.join("ontology.ttl");
        if ont.is_file() {
            out.push(ont);
        }
    }
    out
}

#[test]
fn audit_all_pack_ontologies_dod9_and_hygiene() {
    let ontologies = pack_ontologies();
    assert!(
        ontologies.len() >= 50,
        "expected the full pack corpus, found only {}",
        ontologies.len()
    );
    let mut total_triples = 0usize;
    let mut audited: Vec<String> = Vec::new();
    for path in &ontologies {
        let src = std::fs::read_to_string(path).expect("read ontology.ttl");
        let triples = parse_law_triples(&src);
        let n = triples.len();
        total_triples += n;
        let rel = path
            .strip_prefix(&repo_root())
            .expect("path under repo root")
            .display()
            .to_string();
        check_hygiene(&triples)
            .unwrap_or_else(|e| panic!("{rel}: {e}"));
        scan_dod9(&triples).unwrap_or_else(|e| panic!("{rel}: {e}"));
        audited.push(format!("{rel}: {n} triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none"));
    }
    assert!(
        total_triples >= 1000,
        "corpus too small to be the real pack graph: {total_triples}"
        );
    println!("AUDIT-RECEIPT n_packs={} n_triples={total_triples}", audited.len());
    for line in &audited {
        println!("AUDIT-RECEIPT {line}");
    }
}

#[test]
fn falsifier_dod9_collision_is_refused() {
    let src = format!(
        r#"@prefix ex: <http://example.org/> .
@prefix ea: <{AUTHORITY_ROOT}> .
ex:thing a ea:Pack, ea:Sbb ."#
    );
    let triples = parse_law_triples(&src);
    let err = scan_dod9(&triples).expect_err("DoD9 collision must be refused");
    assert!(err.to_string().contains("E_DOD9_COLLISION"), "got: {err}");
}

#[test]
fn falsifier_hygiene_violation_is_refused() {
    let src = r#"@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> ."#
        .to_string()
        + &format!(
            "\nex:fake a owl:Class ; owl:equivalentClass <{}Pack> .\n",
            AUTHORITY_ROOT
        );
    let triples = parse_law_triples(&src);
    let err = check_hygiene(&triples).expect_err("external core-term definition must be refused");
    assert!(
        err.to_string()
            .starts_with("E_METAMODEL_HYGIENE_VIOLATION"),
        "got: {err}"
    );
}

#[test]
fn falsifier_unpinned_authority_iri_is_refused() {
    let src = r#"@prefix ex: <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> ."#
        .to_string()
        + "\nex:fake owl:equivalentClass <http://www.opengroup.org/togaf/ContentMetamodel> .\n";
    let triples = parse_law_triples(&src);
    let err = check_hygiene(&triples).expect_err("unpinned family IRI must be refused");
    assert!(
        err.to_string().starts_with("E_AUTHORITY_ROOT_UNPINNED"),
        "got: {err}"
    );
}

#[test]
fn falsifier_namespace_leak_is_refused() {
    let ing = PackIngest::new("demo").unwrap();
    let triples: Vec<LawTriple> = vec![(
        "<urn:ggen:pack:demo#R>".into(),
        "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".into(),
        format!("<{PACK_NS_PREFIX}demo#Internal>"),
    )];
    let err = ing.project(&triples).expect_err("leak must be refused");
    assert!(err.to_string().starts_with("E_NAMESPACE_LEAK"), "got: {err}");
}
