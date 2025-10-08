use core::engine::project;
use std::fs;
use tempfile::tempdir;

#[test]
fn shacl_gate_passes_good_graph() {
    let td = tempdir().unwrap();
    let root = td.path();

    fs::create_dir_all(root.join("templates/x/y")).unwrap();
    fs::write(
        root.join("graphs/cli.ttl"),
        r#"@prefix cli: <urn:rgen:cli#> .
[] a cli:Command ; cli:slug "ok" ."#,
    )
    .unwrap();
    fs::write(
        root.join("shapes/cli.shacl.ttl"),
        r#"@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix cli: <urn:rgen:cli#> .
[] a sh:NodeShape ;
   sh:targetClass cli:Command ;
   sh:property [ sh:path cli:slug ; sh:minCount 1 ] ."#,
    )
    .unwrap();

    fs::write(
        root.join("templates/x/y/t.tmpl"),
        r#"---
to: out/{{ slug }}.txt
vars: { seed: s }
rdf: { include: ["graphs/cli.ttl"] }
shape: { include: ["shapes/cli.shacl.ttl"] }
sparql:
  vars:
    - name: slug
      query: |
        PREFIX cli: <urn:rgen:cli#>
        SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
determinism: { seed: "{{ seed }}" }
---
ok
"#,
    )
    .unwrap();

    let report = project(root, "x", "y", Default::default(), false).unwrap();
    assert_eq!(report.artifacts.len(), 1);
}
