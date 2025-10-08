use core::engine::project;
use std::{collections::HashMap, fs};
use tempfile::tempdir;

#[test]
fn matrix_rows_with_order_by_render_in_stable_order() {
    let td = tempdir().unwrap();
    let root = td.path();

    fs::create_dir_all(root.join("templates/cli/subcommand")).unwrap();
    fs::write(
        root.join("templates/cli/subcommand/bash.tmpl"),
        r#"---
to: out/{{ slug }}.sh
vars: { seed: cosmos }
rdf:
  inline:
    - mediaType: text/turtle
      text: |
        @prefix cli: <urn:rgen:cli#> .
        _:a a cli:Command ; cli:slug "alpha" .
        _:b a cli:Command ; cli:slug "beta" .
sparql:
  matrix:
    query: |
      PREFIX cli: <urn:rgen:cli#>
      SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } ORDER BY ?slug
    bind: { slug: "?slug" }
determinism: { seed: "{{ seed }}", sort: "slug" }
---
# {{ slug }}
"#,
    )
    .unwrap();

    let vars = HashMap::new();
    let report = project(root, "cli", "subcommand", vars, false).unwrap();
    let paths: Vec<_> = report
        .artifacts
        .iter()
        .map(|a| a.out_path.clone())
        .collect();
    assert_eq!(
        paths,
        vec!["out/alpha.sh".to_string(), "out/beta.sh".to_string()]
    );
}
