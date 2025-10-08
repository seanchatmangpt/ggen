use core::engine::project;
use std::{collections::HashMap, fs};
use tempfile::tempdir;

#[test]
fn cli_overrides_sparql_overrides_defaults() {
    let td = tempdir().unwrap();
    let root = td.path();
    // defaults set cmd=foo, SPARQL yields cmd=bar, CLI sets cmd=baz
    fs::create_dir_all(root.join("templates/cli/subcommand")).unwrap();
    fs::write(
        root.join("templates/cli/subcommand/rust.tmpl"),
        r#"---
to: out/{{ cmd }}.txt
vars: { cmd: foo }
rdf:
  inline:
    - mediaType: text/turtle
      text: |
        @prefix cli: <urn:rgen:cli#> .
        [] a cli:Command ; cli:slug "bar" .
sparql:
  vars:
    - name: cmd
      query: |
        PREFIX cli: <urn:rgen:cli#>
        SELECT ?cmd WHERE { ?c a cli:Command ; cli:slug ?cmd } LIMIT 1
determinism: { seed: "s" }
---
{{ cmd }}
"#,
    )
    .unwrap();

    let mut vars = HashMap::new();
    vars.insert("cmd".into(), "baz".into());
    project(root, "cli", "subcommand", vars, false).unwrap();

    let rendered = fs::read_to_string(root.join("out/baz.txt")).unwrap();
    assert!(rendered.trim() == "baz");
}
