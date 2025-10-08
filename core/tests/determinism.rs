use std::{collections::HashMap, fs, path::Path};
use tempfile::tempdir;

// Assumed API
use crate::engine::{project, RunReport};

fn write<P: AsRef<Path>>(p: P, s: &str) {
    fs::create_dir_all(p.as_ref().parent().unwrap()).unwrap();
    fs::write(p, s).unwrap();
}

#[test]
fn manifest_key_is_stable_and_idempotent() {
    let td = tempdir().unwrap();
    let root = td.path();

    // template: single render, no matrix
    write(
        root.join("templates/cli/subcommand/rust.tmpl"),
        r#"---
to: src/cmds/{{ slug }}.rs
vars: { cmd: hello, summary: "Print a greeting", seed: cosmos }
rdf:
  inline:
    - mediaType: text/turtle
      text: |
        @prefix cli: <urn:rgen:cli#> .
        [] a cli:Command ; cli:slug "{{ cmd }}" ; cli:summary "{{ summary }}" .
shape: { include: [] }
sparql:
  vars:
    - name: slug
      query: |
        PREFIX cli: <urn:rgen:cli#>
        SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
determinism: { seed: "{{ seed }}" }
---
/* {{ slug }} */ pub fn {{ slug }}(name:&str){ println!("hello {}", name); }
"#,
    );

    let mut vars = HashMap::new();
    vars.insert("cmd".into(), "hello".into());
    vars.insert("summary".into(), "Print a greeting".into());
    let r1: RunReport = project(root, "cli", "subcommand", vars.clone(), false).unwrap();
    assert_eq!(r1.artifacts.len(), 1);
    let k1 = r1.artifacts[0].key.clone();
    let bytes1 = fs::read(root.join("src/cmds/hello.rs")).unwrap();

    // second run should skip writes and keep key identical
    let r2: RunReport = project(root, "cli", "subcommand", vars, false).unwrap();
    assert_eq!(r2.artifacts.len(), 1);
    let k2 = r2.artifacts[0].key.clone();
    let bytes2 = fs::read(root.join("src/cmds/hello.rs")).unwrap();

    assert_eq!(k1, k2);
    assert_eq!(bytes1, bytes2);
}
