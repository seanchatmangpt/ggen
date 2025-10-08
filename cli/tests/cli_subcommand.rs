use assert_cmd::prelude::*;
use assert_fs::prelude::*;
use std::process::Command;

#[test]
fn cli_gen_creates_files() {
    let td = assert_fs::TempDir::new().unwrap();
    let root = td.path();

    let tdir = root.join("templates/cli/subcommand");
    std::fs::create_dir_all(&tdir).unwrap();
    std::fs::write(
        tdir.join("rust.tmpl"),
        r#"---
to: out/{{ slug }}.rs
vars: { cmd: hello, seed: s }
rdf:
  inline:
    - mediaType: text/turtle
      text: |
        @prefix cli: <urn:rgen:cli#> .
        [] a cli:Command ; cli:slug "{{ cmd }}" .
sparql:
  vars:
    - name: slug
      query: |
        PREFIX cli: <urn:rgen:cli#>
        SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
determinism: { seed: "{{ seed }}" }
---
pub fn {{ slug }}() {}
"#,
    )
    .unwrap();

    // Run `rgen gen cli subcommand --vars cmd=hello`
    let mut cmd = Command::cargo_bin("rgen").unwrap();
    cmd.current_dir(root)
        .arg("gen")
        .arg("cli")
        .arg("subcommand")
        .arg("--vars")
        .arg("cmd=hello");
    cmd.assert().success();

    assert!(root.join("out/hello.rs").exists());
}
