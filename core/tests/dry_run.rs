use core::engine::project;
use std::{collections::HashMap, fs};
use tempfile::tempdir;

#[test]
fn dry_run_writes_nothing_but_reports_keys() {
    let td = tempdir().unwrap();
    let root = td.path();

    fs::create_dir_all(root.join("templates/cli/sub")).unwrap();
    fs::write(
        root.join("templates/cli/sub/a.tmpl"),
        r#"---
to: out/a.txt
vars: { seed: s }
rdf: { inline: [] }
shape: { include: [] }
determinism: { seed: "{{ seed }}" }
---
A
"#,
    )
    .unwrap();

    let vars = HashMap::new();
    let r = project(root, "cli", "sub", vars, true).unwrap();
    assert_eq!(r.artifacts.len(), 1);
    assert!(!r.artifacts[0].key.is_empty());
    assert!(!root.join("out/a.txt").exists());
}
