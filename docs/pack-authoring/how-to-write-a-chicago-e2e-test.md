# How to: write a Chicago-TDD e2e test for your pack

ggen's own convention (`crates/ggen-engine/tests/*_pack_e2e.rs`, one file
per pack) is real filesystem, real `sync()`, real assertions on real file
content — no mocks, no test doubles, matching this repo's Chicago TDD
policy. `crates/ggen-engine/tests/support/mod.rs` gives you the four
building blocks so you don't hand-roll scaffolding per pack.

## The four-part pattern

```rust
mod support;
use support::{scaffold_pack, assert_idempotent, assert_gate_refuses};
use std::path::{Path, PathBuf};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

#[test]
fn hello_pack_generates_and_is_idempotent() {
    // 1. SCAFFOLD: copy the real pack + a minimal consumer into a TempDir.
    let (_dir, project) = scaffold_pack(&packs_dir().join("hello-pack"));

    // 2. GENERATE + ASSERT REAL CONTENT: run the real sync(), then read the
    //    real file it wrote and assert on its exact content — not just
    //    that it exists.
    ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions {
        dry_run: false, ..Default::default()
    }).expect("sync");
    let greeting = std::fs::read_to_string(project.join("output/greeting.txt"))
        .expect("greeting.txt");
    assert_eq!(greeting.trim(), "Hello from ggen.");

    // 3. IDEMPOTENCY: a second sync must write nothing and leave ggen.lock
    //    byte-identical.
    assert_idempotent(&project);
}

#[test]
fn hello_pack_gate_refuses_missing_text() {
    let (_dir, project) = scaffold_pack(&packs_dir().join("hello-pack"));
    ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions {
        dry_run: false, ..Default::default()
    }).expect("baseline sync");

    // 4. GATE SABOTAGE: overwrite the CONSUMER's ontology.ttl with a bad
    //    fact and assert the sync refuses, citing the gate by name, with
    //    no partial ggen.lock left behind.
    assert_gate_refuses(
        &project,
        "@prefix hp: <http://example.org/hello-pack#> .\n\
         hp:Sabotage a hp:GreetingClass .\n",
        "010_required",
    );
}
```

## Why all four steps, not fewer

- Skip (2)'s content assertion and a template that silently renders empty
  still "passes" a bare existence check — assert the string, not just the
  file.
- Skip (3) and a template that regenerates differently every run (e.g. an
  unstable iteration order) goes undetected until a consumer's CI flags
  unexpected diffs.
- Skip (4) and a gate that never actually fires (a typo'd predicate name, a
  `FILTER` that's always false) sits in the repo as decoration, not
  admission control — see `.claude/rules/coding-agent-mistakes.md`'s
  "Decorative Completion" mistake class.
