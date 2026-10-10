//! Cross-pack Tera macro imports (`<pack-name>://<subpath>` template URIs).
//!
//! Chicago: a real project tree on disk (TempDir), a real pack resolved via
//! [`ggen_engine::pack::resolve`], and real Tera renders asserting on
//! rendered bytes — no mocks.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use ggen_engine::graph::DeterministicGraph;
use ggen_engine::pack::{self, Pack};
use ggen_engine::template::{build_tera, build_tera_with_packs, resolve_pack_template};
use ggen_engine::{config::GgenConfig, graph::GraphEngine};

/// Build a minimal but fully real project: `ggen.toml` + `ontology.ttl` +
/// `templates/`, with one real local pack under `packs/demolib` carrying a
/// macro file and one frontmatter template (pack resolution requires at
/// least one `templates/*.tmpl`).
fn fixture(root: &Path) {
    std::fs::create_dir_all(root.join("templates")).expect("mkdir project templates");
    std::fs::create_dir_all(root.join("packs/demolib/templates/macros"))
        .expect("mkdir pack templates");
    std::fs::write(
        root.join("ggen.toml"),
        r#"[project]
name = "cross-pack-fixture"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[packs.demolib]
path = "packs/demolib"
"#,
    )
    .expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), "").expect("write ontology");
    std::fs::write(
        root.join("packs/demolib/pack.toml"),
        "[pack]\nname = \"demolib\"\nversion = \"1.0.0\"\ndescription = \"test pack\"\n",
    )
    .expect("write pack.toml");
    std::fs::write(root.join("packs/demolib/ontology.ttl"), "").expect("write pack ontology");
    // A pure Tera macro file (no frontmatter) — what `{% import %}` targets.
    std::fs::write(
        root.join("packs/demolib/templates/macros/util.tera"),
        "{% macro greet(name) %}Hello {{ name }}{% endmacro %}\n",
    )
    .expect("write pack macro file");
    // Pack resolution requires at least one `templates/*.tmpl`.
    std::fs::write(
        root.join("packs/demolib/templates/gen.rs.tmpl"),
        "---\nto: src/gen.rs\n---\n// pack template\n",
    )
    .expect("write pack tmpl");
    // The project template that imports the pack macro across packs.
    std::fs::write(
        root.join("templates/main.tmpl"),
        "---\nto: out/main.rs\n---\n{% import \"demolib://macros/util.tera\" as u %}\
         {{ u::greet(name=\"World\") }}\n",
    )
    .expect("write main.tmpl");
    // A plain, no-import template used for the byte-identical drift check.
    std::fs::write(
        root.join("templates/plain.tmpl"),
        "---\nto: out/plain.txt\n---\nplain:{{ v }}\n",
    )
    .expect("write plain.tmpl");
}

fn resolved_packs(root: &Path) -> (GgenConfig, Vec<Pack>) {
    let raw = std::fs::read_to_string(root.join("ggen.toml")).expect("read ggen.toml");
    let config: GgenConfig = star_toml::from_str(&raw).expect("parse ggen.toml");
    let packs = pack::resolve(&config, root).expect("resolve packs");
    (config, packs)
}

fn graph() -> Arc<dyn GraphEngine> {
    let g = DeterministicGraph::new().expect("graph");
    Arc::new(g)
}

/// Serialize this binary's tests: `set_current_dir` is process-global and a
/// sibling's chdir (or its tempdir teardown deleting the cwd) corrupts the
/// cwd that concurrent subprocesses inherit — observed as
/// `[FM-CHAIN-018] rustc toolchain identity command exited Some(1)` (the
/// rustup shim refuses to run with a deleted cwd). Each test binds the
/// returned guard for its whole body.
fn serial(root: &Path) -> std::sync::MutexGuard<'static, ()> {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    let guard = LOCK.lock().unwrap_or_else(|e| e.into_inner());
    std::env::set_current_dir(root).expect("chdir into fixture");
    guard
}

#[test]
fn cross_pack_macro_import_renders_through_pack_uri() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fixture(root);
    let _serial = serial(root);
    let (_config, packs) = resolved_packs(root);
    assert_eq!(packs.len(), 1, "one resolved pack");
    assert_eq!(packs[0].name, "demolib");

    let mut tera = build_tera_with_packs(graph(), &packs).expect("build tera with packs");
    // The pack template is registered under its URI name.
    assert!(
        tera.get_template("demolib://macros/util.tera").is_ok(),
        "pack template must be registered under its URI name"
    );
    // Real render of the importing project template body: the pack macro
    // actually executes.
    let body = std::fs::read_to_string(root.join("templates/main.tmpl")).expect("read main");
    let tpl = ggen_engine::template::Template::parse(&body).expect("parse main.tmpl");
    let rendered = tera
        .render_str(&tpl.body, &tera::Context::new())
        .expect("render must succeed");
    assert_eq!(rendered.trim(), "Hello World", "macro output proves execution");
}

#[test]
fn zero_drift_plain_template_renders_byte_identically_with_and_without_packs() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fixture(root);
    let _serial = serial(root);
    let (_config, packs) = resolved_packs(root);

    let body = std::fs::read_to_string(root.join("templates/plain.tmpl")).expect("read plain");
    let tpl = ggen_engine::template::Template::parse(&body).expect("parse plain.tmpl");
    let mut ctx = tera::Context::new();
    ctx.insert("v", "ok");

    let without = build_tera(graph())
        .expect("build_tera (legacy path)")
        .render_str(&tpl.body, &ctx)
        .expect("render without packs");
    let without_explicit_empty =
        build_tera_with_packs(graph(), &[]).expect("empty pack slice").render_str(
            &tpl.body,
            &ctx,
        )
        .expect("render with empty packs");
    let with = build_tera_with_packs(graph(), &packs)
        .expect("build with packs")
        .render_str(&tpl.body, &ctx)
        .expect("render with packs");

    assert_eq!(without, "plain:ok\n");
    assert_eq!(without, without_explicit_empty);
    assert_eq!(
        without, with,
        "packs present but unused must not change rendering one byte"
    );
}

#[test]
fn unknown_pack_uri_refuses_closed_at_render_and_in_resolver() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    fixture(root);
    let _serial = serial(root);
    let (_config, packs) = resolved_packs(root);

    let mut tera = build_tera_with_packs(graph(), &packs).expect("build tera with packs");
    let err = tera
        .render_str(
            "{% import \"ghost://macros/util.tera\" as g %}{{ g::greet(name=\"x\") }}",
            &tera::Context::new(),
        )
        .expect_err("unknown pack URI must fail closed at render");
    assert!(
        format!("{err:?}").contains("ghost://macros/util.tera"),
        "render refusal must name the URI: {err:?}"
    );

    let err = resolve_pack_template("ghost://macros/util.tera", &packs)
        .expect_err("unknown pack must refuse in the resolver");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-028"), "typed code: {msg}");
    assert!(msg.contains("ghost://macros/util.tera"), "names URI: {msg}");
    assert!(msg.contains("ghost"), "names the pack: {msg}");
    assert!(msg.contains("demolib"), "lists resolved packs: {msg}");
}

#[test]
fn known_pack_missing_subpath_refuses_closed_at_render_and_in_resolver() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    fixture(root);
    let _serial = serial(root);
    let (_config, packs) = resolved_packs(root);

    let mut tera = build_tera_with_packs(graph(), &packs).expect("build tera with packs");
    let err = tera
        .render_str(
            "{% import \"demolib://macros/nope.tera\" as g %}{{ g::greet(name=\"x\") }}",
            &tera::Context::new(),
        )
        .expect_err("missing subpath must fail closed at render");
    assert!(
        format!("{err:?}").contains("demolib://macros/nope.tera"),
        "render refusal must name the URI: {err:?}"
    );

    let err = resolve_pack_template("demolib://macros/nope.tera", &packs)
        .expect_err("missing subpath must refuse in the resolver");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-028"), "typed code: {msg}");
    assert!(
        msg.contains("demolib://macros/nope.tera"),
        "names URI: {msg}"
    );
    let searched = root.join("packs/demolib/templates/macros/nope.tera");
    assert!(
        msg.contains(&searched.display().to_string()),
        "names the searched path {}: {msg}",
        searched.display()
    );
}

#[test]
fn traversal_and_malformed_uris_refuse_closed_in_resolver() {
    let dir = tempfile::tempdir().expect("tempdir");
    let root = dir.path();
    fixture(root);
    let (_config, packs) = resolved_packs(root);
    let _serial = serial(root);

    let err = resolve_pack_template("demolib://../../etc/passwd", &packs)
        .expect_err("`..` traversal must refuse");
    assert!(err.to_string().contains("FM-TPL-028"), "{err}");
    assert!(err.to_string().contains("traversal"), "{err}");

    let err = resolve_pack_template("not-a-uri", &packs).expect_err("malformed must refuse");
    assert!(err.to_string().contains("FM-TPL-028"), "{err}");
    assert!(err.to_string().contains("not-a-uri"), "{err}");

    // Resolver success path agrees with the eager registration's source.
    let path: PathBuf =
        resolve_pack_template("demolib://macros/util.tera", &packs).expect("resolve");
    assert_eq!(
        path,
        root.join("packs/demolib/templates/macros/util.tera"),
        "URI resolves to <pack.root>/templates/<subpath>"
    );
}

/// Full-pipeline Chicago test: the same real fixture, synced end to end
/// through [`ggen_engine::sync::sync`] (not just `build_tera_with_packs`).
/// The project template imports `demolib://macros/util.tera` and the rendered
/// file on disk must contain the macro's contribution — proving the sync
/// pipeline's `build_tera_with_packs` wiring, not merely the template layer.
#[test]
fn full_sync_renders_cross_pack_macro_import() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fixture(root);
    // Holds the process-wide serial lock (and chdirs into the fixture) so
    // sibling tests cannot chdir the cwd out from under sync's `rustc`
    // subprocess. See [`serial`].
    let _serial = serial(root);
    // NOTE: serialized via [`serial`] — `set_current_dir` is process-global and the
    // sibling tests above chdir in parallel; racing it corrupts the cwd a
    // concurrent test's `rustc` subprocess inherits. `sync` takes an
    // explicit root and pack paths resolve absolutely, so chdir is not
    // needed on this path.
    // `plain.tmpl` references `{{ v }}`, which only the direct-render tests
    // above supply; the sync pipeline renders with an empty context, so
    // remove it here — it is covered by the byte-identical test above.
    std::fs::remove_file(root.join("templates/plain.tmpl")).expect("remove plain.tmpl");

    let report = ggen_engine::sync::sync(
        root,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("real full sync");

    let out = std::fs::read_to_string(root.join("out/main.rs")).expect("read synced output");
    assert!(
        out.contains("Hello World"),
        "synced output must contain the cross-pack macro's contribution: {out:?}"
    );
    assert!(
        report.written.iter().any(|p| p == &std::path::PathBuf::from("out/main.rs")),
        "receipt must name the synced output; got {:?}",
        report.written
    );
}
