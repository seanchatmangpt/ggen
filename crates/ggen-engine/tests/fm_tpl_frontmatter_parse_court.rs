//! Court: frontmatter delimiter refusal codes — `[FM-TPL-001]` (no `---`
//! block at all), `[FM-TPL-012]` (opening `---` not on its own line),
//! `[FM-TPL-013]` (unterminated frontmatter).
//!
//! Chicago: real temp-dir files, the real parser `ggen_engine::template::
//! Template::parse` — the same parser the sync pipeline drives via
//! `sync.rs::parse_template_file` (which wraps these inner codes as
//! `[FM-TPL-006]` at sync time; that wrapper is courts/`fm_tpl_006_sync_court.rs`).
//! Emission site: `crates/ggen-engine/src/template.rs` `Template::parse`.
//! Closes GAP-3 for FM-TPL-001/012/013 (CARRY-09, zd-48).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_engine::template::Template;
use tempfile::TempDir;

fn write_and_parse(
    dir: &TempDir, name: &str, content: &str,
) -> ggen_engine::error::Result<Template> {
    let path = dir.path().join(name);
    std::fs::write(&path, content).expect("write template file");
    let text = std::fs::read_to_string(&path).expect("read template file back");
    Template::parse(&text)
}

#[test]
fn template_without_frontmatter_refuses_with_fm_tpl_001() {
    let dir = TempDir::new().expect("tempdir");
    let err = write_and_parse(&dir, "no_frontmatter.tmpl", "just a body, no delimiters\n")
        .expect_err("missing frontmatter block must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-001"), "{msg}");
    assert!(msg.contains("---"), "{msg}");
}

#[test]
fn delimiter_with_trailing_content_refuses_with_fm_tpl_012() {
    let dir = TempDir::new().expect("tempdir");
    let err = write_and_parse(
        &dir,
        "glued_delimiter.tmpl",
        "---to: out/x.txt\n---\nbody\n",
    )
    .expect_err("`---` glued to content must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-012"), "{msg}");
    assert!(msg.contains("own line"), "{msg}");
}

#[test]
fn unterminated_frontmatter_refuses_with_fm_tpl_013() {
    let dir = TempDir::new().expect("tempdir");
    let err = write_and_parse(
        &dir,
        "unterminated.tmpl",
        "---\nto: out/x.txt\nbody without a closing delimiter\n",
    )
    .expect_err("missing closing `---` must refuse");
    let msg = err.to_string();
    assert!(msg.contains("FM-TPL-013"), "{msg}");
    assert!(msg.contains("closing `---`"), "{msg}");
}
