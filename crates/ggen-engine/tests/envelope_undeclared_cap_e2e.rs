//! E2E: the real `ggen` binary refuses fail-closed when a sync consumes a
//! bounded-resource cap that the envelope never declared — stderr carries the
//! typed `E-ENVELOPE-UNDECLARED` code and the exit status is non-zero, and
//! nothing is written to the project tree.
//!
//! Chicago school: real binary, real tempdir fixture pack, assertions on
//! process exit status + stderr + disk state. No mocks.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::similar_names
)]

use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "envelope-undeclared-cap"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

/// The fact graph references an envelope IRI via the envelope predicate but
/// never declares that IRI as a subject — the undeclared-cap subject the
/// SHACL-path gate (`law_engine::validate_shacl` ->
/// `envelope::undeclared_envelope`) must refuse with `E-ENVELOPE-UNDECLARED`.
const ONTOLOGY: &str = r"@prefix ex: <http://example.org/> .
@prefix env: <http://ggen.dev/vocab/envelope#> .
ex:subject a ex:Item .
ex:workload env:envelope <res:Ghost> .
";

const TEMPLATE: &str = "---\nto: out/result.txt\n---\nhello\n";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/main.tmpl"), TEMPLATE).expect("write template");
}

/// Resolve the compiled `ggen` binary (this test binary belongs to
/// `ggen-engine`, which builds no `ggen` binary of its own, so
/// `CARGO_BIN_EXE_ggen` is never set under `cargo test -p ggen-engine`).
/// Same resolution ladder as `cli_boundary.rs`: env var, then workspace-root
/// `target/{debug,release}/ggen`, then a version-guarded PATH probe.
fn ggen_bin() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .or_else(|| {
            let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
            let mut dir: &Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
        }
    }

    if let Some(path_var) = std::env::var_os("PATH") {
        for dir in std::env::split_paths(&path_var) {
            let candidate = dir.join("ggen");
            if candidate.is_file() {
                return candidate;
            }
        }
    }

    panic!("could not resolve the `ggen` binary: run `cargo build -p ggen` first");
}

// Lane I1 retirement: the interim `<root>/.ggen-v2/envelope.json` sidecar is
// gone — the RDF reticle (`.specify/envelope-shapes.ttl`) is the sole cap
// source of truth. This test's refusal path never needed the sidecar: the
// undeclared `res:Ghost` reference in the ONTOLOGY drives the typed
// `E-ENVELOPE-UNDECLARED` SHACL refusal.

#[test]
fn cli_refuses_undeclared_cap_with_typed_stderr_and_nonzero_exit() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);

    let out = Command::new(ggen_bin())
        .args(["sync", "run"])
        .current_dir(root)
        .output()
        .expect("ggen binary must execute");

    assert!(
        !out.status.success(),
        "undeclared-cap sync must exit non-zero, got {:?}",
        out.status
    );

    let stderr = String::from_utf8_lossy(&out.stderr);
    let stdout = String::from_utf8_lossy(&out.stdout);
    let combined = format!("{stderr}{stdout}");
    assert!(
        combined.contains("E-ENVELOPE-UNDECLARED"),
        "expected typed E-ENVELOPE-UNDECLARED on stderr, got stderr={stderr:?} stdout={stdout:?}"
    );

    // Fail-closed: the refused sync wrote nothing into the project tree.
    assert!(
        !root.join("out/result.txt").exists(),
        "refused sync must write nothing to disk"
    );
    assert!(
        !root.join("out").exists(),
        "refused sync must not even create the output directory"
    );
}
