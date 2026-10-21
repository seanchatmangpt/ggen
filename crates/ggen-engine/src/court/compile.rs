//! Court artifact compilation: catalog → (runner.sh, `register_row.sh`,
//! fixture.rs, shim.patch) + [`CourtManifest`].
//!
//! Rendering is deterministic: the only variable inputs are the catalog
//! fields themselves (timestamps included) — no wall-clock, no ambient
//! state. A given catalog compiles byte-identical artifacts on every run,
//! which is what the KAT test pins.

use std::path::Path;

use tera::Tera;

use crate::court::{CourtError, CourtResult, CourtVerdict, MutantCatalog};

const RUNNER_TEMPLATE: &str = include_str!("../../templates/court/runner.sh.tera");
const REGISTER_ROW_TEMPLATE: &str = include_str!("../../templates/court/register_row.sh.tera");
const FIXTURE_TEMPLATE: &str = include_str!("../../templates/court/fixture.rs.tera");

/// Everything [`compile_court`] produced, with digests for the register.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CourtManifest {
    /// The catalog the artifacts were compiled from.
    pub catalog_gate_id: String,
    /// The court verdict over the witness/shim ordering.
    pub verdict: CourtVerdict,
    /// File name → SHA-256 hex of every emitted artifact, sorted by name.
    pub artifacts: Vec<(String, String)>,
}

fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::Digest as _;
    hex::encode(sha2::Sha256::digest(bytes))
}

fn render(tera: &Tera, name: &str, context: &tera::Context) -> CourtResult<String> {
    tera.render(name, context)
        .map_err(|e| CourtError::TemplateRender(format!("court template render failed: {e}")))
}

/// Shell-quote sanitization for values interpolated inside double-quoted
/// shell strings: a double quote in catalog data must never terminate the
/// assignment (fail-closed against injection through catalog text).
fn shell_safe(value: &str) -> String {
    let mut out = String::with_capacity(value.len());
    for c in value.chars() {
        match c {
            '"' | '`' => out.push('\''),
            '$' => {}
            c => out.push(c),
        }
    }
    out
}

/// The court verdict over the KD-COVERS flip: a witness receipt may
/// authorize a flip only when `witness_ts >= shim_ts` (witness postdates
/// the shim it claims to witness). Missing timestamps = Unknown (loud
/// SKIP), a stale witness = Refused (`WITNESS_STALE`), otherwise Pass.
#[must_use]
pub fn flip_verdict(catalog: &MutantCatalog) -> CourtVerdict {
    match (catalog.witness_ts, catalog.shim_ts) {
        (Some(witness), Some(shim)) if witness >= shim => CourtVerdict::Pass {
            witness_ts: witness,
            shim_ts: shim,
        },
        (Some(witness), Some(shim)) => CourtVerdict::Refused {
            code: "WITNESS_STALE".to_string(),
            target: Some(catalog.target.clone()),
            detail: format!(
                "witness_ts ({witness}) predates shim_ts ({shim}) — a witness \
                 receipt older than the shim it witnesses can never authorize \
                 the KD-COVERS flip"
            ),
        },
        (witness, shim) => CourtVerdict::Unknown {
            reason: format!(
                "flip unverifiable: witness_ts={witness:?}, shim_ts={shim:?} — \
                 at least one timestamp absent from the catalog"
            ),
        },
    }
}

/// Compile the court artifacts for `catalog` into `out_dir`.
///
/// Emits `runner.sh`, `register_row.sh`, `fixture.rs`, and `shim.patch`,
/// returning the manifest with per-artifact SHA-256 digests. Compilation
/// itself never fails on a non-Pass verdict — the artifacts are still
/// produced (the runner and register row carry the refusal), and the
/// verdict's [`CourtVerdict::exit_code`] carries the typed exit.
///
/// # Errors
/// [`CourtError::Io`] on output-dir creation/write failures;
/// [`CourtError::TemplateRender`] on template/context divergence.
pub fn compile_court(catalog: &MutantCatalog, out_dir: &Path) -> CourtResult<CourtManifest> {
    std::fs::create_dir_all(out_dir).map_err(|e| CourtError::Io {
        path: out_dir.display().to_string(),
        detail: e.to_string(),
    })?;

    let verdict = flip_verdict(catalog);
    let (status, code, detail) = match &verdict {
        CourtVerdict::Pass { .. } => ("PASS".to_string(), String::new(), String::new()),
        CourtVerdict::Refused { code, detail, .. } => {
            ("REFUSED".to_string(), code.clone(), shell_safe(detail))
        }
        CourtVerdict::Unknown { reason } => {
            ("UNKNOWN".to_string(), String::new(), shell_safe(reason))
        }
    };
    let shim_ts = catalog
        .shim_ts
        .map_or_else(|| "0".to_string(), |t| t.to_string());
    let witness_ts = catalog
        .witness_ts
        .map_or_else(|| "0".to_string(), |t| t.to_string());

    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("runner", RUNNER_TEMPLATE),
        ("register_row", REGISTER_ROW_TEMPLATE),
        ("fixture", FIXTURE_TEMPLATE),
    ])
    .map_err(|e| CourtError::TemplateRender(format!("court templates failed to parse: {e}")))?;

    let context = tera::Context::from_serialize(serde_json::json!({
        "gate_id": shell_safe(&catalog.gate_id),
        "target": shell_safe(&catalog.target),
        "mutant_kind": shell_safe(&catalog.mutant_kind),
        "expected_post_hash": shell_safe(&catalog.expected_post_hash),
        "status": status,
        "code": code,
        "detail": detail,
        "shim_ts": shim_ts,
        "witness_ts": witness_ts,
    }))
    .map_err(|e| CourtError::TemplateRender(format!("court context build failed: {e}")))?;

    let runner = render(&tera, "runner", &context)?;
    let register_row = render(&tera, "register_row", &context)?;
    let fixture = render(&tera, "fixture", &context)?;

    let mut artifacts: Vec<(String, Vec<u8>)> = vec![
        ("runner.sh".to_string(), runner.into_bytes()),
        ("register_row.sh".to_string(), register_row.into_bytes()),
        ("fixture.rs".to_string(), fixture.into_bytes()),
        (
            "shim.patch".to_string(),
            catalog.shim_patch.clone().into_bytes(),
        ),
    ];

    for (name, bytes) in &artifacts {
        let path = out_dir.join(name);
        std::fs::write(&path, bytes).map_err(|e| CourtError::Io {
            path: path.display().to_string(),
            detail: e.to_string(),
        })?;
    }

    artifacts.sort_by(|a, b| a.0.cmp(&b.0));
    Ok(CourtManifest {
        catalog_gate_id: catalog.gate_id.clone(),
        verdict,
        artifacts: artifacts
            .into_iter()
            .map(|(name, bytes)| (name, sha256_hex(&bytes)))
            .collect(),
    })
}

/// Library seam for the coordinator's cargo-mutants recipes: read a
/// catalog file and compile its court in one call.
///
/// # Errors
/// Propagates [`crate::court::catalog::read_mutant_catalog`] and
/// [`compile_court`] errors.
pub fn compile_court_from_catalog_file(
    catalog_path: &Path, out_dir: &Path,
) -> CourtResult<CourtManifest> {
    let catalog = crate::court::catalog::read_mutant_catalog(catalog_path)?;
    compile_court(&catalog, out_dir)
}
