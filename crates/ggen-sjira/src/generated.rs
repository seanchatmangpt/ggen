//! The projection surface (PG-2 Slice 6): one work order -> one capability
//! row-set -> one rendered artifact, written through a guarded write.
//!
//! RE-SCOPE, not a 1:1 port, of
//! `ggen_igniter/lib/ggen_igniter/semantic_jira/projection.ex` (390L). The
//! Elixir module is IO-heavy through collaborators with no ggen-sjira
//! counterparts (`PackLock`, `Reconcile`, `Actuate`, `GateVerify`); each hop
//! is re-implemented here at its minimal lawful form:
//!
//! - **Pack pinning** (`PackLock.check/2` + `PackLock.digest_checked/1` in
//!   the Elixir): this crate has no pack.lock; the pack is pinned by a
//!   deterministic digest over the pack tree — sorted relative paths, each
//!   file's bytes — via [`crate::work_order::digest_of`]. A staging copy is
//!   what the pipeline renders against and mutates (projected falsifier
//!   gates land there), so the digest-pinned original is never written into;
//!   the receipt's `pack_digest` is always computed over the ORIGINAL pack
//!   dir, never the staging copy (projection.ex moduledoc step 5's law).
//! - **Scope query** (`Reconcile`'s `capability=<path>` named binding): the
//!   capability's row query must exist at `<pack>/projections/<id>.rq`; a
//!   missing scope query refuses
//!   [`ProjectionRefusal::CapabilityUnknown`] (projection.ex step 2).
//! - **Falsifier -> gate projection** (`GateVerify.verify_unbound/2`): each
//!   order falsifier becomes a gate at
//!   `verify/<capability>-falsifier-<n>.unbound.rq` in the STAGING copy,
//!   header-annotated with its falsifier URN. A capability with falsifiers
//!   but no `<pack>/projections/<id>.unbound.rq` refuses
//!   [`ProjectionRefusal::FalsifierGatesMissing`] — a falsifier with no gate
//!   is a court that cannot fire (`admission_vacuous`, projection.ex step 5).
//! - **Render**: the scope query is executed over the pack's `ontology.ttl`
//!   with the real Oxigraph engine (the same engine under ggen-graph; see the
//!   crate Cargo.toml disclosure), rows are serialized to deterministic JSON
//!   (sorted rows, sorted keys), and rendered through a **minimal Tera
//!   one-off bind** — context `{capability_id, rows}`. Disclosed divergence
//!   from the Elixir: ggen_igniter renders the pack's full template files via
//!   `Reconcile` (frontmatter, inject, per-row iteration); this slice binds
//!   one Tera template string per project call (opts.template) against the
//!   whole row-set. Pack template files are not yet consumed — follow-on.
//! - **Guarded write** (`Actuate.write_file!/3`): re-implemented at the
//!   decision-table shape of `ggen-engine/src/write.rs`'s [`plan_write`]
//!   default arm (decision order: path-escape refusal, `unless_exists` skip,
//!   absent -> [`WriteOutcome::Written`], byte-identical ->
//!   [`WriteOutcome::Unchanged`], differs -> refuse — the silent-clobber
//!   refusal FM-WRITE-005's law, cited from write.rs lines 201-209). The
//!   igniter moduledoc law (projection.ex step 3): the guarded write IS the
//!   idempotency surface.
//! - **Post-render gate census** (`GateVerify.run/3`): every
//!   `<staging>/gates/*.rq` must return >= 1 row over the staging ontology,
//!   and every `<staging>/verify/*.unbound.rq` (INCLUDING the projected
//!   falsifier gates) must return 0 rows. Any failure refuses
//!   [`ProjectionRefusal::VerificationFailed`], leaving the actuated file on
//!   disk with standing NOT advanced (projection.ex step 4's fail-closed
//!   honesty).
//!
//! Ordering mirrors the Elixir: scope -> falsifier gates -> render -> digest
//! -> scoped path -> write -> gate census. `#![forbid(unsafe_code)]` holds
//! crate-wide; this module performs real filesystem IO by design (staging
//! copy, guarded write) — the crate's "IO-free" law in lib.rs applies to the
//! kernel modules, and this module is the designated IO surface.
//!
//! [`plan_write`]: ../ggen_engine/write/fn.plan_write.html

use std::collections::BTreeMap;
use std::path::{Component, Path, PathBuf};

use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::work_order::{digest_of, WorkOrder};

/// Outcome of the guarded write — the Elixir `:written | :unchanged |
/// :skipped_exists` tri-state (`Actuate.write_file!/3`'s outcome type).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WriteOutcome {
    /// File was absent (or `force`-like overwrite was permitted) and the
    /// rendered bytes were written.
    Written,
    /// Target existed with byte-identical content; nothing written.
    Unchanged,
    /// `unless_exists` was set and the target already existed, regardless of
    /// content (write.rs decision-table arm 2).
    SkippedExists,
}

impl std::fmt::Display for WriteOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            WriteOutcome::Written => "written",
            WriteOutcome::Unchanged => "unchanged",
            WriteOutcome::SkippedExists => "skipped_exists",
        })
    }
}

/// One gate-census result: the gate file name and its pass/fail status.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GateResult {
    /// Gate file name (e.g. `gates/rows-exist.rq` or
    /// `verify/<cap>-falsifier-1.unbound.rq`).
    pub id: String,
    /// `"pass"` when the gate's row-count law held (>= 1 row for `gates/`,
    /// 0 rows for `verify/*.unbound.rq`).
    pub status: String,
}

/// The reconcile receipt (projection.ex step 6), keyed for the ledger.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProjectionOutcome {
    /// `order["identity"]`.
    pub order_id: String,
    /// The projected capability's id.
    pub capability_id: String,
    /// [`crate::work_order::digest_of`] over the ORIGINAL pack tree (sorted
    /// rel-path -> file bytes), never the staging copy.
    pub pack_digest: String,
    /// `digest_of` over the exact rendered bytes written.
    pub render_digest: String,
    /// The guarded-write outcome.
    pub write_outcome: WriteOutcome,
    /// Gate census results over the staging copy (pack gates + projected
    /// falsifier gates).
    pub gate_results: Vec<GateResult>,
    /// The order-relative target path as requested.
    pub target_path: String,
}

/// Typed refusals — the Elixir `refusal()` type's ported subset. The
/// `{:pack_lock_failed, _}` / `{:target_pack_mismatch, _, _}` refusals have
/// no counterpart in this re-scope (no pack.lock, no `sj:targetPack` world
/// check) and are replaced by the pack-tree digest pinning.
#[derive(Debug, thiserror::Error)]
pub enum ProjectionRefusal {
    /// The capability's scope query `projections/<id>.rq` does not exist in
    /// the pack (projection.ex `{:capability_unknown, id}`).
    #[error("REFUSED:CAPABILITY_UNKNOWN {0}")]
    CapabilityUnknown(String),
    /// The capability has falsifiers but no
    /// `projections/<id>.unbound.rq` falsified-state query
    /// (projection.ex `{:falsifier_gates_missing, id}`).
    #[error("REFUSED:FALSIFIER_GATES_MISSING {0}")]
    FalsifierGatesMissing(String),
    /// `target_path` is absolute, contains a traversal component, or escapes
    /// the target root — refused before any byte is written
    /// (projection.ex `{:bad_output_path, _}`).
    #[error("REFUSED:BAD_OUTPUT_PATH {0}")]
    BadOutputPath(String),
    /// A post-render gate failed; the actuated file (if any) holds on disk
    /// but standing never advances (projection.ex
    /// `{:verification_failed, _}`).
    #[error("REFUSED:VERIFICATION_FAILED {0}")]
    VerificationFailed(String),
    /// The render hop itself failed (ontology parse, scope-query execution,
    /// or template render) (projection.ex `{:sync_failed, _}`).
    #[error("REFUSED:SYNC_FAILED {0}")]
    SyncFailed(String),
    /// The guarded write refused: the target exists with differing content
    /// (the write.rs FM-WRITE-005 silent-clobber law).
    #[error("REFUSED:GUARDED_WRITE_DIFFERS {0}")]
    GuardedWriteDiffers(String),
    /// Filesystem IO failure outside the guarded write (staging copy, pack
    /// digest walk, gate census read).
    #[error("REFUSED:PROJECTION_IO {0}")]
    Io(String),
}

/// Per-call options (the Elixir `opts` keyword list's ported subset:
/// `:capability_id`, `:target_path`, `:falsifiers`, `:unless_exists`,
/// `:template`).
#[derive(Debug, Clone)]
pub struct ProjectionOpts {
    /// Capability id: selects `projections/<capability_id>.rq`.
    pub capability_id: String,
    /// Order-relative target path under `target_dir`.
    pub target_path: String,
    /// Falsifier URNs to project into `verify/` gates. Defaults to the
    /// order's own `falsifiers` when `None`.
    pub falsifiers: Option<Vec<String>>,
    /// Guarded-write `unless_exists` (default false).
    pub unless_exists: bool,
    /// The Tera template string to render the row-set with. When `None`, the
    /// rendered bytes are the deterministic JSON serialization of the
    /// row-set itself (no template engine in the loop).
    pub template: Option<String>,
}

/// Renders the capability's row-set over the pack ontology and writes the
/// guarded artifact at `target_path` (relative to `target_dir`), returning
/// the [`ProjectionOutcome`] receipt or a typed [`ProjectionRefusal`].
///
/// Errors: every [`ProjectionRefusal`] variant documents its hop; see the
/// module docs for the full pipeline and the projection.ex contract steps
/// each hop ports.
pub fn project(
    order: &WorkOrder,
    pack_dir: &Path,
    target_dir: &Path,
    opts: ProjectionOpts,
) -> Result<ProjectionOutcome, ProjectionRefusal> {
    // Pack pinning FIRST (before any staging/mutation): the digest is over
    // the original tree, and a poisoned pack refuses before any byte moves.
    let pack_digest = pack_digest(pack_dir)?;

    // Staging copy: everything downstream renders against the copy; the
    // TempDir guard removes it after the run (success or refusal).
    let staging = tempfile::Builder::new()
        .prefix("ggen_sjira_projection_staging_")
        .tempdir()
        .map_err(|e| ProjectionRefusal::Io(format!("staging tempdir: {e}")))?;
    copy_dir_recursive(pack_dir, staging.path())?;

    let result = staged_project(order, staging.path(), target_dir, &pack_digest, opts);

    // Belt and braces: TempDir's Drop already removes the staging tree; an
    // explicit removal here makes "staging gone after run" observable even
    // when a future refactor drops the TempDir guard.
    let _ = std::fs::remove_dir_all(staging.path());
    result
}

fn staged_project(
    order: &WorkOrder,
    staging: &Path,
    target_dir: &Path,
    pack_digest: &str,
    opts: ProjectionOpts,
) -> Result<ProjectionOutcome, ProjectionRefusal> {
    let ProjectionOpts {
        capability_id,
        target_path,
        falsifiers,
        unless_exists,
        template,
    } = opts;

    // Step 2: the scope query must exist.
    let scope_query_path = staging.join("projections").join(format!("{capability_id}.rq"));
    if !scope_query_path.is_file() {
        return Err(ProjectionRefusal::CapabilityUnknown(capability_id));
    }

    // Step 5: falsifier -> gate projection, into the STAGING verify/ dir.
    let falsifiers = falsifiers.unwrap_or_else(|| order.falsifiers.clone());
    project_falsifier_gates(staging, &capability_id, &falsifiers)?;

    // Render: rows over the staging ontology, then the template bind.
    let ontology_path = default_ontology(staging);
    let rows = run_select(&ontology_path, &std::fs::read_to_string(&scope_query_path).map_err(
        |e| ProjectionRefusal::Io(format!("{}: {e}", scope_query_path.display())),
    )?)?;
    let content = match template.as_deref() {
        Some(t) if !t.is_empty() => render_template(t, &capability_id, &rows)?,
        _ => serde_json::to_string_pretty(&rows).map_err(|e| {
            ProjectionRefusal::SyncFailed(format!("row serialization: {e}"))
        })?,
    };
    let render_digest = digest_of(&Value::String(content.clone()));

    // Step 3: scoped path (traversal refusal BEFORE any byte is written).
    let abs_target = scoped_path(&target_path, target_dir)?;

    // Guarded write (Actuate.write_file!/3's decision table; write.rs law).
    let write_outcome = guarded_write(abs_target, &content, unless_exists)?;

    // Step 4: post-render gate census over the staging copy — every
    // gates/*.rq >= 1 row, every verify/*.unbound.rq 0 rows, INCLUDING the
    // projected falsifier gates.
    let gate_results = gate_census(staging, &ontology_path)?;

    Ok(ProjectionOutcome {
        order_id: order.identity.clone(),
        capability_id,
        pack_digest: pack_digest.to_string(),
        render_digest,
        write_outcome,
        gate_results,
        target_path,
    })
}

/// `PackLock.digest_checked/1`'s re-scoped form: a deterministic digest over
/// the pack tree — sorted relative paths, each file's bytes — through
/// [`digest_of`]'s canonical sorted-key sha256.
fn pack_digest(pack_dir: &Path) -> Result<String, ProjectionRefusal> {
    if !pack_dir.is_dir() {
        return Err(ProjectionRefusal::Io(format!(
            "pack dir {} is not a directory",
            pack_dir.display()
        )));
    }
    let mut files = BTreeMap::new();
    collect_files(pack_dir, pack_dir, &mut files)?;
    let map: serde_json::Map<String, Value> = files
        .into_iter()
        .map(|(rel, bytes)| {
            (
                rel,
                Value::String(String::from_utf8_lossy(&bytes).into_owned()),
            )
        })
        .collect();
    Ok(digest_of(&Value::Object(map)))
}

fn collect_files(
    root: &Path,
    dir: &Path,
    out: &mut BTreeMap<String, Vec<u8>>,
) -> Result<(), ProjectionRefusal> {
    let entries = std::fs::read_dir(dir)
        .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", dir.display())))?;
    for entry in entries {
        let entry = entry.map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", dir.display())))?;
        let path = entry.path();
        if path.is_dir() {
            collect_files(root, &path, out)?;
        } else if path.is_file() {
            let rel = path
                .strip_prefix(root)
                .map_err(|e| ProjectionRefusal::Io(format!("strip_prefix: {e}")))?
                .to_string_lossy()
                .into_owned();
            let bytes =
                std::fs::read(&path).map_err(|e| ProjectionRefusal::Io(format!("{rel}: {e}")))?;
            out.insert(rel, bytes);
        }
    }
    Ok(())
}

fn project_falsifier_gates(
    staging: &Path,
    capability_id: &str,
    falsifiers: &[String],
) -> Result<(), ProjectionRefusal> {
    if falsifiers.is_empty() {
        return Ok(());
    }
    let source = staging
        .join("projections")
        .join(format!("{capability_id}.unbound.rq"));
    let body = std::fs::read_to_string(&source).map_err(|_| {
        // A falsifier with no falsified-state gate is a court that cannot
        // fire — refuse loudly (projection.ex `read_falsified_state/2`).
        ProjectionRefusal::FalsifierGatesMissing(capability_id.to_string())
    })?;
    let verify_dir = staging.join("verify");
    std::fs::create_dir_all(&verify_dir)
        .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", verify_dir.display())))?;
    for (i, falsifier) in falsifiers.iter().enumerate() {
        let n = i + 1;
        let gate_path = verify_dir.join(format!("{capability_id}-falsifier-{n}.unbound.rq"));
        let header = format!(
            "# projected falsifier gate (<{falsifier}> below) -- GENERATED by \
             ggen_sjira::generated; the pack's falsified-state query for this \
             capability, copied per falsifier.\n"
        );
        std::fs::write(&gate_path, format!("{header}{body}")).map_err(|e| {
            ProjectionRefusal::Io(format!("{}: {e}", gate_path.display()))
        })?;
    }
    Ok(())
}

/// The pack's default ontology (`Pack.default_ontology/1`): `ontology.ttl`
/// at the pack root.
fn default_ontology(staging: &Path) -> PathBuf {
    staging.join("ontology.ttl")
}

/// The render hop's SPARQL engine: a real Oxigraph in-memory store over the
/// ontology, the same engine family under ggen-graph (crate Cargo.toml
/// disclosure). Rows serialize deterministically: sorted variable keys and
/// rows sorted by their canonical JSON encoding.
#[allow(deprecated)]
// `Store::query` deprecation mirrors ggen-graph/src/dialect.rs's own
// `#[allow(deprecated)]` over the same call — workspace precedent.
fn run_select(ontology_path: &Path, query: &str) -> Result<Vec<BTreeMap<String, String>>, ProjectionRefusal> {
    let ontology = std::fs::read_to_string(ontology_path).map_err(|e| {
        ProjectionRefusal::SyncFailed(format!("ontology {}: {e}", ontology_path.display()))
    })?;
    let store = oxigraph::store::Store::new()
        .map_err(|e| ProjectionRefusal::SyncFailed(format!("store: {e}")))?;
    store
        .load_from_slice(
            oxigraph::io::RdfParser::from_format(oxigraph::io::RdfFormat::Turtle),
            ontology.as_bytes(),
        )
        .map_err(|e| ProjectionRefusal::SyncFailed(format!("ontology parse: {e}")))?;
    let results = store
        .query(query)
        .map_err(|e| ProjectionRefusal::SyncFailed(format!("scope query: {e}")))?;
    let select = match results {
        oxigraph::sparql::QueryResults::Solutions(solutions) => solutions,
        _ => {
            return Err(ProjectionRefusal::SyncFailed(
                "scope query must be a SELECT (the capability row query's law)".to_string(),
            ))
        }
    };
    let variables: Vec<String> = select
        .variables()
        .iter()
        .map(|v| v.as_str().to_string())
        .collect();
    let mut rows: Vec<BTreeMap<String, String>> = Vec::new();
    for solution in select {
        let solution =
            solution.map_err(|e| ProjectionRefusal::SyncFailed(format!("solution: {e}")))?;
        let mut row = BTreeMap::new();
        for var in &variables {
            if let Some(term) = solution.get(var.as_str()) {
                row.insert(var.clone(), term.to_string());
                continue;
            }
        }
        rows.push(row);
    }
    rows.sort_by(|a, b| {
        let ka = serde_json::to_string(a).unwrap_or_default();
        let kb = serde_json::to_string(b).unwrap_or_default();
        ka.cmp(&kb)
    });
    Ok(rows)
}

/// The minimal Tera bind (divergence disclosed in the module docs): one
/// one-off template against `{capability_id, rows}`.
fn render_template(
    template: &str,
    capability_id: &str,
    rows: &[BTreeMap<String, String>],
) -> Result<String, ProjectionRefusal> {
    let mut ctx = tera::Context::new();
    ctx.insert("capability_id", &capability_id);
    ctx.insert("rows", &rows);
    // autoescape OFF: this is code generation, not HTML rendering — escaping
    // quotes would corrupt the emitted bytes.
    tera::Tera::one_off(template, &ctx, false)
        .map_err(|e| ProjectionRefusal::SyncFailed(format!("template render: {e}")))
}

/// Path-scope law (projection.ex `scoped_path/2`; write.rs `resolve_target`'s
/// refusal arms): `target_path` resolves against `target_dir`; absolute
/// paths, traversal components, and resolution outside the target root are
/// typed refusals before any byte is written.
fn scoped_path(target_path: &str, target_dir: &Path) -> Result<PathBuf, ProjectionRefusal> {
    let rel = Path::new(target_path);
    if rel.is_absolute() {
        return Err(ProjectionRefusal::BadOutputPath(format!(
            "target path {target_path:?} must be relative to the target root"
        )));
    }
    for component in rel.components() {
        match component {
            Component::Normal(_) | Component::CurDir => {}
            _ => {
                return Err(ProjectionRefusal::BadOutputPath(format!(
                    "target path {target_path:?} contains a traversal component; \
                     it must stay inside the target root"
                )));
            }
        }
    }
    let root = target_dir.canonicalize().map_err(|e| {
        ProjectionRefusal::BadOutputPath(format!(
            "target root {} not canonicalizable: {e}",
            target_dir.display()
        ))
    })?;
    let target = root.join(rel);
    // Symlink-aware containment (write.rs's leaf-first probe): canonicalize
    // the nearest existing path STARTING AT THE TARGET ITSELF, so a symlinked
    // leaf pointing outside the root cannot sail through on its parent chain.
    let mut probe = target.clone();
    while probe.symlink_metadata().is_err() {
        match probe.parent() {
            Some(p) => probe = p.to_path_buf(),
            None => break,
        }
    }
    let probe = probe.canonicalize().map_err(|e| {
        ProjectionRefusal::BadOutputPath(format!("cannot canonicalize {}: {e}", probe.display()))
    })?;
    if !probe.starts_with(&root) {
        return Err(ProjectionRefusal::BadOutputPath(format!(
            "resolved path {} escapes target root {}",
            target.display(),
            root.display()
        )));
    }
    Ok(target)
}

/// The guarded write (`Actuate.write_file!/3` port; decision order mirrors
/// ggen-engine write.rs `plan_write`'s default arm). Parent directories are
/// created as needed.
fn guarded_write(
    target: PathBuf,
    content: &str,
    unless_exists: bool,
) -> Result<WriteOutcome, ProjectionRefusal> {
    let exists = target.exists();
    if exists && unless_exists {
        return Ok(WriteOutcome::SkippedExists);
    }
    if !exists {
        if let Some(parent) = target.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", parent.display())))?;
        }
        std::fs::write(&target, content)
            .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", target.display())))?;
        return Ok(WriteOutcome::Written);
    }
    let existing = std::fs::read_to_string(&target)
        .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", target.display())))?;
    if existing == content {
        Ok(WriteOutcome::Unchanged)
    } else {
        // Silent clobber refused (write.rs FM-WRITE-005's law).
        Err(ProjectionRefusal::GuardedWriteDiffers(format!(
            "{} exists with differing content; refusing silent clobber",
            target.display()
        )))
    }
}

/// The post-render gate census (`GateVerify.run/3` + `verify_unbound/2`):
/// every `gates/*.rq` over the staging ontology must bind >= 1 row; every
/// `verify/*.unbound.rq` must bind 0 rows (it binds rows exactly when the
/// falsified condition holds). Any failure fails closed.
fn gate_census(
    staging: &Path,
    ontology_path: &Path,
) -> Result<Vec<GateResult>, ProjectionRefusal> {
    let mut results = Vec::new();
    for gate in sorted_rq_files(&staging.join("gates"))? {
        let query = std::fs::read_to_string(&gate)
            .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", gate.display())))?;
        let rows = run_select(ontology_path, &query)?;
        let status = if rows.is_empty() { "fail" } else { "pass" };
        results.push(GateResult {
            id: gate_rel(staging, &gate),
            status: status.to_string(),
        });
    }
    for gate in sorted_rq_files(&staging.join("verify"))? {
        let query = std::fs::read_to_string(&gate)
            .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", gate.display())))?;
        let rows = run_select(ontology_path, &query)?;
        let status = if rows.is_empty() { "pass" } else { "fail" };
        results.push(GateResult {
            id: gate_rel(staging, &gate),
            status: status.to_string(),
        });
    }
    if results.iter().any(|g| g.status == "fail") {
        let failed: Vec<String> = results
            .iter()
            .filter(|g| g.status == "fail")
            .map(|g| g.id.clone())
            .collect();
        return Err(ProjectionRefusal::VerificationFailed(format!(
            "gate census failed: {}",
            failed.join(", ")
        )));
    }
    Ok(results)
}

fn gate_rel(staging: &Path, gate: &Path) -> String {
    gate.strip_prefix(staging)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| gate.to_string_lossy().into_owned())
}

fn sorted_rq_files(dir: &Path) -> Result<Vec<PathBuf>, ProjectionRefusal> {
    if !dir.is_dir() {
        return Ok(Vec::new());
    }
    let mut files: Vec<PathBuf> = std::fs::read_dir(dir)
        .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", dir.display())))?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_file() && p.extension().is_some_and(|x| x == "rq"))
        .collect();
    files.sort();
    Ok(files)
}

/// Recursive directory copy (`File.cp_r/2`'s staging law).
fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<(), ProjectionRefusal> {
    std::fs::create_dir_all(dst).map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", dst.display())))?;
    for entry in std::fs::read_dir(src).map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", src.display())))? {
        let entry = entry.map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", src.display())))?;
        let ty = entry.file_type().map_err(|e| {
            ProjectionRefusal::Io(format!("{}: {e}", entry.path().display()))
        })?;
        let to = dst.join(entry.file_name());
        if ty.is_dir() {
            copy_dir_recursive(&entry.path(), &to)?;
        } else {
            std::fs::copy(entry.path(), &to)
                .map_err(|e| ProjectionRefusal::Io(format!("{}: {e}", entry.path().display())))?;
        }
    }
    Ok(())
}
