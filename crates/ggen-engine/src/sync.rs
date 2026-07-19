//! The five-stage sync pipeline: Resolve → Enrich → Extract → Render → Write.
//!
//! [`sync`] loads `ggen.toml` and the ontology (Resolve), runs every
//! template `construct:` query once and inserts the produced triples
//! (Enrich — **single pass**: closure is not iterated to a fixed point yet;
//! constructs that depend on other constructs' output require a second
//! `sync` run), evaluates `when:` ASK guards and named `sparql:` SELECTs
//! (Extract), renders each template body via Tera (Render), and applies the
//! Hygen write semantics from [`crate::write`] (Write).
//!
//! After a non-dry-run sync a praxis-core [`ReceiptRecord`] is chained over
//! the payload `{ graph_hash, outputs: { path → BLAKE3 } }` and written to
//! `<root>/.ggen-v2/receipt.json`.
//!
//! Receipt determinism note: `ts_ns` is fixed to `0`. praxis-core's live
//! `LawObject::receipt` path falls back to the system wall clock when
//! `ReceiptMeta::ts_ns` is `None`; this crate forbids wall clocks, so the
//! record is built directly and chained through
//! [`ReceiptRecord::recompute_chain_hash`], which uses the exact same
//! `build_admission_frame`/`chain_from_frame` construction as the live
//! emission path.

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};

use praxis_core::{
    receipt_record::{ReceiptRecord, RECEIPT_RECORD_VERSION},
    Andon,
};
use serde::Serialize;
use tera::Value;

use crate::{
    config::GgenConfig,
    error::{AppError, Result},
    graph::{DeterministicGraph, EngineQueryResults, GraphEngine, GraphLawStore},
    template::{build_tera, sparql_to_value, Frontmatter, Template},
    write::{plan_write, WriteOutcome},
};

/// Which [`GraphEngine`] a sync runs on.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum EngineKind {
    /// praxis-graphlaw (the roxi fork) — the default: law-state engine with
    /// N3/Datalog materialization, SHACL/ShEx gates, and denial checks.
    #[default]
    GraphLaw,
    /// Plain oxigraph — no law-state support (typed `[FM-LAW-*]` refusal on
    /// any law operation). Kept for A/B determinism testing.
    Oxigraph,
}

/// Options controlling a [`sync`] run.
#[derive(Debug, Clone, Copy, Default)]
pub struct SyncOptions {
    /// Compute outcomes without writing any file (and without a receipt).
    pub dry_run: bool,
    /// Which graph engine executes the run (default: GraphLaw).
    pub engine: EngineKind,
}

/// The outcome of one [`sync`] run.
#[derive(Debug, Clone, Serialize)]
pub struct SyncReport {
    /// Files created, overwritten, or injected into (project-root relative).
    pub written: Vec<PathBuf>,
    /// Files not written, with the reason.
    pub skipped: Vec<(PathBuf, String)>,
    /// BLAKE3 hex of the post-Enrich canonical graph state.
    pub graph_hash_hex: String,
    /// Root-relative output path → write decision ("written", "injected",
    /// "skipped: <reason>").
    pub decisions: BTreeMap<String, String>,
    /// Pack name → BLAKE3 hex of the pack's content hash.
    pub packs: BTreeMap<String, String>,
    /// Input-closure binding: root-relative input path (ontology, pack
    /// ontologies, template files, `from:` bodies) → BLAKE3 hex of the file
    /// bytes, plus an `actuator` entry naming the generator version. A
    /// declared input that cannot be read at binding time is recorded as
    /// `MISSING`, never dropped.
    pub closure: BTreeMap<String, String>,
}

/// Relative path of the sync receipt under the project root.
pub const RECEIPT_REL_PATH: &str = ".ggen-v2/receipt.json";

/// One fully-rendered template awaiting `apply` — the boundary between the
/// "may fail" render pass and the write pass, which by construction only
/// begins once every template in the run has rendered successfully.
struct PendingWrite<'a> {
    to: String,
    body: String,
    tpl: &'a Template,
}

/// Relative path of the append-only receipt history log (one JSON line per
/// non-dry-run sync) under the project root.
pub const RECEIPT_LOG_REL_PATH: &str = ".ggen-v2/receipt-log.jsonl";

/// On-disk shape of `<root>/.ggen-v2/receipt.json`.
#[derive(Debug, Clone, Serialize, serde::Deserialize)]
pub struct SyncReceipt {
    /// The praxis-core chain record over `payload`.
    pub record: ReceiptRecord,
    /// The hashed payload: graph hash plus per-output BLAKE3 hashes.
    pub payload: ReceiptPayload,
}

/// The payload a sync receipt is chained over.
///
/// `deny_unknown_fields`: the payload's wire shape is a closed contract —
/// `#[serde(default)]` fields below are the only sanctioned way to add a
/// backward-compatible field. An attacker (or a corrupted receipt) that
/// injects an extra, unauthenticated field into stored payload JSON must
/// fail to deserialize, never be silently accepted and ignored.
#[derive(Debug, Clone, Serialize, serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReceiptPayload {
    /// BLAKE3 hex of the post-Enrich canonical graph state.
    pub graph_hash: String,
    /// Root-relative output path → BLAKE3 hex of the file bytes on disk.
    pub outputs: BTreeMap<String, String>,
    /// Pack name → BLAKE3 hex of the pack's content hash.
    #[serde(default)]
    pub packs: BTreeMap<String, String>,
    /// Root-relative output path → why the file landed (or did not).
    #[serde(default)]
    pub decisions: BTreeMap<String, String>,
    /// Input-closure binding (see [`SyncReport::closure`]): editing a
    /// template, ontology, pack ontology, or `from:` body changes the
    /// receipt even when the rendered outputs happen to be byte-identical.
    #[serde(default)]
    pub closure: BTreeMap<String, String>,
}

/// Run the five-stage pipeline rooted at `root` (the directory containing
/// `ggen.toml`).
///
/// # Errors
/// Fails closed on any resolve/parse/query/render/write failure; see the
/// FM-* codes on [`crate::config`], [`crate::graph`], [`crate::template`],
/// and [`crate::write`].
pub fn sync(root: &Path, opts: SyncOptions) -> Result<SyncReport> {
    // ── Stage 0: Schema dispatch ─────────────────────────────────────────
    //
    // A project's `ggen.toml` is either a frontmatter project (this
    // function's own `GgenConfig` schema below, `deny_unknown_fields`) or a
    // declarative-rules project (`ggen_config::manifest::GgenManifest`,
    // `[[generation.rules]]`) — never both. `crate::schema_dispatch::load`
    // is the ONE shared dispatch point (backed by
    // `ggen_config::classify_ggen_toml`, the shared structural classifier)
    // every ggen.toml-reading call site in this crate goes through —
    // `verbs::handlers::handle_doctor`/`handle_graph_validate`/
    // `build_law_engine` call the exact same function, not a copy of this
    // stage's logic. See that module's own doc comment for the full design
    // and error-behavior contract (missing file falls through to
    // `GgenConfig::load`'s canonical `[FM-CONFIG-001]`; a structurally
    // ambiguous/unsupported/malformed document fails closed with a typed
    // `CONFIG_SCHEMA_*` code instead of a generic TOML deserialization
    // error).
    let parsed = crate::schema_dispatch::load(root)?;
    let config = match parsed {
        crate::schema_dispatch::ParsedGgenToml::DeclarativeRules(manifest) => {
            return crate::generation_rules::run(root, &manifest, opts);
        }
        crate::schema_dispatch::ParsedGgenToml::Frontmatter(config) => *config,
    };

    // ── Stage 1: Resolve ────────────────────────────────────────────────
    let load_start = Instant::now();
    let load_span = tracing::info_span!(
        "pipeline.load",
        "operation.name" = "pipeline.load",
        "operation.type" = "pipeline",
        "pipeline.stage" = "load",
        "pipeline.duration_ms" = tracing::field::Empty,
    );
    let load_guard = load_span.enter();

    let ontology_path = root.join(&config.ontology.source);
    let ttl = std::fs::read_to_string(&ontology_path).map_err(|e| {
        AppError::fm_config(
            3,
            format!(
                "ontology `{}` unreadable: {e}. Remediation: fix [ontology].source.",
                ontology_path.display()
            ),
        )
    })?;
    let graph: Arc<dyn GraphEngine> = match opts.engine {
        EngineKind::GraphLaw => Arc::new(GraphLawStore::new()?),
        EngineKind::Oxigraph => Arc::new(DeterministicGraph::new()?),
    };
    graph.insert_turtle(&ttl)?;

    // Resolve packs: union their ontologies into the same graph and append
    // their templates (packs sorted by name, then template path). Pack
    // content hashes are checked against ggen.lock before anything renders.
    let packs = crate::pack::resolve(&config, root)?;
    let lock_entries = crate::pack::lock_entries(&config, &packs)?;
    crate::pack::check_lock(root, &lock_entries)?;
    for pack in &packs {
        let pack_ttl = std::fs::read_to_string(&pack.ontology_path).map_err(|e| {
            AppError::fm_pack(
                4,
                format!(
                    "pack `{}`: ontology `{}` unreadable: {e}",
                    pack.name,
                    pack.ontology_path.display()
                ),
            )
        })?;
        graph.insert_turtle(&pack_ttl)?;
        // Union each declared extra ontology (ggen.toml `extra_ontologies`)
        // after the pack's own ontology.ttl, in declaration order — the
        // in-manifest replacement for per-pack make-ontology.sh committed
        // unions.
        for (declared, extra_path) in &pack.extra_ontology_paths {
            let extra_ttl = std::fs::read_to_string(extra_path).map_err(|e| {
                AppError::fm_pack(
                    4,
                    format!(
                        "pack `{}`: extra ontology `{declared}` unreadable at `{}`: {e}",
                        pack.name,
                        extra_path.display()
                    ),
                )
            })?;
            graph.insert_turtle(&extra_ttl)?;
        }
    }

    let templates = discover_templates(root, &config, &packs)?;

    // Bind the input closure: everything that determines the outputs beyond
    // the graph itself. Hashed from disk at binding time; a declared input
    // that has become unreadable is recorded as MISSING so a verifier sees
    // the gap instead of a silently narrower closure.
    let mut closure: BTreeMap<String, String> = BTreeMap::new();
    closure.insert(
        "actuator".to_string(),
        concat!("ggen@", env!("CARGO_PKG_VERSION")).to_string(),
    );
    // Project identity: binds `[project].name` into the receipt so a receipt
    // cannot be silently relabeled as belonging to a different project.
    closure.insert("project".to_string(), config.project.name.clone());
    // The manifest itself (`ggen.toml`) determines the whole pipeline
    // (ontology source, templates dir, law rules/shapes, packs) and must be
    // part of the closure like every other governing input -- previously
    // omitted, which meant editing ggen.toml left no trace in the receipt.
    let manifest_path = root.join("ggen.toml");
    closure.insert(
        rel_display(root, &manifest_path),
        hash_file_or_missing(&manifest_path),
    );
    closure.insert(
        rel_display(root, &ontology_path),
        hash_file_or_missing(&ontology_path),
    );
    for pack in &packs {
        closure.insert(
            rel_display(root, &pack.ontology_path),
            hash_file_or_missing(&pack.ontology_path),
        );
        for (declared, extra_path) in &pack.extra_ontology_paths {
            closure.insert(declared.clone(), hash_file_or_missing(extra_path));
        }
    }
    for (tpl_path, tpl) in &templates {
        closure.insert(rel_display(root, tpl_path), hash_file_or_missing(tpl_path));
        let template_dir = tpl_path.parent().unwrap_or_else(|| Path::new("."));
        if let Some(from) = tpl.frontmatter.from.as_deref() {
            if let Ok(from_path) = crate::write::resolve_target(template_dir, from) {
                closure.insert(
                    rel_display(root, &from_path),
                    hash_file_or_missing(&from_path),
                );
            }
        }
        for rdf_file in &tpl.frontmatter.rdf {
            if let Ok(rdf_path) = crate::write::resolve_target(template_dir, rdf_file) {
                closure.insert(
                    rel_display(root, &rdf_path),
                    hash_file_or_missing(&rdf_path),
                );
            }
        }
    }

    drop(load_guard);
    load_span.record(
        "pipeline.duration_ms",
        load_start.elapsed().as_millis() as u64,
    );

    // ── Stage 2: Enrich (single pass; see module docs) ──────────────────
    //
    // A template declaring `rdf:`/`rdf_inline:` builds its own per-template
    // overlay graph in Stage 3/4 below (see `build_rdf_overlay`), and its
    // `construct:` runs there, against the overlay — not here, against the
    // shared graph. Running it here would leak that template's rdf-sourced
    // triples into the shared graph and hence into every other template,
    // breaking overlay isolation (see `frontmatter_rdf_e2e.rs`).
    let extract_start = Instant::now();
    let extract_span = tracing::info_span!(
        "pipeline.extract",
        "operation.name" = "pipeline.extract",
        "operation.type" = "pipeline",
        "pipeline.stage" = "extract",
        "pipeline.duration_ms" = tracing::field::Empty,
    );
    let extract_guard = extract_span.enter();

    for (_, tpl) in &templates {
        if declares_rdf_overlay(&tpl.frontmatter) {
            continue;
        }
        if let Some(construct) = tpl.frontmatter.construct.as_deref() {
            insert_construct(graph.as_ref(), construct)?;
        }
    }

    drop(extract_guard);
    extract_span.record(
        "pipeline.duration_ms",
        extract_start.elapsed().as_millis() as u64,
    );

    // ── Stage 2b: Law — materialize rules, then gate (SHACL, denials) ───
    //
    // All optional-when-unconfigured: an absent `[law]` table runs no law
    // stage at all, so pre-law projects sync unchanged. With the oxigraph
    // engine any configured law input is a typed `[FM-LAW-*]` refusal.
    let validate_start = Instant::now();
    let validate_span = tracing::info_span!(
        "pipeline.validate",
        "operation.name" = "pipeline.validate",
        "operation.type" = "pipeline",
        "pipeline.stage" = "validate",
        "pipeline.duration_ms" = tracing::field::Empty,
    );
    let validate_guard = validate_span.enter();

    if !config.law.rules.is_empty() {
        for rel in &config.law.rules {
            let rule_path = root.join(rel);
            let src = std::fs::read_to_string(&rule_path).map_err(|e| {
                AppError::fm_law(
                    10,
                    format!(
                        "rule file `{}` unreadable: {e}. Remediation: fix [law].rules.",
                        rule_path.display()
                    ),
                )
            })?;
            closure.insert(
                rel_display(root, &rule_path),
                hash_file_or_missing(&rule_path),
            );
            graph.load_rules(&src)?;
        }
        graph.materialize()?;
        let denials = graph.check_denials()?;
        if !denials.is_empty() {
            return Err(AppError::fm_law(
                11,
                format!(
                    "{} denial rule(s) violated after materialization: {}. \
                     Remediation: fix the facts or the denial rules in [law].rules.",
                    denials.len(),
                    denials.join("; ")
                ),
            ));
        }
    }
    for rel in &config.law.shapes {
        let shape_path = root.join(rel);
        let shapes_ttl = std::fs::read_to_string(&shape_path).map_err(|e| {
            AppError::fm_law(
                12,
                format!(
                    "SHACL shapes file `{}` unreadable: {e}. Remediation: fix [law].shapes.",
                    shape_path.display()
                ),
            )
        })?;
        closure.insert(
            rel_display(root, &shape_path),
            hash_file_or_missing(&shape_path),
        );
        let outcome = graph.validate_shacl(&shapes_ttl)?;
        if !outcome.conforms {
            return Err(AppError::fm_law(
                13,
                format!(
                    "SHACL shapes `{}` report {} violation(s): {}. \
                     Remediation: fix the offending focus nodes or the shapes.",
                    rel.display(),
                    outcome.violations.len(),
                    outcome.violations.join("; ")
                ),
            ));
        }
    }

    // Pack-shipped SHACL shapes: any resolved pack may carry a `shapes.ttl`
    // next to its `ontology.ttl`. Each one is evaluated against the same
    // union graph the templates will render from, and any violation refuses
    // the sync outright — this is what makes cross-pack data drift (e.g. two
    // packs mirroring the same subject IRI with diverged property values,
    // which previously surfaced only as a downstream rustc duplicate-variant
    // error, or not at all) refusable at sync time instead of detectable
    // only by whichever consumer happens to compile the union. Packs without
    // a `shapes.ttl` are unaffected. The shapes file joins the receipt
    // closure like every other governing input.
    for pack in &packs {
        let shape_path = pack.root.join("shapes.ttl");
        if !shape_path.is_file() {
            continue;
        }
        let shapes_ttl = std::fs::read_to_string(&shape_path).map_err(|e| {
            AppError::fm_pack(
                12,
                format!(
                    "pack `{}`: shapes.ttl at `{}` exists but is unreadable: {e}. \
                     Remediation: fix the file's permissions/encoding or remove it.",
                    pack.name,
                    shape_path.display()
                ),
            )
        })?;
        closure.insert(
            rel_display(root, &shape_path),
            hash_file_or_missing(&shape_path),
        );
        let outcome = graph.validate_shacl(&shapes_ttl)?;
        if !outcome.conforms {
            return Err(AppError::fm_pack(
                13,
                format!(
                    "pack `{}` shapes.ttl reports {} violation(s) against the union \
                     graph: {}. Remediation: fix the offending facts (in this pack, \
                     another pack, or the project ontology — the shapes gate the \
                     UNION, so a violation may come from any graph source), or fix \
                     the shapes.",
                    pack.name,
                    outcome.violations.len(),
                    outcome.violations.join("; ")
                ),
            ));
        }
    }

    drop(validate_guard);
    validate_span.record(
        "pipeline.duration_ms",
        validate_start.elapsed().as_millis() as u64,
    );

    let graph_hash_hex = hex32(&graph.state_hash()?);

    // ── Stages 3–4: Extract, Render every template into memory ──────────
    //
    // All rendering happens before any write. A render failure on template N
    // (a bad SPARQL query, a Tera error, a determinism mismatch) must leave
    // NO partial result on disk from templates 1..N-1 — so writes are
    // deferred to a second pass (below) that only starts once every
    // template has rendered successfully.
    let generate_start = Instant::now();
    let generate_span = tracing::info_span!(
        "pipeline.generate",
        "operation.name" = "pipeline.generate",
        "operation.type" = "pipeline",
        "pipeline.stage" = "generate",
        "pipeline.duration_ms" = tracing::field::Empty,
    );
    let generate_guard = generate_span.enter();

    let mut tera = build_tera(Arc::clone(&graph))?;
    let mut skipped: Vec<(PathBuf, String)> = Vec::new();
    let mut decisions: BTreeMap<String, String> = BTreeMap::new();
    // Storage slot for the synthetic module-aggregator template (declared
    // before `pending` so its borrow can live as long as `pending`'s
    // entries do; only populated when `[templates] aggregate_modules` is
    // set — see below, after the render loop).
    let mut aggregator_slot: Option<Template> = None;
    let mut pending: Vec<PendingWrite<'_>> = Vec::new();

    for (tpl_path, tpl) in &templates {
        check_shape_files_exist(root, tpl_path, &tpl.frontmatter.shape)?;

        // Gap 1: per-template RDF overlay (`rdf:`/`rdf_inline:`) — a brand
        // new store layered over the base graph's current triples, built
        // fresh for every template that declares either field so it is
        // deterministic and side-effect-free across templates processed in
        // this same run (the shared `graph`/`tera` are never touched).
        // Templates declaring neither field keep using the shared
        // graph+tera exactly as before this gap existed.
        let overlay = build_rdf_overlay(&graph, tpl_path, &tpl.frontmatter)?;
        if let Some(overlay_graph) = &overlay {
            if let Some(construct) = tpl.frontmatter.construct.as_deref() {
                insert_construct(overlay_graph.as_ref(), construct)?;
            }
        }
        let mut overlay_tera_slot;
        let (active_graph, active_tera): (&Arc<dyn GraphEngine>, &mut tera::Tera) = match &overlay {
            Some(og) => {
                overlay_tera_slot = build_tera(Arc::clone(og))?;
                (og, &mut overlay_tera_slot)
            }
            None => (&graph, &mut tera),
        };

        // Extract: `when:` ASK guard + named `sparql:` SELECTs → rows.
        let Some((named, results)) =
            extract_query_results(active_graph.as_ref(), tpl_path, &tpl.frontmatter, "primary")?
        else {
            let reason = format!("when guard false ({})", tpl_path.display());
            decisions.insert(tpl.frontmatter.to.clone(), format!("skipped: {reason}"));
            skipped.push((PathBuf::from(&tpl.frontmatter.to), reason));
            continue;
        };

        // `determinism: true` — run the SAME extraction a SECOND,
        // independent time against the same graph, once per template (not
        // once per row). `check_determinism` reuses this single recheck for
        // every row/the one non-per-row render below, instead of re-running
        // the query per row. The common case (`determinism` unset/false)
        // never calls `extract_query_results` a second time: zero extra
        // query cost, zero behavior change.
        let determinism_recheck: Option<ExtractedRows> =
            if tpl.frontmatter.determinism == Some(true) {
                Some(extract_query_results(
                    active_graph.as_ref(),
                    tpl_path,
                    &tpl.frontmatter,
                    "determinism_recheck",
                )?)
            } else {
                None
            };

        let per_row = tpl.frontmatter.to.contains("{{");
        if per_row {
            for (row_index, row) in results.iter().enumerate() {
                let mut ctx = base_context(&named, &results);
                ctx.insert("row", row);
                if let Value::Object(map) = row {
                    for (k, v) in map {
                        ctx.insert(k, v);
                    }
                }
                let to = render_str(active_tera, &tpl.frontmatter.to, &ctx, tpl_path)?;
                let body = render_str(active_tera, &tpl.body, &ctx, tpl_path)?;
                check_determinism(
                    active_tera,
                    &tpl.body,
                    tpl_path,
                    &tpl.frontmatter,
                    determinism_recheck.as_ref(),
                    Some(row_index),
                    &body,
                    &to,
                )?;
                pending.push(PendingWrite { to, body, tpl });
            }
        } else {
            let ctx = base_context(&named, &results);
            let body = render_str(active_tera, &tpl.body, &ctx, tpl_path)?;
            check_determinism(
                active_tera,
                &tpl.body,
                tpl_path,
                &tpl.frontmatter,
                determinism_recheck.as_ref(),
                None,
                &body,
                &tpl.frontmatter.to,
            )?;
            pending.push(PendingWrite {
                to: tpl.frontmatter.to.clone(),
                body,
                tpl,
            });
        }
    }

    // `[templates] aggregate_modules = true`: emit one engine-owned
    // aggregator (`src/ggen_pack_mods.rs`) mounting every generated
    // `src/*.rs` output, so the consumer's own lib.rs needs exactly one
    // permanent `include!("ggen_pack_mods.rs");` line rather than one
    // hand-written mount per pack. Exactly one writer — this stage — so two
    // packs can never collide on it the way per-pack `to: src/lib.rs`
    // templates did (the duplicate-target refusal below still guards the
    // aggregator path itself against a template claiming it directly).
    // Deterministic: entries sorted by target path; mod names derived from
    // the file stem. Skipped entirely (no file, not an empty file) when no
    // `src/*.rs` outputs exist this run.
    if config.templates.aggregate_modules {
        const AGGREGATOR_REL_PATH: &str = "src/ggen_pack_mods.rs";
        let mut mounts: Vec<(String, String)> = pending
            .iter()
            .filter(|pw| {
                pw.to.starts_with("src/") && pw.to.ends_with(".rs") && pw.to != AGGREGATOR_REL_PATH
            })
            .map(|pw| {
                let rel_to_src = pw.to.trim_start_matches("src/").to_string();
                let mod_name = rel_to_src
                    .trim_end_matches(".rs")
                    .chars()
                    .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
                    .collect::<String>();
                (mod_name, rel_to_src)
            })
            .collect();
        mounts.sort();
        mounts.dedup();
        if !mounts.is_empty() {
            // Plain `//` comments, NOT `//!` inner docs: the file is meant
            // to be consumed via `include!(...)` from the consumer's lib.rs,
            // where pasted-in inner doc comments are a hard E0753 compile
            // error (found by actually building a real consumer, not
            // assumed).
            let mut body = String::from(
                "// GENERATED by `ggen sync` (`[templates] aggregate_modules = true`).\n\
                 // One mount per generated `src/*.rs` output this run produced. Do not\n\
                 // edit by hand — regenerated every sync. Consumer wiring: exactly one\n\
                 // permanent line in your own lib.rs:\n\
                 //     include!(\"ggen_pack_mods.rs\");\n\n",
            );
            for (mod_name, rel_to_src) in &mounts {
                body.push_str(&format!(
                    "#[path = \"{rel_to_src}\"]\npub mod {mod_name};\n"
                ));
            }
            aggregator_slot = Some(Template {
                frontmatter: Frontmatter {
                    to: AGGREGATOR_REL_PATH.to_string(),
                    sparql: BTreeMap::new(),
                    construct: None,
                    inject: false,
                    before: None,
                    after: None,
                    at_line: None,
                    skip_if: None,
                    unless_exists: false,
                    force: true,
                    when: None,
                    skip_empty: false,
                    from: None,
                    sh_before: None,
                    sh_after: None,
                    backup: false,
                    shape: Vec::new(),
                    determinism: None,
                    freeze_policy: None,
                    freeze_slots_dir: None,
                    rdf: Vec::new(),
                    rdf_inline: Vec::new(),
                    prefixes: BTreeMap::new(),
                    base: None,
                },
                body: String::new(),
            });
            let tpl_ref: &Template = aggregator_slot
                .as_ref()
                .expect("aggregator_slot was assigned Some on the previous statement");
            pending.push(PendingWrite {
                to: AGGREGATOR_REL_PATH.to_string(),
                body,
                tpl: tpl_ref,
            });
        }
    }

    // Two rendered templates (or two rows of one template) resolving to the
    // same target would silently last-row-win on disk and in the decisions
    // map — refuse instead.
    {
        let mut seen: BTreeMap<&str, usize> = BTreeMap::new();
        for pw in &pending {
            *seen.entry(pw.to.as_str()).or_default() += 1;
        }
        if let Some((to, n)) = seen.into_iter().find(|(_, n)| *n > 1) {
            return Err(AppError::fm_write(
                8,
                format!(
                    "{n} rendered templates target the same output `{to}`. \
                     Remediation: make the `to:` pattern unique per row/template \
                     (e.g. include a distinguishing SPARQL variable in the path)."
                ),
            ));
        }
    }

    drop(generate_guard);
    generate_span.record(
        "pipeline.duration_ms",
        generate_start.elapsed().as_millis() as u64,
    );

    // ── Stage 5: Write every already-rendered template ───────────────────
    let emit_start = Instant::now();
    let emit_span = tracing::info_span!(
        "pipeline.emit",
        "operation.name" = "pipeline.emit",
        "operation.type" = "pipeline",
        "pipeline.stage" = "emit",
        "pipeline.duration_ms" = tracing::field::Empty,
        "pipeline.files_generated" = tracing::field::Empty,
    );
    let emit_guard = emit_span.enter();

    let mut written: Vec<PathBuf> = Vec::new();
    for pw in &pending {
        apply(
            root,
            &pw.to,
            &pw.body,
            pw.tpl,
            opts,
            &mut written,
            &mut skipped,
            &mut decisions,
        )?;
    }

    drop(emit_guard);
    emit_span.record(
        "pipeline.duration_ms",
        emit_start.elapsed().as_millis() as u64,
    );
    emit_span.record("pipeline.files_generated", written.len() as u64);

    let pack_hashes: BTreeMap<String, String> = lock_entries
        .iter()
        .map(|e| {
            (
                e.name.clone(),
                e.content_hash.trim_start_matches("blake3:").to_string(),
            )
        })
        .collect();

    let report = SyncReport {
        written,
        skipped,
        graph_hash_hex,
        decisions,
        packs: pack_hashes,
        closure,
    };

    if !opts.dry_run {
        write_receipt(root, &report)?;
        if !lock_entries.is_empty() {
            crate::pack::write_lock(root, &lock_entries)?;
        }
    }
    Ok(report)
}

/// Root-relative display form of a closure input path (falls back to the
/// full path for inputs outside the project root, e.g. absolute pack dirs).
///
/// `pub(crate)`: also used by `crate::generation_rules` to bind its own
/// input closure entries (ontology imports, query/template files) the exact
/// same way.
pub(crate) fn rel_display(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .display()
        .to_string()
}

/// BLAKE3 hex of a closure input's bytes, or the literal `MISSING` marker
/// when the declared input cannot be read — recorded, never dropped.
///
/// `pub(crate)`: also used by `crate::generation_rules` (see
/// [`rel_display`]'s doc comment).
pub(crate) fn hash_file_or_missing(path: &Path) -> String {
    match std::fs::read(path) {
        Ok(bytes) => blake3::hash(&bytes).to_hex().to_string(),
        Err(_) => "MISSING".to_string(),
    }
}

/// The outcome of [`extract_query_results`] when the `when:` guard (if any)
/// evaluated `true`: the named `sparql:` results plus the driving row array.
/// `None` (the outer [`Option`] returned by [`extract_query_results`] itself)
/// means the `when:` guard evaluated `false` — the template must be skipped.
type ExtractedRows = Option<(BTreeMap<String, Value>, Vec<Value>)>;

/// Run this template's `when:` ASK guard and named `sparql:` SELECTs against
/// `active_graph`. Returns `Ok(None)` when a `when:` guard is present and
/// evaluates `false` (the template must be skipped this run); otherwise
/// `Ok(Some((named, results)))`.
///
/// Shared by the per-template loop's primary extraction and
/// [`check_determinism`]'s second, independent recheck — the exact same
/// code path runs both times, so `determinism: true` catches
/// query-execution nondeterminism (different bindings/ordering across two
/// runs against identical graph state), not merely Tera-render
/// nondeterminism against a single, already-fixed context.
///
/// `phase` (`"primary"` / `"determinism_recheck"`) is a `tracing` tag only —
/// no behavior depends on it. It lets a test observe, via a
/// `tracing::Subscriber`, that a determinism recheck genuinely re-executes
/// every declared query rather than re-rendering cached values.
///
/// # Errors
/// `[FM-TPL-004]` if `when:` is present and is not an ASK query.
/// Propagates `[FM-GRAPH-*]` from the underlying `GraphEngine::query` calls.
fn extract_query_results(
    active_graph: &dyn GraphEngine, tpl_path: &Path, frontmatter: &Frontmatter, phase: &'static str,
) -> Result<ExtractedRows> {
    if let Some(ask) = frontmatter.when.as_deref() {
        tracing::debug!(
            target: "ggen_engine::sync::query_extraction",
            phase,
            tpl = %tpl_path.display(),
            "executing `when:` ASK guard"
        );
        match active_graph.query(ask)? {
            EngineQueryResults::Boolean(true) => {}
            EngineQueryResults::Boolean(false) => return Ok(None),
            _ => {
                return Err(AppError::fm_tpl(
                    16,
                    format!(
                        "`when:` in {} must be an ASK query. \
                         Remediation: use ASK {{ … }}.",
                        tpl_path.display()
                    ),
                ));
            }
        }
    }
    let mut named: BTreeMap<String, Value> = BTreeMap::new();
    for (key, query) in &frontmatter.sparql {
        tracing::debug!(
            target: "ggen_engine::sync::query_extraction",
            phase,
            tpl = %tpl_path.display(),
            sparql_key = %key,
            "executing named sparql query"
        );
        named.insert(key.clone(), sparql_to_value(active_graph, query)?);
    }
    // Driving rows: the first named query (BTreeMap key order) with an
    // array result; empty when no SELECT produced rows.
    let results: Vec<Value> = named
        .values()
        .find_map(|v| v.as_array().cloned())
        .unwrap_or_default();
    Ok(Some((named, results)))
}

/// Build the shared Tera context: `results` plus every named sparql key.
fn base_context(named: &BTreeMap<String, Value>, results: &[Value]) -> tera::Context {
    let mut ctx = tera::Context::new();
    ctx.insert("results", results);
    for (k, v) in named {
        ctx.insert(k, v);
    }
    ctx
}

/// Render a Tera string, mapping errors to `[FM-TPL-017]`.
fn render_str(
    tera: &mut tera::Tera, template: &str, ctx: &tera::Context, tpl_path: &Path,
) -> Result<String> {
    tera.render_str(template, ctx).map_err(|e| {
        AppError::fm_tpl(
            17,
            format!(
                "render failed for {}: {e}. Available top-level context keys: {}.",
                tpl_path.display(),
                context_key_summary(ctx)
            ),
        )
    })
}

/// Summarize a Tera context's top-level keys (and, for arrays, their
/// length) for a render-failure error message — enough to spot a typo'd
/// variable name (`row.nuon` vs `row.noun`) without dumping the full,
/// potentially large, context payload.
fn context_key_summary(ctx: &tera::Context) -> String {
    let Value::Object(map) = ctx.clone().into_json() else {
        return "(none)".to_string();
    };
    if map.is_empty() {
        return "(none)".to_string();
    }
    map.iter()
        .map(|(k, v)| match v {
            Value::Array(items) => format!("{k} (array, {} items)", items.len()),
            Value::Object(fields) => format!("{k} (object, {} fields)", fields.len()),
            other => format!("{k} ({})", value_type_name(other)),
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn value_type_name(v: &Value) -> &'static str {
    match v {
        Value::Null => "null",
        Value::Bool(_) => "bool",
        Value::Number(_) => "number",
        Value::String(_) => "string",
        Value::Array(_) => "array",
        Value::Object(_) => "object",
    }
}

/// Apply (or dry-run) one write and record the outcome and its decision.
#[allow(clippy::too_many_arguments)]
fn apply(
    root: &Path, rel_to: &str, body: &str, tpl: &Template, opts: SyncOptions,
    written: &mut Vec<PathBuf>, skipped: &mut Vec<(PathBuf, String)>,
    decisions: &mut BTreeMap<String, String>,
) -> Result<()> {
    if tpl.frontmatter.skip_empty && body.trim().is_empty() {
        let reason = "skip_empty: rendered body empty".to_string();
        decisions.insert(rel_to.to_string(), format!("skipped: {reason}"));
        skipped.push((PathBuf::from(rel_to), reason));
        return Ok(());
    }
    if opts.dry_run {
        // Dry run: classify without touching the filesystem or running
        // sh_before/sh_after (a dry run must have zero side effects).
        // Identical existing content reports Skipped(unchanged); everything
        // else is reported as a planned write.
        let target = root.join(rel_to);
        match std::fs::read_to_string(&target) {
            Ok(existing) if existing == body => {
                let reason = "unchanged: content identical".to_string();
                decisions.insert(rel_to.to_string(), format!("skipped: {reason}"));
                skipped.push((PathBuf::from(rel_to), reason));
            }
            Ok(_) => {
                decisions.insert(rel_to.to_string(), "planned: write (dry-run)".to_string());
                written.push(PathBuf::from(rel_to));
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                decisions.insert(rel_to.to_string(), "planned: write (dry-run)".to_string());
                written.push(PathBuf::from(rel_to));
            }
            Err(e) => {
                // An existing target that cannot be read as UTF-8 (or at
                // all) is not a plain "planned: write" — the real run would
                // fail closed here, and a dry run must not classify it
                // more optimistically than the run it predicts.
                return Err(AppError::fm_write(
                    9,
                    format!(
                        "dry-run: target `{}` exists but is unreadable as UTF-8: {e}. \
                         A non-dry-run sync would refuse here too. \
                         Remediation: remove or fix the target file.",
                        target.display()
                    ),
                ));
            }
        }
        return Ok(());
    }

    if let Some(cmd) = tpl.frontmatter.sh_before.as_deref() {
        run_shell_hook(root, cmd, "sh_before")?;
    }

    match plan_write(root, rel_to, body, &tpl.frontmatter)? {
        WriteOutcome::Written => {
            decisions.insert(rel_to.to_string(), "written".to_string());
            written.push(PathBuf::from(rel_to));
            if let Some(cmd) = tpl.frontmatter.sh_after.as_deref() {
                run_shell_hook(root, cmd, "sh_after")?;
            }
        }
        WriteOutcome::Injected => {
            decisions.insert(rel_to.to_string(), "injected".to_string());
            written.push(PathBuf::from(rel_to));
            if let Some(cmd) = tpl.frontmatter.sh_after.as_deref() {
                run_shell_hook(root, cmd, "sh_after")?;
            }
        }
        WriteOutcome::Skipped(reason) => {
            decisions.insert(rel_to.to_string(), format!("skipped: {reason}"));
            skipped.push((PathBuf::from(rel_to), reason));
        }
    }
    Ok(())
}

/// Refuse `cmd` against the shell-command denylist, then run it with `root`
/// as its working directory. Non-zero exit is a hard error.
fn run_shell_hook(root: &Path, cmd: &str, which: &str) -> Result<()> {
    crate::shell_safety::check_shell_command_safe(cmd)?;
    let status = std::process::Command::new("sh")
        .arg("-c")
        .arg(cmd)
        .current_dir(root)
        .status()
        .map_err(|e| AppError::fm_shell(2, format!("{which} failed to launch: {e}")))?;
    if !status.success() {
        return Err(AppError::fm_shell(
            3,
            format!("{which} command `{cmd}` exited with {status}. Remediation: fix the command or remove it from frontmatter."),
        ));
    }
    Ok(())
}

/// Discover and parse every template a sync would process: project
/// templates (recursive under `[templates].dir`, sorted) followed by each
/// resolved pack's templates in pack order. Shared by [`sync`] and
/// `ggen graph validate` so both surfaces see the identical template set.
///
/// # Errors
/// Unreadable templates dir or any template parse failure (fail closed).
pub fn discover_templates(
    root: &Path, config: &GgenConfig, packs: &[crate::pack::Pack],
) -> Result<Vec<(PathBuf, Template)>> {
    let mut templates = load_templates(&root.join(&config.templates.dir))?;
    for pack in packs {
        for path in &pack.template_paths {
            templates.push((path.clone(), parse_template_file(path)?));
        }
    }
    Ok(templates)
}

/// Load and parse every `*.tmpl` under `dir` (recursive), in sorted path
/// order for deterministic processing.
fn load_templates(dir: &Path) -> Result<Vec<(PathBuf, Template)>> {
    let mut paths: Vec<PathBuf> = Vec::new();
    collect_tmpl_paths(dir, &mut paths)?;
    paths.sort();
    let mut out = Vec::with_capacity(paths.len());
    for path in paths {
        let tpl = parse_template_file(&path)?;
        out.push((path, tpl));
    }
    Ok(out)
}

/// Read and parse one `*.tmpl` file (identical frontmatter contract for
/// project and pack templates — no special casing). If the frontmatter sets
/// `from:`, the Tera body is replaced with the content of that path
/// (resolved relative to this template file's own directory); frontmatter
/// fields still come from `path` itself.
///
/// # Errors
/// `[FM-TPL-008]` when `from:` escapes the template's own directory —
/// distinct from [`check_shape_files_exist`]'s `[FM-TPL-014]` (a missing
/// `shape:` file); the two used to share this numeric code despite being
/// unrelated causes.
fn parse_template_file(path: &Path) -> Result<Template> {
    let content = std::fs::read_to_string(path)?;
    let mut tpl = Template::parse(&content)
        .map_err(|e| AppError::fm_tpl(6, format!("{}: {e}", path.display())))?;
    if let Some(from) = tpl.frontmatter.from.clone() {
        let base = path.parent().unwrap_or_else(|| Path::new("."));
        let from_path = crate::write::resolve_target(base, &from).map_err(|e| {
            AppError::fm_tpl(
                8,
                format!(
                    "{}: `from: {from}` is not a safe path (must stay inside the \
                     template's own directory): {e}",
                    path.display()
                ),
            )
        })?;
        tpl.body = std::fs::read_to_string(&from_path).map_err(|e| {
            AppError::fm_tpl(
                7,
                format!(
                    "{}: `from: {from}` unreadable at `{}`: {e}. \
                     Remediation: fix the `from:` path (relative to the template's own directory).",
                    path.display(),
                    from_path.display()
                ),
            )
        })?;
    }
    Ok(tpl)
}

/// Refuse if any `shape:` file listed in `frontmatter` does not exist.
/// **Existence-checked only** — no SHACL engine runs in this crate yet, so
/// this does not evaluate the shapes against rendered output; see
/// `docs/v26.7.4/GGEN_TOML_SCHEMA_MAPPING.md`.
///
/// # Errors
/// `[FM-TPL-014]` — distinct from `[FM-TPL-008]` (`from:` path traversal in
/// [`parse_template_file`]): a missing `shape:` file is an unrelated failure
/// cause that used to share `FM-TPL-008`'s numeric code.
fn check_shape_files_exist(root: &Path, tpl_path: &Path, shapes: &[String]) -> Result<()> {
    for shape in shapes {
        let shape_path = root.join(shape);
        if !shape_path.exists() {
            return Err(AppError::fm_tpl(
                14,
                format!(
                    "{}: `shape:` entry `{shape}` does not exist at `{}`. \
                     Remediation: fix the path or remove the entry.",
                    tpl_path.display(),
                    shape_path.display()
                ),
            ));
        }
    }
    Ok(())
}

/// When `frontmatter.determinism == Some(true)`, re-render the template's
/// `to:` path and body against `determinism_recheck` — the result of a
/// genuinely SECOND, independent execution of this template's declared
/// queries (`when:`, `sparql:`; see [`extract_query_results`]), built by the
/// caller once per template — and refuse if either differs from the first
/// render. This is a real, enforced assertion of query-*and*-render
/// determinism, not merely a second render against the first run's
/// already-fixed context (which would only catch Tera-level nondeterminism,
/// e.g. a `now()` call, never a query that returns different bindings or
/// row order on a second, independent execution against identical graph
/// state).
///
/// `row_index` selects which row of the second extraction's `results`
/// corresponds to the row being checked (`None` for a non-per-row
/// template). A row-count mismatch between the first and second extraction
/// (the second run producing fewer rows than `row_index` needs) is itself a
/// determinism violation, refused with the same `[FM-TPL-009]` code.
///
/// `determinism_recheck` is `None` when `frontmatter.determinism` is not
/// `Some(true)` — in that case this function is a no-op, at zero cost
/// (`extract_query_results` was never called a second time by the caller).
#[allow(clippy::too_many_arguments)]
fn check_determinism(
    tera: &mut tera::Tera, body_template: &str, tpl_path: &Path,
    frontmatter: &crate::template::Frontmatter, determinism_recheck: Option<&ExtractedRows>,
    row_index: Option<usize>, first_render: &str, first_to: &str,
) -> Result<()> {
    let Some(recheck) = determinism_recheck else {
        return Ok(());
    };
    let Some((named2, results2)) = recheck else {
        return Err(AppError::fm_tpl(
            9,
            format!(
                "{}: `determinism: true` violated — re-running the `when:` guard a second, \
                 independent time evaluated false after the first run evaluated true. \
                 Remediation: remove non-deterministic terms from `when:`.",
                tpl_path.display()
            ),
        ));
    };

    let ctx2 = match row_index {
        Some(idx) => {
            let row2 = results2.get(idx).ok_or_else(|| {
                AppError::fm_tpl(
                    9,
                    format!(
                        "{}: `determinism: true` violated — re-running the named `sparql:` \
                         queries a second, independent time produced {} row(s), which does \
                         not cover row index {idx} the first run produced. \
                         Remediation: add an ORDER BY (and, if paginating, a stable tie-break) \
                         to every `sparql:` query so row count and order are stable across runs.",
                        tpl_path.display(),
                        results2.len()
                    ),
                )
            })?;
            let mut ctx = base_context(named2, results2);
            ctx.insert("row", row2);
            if let Value::Object(map) = row2 {
                for (k, v) in map {
                    ctx.insert(k, v);
                }
            }
            ctx
        }
        None => base_context(named2, results2),
    };

    // The templated `to:` path is part of the output — a non-deterministic
    // path escapes a body-only check.
    let second_to = render_str(tera, &frontmatter.to, &ctx2, tpl_path)?;
    if second_to != first_to {
        return Err(AppError::fm_tpl(
            9,
            format!(
                "{}: `determinism: true` violated — re-rendering the `to:` path from a \
                 second, independent query execution produced `{second_to}` after \
                 `{first_to}`. \
                 Remediation: remove non-deterministic terms from the query or from `to:`.",
                tpl_path.display()
            ),
        ));
    }
    let second_render = render_str(tera, body_template, &ctx2, tpl_path)?;
    if second_render != first_render {
        return Err(AppError::fm_tpl(
            9,
            format!(
                "{}: `determinism: true` violated — re-rendering the template body from a \
                 second, independent query execution produced different output. \
                 Remediation: remove non-deterministic terms from the query or from this \
                 template.",
                tpl_path.display()
            ),
        ));
    }
    Ok(())
}

/// Recursively collect `*.tmpl` files. A missing templates dir fails closed.
fn collect_tmpl_paths(dir: &Path, out: &mut Vec<PathBuf>) -> Result<()> {
    let entries = std::fs::read_dir(dir).map_err(|e| {
        AppError::fm_config(
            4,
            format!(
                "templates dir `{}` unreadable: {e}. Remediation: fix [templates].dir.",
                dir.display()
            ),
        )
    })?;
    for entry in entries {
        let entry = entry?;
        let path = entry.path();
        if path.is_dir() {
            collect_tmpl_paths(&path, out)?;
        } else if path.extension().is_some_and(|e| e == "tmpl") {
            out.push(path);
        }
    }
    Ok(())
}

/// `true` when a template's frontmatter declares `rdf:` and/or
/// `rdf_inline:` — such a template gets its own per-template overlay graph
/// (see [`build_rdf_overlay`]) instead of using the shared graph directly.
fn declares_rdf_overlay(fm: &Frontmatter) -> bool {
    !fm.rdf.is_empty() || !fm.rdf_inline.is_empty()
}

/// Turtle prolog (`@base`/`@prefix` directives, literal Turtle syntax —
/// *not* the SPARQL-style `BASE`/`PREFIX` prolog ggen-core's `build_prolog`
/// emits for SPARQL query text) prepended to `rdf:`/`rdf_inline:` content
/// before parsing, so relative/prefixed IRIs in that content resolve as the
/// frontmatter's `base:`/`prefixes:` declare. `@base` first (when present),
/// then `@prefix` lines in deterministic (`BTreeMap`) key order.
fn build_turtle_prolog(prefixes: &BTreeMap<String, String>, base: Option<&str>) -> String {
    let mut out = String::new();
    if let Some(b) = base {
        out.push_str("@base <");
        out.push_str(b);
        out.push_str("> .\n");
    }
    for (name, iri) in prefixes {
        out.push_str("@prefix ");
        out.push_str(name);
        out.push_str(": <");
        out.push_str(iri);
        out.push_str("> .\n");
    }
    out
}

/// Build a per-template overlay graph for a template declaring
/// `rdf:`/`rdf_inline:`: a brand-new store containing the base graph's
/// current triples (re-serialized as N-Triples — a syntactic subset of
/// Turtle, same trick [`insert_construct`] relies on) plus this template's
/// own RDF/Turtle content. `base` is never mutated — this is a fresh store,
/// so two templates in the same run with different `rdf:`/`rdf_inline:`
/// never see each other's extra triples, and templates that declare
/// neither field are entirely unaffected.
///
/// Returns `None` when the template declares neither field, meaning callers
/// should keep using `base` (and the shared `tera`) directly.
///
/// # Errors
/// - `[FM-TPL-010]` an `rdf:` path escapes the template's own directory —
///   the identical traversal-safety check `from:` uses
///   ([`crate::write::resolve_target`]), applied relative to the template
///   file's own directory (never the project root).
/// - `[FM-TPL-011]` an `rdf:` path is unreadable.
/// - Propagates `[FM-GRAPH-*]` on any Turtle parse failure (the base
///   graph's re-serialized triples, or `rdf:`/`rdf_inline:` content).
fn build_rdf_overlay(
    base: &Arc<dyn GraphEngine>, tpl_path: &Path, fm: &Frontmatter,
) -> Result<Option<Arc<dyn GraphEngine>>> {
    if !declares_rdf_overlay(fm) {
        return Ok(None);
    }

    let overlay = DeterministicGraph::new()?;
    let base_ntriples: String = base
        .canonical_quads()?
        .into_iter()
        .map(|line| format!("{line} .\n"))
        .collect();
    if !base_ntriples.is_empty() {
        overlay.insert_turtle(&base_ntriples)?;
    }

    let prolog = build_turtle_prolog(&fm.prefixes, fm.base.as_deref());
    let template_dir = tpl_path.parent().unwrap_or_else(|| Path::new("."));

    for rdf_file in &fm.rdf {
        let rdf_path = crate::write::resolve_target(template_dir, rdf_file).map_err(|e| {
            AppError::fm_tpl(
                18,
                format!(
                    "{}: `rdf: {rdf_file}` is not a safe path (must stay inside the \
                     template's own directory): {e}",
                    tpl_path.display()
                ),
            )
        })?;
        let content = std::fs::read_to_string(&rdf_path).map_err(|e| {
            AppError::fm_tpl(
                11,
                format!(
                    "{}: `rdf: {rdf_file}` unreadable at `{}`: {e}. \
                     Remediation: fix the path (relative to the template's own directory).",
                    tpl_path.display(),
                    rdf_path.display()
                ),
            )
        })?;
        overlay.insert_turtle(&format!("{prolog}{content}"))?;
    }

    for inline in &fm.rdf_inline {
        overlay.insert_turtle(&format!("{prolog}{inline}"))?;
    }

    Ok(Some(Arc::new(overlay)))
}

/// Run a CONSTRUCT query and insert its triples back into the graph.
fn insert_construct(graph: &dyn GraphEngine, construct: &str) -> Result<()> {
    use std::fmt::Write as _;
    let EngineQueryResults::Graph(triples) = graph.query(construct)? else {
        return Err(AppError::fm_graph(
            7,
            "`construct:` frontmatter must be a CONSTRUCT/DESCRIBE query. \
             Remediation: use CONSTRUCT { … } WHERE { … }.",
        ));
    };
    let mut doc = String::new();
    for triple in triples {
        let _ = writeln!(doc, "{} .", triple.ntriples);
    }
    if !doc.is_empty() {
        // N-Triples is a syntactic subset of Turtle.
        graph.insert_turtle(&doc)?;
    }
    Ok(())
}

/// Read the previous chain head, if any. The receipt log's tail is the
/// source of truth (it is appended before the head pointer is rewritten, so
/// a partially-failed sync leaves a re-runnable state); `receipt.json` is a
/// fallback for pre-log projects.
fn read_prev_head(receipt_path: &Path, log_path: &Path) -> Result<Option<SyncReceipt>> {
    match std::fs::read_to_string(log_path) {
        Ok(raw) => {
            if let Some(line) = raw.lines().rev().find(|l| !l.trim().is_empty()) {
                let prev: SyncReceipt = serde_json::from_str(line).map_err(|e| {
                    AppError::fm_chain(
                        3,
                        format!(
                            "receipt log `{}` tail malformed: {e}. \
                             Remediation: run `ggen receipt history` and repair the log.",
                            log_path.display()
                        ),
                    )
                })?;
                return Ok(Some(prev));
            }
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => {
            return Err(AppError::fm_chain(
                3,
                format!("receipt log `{}` unreadable: {e}", log_path.display()),
            ))
        }
    }
    match std::fs::read_to_string(receipt_path) {
        Ok(raw) => {
            let prev: SyncReceipt = serde_json::from_str(&raw).map_err(|e| {
                AppError::fm_chain(
                    3,
                    format!(
                        "previous receipt `{}` malformed: {e}. \
                         Remediation: verify or remove the stale receipt.",
                        receipt_path.display()
                    ),
                )
            })?;
            Ok(Some(prev))
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(None),
        Err(e) => Err(AppError::fm_chain(
            3,
            format!(
                "previous receipt `{}` unreadable: {e}",
                receipt_path.display()
            ),
        )),
    }
}

/// Chain a praxis-core [`ReceiptRecord`] over `{ graph_hash, outputs }` and
/// write it to [`RECEIPT_REL_PATH`]. `ts_ns` is fixed to 0 (no wall clock;
/// see module docs).
///
/// Both files are written compact (not pretty-printed) so the payload's
/// on-disk bytes are exactly the bytes `payload_hash_hex` was computed
/// over — the verifier hashes the raw stored payload rather than
/// re-serializing it, which keeps old receipts verifiable across payload
/// schema additions.
///
/// `pub(crate)`: also called by `crate::generation_rules::run` so the
/// declarative `[[generation.rules]]` sync path chains onto the exact same
/// receipt history/log as the frontmatter path (one receipt chain per
/// project, regardless of which schema `ggen.toml` uses).
///
/// This is one of two deliberately coexisting receipt mechanisms: the
/// chained BLAKE3 ledger written here is a continuous chain-of-custody
/// record across syncs. `ggen_config::receipt::Receipt` is the other —
/// Ed25519-signed over SHA-256, emitted once per pack install to
/// `.ggen/receipts/pack-*.json`. Different lifecycles, different hash
/// algorithms; neither replaces the other.
pub(crate) fn write_receipt(root: &Path, report: &SyncReport) -> Result<()> {
    use std::io::Write as _;

    // Bind every decision target that exists on disk (written this run or
    // skipped-unchanged), so a no-op re-sync produces the identical payload.
    // A target that is absent is fine (e.g. a false `when` guard whose file
    // was never generated); a target that exists but cannot be read must
    // fail closed — silently omitting it would unbind the file from the
    // receipt and from every later doctor/staleness check.
    let mut outputs = BTreeMap::new();
    for rel in report.decisions.keys() {
        let target = root.join(rel);
        match std::fs::read(&target) {
            Ok(bytes) => {
                outputs.insert(rel.clone(), blake3::hash(&bytes).to_hex().to_string());
            }
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                return Err(AppError::fm_chain(
                    8,
                    format!(
                        "output `{}` cannot be read for receipt binding: {e}. \
                         Remediation: fix the file's permissions.",
                        target.display()
                    ),
                ))
            }
        }
    }
    let payload = ReceiptPayload {
        graph_hash: report.graph_hash_hex.clone(),
        outputs,
        packs: report.packs.clone(),
        decisions: report.decisions.clone(),
        closure: report.closure.clone(),
    };
    let payload_bytes = serde_json::to_vec(&payload)?;
    let payload_hash_hex = blake3::hash(&payload_bytes).to_hex().to_string();

    // Chain onto the previous sync's receipt when one exists; a genesis
    // receipt chains from all-zeros. Never extend an unverified head: a
    // tampered previous receipt must be refused here, not discovered later
    // by `receipt history`.
    let receipt_path = root.join(RECEIPT_REL_PATH);
    let log_path = root.join(RECEIPT_LOG_REL_PATH);
    let prev_chain_hash_hex = match read_prev_head(&receipt_path, &log_path)? {
        Some(prev) => {
            let recomputed = prev.record.recompute_chain_hash().map_err(|e| {
                AppError::fm_chain(9, format!("previous receipt chain recompute failed: {e}"))
            })?;
            if hex32(&recomputed) != prev.record.chain_hash_hex {
                return Err(AppError::fm_chain(
                    9,
                    format!(
                        "previous receipt head is invalid: stored chain hash {} does not \
                         match recompute {}. Refusing to extend a tampered chain. \
                         Remediation: run `ggen receipt history` and restore the receipts.",
                        prev.record.chain_hash_hex,
                        hex32(&recomputed)
                    ),
                ));
            }
            prev.record.chain_hash_hex
        }
        None => "0".repeat(64),
    };

    let mut record = ReceiptRecord {
        version: RECEIPT_RECORD_VERSION,
        instruction_id: 0,
        activity_idx: 0,
        activity: Some("ggen.sync".to_string()),
        node_kind: 0,
        ts_ns: 0,
        duration_ms: None,
        object_ids: vec![format!("law:{}", &payload_hash_hex[..16])],
        payload_hash_hex,
        prev_chain_hash_hex,
        chain_hash_hex: String::new(),
        andon: Andon::Green,
        obligation_count: 0,
        signature_hex: None,
    };
    let chain = record
        .recompute_chain_hash()
        .map_err(|e| AppError::fm_chain(2, format!("receipt chain computation failed: {e}")))?;
    record.chain_hash_hex = hex32(&chain);

    // Sign the chain hash (T063): resolve the signing key per the documented
    // policy (`GGEN_SIGNING_KEY` env var, else `.ggen/keys/signing.key`, else
    // generate+persist a fresh keypair), sign `chain_hash_hex`'s UTF-8 bytes,
    // and hex-encode the raw ed25519 signature into `signature_hex`. Fails
    // closed: a malformed env var/key file is a hard error, never a silently
    // unsigned receipt.
    {
        use ed25519_dalek::Signer as _;
        let signing_key = crate::keys::resolve_signing_key(root)?;
        let signature = signing_key.sign(record.chain_hash_hex.as_bytes());
        record.signature_hex = Some(hex::encode(signature.to_bytes()));
    }

    let receipt = SyncReceipt { record, payload };
    if let Some(parent) = receipt_path.parent() {
        std::fs::create_dir_all(parent)?;
    }

    // Append-only history first (it is the chain's source of truth for the
    // next sync), head pointer second: if the head write fails, a re-run
    // chains from the log tail and the history stays linear.
    let mut line = serde_json::to_vec(&receipt)?;
    line.push(b'\n');
    std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_path)
        .and_then(|mut f| f.write_all(&line))
        .map_err(|e| {
            AppError::fm_chain(
                4,
                format!("receipt log `{}` append failed: {e}", log_path.display()),
            )
        })?;
    std::fs::write(&receipt_path, serde_json::to_vec(&receipt)?)?;
    Ok(())
}

/// Lowercase hex of a 32-byte hash.
pub(crate) fn hex32(bytes: &[u8; 32]) -> String {
    use std::fmt::Write as _;
    let mut s = String::with_capacity(64);
    for b in bytes {
        let _ = write!(s, "{b:02x}");
    }
    s
}

#[cfg(test)]
mod tests {
    use super::hash_file_or_missing;

    /// A declared closure input that cannot be read binds as the literal
    /// `MISSING` marker — recorded in the receipt, never dropped.
    #[test]
    fn unreadable_closure_input_binds_as_missing() {
        let dir = tempfile::tempdir().expect("tempdir");
        let gone = dir.path().join("deleted.tmpl");
        assert_eq!(hash_file_or_missing(&gone), "MISSING");

        let real = dir.path().join("real.tmpl");
        std::fs::write(&real, b"body").expect("write");
        assert_eq!(
            hash_file_or_missing(&real),
            blake3::hash(b"body").to_hex().to_string()
        );
    }
}
