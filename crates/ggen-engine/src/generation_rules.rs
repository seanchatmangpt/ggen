//! The declarative `[[generation.rules]]` sync path.
//!
//! Additive to (never a replacement of) [`crate::sync::sync`]'s existing
//! frontmatter-per-template-file convention (`crate::config::GgenConfig` +
//! `crate::template::Frontmatter`): a project's `ggen.toml` is either a
//! frontmatter project or a declarative-rules project, decided once by
//! `crate::schema_dispatch::load` (backed by the shared
//! `ggen_config::classify_ggen_toml` structural classifier) before any
//! typed parse runs — see that module's own doc comment for the full
//! dispatch design (it replaced this module's original narrower
//! `has_generation_rules` raw-text pre-parse, specs/014-ggen-core-replacement
//! correction 2 / Blocker A part 2). `crate::config::GgenConfig`
//! (`deny_unknown_fields`) would refuse a `[[generation.rules]]` project
//! outright; this module never sees a frontmatter project. An existing
//! frontmatter project (no `[generation]` table, or one with an
//! empty/absent `rules` array) is entirely unaffected — the classifier
//! reports `Frontmatter` and [`crate::sync::sync`] falls through to the
//! unchanged original path.
//!
//! # Design (specs/014-ggen-core-replacement, T070)
//!
//! Ported from ggen-core's `codegen::pipeline::GenerationPipeline::execute_generation_rules`
//! *by observable behavior*, not verbatim: this crate's own `GraphEngine` trait, Tera
//! integration (`crate::template::build_tera`/`solutions_to_values`), and write/receipt
//! conventions (`crate::write::resolve_target`, `crate::sync::write_receipt`) are reused
//! directly instead of re-deriving ggen-core's parallel implementations of the same
//! concepts.
//!
//! # Scope — deliberately bounded, not a silent gap
//!
//! Implemented and tested:
//! - `QuerySource::{File, Inline}`, `TemplateSource::{File, Inline}`.
//! - `GenerationRule.when` (SPARQL ASK guard) and `.skip_empty`.
//! - Per-row vs. static rendering, selected by `output_file.contains("{{")` (same rule
//!   `crate::sync::sync` uses for `Frontmatter.to`).
//! - `GenerationMode::{Create, Overwrite, Merge}` — `Merge` ports
//!   ggen-core's `codegen::merge::merge_sections` marker algorithm verbatim (see the nested
//!   `merge` module below), mapped onto this crate's own [`AppError`].
//! - A sync receipt chained through [`crate::sync::write_receipt`] exactly like the
//!   frontmatter path, so both schemas share one receipt history per project.
//! - [`validate_rendered_body`] — ported *by check* (not verbatim) from ggen-core's
//!   `validate_generated_output`'s E0004 (empty output) and E0005 (over
//!   [`crate::write::MAX_OUTPUT_BYTES`]) checks. Its third check, E0006 (`../` path
//!   traversal), is intentionally not re-implemented: [`decide_and_maybe_apply`]'s call to
//!   [`crate::write::resolve_target`] already refuses any `to` that escapes the project root,
//!   a strictly stronger guarantee than a substring check.
//! - [`enforce_validation_policy`] — `[validation].no_unsafe` (refuses rendered output
//!   containing the `unsafe` keyword) and `[validation].validate_syntax` (refused loudly, by
//!   design — see the "Deliberately deferred" bullet below for why) are both read here. Neither
//!   had a reader anywhere in this crate before this fix (red-team finding F2) — a manifest
//!   setting either flag got silent, unconditional success regardless of the rendered output.
//!   Load-bearing proof: `tests/generation_rules_e2e.rs`'s
//!   `no_unsafe_refuses_rendered_output_containing_unsafe_block` and
//!   `validate_syntax_true_refuses_rs_output_as_not_yet_implemented`.
//! - `[[inference.rules]]` — sorted by `.order`, each an optional `when:` ASK guard then a
//!   CONSTRUCT query whose derived triples are folded into the graph *before* any
//!   `[[generation.rules]]` query runs, exactly matching ggen-core's stage order
//!   (`execute_inference_rules` before `execute_generation_rules`). Load-bearing proof:
//!   `tests/generation_rules_e2e.rs`'s
//!   `inference_rule_construct_is_visible_to_generation_rule_query` and
//!   `inference_rule_when_guard_false_skips_construct`.
//! - The law gate (N3 rule materialization + denial check, SPARQL gate queries) — reuses
//!   `crate::sync::sync`'s exact frontmatter-path stage (same `GraphEngine` calls, same
//!   `[FM-LAW-*]` refusal shape, same `crate::sync::{parse_gate_source,evaluate_gate}`
//!   helpers) rather than re-deriving it. Reads N3 rules from `manifest.law.rules` and
//!   SPARQL gate files from `manifest.validation.gates` (**not** a duplicated `law.gates`
//!   field — see [`ggen_config::manifest::types::GgenManifest`]'s struct doc comment for
//!   why; a legacy non-empty `manifest.validation.shacl` is a loud `[FM-LAW-017]`
//!   migration refusal, never silently ignored). Load-bearing proof:
//!   `tests/generation_rules_e2e.rs`'s `law_gate_denial_violation_refuses_declarative_rules_sync`
//!   and `law_gate_violation_refuses_declarative_rules_sync_naming_offending_node`.
//! - `[[validation.rules]]` (inline named ASK-based custom validation rules, distinct from
//!   the file-based `[validation].gates` above) — each rule's `ask` query is evaluated
//!   against the same post-inference/post-law graph in the same validate stage, `true`
//!   meaning the rule holds. An `Error`-severity violation is a loud `[FM-LAW-020]` refusal
//!   before any file is written (same "gate precedes writes" invariant as `.gates`); a
//!   `Warning`-severity violation is logged via `tracing::warn!` and the sync continues.
//!   Previously parsed but never executed — see git history for the pre-fix silent-inert
//!   state. Load-bearing proof: `tests/generation_rules_e2e.rs`'s
//!   `validation_rule_error_severity_violation_refuses_declarative_rules_sync` and
//!   `validation_rule_warning_severity_violation_logs_and_continues`.
//! - `generation.output_dir` — joined onto every rule's (already-rendered) `output_file`
//!   before the write-target is resolved, for both the per-row and static branches. Default
//!   `"."` is a no-op (unchanged behavior for every existing project that never set it).
//!   Previously parsed but never read — see git history for the pre-fix silent-inert state.
//!   Load-bearing proof: `tests/generation_rules_e2e.rs`'s
//!   `output_dir_is_joined_onto_rendered_output_file`.
//! - `QuerySource::Pack` / `TemplateSource::Pack` — resolves `pack`'s `[[packs]]`-declared
//!   local `path`, then `output`/`file` through that pack's `package.toml` `[pack.outputs]`
//!   table ([`resolve_pack_root`]/[`resolve_pack_file`]) — NOT `crate::pack::Pack`'s heavier
//!   marketplace/git-fetch model (see those functions' doc comments for why this is a
//!   deliberately separate, lighter-weight convention).
//!
//! Deliberately deferred (a typed, loud [`AppError::fm_gen`] refusal naming the rule and the
//! unimplemented variant — never a silent skip or a decorative success):
//! - `TemplateSource::{Git, Package}` — a future implementation should reuse
//!   `crate::pack`'s existing git-clone-and-cache convention (`.ggen-v2/git-packs/<name>/` +
//!   `.ggen-git-pin`), not re-derive ggen-core's original one-shot clone.
//! - `[validation].validate_syntax` (ggen-core's Rust-syntax-check concept) — refused loudly
//!   (`[FM-GEN-016]`, via [`enforce_validation_policy`]) rather than silently ignored whenever a
//!   manifest sets it to `true` and a rule targets a `.rs` output: this crate has no
//!   Rust-parser dependency (e.g. `syn`) wired in to honestly perform the check, and adding one
//!   is a Cargo.toml/dependency-graph change out of scope for the fix that closed this bullet's
//!   sibling, `[validation].no_unsafe` (now implemented and tested — see
//!   [`enforce_validation_policy`] above, no longer in this deferred list).
//!
//! No atomic multi-file transaction/rollback type exists in this crate (none did before this
//! module either): every rule's query+template renders into memory across a first pass
//! before any write in a second pass runs, so a later rule's render failure never leaves an
//! earlier rule's write on disk from the same run — the same render-all-then-write-all
//! boundary `crate::sync::sync` itself already relies on.

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};

use ggen_config::manifest::{
    GenerationMode, GenerationRule, GgenManifest, PackRef, PackageToml, QuerySource,
    TemplateSource, ValidationSeverity,
};
use tera::Value;

use crate::{
    error::{AppError, Result, TemplateFailureCause},
    graph::{EngineQueryResults, GraphEngine, TurtleDocument},
    sync::{
        hash_file_or_missing, hex32, new_graph_engine, read_ontology_file, rel_display,
        write_receipt, SyncOptions, SyncReport,
    },
    template::{
        build_tera, classify_tera_render_error, solutions_to_values, tera_error_full_chain,
        tera_error_location,
    },
};

/// `elapsed.as_millis()` as a `u64` for an OTEL span attribute, saturating
/// instead of silently wrapping on the practically-unreachable case of a
/// pipeline stage running longer than `u64::MAX` milliseconds (~584 million
/// years). Shared with [`crate::sync::sync`]'s identical pipeline-stage
/// timing need.
pub(crate) fn duration_ms(elapsed: std::time::Duration) -> u64 {
    u64::try_from(elapsed.as_millis()).unwrap_or(u64::MAX)
}

/// Run every `[[generation.rules]]` entry in `manifest` against a fresh
/// graph loaded from `manifest.ontology`, producing the same
/// [`SyncReport`]/receipt shape [`crate::sync::sync`]'s frontmatter path
/// produces — see the module doc comment for the full design and scope.
///
/// # Errors
/// Fails closed on any resolve/query/render/write failure, or on an
/// unimplemented `QuerySource`/`TemplateSource` variant (`[FM-GEN-*]`).
// One sequential five-stage pipeline orchestrator (load/extract/validate/
// generate/emit), each stage carrying its own OTEL span open/close pair;
// splitting it into sub-functions would scatter that span bracketing across
// call boundaries rather than shrink real complexity.
#[allow(clippy::too_many_lines)]
pub(crate) fn run(root: &Path, manifest: &GgenManifest, opts: SyncOptions) -> Result<SyncReport> {
    // ── Resolve: ontology + imports ──────────────────────────────────────
    let load_start = Instant::now();
    let load_span = tracing::info_span!(
        "pipeline.load",
        "operation.name" = "pipeline.load",
        "operation.type" = "pipeline",
        "pipeline.stage" = "load",
        "pipeline.duration_ms" = tracing::field::Empty,
    );
    let load_guard = load_span.enter();

    let graph: Arc<dyn GraphEngine> = new_graph_engine(opts.engine)?;

    let mut closure: BTreeMap<String, String> = BTreeMap::new();
    closure.insert(
        "actuator".to_string(),
        concat!("ggen@", env!("CARGO_PKG_VERSION")).to_string(),
    );

    let ontology_path = root.join(&manifest.ontology.source);
    let (ontology_label, ttl) = read_ontology_file(root, &ontology_path)?;
    closure.insert(ontology_label.clone(), hash_file_or_missing(&ontology_path));
    let mut ontology_sources = Vec::with_capacity(1 + manifest.ontology.imports.len());
    ontology_sources.push((ontology_label, ttl));

    for import in &manifest.ontology.imports {
        let import_path = root.join(import);
        let (import_label, import_ttl) = read_ontology_file(root, &import_path)?;
        closure.insert(import_label.clone(), hash_file_or_missing(&import_path));
        ontology_sources.push((import_label, import_ttl));
    }
    let ontology_documents: Vec<TurtleDocument<'_>> = ontology_sources
        .iter()
        .map(|(label, content)| TurtleDocument::new(label, content))
        .collect();
    let ontology_receipt = graph.insert_turtle_documents(&ontology_documents)?;
    tracing::debug!(
        ontology.documents = ontology_receipt.documents,
        ontology.parsed_quads = ontology_receipt.parsed_quads,
        ontology.inserted_quads = ontology_receipt.inserted_quads,
        "ontology batch admitted"
    );

    drop(load_guard);
    load_span.record("pipeline.duration_ms", duration_ms(load_start.elapsed()));

    // ── Inference — `[[inference.rules]]` CONSTRUCT materialization ───────
    //
    // Ported from ggen-core's `codegen::pipeline::execute_inference_rules` *by
    // observable behavior*: sorted by `.order`, each an optional `when:` ASK
    // guard then a CONSTRUCT query whose derived triples are folded back into
    // the graph before any generation rule runs (so a generation rule's SELECT
    // can see inference-derived facts, matching ggen-core's stage order:
    // inference before generation). Absent/empty `[[inference.rules]]` runs
    // nothing here — pre-inference declarative-rules projects are unaffected.
    let extract_start = Instant::now();
    let extract_span = tracing::info_span!(
        "pipeline.extract",
        "operation.name" = "pipeline.extract",
        "operation.type" = "pipeline",
        "pipeline.stage" = "extract",
        "pipeline.duration_ms" = tracing::field::Empty,
    );
    let extract_guard = extract_span.enter();
    {
        let mut ordered: Vec<&ggen_config::manifest::InferenceRule> =
            manifest.inference.rules.iter().collect();
        ordered.sort_by_key(|r| r.order);
        for rule in ordered {
            if let Some(ask) = rule.when.as_deref() {
                match graph.query(ask)? {
                    EngineQueryResults::Boolean(true) => {}
                    EngineQueryResults::Boolean(false) => continue,
                    _ => {
                        return Err(AppError::fm_gen(
                            13,
                            format!(
                                "inference rule `{}`: `when:` must be an ASK query. \
                                 Remediation: use ASK {{ … }}.",
                                rule.name
                            ),
                        ));
                    }
                }
            }
            let EngineQueryResults::Graph(derived) = graph.query(&rule.construct)? else {
                return Err(AppError::fm_gen(
                    14,
                    format!(
                        "inference rule `{}`: `construct:` must be a CONSTRUCT query. \
                         Remediation: use CONSTRUCT {{ … }} WHERE {{ … }}.",
                        rule.name
                    ),
                ));
            };
            if !derived.is_empty() {
                use std::fmt::Write as _;
                let doc: String = derived.iter().fold(String::new(), |mut doc, t| {
                    let _ = writeln!(doc, "{} .", t.ntriples);
                    doc
                });
                graph.insert_turtle(&doc)?;
            }
        }
    }

    drop(extract_guard);
    extract_span.record("pipeline.duration_ms", duration_ms(extract_start.elapsed()));

    // ── Law gate — N3 rule materialization + denial/SPARQL-gate checks ────
    //
    // Reuses the exact stage `crate::sync::sync` already runs for frontmatter
    // projects (same `GraphEngine::{load_rules,materialize,check_denials}`
    // calls, same `crate::sync::{parse_gate_source,evaluate_gate}` gate
    // helpers, same `[FM-LAW-*]` refusal shape) rather than re-deriving it —
    // `manifest.law.rules` (N3/Datalog) and `manifest.validation.gates`
    // (SPARQL gate files; deliberately not duplicated as `law.gates` — see
    // `ggen_config::manifest::types::GgenManifest`'s struct doc comment) are
    // this schema's equivalents of `GgenConfig.law.{rules,gates}`.
    // Absent/empty on both fields runs no law stage at all, so pre-law
    // declarative-rules projects sync unchanged.
    let validate_start = Instant::now();
    let validate_span = tracing::info_span!(
        "pipeline.validate",
        "operation.name" = "pipeline.validate",
        "operation.type" = "pipeline",
        "pipeline.stage" = "validate",
        "pipeline.duration_ms" = tracing::field::Empty,
    );
    let validate_guard = validate_span.enter();

    if !manifest.law.rules.is_empty() {
        for rel in &manifest.law.rules {
            let rule_path = root.join(rel);
            let src = std::fs::read_to_string(&rule_path).map_err(|e| {
                AppError::fm_law(
                    15,
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
                16,
                format!(
                    "{} denial rule(s) violated after materialization: {}. \
                     Remediation: fix the facts or the denial rules in [law].rules.",
                    denials.len(),
                    denials.join("; ")
                ),
            ));
        }
    }
    // `[validation].shacl` (SHACL) was replaced by `[validation].gates`
    // (SPARQL gate queries, below). The field stays deserializable so a
    // legacy manifest gets THIS clear, typed refusal instead of a serde
    // unknown-field error — a file that used to be law must never be
    // silently ignored.
    if !manifest.validation.shacl.is_empty() {
        return Err(AppError::fm_law(
            17,
            format!(
                "[validation].shacl ({} SHACL shapes file(s) declared) is no longer \
                 supported at sync time; SHACL shape gates were replaced by \
                 engine-independent SPARQL gate queries. Remediation: migrate to \
                 [validation].gates = [\"…/*.rq\"] — each file holds one ASK (true = \
                 violation) or SELECT (any row = violation) query, optionally \
                 preceded by `# MESSAGE: <text>` comment lines.",
                manifest.validation.shacl.len()
            ),
        ));
    }
    for rel in &manifest.validation.gates {
        let gate_path = root.join(rel);
        let src = std::fs::read_to_string(&gate_path).map_err(|e| {
            AppError::fm_law(
                17,
                format!(
                    "SPARQL gate file `{}` unreadable: {e}. Remediation: fix [validation].gates.",
                    gate_path.display()
                ),
            )
        })?;
        closure.insert(
            rel_display(root, &gate_path),
            hash_file_or_missing(&gate_path),
        );
        let gate = crate::sync::parse_gate_source(&src);
        match crate::sync::evaluate_gate(graph.as_ref(), &gate.query)? {
            crate::sync::GateOutcome::Pass => {}
            crate::sync::GateOutcome::NotAGate => {
                return Err(AppError::fm_law(
                    17,
                    format!(
                        "SPARQL gate `{}` is not a gate query: it must be an ASK \
                         (true = violation) or a SELECT (any row = violation), not \
                         a CONSTRUCT/DESCRIBE. Remediation: fix [validation].gates.",
                        rel.display()
                    ),
                ));
            }
            crate::sync::GateOutcome::Violation(detail) => {
                return Err(AppError::fm_law(
                    18,
                    format!(
                        "SPARQL gate `{}` refused the sync: {}{detail}. \
                         Remediation: fix the offending facts or the gate query.",
                        rel.display(),
                        gate.message_prefix(),
                    ),
                ));
            }
        }
    }

    // `[[validation.rules]]` — inline named ASK-based custom validation rules,
    // distinct from the file-based `[validation].gates` above: each `ask`
    // query is evaluated against the same post-inference/post-law graph,
    // `true` meaning the rule holds (valid), `false` meaning it is violated.
    // An `Error`-severity violation refuses the sync before any file is
    // written, exactly like a `.gates` violation; a `Warning`-severity
    // violation is logged and the sync continues. Absent/empty
    // `[[validation.rules]]` runs nothing here, so pre-existing declarative-
    // rules projects are unaffected.
    for rule in &manifest.validation.rules {
        match graph.query(&rule.ask)? {
            EngineQueryResults::Boolean(true) => {}
            EngineQueryResults::Boolean(false) => match rule.severity {
                ValidationSeverity::Error => {
                    return Err(AppError::fm_law(
                        20,
                        format!(
                            "validation rule `{}` violated: {}. \
                             Remediation: fix the underlying facts, or set \
                             `severity = \"Warning\"` on this [[validation.rules]] entry \
                             if it should log instead of refusing the sync.",
                            rule.name, rule.description
                        ),
                    ));
                }
                ValidationSeverity::Warning => {
                    tracing::warn!(
                        validation.rule = %rule.name,
                        validation.description = %rule.description,
                        "validation rule violated (severity=Warning): sync continues"
                    );
                }
            },
            _ => {
                return Err(AppError::fm_law(
                    19,
                    format!(
                        "validation rule `{}`: `ask` must be an ASK query. \
                         Remediation: use ASK {{ … }}.",
                        rule.name
                    ),
                ));
            }
        }
    }

    drop(validate_guard);
    validate_span.record(
        "pipeline.duration_ms",
        duration_ms(validate_start.elapsed()),
    );

    let graph_hash_hex = hex32(&graph.state_hash()?);

    // ── Extract + Render every rule into memory (no writes yet) ──────────
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
    let mut pending: Vec<PendingGenWrite> = Vec::new();
    // Reported in every `[FM-GEN-008]` message this loop produces (see
    // `AppError::fm_gen_render_failure`) — the project root every rule's
    // `TemplateSource`/`QuerySource` is resolved against.
    let root_display = root.display().to_string();

    for rule in &manifest.generation.rules {
        if let Some(ask) = rule.when.as_deref() {
            match graph.query(ask)? {
                EngineQueryResults::Boolean(true) => {}
                EngineQueryResults::Boolean(false) => {
                    let reason = format!("when guard false (rule `{}`)", rule.name);
                    decisions.insert(rule.output_file.clone(), format!("skipped: {reason}"));
                    skipped.push((PathBuf::from(&rule.output_file), reason));
                    continue;
                }
                _ => {
                    return Err(AppError::fm_gen(
                        1,
                        format!(
                            "rule `{}`: `when:` must be an ASK query. \
                             Remediation: use ASK {{ … }}.",
                            rule.name
                        ),
                    ));
                }
            }
        }

        let query_text =
            resolve_query_source(root, &manifest.packs, rule, &rule.query, &mut closure)?;
        let rows = match graph.query(&query_text)? {
            EngineQueryResults::Solutions(rows) => rows,
            EngineQueryResults::Boolean(_) => {
                return Err(AppError::fm_gen(
                    2,
                    format!(
                        "rule `{}`: query must be a SELECT (got an ASK). \
                         Remediation: use `when:` for ASK guards, `query:` for SELECT.",
                        rule.name
                    ),
                ));
            }
            EngineQueryResults::Graph(_) => {
                return Err(AppError::fm_gen(
                    2,
                    format!(
                        "rule `{}`: query must be a SELECT (got a CONSTRUCT/DESCRIBE). \
                         Remediation: use a SELECT query.",
                        rule.name
                    ),
                ));
            }
        };

        if rows.is_empty() && rule.skip_empty {
            let reason = "skip_empty: query returned no rows".to_string();
            decisions.insert(rule.output_file.clone(), format!("skipped: {reason}"));
            skipped.push((PathBuf::from(&rule.output_file), reason));
            continue;
        }

        let row_values = solutions_to_values(rows);
        let template_text =
            resolve_template_source(root, &manifest.packs, rule, &rule.template, &mut closure)?;
        let template_descriptor = template_source_descriptor(&rule.template);

        // Cluster B structural guard: a YAML file-tree meta-spec
        // (`structure:` + `foreach:`) is not Tera content, but Tera happily
        // parses it anyway (its literal `{{ project.name }}` markers ARE
        // syntactically valid Tera expressions) and only fails at RENDER
        // time with a plain "Variable ... not found" message —
        // indistinguishable on the surface from Cluster D's real bug.
        // Detect the shape structurally, before Tera ever sees the text, so
        // the typed cause names the actual capability gap instead of
        // misclassifying it as a missing context key.
        if detect_file_tree_meta_spec(&template_text) {
            return Err(AppError::fm_gen_render_failure(
                TemplateFailureCause::TemplateSchemaIncompatible,
                &root_display,
                &template_descriptor,
                &rule.name,
                "template is a YAML file-tree meta-spec (`structure:` + `foreach:` \
                 directives), not Tera content — ggen-engine's declarative-rules path \
                 has no file-tree/foreach interpreter yet. Remediation: rewrite the rule \
                 as one flat Tera template per output file, or track the interpreter as \
                 a follow-up (see specs/014-ggen-core-replacement/tasks.md).",
                None,
            ));
        }

        // Ephemeral per-rule template name so Tera errors can point back at
        // the rule; the body itself is registered fresh for each rule (an
        // inline/file template has no stable path to reuse as a Tera name
        // across rules).
        let tpl_name = format!("generation_rule::{}", rule.name);
        tera.add_raw_template(&tpl_name, &template_text)
            .map_err(|e| {
                AppError::fm_gen_render_failure(
                    TemplateFailureCause::TemplateParseFailed,
                    &root_display,
                    &template_descriptor,
                    &rule.name,
                    format!("template rejected by Tera: {}", tera_error_full_chain(&e)),
                    tera_error_location(&e).as_deref(),
                )
            })?;

        let per_row = rule.output_file.contains("{{");
        if per_row {
            for row in &row_values {
                let mut ctx = tera::Context::new();
                ctx.insert("results", &row_values);
                // Alias for templates authored against the (documented but
                // never actually supplied — see Cluster D's root-cause
                // writeup) `sparql_results` binding name. Both names carry
                // identical row data; templates may use either.
                ctx.insert("sparql_results", &row_values);
                ctx.insert("row", row);
                if let Value::Object(map) = row {
                    for (k, v) in map {
                        ctx.insert(k, v);
                    }
                }
                let to = render_output_file(
                    &mut tera,
                    &rule.output_file,
                    &ctx,
                    &rule.name,
                    &root_display,
                    &template_descriptor,
                )?;
                let to = join_output_dir(&manifest.generation.output_dir, &to);
                let body = render_template(
                    &mut tera,
                    &tpl_name,
                    &ctx,
                    &rule.name,
                    &root_display,
                    &template_descriptor,
                )?;
                validate_rendered_body(&rule.name, &to, &body)?;
                enforce_validation_policy(manifest, &rule.name, &to, &body)?;
                pending.push(PendingGenWrite {
                    to,
                    body,
                    mode: rule.mode.clone(),
                });
            }
        } else {
            let mut ctx = tera::Context::new();
            ctx.insert("results", &row_values);
            ctx.insert("sparql_results", &row_values);
            // Static (non-per-row) rules never flattened a single row's
            // columns onto the top-level context before this fix — only
            // the per-row branch above did. Flatten the FIRST row's
            // columns (same "first row wins" convention `sparql_first()`
            // already uses), matching the per-row branch's behavior for
            // the common one-row/static-output-file case (Cluster D,
            // `examples/llm-full-integration`'s bare `{{ agent_name }}`).
            if let Some(Value::Object(map)) = row_values.first() {
                for (k, v) in map {
                    ctx.insert(k, v);
                }
            }
            let body = render_template(
                &mut tera,
                &tpl_name,
                &ctx,
                &rule.name,
                &root_display,
                &template_descriptor,
            )?;
            let to = join_output_dir(&manifest.generation.output_dir, &rule.output_file);
            validate_rendered_body(&rule.name, &to, &body)?;
            enforce_validation_policy(manifest, &rule.name, &to, &body)?;
            pending.push(PendingGenWrite {
                to,
                body,
                mode: rule.mode.clone(),
            });
        }
    }

    // Two rendered rules (or two rows of one rule) resolving to the same
    // target would silently last-row-win on disk — refuse instead, same
    // invariant `crate::sync::sync` enforces for frontmatter templates.
    {
        let mut seen: BTreeMap<&str, usize> = BTreeMap::new();
        for pw in &pending {
            *seen.entry(pw.to.as_str()).or_default() += 1;
        }
        if let Some((to, n)) = seen.into_iter().find(|(_, n)| *n > 1) {
            return Err(AppError::fm_gen(
                4,
                format!(
                    "{n} rendered generation rules target the same output `{to}`. \
                     Remediation: make `output_file` unique per row/rule."
                ),
            ));
        }
    }

    drop(generate_guard);
    generate_span.record(
        "pipeline.duration_ms",
        duration_ms(generate_start.elapsed()),
    );

    // ── Write every already-rendered rule ─────────────────────────────────
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
        match decide_and_maybe_apply(root, &pw.to, &pw.body, &pw.mode, opts.dry_run)? {
            GenWriteOutcome::Written => {
                decisions.insert(pw.to.clone(), "written".to_string());
                written.push(PathBuf::from(&pw.to));
            }
            GenWriteOutcome::PlannedWrite => {
                decisions.insert(pw.to.clone(), "planned: write (dry-run)".to_string());
                written.push(PathBuf::from(&pw.to));
            }
            GenWriteOutcome::Skipped(reason) => {
                decisions.insert(pw.to.clone(), format!("skipped: {reason}"));
                skipped.push((PathBuf::from(&pw.to), reason));
            }
        }
    }

    drop(emit_guard);
    emit_span.record("pipeline.duration_ms", duration_ms(emit_start.elapsed()));
    emit_span.record("pipeline.files_generated", written.len() as u64);

    let report = SyncReport {
        written,
        skipped,
        graph_hash_hex,
        decisions,
        packs: BTreeMap::new(),
        closure,
    };

    if !opts.dry_run {
        write_receipt(root, &report, graph.as_ref(), opts.receipt_origin)?;
    }
    Ok(report)
}

/// One fully-rendered generation rule (or one rendered row of a per-row
/// rule) awaiting `decide_and_maybe_apply` — the same render/write
/// boundary [`crate::sync::sync`] uses for frontmatter templates.
struct PendingGenWrite {
    to: String,
    body: String,
    mode: GenerationMode,
}

/// Join `manifest.generation.output_dir` onto a rule's already-rendered
/// (per-row or static) relative `output_file`, producing the final path
/// [`decide_and_maybe_apply`] resolves and writes to.
///
/// The default `output_dir` (`"."`) is special-cased to return `rel_to`
/// unchanged: naively `Path::join`-ing `"."` onto `rel_to` produces a
/// leading `./` — a genuinely *different* [`PathBuf`] (a leading `.`
/// normalizes to its own [`std::path::Component::CurDir`], so `"./out/x"`
/// and `"out/x"` do not compare equal) even though the two paths address the
/// same file on disk. Every existing declarative-rules project that never
/// set `output_dir` must keep writing to (and reporting) exactly the same
/// targets as before this function existed, byte-for-byte in
/// `SyncReport::written` — this special case is what makes that true rather
/// than merely "the same file, differently spelled". A non-default
/// `output_dir` is prepended to every rule's output —
/// [`crate::write::resolve_target`] (called downstream by
/// [`decide_and_maybe_apply`]) still refuses the *final* joined path if it
/// escapes the project root or contains a traversal component, so a
/// malicious/malformed `output_dir` is caught there, not silently trusted
/// here.
fn join_output_dir(output_dir: &Path, rel_to: &str) -> String {
    if output_dir.as_os_str().is_empty() || output_dir == Path::new(".") {
        return rel_to.to_string();
    }
    output_dir.join(rel_to).to_string_lossy().into_owned()
}

/// Resolve `pack_name` (as referenced by a rule's `QuerySource::Pack` /
/// `TemplateSource::Pack`) against the manifest's declared `[[packs]]`, to
/// that pack's root directory on disk.
///
/// Only `PackRef`s with a local `path` are supported here — a pack declared
/// with `registry != "local"` (no `path`) has no on-disk root to resolve
/// query/template files against in this path (it would need the full
/// marketplace/git pack-fetch machinery in [`crate::pack`], a separate,
/// heavier convention — see that module's doc comment — not this
/// lightweight `[pack.outputs]`-keyed file lookup).
///
/// A pack name absent from `packs` is defensive-only: `[E0014]` in
/// `ggen-config`'s manifest validation already refuses an undeclared pack
/// before the sync pipeline ever reaches this function.
fn resolve_pack_root(root: &Path, packs: &[PackRef], pack_name: &str) -> Result<PathBuf> {
    let pack_ref = packs.iter().find(|p| p.name == pack_name).ok_or_else(|| {
        AppError::fm_gen(
            6,
            format!(
                "pack `{pack_name}` is not declared in [[packs]]. \
                 Remediation: add a `[[packs]]` entry with this `name`."
            ),
        )
    })?;
    let pack_path = pack_ref.path.as_ref().ok_or_else(|| {
        AppError::fm_gen(
            6,
            format!(
                "pack `{pack_name}` has registry `{}` with no local `path` — only local packs \
                 are resolvable for QuerySource::Pack/TemplateSource::Pack. \
                 Remediation: vendor the pack locally (a `[[packs]]` entry with `path = ...`), \
                 or use QuerySource::File/TemplateSource::File pointing at an already-fetched copy.",
                pack_ref.registry
            ),
        )
    })?;
    Ok(root.join(pack_path))
}

/// Resolve a pack-relative `(output, file)` pair (as used by
/// `QuerySource::Pack`/`TemplateSource::Pack`) to the file's real path on
/// disk, via that pack's `package.toml` `[pack.outputs]` table.
///
/// [`PackageToml::load`]/[`PackageToml::resolve_output_key`] fail OPEN by
/// design (missing/unparseable `package.toml`, or no matching `output` key,
/// both silently fall back to treating `output` as a literal directory
/// name) — that fallback is real and can resolve to the WRONG file if the
/// literal `output` string happens to also be a real subdirectory of the
/// pack that isn't what the pack author intended via `[pack.outputs]`.
/// `[FM-GEN-006]`'s prior state was a loud hard error for this whole path;
/// silently swallowing that fallback here would be a real regression from
/// loud-refusal to silent-wrong-output, so it's logged at `warn` level
/// (rule-name-attributed) whenever the pack's `package.toml` doesn't
/// resolve `output` through its `[pack.outputs]` table — a human auditing
/// a sync's logs sees exactly when a rule is relying on the fallback.
fn resolve_pack_file(rule_name: &str, pack_root: &Path, output: &str, file: &Path) -> PathBuf {
    let package = PackageToml::load(pack_root);
    let output_dir = package.resolve_output_key(output);
    if output_dir == output {
        tracing::warn!(
            rule = rule_name,
            pack_root = %pack_root.display(),
            output,
            "no `[pack.outputs]` entry for `{output}` in this pack's package.toml (or none \
             found) — falling back to treating `{output}` as a literal subdirectory name; \
             verify that's actually correct, not a coincidental directory-name match"
        );
    }
    pack_root.join(output_dir).join(file)
}

/// Resolve a [`QuerySource`] to its SPARQL query text, binding any file it
/// reads into `closure`.
///
/// # Errors
/// `[FM-GEN-005]` on an unreadable query file; `[FM-GEN-006]` if the
/// referenced pack is undeclared or has no local `path` (see
/// [`resolve_pack_root`]).
fn resolve_query_source(
    root: &Path, packs: &[PackRef], rule: &GenerationRule, source: &QuerySource,
    closure: &mut BTreeMap<String, String>,
) -> Result<String> {
    match source {
        QuerySource::File { file } => {
            let path = root.join(file);
            let text = std::fs::read_to_string(&path).map_err(|e| {
                AppError::fm_gen(
                    5,
                    format!(
                        "rule `{}`: query file `{}` unreadable: {e}",
                        rule.name,
                        path.display()
                    ),
                )
            })?;
            closure.insert(rel_display(root, &path), hash_file_or_missing(&path));
            Ok(text)
        }
        QuerySource::Inline { inline } => Ok(inline.clone()),
        QuerySource::Pack { pack, output, file } => {
            let pack_root = resolve_pack_root(root, packs, pack)?;
            let path = resolve_pack_file(&rule.name, &pack_root, output, file);
            let text = std::fs::read_to_string(&path).map_err(|e| {
                AppError::fm_gen(
                    5,
                    format!(
                        "rule `{}`: query file `{}` (pack `{pack}`, output `{output}`) unreadable: {e}",
                        rule.name,
                        path.display()
                    ),
                )
            })?;
            closure.insert(rel_display(root, &path), hash_file_or_missing(&path));
            Ok(text)
        }
    }
}

/// Resolve a [`TemplateSource`] to its Tera template text, binding any file
/// it reads into `closure`.
///
/// # Errors
/// `[FM-GEN-005]` on an unreadable template file; `[FM-GEN-006]` if a
/// referenced pack is undeclared or has no local `path` (see
/// [`resolve_pack_root`]); `[FM-GEN-007]` for the still-not-implemented
/// `Git`/`Package` variants (see the module doc comment).
fn resolve_template_source(
    root: &Path, packs: &[PackRef], rule: &GenerationRule, source: &TemplateSource,
    closure: &mut BTreeMap<String, String>,
) -> Result<String> {
    match source {
        TemplateSource::File { file } => {
            let path = root.join(file);
            let text = std::fs::read_to_string(&path).map_err(|e| {
                AppError::fm_gen(
                    5,
                    format!(
                        "rule `{}`: template file `{}` unreadable: {e}",
                        rule.name,
                        path.display()
                    ),
                )
            })?;
            closure.insert(rel_display(root, &path), hash_file_or_missing(&path));
            Ok(text)
        }
        TemplateSource::Inline { inline } => Ok(inline.clone()),
        TemplateSource::Pack { pack, output, file } => {
            let pack_root = resolve_pack_root(root, packs, pack)?;
            let path = resolve_pack_file(&rule.name, &pack_root, output, file);
            let text = std::fs::read_to_string(&path).map_err(|e| {
                AppError::fm_gen(
                    5,
                    format!(
                        "rule `{}`: template file `{}` (pack `{pack}`, output `{output}`) unreadable: {e}",
                        rule.name,
                        path.display()
                    ),
                )
            })?;
            closure.insert(rel_display(root, &path), hash_file_or_missing(&path));
            Ok(text)
        }
        TemplateSource::Git { git, .. } => Err(AppError::fm_gen(
            7,
            format!(
                "rule `{}`: TemplateSource::Git (`{git}`) is not implemented yet. \
                 Remediation: vendor the template locally and use TemplateSource::File; \
                 see specs/014-ggen-core-replacement/tasks.md for the tracked follow-up \
                 (a future implementation should reuse crate::pack's existing \
                 git-clone-and-cache convention).",
                rule.name
            ),
        )),
        TemplateSource::Package { package, .. } => Err(AppError::fm_gen(
            7,
            format!(
                "rule `{}`: TemplateSource::Package (`{package}`) is not implemented yet. \
                 Remediation: vendor the template locally and use TemplateSource::File; \
                 see specs/014-ggen-core-replacement/tasks.md for the tracked follow-up.",
                rule.name
            ),
        )),
    }
}

/// A friendly, error-reportable descriptor for a [`TemplateSource`] — the
/// "template path" field every `[FM-GEN-008]` message reports (see
/// [`AppError::fm_gen_render_failure`]). Never used for resolution (that's
/// still [`resolve_template_source`]'s job) — purely for diagnostics.
fn template_source_descriptor(source: &TemplateSource) -> String {
    match source {
        TemplateSource::File { file } => file.display().to_string(),
        TemplateSource::Inline { .. } => "<inline>".to_string(),
        TemplateSource::Pack { pack, output, file } => {
            format!("pack:{pack}/{output}/{}", file.display())
        }
        TemplateSource::Git { git, path, .. } => format!("git:{git}/{}", path.display()),
        TemplateSource::Package { package, .. } => format!("package:{package}"),
    }
}

/// Structural pre-check: is `template_text` actually a YAML file-tree
/// meta-spec (Cluster B's `examples/clap-noun-verb-demo` gap — a
/// `structure:` list of per-file sub-templates with `foreach:` loop
/// directives) rather than literal Tera content?
/// [`crate::generation_rules`] has no interpreter for this shape (see the
/// module's own scope doc comment) — detecting it structurally, before
/// Tera ever sees the text, avoids misclassifying the resulting runtime
/// failure as a plain [`TemplateFailureCause::TemplateVariableMissing`]:
/// Tera happily parses the file's literal `{{ project.name }}` markers as
/// real expressions (they ARE syntactically valid Tera) and only fails at
/// render time, which looks identical to the Cluster D bug on the surface
/// — confirmed live for `examples/clap-noun-verb-demo`'s
/// `cli-template.yaml`. Structural, not a fragile `contains("foreach:")`
/// substring check: requires a top-level YAML mapping with a `structure:`
/// sequence where at least one entry is itself a mapping containing a
/// `foreach` key — the exact shape that file uses.
fn detect_file_tree_meta_spec(template_text: &str) -> bool {
    let Ok(serde_yaml::Value::Mapping(map)) = serde_yaml::from_str(template_text) else {
        return false;
    };
    let structure_key = serde_yaml::Value::String("structure".to_string());
    let Some(serde_yaml::Value::Sequence(structure)) = map.get(&structure_key) else {
        return false;
    };
    let foreach_key = serde_yaml::Value::String("foreach".to_string());
    structure
        .iter()
        .any(|entry| matches!(entry, serde_yaml::Value::Mapping(m) if m.contains_key(&foreach_key)))
}

/// Render `rule.output_file` through Tera (it may reference the same
/// context as the body, e.g. a per-row `{{name}}` path segment).
///
/// Always classified as [`TemplateFailureCause::TemplateOutputPathInvalid`]
/// regardless of the underlying `tera::ErrorKind`: from the operator's
/// perspective "your `output_file`/`to` pattern is broken" is the
/// actionable class whether the concrete cause is a missing variable, an
/// unknown filter, or a syntax error in the pattern itself (`render_str`
/// re-parses `output_file` on every call, so a parse failure surfaces
/// here, not at the one-time `add_raw_template` above).
fn render_output_file(
    tera: &mut tera::Tera, output_file: &str, ctx: &tera::Context, rule_name: &str, example: &str,
    template: &str,
) -> Result<String> {
    tera.render_str(output_file, ctx).map_err(|e| {
        AppError::fm_gen_render_failure(
            TemplateFailureCause::TemplateOutputPathInvalid,
            example,
            template,
            rule_name,
            format!("output_file render failed: {}", tera_error_full_chain(&e)),
            tera_error_location(&e).as_deref(),
        )
    })
}

/// Render the rule's registered template body through Tera, sub-classified
/// via [`classify_tera_render_error`] (see [`TemplateFailureCause`]'s own
/// doc comment for the taxonomy this maps onto).
fn render_template(
    tera: &mut tera::Tera, tpl_name: &str, ctx: &tera::Context, rule_name: &str, example: &str,
    template: &str,
) -> Result<String> {
    tera.render(tpl_name, ctx).map_err(|e| {
        let cause = classify_tera_render_error(&e, tpl_name);
        AppError::fm_gen_render_failure(
            cause,
            example,
            template,
            rule_name,
            format!("template render failed: {}", tera_error_full_chain(&e)),
            tera_error_location(&e).as_deref(),
        )
    })
}

/// Sanity-check a freshly-rendered body before it is queued for writing.
/// Ported *by check*, not verbatim, from ggen-core's
/// `GenerationPipeline::validate_generated_output`'s first two checks
/// (E0004 empty content, E0005 oversized content) — its third check
/// (E0006, `../` path traversal in the output path) is not re-implemented
/// here because [`decide_and_maybe_apply`]'s call to
/// [`crate::write::resolve_target`] already refuses a `to` that escapes
/// the project root or contains a `..` component, a strictly stronger
/// guarantee than a substring check. Reuses [`crate::write::MAX_OUTPUT_BYTES`]
/// as the one size-cap constant this crate enforces, rather than a second
/// hardcoded `10 * 1024 * 1024` literal.
///
/// # Errors
/// `[FM-GEN-011]` empty rendered body; `[FM-GEN-012]` body exceeds
/// [`crate::write::MAX_OUTPUT_BYTES`].
fn validate_rendered_body(rule_name: &str, to: &str, body: &str) -> Result<()> {
    if body.is_empty() {
        return Err(AppError::fm_gen(
            11,
            format!(
                "rule `{rule_name}`: rendered output for `{to}` is empty. \
                 Remediation: check that the query returned rows (or that a static \
                 template body isn't blank) and that `skip_empty` is set if an empty \
                 result is expected and should be skipped instead of refused."
            ),
        ));
    }
    if body.len() > crate::write::MAX_OUTPUT_BYTES {
        return Err(AppError::fm_gen(
            12,
            format!(
                "rule `{rule_name}`: rendered output for `{to}` is {} bytes, over the \
                 {}-byte cap. Remediation: check the template for an unbounded loop over \
                 query results, or split it into multiple templates/output files.",
                body.len(),
                crate::write::MAX_OUTPUT_BYTES
            ),
        ));
    }
    Ok(())
}

/// Enforce `[validation].no_unsafe` and `[validation].validate_syntax` against a
/// freshly-rendered generation-rule body, in addition to
/// [`validate_rendered_body`]'s content-shape checks above. Closes red-team
/// finding F2: both flags were declared, parsed, and defaulted
/// (`ggen_config::manifest::types::ValidationConfig`) but had zero reader
/// anywhere in this crate before this function existed — a manifest setting
/// either flag got silent, unconditional success regardless of its rendered
/// output. Load-bearing proof: `tests/generation_rules_e2e.rs`'s
/// `no_unsafe_refuses_rendered_output_containing_unsafe_block` and
/// `validate_syntax_true_refuses_rs_output_as_not_yet_implemented`.
///
/// # Errors
/// `[FM-GEN-015]` if `no_unsafe = true` and `body` contains the `unsafe`
/// keyword at a word boundary (see [`contains_unsafe_keyword`]'s own doc
/// comment for the textual-heuristic tradeoff, symmetric with
/// `ggen_config::manifest::validation::query_has_order_by`'s documented
/// keyword search). `[FM-GEN-016]` if `validate_syntax = true` and `to` is a
/// `.rs` target: this crate has no Rust-parser dependency wired in to
/// honestly perform that check yet, so it refuses loudly instead of
/// silently reporting success.
fn enforce_validation_policy(
    manifest: &GgenManifest, rule_name: &str, to: &str, body: &str,
) -> Result<()> {
    if manifest.validation.no_unsafe && contains_unsafe_keyword(body) {
        return Err(AppError::fm_gen(
            15,
            format!(
                "rule `{rule_name}`: rendered output for `{to}` contains the `unsafe` \
                 keyword, refused because `[validation].no_unsafe = true`. \
                 Remediation: remove the unsafe block from the source template, or set \
                 `no_unsafe = false` in `[validation]` if unsafe code is intentional for \
                 this project."
            ),
        ));
    }
    if manifest.validation.validate_syntax
        && Path::new(to)
            .extension()
            .is_some_and(|ext| ext.eq_ignore_ascii_case("rs"))
    {
        return Err(AppError::fm_gen(
            16,
            format!(
                "rule `{rule_name}`: `[validation].validate_syntax = true` requests Rust \
                 syntax validation for `{to}`, but ggen-engine has no Rust-parser \
                 dependency wired in yet to honestly perform that check. \
                 Remediation: set `validate_syntax = false` in `[validation]` until real \
                 syntax validation ships, or validate generated output with an external \
                 `cargo check`/`rustc` step in your own pipeline."
            ),
        ));
    }
    Ok(())
}

/// Returns true if `body` contains the `unsafe` keyword at a word boundary
/// (not merely as a substring of a longer identifier such as `unsafely` or
/// `MyUnsafeType`). Detection is a plain byte scan for ASCII `unsafe`, not a
/// tokenizer/`syn`-based check — `unsafe` appearing inside a string literal
/// or comment in the rendered output would be a false positive. That
/// tradeoff is deliberate and mirrors
/// `ggen_config::manifest::validation::query_has_order_by`'s own documented
/// heuristic, except the conservative direction runs the other way here:
/// `no_unsafe` is an opt-in safety gate, so refusing on a possible false
/// positive is the correct default, unlike a determinism warning where a
/// false positive would only be noise.
#[must_use]
fn contains_unsafe_keyword(body: &str) -> bool {
    const NEEDLE: &[u8] = b"unsafe";
    let bytes = body.as_bytes();
    if bytes.len() < NEEDLE.len() {
        return false;
    }
    for start in 0..=(bytes.len() - NEEDLE.len()) {
        if &bytes[start..start + NEEDLE.len()] != NEEDLE {
            continue;
        }
        let before_is_ident = start > 0 && is_ident_byte(bytes[start - 1]);
        let after_idx = start + NEEDLE.len();
        let after_is_ident = after_idx < bytes.len() && is_ident_byte(bytes[after_idx]);
        if !before_is_ident && !after_is_ident {
            return true;
        }
    }
    false
}

/// ASCII identifier-continuation byte (`[A-Za-z0-9_]`), used by
/// [`contains_unsafe_keyword`] to check word boundaries around a candidate
/// `unsafe` match.
fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == b'_'
}

/// Outcome of [`decide_and_maybe_apply`].
enum GenWriteOutcome {
    /// The file was created, overwritten, or merged on disk.
    Written,
    /// Dry run: this write would have happened; nothing was touched.
    PlannedWrite,
    /// Nothing was written; the reason is recorded.
    Skipped(String),
}

/// Decide a [`GenerationMode`] write outcome for `rel_to` and, when
/// `dry_run` is `false`, apply it. Reading the existing target (to decide)
/// is not itself a side effect, so this function is safe to call
/// unconditionally; only the final `create_dir_all`/`write` are gated on
/// `!dry_run`, matching the "a dry run must have zero side effects"
/// invariant `crate::sync::apply` already enforces for frontmatter writes.
///
/// # Errors
/// `[FM-GEN-009]` if an existing target cannot be read as UTF-8;
/// propagates [`merge::merge_sections`] failures for `GenerationMode::Merge`.
fn decide_and_maybe_apply(
    root: &Path, rel_to: &str, body: &str, mode: &GenerationMode, dry_run: bool,
) -> Result<GenWriteOutcome> {
    let target = crate::write::resolve_target(root, rel_to)?;
    let existing = match std::fs::read_to_string(&target) {
        Ok(s) => Some(s),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
        Err(e) => {
            return Err(AppError::fm_gen(
                9,
                format!(
                    "target `{}` exists but is unreadable as UTF-8: {e}. \
                     Remediation: remove or fix the target file.",
                    target.display()
                ),
            ));
        }
    };

    let (final_body, plan) = match mode {
        GenerationMode::Create => {
            if existing.is_some() {
                return Ok(GenWriteOutcome::Skipped(
                    "mode=create: target already exists".to_string(),
                ));
            }
            (body.to_string(), GenWriteOutcome::Written)
        }
        GenerationMode::Overwrite => {
            if existing.as_deref() == Some(body) {
                return Ok(GenWriteOutcome::Skipped(
                    "unchanged: content identical".to_string(),
                ));
            }
            (body.to_string(), GenWriteOutcome::Written)
        }
        GenerationMode::Merge => {
            let merged = merge::merge_sections(body, existing.as_deref().unwrap_or(""))?;
            if existing.as_deref() == Some(merged.as_str()) {
                return Ok(GenWriteOutcome::Skipped(
                    "unchanged: merged content identical".to_string(),
                ));
            }
            (merged, GenWriteOutcome::Written)
        }
    };

    if dry_run {
        return Ok(GenWriteOutcome::PlannedWrite);
    }
    if let Some(parent) = target.parent() {
        std::fs::create_dir_all(parent)?;
    }
    std::fs::write(&target, &final_body)?;
    Ok(plan)
}

// ---------------------------------------------------------------------------
// merge — GenerationMode::Merge marker-based section merging
// ---------------------------------------------------------------------------

/// Marker-based merging for `mode = "Merge"`, ported *by algorithm* from
/// ggen-core's `codegen::merge` (verbatim marker format and merge logic,
/// `crate::utils::error::{Error, Result}` mapped onto this crate's own
/// [`AppError::fm_gen`] per this migration's established error-mapping
/// convention).
///
/// # Marker format
///
/// ```text
/// <<<<<<< GENERATED
/// // Generated code goes here
/// =======
/// // Manual code is preserved here
/// >>>>>>> MANUAL
/// ```
mod merge {
    use crate::error::{AppError, Result};

    /// Line positions of the three merge markers.
    struct MergeMarkers {
        generated_start: usize,
        manual_start: usize,
        manual_end: usize,
    }

    /// Parse merge markers from existing file content. `None` means
    /// first-time generation (no markers yet, or the file is empty).
    fn parse_merge_markers(content: &str) -> Option<MergeMarkers> {
        let lines: Vec<&str> = content.lines().collect();
        let mut generated_start = None;
        let mut manual_start = None;
        let mut manual_end = None;
        for (idx, line) in lines.iter().enumerate() {
            let trimmed = line.trim();
            if trimmed.starts_with("<<<<<<< GENERATED") {
                generated_start = Some(idx);
            } else if trimmed == "=======" {
                manual_start = Some(idx);
            } else if trimmed.starts_with(">>>>>>> MANUAL") {
                manual_end = Some(idx);
            }
        }
        match (generated_start, manual_start, manual_end) {
            (Some(gs), Some(ms), Some(me)) => Some(MergeMarkers {
                generated_start: gs,
                manual_start: ms,
                manual_end: me,
            }),
            _ => None,
        }
    }

    /// Merge freshly-rendered `generated_code` into `existing_content`,
    /// replacing the previous generated section while preserving the
    /// manual section byte-for-byte. First-time generation (no existing
    /// markers) wraps `generated_code` in fresh markers with a placeholder
    /// manual section.
    ///
    /// # Errors
    /// `[FM-GEN-010]` if `existing_content` has malformed/out-of-order
    /// markers (fail closed rather than guess at intent).
    pub(super) fn merge_sections(generated_code: &str, existing_content: &str) -> Result<String> {
        let Some(markers) = parse_merge_markers(existing_content) else {
            return Ok(format!(
                "<<<<<<< GENERATED\n{generated_code}\n=======\n// Add your manual code here\n>>>>>>> MANUAL\n"
            ));
        };

        let lines: Vec<&str> = existing_content.lines().collect();

        if markers.manual_start <= markers.generated_start {
            return Err(AppError::fm_gen(
                10,
                format!(
                    "invalid merge marker order: GENERATED marker at line {}, ======= marker \
                     at line {}. Remediation: markers must appear as <<<<<<< GENERATED, then \
                     =======, then >>>>>>> MANUAL, in that order.",
                    markers.generated_start, markers.manual_start
                ),
            ));
        }
        if markers.manual_end <= markers.manual_start {
            return Err(AppError::fm_gen(
                10,
                format!(
                    "invalid merge marker order: ======= marker at line {}, >>>>>>> MANUAL \
                     marker at line {}. Remediation: the >>>>>>> MANUAL marker must come after \
                     the ======= separator.",
                    markers.manual_start, markers.manual_end
                ),
            ));
        }

        let manual_section: String =
            lines[(markers.manual_start + 1)..markers.manual_end].join("\n");

        let mut merged = String::new();
        for line in &lines[..markers.generated_start] {
            merged.push_str(line);
            merged.push('\n');
        }
        merged.push_str("<<<<<<< GENERATED\n");
        merged.push_str(generated_code);
        merged.push_str("\n=======\n");
        merged.push_str(&manual_section);
        merged.push_str("\n>>>>>>> MANUAL\n");
        for line in &lines[(markers.manual_end + 1)..] {
            merged.push_str(line);
            merged.push('\n');
        }
        Ok(merged)
    }

    #[cfg(test)]
    #[allow(clippy::unwrap_used, clippy::expect_used)]
    mod tests {
        use super::*;

        #[test]
        fn first_time_wraps_in_fresh_markers() {
            let result = merge_sections("fn new_fn() {}", "").expect("merge");
            assert!(result.contains("<<<<<<< GENERATED"));
            assert!(result.contains("fn new_fn() {}"));
            assert!(result.contains("======="));
            assert!(result.contains(">>>>>>> MANUAL"));
        }

        #[test]
        fn preserves_manual_section_and_replaces_generated() {
            let existing = "<<<<<<< GENERATED\nfn old_generated() {}\n=======\nfn manual_code() {}\n>>>>>>> MANUAL\n";
            let result = merge_sections("fn new_generated() {}", existing).expect("merge");
            assert!(result.contains("fn new_generated() {}"));
            assert!(result.contains("fn manual_code() {}"));
            assert!(!result.contains("fn old_generated() {}"));
        }

        #[test]
        fn out_of_order_markers_is_err() {
            // ======= appears before <<<<<<< GENERATED.
            let existing = "=======\n<<<<<<< GENERATED\n>>>>>>> MANUAL\n";
            let err = merge_sections("x", existing).expect_err("must refuse");
            assert!(err.to_string().contains("FM-GEN-010"), "{err}");
        }
    }
}

// The narrower `has_generation_rules` raw-text pre-parse this module used to
// export here (and its dedicated tests) were removed
// (specs/014-ggen-core-replacement, correction 2 / Blocker A part 2): it is
// fully superseded by the shared `ggen_config::classify_ggen_toml`
// classifier via `crate::schema_dispatch::load`, which every ggen.toml
// dispatch call site in this crate now goes through -- see that module's
// own doc comment. Equivalent coverage (empty-rules-array,
// non-empty-rules, frontmatter-shaped, malformed-TOML) lives in
// `ggen_config::config_schema`'s own test module and
// `crate::schema_dispatch`'s test module.
