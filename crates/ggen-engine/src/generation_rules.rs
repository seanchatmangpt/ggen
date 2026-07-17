//! The declarative `[[generation.rules]]` sync path.
//!
//! Additive to (never a replacement of) [`crate::sync::sync`]'s existing
//! frontmatter-per-template-file convention (`crate::config::GgenConfig` +
//! `crate::template::Frontmatter`): a project's `ggen.toml` is either a
//! frontmatter project or a declarative-rules project, decided once by
//! [`has_generation_rules`] on the raw TOML text before any typed parse
//! runs. `crate::config::GgenConfig` (`deny_unknown_fields`) would refuse a
//! `[[generation.rules]]` project outright; this module never sees a
//! frontmatter project. An existing frontmatter project (no `[generation]`
//! table, or one with an empty/absent `rules` array) is entirely
//! unaffected — [`has_generation_rules`] returns `false` and
//! [`crate::sync::sync`] falls through to the unchanged original path.
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
//! - `[[inference.rules]]` — sorted by `.order`, each an optional `when:` ASK guard then a
//!   CONSTRUCT query whose derived triples are folded into the graph *before* any
//!   `[[generation.rules]]` query runs, exactly matching ggen-core's stage order
//!   (`execute_inference_rules` before `execute_generation_rules`). Load-bearing proof:
//!   `tests/generation_rules_e2e.rs`'s
//!   `inference_rule_construct_is_visible_to_generation_rule_query` and
//!   `inference_rule_when_guard_false_skips_construct`.
//! - The law gate (N3 rule materialization + denial check, SHACL validation) — reuses
//!   `crate::sync::sync`'s exact frontmatter-path stage (same `GraphEngine` calls, same
//!   `[FM-LAW-*]` refusal shape) rather than re-deriving it. Reads N3 rules from
//!   `manifest.law.rules` and SHACL shapes from `manifest.validation.shacl` (**not** a
//!   duplicated `law.shapes` field — see [`ggen_config::manifest::types::GgenManifest`]'s
//!   struct doc comment for why). Load-bearing proof:
//!   `tests/generation_rules_e2e.rs`'s `law_gate_denial_violation_refuses_declarative_rules_sync`
//!   and `law_gate_shacl_violation_refuses_declarative_rules_sync_naming_focus_node`.
//!
//! Deliberately deferred (a typed, loud [`AppError::fm_gen`] refusal naming the rule and the
//! unimplemented variant — never a silent skip or a decorative success):
//! - `QuerySource::Pack` / `TemplateSource::Pack` — no destination in `crate::pack::Pack`'s
//!   model for a named-output-directory lookup yet (the exact gap tasks.md's T028 note
//!   already flagged as unresolved).
//! - `TemplateSource::{Git, Package}` — a future implementation should reuse
//!   `crate::pack`'s existing git-clone-and-cache convention (`.ggen-v2/git-packs/<name>/` +
//!   `.ggen-git-pin`), not re-derive ggen-core's original one-shot clone.
//! - `[validation].no_unsafe` (ggen-core's E0012 `unsafe`-block check) — `manifest.validation`
//!   is now read for `.shacl` (the law gate above), but `.no_unsafe` has no reader here yet;
//!   left as a documented gap, not a check this module claims to run.
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
    GenerationMode, GenerationRule, GgenManifest, QuerySource, TemplateSource,
};
use tera::Value;

use crate::{
    error::{AppError, Result},
    graph::{DeterministicGraph, EngineQueryResults, GraphEngine, GraphLawStore},
    sync::{hash_file_or_missing, hex32, rel_display, write_receipt, EngineKind, SyncOptions, SyncReport},
    template::{build_tera, solutions_to_values},
};

/// Cheap structural pre-parse: does `raw_toml` declare a non-empty
/// `[[generation.rules]]`? Used by [`crate::sync::sync`] to decide, before
/// any typed parse, whether a project's `ggen.toml` belongs to this
/// module's declarative-rules schema or `crate::config::GgenConfig`'s
/// frontmatter schema.
///
/// A TOML syntax error here is surfaced (not swallowed to `Ok(false)`):
/// swallowing it would silently route a malformed file to the frontmatter
/// path, which would then report a *different*, potentially confusing
/// error for the same root cause.
///
/// # Errors
/// `[FM-CONFIG-002]` if `raw_toml` is not syntactically valid TOML.
pub(crate) fn has_generation_rules(raw_toml: &str) -> Result<bool> {
    // `toml::Value: FromStr` parses a single bare *value* expression (see
    // `toml::Value::from_str`'s `ValueDeserializer::parse`), not a whole
    // multi-table document -- it fails on any real `ggen.toml` starting with
    // a `[table]` header ("unexpected content, expected nothing"). A TOML
    // *document* is a `Table` (`toml::Table: FromStr` calls the real
    // document parser, `toml::from_str`, per the crate's own top-level
    // "easiest way to parse a TOML document" example) -- parse into that
    // instead. `Table` derefs the same `.get(&str) -> Option<&Value>` API
    // this function's chained lookup relies on.
    let value: toml::Table = raw_toml.parse().map_err(|e| {
        AppError::fm_config(
            2,
            format!(
                "invalid ggen.toml document: {e}. \
                 Remediation: fix the TOML syntax or remove unknown keys."
            ),
        )
    })?;
    Ok(value
        .get("generation")
        .and_then(|g| g.get("rules"))
        .and_then(toml::Value::as_array)
        .is_some_and(|a| !a.is_empty()))
}

/// Run every `[[generation.rules]]` entry in `manifest` against a fresh
/// graph loaded from `manifest.ontology`, producing the same
/// [`SyncReport`]/receipt shape [`crate::sync::sync`]'s frontmatter path
/// produces — see the module doc comment for the full design and scope.
///
/// # Errors
/// Fails closed on any resolve/query/render/write failure, or on an
/// unimplemented `QuerySource`/`TemplateSource` variant (`[FM-GEN-*]`).
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

    let graph: Arc<dyn GraphEngine> = match opts.engine {
        EngineKind::GraphLaw => Arc::new(GraphLawStore::new()?),
        EngineKind::Oxigraph => Arc::new(DeterministicGraph::new()?),
    };

    let mut closure: BTreeMap<String, String> = BTreeMap::new();
    closure.insert(
        "actuator".to_string(),
        concat!("ggen@", env!("CARGO_PKG_VERSION")).to_string(),
    );

    let ontology_path = root.join(&manifest.ontology.source);
    let ttl = std::fs::read_to_string(&ontology_path).map_err(|e| {
        AppError::fm_config(
            3,
            format!(
                "ontology `{}` unreadable: {e}. Remediation: fix [ontology].source.",
                ontology_path.display()
            ),
        )
    })?;
    graph.insert_turtle(&ttl)?;
    closure.insert(
        rel_display(root, &ontology_path),
        hash_file_or_missing(&ontology_path),
    );

    for import in &manifest.ontology.imports {
        let import_path = root.join(import);
        let import_ttl = std::fs::read_to_string(&import_path).map_err(|e| {
            AppError::fm_config(
                3,
                format!(
                    "ontology import `{}` unreadable: {e}. Remediation: fix [ontology].imports.",
                    import_path.display()
                ),
            )
        })?;
        graph.insert_turtle(&import_ttl)?;
        closure.insert(
            rel_display(root, &import_path),
            hash_file_or_missing(&import_path),
        );
    }

    drop(load_guard);
    load_span.record("pipeline.duration_ms", load_start.elapsed().as_millis() as u64);

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
            let derived = match graph.query(&rule.construct)? {
                EngineQueryResults::Graph(triples) => triples,
                _ => {
                    return Err(AppError::fm_gen(
                        14,
                        format!(
                            "inference rule `{}`: `construct:` must be a CONSTRUCT query. \
                             Remediation: use CONSTRUCT {{ … }} WHERE {{ … }}.",
                            rule.name
                        ),
                    ));
                }
            };
            if !derived.is_empty() {
                let doc: String = derived
                    .iter()
                    .map(|t| format!("{} .\n", t.ntriples))
                    .collect();
                graph.insert_turtle(&doc)?;
            }
        }
    }

    drop(extract_guard);
    extract_span.record(
        "pipeline.duration_ms",
        extract_start.elapsed().as_millis() as u64,
    );

    // ── Law gate — N3 rule materialization + denial/SHACL validation ──────
    //
    // Reuses the exact stage `crate::sync::sync` already runs for frontmatter
    // projects (same `GraphEngine::{load_rules,materialize,check_denials,
    // validate_shacl}` calls, same `[FM-LAW-*]` refusal shape) rather than
    // re-deriving it — `manifest.law.rules` (N3/Datalog) and
    // `manifest.validation.shacl` (SHACL shapes; deliberately not duplicated
    // as `law.shapes` — see `ggen_config::manifest::types::GgenManifest`'s
    // struct doc comment) are this schema's equivalents of
    // `GgenConfig.law.{rules,shapes}`. Absent/empty on both fields runs no law
    // stage at all, so pre-law declarative-rules projects sync unchanged.
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
    for rel in &manifest.validation.shacl {
        let shape_path = root.join(rel);
        let shapes_ttl = std::fs::read_to_string(&shape_path).map_err(|e| {
            AppError::fm_law(
                17,
                format!(
                    "SHACL shapes file `{}` unreadable: {e}. Remediation: fix [validation].shacl.",
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
                18,
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

    drop(validate_guard);
    validate_span.record(
        "pipeline.duration_ms",
        validate_start.elapsed().as_millis() as u64,
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

    let mut tera = build_tera(Arc::clone(&graph));
    let mut skipped: Vec<(PathBuf, String)> = Vec::new();
    let mut decisions: BTreeMap<String, String> = BTreeMap::new();
    let mut pending: Vec<PendingGenWrite> = Vec::new();

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

        let query_text = resolve_query_source(root, rule, &rule.query, &mut closure)?;
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
        let template_text = resolve_template_source(root, rule, &rule.template, &mut closure)?;
        // Ephemeral per-rule template name so Tera errors can point back at
        // the rule; the body itself is registered fresh for each rule (an
        // inline/file template has no stable path to reuse as a Tera name
        // across rules).
        let tpl_name = format!("generation_rule::{}", rule.name);
        tera.add_raw_template(&tpl_name, &template_text).map_err(|e| {
            AppError::fm_gen(
                3,
                format!("rule `{}`: template rejected by Tera: {e}", rule.name),
            )
        })?;

        let per_row = rule.output_file.contains("{{");
        if per_row {
            for row in &row_values {
                let mut ctx = tera::Context::new();
                ctx.insert("results", &row_values);
                ctx.insert("row", row);
                if let Value::Object(map) = row {
                    for (k, v) in map {
                        ctx.insert(k, v);
                    }
                }
                let to = render_output_file(&mut tera, &rule.output_file, &ctx, &rule.name)?;
                let body = render_template(&mut tera, &tpl_name, &ctx, &rule.name)?;
                validate_rendered_body(&rule.name, &to, &body)?;
                pending.push(PendingGenWrite {
                    to,
                    body,
                    mode: rule.mode.clone(),
                });
            }
        } else {
            let mut ctx = tera::Context::new();
            ctx.insert("results", &row_values);
            let body = render_template(&mut tera, &tpl_name, &ctx, &rule.name)?;
            validate_rendered_body(&rule.name, &rule.output_file, &body)?;
            pending.push(PendingGenWrite {
                to: rule.output_file.clone(),
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
        generate_start.elapsed().as_millis() as u64,
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
    emit_span.record(
        "pipeline.duration_ms",
        emit_start.elapsed().as_millis() as u64,
    );
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
        write_receipt(root, &report)?;
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

/// Resolve a [`QuerySource`] to its SPARQL query text, binding any file it
/// reads into `closure`.
///
/// # Errors
/// `[FM-GEN-005]` on an unreadable query file; `[FM-GEN-006]` for the
/// not-yet-implemented `Pack` variant (see the module doc comment).
fn resolve_query_source(
    root: &Path,
    rule: &GenerationRule,
    source: &QuerySource,
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
        QuerySource::Pack { pack, .. } => Err(AppError::fm_gen(
            6,
            format!(
                "rule `{}`: QuerySource::Pack (pack `{pack}`) is not implemented yet in \
                 ggen-engine's declarative-rules sync path. \
                 Remediation: use QuerySource::File or QuerySource::Inline for now; \
                 see specs/014-ggen-core-replacement/tasks.md for the tracked follow-up.",
                rule.name
            ),
        )),
    }
}

/// Resolve a [`TemplateSource`] to its Tera template text, binding any file
/// it reads into `closure`.
///
/// # Errors
/// `[FM-GEN-005]` on an unreadable template file; `[FM-GEN-007]` for the
/// not-yet-implemented `Pack`/`Git`/`Package` variants (see the module doc
/// comment).
fn resolve_template_source(
    root: &Path,
    rule: &GenerationRule,
    source: &TemplateSource,
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
        TemplateSource::Pack { pack, .. } => Err(AppError::fm_gen(
            7,
            format!(
                "rule `{}`: TemplateSource::Pack (pack `{pack}`) is not implemented yet. \
                 Remediation: use TemplateSource::File or TemplateSource::Inline for now; \
                 see specs/014-ggen-core-replacement/tasks.md for the tracked follow-up.",
                rule.name
            ),
        )),
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

/// Render `rule.output_file` through Tera (it may reference the same
/// context as the body, e.g. a per-row `{{name}}` path segment).
fn render_output_file(
    tera: &mut tera::Tera,
    output_file: &str,
    ctx: &tera::Context,
    rule_name: &str,
) -> Result<String> {
    tera.render_str(output_file, ctx).map_err(|e| {
        AppError::fm_gen(
            8,
            format!("rule `{rule_name}`: output_file render failed: {e}"),
        )
    })
}

/// Render the rule's registered template body through Tera.
fn render_template(
    tera: &mut tera::Tera,
    tpl_name: &str,
    ctx: &tera::Context,
    rule_name: &str,
) -> Result<String> {
    tera.render(tpl_name, ctx).map_err(|e| {
        AppError::fm_gen(
            8,
            format!("rule `{rule_name}`: template render failed: {e}"),
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
    root: &Path,
    rel_to: &str,
    body: &str,
    mode: &GenerationMode,
    dry_run: bool,
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
        let markers = match parse_merge_markers(existing_content) {
            None => {
                return Ok(format!(
                    "<<<<<<< GENERATED\n{generated_code}\n=======\n// Add your manual code here\n>>>>>>> MANUAL\n"
                ));
            }
            Some(m) => m,
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn has_generation_rules_false_for_frontmatter_project() {
        let toml = "[project]\nname = \"x\"\n\n[ontology]\nsource = \"o.ttl\"\n\n[templates]\ndir = \"t\"\n";
        assert!(!has_generation_rules(toml).expect("parse"));
    }

    #[test]
    fn has_generation_rules_false_when_rules_array_is_empty() {
        let toml = "[generation]\nrules = []\n";
        assert!(!has_generation_rules(toml).expect("parse"));
    }

    #[test]
    fn has_generation_rules_true_when_rules_non_empty() {
        let toml = r#"
[generation]

[[generation.rules]]
name = "x"
query = { inline = "SELECT * WHERE { ?s ?p ?o }" }
template = { inline = "hi" }
output_file = "out.txt"
"#;
        assert!(has_generation_rules(toml).expect("parse"));
    }

    #[test]
    fn has_generation_rules_surfaces_toml_syntax_errors() {
        let err = has_generation_rules("not [ valid toml").expect_err("must fail");
        assert!(err.to_string().contains("FM-CONFIG-002"), "{err}");
    }
}
