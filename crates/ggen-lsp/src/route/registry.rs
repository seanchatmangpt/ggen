//! Repair-route registry: family → routes, with cold-start seeds and O(1) lookup.
//!
//! The registry is built once (seeded built-ins, optionally merged with mined
//! routes from a pack `powl/` dir) and read on the hot path via two hashmap
//! probes — no mining, no I/O, sub-100ms.

use lsp_max::lsp_types::{Diagnostic, NumberOrString};
use std::collections::HashMap;
use std::path::Path;

use super::model::{
    Anchor, EditTemplate, PartialOrder, Provenance, RepairFamily, RepairRoute, RepairStep, RouteId,
    StepId,
};
use super::promoted::{is_promotable, load_promoted};

/// Family → routes (priority-sorted) + id → route, for fast lookup.
#[derive(Debug, Clone, Default)]
pub struct RouteRegistry {
    by_family: HashMap<RepairFamily, Vec<RepairRoute>>,
    by_id: HashMap<RouteId, RepairRoute>,
}

impl RouteRegistry {
    /// Cold-start registry: built-in seeded routes only. No I/O.
    #[must_use]
    pub fn seeded() -> Self {
        let mut reg = Self::default();
        for route in seed_routes() {
            reg.insert(route);
        }
        reg
    }

    /// Merge mined routes from a promoted-route artifact over the seeds
    /// (best-effort; a missing/invalid/old-version file leaves seeds intact).
    /// Mined routes never *remove* seeds — precedence is decided per-lookup by
    /// the conformance gate in [`Self::select_for_diagnostic`].
    #[must_use]
    pub fn with_pack_routes(mut self, path: &Path) -> Self {
        if let Some(promoted) = load_promoted(path) {
            for route in promoted.routes {
                self.insert(route);
            }
        }
        self
    }

    fn insert(&mut self, route: RepairRoute) {
        self.by_id.insert(route.id.clone(), route.clone());
        let v = self.by_family.entry(route.family).or_default();
        v.push(route);
        // Highest priority first.
        v.sort_by_key(|r| std::cmp::Reverse(r.priority));
    }

    /// Number of registered routes.
    #[must_use]
    pub fn len(&self) -> usize {
        self.by_id.len()
    }

    /// True if no routes are registered.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.by_id.is_empty()
    }

    /// Select the route for a diagnostic under **conformance-gated precedence**
    /// (van der Aalst: trust only what the log proves). A mined route is
    /// preferred over the family's seed only when it clears the support +
    /// measured-success thresholds ([`is_promotable`]); otherwise the seed
    /// holds. Pure + O(routes-in-family) — no I/O, no mining on the hot path.
    #[must_use]
    pub fn select_for_diagnostic(&self, diag: &Diagnostic) -> Option<&RepairRoute> {
        let family = family_of_diagnostic(diag)?;
        let routes = self.by_family.get(&family)?;
        // 1. A promotable (evidence-proven) mined route wins.
        routes
            .iter()
            .find(|r| is_promotable(&r.provenance))
            // 2. else the seed (doctrine is the safe default).
            .or_else(|| {
                routes
                    .iter()
                    .find(|r| matches!(r.provenance, Provenance::Seeded))
            })
            // 3. else whatever exists (e.g. a sub-threshold mined route).
            .or_else(|| routes.first())
    }

    /// Fetch a route by id (used to reconstruct from a carried `RoutePlanRef`).
    #[must_use]
    pub fn get(&self, id: &RouteId) -> Option<&RepairRoute> {
        self.by_id.get(id)
    }
}

/// Map a diagnostic to its repair family — the single reconciliation point
/// between E-codes/sources and families.
#[must_use]
pub fn family_of_diagnostic(diag: &Diagnostic) -> Option<RepairFamily> {
    if let Some(NumberOrString::String(code)) = &diag.code {
        return family_of_code(code);
    }
    // RDF located-parse diagnostics carry no code; disambiguate by message.
    if diag.message.starts_with("RDF syntax error") {
        return Some(RepairFamily::ParseFailure);
    }
    None
}

/// Map a diagnostic code string to its repair family.
///
/// Used by the offline miner
/// (which has only the captured code string, not a full `Diagnostic`). The
/// capture sentinel `"RDF"` (a code-less located-parse diagnostic) maps to
/// `ParseFailure`.
#[must_use]
pub fn family_of_code(code: &str) -> Option<RepairFamily> {
    match code {
        "E0010" | "E0011" | "E0013" | "E0015" | "E0024" => Some(RepairFamily::TemplateFailure),
        // GGEN-TPL-001 (unbound projection): a template references a variable the
        // SPARQL SELECT never binds — i.e. a dangling reference across source-law
        // surfaces. Mapped to DanglingReference (an otherwise unseeded family) so
        // its dedicated source-law route is selected WITHOUT colliding with the
        // TemplateFailure seeds. (`select_for_diagnostic` keys only on family, so
        // GGEN-TPL-001 must own its family to avoid cross-code contamination.)
        // The model has no dedicated `SourceLaw` family — see the Agent 3 handoff
        // for that orchestrator request. The species-level route slug is
        // "source_law_repair" (route::diagnostic_species).
        "GGEN-TPL-001" => Some(RepairFamily::DanglingReference),
        // GGEN-HARNESS-001 (harness mismatch): a declared proof/test/bench target
        // (Cargo.toml [[test]]/[[bench]] explicit `path`) points at a file that
        // does not exist on disk. Mapped to AdmissionFailure (an otherwise
        // UNSEEDED family) so its dedicated proof-topology source-law route is
        // selected WITHOUT colliding with any other code's seeds. As with
        // GGEN-TPL-001, HARNESS owns its family exclusively, so
        // `select_for_diagnostic` (which keys only on family) never contaminates
        // another code. The species-level route slug is "proof_topology_repair"
        // (route::diagnostic_species).
        "GGEN-HARNESS-001" => Some(RepairFamily::AdmissionFailure),
        // GGEN-OUT-001 (unbound output path): a rule's dynamic `output_file` Tera
        // pattern references a variable the SPARQL SELECT never binds — a dangling
        // reference across source-law surfaces, the dual of GGEN-TPL-001 but on the
        // ggen.toml/SPARQL surfaces (anchor: ggen.toml, not the .tera body). Mapped
        // to LoadFailure (an otherwise UNSEEDED family) so its dedicated source-law
        // route is selected WITHOUT colliding with any other code's seeds. As with
        // TPL-001/HARNESS-001, OUT owns its family exclusively, so
        // `select_for_diagnostic` (which keys only on family) never contaminates
        // another code (in particular it must NOT reuse DanglingReference, which
        // would steal the TPL-001 route). The species-level route slug is
        // "source_law_repair" (route::diagnostic_species).
        "GGEN-OUT-001" => Some(RepairFamily::LoadFailure),
        // GGEN-RULE-001 (unbound rule file): a rule's query/template {file=...}
        // points at a missing file. Mapped to RuleFileMissing (a NEW family it
        // owns EXCLUSIVELY) so its source-law route is selected without colliding
        // with TPL (DanglingReference), OUT (LoadFailure), or HARNESS
        // (AdmissionFailure). Species slug "source_law_repair".
        "GGEN-RULE-001" => Some(RepairFamily::RuleFileMissing),
        // GGEN-SRC-001 (source-caste path violation): a rule's `output_file`
        // targets a source-caste directory (rendered source masquerading as
        // first-class source). Mapped to SourceCastePath (a NEW family it owns
        // EXCLUSIVELY) so its source-law route is selected without colliding
        // with TPL (DanglingReference), OUT (LoadFailure), RULE
        // (RuleFileMissing), or HARNESS (AdmissionFailure). The species table
        // (route::diagnostic_species) declares this species' route slug as
        // "source_law_repair" and `detector_active: true` — this arm is what
        // makes that route slug actually resolvable via `select_for_diagnostic`
        // instead of silently falling through to `_ => None`.
        "GGEN-SRC-001" => Some(RepairFamily::SourceCastePath),
        // GGEN-YIELD-003 (orphaned output): a rule's `output_file` pattern
        // lacks a static filename base (e.g. resolves to just `.rs` or empty
        // after stripping Tera variables). Mapped to OrphanedOutput (a NEW
        // family it owns EXCLUSIVELY) so its source-law route is selected
        // without colliding with TPL (DanglingReference), OUT (LoadFailure),
        // RULE (RuleFileMissing), SRC (SourceCastePath), or HARNESS
        // (AdmissionFailure). The species table (route::diagnostic_species)
        // declares this species' route slug as "source_law_repair" and
        // `detector_active: true` — this arm is what makes that route slug
        // actually resolvable via `select_for_diagnostic` instead of silently
        // falling through to `_ => None` (F5, decorative-completion).
        "GGEN-YIELD-003" => Some(RepairFamily::OrphanedOutput),
        // GGEN-YIELD-004 (competing authority): two or more ggen.toml rules
        // target the same `output_file`, so it is undecidable which rule's
        // content wins. Mapped to CompetingAuthority (a NEW family it owns
        // EXCLUSIVELY) so its source-law route is selected without colliding
        // with TPL (DanglingReference), OUT (LoadFailure), RULE
        // (RuleFileMissing), SRC (SourceCastePath), YIELD-003
        // (OrphanedOutput), or HARNESS (AdmissionFailure). The species table
        // (route::diagnostic_species) declares this species' route slug as
        // "source_law_repair" and `detector_active: true` — this arm is what
        // makes that route slug actually resolvable via `select_for_diagnostic`
        // instead of silently falling through to `_ => None` (F6,
        // decorative-completion).
        "GGEN-YIELD-004" => Some(RepairFamily::CompetingAuthority),
        // GGEN-PACK-001 (pack-source indirection): a rule's query/template is
        // supplied via `{ pack = "...", ... }`, which cannot be resolved at
        // author time, so GGEN-TPL-001/GGEN-OUT-001 checks are vacuous for
        // that rule. Mapped to PackIndirection (a NEW family it owns
        // EXCLUSIVELY) so its advisory route is selected without colliding
        // with TPL (DanglingReference), OUT (LoadFailure), RULE
        // (RuleFileMissing), SRC (SourceCastePath), YIELD-003
        // (OrphanedOutput), YIELD-004 (CompetingAuthority), or HARNESS
        // (AdmissionFailure). The species table (route::diagnostic_species)
        // declares this species' route slug as "advisory" and
        // `detector_active: true` — this arm is what makes that route slug
        // actually resolvable via `select_for_diagnostic` instead of
        // silently falling through to `_ => None` (F3, decorative-completion).
        "GGEN-PACK-001" => Some(RepairFamily::PackIndirection),
        // GGEN-QUERY-002 (SELECT * blindspot): a rule's SPARQL query uses
        // `SELECT *` instead of explicit projections, which disables the
        // GGEN-TPL-001/GGEN-OUT-001 unbound-variable checks for that rule
        // (they cannot see what variables the query actually provides).
        // Mapped to Blindspot (a NEW family it owns EXCLUSIVELY) so its
        // advisory route is selected without colliding with TPL
        // (DanglingReference), OUT (LoadFailure), RULE (RuleFileMissing),
        // SRC (SourceCastePath), YIELD-003 (OrphanedOutput), YIELD-004
        // (CompetingAuthority), PACK-001 (PackIndirection), or HARNESS
        // (AdmissionFailure). The species table (route::diagnostic_species)
        // declares this species' route slug as "advisory" and
        // `detector_active: true` — this arm is what makes that route slug
        // actually resolvable via `select_for_diagnostic` instead of
        // silently falling through to `_ => None` (F2, decorative-completion:
        // the analyzer (`analyzers::detect_query_002`) is genuinely wired
        // live into `check.rs::fold_query_002`, so this arm's absence meant
        // every real GGEN-QUERY-002 diagnostic hit a route refusal despite
        // the species table's claim otherwise).
        "GGEN-QUERY-002" => Some(RepairFamily::Blindspot),
        "E0023" => Some(RepairFamily::ConfigValue),
        "E0001" => Some(RepairFamily::ParseFailure),
        "RDF" => Some(RepairFamily::ParseFailure),
        _ => None,
    }
}

/// Built-in cold-start routes. Kept minimal and high-confidence; mined routes
/// extend these per family.
fn seed_routes() -> Vec<RepairRoute> {
    vec![
        // Invalid enum value → ADVISORY only. We cannot safely choose the
        // replacement (the diagnostic lists the allowed set but not the intent),
        // so no destructive auto-edit is offered — just guidance.
        RepairRoute {
            id: RouteId("config.fix-enum-value".into()),
            family: RepairFamily::ConfigValue,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "Replace with one of the admitted values listed in the diagnostic"
                        .into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Use an admitted enum value".into(),
            provenance: Provenance::Seeded,
            priority: 5,
        },
        // ParseFailure (UnknownPrefix): declare the missing prefix.
        RepairRoute {
            id: RouteId("parse.declare-prefix".into()),
            family: RepairFamily::ParseFailure,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("declare".into()),
                    title: "Declare the missing @prefix".into(),
                    edit: EditTemplate::InsertLine {
                        anchor: Anchor::AfterLastPrefix,
                        text: "@prefix {prefix}: <{iri}> .".into(),
                    },
                }],
                edges: vec![],
            },
            description: "Declare the undeclared prefix".into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // TemplateFailure (E0010): advisory — VALUES must move inline to ggen.toml.
        RepairRoute {
            id: RouteId("template.values-inline".into()),
            family: RepairFamily::TemplateFailure,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "Move VALUES data inline into ggen.toml".into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Move VALUES out of the external .rq into ggen.toml".into(),
            provenance: Provenance::Seeded,
            priority: 5,
        },
        // GGEN-TPL-001 (unbound projection): a template consumes a variable the
        // SPARQL SELECT does not project. The fix lives ONLY in source law —
        // never in emitted output. This route is purely ADVISORY (all NoOp
        // edits): three concurrent source-law surfaces — (a) the SPARQL SELECT
        // vars, (b) the Tera template variable reference, (c) the ggen.toml rule
        // binding. No step targets, references, or edits an emitted output file.
        //
        // Owns the DanglingReference family exclusively, so a GGEN-TPL-001
        // diagnostic selects THIS route and nothing else (no contamination of
        // the TemplateFailure seeds).
        RepairRoute {
            id: RouteId("source-law.bind-projection".into()),
            family: RepairFamily::DanglingReference,
            steps: PartialOrder {
                nodes: vec![
                    RepairStep {
                        id: StepId("edit-sparql-select".into()),
                        title: "Project the variable in the SPARQL SELECT (source law)".into(),
                        edit: EditTemplate::NoOp,
                    },
                    RepairStep {
                        id: StepId("edit-template-ref".into()),
                        title: "Fix the Tera template variable reference (source law)".into(),
                        edit: EditTemplate::NoOp,
                    },
                    RepairStep {
                        id: StepId("inspect-ggen-toml-rule".into()),
                        title: "Inspect/edit the ggen.toml rule binding (source law)".into(),
                        edit: EditTemplate::NoOp,
                    },
                ],
                // Independent source-law surfaces — no ordering edge (concurrent).
                edges: vec![],
            },
            description: "Unbound projection — bind the variable at its source law \
                          (SPARQL SELECT, Tera template, or ggen.toml rule). Advisory \
                          only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // GGEN-HARNESS-001 (harness mismatch): a declared proof/test/bench target
        // points at a file that does not exist on disk. The fix lives ONLY in
        // source law — reconcile the DECLARATION (Cargo.toml [[test]]/[[bench]]
        // path, Makefile.toml task target reference) with the proof file on disk.
        // Purely ADVISORY (all NoOp edits): three concurrent source-law surfaces.
        // NO step fabricates a passing proof, forces a test to pass, or targets an
        // emitted/generated output (cf. real commit 47656dbf "replace non-existent
        // benchmark targets" — the lawful repair removed dead declarations).
        //
        // Owns the AdmissionFailure family exclusively, so a GGEN-HARNESS-001
        // diagnostic selects THIS route and nothing else (no contamination of the
        // TPL-001 DanglingReference route or the TemplateFailure seeds).
        RepairRoute {
            id: RouteId("proof-topology.repair".into()),
            family: RepairFamily::AdmissionFailure,
            steps: PartialOrder {
                nodes: vec![
                    RepairStep {
                        id: StepId("fix-cargo-toml-declaration".into()),
                        title: "Correct the Cargo.toml [[test]]/[[bench]] declaration: align the \
                                `path` to the real proof file, or remove the non-existent target \
                                (source law)"
                            .into(),
                        edit: EditTemplate::NoOp,
                    },
                    RepairStep {
                        id: StepId("fix-makefile-toml-reference".into()),
                        title:
                            "Correct the Makefile.toml proof/test task target reference to name \
                                only existing targets (source law)"
                                .into(),
                        edit: EditTemplate::NoOp,
                    },
                    RepairStep {
                        id: StepId("inspect-proof-file-path".into()),
                        title:
                            "Inspect/create the missing proof file under tests or tests/proof so \
                                the declared path resolves (source law)"
                                .into(),
                        edit: EditTemplate::NoOp,
                    },
                ],
                // Three independent source-law surfaces — no ordering edge (concurrent).
                edges: vec![],
            },
            description: "Harness mismatch — reconcile the declared proof/test topology \
                          (Cargo.toml [[test]]/[[bench]], Makefile.toml task targets) with the \
                          proof files on disk. Repair the declaration or the file path; NEVER \
                          fabricate or force a passing proof, NEVER target a generated artifact. \
                          Advisory only (inspect_only)."
                .into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // GGEN-OUT-001 (unbound output path): a rule's dynamic `output_file` Tera
        // pattern consumes a variable the SPARQL SELECT does not project. The fix
        // lives ONLY in source law — never in emitted output. This route is purely
        // ADVISORY (all NoOp edits): two concurrent source-law surfaces — (a) the
        // SPARQL SELECT vars, (b) the ggen.toml rule `output_file` pattern. No step
        // targets, references, or edits an emitted output file.
        //
        // Owns the LoadFailure family exclusively, so a GGEN-OUT-001 diagnostic
        // selects THIS route and nothing else (no contamination of the TPL-001
        // DanglingReference route or the HARNESS AdmissionFailure route).
        RepairRoute {
            id: RouteId("source-law.bind-output-path".into()),
            family: RepairFamily::LoadFailure,
            steps: PartialOrder {
                nodes: vec![
                    RepairStep {
                        id: StepId("edit-sparql-select".into()),
                        title: "Project the variable in the SPARQL SELECT so the output_file \
                                pattern can bind it (source law)"
                            .into(),
                        edit: EditTemplate::NoOp,
                    },
                    RepairStep {
                        id: StepId("edit-output-file-pattern".into()),
                        title: "Fix the ggen.toml rule output_file pattern variable reference \
                                (source law)"
                            .into(),
                        edit: EditTemplate::NoOp,
                    },
                ],
                // Two independent source-law surfaces — no ordering edge (concurrent).
                edges: vec![],
            },
            description: "Unbound output path — bind the variable at its source law \
                          (the SPARQL SELECT or the ggen.toml rule output_file pattern). \
                          Advisory only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // GGEN-RULE-001 (unbound rule file): a ggen.toml rule binds a query or
        // template {file=...} that does not exist. The fix lives ONLY in source
        // law — create the missing source file, or correct the rule's path.
        // Purely ADVISORY (NoOp). NO step fabricates emitted/generated output.
        // Owns the RuleFileMissing family exclusively.
        RepairRoute {
            id: RouteId("source-law.bind-rule-file".into()),
            family: RepairFamily::RuleFileMissing,
            steps: PartialOrder {
                nodes: vec![
                    RepairStep {
                        id: StepId("create-missing-rule-file".into()),
                        title: "Create the missing query/template file at the path the \
                                ggen.toml rule binds (source law); never fabricate \
                                generated output"
                            .into(),
                        edit: EditTemplate::NoOp,
                    },
                    RepairStep {
                        id: StepId("fix-rule-file-path".into()),
                        title: "Or correct the ggen.toml rule query/template `file` path \
                                to point at an existing source-law file (source law)"
                            .into(),
                        edit: EditTemplate::NoOp,
                    },
                ],
                edges: vec![],
            },
            description: "Unbound rule file — the ggen.toml rule binds a query/template \
                          file that does not exist. Create the source file or fix the \
                          rule path. Advisory only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // GGEN-SRC-001 (source-caste path violation): a rule's `output_file`
        // targets a source-caste directory (rendered source masquerading as
        // first-class source). The fix lives ONLY in source law — retarget the
        // ggen.toml rule's `output_file` at a first-class path. Purely ADVISORY
        // (NoOp). Owns the SourceCastePath family exclusively.
        RepairRoute {
            id: RouteId("source-law.fix-source-caste-path".into()),
            family: RepairFamily::SourceCastePath,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("retarget-output-file".into()),
                    title: "Retarget the ggen.toml rule's output_file away from the \
                            source-caste directory to a first-class path (source law)"
                        .into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Source-caste path violation — the rule's output_file targets a \
                          source-caste directory. Rendered source is source; retarget the \
                          ggen.toml rule's output_file at a first-class path. Advisory \
                          only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // GGEN-YIELD-003 (orphaned output): a rule's `output_file` pattern
        // lacks a static filename base (e.g. resolves to just `.rs` or empty
        // after stripping Tera variables). The fix lives ONLY in source law —
        // give the ggen.toml rule's `output_file` pattern a static filename
        // base. Purely ADVISORY (NoOp). Owns the OrphanedOutput family
        // exclusively (F5, decorative-completion: this route is what makes
        // the diagnostic_species table's `route: "source_law_repair"` /
        // `detector_active: true` claim for GGEN-YIELD-003 actually true).
        RepairRoute {
            id: RouteId("source-law.fix-orphaned-output".into()),
            family: RepairFamily::OrphanedOutput,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("add-static-filename-base".into()),
                    title: "Give the ggen.toml rule's output_file pattern a static \
                            filename base instead of only Tera variables/extension \
                            (source law)"
                        .into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Orphaned output — the rule's output_file pattern lacks a static \
                          filename base. Give the ggen.toml rule's output_file pattern a \
                          static filename base. Advisory only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // GGEN-YIELD-004 (competing authority): two or more ggen.toml rules
        // target the same `output_file`, so it is undecidable which rule's
        // content wins. The fix lives ONLY in source law — retarget or merge
        // the competing ggen.toml rules so at most one owns the output_file.
        // Purely ADVISORY (NoOp). Owns the CompetingAuthority family
        // exclusively (F6, decorative-completion: this route is what makes
        // the diagnostic_species table's `route: "source_law_repair"` /
        // `detector_active: true` claim for GGEN-YIELD-004 actually true).
        RepairRoute {
            id: RouteId("source-law.resolve-competing-authority".into()),
            family: RepairFamily::CompetingAuthority,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("resolve-competing-rules".into()),
                    title: "Retarget or merge the competing ggen.toml rules so only one \
                            rule owns this output_file (source law)"
                        .into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Competing authority — multiple ggen.toml rules target the same \
                          output_file. Retarget or merge the competing rules so at most \
                          one owns it. Advisory only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 10,
        },
        // GGEN-PACK-001 (pack-source indirection): a rule's query/template is
        // supplied via `{ pack = "...", ... }`, which cannot be resolved at
        // author time, so GGEN-TPL-001/GGEN-OUT-001 checks are vacuous for
        // that rule. The fix is informational only — there is no destructive
        // auto-edit to offer (the pack content is not knowable until
        // generation time). Purely ADVISORY (NoOp). Owns the PackIndirection
        // family exclusively (F3, decorative-completion: this route is what
        // makes the diagnostic_species table's `route: "advisory"` /
        // `detector_active: true` claim for GGEN-PACK-001 actually true,
        // instead of every GGEN-PACK-001 diagnostic silently refusing at
        // `select_for_diagnostic`'s `family_of_code` lookup).
        RepairRoute {
            id: RouteId("pack.acknowledge-indirection".into()),
            family: RepairFamily::PackIndirection,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "Use a direct file path instead of a pack source if author-time \
                            GGEN-TPL-001/GGEN-OUT-001 checks are needed for this rule"
                        .into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "Pack-source indirection — the rule's query/template is resolved \
                          from a pack at generation time, disabling GGEN-TPL-001/GGEN-OUT-001 \
                          author-time checks. Advisory only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 5,
        },
        // GGEN-QUERY-002 (SELECT * blindspot): a rule's SPARQL query uses
        // `SELECT *`, which disables the GGEN-TPL-001/GGEN-OUT-001
        // unbound-variable checks for that rule (they cannot see what
        // variables the query actually provides). The fix is informational
        // only — there is no destructive auto-edit to offer (choosing the
        // "right" explicit projection list is the query author's call, not
        // knowable from the diagnostic alone). Purely ADVISORY (NoOp). Owns
        // the Blindspot family exclusively (F2, decorative-completion: this
        // route is what makes the diagnostic_species table's
        // `route: "advisory"` / `detector_active: true` claim for
        // GGEN-QUERY-002 actually true, instead of every GGEN-QUERY-002
        // diagnostic silently refusing at `select_for_diagnostic`'s
        // `family_of_code` lookup).
        RepairRoute {
            id: RouteId("query.acknowledge-select-star".into()),
            family: RepairFamily::Blindspot,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "Replace SELECT * with explicit projections so \
                            GGEN-TPL-001/GGEN-OUT-001 can verify this rule's \
                            variable bindings"
                        .into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "SELECT * blindspot — the rule's SPARQL query uses SELECT *, \
                          disabling GGEN-TPL-001/GGEN-OUT-001 author-time checks for \
                          this rule. Advisory only; never edits emitted output."
                .into(),
            provenance: Provenance::Seeded,
            priority: 5,
        },
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_max::lsp_types::{DiagnosticSeverity, Position, Range};

    fn diag(code: &str, msg: &str) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position {
                    line: 0,
                    character: 0,
                },
                end: Position {
                    line: 0,
                    character: 1,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: if code.is_empty() {
                None
            } else {
                Some(NumberOrString::String(code.to_string()))
            },
            code_description: None,
            source: Some("ggen-lsp".to_string()),
            message: msg.to_string(),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    #[test]
    fn seeded_registry_is_nonempty_and_sound() {
        let reg = RouteRegistry::seeded();
        assert!(!reg.is_empty());
        for route in seed_routes() {
            assert!(
                route.steps.is_sound(),
                "seed route {:?} must be sound",
                route.id
            );
        }
    }

    #[test]
    fn enum_violation_is_advisory_not_destructive() {
        // The critical regression guard: an invalid enum value must NOT get a
        // delete/replace edit — only advisory guidance.
        let reg = RouteRegistry::seeded();
        let d = diag(
            "E0023",
            "invalid value \"verbose\" for `level` — expected one of: ...",
        );
        let route = reg.select_for_diagnostic(&d).expect("advisory route");
        assert_eq!(route.family, RepairFamily::ConfigValue);
        assert!(
            matches!(route.steps.nodes[0].edit, EditTemplate::NoOp),
            "enum-violation repair must be advisory (NoOp), never a destructive edit"
        );
    }

    #[test]
    fn rdf_syntax_message_maps_to_parse_failure() {
        let d = diag("", "RDF syntax error: unexpected token");
        assert_eq!(family_of_diagnostic(&d), Some(RepairFamily::ParseFailure));
    }

    #[test]
    fn unknown_code_has_no_route() {
        let reg = RouteRegistry::seeded();
        assert!(reg.select_for_diagnostic(&diag("E9999", "x")).is_none());
    }

    // ---- conformance-gated precedence (the vdA gate) ----

    fn mined_route(id: &str, support: u32, success: f32) -> RepairRoute {
        RepairRoute {
            id: RouteId(id.into()),
            family: RepairFamily::TemplateFailure, // same family as seed `template.values-inline`
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("advise".into()),
                    title: "mined advisory".into(),
                    edit: EditTemplate::NoOp,
                }],
                edges: vec![],
            },
            description: "mined route".into(),
            provenance: Provenance::Mined {
                confidence: 0.9,
                support,
                success_rate: success,
                first_seen: "2026-05-28T00:00:00+00:00".into(),
                last_seen: "2026-05-28T01:00:00+00:00".into(),
                source_report_hash: "h".into(),
            },
            priority: 1, // LOWER than the seed's priority — proves precedence is
                         // by conformance, not by priority number.
        }
    }

    fn registry_with(mined: RepairRoute) -> RouteRegistry {
        use crate::route::promoted::{default_pack_routes_path, write_promoted, PromotedRoutes};
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = default_pack_routes_path(dir.path());
        write_promoted(
            &path,
            &PromotedRoutes {
                version: PromotedRoutes::VERSION,
                source_log_hash: "x".into(),
                routes: vec![mined],
            },
        )
        .expect("write");
        let reg = RouteRegistry::seeded().with_pack_routes(&path);
        // keep dir alive until after load
        drop(dir);
        reg
    }

    #[test]
    fn proven_mined_route_beats_seed() {
        // support>=3 AND success>=0.6 → evidence supersedes doctrine, even though
        // the mined route has LOWER priority than the seed.
        let reg = registry_with(mined_route("mined.proven", 5, 0.8));
        let route = reg
            .select_for_diagnostic(&diag("E0010", "VALUES…"))
            .expect("route");
        assert_eq!(route.id.0, "mined.proven", "proven mined route must win");
    }

    #[test]
    fn unproven_mined_route_loses_to_seed() {
        // Below threshold (low support) → seed (doctrine) holds.
        let reg = registry_with(mined_route("mined.weak", 1, 0.9));
        let route = reg
            .select_for_diagnostic(&diag("E0010", "VALUES…"))
            .expect("route");
        assert_eq!(
            route.id.0, "template.values-inline",
            "sub-threshold mined route must NOT override the seed"
        );
    }

    // ---- GGEN-TPL-001: source-law repair route ----

    #[test]
    fn ggen_tpl_001_maps_to_its_own_family() {
        // `family_of_code` must resolve GGEN-TPL-001 to a concrete (non-None)
        // family that it owns exclusively (so it never contaminates other codes).
        assert_eq!(
            family_of_code("GGEN-TPL-001"),
            Some(RepairFamily::DanglingReference)
        );
    }

    #[test]
    fn ggen_tpl_001_does_not_contaminate_template_failure_codes() {
        // Regression guard: introducing the GGEN-TPL-001 route must NOT change the
        // route selected for the other TemplateFailure codes (E0010/E0011/E0024).
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("E0010", "VALUES…"))
            .expect("route");
        assert_eq!(route.id.0, "template.values-inline");
    }

    #[test]
    fn ggen_tpl_001_selects_the_source_law_route() {
        // A GGEN-TPL-001 diagnostic must resolve to the seeded source-law route,
        // not the other TemplateFailure seed (priority 20 > 5).
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-TPL-001", "unbound projection: `name`"))
            .expect("GGEN-TPL-001 must resolve to a seeded route");
        assert_eq!(route.id.0, "source-law.bind-projection");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_tpl_001_route_is_source_law_only() {
        // The load-bearing invariant: the route must NEVER target emitted output.
        // Every step is advisory (NoOp — no output-mutating edit), and every step
        // title references only a source-law surface (SPARQL / Tera template /
        // ggen.toml rule), never an emitted-output path.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-TPL-001", "unbound projection"))
            .expect("route");

        assert!(
            !route.steps.nodes.is_empty(),
            "route must have at least one step"
        );

        const FORBIDDEN_OUTPUT: &[&str] = &["out/", "output/", "dist/", "gen/", "emitted"];
        for step in &route.steps.nodes {
            // No step may produce a textual edit — repairing source law is
            // advisory in the MVP (inspect_only actuation boundary).
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-TPL-001 step {:?} must be advisory (NoOp), never an output edit",
                step.id
            );
            // No step title may reference an emitted-output path.
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN_OUTPUT {
                assert!(
                    !title.contains(forbidden),
                    "GGEN-TPL-001 step title {:?} references forbidden emitted-output marker {:?}",
                    step.title,
                    forbidden
                );
            }
            // Each step must reference a source-law surface.
            assert!(
                title.contains("sparql")
                    || title.contains("template")
                    || title.contains("ggen.toml"),
                "GGEN-TPL-001 step title {:?} must reference a source-law surface",
                step.title
            );
        }
    }

    #[test]
    fn ggen_tpl_001_route_covers_three_source_law_surfaces() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-TPL-001", "unbound projection"))
            .expect("route");
        let titles: String = route
            .steps
            .nodes
            .iter()
            .map(|s| s.title.to_lowercase())
            .collect::<Vec<_>>()
            .join(" | ");
        assert!(titles.contains("sparql"), "must cover SPARQL SELECT");
        assert!(titles.contains("template"), "must cover Tera template");
        assert!(titles.contains("ggen.toml"), "must cover ggen.toml rule");
    }

    // ---- GGEN-HARNESS-001: proof-topology source-law repair route ----

    #[test]
    fn ggen_harness_001_maps_to_its_own_family() {
        // GGEN-HARNESS-001 must resolve to the AdmissionFailure family, which it
        // owns exclusively (no other code maps there) — zero cross-contamination.
        assert_eq!(
            family_of_code("GGEN-HARNESS-001"),
            Some(RepairFamily::AdmissionFailure)
        );
    }

    #[test]
    fn ggen_harness_001_selects_the_proof_topology_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-HARNESS-001", "harness mismatch"))
            .expect("GGEN-HARNESS-001 must resolve to a seeded route");
        assert_eq!(route.id.0, "proof-topology.repair");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_harness_001_route_is_source_law_only() {
        // Load-bearing invariant: the route must NEVER fabricate a proof or target
        // an emitted/generated artifact. Every step is advisory (NoOp), references
        // a harness source-law surface, and contains no emitted-output / fabrication
        // marker.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-HARNESS-001", "harness mismatch"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");

        const FORBIDDEN: &[&str] = &[
            "out/",
            "output/",
            "dist/",
            "gen/",
            "emitted",
            "fabricate",
            "force the",
            "make the proof pass",
            "pass the test",
            "stub",
        ];
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "HARNESS step {:?} must be advisory (NoOp)",
                step.id
            );
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN {
                assert!(
                    !title.contains(forbidden),
                    "HARNESS step title {:?} contains forbidden marker {forbidden:?}",
                    step.title
                );
            }
            // Each step must reference a source-law surface.
            assert!(
                title.contains("cargo.toml")
                    || title.contains("makefile.toml")
                    || title.contains("tests"),
                "HARNESS step title {:?} must reference a source-law surface",
                step.title
            );
        }
    }

    #[test]
    fn ggen_harness_001_does_not_contaminate_tpl_001() {
        // A TPL-001 diagnostic must still resolve to its own source-law route,
        // proving HARNESS did not steal the DanglingReference family.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-TPL-001", "unbound projection"))
            .expect("route");
        assert_eq!(route.id.0, "source-law.bind-projection");
    }

    // ---- GGEN-OUT-001: output-path source-law repair route ----

    #[test]
    fn ggen_out_001_maps_to_its_own_family() {
        // GGEN-OUT-001 must resolve to the LoadFailure family, which it owns
        // exclusively (no other code maps there) — zero cross-contamination.
        assert_eq!(
            family_of_code("GGEN-OUT-001"),
            Some(RepairFamily::LoadFailure)
        );
    }

    #[test]
    fn ggen_out_001_selects_the_source_law_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-OUT-001", "unbound output path: `slug`"))
            .expect("GGEN-OUT-001 must resolve to a seeded route");
        assert_eq!(route.id.0, "source-law.bind-output-path");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_out_001_route_is_source_law_only() {
        // Load-bearing invariant: the route must NEVER target emitted output.
        // Every step is advisory (NoOp), references only a source-law surface
        // (SPARQL SELECT / ggen.toml output_file), and contains no emitted-output
        // marker.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-OUT-001", "unbound output path"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");

        const FORBIDDEN_OUTPUT: &[&str] = &["out/", "output/", "dist/", "gen/", "emitted"];
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-OUT-001 step {:?} must be advisory (NoOp), never an output edit",
                step.id
            );
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN_OUTPUT {
                assert!(
                    !title.contains(forbidden),
                    "GGEN-OUT-001 step title {:?} references forbidden emitted-output marker \
                     {forbidden:?}",
                    step.title
                );
            }
            // Each step must reference a source-law surface (SPARQL or the
            // ggen.toml `output_file` pattern). "output_file" is the literal
            // ggen.toml field name (source law), NOT an emitted-output path.
            assert!(
                title.contains("sparql")
                    || title.contains("ggen.toml")
                    || title.contains("output_file"),
                "GGEN-OUT-001 step title {:?} must reference a source-law surface",
                step.title
            );
        }
    }

    #[test]
    fn ggen_out_001_does_not_contaminate_tpl_001() {
        // A TPL-001 diagnostic must still resolve to its own source-law route,
        // proving OUT did not steal the DanglingReference family.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-TPL-001", "unbound projection"))
            .expect("route");
        assert_eq!(route.id.0, "source-law.bind-projection");
    }

    // ---- GGEN-RULE-001: unbound-rule-file source-law repair route ----

    #[test]
    fn ggen_rule_001_maps_to_its_own_family() {
        // GGEN-RULE-001 must resolve to the RuleFileMissing family, which it owns
        // exclusively (no other code maps there) — zero cross-contamination.
        assert_eq!(
            family_of_code("GGEN-RULE-001"),
            Some(RepairFamily::RuleFileMissing)
        );
    }

    #[test]
    fn ggen_rule_001_selects_the_source_law_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-RULE-001", "unbound rule file"))
            .expect("GGEN-RULE-001 must resolve to a seeded route");
        assert_eq!(route.id.0, "source-law.bind-rule-file");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_rule_001_route_is_source_law_only() {
        // Load-bearing invariant: the route must NEVER fabricate generated output.
        // Every step is advisory (NoOp), references a source-law surface, and
        // contains no emitted-output / fabrication marker.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-RULE-001", "unbound rule file"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");

        // Emitted-output path markers only. ("fabricate" is NOT forbidden here —
        // the route's own text legitimately says "never fabricate generated
        // output", an anti-fabrication instruction, not a fabrication step.)
        const FORBIDDEN: &[&str] = &["out/", "output/", "dist/", "emitted"];
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-RULE-001 step {:?} must be advisory (NoOp)",
                step.id
            );
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN {
                assert!(
                    !title.contains(forbidden),
                    "GGEN-RULE-001 step title {:?} contains forbidden marker {forbidden:?}",
                    step.title
                );
            }
            // Each step must reference a source-law surface (the rule's
            // ggen.toml binding, its query/template file, or the file path).
            assert!(
                title.contains("ggen.toml")
                    || title.contains("query")
                    || title.contains("template")
                    || title.contains("file"),
                "GGEN-RULE-001 step title {:?} must reference a source-law surface",
                step.title
            );
        }
    }

    #[test]
    fn ggen_rule_001_does_not_contaminate_other_species() {
        // Introducing the RULE-001 route must NOT change the route selected for
        // TPL-001 / OUT-001 / HARNESS-001 (each owns its own family).
        let reg = RouteRegistry::seeded();
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-TPL-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-projection"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-OUT-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-output-path"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-HARNESS-001", "x"))
                .expect("route")
                .id
                .0,
            "proof-topology.repair"
        );
    }

    // ---- GGEN-SRC-001: source-caste-path source-law repair route ----
    //
    // Regression guard for the finding: the species table
    // (route::diagnostic_species) advertises GGEN-SRC-001 as
    // `route: "source_law_repair"` with `detector_active: true`, and the
    // detector (analyzers::detect_src_001) IS wired live into check.rs
    // (fold_src_001). Before this fix, `family_of_code("GGEN-SRC-001")` fell
    // through to `_ => None`, so `select_for_diagnostic` always returned
    // `None` for a real GGEN-SRC-001 diagnostic — a decorative-completion gap
    // between the advertised species metadata and the actual routing table.

    #[test]
    fn ggen_src_001_maps_to_its_own_family() {
        // GGEN-SRC-001 must resolve to a concrete (non-None) family that it
        // owns exclusively — zero cross-contamination with TPL/OUT/RULE/HARNESS.
        assert_eq!(
            family_of_code("GGEN-SRC-001"),
            Some(RepairFamily::SourceCastePath)
        );
    }

    #[test]
    fn ggen_src_001_selects_the_source_law_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-SRC-001", "source-caste path violation"))
            .expect("GGEN-SRC-001 must resolve to a seeded route, not None");
        assert_eq!(route.id.0, "source-law.fix-source-caste-path");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_src_001_route_is_source_law_only() {
        // Load-bearing invariant: the route must NEVER target emitted output.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-SRC-001", "source-caste path violation"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");

        const FORBIDDEN_OUTPUT: &[&str] = &["out/", "dist/", "gen/", "emitted"];
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-SRC-001 step {:?} must be advisory (NoOp), never an output edit",
                step.id
            );
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN_OUTPUT {
                assert!(
                    !title.contains(forbidden),
                    "GGEN-SRC-001 step title {:?} references forbidden emitted-output marker \
                     {forbidden:?}",
                    step.title
                );
            }
            assert!(
                title.contains("ggen.toml") || title.contains("output_file"),
                "GGEN-SRC-001 step title {:?} must reference a source-law surface",
                step.title
            );
        }
    }

    #[test]
    fn ggen_src_001_does_not_contaminate_other_species() {
        // Introducing the SRC-001 route must NOT change the route selected for
        // TPL-001 / OUT-001 / RULE-001 / HARNESS-001 (each owns its own family).
        let reg = RouteRegistry::seeded();
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-TPL-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-projection"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-OUT-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-output-path"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-RULE-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-rule-file"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-HARNESS-001", "x"))
                .expect("route")
                .id
                .0,
            "proof-topology.repair"
        );
    }

    // ---- GGEN-YIELD-003: orphaned-output source-law repair route ----
    //
    // Regression guard for finding F5 (decorative-completion,
    // route/diagnostic_species.rs:159): the species table
    // (route::diagnostic_species) advertises GGEN-YIELD-003 as
    // `route: "source_law_repair"` with `detector_active: true`, and the
    // detector (analyzers::detect_yield_003, wired live via
    // check.rs::fold_yield_003) IS genuinely raised as an ERROR diagnostic.
    // Before this fix, `family_of_code("GGEN-YIELD-003")` fell through to
    // `_ => None`, so `select_for_diagnostic` always returned `None` for a
    // real GGEN-YIELD-003 diagnostic — a decorative-completion gap between
    // the advertised species metadata and the actual routing table (the MCP/
    // headless/hover channels got a `RouteRefusal` instead of a route).

    #[test]
    fn ggen_yield_003_maps_to_its_own_family() {
        // GGEN-YIELD-003 must resolve to a concrete (non-None) family that it
        // owns exclusively — zero cross-contamination with
        // TPL/OUT/RULE/SRC/HARNESS.
        assert_eq!(
            family_of_code("GGEN-YIELD-003"),
            Some(RepairFamily::OrphanedOutput)
        );
    }

    #[test]
    fn ggen_yield_003_selects_the_source_law_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-YIELD-003", "orphaned output"))
            .expect("GGEN-YIELD-003 must resolve to a seeded route, not None");
        assert_eq!(route.id.0, "source-law.fix-orphaned-output");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_yield_003_route_is_source_law_only() {
        // Load-bearing invariant: the route must NEVER target emitted output.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-YIELD-003", "orphaned output"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");

        const FORBIDDEN_OUTPUT: &[&str] = &["out/", "dist/", "gen/", "emitted"];
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-YIELD-003 step {:?} must be advisory (NoOp), never an output edit",
                step.id
            );
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN_OUTPUT {
                assert!(
                    !title.contains(forbidden),
                    "GGEN-YIELD-003 step title {:?} references forbidden emitted-output \
                     marker {forbidden:?}",
                    step.title
                );
            }
            assert!(
                title.contains("ggen.toml") || title.contains("output_file"),
                "GGEN-YIELD-003 step title {:?} must reference a source-law surface",
                step.title
            );
        }
    }

    #[test]
    fn ggen_yield_003_does_not_contaminate_other_species() {
        // Introducing the YIELD-003 route must NOT change the route selected
        // for TPL-001 / OUT-001 / RULE-001 / HARNESS-001 (each owns its own
        // family).
        let reg = RouteRegistry::seeded();
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-TPL-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-projection"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-OUT-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-output-path"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-RULE-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-rule-file"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-HARNESS-001", "x"))
                .expect("route")
                .id
                .0,
            "proof-topology.repair"
        );
    }

    // ---- GGEN-YIELD-004: competing-authority source-law repair route ----
    //
    // Regression guard for finding F6 (decorative-completion,
    // route/diagnostic_species.rs:171): the species table
    // (route::diagnostic_species) advertises GGEN-YIELD-004 as
    // `route: "source_law_repair"` with `detector_active: true`, and the
    // detector (analyzers::detect_yield_004, wired live via
    // check.rs::fold_yield_004) IS genuinely raised as an ERROR diagnostic
    // when multiple rules target the same output file. Before this fix,
    // `family_of_code("GGEN-YIELD-004")` fell through to `_ => None`, so
    // `select_for_diagnostic` always returned `None` for a real
    // GGEN-YIELD-004 diagnostic — a decorative-completion gap between the
    // advertised species metadata and the actual routing table (the MCP/
    // headless/hover channels got a `RouteRefusal` instead of a route).

    #[test]
    fn ggen_yield_004_maps_to_its_own_family() {
        // GGEN-YIELD-004 must resolve to a concrete (non-None) family that it
        // owns exclusively — zero cross-contamination with
        // TPL/OUT/RULE/SRC/YIELD-003/HARNESS.
        assert_eq!(
            family_of_code("GGEN-YIELD-004"),
            Some(RepairFamily::CompetingAuthority)
        );
    }

    #[test]
    fn ggen_yield_004_selects_the_source_law_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-YIELD-004", "competing authority"))
            .expect("GGEN-YIELD-004 must resolve to a seeded route, not None");
        assert_eq!(route.id.0, "source-law.resolve-competing-authority");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_yield_004_route_is_source_law_only() {
        // Load-bearing invariant: the route must NEVER target emitted output.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-YIELD-004", "competing authority"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");

        const FORBIDDEN_OUTPUT: &[&str] = &["out/", "dist/", "gen/", "emitted"];
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-YIELD-004 step {:?} must be advisory (NoOp), never an output edit",
                step.id
            );
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN_OUTPUT {
                assert!(
                    !title.contains(forbidden),
                    "GGEN-YIELD-004 step title {:?} references forbidden emitted-output \
                     marker {forbidden:?}",
                    step.title
                );
            }
            assert!(
                title.contains("ggen.toml")
                    || title.contains("output_file")
                    || title.contains("rule"),
                "GGEN-YIELD-004 step title {:?} must reference a source-law surface",
                step.title
            );
        }
    }

    #[test]
    fn ggen_yield_004_does_not_contaminate_other_species() {
        // Introducing the YIELD-004 route must NOT change the route selected
        // for TPL-001 / OUT-001 / RULE-001 / HARNESS-001 (each owns its own
        // family).
        let reg = RouteRegistry::seeded();
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-TPL-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-projection"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-OUT-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-output-path"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-RULE-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-rule-file"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-HARNESS-001", "x"))
                .expect("route")
                .id
                .0,
            "proof-topology.repair"
        );
    }

    // ---- GGEN-PACK-001: pack-source-indirection advisory repair route ----
    //
    // Regression guard for finding F3 (decorative-completion,
    // route/diagnostic_species.rs:135): the species table
    // (route::diagnostic_species) advertises GGEN-PACK-001 as
    // `route: "advisory"` with `detector_active: true`, and the detector
    // (analyzers::detect_pack_001, calling pack_001_diagnostics in
    // tera_analyzer.rs, wired live via check.rs::fold_pack_001) IS genuinely
    // raised as a WARNING diagnostic for a pack-indirected rule. Before this
    // fix, `family_of_code("GGEN-PACK-001")` fell through to `_ => None`, so
    // `select_for_diagnostic` always returned `None` for a real
    // GGEN-PACK-001 diagnostic — a decorative-completion gap between the
    // advertised species metadata and the actual routing table (the MCP/
    // headless/hover channels got a `RouteRefusal` instead of a route).

    #[test]
    fn ggen_pack_001_maps_to_its_own_family() {
        // GGEN-PACK-001 must resolve to a concrete (non-None) family that it
        // owns exclusively — zero cross-contamination with
        // TPL/OUT/RULE/SRC/YIELD-003/YIELD-004/HARNESS.
        assert_eq!(
            family_of_code("GGEN-PACK-001"),
            Some(RepairFamily::PackIndirection)
        );
    }

    #[test]
    fn ggen_pack_001_selects_the_advisory_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-PACK-001", "pack query (...)"))
            .expect("GGEN-PACK-001 must resolve to a seeded route, not None");
        assert_eq!(route.id.0, "pack.acknowledge-indirection");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_pack_001_route_is_advisory_only() {
        // Load-bearing invariant: the route must NEVER offer a destructive
        // edit — the pack's real content isn't knowable at author time.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-PACK-001", "pack query (...)"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-PACK-001 step {:?} must be advisory (NoOp), never a destructive edit",
                step.id
            );
        }
    }

    #[test]
    fn ggen_pack_001_does_not_contaminate_other_species() {
        // Introducing the PACK-001 route must NOT change the route selected
        // for TPL-001 / OUT-001 / RULE-001 / HARNESS-001 (each owns its own
        // family).
        let reg = RouteRegistry::seeded();
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-TPL-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-projection"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-OUT-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-output-path"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-RULE-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-rule-file"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-HARNESS-001", "x"))
                .expect("route")
                .id
                .0,
            "proof-topology.repair"
        );
    }

    #[test]
    fn ggen_pack_001_route_plan_is_produced_not_a_refusal() {
        // End-to-end regression: the production call site
        // (`route::route_plan_for_diagnostic`, backing the MCP tool
        // `ggen.lsp.repair_route`, `check.rs`'s `--with-routes` gate, and
        // `hover.rs`) must yield a `RoutePlan` for a real GGEN-PACK-001
        // diagnostic instead of `None` (which upstream turns into a
        // `RouteRefusal` in `mcp::build_repair_routes_in`).
        let reg = RouteRegistry::seeded();
        let d = diag(
            "GGEN-PACK-001",
            "GGEN-PACK-001: pack source resolved at generation time — \
             GGEN-TPL-001 and GGEN-OUT-001 are disabled for this rule.",
        );
        let plan = crate::route::route_plan_for_diagnostic(&reg, &d, "ggen.toml content\n")
            .expect("GGEN-PACK-001 must produce a RoutePlan, not a RouteRefusal");
        assert_eq!(plan.route_id.0, "pack.acknowledge-indirection");
    }

    // ---- GGEN-QUERY-002: SELECT * blindspot advisory route ----

    #[test]
    fn ggen_query_002_maps_to_its_own_family() {
        // GGEN-QUERY-002 must resolve to the Blindspot family, which it owns
        // exclusively (no other code maps there) — zero cross-contamination.
        assert_eq!(
            family_of_code("GGEN-QUERY-002"),
            Some(RepairFamily::Blindspot)
        );
    }

    #[test]
    fn ggen_query_002_selects_the_advisory_route() {
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-QUERY-002", "SELECT * blindspot"))
            .expect("GGEN-QUERY-002 must resolve to a seeded route");
        assert_eq!(route.id.0, "query.acknowledge-select-star");
        assert_eq!(route.provenance, Provenance::Seeded);
        assert!(route.steps.is_sound(), "route must be structurally sound");
    }

    #[test]
    fn ggen_query_002_route_is_advisory_only() {
        // Load-bearing invariant: the route must NEVER edit emitted output —
        // choosing the "right" explicit projection list is not knowable from
        // the diagnostic alone.
        let reg = RouteRegistry::seeded();
        let route = reg
            .select_for_diagnostic(&diag("GGEN-QUERY-002", "SELECT * blindspot"))
            .expect("route");

        assert!(!route.steps.nodes.is_empty(), "route must have steps");

        const FORBIDDEN_OUTPUT: &[&str] = &["out/", "output/", "dist/", "gen/", "emitted"];
        for step in &route.steps.nodes {
            assert!(
                matches!(step.edit, EditTemplate::NoOp),
                "GGEN-QUERY-002 step {:?} must be advisory (NoOp), never an output edit",
                step.id
            );
            let title = step.title.to_lowercase();
            for forbidden in FORBIDDEN_OUTPUT {
                assert!(
                    !title.contains(forbidden),
                    "GGEN-QUERY-002 step title {:?} references forbidden emitted-output \
                     marker {forbidden:?}",
                    step.title
                );
            }
        }
    }

    #[test]
    fn ggen_query_002_does_not_contaminate_other_species() {
        // Introducing the QUERY-002 route must NOT change the route selected
        // for TPL-001 / OUT-001 / RULE-001 / HARNESS-001 (each owns its own
        // family).
        let reg = RouteRegistry::seeded();
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-TPL-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-projection"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-OUT-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-output-path"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-RULE-001", "x"))
                .expect("route")
                .id
                .0,
            "source-law.bind-rule-file"
        );
        assert_eq!(
            reg.select_for_diagnostic(&diag("GGEN-HARNESS-001", "x"))
                .expect("route")
                .id
                .0,
            "proof-topology.repair"
        );
    }

    #[test]
    fn ggen_query_002_route_plan_is_produced_not_a_refusal() {
        // End-to-end regression matching finding F2's exact failure scenario:
        // the production call site (`route::route_plan_for_diagnostic`,
        // backing the MCP tool `ggen.lsp.repair_route`, `check.rs`'s
        // `--with-routes` gate via `fold_query_002`/`fold_species`, and
        // `hover.rs`) must yield a `RoutePlan` for a real GGEN-QUERY-002
        // diagnostic instead of `None` (which upstream turns into a
        // `RouteRefusal` in `mcp::build_repair_routes_in`).
        let reg = RouteRegistry::seeded();
        let d = diag(
            "GGEN-QUERY-002",
            "GGEN-QUERY-002 rule `r1` uses SELECT * — explicit projections required.",
        );
        let plan = crate::route::route_plan_for_diagnostic(&reg, &d, "ggen.toml content\n")
            .expect("GGEN-QUERY-002 must produce a RoutePlan, not a RouteRefusal");
        assert_eq!(plan.route_id.0, "query.acknowledge-select-star");
    }
}
