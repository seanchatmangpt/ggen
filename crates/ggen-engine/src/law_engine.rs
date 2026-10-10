//! `LawEngine` — the seam between this crate's law/SHACL/N3 evaluation
//! (backed by `graphlaw`: Eyeron for N3, `PurRDF` for SHACL/ShEx/SPARQL hooks)
//! and the oxigraph-based crates (`ggen-graph`, `ggen-marketplace`) that need
//! its output without taking on that dependency themselves.
//!
//! Contract: `specs/014-ggen-core-replacement/contracts/law-engine-trait.md`.
//! No `oxrdf`/`spargebra`/`oxigraph` model type may appear in this trait's
//! signature — only plain strings (N-Triples facts, N3 rules, Turtle
//! shapes) cross the boundary. Callers own re-ingestion into their own
//! store; an implementation never reaches into a caller's store.
//!
//! The `pub(crate)` helpers below (`n3_run`, `hooks_apply`,
//! `shacl_check`, `shex_check`) are the single graphlaw call surface;
//! [`crate::graph::GraphLawStore`] reuses them so there is one kernel binding.

use std::collections::BTreeSet;

use graphlaw::{
    dialect::Dialect,
    hooks::{HookPack, Verdict},
    law::{LawError, LawState, Step},
    n3,
};

use crate::error::{AppError, Result};
use crate::graph::{MaterializeOutcome, ShaclOutcome};

/// Why an N3 run did not produce a derivation.
#[derive(Debug)]
pub(crate) enum N3Failure {
    /// Facts/rules failed to parse.
    Load(String),
    /// Reasoning stopped on a resource limit or engine refusal.
    Reason(String),
}

/// Result of one bounded N3 forward-chaining run.
#[derive(Debug, Default)]
pub(crate) struct N3Run {
    /// Newly derived triples as an N-Triples document.
    pub derived_nt: String,
    /// Rendered `DENIED: …` line when a `{ body } => false.` fuse fired.
    pub fuse: Option<String>,
    /// Number of rules (including denial fuses) in the rule documents.
    pub rules: usize,
}

fn to_ntriples(n3_text: &str) -> std::result::Result<String, String> {
    if n3_text.trim().is_empty() {
        return Ok(String::new());
    }
    let ds = graphlaw::purrdf::parse_dataset(n3_text.as_bytes(), "text/turtle", None)
        .map_err(|e| format!("derived facts are not valid Turtle: {e:?}"))?;
    let bytes = graphlaw::purrdf::serialize_dataset(
        ds.as_ref(),
        "application/n-quads",
        graphlaw::rdf::SerializeGraph::Dataset,
    )
    .map_err(|e| format!("derived facts could not be serialized: {e:?}"))?;
    String::from_utf8(bytes).map_err(|e| e.to_string())
}

/// Forward-chain `rules` over `facts_nt` with `GraphLaw`'s resource limits.
/// A fired `=> false` fuse is reported in [`N3Run::fuse`] (Eyeron clears the
/// derivation in that case; `graphlaw::law::reason_n3_bounded` would hide it).
pub(crate) fn n3_run(facts_nt: &str, rules: &[&str]) -> std::result::Result<N3Run, N3Failure> {
    if rules.is_empty() {
        return Ok(N3Run::default());
    }
    let mut text = String::from(facts_nt);
    text.push('\n');
    for rule in rules {
        text.push_str(rule);
        text.push('\n');
    }
    let doc = n3::parse_n3(&text, None).map_err(|e| N3Failure::Load(e.to_string()))?;
    let options = n3::ReasonerOptions {
        include_explicit: false,
        max_iterations: graphlaw::law::N3_MAX_ITERATIONS,
        max_term_bytes: graphlaw::law::N3_MAX_TERM_BYTES,
        max_total_bytes: graphlaw::law::N3_MAX_TOTAL_BYTES,
        max_closure_facts: graphlaw::law::N3_MAX_DERIVED_FACTS,
        max_total_steps: graphlaw::law::N3_MAX_TOTAL_STEPS,
        ..n3::ReasonerOptions::default()
    };
    let result = n3::reason_document(&doc, &options);
    let rule_count = doc.rules.len();
    if let Some(fuse) = &result.fuse {
        let instance = n3::triples_to_n3(&doc.prefixes, &fuse.instance);
        let flat = instance.split_whitespace().collect::<Vec<_>>().join(" ");
        return Ok(N3Run {
            derived_nt: String::new(),
            fuse: Some(format!("DENIED: {{{flat}}} => false.")),
            rules: rule_count,
        });
    }
    if let Some(summary) = result.incomplete_summary() {
        return Err(N3Failure::Reason(summary));
    }
    let derived_nt = to_ntriples(&n3::triples_to_n3(&doc.prefixes, &result.derived))
        .map_err(N3Failure::Reason)?;
    Ok(N3Run {
        derived_nt,
        fuse: None,
        rules: rule_count,
    })
}

fn parse_state(facts_nt: &str) -> std::result::Result<LawState, String> {
    LawState::parse(facts_nt.as_bytes(), Dialect::NTriples, None).map_err(|e| e.to_string())
}

/// Validate a `kh:` hook pack document (Turtle) without running it.
pub(crate) fn hook_pack_check(hook_ttl: &str) -> std::result::Result<(), String> {
    let pack_state =
        LawState::parse(hook_ttl.as_bytes(), Dialect::Turtle, None).map_err(|e| e.to_string())?;
    HookPack::load(&pack_state)
        .map(|_| ())
        .map_err(|e| e.to_string())
}

/// Result of running hook packs over a fact state.
pub(crate) struct HookApplyOutcome {
    /// The whole resulting state as N-Triples (hook-firing markers
    /// included, so a re-run over the folded-back state fires nothing
    /// twice).
    pub state_nt: String,
    /// One `DENIED:` line per [`graphlaw::hooks::Verdict::Refuse`] verdict,
    /// naming the hook and its refuse reason. A refuse firing merges no
    /// delta — it is a named, pack-declared fuse the consumer refuses on.
    pub denied: Vec<String>,
}

/// Run every hook pack to fixpoint over `facts_nt`.
pub(crate) fn hooks_apply(
    facts_nt: &str, hooks: &[String],
) -> std::result::Result<HookApplyOutcome, String> {
    let mut state = parse_state(facts_nt)?;
    let mut denied = Vec::new();
    for hook_ttl in hooks {
        let pack_state = LawState::parse(hook_ttl.as_bytes(), Dialect::Turtle, None)
            .map_err(|e| e.to_string())?;
        let pack = HookPack::load(&pack_state).map_err(|e| e.to_string())?;
        let m = pack.materialize(&state).map_err(|e| e.to_string())?;
        for v in &m.verdicts {
            if let Verdict::Refuse(reason) = &v.verdict {
                denied.push(format!("DENIED: hook <{}> refused: {reason}", v.hook));
            }
        }
        state = m.state;
    }
    let bytes = graphlaw::purrdf::serialize_dataset(
        state.dataset().as_ref(),
        "application/n-quads",
        graphlaw::rdf::SerializeGraph::Dataset,
    )
    .map_err(|e| format!("{e:?}"))?;
    Ok(HookApplyOutcome {
        state_nt: String::from_utf8(bytes).map_err(|e| e.to_string())?,
        denied,
    })
}

/// SHACL-validate `facts_nt` against a Turtle shapes document.
pub(crate) fn shacl_check(
    facts_nt: &str, shapes_ttl: &str,
) -> std::result::Result<ShaclOutcome, String> {
    let state = parse_state(facts_nt)?;
    match state.transition(&Step::AdmitShacl { shapes_ttl }) {
        Ok(_) => Ok(ShaclOutcome {
            conforms: true,
            violations: Vec::new(),
        }),
        Err(LawError::NotAdmitted { results, .. }) => Ok(ShaclOutcome {
            conforms: false,
            violations: results
                .iter()
                .map(|r| {
                    let msg = if r.message.is_empty() {
                        "constraint violated"
                    } else {
                        r.message.as_str()
                    };
                    format!(
                        "focus node {}: {msg} (source shape {})",
                        r.focus, r.component
                    )
                })
                .collect(),
        }),
        Err(other) => Err(other.to_string()),
    }
}

fn shape_map_term(term: &str) -> String {
    if term.starts_with('<') || term.starts_with("_:") || term.starts_with('"') {
        term.to_string()
    } else {
        format!("<{term}>")
    }
}

/// ShExC-validate `facts_nt` for the given `(node, shape)` pairs.
pub(crate) fn shex_check(
    facts_nt: &str, schema_shexc: &str, shape_map: &[(String, String)],
) -> std::result::Result<ShaclOutcome, String> {
    let state = parse_state(facts_nt)?;
    let schema = graphlaw::shex::parse_shexc(schema_shexc, None).map_err(|e| e.to_string())?;
    graphlaw::shex::check_structure(&schema).map_err(|e| format!("{e:?}"))?;
    let map_text = shape_map
        .iter()
        .map(|(n, s)| format!("{}@{}", shape_map_term(n), shape_map_term(s)))
        .collect::<Vec<_>>()
        .join(", ");
    let map = graphlaw::shex::validate_shape_map(
        &schema,
        state.dataset(),
        &map_text,
        None,
        &graphlaw::shex::ValidationOptions::default(),
    )
    .map_err(|e| e.to_string())?;
    let violations = map
        .entries
        .iter()
        .filter(|e| format!("{:?}", e.status) == "Nonconformant")
        .map(|e| {
            format!(
                "focus node {:?}: does not conform to shape {:?}: {:?}",
                e.node, e.shape, e.reason
            )
        })
        .collect::<Vec<_>>();
    Ok(ShaclOutcome {
        conforms: map.all_conformant(),
        violations,
    })
}

/// Exposed by this crate to `ggen-graph`/`ggen-marketplace`. Every call is
/// independent — each runs graphlaw over its `facts_ntriples`/`rules_n3`
/// arguments (the same kernel [`crate::graph::GraphLawStore`] uses
/// internally, minus the persistent mirror: callers here own re-ingestion,
/// per contract rule 2).
pub trait LawEngine: Send + Sync {
    /// Forward-chain `rules_n3` over `facts_ntriples` to fixpoint.
    ///
    /// # Errors
    /// Typed refusal (`[FM-LAW-*]`) on unparseable facts/rules or a
    /// resource-limit refusal during materialization.
    fn materialize(&self, facts_ntriples: &str, rules_n3: &str) -> Result<MaterializeOutcome>;

    /// Validate `facts_ntriples` against a Turtle SHACL shapes graph.
    ///
    /// # Errors
    /// Typed refusal on unparseable facts or an invalid shapes graph.
    fn validate_shacl(&self, facts_ntriples: &str, shapes_ttl: &str) -> Result<ShaclOutcome>;

    /// Evaluate every denial rule (`{ body } => false.`) in `rules_n3`
    /// against `facts_ntriples`, after materializing to fixpoint; one line
    /// per violated denial.
    ///
    /// # Errors
    /// Typed refusal on unparseable facts/rules.
    fn check_denials(&self, facts_ntriples: &str, rules_n3: &str) -> Result<Vec<String>>;
}

/// The only [`LawEngine`] implementation: `graphlaw` as the law-state
/// engine. Carries no state of its own — see the trait's doc comment.
#[derive(Debug, Default, Clone, Copy)]
pub struct GraphLawEngine;

impl GraphLawEngine {
    /// Construct the engine. Takes no state.
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    fn run(facts_ntriples: &str, rules_n3: &str, code: u16) -> Result<N3Run> {
        let rules: Vec<&str> = if rules_n3.trim().is_empty() {
            Vec::new()
        } else {
            vec![rules_n3]
        };
        n3_run(facts_ntriples, &rules).map_err(|e| match e {
            N3Failure::Load(m) => {
                AppError::fm_law(code, format!("GraphLaw rule/fact load refused: {m}"))
            }
            N3Failure::Reason(m) => {
                AppError::fm_law(code + 1, format!("Reasoner materialize failed: {m}"))
            }
        })
    }
}

impl LawEngine for GraphLawEngine {
    fn materialize(&self, facts_ntriples: &str, rules_n3: &str) -> Result<MaterializeOutcome> {
        let run = Self::run(facts_ntriples, rules_n3, 10)?;
        let before: BTreeSet<String> = facts_ntriples
            .lines()
            .map(|l| l.trim().trim_end_matches('.').trim_end().to_string())
            .collect();
        let mut derived: Vec<String> = run
            .derived_nt
            .lines()
            .map(|l| l.trim().trim_end_matches('.').trim_end().to_string())
            .filter(|l| !l.is_empty() && !before.contains(l))
            .collect();
        derived.sort();
        derived.dedup();
        Ok(MaterializeOutcome {
            derived,
            rules_loaded: run.rules,
        })
    }

    fn validate_shacl(&self, facts_ntriples: &str, shapes_ttl: &str) -> Result<ShaclOutcome> {
        shacl_check(facts_ntriples, shapes_ttl)
            .map_err(|e| AppError::fm_law(13, format!("SHACL shapes graph refused: {e}")))
    }

    fn check_denials(&self, facts_ntriples: &str, rules_n3: &str) -> Result<Vec<String>> {
        let run = Self::run(facts_ntriples, rules_n3, 14)?;
        Ok(run.fuse.into_iter().collect())
    }
}
