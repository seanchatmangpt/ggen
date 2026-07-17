//! `LawEngine` — the seam between this crate's law/SHACL/N3 evaluation
//! (backed by `praxis-graphlaw`, an oxrdf-0.3.x-family RDF library distinct
//! from this crate's own oxigraph 0.5.9) and the oxigraph-based crates
//! (`ggen-graph`, `ggen-marketplace`) that need its output without taking on
//! that dependency themselves.
//!
//! Contract: `specs/014-ggen-core-replacement/contracts/law-engine-trait.md`.
//! No `oxrdf`/`spargebra`/`oxigraph` model type may appear in this trait's
//! signature — only plain strings (N-Triples facts, N3 rules, Turtle
//! shapes) cross the boundary. Callers own re-ingestion into their own
//! store; an implementation never reaches into a caller's store.

use std::collections::BTreeSet;

use crate::error::{AppError, Result};
use crate::graph::{MaterializeOutcome, ShaclOutcome};

/// Exposed by this crate to `ggen-graph`/`ggen-marketplace`. Every call is
/// independent — each builds a fresh `praxis_graphlaw::TripleStore` from its
/// `facts_ntriples`/`rules_n3` arguments (the same technique
/// [`crate::graph::GraphLawStore`] uses internally, minus the persistent
/// mirror: callers here own re-ingestion, per contract rule 2).
pub trait LawEngine: Send + Sync {
    /// Forward-chain `rules_n3` over `facts_ntriples` to fixpoint.
    ///
    /// # Errors
    /// Typed refusal (`[FM-LAW-*]`) on unparseable facts/rules, or when a
    /// `Refuse` hook fires during materialization.
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

/// The only [`LawEngine`] implementation: `praxis-graphlaw` as the law-state
/// engine. Carries no state of its own — see the trait's doc comment.
#[derive(Debug, Default, Clone, Copy)]
pub struct GraphLawEngine;

impl GraphLawEngine {
    /// Construct the engine. Takes no state: every [`LawEngine`] call below
    /// builds its own `praxis_graphlaw::TripleStore` from its arguments.
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Build a `TripleStore` loaded with `facts_ntriples` and (if non-empty)
    /// `rules_n3`.
    fn build_store(facts_ntriples: &str, rules_n3: &str) -> Result<praxis_graphlaw::TripleStore> {
        use praxis_graphlaw::parser::Syntax;
        let mut ts = praxis_graphlaw::TripleStore::new();
        ts.load_triples(facts_ntriples, Syntax::NTriples)
            .map_err(|e| {
                AppError::fm_law(10, format!("GraphLaw fact load (N-Triples) refused: {e}"))
            })?;
        if !rules_n3.trim().is_empty() {
            ts.load_rules(rules_n3)
                .map_err(|e| AppError::fm_law(11, format!("GraphLaw rule load refused: {e}")))?;
        }
        Ok(ts)
    }

    /// Decode a triple slice into a set of N-Triples lines without the
    /// terminating `.`, via the same `decode_triples` codec used on both
    /// sides of a materialize diff (avoids format mismatches between
    /// caller-supplied N-Triples and this crate's own decoder output).
    fn decoded_line_set(triples: &[praxis_graphlaw::term::Triple]) -> BTreeSet<String> {
        praxis_graphlaw::TripleStore::decode_triples(triples)
            .lines()
            .map(str::trim)
            .filter(|l| !l.is_empty())
            .map(|l| l.strip_suffix('.').unwrap_or(l).trim_end().to_string())
            .collect()
    }
}

impl LawEngine for GraphLawEngine {
    fn materialize(&self, facts_ntriples: &str, rules_n3: &str) -> Result<MaterializeOutcome> {
        let mut ts = Self::build_store(facts_ntriples, rules_n3)?;
        let rules_loaded = ts.rules.len();
        let before = Self::decoded_line_set(&ts.triple_index.triples);

        let inferred = ts
            .materialize()
            .map_err(|e| AppError::fm_law(12, format!("Reasoner materialize failed: {e}")))?;

        if let Some(refused) = ts.verdicts.iter().find(|v| {
            v.effect == praxis_graphlaw::hooks::EffectKind::Refuse
                && v.verdict == praxis_graphlaw::hooks::HookVerdict::Fired
        }) {
            let reason = refused
                .diagnostics
                .as_ref()
                .and_then(|d| d.details.first())
                .map_or_else(|| "refused by hook".to_string(), |det| det.message.clone());
            return Err(AppError::fm_law(
                12,
                format!("Reasoner materialize failed: {reason}"),
            ));
        }

        let mut derived: Vec<String> = Self::decoded_line_set(&inferred)
            .into_iter()
            .filter(|l| !before.contains(l))
            .collect();
        derived.sort();

        Ok(MaterializeOutcome {
            derived,
            rules_loaded,
        })
    }

    fn validate_shacl(&self, facts_ntriples: &str, shapes_ttl: &str) -> Result<ShaclOutcome> {
        let ts = Self::build_store(facts_ntriples, "")?;
        let report = ts
            .validate_shacl(shapes_ttl)
            .map_err(|e| AppError::fm_law(13, format!("SHACL shapes graph refused: {e}")))?;
        let violations = report
            .results
            .iter()
            .map(|r| {
                let msg = r.message.as_deref().unwrap_or("constraint violated");
                format!(
                    "focus node {}: {msg} (source shape {})",
                    r.focus_node, r.source_shape
                )
            })
            .collect();
        Ok(ShaclOutcome {
            conforms: report.conforms,
            violations,
        })
    }

    fn check_denials(&self, facts_ntriples: &str, rules_n3: &str) -> Result<Vec<String>> {
        let mut ts = Self::build_store(facts_ntriples, rules_n3)?;
        ts.materialize()
            .map_err(|e| AppError::fm_law(14, format!("Reasoner materialize failed: {e}")))?;
        Ok(ts.check_denials())
    }
}
