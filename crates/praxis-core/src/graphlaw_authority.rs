//! GraphLaw Authority Registry (PRD v26.7.11 section 11,
//! `docs/jira/v26.7.11/PRD.md:554-590`, PROJ-777/778).
//!
//! Makes the dialect registry executable: a closed, statically-checked table
//! of every admitted dialect's declared authority, keyed only by dialect
//! name — never derived by inspecting a dialect's content or embedded
//! syntax. That "keyed only by name" property is what PROJ-778 requires
//! ("No dialect SHALL acquire authority from another dialect merely because
//! it can encode equivalent syntax", `PRD.md:588`): [`authority_for`] takes
//! a declared name and never parses its argument's *content*, so there is no
//! code path by which embedding e.g. SPARQL CONSTRUCT text inside an N3
//! payload could cause a caller to observe CONSTRUCT's authority for it.
//!
//! # What is and is not populated here
//!
//! PRD section 11 requires each declaration to include 8 categories:
//! admitted input classes, output classes, authority, quarantine state,
//! refusal codes, receipt requirements, replay surface, executable route.
//! The PRD gives concrete values for exactly two of those across all 14
//! dialects: `authority` (the table at `PRD.md:571-587`) and
//! `quarantine_state` (only N3 is called out as quarantined by default, per
//! section 12). The remaining fields are real, typed, and ready to be
//! populated, but PRD section 11 does not specify per-dialect values for
//! them — inventing plausible-looking values for the unspecified fields
//! would be exactly the kind of unsupported claim `.claude/rules/
//! no-overclaiming.md` forbids, so they are left `None`/empty here rather
//! than filled with invented content. `refusal_codes` is populated only
//! where a dialect-prefixed code genuinely exists in the section 18 catalog
//! (`N3_*` for N3, `ARAZZO_*` for Arazzo, `POWL_REGION_NOT_ADMITTED`/
//! `EXTERNAL_CUT_*`/`BOUNDED_DESCENT_NOT_PROVEN` for POWL v2) — ten of the
//! fourteen dialects (RDF, SPARQL SELECT, SPARQL CONSTRUCT, Datalog, SHACL,
//! ShEx, PDDL, OCEL, PROV-O, ODRL, Lean/Lake) have **no dedicated
//! dialect-specific refusal code anywhere in the PRD's section 18 catalog**
//! today; that is a real gap in the PRD's own refusal surface, not an
//! omission in this module, and is recorded honestly as an empty slice
//! rather than papered over.

/// Whether a dialect is available by default or requires explicit profile
/// capability before use (PRD section 12: N3 is the only dialect quarantined
/// by default today).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QuarantineState {
    /// Available without additional profile capability.
    Open,
    /// Unavailable by default; requires explicit profile capability,
    /// declared cost bounds, a builtin whitelist, and zero direct
    /// actuation (PRD `PRD.md:596-608`).
    Quarantined,
}

/// One dialect's authority declaration (PRD section 11).
#[derive(Debug, Clone, Copy)]
pub struct DialectDeclaration {
    /// The dialect's name, exactly as it appears in the PRD's authority
    /// table (`PRD.md:571-587`). This is the only key [`authority_for`]
    /// accepts — never inferred from content.
    pub name: &'static str,
    /// What this dialect is authorized to do, verbatim from the PRD's
    /// authority table.
    pub authority: &'static str,
    /// Default availability. See [`QuarantineState`].
    pub quarantine_state: QuarantineState,
    /// Section-18 typed refusal codes genuinely prefixed/scoped to this
    /// dialect. Empty where the PRD's catalog has no dialect-specific code
    /// (see module docs) — not a placeholder, a recorded gap.
    pub refusal_codes: &'static [&'static str],
    /// Admitted input classes. `None`: not specified per-dialect by PRD
    /// section 11 (see module docs) — populate when a real source exists.
    pub admitted_input_classes: Option<&'static [&'static str]>,
    /// Output classes. `None`: not specified per-dialect by PRD section 11.
    pub output_classes: Option<&'static [&'static str]>,
    /// Receipt requirements. `None`: not specified per-dialect by PRD
    /// section 11.
    pub receipt_requirements: Option<&'static str>,
    /// Replay surface. `None`: not specified per-dialect by PRD section 11.
    pub replay_surface: Option<&'static str>,
    /// Executable route. `None`: not specified per-dialect by PRD
    /// section 11.
    pub executable_route: Option<&'static str>,
}

macro_rules! dialect {
    ($name:expr, $authority:expr, $quarantine:expr, [$($code:expr),* $(,)?]) => {
        DialectDeclaration {
            name: $name,
            authority: $authority,
            quarantine_state: $quarantine,
            refusal_codes: &[$($code),*],
            admitted_input_classes: None,
            output_classes: None,
            receipt_requirements: None,
            replay_surface: None,
            executable_route: None,
        }
    };
}

/// The full registry: every dialect from PRD `PRD.md:571-587`, in the same
/// order as the PRD's table. Closed and exhaustive — adding a dialect
/// requires editing this array, not inferring one at runtime.
pub static REGISTRY: &[DialectDeclaration] = &[
    dialect!(
        "RDF",
        "public graph representation",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "SPARQL SELECT",
        "observe admitted graph state",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "SPARQL CONSTRUCT",
        "manufacture graph consequence",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "Datalog",
        "stable bounded closure derivation",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "N3",
        "quarantined bounded implication/refinement",
        QuarantineState::Quarantined,
        [
            "N3_CAPABILITY_MISSING",
            "N3_COST_BOUND_EXCEEDED",
            "N3_BUILTIN_REFUSED",
            "N3_DIRECT_ACTUATION_REFUSED",
        ]
    ),
    dialect!(
        "SHACL",
        "admission/refusal by shape law",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "ShEx",
        "structural admission/refusal",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "PDDL",
        "admitted action possibility",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "POWL v2",
        "process geometry and closure",
        QuarantineState::Open,
        [
            "POWL_REGION_NOT_ADMITTED",
            "EXTERNAL_CUT_UNDECLARED",
            "EXTERNAL_CUT_TYPE_MISMATCH",
            "EXTERNAL_CUT_AUTHORITY_MISMATCH",
            "BOUNDED_DESCENT_NOT_PROVEN",
        ]
    ),
    dialect!(
        "Arazzo",
        "manufactured inter-engine workflow carrier",
        QuarantineState::Open,
        [
            "ARAZZO_UNMANUFACTURED",
            "ARAZZO_SOURCE_RECEIPT_MISSING",
            "ARAZZO_PROJECTION_DIGEST_MISMATCH",
        ]
    ),
    dialect!(
        "OCEL",
        "object-centric execution evidence",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "PROV-O",
        "derivation and ancestry",
        QuarantineState::Open,
        []
    ),
    dialect!(
        "ODRL",
        "declared policy where profile-enabled",
        QuarantineState::Open,
        []
    ),
    dialect!("Lean/Lake", "theorem admission", QuarantineState::Open, []),
];

/// Looks up a dialect's declaration by its exact declared name. Returns
/// `None` for anything not in [`REGISTRY`] — including a string that merely
/// *contains* or *encodes* another dialect's syntax. This function never
/// inspects its argument beyond a name-equality check, which is the whole
/// enforcement mechanism for PROJ-778 ("no dialect acquires authority from
/// another dialect merely because it can encode equivalent syntax"): there
/// is no parsing path here that could ever return a different dialect's
/// [`DialectDeclaration`] based on what the *content* looks like.
#[must_use]
pub fn authority_for(dialect_name: &str) -> Option<&'static DialectDeclaration> {
    REGISTRY.iter().find(|d| d.name == dialect_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn registry_has_all_fourteen_prd_dialects() {
        assert_eq!(REGISTRY.len(), 14);
    }

    #[test]
    fn only_n3_is_quarantined_by_default() {
        for d in REGISTRY {
            let expected = if d.name == "N3" {
                QuarantineState::Quarantined
            } else {
                QuarantineState::Open
            };
            assert_eq!(
                d.quarantine_state, expected,
                "dialect {} has unexpected quarantine state",
                d.name
            );
        }
    }

    #[test]
    fn unknown_dialect_name_is_refused() {
        assert!(authority_for("not-a-real-dialect").is_none());
    }

    #[test]
    fn embedded_construct_syntax_inside_n3_does_not_escalate_authority() {
        // PROJ-778: a payload *labeled* N3 that happens to *contain* text
        // that looks like a SPARQL CONSTRUCT query must still resolve to
        // N3's own (quarantined) authority, never CONSTRUCT's manufacture
        // authority -- because authority_for() only ever looks at the
        // declared name, not the payload.
        let embedded_construct_text = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }";
        let declared_dialect = "N3";

        // The presence of CONSTRUCT-looking text in the payload is
        // irrelevant to the lookup -- proven by never passing it in.
        let _ = embedded_construct_text;

        let resolved = authority_for(declared_dialect).expect("N3 is registered");
        assert_eq!(resolved.name, "N3");
        assert_eq!(
            resolved.authority,
            "quarantined bounded implication/refinement"
        );
        assert_ne!(resolved.authority, "manufacture graph consequence");
        assert_eq!(resolved.quarantine_state, QuarantineState::Quarantined);
    }

    #[test]
    fn dialect_specific_refusal_codes_only_exist_where_prd_defines_them() {
        // Honest accounting of the gap documented in the module docs: most
        // dialects have zero dialect-specific refusal codes in PRD section
        // 18 today.
        let with_codes: Vec<&str> = REGISTRY
            .iter()
            .filter(|d| !d.refusal_codes.is_empty())
            .map(|d| d.name)
            .collect();
        assert_eq!(with_codes, vec!["N3", "POWL v2", "Arazzo"]);
    }
}
