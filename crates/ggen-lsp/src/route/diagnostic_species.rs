//! Diagnostic species registry — the taxonomy of living-LSP diagnostics.
//!
//! A *diagnostic species* is the stable, declarative description of a class of
//! defect the language server can recognise. It is metadata only: it records
//! the failure class, the source-law surfaces involved, the severity policy,
//! the repair route slug, provenance origin, actuation boundary, and the kind
//! of receipt a repair must emit. Crucially it also records whether a *detector*
//! is active for the species — Phase 2 species may be registered as metadata
//! before any detector exists.
//!
//! This registry is the shared contract between the analyzers (which produce
//! diagnostics) and the route registry (which selects repairs). The canonical
//! string values are fixed by the GGEN-TPL-001 pre-implementation inventory and
//! MUST NOT vary.

/// Declarative description of a class of diagnostic the LSP recognises.
///
/// All fields are `&'static` so a species table can live in `rodata` and be
/// shared without allocation. Species are *metadata*: registering a species does
/// not by itself make the LSP emit it — that requires `detector_active == true`
/// and a corresponding analyzer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiagnosticSpecies {
    /// Stable diagnostic code (e.g. `"GGEN-TPL-001"`).
    pub code: &'static str,
    /// The failure class this species belongs to (e.g. `"unbound_projection"`).
    pub failure_class: &'static str,
    /// Source-law surfaces involved in the defect (e.g. `["ggen.toml", "SPARQL", "Tera"]`).
    pub surfaces: &'static [&'static str],
    /// Severity policy slug (e.g. `"error"`, `"release_blocking"`).
    pub severity_policy: &'static str,
    /// Repair route slug this species maps to (e.g. `"source_law_repair"`).
    pub route: &'static str,
    /// Where this species originated (provenance for explainability).
    pub origin: &'static str,
    /// How far the system may actuate when repairing — e.g. `"inspect_only"`.
    pub actuation_boundary: &'static str,
    /// The kind of receipt a repair of this species must emit.
    pub receipt_requirement: &'static str,
    /// Whether a detector is currently active for this species.
    ///
    /// `false` means the species is registered as metadata only (Phase 2): the
    /// taxonomy knows about it, but no analyzer emits it yet.
    pub detector_active: bool,
}

/// The compiled-in species table.
///
/// Values are bound by the GGEN-TPL-001 inventory's "Canonical Values" section.
static SPECIES: &[DiagnosticSpecies] = &[
    // ── GGEN-TPL-001: unbound projection (active) ─────────────────────────
    DiagnosticSpecies {
        code: "GGEN-TPL-001",
        failure_class: "unbound_projection",
        surfaces: &["ggen.toml", "SPARQL", "Tera"],
        severity_policy: "error",
        route: "source_law_repair",
        origin: "ark-covenant / living-lsp MVP",
        actuation_boundary: "inspect_only",
        receipt_requirement: "diagnostic_receipt",
        detector_active: true,
    },
    // ── GGEN-HARNESS-001: harness mismatch (ACTIVE — GALL-CHECKPOINT-002) ─
    DiagnosticSpecies {
        code: "GGEN-HARNESS-001",
        failure_class: "harness_mismatch",
        surfaces: &["Cargo.toml", "tests/proof", "Makefile.toml"],
        severity_policy: "release_blocking",
        route: "proof_topology_repair",
        origin: "ark-covenant ProofPack runner failure",
        actuation_boundary: "inspect_only",
        receipt_requirement: "boundary_receipt",
        // Activated (GALL-CHECKPOINT-002): a live detector now compares Cargo.toml
        // [[test]]/[[bench]] explicit-`path` declarations against the proof files
        // on disk (analyzers::detect_harness_001 over a harness_index::HarnessIndex).
        detector_active: true,
    },
    // ── GGEN-OUT-001: unbound output path (ACTIVE — GALL-OUT-001) ────────────
    DiagnosticSpecies {
        code: "GGEN-OUT-001",
        failure_class: "unbound_output_path",
        surfaces: &["ggen.toml", "SPARQL"],
        severity_policy: "error",
        route: "source_law_repair",
        origin: "ark-covenant / living-lsp OUT-001",
        actuation_boundary: "inspect_only",
        receipt_requirement: "diagnostic_receipt",
        // Activated (GALL-OUT-001): a live detector now compares each rule's
        // dynamic `output_file` Tera pattern against the rule's SPARQL SELECT
        // variables (analyzers::detect_out_001 over a project_index::ProjectIndex).
        // The dual of GGEN-TPL-001 on the ggen.toml/SPARQL surfaces.
        detector_active: true,
    },
];

/// Borrow the full species registry.
pub fn species_registry() -> &'static [DiagnosticSpecies] {
    SPECIES
}

/// Look up a species by its diagnostic code.
///
/// Returns `None` for codes not in the registry.
pub fn species_for(code: &str) -> Option<&'static DiagnosticSpecies> {
    SPECIES.iter().find(|s| s.code == code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ggen_tpl_001_is_active_with_canonical_values() {
        let species = species_for("GGEN-TPL-001").expect("GGEN-TPL-001 must be registered");
        assert!(
            species.detector_active,
            "GGEN-TPL-001 must have an active detector"
        );
        assert_eq!(species.failure_class, "unbound_projection");
        assert_eq!(species.severity_policy, "error");
        assert_eq!(species.route, "source_law_repair");
        assert_eq!(species.origin, "ark-covenant / living-lsp MVP");
        assert_eq!(species.actuation_boundary, "inspect_only");
        assert_eq!(species.receipt_requirement, "diagnostic_receipt");
        assert_eq!(species.surfaces, &["ggen.toml", "SPARQL", "Tera"]);
    }

    #[test]
    fn ggen_harness_001_is_active() {
        let species = species_for("GGEN-HARNESS-001").expect("GGEN-HARNESS-001 must be registered");
        assert!(
            species.detector_active,
            "GGEN-HARNESS-001 detector must be active (GALL-CHECKPOINT-002)"
        );
        assert_eq!(species.failure_class, "harness_mismatch");
        assert_eq!(species.severity_policy, "release_blocking");
        assert_eq!(species.route, "proof_topology_repair");
        assert_eq!(species.receipt_requirement, "boundary_receipt");
        assert_eq!(
            species.surfaces,
            &["Cargo.toml", "tests/proof", "Makefile.toml"]
        );
    }

    #[test]
    fn ggen_out_001_is_active_with_canonical_values() {
        let species = species_for("GGEN-OUT-001").expect("GGEN-OUT-001 must be registered");
        assert!(
            species.detector_active,
            "GGEN-OUT-001 detector must be active (GALL-OUT-001)"
        );
        assert_eq!(species.failure_class, "unbound_output_path");
        assert_eq!(species.severity_policy, "error");
        assert_eq!(species.route, "source_law_repair");
        assert_eq!(species.origin, "ark-covenant / living-lsp OUT-001");
        assert_eq!(species.actuation_boundary, "inspect_only");
        assert_eq!(species.receipt_requirement, "diagnostic_receipt");
        assert_eq!(species.surfaces, &["ggen.toml", "SPARQL"]);
    }

    #[test]
    fn unknown_code_has_no_species() {
        assert!(species_for("E0011").is_none());
        assert!(species_for("does-not-exist").is_none());
    }

    #[test]
    fn registry_contains_exactly_three_species() {
        assert_eq!(species_registry().len(), 3);
    }

    #[test]
    fn actuation_boundary_is_inspect_only_for_all_species() {
        // No species may actuate beyond inspection in the MVP.
        for species in species_registry() {
            assert_eq!(
                species.actuation_boundary, "inspect_only",
                "{} must be inspect_only",
                species.code
            );
        }
    }
}
