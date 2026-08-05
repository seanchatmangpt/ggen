//! CP8 (Gall's-Law checkpoint 8): a NEW, additive analyzer function
//! surfacing `ggen_engine::frontmatter_rdf::validate_frontmatter_shacl`
//! (a real SHACL check against 4 of `Frontmatter`'s 25 fields -- see that
//! module's own doc comment for the exact field list and rationale) as an
//! author-time `ggen-lsp` diagnostic.
//!
//! Does not touch `tera_analyzer.rs` (CP7's file) or any of its existing
//! pure-Rust checks -- `GGEN-TPL-001`/`GGEN-OUT-001`/etc. keep validating
//! the Tera/SPARQL/`ggen.toml` surfaces exactly as before. This module adds
//! a new, independent surface: the frontmatter block's own field values,
//! checked against the CP8 SHACL shape via the real `praxis-graphlaw`
//! engine `ggen graph validate`/`admit_shape_files` already use -- not a
//! reimplementation of that check, a direct call into it.

use ggen_engine::frontmatter_rdf::validate_frontmatter_shacl;
use ggen_engine::template::Frontmatter;
use lsp_max::lsp_types_max::DiagnosticSeverity;
use lsp_max_protocol::{LawAxis, MaxDiagnostic};

use crate::analyzers::diag;

/// Diagnostic code for a frontmatter block that fails the CP8 SHACL shape
/// (`ggenspec:FrontmatterInstanceShape` in `schema/frontmatter-schema.ttl`).
/// Distinct from the `GGEN-*` family (author-time Tera/SPARQL-surface
/// codes, `tera_analyzer.rs`) and the `E00XX` family (SPARQL-analyzer
/// codes) -- this check runs a real external engine call
/// (`GraphEngine::validate_shacl`), not a local pure-Rust rule, so it is
/// deliberately its own namespace.
pub const GGEN_FM_SHACL_001: &str = "GGEN-FM-SHACL-001";

/// Parse `content` as a template's YAML frontmatter block (the same
/// deserialization production code uses, `crate::template::Frontmatter`'s
/// real `Deserialize` impl -- not a hand-rolled parser) and, if it parses,
/// run the CP8 SHACL check against it via
/// [`validate_frontmatter_shacl`]. A block that fails to parse as YAML at
/// all is not this analyzer's concern (the frontmatter/YAML surface has its
/// own existing parse-error path in `crate::template`) and yields no
/// diagnostics here.
#[must_use]
pub fn frontmatter_shacl_diagnostics(frontmatter_yaml: &str) -> Vec<MaxDiagnostic> {
    let Ok(fm) = serde_yaml::from_str::<Frontmatter>(frontmatter_yaml) else {
        return Vec::new();
    };
    match validate_frontmatter_shacl(&fm) {
        Ok(()) => Vec::new(),
        Err(e) => vec![diag::max_whole_line(
            0,
            DiagnosticSeverity::ERROR,
            Some(GGEN_FM_SHACL_001),
            format!("{GGEN_FM_SHACL_001} frontmatter_shacl_violation: {e}"),
            LawAxis::Domain,
        )],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn conforming_frontmatter_yields_no_diagnostics() {
        let yaml = "to: \"out.rs\"\nat_line: 3\n";
        let diags = frontmatter_shacl_diagnostics(yaml);
        assert!(diags.is_empty(), "expected no diagnostics, got: {diags:?}");
    }

    #[test]
    fn at_line_zero_emits_ggen_fm_shacl_001() {
        let yaml = "to: \"out.rs\"\nat_line: 0\n";
        let diags = frontmatter_shacl_diagnostics(yaml);
        assert!(!diags.is_empty(), "expected a GGEN-FM-SHACL-001 diagnostic");
        assert!(
            diags[0].lsp.message.contains(GGEN_FM_SHACL_001),
            "message must contain {GGEN_FM_SHACL_001}: {}",
            diags[0].lsp.message
        );
    }

    #[test]
    fn empty_sh_after_emits_ggen_fm_shacl_001() {
        let yaml = "to: \"out.rs\"\nsh_after: \"\"\n";
        let diags = frontmatter_shacl_diagnostics(yaml);
        assert!(!diags.is_empty());
    }

    #[test]
    fn unparseable_yaml_yields_no_diagnostics_from_this_analyzer() {
        // Not this analyzer's job -- `to` deny_unknown_fields rejects an
        // unrecognized key, but that is a parse failure this analyzer
        // silently defers on, not a SHACL violation it reports.
        let yaml = "to: \"out.rs\"\nnot_a_real_field: 1\n";
        let diags = frontmatter_shacl_diagnostics(yaml);
        assert!(diags.is_empty());
    }
}
