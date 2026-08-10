//! CP8 (Gall's-Law checkpoint 8): project a real, parsed
//! [`crate::template::Frontmatter`] into RDF facts under the `fm:` namespace
//! declared in `schema/frontmatter-schema.ttl`, so it can be SHACL-validated
//! via [`crate::graph::GraphEngine::validate_shacl`] the same way rendered
//! template output already is (`crate::sync::admit_shape_files`).
//!
//! Nothing in this crate did this before CP8: [`crate::template::Frontmatter`]
//! deserializes straight from a template file's YAML block into the Rust
//! struct and is never round-tripped through the graph. This module is a
//! *new* code path, deliberately narrower than the full 25-field struct --
//! it projects only the 7 fields that carry a real SHACL shape
//! (`ggenspec:FrontmatterInstanceShape` in `schema/frontmatter-schema.ttl`):
//!
//!   - `to`          (always present -- required field)
//!   - `at_line`     (`Option<usize>`, projected only when `Some`)
//!   - `sh_before`   (`Option<String>`, projected only when `Some`)
//!   - `sh_after`    (`Option<String>`, projected only when `Some`)
//!   - `when`        (`Option<String>`, projected only when `Some`)
//!   - `from`        (`Option<String>`, projected only when `Some`)
//!   - `base`        (`Option<String>`, projected only when `Some`)
//!
//! The last three were added in a later pass, chosen by the same criterion
//! CP8's original 4 used: a value constraint the Rust type system does not
//! already enforce (non-emptiness on an `Option<String>`), not enum/type
//! membership Rust already guarantees. Two other candidates considered and
//! rejected for that reason: `inject` is a plain `bool` with no value range
//! to constrain beyond its type; `freeze_policy` is a closed Rust enum
//! (`FreezePolicy`) that `serde` already refuses to parse if malformed, so a
//! SHACL `sh:in` membership check on it would be checking something the
//! type system already guarantees -- a vacuous gate, not a real one.
//!
//! Deliberately unprojected in this slice (same list as the schema TTL's CP8
//! comment block, kept in sync by hand -- no drift gate for this list exists
//! yet, unlike the field-*name* drift gate in
//! `tests/frontmatter_schema_match.rs`, which is untouched by CP8): `sparql`,
//! `for_each`, `construct`, `inject`, `before`, `after`, `skip_if`,
//! `unless_exists`, `force`, `skip_empty`, `backup`, `shape`,
//! `determinism`, `freeze_policy`, `freeze_slots_dir`, `rdf`, `rdf_inline`,
//! `prefixes`.

use crate::error::AppError;
use crate::graph::{GraphEngine as _, GraphLawStore};
use crate::template::Frontmatter;

/// The CP8 slice of `schema/frontmatter-schema.ttl` (the `sh:`/`fm:` triples
/// appended by CP8, plus the `@prefix` block they need -- NOT the full file,
/// since the full file's `ggenspec:Frontmatter`/`ggenspec:hasField`
/// declarations are meta-schema, not instance shapes, and are irrelevant to
/// (harmless but unnecessary alongside) this validation).
const FRONTMATTER_SCHEMA_TTL: &str = include_str!("../schema/frontmatter-schema.ttl");

/// The `fm:` namespace `schema/frontmatter-schema.ttl`'s CP8 shapes target.
pub const FM_NS: &str = "https://praxis.dev/ggen/frontmatter#";

/// Turtle-escape a literal string body (backslash and double-quote only --
/// the two characters that are unsafe inside a `"..."` short literal).
fn escape_turtle_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

/// Project one parsed [`Frontmatter`] into a standalone Turtle document
/// containing exactly one `fm:Frontmatter` instance (a fresh blank node)
/// carrying the 7 CP8-projected fields. Always produces at least the
/// required `fm:to` triple; every other field's triple is emitted only when
/// the corresponding field is `Some`.
///
/// # Errors
/// Never fails today (string/typed-value emission cannot fail) -- returns
/// `Result` for forward compatibility with a future graph-backed variant
/// that inserts directly rather than emitting Turtle text, matching the
/// `Result`-returning shape of every other admission function in this
/// crate.
pub fn project_frontmatter(fm: &Frontmatter) -> crate::error::Result<String> {
    use std::fmt::Write as _;

    let mut ttl = String::new();
    // `write!`/`writeln!` into a `String` cannot fail (the only error type,
    // `fmt::Error`, comes from a `Write` impl that reports failure -- `String`'s
    // never does), so `let _ =` is deliberate, not a swallowed error.
    let _ = writeln!(ttl, "@prefix fm: <{FM_NS}> .");
    let _ = writeln!(ttl, "@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .");
    let _ = writeln!(ttl, "_:frontmatter a fm:Frontmatter ;");
    let _ = write!(ttl, "    fm:to \"{}\"", escape_turtle_string(&fm.to));
    if let Some(at_line) = fm.at_line {
        let _ = write!(ttl, " ;\n    fm:atLine {at_line}");
    }
    if let Some(sh_before) = &fm.sh_before {
        let _ = write!(
            ttl,
            " ;\n    fm:shBefore \"{}\"",
            escape_turtle_string(sh_before)
        );
    }
    if let Some(sh_after) = &fm.sh_after {
        let _ = write!(
            ttl,
            " ;\n    fm:shAfter \"{}\"",
            escape_turtle_string(sh_after)
        );
    }
    if let Some(when) = &fm.when {
        let _ = write!(ttl, " ;\n    fm:when \"{}\"", escape_turtle_string(when));
    }
    if let Some(from) = &fm.from {
        let _ = write!(ttl, " ;\n    fm:from \"{}\"", escape_turtle_string(from));
    }
    if let Some(base) = &fm.base {
        let _ = write!(ttl, " ;\n    fm:base \"{}\"", escape_turtle_string(base));
    }
    ttl.push_str(" .\n");
    Ok(ttl)
}

/// Project `fm` and SHACL-validate the result against
/// `ggenspec:FrontmatterInstanceShape` (the CP8 shapes appended to
/// `schema/frontmatter-schema.ttl`), using the exact caller shape
/// `sync::admit_shape_files` already establishes for `shape:`-declared
/// rendered-output validation: build a fresh isolated engine, insert the
/// data, call [`crate::graph::GraphEngine::validate_shacl`], branch on
/// `.conforms`, map violations to a typed [`AppError`].
///
/// Unlike `admit_shape_files` (which validates the shared/overlay project
/// graph against a *user-supplied* shape path list), this validates a
/// *fresh, isolated* graph containing only this one projected frontmatter
/// instance against the *fixed* CP8 shape declared in the schema file --
/// the frontmatter block is not part of the project's own RDF graph, so
/// there is no shared state to isolate it from, and no `shape:` field is
/// involved.
///
/// # Errors
/// - `[FM-TPL-023]` (via [`AppError::fm_tpl`]) when the SHACL engine itself
///   refuses (parse/engine failure) or when the projected frontmatter does
///   not conform, listing every violation.
pub fn validate_frontmatter_shacl(fm: &Frontmatter) -> crate::error::Result<()> {
    let ttl = project_frontmatter(fm)?;
    let engine = GraphLawStore::new()?;
    engine.insert_turtle(&ttl).map_err(|e| {
        AppError::fm_tpl(
            23,
            format!("projected frontmatter facts failed to load into a fresh graph: {e}"),
        )
    })?;
    let outcome = engine.validate_shacl(FRONTMATTER_SCHEMA_TTL).map_err(|e| {
        AppError::fm_tpl(
            23,
            format!(
                "frontmatter SHACL validation could not run: {e}. Remediation: use the \
                 default GraphLaw engine (no `--engine oxigraph`), which is the only \
                 engine with SHACL support."
            ),
        )
    })?;
    if !outcome.conforms {
        return Err(AppError::fm_tpl(
            23,
            format!(
                "frontmatter block failed SHACL validation, {} violation(s): {}. \
                 Remediation: fix the listed field(s) in the template's frontmatter block.",
                outcome.violations.len(),
                outcome.violations.join("; ")
            ),
        ));
    }
    Ok(())
}

#[cfg(test)]
#[allow(clippy::expect_used, clippy::expect_fun_call)]
mod tests {
    use super::*;

    fn minimal_frontmatter(to: &str) -> Frontmatter {
        // Frontmatter has no public constructor (real fields only, parsed
        // from YAML) -- round-trip through the real YAML parser so this
        // test exercises the same construction path production code does.
        let yaml = format!("to: \"{to}\"\n");
        serde_yaml::from_str(&yaml).expect("minimal frontmatter parses")
    }

    #[test]
    fn projects_required_to_field() {
        let fm = minimal_frontmatter("src/{{ name }}.rs");
        let ttl = project_frontmatter(&fm).expect("projection");
        assert!(ttl.contains("fm:Frontmatter"));
        assert!(ttl.contains("fm:to \"src/{{ name }}.rs\""));
        assert!(!ttl.contains("fm:atLine"));
        assert!(!ttl.contains("fm:shBefore"));
        assert!(!ttl.contains("fm:shAfter"));
    }

    #[test]
    fn projects_at_line_and_sh_hooks_when_present() {
        let yaml =
            "to: \"out.rs\"\nat_line: 5\nsh_before: \"echo before\"\nsh_after: \"echo after\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let ttl = project_frontmatter(&fm).expect("projection");
        assert!(ttl.contains("fm:atLine 5"));
        assert!(ttl.contains("fm:shBefore \"echo before\""));
        assert!(ttl.contains("fm:shAfter \"echo after\""));
    }

    #[test]
    fn projects_when_from_and_base_when_present() {
        let yaml = "to: \"out.rs\"\nwhen: \"ASK { ?s a <http://example.org/Dog> }\"\nfrom: \"other.tmpl\"\nbase: \"http://example.org/\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let ttl = project_frontmatter(&fm).expect("projection");
        assert!(ttl.contains("fm:when \"ASK { ?s a <http://example.org/Dog> }\""));
        assert!(ttl.contains("fm:from \"other.tmpl\""));
        assert!(ttl.contains("fm:base \"http://example.org/\""));
    }

    #[test]
    fn escapes_quotes_and_backslashes_in_string_literals() {
        let yaml = "to: \"a\\\"b\\\\c\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let ttl = project_frontmatter(&fm).expect("projection");
        assert!(ttl.contains(r#"fm:to "a\"b\\c""#));
    }

    #[test]
    fn projected_turtle_is_valid_and_loads_into_a_real_graph() {
        use crate::graph::GraphEngine as _;

        let fm = minimal_frontmatter("out.rs");
        let ttl = project_frontmatter(&fm).expect("projection");
        let graph = crate::graph::GraphLawStore::new().expect("graphlaw store");
        let inserted = graph.insert_turtle(&ttl).expect("turtle must parse");
        assert!(inserted > 0, "expected at least one quad inserted");
    }

    // ───────────── real GraphEngine::validate_shacl accept/refuse proof ────

    #[test]
    fn conforming_frontmatter_passes_real_shacl_validation() {
        let yaml =
            "to: \"out.rs\"\nat_line: 5\nsh_before: \"echo before\"\nsh_after: \"echo after\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let result = validate_frontmatter_shacl(&fm);
        assert!(
            result.is_ok(),
            "conforming frontmatter must pass SHACL validation, got: {result:?}"
        );
    }

    #[test]
    fn at_line_zero_is_refused_by_real_shacl_validation() {
        // sh:minInclusive 1 on fm:atLine -- at_line: 0 is a real, checkable
        // violation of the CP8 shape (a 1-based line number cannot be 0).
        let yaml = "to: \"out.rs\"\nat_line: 0\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let result = validate_frontmatter_shacl(&fm);
        let err = result.expect_err("at_line: 0 must be refused by the CP8 shape");
        let msg = err.to_string();
        assert!(
            msg.contains("FM-TPL-023"),
            "expected FM-TPL-023, got: {msg}"
        );
        assert!(
            msg.contains("violation"),
            "expected a violation message, got: {msg}"
        );
    }

    #[test]
    fn empty_sh_before_is_refused_by_real_shacl_validation() {
        // sh:minLength 1 on fm:shBefore -- an empty (but present) sh_before
        // would silently no-op past shell_safety without ever being flagged.
        let yaml = "to: \"out.rs\"\nsh_before: \"\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let result = validate_frontmatter_shacl(&fm);
        let err = result.expect_err("empty sh_before must be refused by the CP8 shape");
        assert!(err.to_string().contains("FM-TPL-023"));
    }

    #[test]
    fn empty_when_is_refused_by_real_shacl_validation() {
        // sh:minLength 1 on fm:when -- an empty (but present) SPARQL ASK
        // guard would silently mean "always generate", the opposite of what
        // an author who wrote `when: ""` almost certainly intended.
        let yaml = "to: \"out.rs\"\nwhen: \"\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let result = validate_frontmatter_shacl(&fm);
        let err = result.expect_err("empty when must be refused by the CP8 shape");
        assert!(err.to_string().contains("FM-TPL-023"));
    }

    #[test]
    fn empty_from_is_refused_by_real_shacl_validation() {
        // sh:minLength 1 on fm:from -- an empty (but present) alternate
        // template-body source path cannot resolve to any real file.
        let yaml = "to: \"out.rs\"\nfrom: \"\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let result = validate_frontmatter_shacl(&fm);
        let err = result.expect_err("empty from must be refused by the CP8 shape");
        assert!(err.to_string().contains("FM-TPL-023"));
    }

    #[test]
    fn empty_base_is_refused_by_real_shacl_validation() {
        // sh:minLength 1 on fm:base -- an empty (but present) RDF base IRI
        // is never a valid @base declaration.
        let yaml = "to: \"out.rs\"\nbase: \"\"\n";
        let fm: Frontmatter = serde_yaml::from_str(yaml).expect("frontmatter parses");
        let result = validate_frontmatter_shacl(&fm);
        let err = result.expect_err("empty base must be refused by the CP8 shape");
        assert!(err.to_string().contains("FM-TPL-023"));
    }

    #[test]
    fn drift_injection_and_revert_proves_the_shape_is_not_vacuous() {
        // Drift-injection-and-revert pattern (established by lsp-max's CP2):
        // deliberately corrupt a value in-memory, show the check now fails,
        // then show the original real value still passes -- proving the
        // gate rejects bad input rather than vacuously accepting everything.
        let good_yaml = "to: \"out.rs\"\nat_line: 3\n";
        let good: Frontmatter = serde_yaml::from_str(good_yaml).expect("parses");
        assert!(
            validate_frontmatter_shacl(&good).is_ok(),
            "the real, uncorrupted value must conform"
        );

        let mut corrupted = good.clone();
        corrupted.at_line = Some(0);
        assert!(
            validate_frontmatter_shacl(&corrupted).is_err(),
            "the deliberately corrupted value (at_line: 0) must be refused"
        );

        // Revert: the original `good` value (untouched by the mutation
        // above, since `corrupted` was a clone) still conforms.
        assert!(
            validate_frontmatter_shacl(&good).is_ok(),
            "the original value must still conform after the corrupted clone was checked"
        );
    }
}
