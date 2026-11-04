//! EA graph schema parity court (WO-04 deliverable 2).
//!
//! Proves the canonical declarative schema `ontologies/shapes/ea-graph.shacl.ttl`
//! (ggen.ea.graph.v1) is a FAITHFUL mirror of the pinned serde structs in
//! `crates/ggen-abb-sbb/src/lib.rs` — not a shape file someone once typed next
//! to them. Precedent: `system_crate_map_parity_test.rs` /
//! `frontmatter_schema_match.rs` (text-level extraction, no extra crate deps).
//!
//! Extraction method (disclosed choice): **text-level field extraction**, not
//! `syn`. The pinned structs use a uniform `pub <field>: <Type>,` line form
//! with no attributes between fields, so line scanning is exact for this
//! surface; `syn` would add a proc-macro-weight dependency to `ggen-config`'s
//! dev-deps for zero additional extraction power here. The structs themselves
//! are NEVER edited by this lane — they are the pinned target; the court only
//! reads them.
//!
//! Anti-vacuity: a fixture string containing a mutated copy of the shapes
//! (one property deleted) MUST fail the same court function that passes on
//! the real file — a court that cannot flip is FAIL.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeSet;
use std::path::PathBuf;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

fn lib_rs_path() -> PathBuf {
    repo_root().join("crates/ggen-abb-sbb/src/lib.rs")
}

fn shapes_ttl_path() -> PathBuf {
    repo_root().join("ontologies/shapes/ea-graph.shacl.ttl")
}

/// Struct name -> `sh:targetClass` IRI local name in the shapes file.
const STRUCT_TO_CLASS: &[(&str, &str)] = &[
    ("Strategy", "Strategy"),
    ("Capability", "Capability"),
    ("Abb", "Abb"),
    ("ArchitectureContract", "ArchitectureContract"),
    ("ArtifactSpec", "ArtifactSpec"),
    ("CandidateSbb", "CandidateSbb"),
    ("Qualification", "Qualification"),
    ("Pack", "Pack"),
];

/// One serde field extracted from a pinned struct.
#[derive(Debug, Clone, PartialEq, Eq)]
struct StructField {
    name: String,
    /// `Option<...>` fields are optional in serde's default derive (missing key
    /// -> `None` only with `#[serde(default)]`; on these structs `digest` is
    /// `Option<String>` WITHOUT `default`, so a missing key is a *deserialize
    /// error*, not a None — but in the RDF projection an absent optional value
    /// is representable, hence `minCount 0`). Everything else: `minCount 1`.
    optional: bool,
    /// `Vec<_>` / `BTreeSet<_>` fields always serialize (serde emits the key
    /// with an empty array), but in the RDF projection an empty collection has
    /// no value triples — so a collection field carries `sh:minCount 0`
    /// regardless of Option-ness.
    collection: bool,
}

/// Extract `pub <name>: <Type>,` fields for `struct_name` from the lib.rs text.
/// Scans from the `pub struct <name> {` line to the closing `}` line. Skips
/// doc comments and attributes; a continuation doc line between fields is fine
/// because only `pub`-prefixed lines are collected.
fn extract_struct_fields(lib_rs: &str, struct_name: &str) -> Vec<StructField> {
    let anchor = format!("pub struct {struct_name} {{");
    let mut lines = lib_rs.lines().peekable();
    let mut fields = Vec::new();
    let mut inside = false;
    while let Some(line) = lines.next() {
        let trimmed = line.trim();
        if !inside {
            if trimmed.contains(&anchor) {
                inside = true;
            }
            continue;
        }
        if trimmed == "}" {
            break;
        }
        if let Some(rest) = trimmed.strip_prefix("pub ") {
            let rest = rest.trim_end_matches(',');
            let (name, ty) = rest.split_once(':').unwrap_or_else(|| {
                panic!("unparseable field line in {struct_name}: {rest:?}")
            });
            let ty = ty.trim();
            let optional = ty.starts_with("Option<") && ty.ends_with('>');
            let collection =
                (ty.starts_with("Vec<") && ty.ends_with('>')) || (ty.starts_with("BTreeSet<") && ty.ends_with('>'));
            fields.push(StructField {
                name: name.trim().to_string(),
                optional,
                collection,
            });
        }
    }
    assert!(
        inside || !fields.is_empty(),
        "struct {struct_name} not found in lib.rs — extraction is broken, not the shapes"
    );
    assert!(
        !fields.is_empty(),
        "struct {struct_name} extracted zero fields — extraction is broken"
    );
    fields
}

/// Extract the `sh:path geag:<name>` set declared under the shape whose
/// `sh:targetClass geag:<class>` matches, from the shapes TTL text. Line- and
/// block-oriented: walks shape blocks (from `geag:<Class>Shape` header line to
/// the blank line at depth 0) collecting `sh:path geag:<x>` occurrences.
fn extract_shape_paths(ttl: &str, class: &str) -> BTreeSet<String> {
    let target = format!("sh:targetClass geag:{class} ;");
    let mut paths = BTreeSet::new();
    let mut in_shape = false;
    let mut found_target = false;
    for line in ttl.lines() {
        let trimmed = line.trim();
        // A new shape block starts with `geag:<Something>Shape`.
        if trimmed.starts_with("geag:") && trimmed.ends_with("Shape") && !trimmed.contains("sh:")
        {
            if in_shape && found_target && !paths.is_empty() {
                break; // next shape reached
            }
            in_shape = false;
            found_target = false;
            continue;
        }
        if trimmed == &target {
            in_shape = true;
            found_target = true;
            continue;
        }
        if in_shape {
            if let Some(rest) = trimmed.strip_prefix("sh:path geag:") {
                let name = rest.trim().trim_end_matches(';').trim();
                paths.insert(name.to_string());
            }
        }
    }
    paths
}

/// Check `sh:closed true` is present in the shape targeting `class`.
fn shape_is_closed(ttl: &str, class: &str) -> bool {
    let target = format!("sh:targetClass geag:{class} ;");
    let mut in_shape = false;
    for line in ttl.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("geag:") && trimmed.ends_with("Shape") && !trimmed.contains("sh:") {
            in_shape = false;
            continue;
        }
        if trimmed == &target {
            in_shape = true;
            continue;
        }
        if in_shape && trimmed == "sh:closed true ;" {
            return true;
        }
    }
    false
}

/// The court: for every pinned struct, the serde field set and the shape's
/// `sh:path` set must be set-equal in BOTH directions. Returns divergence
/// strings; empty = parity holds.
fn parity_errors(lib_rs: &str, ttl: &str) -> Vec<String> {
    let mut errors = Vec::new();
    for (struct_name, class) in STRUCT_TO_CLASS {
        let fields: BTreeSet<String> = extract_struct_fields(lib_rs, struct_name)
            .into_iter()
            .map(|f| f.name)
            .collect();
        let paths = extract_shape_paths(ttl, class);

        for field in &fields {
            if !paths.contains(field) {
                errors.push(format!(
                    "{struct_name}: serde field `{field}` has NO matching sh:path geag:{field} in the {class} shape"
                ));
            }
        }
        for path in &paths {
            if !fields.contains(path) {
                errors.push(format!(
                    "{struct_name}: shape declares sh:path geag:{path} but the pinned struct has NO such serde field"
                ));
            }
        }
        if paths.is_empty() {
            errors.push(format!("{struct_name}: no sh:path entries found for class {class} — shape missing or targetClass mismatch"));
        }
        if !shape_is_closed(ttl, class) {
            errors.push(format!("{struct_name}: shape for {class} is not `sh:closed true`"));
        }
    }
    errors
}

/// The court must also pin optionality: every `Option<...>` struct field maps
/// to a `sh:minCount 0` property; every non-Option field to `sh:minCount 1`.
fn min_count_errors(lib_rs: &str, ttl: &str) -> Vec<String> {
    let mut errors = Vec::new();
    for (struct_name, class) in STRUCT_TO_CLASS {
        let fields = extract_struct_fields(lib_rs, struct_name);
        for field in &fields {
            let prop_line = format!("sh:path geag:{} ;", field.name);
            let target = format!("sh:targetClass geag:{class} ;");
            let expected = if field.optional || field.collection {
                "sh:minCount 0 ;"
            } else {
                "sh:minCount 1 ;"
            };
            let mut in_shape = false;
            let mut in_prop = false;
            let mut found = false;
            for line in ttl.lines() {
                let trimmed = line.trim();
                if trimmed.starts_with("geag:") && trimmed.ends_with("Shape")
                    && !trimmed.contains("sh:")
                {
                    in_shape = false;
                    in_prop = false;
                    continue;
                }
                if trimmed == &target {
                    in_shape = true;
                    continue;
                }
                if !in_shape {
                    continue;
                }
                if trimmed == "sh:property [" {
                    in_prop = true;
                    continue;
                }
                if in_prop && trimmed.starts_with(']') {
                    in_prop = false;
                    continue;
                }
                if in_prop && trimmed == &prop_line {
                    // minCount is declared within the same property block;
                    // the following sh:minCount line seen while `found` is
                    // set belongs to this block.
                    found = true;
                }
                if in_prop && found && trimmed.starts_with("sh:minCount") {
                    if trimmed != expected {
                        errors.push(format!(
                            "{struct_name}.{}: expected `{}` for an Option={} collection={} field, found `{}`",
                            field.name, expected, field.optional, field.collection, trimmed
                        ));
                    }
                    break;
                }
            }
            if !found {
                errors.push(format!(
                    "{struct_name}.{}: property block with sh:path geag:{} has no sh:minCount — required/optional not pinned",
                    field.name, field.name
                ));
            }
        }
    }
    errors
}

// ---------------------------------------------------------------------------
// Courts
// ---------------------------------------------------------------------------

/// PRIMARY COURT: the real shapes file is in exact two-way parity with the
/// pinned structs, closed, and minCount-pinned. Currently-passing receipt.
#[test]
fn ea_graph_shapes_mirror_pinned_serde_structs_exactly() {
    let lib_rs = std::fs::read_to_string(lib_rs_path())
        .unwrap_or_else(|e| panic!("failed to read pinned lib.rs: {e}"));
    let ttl = std::fs::read_to_string(shapes_ttl_path())
        .unwrap_or_else(|e| panic!("failed to read shapes file: {e}"));

    let errors = parity_errors(&lib_rs, &ttl);
    assert!(
        errors.is_empty(),
        "ea-graph SHACL shapes drifted from pinned ggen-abb-sbb structs:\n  - {}",
        errors.join("\n  - ")
    );

    let mc_errors = min_count_errors(&lib_rs, &ttl);
    assert!(
        mc_errors.is_empty(),
        "ea-graph SHACL minCount pinning drifted from pinned struct optionality:\n  - {}",
        mc_errors.join("\n  - ")
    );
}

/// Referential-constraint presence court: the SPARQL referential checks and
/// the PackConflation disjointness constraint named in WO-04 must exist in the
/// shapes file (presence of the named constraint shapes + their key FILTER
/// machinery), so a delete of the referential layer cannot pass silently.
#[test]
fn ea_graph_shapes_carry_referential_and_conflation_constraints() {
    let ttl = std::fs::read_to_string(shapes_ttl_path())
        .unwrap_or_else(|e| panic!("failed to read shapes file: {e}"));

    for constraint in [
        "geag:abb-capability-resolves",
        "geag:abb-contract-bidirectional",
        "geag:sbb-abb-resolves",
        "geag:sbb-pack-resolves",
        "geag:qualification-references-resolve",
        "geag:PackConflationDisjoint",
    ] {
        assert!(
            ttl.contains(constraint),
            "shapes file is missing required constraint `{constraint}`"
        );
    }
    // The bidirectional abb<->contract check must consult BOTH directions.
    for fragment in ["geag:abb ?abbId", "$this geag:id ?abbId"] {
        assert!(
            ttl.contains(fragment),
            "abb-contract bidirectional SPARQL lost fragment `{fragment}`"
        );
    }
}

/// ANTI-VACUITY COURT: a fixture copy of the shapes with ONE property deleted
/// MUST fail the same court function that passes on the real file. The real
/// shapes file is never edited — the mutation lives in this test string.
#[test]
fn deleting_one_shape_property_from_a_fixture_fails_the_court() {
    let lib_rs = std::fs::read_to_string(lib_rs_path())
        .unwrap_or_else(|e| panic!("failed to read pinned lib.rs: {e}"));

    // Real shapes as fixture base.
    let real = std::fs::read_to_string(shapes_ttl_path())
        .unwrap_or_else(|e| panic!("failed to read shapes file: {e}"));
    assert!(
        parity_errors(&lib_rs, &real).is_empty(),
        "fixture base (the real shapes file) must pass the court before mutation"
    );

    // Mutation 1: delete the `title` property from the Strategy shape.
    let mut mutated = String::new();
    let mut in_strategy_title_block = false;
    for line in real.lines() {
        let trimmed = line.trim();
        if trimmed == "sh:path geag:title ;" {
            in_strategy_title_block = true;
            continue; // drop the sh:path line
        }
        if in_strategy_title_block {
            if trimmed.starts_with(']') {
                in_strategy_title_block = false;
                mutated.push_str("    ] ;\n");
                continue;
            }
            continue; // drop the rest of the deleted property block
        }
        mutated.push_str(line);
        mutated.push('\n');
    }
    assert_ne!(mutated, real, "mutation 1 must actually change the text");
    let errors = parity_errors(&lib_rs, &mutated);
    assert!(
        errors.iter().any(|e| e.contains("Strategy")
            && e.contains("title")
            && e.contains("NO matching sh:path")),
        "court accepted a Strategy shape with the `title` property deleted (VACUOUS GATE); got: {errors:?}"
    );

    // Mutation 2: add a phantom property the struct does not have — inserted
    // AFTER `sh:targetClass geag:Pack ;` so the court's extraction window
    // (which opens at the targetClass line) actually sees it.
    let mut phantom = real.clone();
    phantom = phantom.replace(
        "sh:targetClass geag:Pack ;",
        "sh:targetClass geag:Pack ;\n    sh:property [\n        sh:path geag:phantom_field ;\n        sh:minCount 1 ;\n        sh:datatype xsd:string\n    ] ;",
    );
    assert_ne!(phantom, real, "mutation 2 must actually change the text");
    let errors = parity_errors(&lib_rs, &phantom);
    assert!(
        errors.iter().any(|e| e.contains("Pack")
            && e.contains("phantom_field")
            && e.contains("NO such serde field")),
        "court accepted a Pack shape declaring a field the struct does not have (VACUOUS GATE); got: {errors:?}"
    );

    // Mutation 3: un-close a shape (deny_unknown_fields equivalent lost).
    let unclosed = real.replace("sh:closed true ;", "sh:closed false ;");
    assert_ne!(unclosed, real, "mutation 3 must actually change the text");
    let errors = parity_errors(&lib_rs, &unclosed);
    assert!(
        !errors.is_empty() && errors.iter().all(|e| e.contains("not `sh:closed true`")),
        "court accepted un-closed shapes (VACUOUS GATE); got: {errors:?}"
    );
}

/// Quietly guards the extraction lens itself: the pinned lib.rs must still
/// contain every struct the court claims to check. If a struct is renamed or
/// moved, this fails with a precise name instead of a silently-shorter parity
/// sweep.
#[test]
fn extraction_lens_covers_all_pinned_structs() {
    let lib_rs = std::fs::read_to_string(lib_rs_path())
        .unwrap_or_else(|e| panic!("failed to read pinned lib.rs: {e}"));
    for (struct_name, _) in STRUCT_TO_CLASS {
        let anchor = format!("pub struct {struct_name} {{");
        assert!(
            lib_rs.contains(&anchor),
            "pinned struct `{struct_name}` no longer present in ggen-abb-sbb/src/lib.rs — \
             update STRUCT_TO_CLASS; the parity sweep is silently narrower without it"
        );
    }
}
