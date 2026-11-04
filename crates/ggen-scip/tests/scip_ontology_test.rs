//! R80-04 (v26.11.3) — SCIP ontology parity court.
//!
//! Text-level court (precedent: ggen-abb-sbb's refusal_crosswalk_test.rs,
//! ggen-engine's frontmatter canonical schema courts): asserts set equality
//! between the struct fields of crates/ggen-scip/src/lib.rs and the
//! `sh:path` properties of ontologies/shapes/scip-index.shacl.ttl, per
//! struct/shape, in BOTH directions, plus anti-vacuity (deleting a
//! property from a scratch copy of the shapes file flips the court red).
//!
//! Chicago discipline: real files, real parsing, real serde round-trip;
//! no mocks.

use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;

const SHAPES_IRI: &str = "https://ggen.dev/ontology/shapes/scip-index";

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .ancestors()
        .nth(2)
        .expect("manifest two levels under repo root")
        .to_path_buf()
}

fn lib_rs() -> String {
    fs::read_to_string(repo_root().join("crates/ggen-scip/src/lib.rs"))
        .expect("read crates/ggen-scip/src/lib.rs")
}

fn shacl_ttl() -> String {
    fs::read_to_string(repo_root().join("ontologies/shapes/scip-index.shacl.ttl"))
        .expect("read ontologies/shapes/scip-index.shacl.ttl")
}

fn scip_ttl() -> String {
    fs::read_to_string(repo_root().join("ontologies/scip.ttl")).expect("read ontologies/scip.ttl")
}

/// Named-field structs in lib.rs: struct name -> sorted field names.
/// Tuple structs are counted separately (returned as the second element)
/// so the court's skip of them is loud, never silent.
fn lib_structs(text: &str) -> (BTreeMap<String, Vec<String>>, Vec<String>) {
    let mut named = BTreeMap::new();
    let mut tuple = Vec::new();
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let line = lines[i].trim();
        if let Some(rest) = line.strip_prefix("pub struct ") {
            let name = rest
                .split(|c: char| c == '{' || c == '(')
                .next()
                .unwrap_or("")
                .trim()
                .to_string();
            if rest.contains('{') {
                // Wire name = serde name: the field's Rust name unless a
                // `#[serde(rename = "...")]` attribute overrides it (read
                // from the real attribute lines, not guessed).
                let mut pending_rename: Option<String> = None;
                let mut fields = Vec::new();
                i += 1;
                while i < lines.len() && !lines[i].trim().starts_with('}') {
                    let f = lines[i].trim();
                    if let Some(r) = f.strip_prefix("#[serde(rename = \"") {
                        if let Some(wire) = r.split('"').next() {
                            pending_rename = Some(wire.to_string());
                        }
                    } else if let Some(f) = f.strip_prefix("pub ") {
                        if let Some(rust_name) = f.split(':').next() {
                            let wire = pending_rename
                                .take()
                                .unwrap_or_else(|| rust_name.trim().to_string());
                            fields.push(wire);
                        }
                    } else if !f.starts_with('#') && !f.starts_with('/') && !f.starts_with("///") {
                        // a non-attribute, non-field line resets the pending rename
                        pending_rename = None;
                    }
                    i += 1;
                }
                fields.sort();
                named.insert(name, fields);
            } else {
                tuple.push(name);
            }
        }
        i += 1;
    }
    (named, tuple)
}

/// Shapes in the TTL: shape local name -> (target class local name,
/// sorted scip: property local names in wire form). Paths
/// outside the scip: namespace (e.g. rdf:value on the Dotted newtype
/// shape) are excluded and reported in the third element so the
/// exclusion is observable.
fn shacl_shapes(
    text: &str,
) -> (
    BTreeMap<String, (String, Vec<String>)>,
    BTreeMap<String, Vec<String>>,
) {
    let mut shapes = BTreeMap::new();
    let mut foreign = BTreeMap::new();
    let lines: Vec<&str> = text.lines().collect();
    let mut i = 0;
    while i < lines.len() {
        let line = lines[i].trim();
        if let Some(rest) = line.strip_prefix("shapes:") {
            if let Some(shape_name) = rest.strip_suffix(" a sh:NodeShape ;") {
                let mut target = String::new();
                let mut props = Vec::new();
                let mut foreign_paths = Vec::new();
                i += 1;
                while i < lines.len() && !lines[i].trim().starts_with("shapes:") {
                    let l = lines[i].trim();
                    if let Some(t) = l.strip_prefix("sh:targetClass scip:") {
                        target = t.trim_end_matches(" ;").to_string();
                    }
                    if let Some(p) = l.strip_prefix("sh:path scip:") {
                        props.push(p.trim_end_matches(" ;").trim().to_string());
                    } else if let Some(p) = l.strip_prefix("sh:path ") {
                        foreign_paths.push(p.trim_end_matches(" ;").trim().to_string());
                    }
                    i += 1;
                }
                props.sort();
                foreign_paths.sort();
                foreign.insert(shape_name.to_string(), foreign_paths);
                shapes.insert(shape_name.to_string(), (target, props));
                continue;
            }
        }
        i += 1;
    }
    (shapes, foreign)
}

/// The parity predicate, factored so the anti-vacuity arm can run it
/// against a mutated scratch copy.
fn parity(
    lib: &str, shacl: &str,
) -> Result<
    (
        BTreeMap<String, Vec<String>>,
        BTreeMap<String, (String, Vec<String>)>,
    ),
    String,
> {
    // SLA minimal-unblock (r80-03 lane): return type aligned with
    // shacl_shapes' (target, props) tuple map; owning lane supersedes.
    let (structs, tuple) = lib_structs(lib);
    let (shapes, _foreign) = shacl_shapes(shacl);

    if tuple.len() != 1 || tuple[0] != "Dotted" {
        return Err(format!(
            "expected exactly one tuple struct `Dotted` (skipped loudly), got {:?}",
            tuple
        ));
    }
    // DottedShape covers the tuple newtype via rdf:value; the named-field
    // parity below skips it by the same loud skip.
    let shapes: BTreeMap<_, _> = shapes
        .into_iter()
        .filter(|(k, _)| k != "DottedShape")
        .collect();
    if shapes.contains_key("DottedShape") {
        return Err("DottedShape exclusion failed".to_string());
    }
    if structs.len() != shapes.len() {
        return Err(format!(
            "struct count {} != shape count {} (structs: {:?}; shapes: {:?})",
            structs.len(),
            shapes.len(),
            structs.keys().collect::<Vec<_>>(),
            shapes.keys().collect::<Vec<_>>()
        ));
    }
    for (sname, fields) in &structs {
        let shape_name = format!("{}Shape", sname);
        let (target, props) = shapes
            .get(&shape_name)
            .ok_or_else(|| format!("no shape {} for struct {}", shape_name, sname))?;
        if target != sname {
            return Err(format!(
                "shape {} targets scip:{}, expected scip:{}",
                shape_name, target, sname
            ));
        }
        if fields != props {
            return Err(format!(
                "field/property mismatch on {}: struct {:?} vs shape {:?}",
                sname, fields, props
            ));
        }
    }
    for sname in shapes.keys() {
        let struct_name = sname.trim_end_matches("Shape");
        if !structs.contains_key(struct_name) {
            return Err(format!("shape {} has no struct {}", sname, struct_name));
        }
    }
    Ok((structs, shapes))
}

/// WO contract: deleting a property from a scratch copy flips red.
#[test]
fn scip_ontology_anti_vacuity_delete_prop_flips_red() {
    let shacl = shacl_ttl();
    let killed = shacl
        .lines()
        .filter(|l| !l.trim().starts_with("sh:path scip:kind"))
        .collect::<Vec<_>>()
        .join("\n");
    assert_ne!(killed, shacl, "mutation must actually change the file");
    let err = parity(&lib_rs(), &killed)
        .expect_err("court must flip red when a shape property is deleted");
    assert!(
        err.contains("DefInfo"),
        "expected the DefInfo mismatch to surface, got: {}",
        err
    );
}

/// Contract item 2: schema-mirror parity, both directions, on the real
/// files.
#[test]
fn scip_ontology_parity_both_ways() {
    let (structs, shapes) =
        parity(&lib_rs(), &shacl_ttl()).expect("parity court failed on the real files");
    assert!(structs.contains_key("Occurrence"));
    assert!(shapes.contains_key("OccurrenceShape"));
}

/// Every shape is closed.
#[test]
fn scip_ontology_shapes_closed() {
    let shacl = shacl_ttl();
    let count = shacl.matches("a sh:NodeShape").count();
    let closed = shacl.matches("sh:closed true").count();
    assert!(count >= 7, "expected >= 7 shapes, found {}", count);
    assert_eq!(count, closed, "every shape must declare sh:closed true");
}

/// Contract item 2 constraints: ggen scheme pattern, wire-symbol pattern,
/// roles bitmask bounds, kind enumeration.
#[test]
fn scip_ontology_constraints_present() {
    let shacl = shacl_ttl();
    for needle in [
        r#""^ggen$""#,
        r#"^ggen [A-Za-z0-9_-]+ [A-Za-z0-9_.]+$"#,
        "sh:maxInclusive 3",
        "sh:minInclusive 0",
        r#"sh:in ( "fn" "struct" "enum" "const" )"#,
    ] {
        assert!(shacl.contains(needle), "shapes file missing: {}", needle);
    }
}

/// Vocabulary: every sh:path property in the shapes is declared in
/// scip.ttl with an rdfs:comment, and the classes exist.
#[test]
fn scip_ontology_vocabulary_declares_terms() {
    let vocab = scip_ttl();
    let (structs, _shapes) =
        parity(&lib_rs(), &shacl_ttl()).expect("parity court failed on the real files");
    let mut all_fields = Vec::new();
    for fields in structs.values() {
        all_fields.extend(fields.iter().cloned());
    }
    all_fields.sort();
    all_fields.dedup();
    for field in &all_fields {
        let decl = format!("scip:{} a rdf:Property", field);
        assert!(
            vocab.contains(&decl),
            "scip.ttl does not declare property scip:{}",
            field
        );
    }
    for class in [
        "scip:Index a rdfs:Class",
        "scip:Symbol a rdfs:Class",
        "scip:Occurrence a rdfs:Class",
        "scip:Document a rdfs:Class",
        "scip:Metadata a rdfs:Class",
        "scip:DefInfo a rdfs:Class",
    ] {
        assert!(vocab.contains(class), "scip.ttl missing class: {}", class);
    }
    // rdfs:comment per declared term (block-scoped: comment within the
    // term's statement block).
    let mut uncommented = Vec::new();
    for block in vocab.split("\nscip:") {
        if block.contains(" a rdf:Property") || block.contains(" a rdfs:Class") {
            if !block.contains("rdfs:comment") {
                uncommented.push(block.lines().next().unwrap_or("").to_string());
            }
        }
    }
    assert!(
        uncommented.is_empty(),
        "terms without rdfs:comment: {:?}",
        uncommented
    );
}

/// Contract item 4: the index emits scip:conformsTo <shapes-iri> in its
/// output — exercised through the real serializer, not a stub.
#[test]
fn scip_ontology_index_emits_conforms_to() {
    let index = ggen_scip::Index {
        metadata: ggen_scip::Metadata {
            project_root: "/tmp/repo".to_string(),
            emitter: ggen_scip::emit::EMITTER.to_string(),
            conforms_to: ggen_scip::default_conforms_to(),
        },
        documents: vec![ggen_scip::Document {
            path: "crates/x/src/lib.rs".to_string(),
            occurrences: vec![ggen_scip::Occurrence {
                symbol: "ggen x lib".to_string(),
                range: [0, 0, 7],
                roles: ggen_scip::ROLE_DEFINITION,
                def_info: Some(ggen_scip::DefInfo {
                    name: "lib".to_string(),
                    kind: "fn".to_string(),
                }),
            }],
        }],
    };
    let json = ggen_scip::canonical_json(&index).expect("canonical json");
    assert!(
        json.contains(r#""conformsTo":"https://ggen.dev/ontology/shapes/scip-index""#),
        "emitted index missing conformsTo pin: {}",
        json
    );
    // Self-describing default: an index deserialized from JSON without a
    // conformsTo key still carries the shapes IRI (serde default).
    let without: ggen_scip::Index = serde_json::from_str(&json.replace(
        r#""conformsTo":"https://ggen.dev/ontology/shapes/scip-index","#,
        "",
    ))
    .expect("deserialize without conformsTo");
    assert_eq!(without.metadata.conforms_to, SHAPES_IRI);
    // Wire symbol round-trips through the real parser.
    let sym = ggen_scip::parse_symbol("ggen x lib").expect("parse");
    assert_eq!(sym.to_wire(), "ggen x lib");
}

/// sha256 digest contract note: no struct field carries a digest yet, so
/// the shapes file must NOT claim one; scip.ttl reserves scip:contentDigest
/// with the sha256 hex pattern documented for the admission step.
#[test]
fn scip_ontology_digest_reserved() {
    assert!(
        !shacl_ttl().contains("scip:contentDigest"),
        "shapes must not constrain a property no struct field carries"
    );
    let vocab = scip_ttl();
    assert!(
        vocab.contains("scip:contentDigest") && vocab.contains("sha256"),
        "scip.ttl must reserve contentDigest with the sha256 hex pattern note"
    );
}
