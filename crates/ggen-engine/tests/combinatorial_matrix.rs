//! Combinatorial-maximalism suite.
//!
//! A. Exhaustive write-decision matrix over `plan_write` (reference model vs
//!    implementation, first-match-wins per `src/write.rs`).
//! B. Property tests: sync idempotence, hash insertion-order invariance,
//!    delta laws (compute→apply, compose(inverse) empty).
//! C. Frontmatter closed-vocabulary fuzz + `when/sparql/skip_empty` cross.
//!
//! All filesystem work happens in `TempDir`s; real oxigraph, zero mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::{collections::BTreeMap, path::Path};

use ggen_engine::{
    graph::{Delta, DeterministicGraph},
    sync::{sync, ReceiptPayload, SyncOptions, SyncReceipt, RECEIPT_REL_PATH},
    template::{Frontmatter, MatchSpec, Template},
    write::{plan_write, WriteOutcome},
};
use proptest::prelude::*;
use tempfile::TempDir;

// ─────────────────────────────────────────────────────────────────────────
// Part A: exhaustive write-decision matrix
// ─────────────────────────────────────────────────────────────────────────

const BODY: &str = "generated line\n";
const DIFFERENT: &str = "something else\n";
const ANCHORED: &str = "// ANCHOR\nother line\n";
/// Substring present in every "present" target-state content above.
const MATCHING_NEEDLE: &str = "e";
const NON_MATCHING_NEEDLE: &str = "ZZZ_NOT_PRESENT";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TargetState {
    Absent,
    PresentIdentical,
    PresentDifferent,
    PresentWithAnchor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SkipIf {
    None,
    Matching,
    NonMatching,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Anchor {
    /// No `before/after/at_line`: inject appends.
    Append,
    Before,
    After,
    AtLine,
    /// `at_line` far beyond EOF: must be FM-WRITE-004.
    AtLineOutOfRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Expected {
    Written,
    Injected,
    SkippedUnlessExists,
    SkippedSkipIf,
    SkippedUnchanged,
    ErrWrite3,
    ErrWrite4,
    ErrWrite5,
}

/// Reference model of the documented decision table (first match wins):
/// 1. path escape (not in matrix)
/// 2. `unless_exists` && exists → Skip
/// 3. `skip_if` substring in existing → Skip
/// 4. inject (absent → 003, bad anchor/line → 004, else Injected)
/// 5. absent → Written; identical → Skip(unchanged); differs && force →
///    Written; differs && !force → 005.
///
/// Note (2026-08-03, TECH-DEBT-003 fix in `write.rs`): content-equality is
/// now checked BEFORE the `force` arm, not after. The original ordering put
/// `force` first, so match-arm shadowing meant ANY `force: true` template
/// always reported `Written` — even when the rendered content was
/// byte-identical to what was already on disk — defeating idempotent
/// second-sync detection. `force` only changes the outcome when content
/// actually differs (permitting the overwrite instead of refusing with
/// FM-WRITE-005); it was never meant to disable the identical-content skip
/// path. See `write.rs::plan_write`'s match arms for the authoritative
/// order and `sync_is_idempotent` below for the decision-level consequence.
fn reference_model(
    force: bool, unless_exists: bool, inject: bool, skip_if: SkipIf, state: TargetState,
    anchor: Anchor,
) -> Expected {
    let exists = state != TargetState::Absent;
    if unless_exists && exists {
        return Expected::SkippedUnlessExists;
    }
    if exists && skip_if == SkipIf::Matching {
        return Expected::SkippedSkipIf;
    }
    if inject {
        if !exists {
            return Expected::ErrWrite3;
        }
        return match anchor {
            Anchor::AtLineOutOfRange => Expected::ErrWrite4,
            // Before/After only used against PresentWithAnchor, whose
            // content contains the marker; Append/AtLine always valid.
            _ => Expected::Injected,
        };
    }
    match state {
        TargetState::Absent => Expected::Written,
        TargetState::PresentIdentical => Expected::SkippedUnchanged,
        _ if force => Expected::Written,
        _ => Expected::ErrWrite5,
    }
}

fn classify(actual: &ggen_engine::error::Result<WriteOutcome>) -> Expected {
    match actual {
        Ok(WriteOutcome::Written) => Expected::Written,
        Ok(WriteOutcome::Injected) => Expected::Injected,
        Ok(WriteOutcome::Skipped(reason)) => {
            if reason.starts_with("unless_exists") {
                Expected::SkippedUnlessExists
            } else if reason.starts_with("skip_if") {
                Expected::SkippedSkipIf
            } else if reason.starts_with("unchanged") {
                Expected::SkippedUnchanged
            } else {
                panic!("unclassifiable skip reason: {reason}");
            }
        }
        Err(e) => {
            let msg = e.to_string();
            if msg.contains("FM-WRITE-003") {
                Expected::ErrWrite3
            } else if msg.contains("FM-WRITE-004") {
                Expected::ErrWrite4
            } else if msg.contains("FM-WRITE-005") {
                Expected::ErrWrite5
            } else {
                panic!("unclassifiable error: {msg}");
            }
        }
    }
}

fn frontmatter_for(
    force: bool, unless_exists: bool, inject: bool, skip_if: SkipIf, anchor: Anchor,
) -> Frontmatter {
    Frontmatter {
        to: "out.txt".to_string(),
        sparql: BTreeMap::new(),
        for_each: None,
        construct: None,
        inject,
        before: (inject && anchor == Anchor::Before)
            .then(|| MatchSpec::Literal("// ANCHOR".to_string())),
        after: (inject && anchor == Anchor::After)
            .then(|| MatchSpec::Literal("// ANCHOR".to_string())),
        at_line: if inject && anchor == Anchor::AtLine {
            Some(1)
        } else if inject && anchor == Anchor::AtLineOutOfRange {
            Some(9999)
        } else {
            None
        },
        skip_if: match skip_if {
            SkipIf::None => None,
            SkipIf::Matching => Some(MatchSpec::Literal(MATCHING_NEEDLE.to_string())),
            SkipIf::NonMatching => Some(MatchSpec::Literal(NON_MATCHING_NEEDLE.to_string())),
        },
        unless_exists,
        force,
        when: None,
        skip_empty: false,
        from: None,
        sh_before: None,
        sh_after: None,
        backup: false,
        shape: Vec::new(),
        determinism: None,
        freeze_policy: None,
        freeze_slots_dir: None,
        rdf: Vec::new(),
        rdf_inline: Vec::new(),
        prefixes: BTreeMap::new(),
        base: None,
    }
}

#[test]
fn exhaustive_write_decision_matrix() {
    let states = [
        TargetState::Absent,
        TargetState::PresentIdentical,
        TargetState::PresentDifferent,
        TargetState::PresentWithAnchor,
    ];
    let skip_ifs = [SkipIf::None, SkipIf::Matching, SkipIf::NonMatching];
    let mut cells = 0usize;

    for force in [false, true] {
        for unless_exists in [false, true] {
            for inject in [false, true] {
                for skip_if in skip_ifs {
                    for state in states {
                        // Anchor sub-dimensions fold in where inject=true and
                        // the target carries the anchor marker; elsewhere the
                        // only meaningful inject position is default append.
                        let anchors: &[Anchor] =
                            if inject && state == TargetState::PresentWithAnchor {
                                &[
                                    Anchor::Append,
                                    Anchor::Before,
                                    Anchor::After,
                                    Anchor::AtLine,
                                    Anchor::AtLineOutOfRange,
                                ]
                            } else {
                                &[Anchor::Append]
                            };
                        for &anchor in anchors {
                            cells += 1;
                            let dir = TempDir::new().expect("tempdir");
                            match state {
                                TargetState::Absent => {}
                                TargetState::PresentIdentical => {
                                    std::fs::write(dir.path().join("out.txt"), BODY)
                                        .expect("seed identical");
                                }
                                TargetState::PresentDifferent => {
                                    std::fs::write(dir.path().join("out.txt"), DIFFERENT)
                                        .expect("seed different");
                                }
                                TargetState::PresentWithAnchor => {
                                    std::fs::write(dir.path().join("out.txt"), ANCHORED)
                                        .expect("seed anchored");
                                }
                            }
                            let fm = frontmatter_for(force, unless_exists, inject, skip_if, anchor);
                            let expected = reference_model(
                                force,
                                unless_exists,
                                inject,
                                skip_if,
                                state,
                                anchor,
                            );
                            let actual = plan_write(dir.path(), "out.txt", BODY, &fm);
                            let got = classify(&actual);
                            assert_eq!(
                                got, expected,
                                "MATRIX MISMATCH: force={force} unless_exists={unless_exists} \
                                 inject={inject} skip_if={skip_if:?} state={state:?} \
                                 anchor={anchor:?} → expected {expected:?}, got {got:?} \
                                 (raw: {actual:?})"
                            );
                        }
                    }
                }
            }
        }
    }
    // 2*2*3 * (inject=false: 4 states + inject=true: 3 non-anchor states +
    // 5 anchor variants on the anchored state) = 12 * (4 + 3 + 5) = 144.
    assert_eq!(cells, 144, "matrix cell count drifted");
}

// ─────────────────────────────────────────────────────────────────────────
// Part B: property tests
// ─────────────────────────────────────────────────────────────────────────

fn write_project(root: &Path, n_entities: usize, n_templates: usize, paths: &[String]) {
    use std::fmt::Write as _;

    std::fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"prop\"\n\n[ontology]\nsource = \"onto.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    let mut ttl = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..n_entities {
        let _ = writeln!(ttl, "ex:e{i} ex:name \"entity_{i}\" .");
    }
    std::fs::write(root.join("onto.ttl"), ttl).expect("write onto.ttl");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    for (j, to) in paths.iter().enumerate().take(n_templates) {
        let tmpl = format!(
            "---\nto: {to}\nforce: true\nsparql:\n  rows: \"SELECT ?name WHERE {{ ?s <http://example.org/name> ?name }} ORDER BY ?name\"\n---\n// template {j}\n{{% for row in rows %}}{{{{ row.name }}}}\n{{% endfor %}}"
        );
        std::fs::write(root.join("templates").join(format!("t{j}.tmpl")), tmpl)
            .expect("write template");
    }
}

fn read_receipt_payload(root: &Path) -> ReceiptPayload {
    let raw = std::fs::read_to_string(root.join(RECEIPT_REL_PATH)).expect("read receipt");
    let receipt: SyncReceipt = serde_json::from_str(&raw).expect("parse receipt");
    receipt.payload
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(16))]

    /// Sync idempotence: content-derived receipt fields and output bytes
    /// are byte-identical across a no-op re-sync; the `decisions` field is
    /// NOT, and that difference is itself asserted as the correct behavior.
    ///
    /// UPDATED (2026-08-03, TECH-DEBT-003 fix in `write.rs`): identical
    /// content is now checked BEFORE the `force` arm, so a re-sync of
    /// force-templates whose rendered content is unchanged correctly
    /// reports "skipped: unchanged" rather than "written" again — see
    /// `reference_model`'s doc comment above for the full rationale. The
    /// prior version of this test asserted full receipt-payload byte
    /// identity across re-syncs, including `decisions`; that no longer
    /// holds (run 1's decision text is "written...", run 2's is
    /// "skipped: unchanged...") and asserting it would re-encode the same
    /// bug the fix corrects: the receipt's `decisions` field exists
    /// precisely to record what actually happened each run, so a
    /// legitimate written→skipped transition MUST show up there. Only the
    /// content-derived fields (`graph_hash`, `outputs`, `packs`,
    /// `closure`) are asserted invariant below.
    #[test]
    fn sync_is_idempotent(
        n_entities in 1usize..=4,
        n_templates in 1usize..=3,
        salt in 0u32..1000,
    ) {
        let dir = TempDir::new().expect("tempdir");
        let paths: Vec<String> = (0..n_templates)
            .map(|j| format!("out/gen_{salt}_{j}.rs"))
            .collect();
        write_project(dir.path(), n_entities, n_templates, &paths);

        let r1 = sync(dir.path(), SyncOptions::default()).expect("sync 1");
        prop_assert_eq!(r1.written.len(), n_templates);
        let p1 = read_receipt_payload(dir.path());
        let bytes1: Vec<Vec<u8>> = paths
            .iter()
            .map(|p| std::fs::read(dir.path().join(p)).expect("read out 1"))
            .collect();

        let r2 = sync(dir.path(), SyncOptions::default()).expect("sync 2");
        // force:true, content unchanged → identical-content Skip now wins
        // over force (see TECH-DEBT-003 fix note above): nothing is
        // (re-)written on the second sync.
        prop_assert_eq!(r2.written.len(), 0);
        prop_assert_eq!(&r1.graph_hash_hex, &r2.graph_hash_hex);
        let bytes2: Vec<Vec<u8>> = paths
            .iter()
            .map(|p| std::fs::read(dir.path().join(p)).expect("read out 2"))
            .collect();
        prop_assert_eq!(bytes1, bytes2, "re-sync changed output bytes");
        let p2 = read_receipt_payload(dir.path());

        // Content-derived fields are fully invariant across the re-sync.
        prop_assert_eq!(&p1.graph_hash, &p2.graph_hash, "graph_hash drifted");
        prop_assert_eq!(&p1.outputs, &p2.outputs, "output content hashes drifted");
        prop_assert_eq!(&p1.packs, &p2.packs, "pack content hashes drifted");
        prop_assert_eq!(&p1.closure, &p2.closure, "input closure drifted");

        // `decisions` legitimately differs: run 1 creates the file
        // (written), run 2 observes byte-identical content and skips.
        // Asserting this transition is the point of the TECH-DEBT-003 fix,
        // not an accident to paper over.
        for path in &paths {
            let d1 = p1.decisions.get(path.as_str()).expect("run1 decision present");
            let d2 = p2.decisions.get(path.as_str()).expect("run2 decision present");
            prop_assert!(
                d1.starts_with("written"),
                "run1 decision for {path}: expected `written*`, got {d1:?}"
            );
            prop_assert!(
                d2.starts_with("skipped: unchanged"),
                "run2 decision for {path}: expected `skipped: unchanged*`, got {d2:?}"
            );
        }

        // Sanity: without force, a re-sync IS decision-level unchanged.
        // (covered by the matrix: force=false + identical → Skip(unchanged))
        let receipt2: SyncReceipt = serde_json::from_str(
            &std::fs::read_to_string(dir.path().join(RECEIPT_REL_PATH)).expect("read"),
        ).expect("parse");
        prop_assert_eq!(receipt2.payload.outputs.len(), n_templates);
    }

    /// state_hash is invariant under triple insertion order.
    #[test]
    fn hash_insertion_order_invariance(
        idx in prop::collection::vec((0usize..5, 0usize..3, 0usize..5), 1..12),
        seed in 0u64..u64::MAX,
    ) {
        let triples: Vec<String> = idx
            .iter()
            .map(|(s, p, o)| {
                format!(
                    "<http://ex.org/s{s}> <http://ex.org/p{p}> \"o{o}\" ."
                )
            })
            .collect();
        let mut shuffled = triples.clone();
        // Deterministic Fisher–Yates with a tiny LCG (no rand dep).
        let mut state = seed | 1;
        for i in (1..shuffled.len()).rev() {
            state = state
                .wrapping_mul(6_364_136_223_846_793_005)
                .wrapping_add(1_442_695_040_888_963_407);
            #[allow(clippy::cast_possible_truncation)]
            let j = (state % (i as u64 + 1)) as usize;
            shuffled.swap(i, j);
        }

        let g1 = DeterministicGraph::new().expect("g1");
        for t in &triples {
            g1.insert_turtle(t).expect("insert g1");
        }
        let g2 = DeterministicGraph::new().expect("g2");
        for t in &shuffled {
            g2.insert_turtle(t).expect("insert g2");
        }
        prop_assert_eq!(
            g1.state_hash().expect("hash g1"),
            g2.state_hash().expect("hash g2"),
            "insertion order changed state_hash"
        );
    }

    /// Delta laws: compute→apply reaches the target hash; compose with the
    /// inverse is the empty delta.
    #[test]
    fn delta_laws(
        a_idx in prop::collection::vec((0usize..4, 0usize..2, 0usize..4), 0..8),
        b_idx in prop::collection::vec((0usize..4, 0usize..2, 0usize..4), 0..8),
    ) {
        let ttl_of = |idx: &[(usize, usize, usize)]| -> String {
            use std::fmt::Write as _;
            idx.iter().fold(String::new(), |mut ttl, (s, p, o)| {
                let _ = writeln!(ttl, "<http://ex.org/s{s}> <http://ex.org/p{p}> \"o{o}\" .");
                ttl
            })
        };
        let ga = DeterministicGraph::new().expect("ga");
        ga.insert_turtle(&ttl_of(&a_idx)).expect("load a");
        let gb = DeterministicGraph::new().expect("gb");
        gb.insert_turtle(&ttl_of(&b_idx)).expect("load b");

        let d = Delta::compute(&ga, &gb).expect("compute");
        d.apply(&ga).expect("apply");
        prop_assert_eq!(
            ga.state_hash().expect("hash a'"),
            gb.state_hash().expect("hash b"),
            "delta apply did not reach target state"
        );
        prop_assert!(
            d.compose(&d.inverse()).is_empty(),
            "compose(inverse) not empty: {:?}",
            d.compose(&d.inverse())
        );
        prop_assert!(
            d.inverse().compose(&d).is_empty(),
            "inverse.compose(self) not empty"
        );
    }
}

// ─────────────────────────────────────────────────────────────────────────
// Part C: frontmatter vocabulary fuzz + guard cross
// ─────────────────────────────────────────────────────────────────────────

#[test]
fn every_closed_vocabulary_key_parses() {
    let cases: Vec<(&str, String)> = vec![
        ("to", "to: out.rs".to_string()),
        (
            "sparql",
            "to: out.rs\nsparql:\n  q: SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        ),
        (
            "construct",
            "to: out.rs\nconstruct: \"CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\"".to_string(),
        ),
        ("inject", "to: out.rs\ninject: true".to_string()),
        (
            "before",
            "to: out.rs\ninject: true\nbefore: \"// x\"".to_string(),
        ),
        (
            "after",
            "to: out.rs\ninject: true\nafter: \"// x\"".to_string(),
        ),
        (
            "at_line",
            "to: out.rs\ninject: true\nat_line: 3".to_string(),
        ),
        ("skip_if", "to: out.rs\nskip_if: \"marker\"".to_string()),
        (
            "unless_exists",
            "to: out.rs\nunless_exists: true".to_string(),
        ),
        ("force", "to: out.rs\nforce: true".to_string()),
        ("when", "to: out.rs\nwhen: \"ASK { ?s ?p ?o }\"".to_string()),
        ("skip_empty", "to: out.rs\nskip_empty: true".to_string()),
    ];
    for (key, yaml) in cases {
        let content = format!("---\n{yaml}\n---\nbody");
        let parsed = Template::parse(&content);
        assert!(
            parsed.is_ok(),
            "closed-set key `{key}` failed to parse: {parsed:?}"
        );
    }
}

#[test]
fn unknown_keys_fail_with_fm_tpl_002_naming_the_key() {
    // `sh_before`/`backup`/`from` moved out of this list when the PROJ-302
    // frontmatter-schema extension made them real, valid keys (see
    // schema/frontmatter-schema.ttl); `rdf`/`rdf_inline`/`prefixes`/`base`
    // moved out the same way when the frontmatter-level RDF loading gap
    // (see `frontmatter_rdf_e2e.rs`) made them real, valid keys too.
    // `vars`/`mode`/`output_file`/`tera`/`query`/`foo` remain genuinely
    // unknown — ggen-core-only or invented keys this crate deliberately
    // does not adopt (docs/v26.7.4/GGEN_TOML_SCHEMA_MAPPING.md).
    let wrong = ["vars", "mode", "output_file", "tera", "query", "foo"];
    for key in wrong {
        let content = format!("---\nto: out.rs\n{key}: something\n---\nbody");
        let err =
            Template::parse(&content).expect_err(&format!("unknown key `{key}` must be rejected"));
        let msg = err.to_string();
        assert!(
            msg.contains("FM-TPL-002"),
            "`{key}`: missing FM-TPL-002 in: {msg}"
        );
        assert!(
            msg.contains(key),
            "`{key}`: error does not name the key: {msg}"
        );
    }
}

/// Cross: when ASK {true,false} × SELECT {0,N} rows × `skip_empty` {false,true},
/// asserted through real `sync()` runs.
#[test]
fn when_sparql_skip_empty_cross() {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum Verdict {
        Written,
        SkippedWhen,
        SkippedEmpty,
    }

    for ask_true in [false, true] {
        for select_rows in [false, true] {
            for skip_empty in [false, true] {
                let expected = if !ask_true {
                    Verdict::SkippedWhen
                } else if !select_rows && skip_empty {
                    Verdict::SkippedEmpty
                } else {
                    Verdict::Written
                };

                let dir = TempDir::new().expect("tempdir");
                std::fs::write(
                    dir.path().join("ggen.toml"),
                    "[project]\nname = \"cross\"\n\n[ontology]\nsource = \"onto.ttl\"\n\n[templates]\ndir = \"templates\"\n",
                )
                .expect("toml");
                std::fs::write(
                    dir.path().join("onto.ttl"),
                    "@prefix ex: <http://example.org/> .\nex:a ex:name \"alpha\" .\n",
                )
                .expect("ttl");
                std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir");

                let ask = if ask_true {
                    "ASK { ?s <http://example.org/name> ?o }"
                } else {
                    "ASK { ?s <http://example.org/absent> ?o }"
                };
                let select = if select_rows {
                    "SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name"
                } else {
                    "SELECT ?name WHERE { ?s <http://example.org/absent> ?name } ORDER BY ?name"
                };
                let tmpl = format!(
                    "---\nto: out.rs\nwhen: \"{ask}\"\nskip_empty: {skip_empty}\nsparql:\n  rows: \"{select}\"\n---\n{{% for row in rows %}}row {{{{ row.name }}}}\n{{% endfor %}}"
                );
                std::fs::write(dir.path().join("templates/t.tmpl"), tmpl).expect("tmpl");

                let report = sync(dir.path(), SyncOptions::default()).expect("sync");
                let decision = report
                    .decisions
                    .get("out.rs")
                    .cloned()
                    .unwrap_or_else(|| "MISSING".to_string());
                let got = if decision == "written" {
                    Verdict::Written
                } else if decision.contains("when guard") {
                    Verdict::SkippedWhen
                } else if decision.contains("skip_empty") {
                    Verdict::SkippedEmpty
                } else {
                    panic!(
                        "unclassifiable decision `{decision}` for ask_true={ask_true} \
                         select_rows={select_rows} skip_empty={skip_empty}"
                    );
                };
                assert_eq!(
                    got, expected,
                    "CROSS MISMATCH: ask_true={ask_true} select_rows={select_rows} \
                     skip_empty={skip_empty} decision={decision}"
                );
                let exists = dir.path().join("out.rs").exists();
                assert_eq!(
                    exists,
                    expected == Verdict::Written,
                    "file existence disagrees with verdict for ask_true={ask_true} \
                     select_rows={select_rows} skip_empty={skip_empty}"
                );
            }
        }
    }
}
