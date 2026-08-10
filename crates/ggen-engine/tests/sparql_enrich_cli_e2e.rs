//! Chicago-TDD subprocess proof of the Enrich pipeline stage
//! (Resolve -> **Enrich** -> Extract -> Render -> Write -> Receipt,
//! `crate::sync`'s Stage 2, `crates/ggen-engine/src/sync.rs`): a template's
//! `construct:` frontmatter query must derive a NEW fact that did not exist
//! in the source ontology, insert it into the shared graph, and have a
//! SECOND template's `sparql:` query see and consume that derived fact --
//! real subprocess round trip, not a unit-level parse check.
//!
//! Shape copied from `hygen_parity_e2e.rs` (real `ggen` binary via
//! `chicago_tdd_tools::cli_proof::CliHarness`, no mocks, `TempDir` scaffold).
//!
//! The proof: `ex:widgetA`/`ex:widgetB` both start with only `ex:name` and
//! `ex:count` facts -- neither has an `ex:status` triple anywhere in the
//! source ontology. Template 1's `construct:` derives `ex:status
//! "high-count"` only for the widget whose `ex:count > 100` (widgetA, not
//! widgetB). Template 2's `sparql:` query selects on that derived
//! `ex:status` triple, which cannot match anything in the un-enriched
//! graph. If Enrich did not actually run `construct:` against the shared
//! graph before Render, template 2's query returns zero rows for both
//! widgets (a lawful `for_each` skip, not a hard failure -- see
//! `sync.rs`'s "`for_each` `entities` produced 0 rows" path) and neither
//! `report_*.txt` file is written. The real, positive-and-negative
//! assertions below (widgetA's report exists with real derived content,
//! widgetB's report does not exist at all) are the only way this test can
//! pass: they require enrichment to have actually run, filtered correctly,
//! and round-tripped through the shared graph into a second template's
//! query -- not merely that `construct:` parses.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "sparql-enrich-demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/enrich#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:widgetA ex:name "WidgetA" ; ex:count "150"^^xsd:integer .
ex:widgetB ex:name "WidgetB" ; ex:count "5"^^xsd:integer .
"#;

/// Stage 2 (Enrich): derives `ex:status "high-count"` for any subject whose
/// `ex:count > 100`. Neither `ex:widgetA` nor `ex:widgetB` has an
/// `ex:status` triple in `ONTOLOGY` above -- this is the only place that
/// triple can come from.
const DERIVE_STATUS_TMPL: &str = "---\n\
to: derive_marker.txt\n\
force: true\n\
construct: |\n\
\x20 CONSTRUCT { ?s <http://example.org/enrich#status> \"high-count\" }\n\
\x20 WHERE { ?s <http://example.org/enrich#count> ?c . FILTER(?c > 100) }\n\
---\n\
derive ran\n";

/// Consumes the derived `ex:status` triple via a plain `sparql:`/`for_each:`
/// query -- exactly the pattern a real generator template would use, no
/// special-casing of the enrichment mechanism.
const REPORT_TMPL: &str = "---\n\
to: \"report_{{ row.name }}.txt\"\n\
force: true\n\
sparql:\n\
\x20 entities: |\n\
\x20   PREFIX ex: <http://example.org/enrich#>\n\
\x20   SELECT ?name WHERE { ?s ex:status \"high-count\" ; ex:name ?name }\n\
for_each: entities\n\
---\n\
{{ row.name }} is high-count\n";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(
        root.join("templates").join("derive_status.tmpl"),
        DERIVE_STATUS_TMPL,
    )
    .expect("write derive_status.tmpl");
    std::fs::write(root.join("templates").join("report.tmpl"), REPORT_TMPL)
        .expect("write report.tmpl");
}

#[test]
fn construct_derived_fact_round_trips_into_a_second_templates_sparql_query() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());

    let output = CliHarness::cargo_bin("ggen")
        .args(["sync", "run"])
        .current_dir(dir.path())
        .run()
        .expect("spawn ggen sync run");
    output.assert_success();

    // Template 1 ran at all (sanity check, not the real proof).
    let marker = std::fs::read_to_string(dir.path().join("derive_marker.txt"))
        .expect("derive_marker.txt must be written");
    assert_eq!(marker, "derive ran\n");

    // The real proof: template 2's `sparql:` query matched the `ex:status`
    // triple that ONLY `construct:` could have produced -- that triple is
    // nowhere in `ONTOLOGY`. If Enrich had not run (or ran against an
    // isolated/discarded graph instead of the shared one Render reads),
    // this file would not exist at all (a lawful zero-row `for_each` skip,
    // not a crash) rather than existing with the wrong content.
    let report_a = std::fs::read_to_string(dir.path().join("report_WidgetA.txt")).expect(
        "report_WidgetA.txt must exist: the derived ex:status triple must have \
                 round-tripped through the shared graph into template 2's sparql: query",
    );
    assert_eq!(
        report_a, "WidgetA is high-count\n",
        "report content must reflect the derived fact, not something coincidental"
    );

    // Negative control: widgetB's count (5) fails the construct's FILTER, so
    // no ex:status triple is derived for it, so template 2's query must not
    // match it either. Proves the enrichment is selective (real FILTER
    // evaluation over real data), not a construct that fires unconditionally.
    let report_b_path = dir.path().join("report_WidgetB.txt");
    assert!(
        !report_b_path.exists(),
        "widgetB must NOT get a report: its count (5) does not satisfy the construct's \
         FILTER(?c > 100), so no ex:status triple should have been derived for it"
    );
}
