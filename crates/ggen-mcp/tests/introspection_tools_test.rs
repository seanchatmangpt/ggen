//! Chicago TDD for the read-only introspection tools: real `TempDir`, real
//! `ggen.toml`, real ontology, real templates. No mocks.

mod common;

use common::{write_declarative_project, write_frontmatter_project};

use ggen_mcp::error::ErrorCategory;
use ggen_mcp::tools::{
    capability_status::{capability_status, CapabilityStatusParams},
    config_classify::{config_classify, ConfigClassifyParams},
    frontmatter_lint::{frontmatter_lint, FrontmatterLintParams},
    frontmatter_schema::{frontmatter_schema, FrontmatterSchemaParams},
    rule_graph::{rule_graph, RuleGraphParams},
};

// ---------------------------------------------------------------------------
// ggen_config_classify
// ---------------------------------------------------------------------------

#[test]
fn classify_distinguishes_the_two_real_schemas() {
    let fm = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(fm.path());
    let got = config_classify(&ConfigClassifyParams {
        root: fm.path().display().to_string(),
    })
    .expect("classify frontmatter project");
    assert_eq!(got.schema, "frontmatter");

    let decl = tempfile::tempdir().expect("tempdir");
    write_declarative_project(decl.path());
    let got = config_classify(&ConfigClassifyParams {
        root: decl.path().display().to_string(),
    })
    .expect("classify declarative project");
    assert_eq!(got.schema, "declarative_rules");
}

/// Classification must not mutate the project -- it is advertised as
/// reading exactly one file and running no pipeline stage.
#[test]
fn classify_does_not_touch_the_project() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let before = dir_listing(dir.path());
    let _ = config_classify(&ConfigClassifyParams {
        root: dir.path().display().to_string(),
    })
    .expect("classify");
    assert_eq!(
        before,
        dir_listing(dir.path()),
        "classify must not create or remove files"
    );
}

fn dir_listing(root: &std::path::Path) -> Vec<String> {
    let mut out = Vec::new();
    for entry in walk(root) {
        out.push(entry);
    }
    out.sort();
    out
}

fn walk(root: &std::path::Path) -> Vec<String> {
    let mut out = Vec::new();
    let Ok(entries) = std::fs::read_dir(root) else {
        return out;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        out.push(path.display().to_string());
        if path.is_dir() {
            out.extend(walk(&path));
        }
    }
    out
}

// ---------------------------------------------------------------------------
// ggen_frontmatter_schema
// ---------------------------------------------------------------------------

/// The key count is asserted against the live derive, not a hardcoded
/// number -- so this test cannot rot when `Frontmatter` gains or loses a
/// field, while still proving the tool reports the real set.
#[test]
fn frontmatter_schema_matches_the_live_derive() {
    let derived = schemars::schema_for!(ggen_engine::template::Frontmatter);
    let expected_count = serde_json::to_value(&derived)
        .expect("schema to json")
        .get("properties")
        .and_then(|p| p.as_object())
        .map(serde_json::Map::len)
        .expect("frontmatter schema must expose properties");

    let got = frontmatter_schema(&FrontmatterSchemaParams::default()).expect("schema");
    assert_eq!(got.key_count, expected_count);
    assert_eq!(got.keys.len(), expected_count);
    assert!(
        expected_count >= 20,
        "sanity: expected a large key set, got {expected_count}"
    );
}

/// `to` is the one required key, and `for_each` -- the fan-out mechanism
/// the verified incident never discovered -- must be present and findable.
#[test]
fn frontmatter_schema_surfaces_to_and_for_each() {
    let got = frontmatter_schema(&FrontmatterSchemaParams::default()).expect("schema");
    let to = got
        .keys
        .iter()
        .find(|k| k.name == "to")
        .expect("`to` key must exist");
    assert!(to.required, "`to` is the one required frontmatter key");
    assert!(
        got.keys.iter().any(|k| k.name == "for_each"),
        "`for_each` must be discoverable -- not surfacing it is the verified friction"
    );
}

/// The projection-mode rule is control flow, not schema, so it must be
/// stated explicitly or it is undiscoverable.
#[test]
fn frontmatter_schema_states_all_three_projection_modes() {
    let got = frontmatter_schema(&FrontmatterSchemaParams::default()).expect("schema");
    let modes: Vec<&str> = got
        .projection_modes
        .iter()
        .map(|m| m.mode.as_str())
        .collect();
    assert!(modes.contains(&"fan_out"));
    assert!(modes.contains(&"aggregate"));
    assert!(modes.contains(&"single"));
}

/// Filtering to one existing key must NOT collapse `key_count` to `1`.
/// `key_count` is documented as "the total number of legal frontmatter
/// keys", and it used to be computed AFTER `params.key`'s filter had
/// already shrunk the keys vector -- this is the case that exposes it.
#[test]
fn frontmatter_schema_key_count_is_the_total_even_when_filtered_to_one_key() {
    let full = frontmatter_schema(&FrontmatterSchemaParams::default()).expect("full schema");
    let filtered = frontmatter_schema(&FrontmatterSchemaParams {
        key: Some("to".to_string()),
    })
    .expect("filtered schema");

    assert_eq!(filtered.keys.len(), 1, "filtered response returns one key");
    assert_eq!(
        filtered.key_count,
        full.key_count,
        "key_count must remain the total legal-key count ({}), not the filtered \
         response's length ({}), regardless of `params.key`",
        full.key_count,
        filtered.keys.len()
    );
}

/// An unknown key name must name the real key set rather than return an
/// empty result the caller could mistake for "no such concept".
#[test]
fn frontmatter_schema_unknown_key_lists_the_real_keys() {
    let err = frontmatter_schema(&FrontmatterSchemaParams {
        key: Some("definitely_not_a_key".to_string()),
    })
    .expect_err("unknown key must error");
    assert_eq!(err.category, ErrorCategory::NotFound);
    assert!(
        err.message.contains("to"),
        "error must list the legal keys: {}",
        err.message
    );
}

// ---------------------------------------------------------------------------
// ggen_frontmatter_lint
// ---------------------------------------------------------------------------

#[test]
fn lint_reports_bound_vars_on_a_good_template() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let got = frontmatter_lint(&FrontmatterLintParams {
        root: dir.path().display().to_string(),
        template_path: "templates/names.tmpl".to_string(),
    })
    .expect("lint");
    assert!(
        got.undefined_vars.is_empty(),
        "no unbound vars: {:?}",
        got.undefined_vars
    );
    assert_eq!(got.projection_mode, "single");
}

/// The var-diff check: a template iterating a root name its frontmatter
/// never projects must land in `undefined_vars`.
///
/// Scope note (verified against `ggen_engine::lint::consumed_vars`): the
/// diff operates on ROOT identifiers. A field access on a loop-local, e.g.
/// `{{ row.whatever }}` inside `{% for row in people %}`, is deliberately
/// NOT checked -- `row` is a local and its columns are not statically
/// known from the SELECT text. So the checkable case is an unbound root,
/// which is what this asserts.
#[test]
fn lint_catches_a_consumed_but_unprojected_root_var() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    std::fs::write(
        dir.path().join("templates/bad.tmpl"),
        common::UNBOUND_VAR_TEMPLATE,
    )
    .expect("write bad template");

    let got = frontmatter_lint(&FrontmatterLintParams {
        root: dir.path().display().to_string(),
        template_path: "templates/bad.tmpl".to_string(),
    })
    .expect("lint must parse this template");
    assert!(
        got.undefined_vars.contains("nosuchquery"),
        "`nosuchquery` is iterated but never projected; got {:?}",
        got.undefined_vars
    );
}

/// Regression for the second verified incident: Jinja2 ternary syntax is
/// not valid Tera and must be reported, not silently accepted.
#[test]
fn lint_flags_jinja_ternary_that_tera_cannot_parse() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    std::fs::write(
        dir.path().join("templates/ternary.tmpl"),
        common::JINJA_TERNARY_TEMPLATE,
    )
    .expect("write ternary template");

    let result = frontmatter_lint(&FrontmatterLintParams {
        root: dir.path().display().to_string(),
        template_path: "templates/ternary.tmpl".to_string(),
    });

    // Either the frontmatter/template parse refuses outright, or the lint
    // pass reports a diagnostic. Both are acceptable "reported"; silently
    // clean is not.
    match result {
        Err(e) => assert_eq!(e.category, ErrorCategory::SyntaxError),
        Ok(got) => assert!(
            !got.diagnostics.is_empty(),
            "Tera cannot parse a Jinja2 ternary; this must not lint clean"
        ),
    }
}

/// A path escaping the project root must be refused, not read.
#[test]
fn lint_refuses_path_traversal() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let err = frontmatter_lint(&FrontmatterLintParams {
        root: dir.path().display().to_string(),
        template_path: "../../../etc/passwd".to_string(),
    })
    .expect_err("traversal must be refused");
    assert_eq!(err.category, ErrorCategory::PathTraversal);
}

// ---------------------------------------------------------------------------
// ggen_rule_graph
// ---------------------------------------------------------------------------

#[test]
fn rule_graph_maps_rules_to_queries_and_outputs() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_declarative_project(dir.path());
    let got = rule_graph(&RuleGraphParams {
        root: dir.path().display().to_string(),
        rule_name: None,
        offset: None,
        limit: None,
    })
    .expect("rule graph");

    assert_eq!(got.total_rules, 2);
    let names = got
        .rules
        .iter()
        .find(|r| r.rule_id == "names")
        .expect("`names` rule");
    assert_eq!(names.output_file, "out/names.txt");
    assert!(names.query_inline);
    assert!(
        names.selected_vars.contains(&"name".to_string()),
        "SELECT ?name must be reported as a bound var: {:?}",
        names.selected_vars
    );
}

/// Filtering to one existing rule must NOT collapse `total_rules` to `1`.
/// The field's own doc comment promises "before paging/filtering"; it used
/// to be computed AFTER `rule_name`'s `retain()` had already shrunk the
/// working set to just the matched rule.
#[test]
fn rule_graph_total_rules_is_the_project_total_even_when_filtered_to_one_rule() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_declarative_project(dir.path());
    let got = rule_graph(&RuleGraphParams {
        root: dir.path().display().to_string(),
        rule_name: Some("names".to_string()),
        offset: None,
        limit: None,
    })
    .expect("rule graph filtered to one rule");

    assert_eq!(got.rules.len(), 1, "filtered response returns one rule");
    assert_eq!(
        got.total_rules,
        2,
        "total_rules must remain the project's real total (2), not the filtered \
         response's length ({}), regardless of rule_name",
        got.rules.len()
    );
}

#[test]
fn rule_graph_unknown_rule_names_the_available_rules() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_declarative_project(dir.path());
    let err = rule_graph(&RuleGraphParams {
        root: dir.path().display().to_string(),
        rule_name: Some("nope".to_string()),
        offset: None,
        limit: None,
    })
    .expect_err("unknown rule must error");
    assert_eq!(err.category, ErrorCategory::NotFound);
    assert!(
        err.message.contains("names"),
        "must list real rules: {}",
        err.message
    );
}

// ---------------------------------------------------------------------------
// ggen_capability_status
// ---------------------------------------------------------------------------

/// The declarative fixture deliberately uses `template.pack`, which is
/// structurally accepted but refused at sync time. The tool must report
/// that THIS project is affected, and name the rule.
#[test]
fn capability_status_detects_actual_use_of_an_inert_field() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_declarative_project(dir.path());
    let got = capability_status(&CapabilityStatusParams {
        root: dir.path().display().to_string(),
    })
    .expect("capability status");

    assert!(got.project_is_affected, "the fixture uses template.pack");
    let pack = got
        .inert_fields
        .iter()
        .find(|f| f.field.contains("Pack"))
        .expect("pack entry must exist");
    assert_eq!(pack.used_by_rules, vec!["from-pack".to_string()]);
    assert_eq!(pack.code, "FM-GEN-007");
}

/// A project that uses none of the inert fields must report unaffected --
/// the fields are still listed (they are still inert), but nothing here
/// depends on them.
#[test]
fn capability_status_reports_unaffected_when_nothing_uses_them() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_frontmatter_project(dir.path());
    let got = capability_status(&CapabilityStatusParams {
        root: dir.path().display().to_string(),
    })
    .expect("capability status");
    assert!(!got.project_is_affected);
    assert!(
        !got.inert_fields.is_empty(),
        "inert fields are still reported"
    );
    assert!(got.inert_fields.iter().all(|f| f.used_by_rules.is_empty()));
}

/// The `reason` field is documented as NOT verbatim (a fixed summary, not
/// the real per-rule pipeline message) -- guard the load-bearing substrings
/// it does claim to carry so the two sides can't silently diverge further.
#[test]
fn capability_status_reasons_name_the_field_and_say_not_implemented() {
    let dir = tempfile::tempdir().expect("tempdir");
    write_declarative_project(dir.path());
    let got = capability_status(&CapabilityStatusParams {
        root: dir.path().display().to_string(),
    })
    .expect("capability status");

    assert_eq!(got.inert_fields.len(), 3, "Pack, Git, Package");
    for (field, marker) in [("Pack", "Pack"), ("Git", "Git"), ("Package", "Package")] {
        let entry = got
            .inert_fields
            .iter()
            .find(|f| f.field.contains(field))
            .unwrap_or_else(|| panic!("{field} entry must exist"));
        assert!(
            entry.reason.contains(&format!("TemplateSource::{marker}")),
            "{field} reason must name its own variant: {:?}",
            entry.reason
        );
        assert!(
            entry.reason.contains("not implemented yet"),
            "{field} reason must say why: {:?}",
            entry.reason
        );
        assert_eq!(entry.code, "FM-GEN-007");
    }
}
