//! Chicago TDD: end-to-end repair-route projection over real analyzer output.
//!
//! Proves the POWL-native loop without an LSP client: a real ggen config
//! diagnostic (invalid enum value) from the TOML analyzer → family mapping →
//! registry route selection → an ADVISORY RoutePlan (never a destructive edit,
//! since the correct value isn't knowable from the diagnostic alone).

use ggen_lsp::analyzers::build_analyzer;
use ggen_lsp::route::{family_of_diagnostic, route_plan_for_diagnostic, RepairFamily};
use ggen_lsp::RouteRegistry;

#[test]
fn invalid_enum_value_yields_an_advisory_route_not_a_destructive_edit() {
    // Real analyzer → real diagnostic (a genuine ggen config law, not LLM).
    let analyzer = build_analyzer("ggen.toml", "[logging]\nlevel = \"verbose\"\n")
        .expect("ggen.toml is a law surface");
    let diags = analyzer.diagnostics();
    let enum_diag = diags
        .iter()
        .find(|d| d.message.contains("invalid value"))
        .expect("invalid-enum diagnostic present");

    // Family mapping (E0023 → ConfigValue).
    assert_eq!(
        family_of_diagnostic(enum_diag),
        Some(RepairFamily::ConfigValue)
    );

    // Registry selects an advisory route; its steps carry NO concrete edit
    // (we must never guess/delete the value).
    let registry = RouteRegistry::seeded();
    let plan = route_plan_for_diagnostic(&registry, enum_diag, "[logging]\nlevel = \"verbose\"\n")
        .expect("an advisory route exists");
    assert!(
        !plan.ordered_steps.is_empty(),
        "advisory guidance is present"
    );
    assert!(
        plan.ordered_steps.iter().all(|s| s.edit.is_none()),
        "advisory route must offer NO destructive edit, got {:?}",
        plan.ordered_steps
    );
}

#[test]
fn ggen_does_not_flag_llm_or_unknown_sections() {
    // ggen is not in the LLM business: an [ai] section is unknown, not flagged.
    let analyzer =
        build_analyzer("ggen.toml", "[ai]\nprovider = \"openai\"\n").expect("law surface");
    assert!(
        analyzer.diagnostics().is_empty(),
        "ggen must not emit diagnostics about LLM/unknown sections"
    );
}

#[test]
fn clean_config_has_no_diagnostics() {
    let analyzer =
        build_analyzer("ggen.toml", "[logging]\nlevel = \"info\"\n").expect("law surface");
    assert!(analyzer.diagnostics().is_empty(), "valid config is clean");
}
