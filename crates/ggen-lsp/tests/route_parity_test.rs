//! Cross-channel route parity: editor (`action_route_for`) and headless/MCP
//! (`route_plan_for_diagnostic`) must return the SAME route id for the same
//! (registry, diagnostic) — proven for both the promotable-mined case (mined
//! wins) and the sub-threshold case (seed holds).

use ggen_lsp::route::{
    action_route_for, default_pack_routes_path, route_plan_for_diagnostic, write_promoted,
    EditTemplate, PartialOrder, PromotedRoutes, Provenance, RepairFamily, RepairRoute, RepairStep,
    RouteId, RouteRegistry, StepId,
};
use tempfile::TempDir;
use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range};

fn diag(code: &str) -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 1,
            },
        },
        severity: Some(DiagnosticSeverity::WARNING),
        code: Some(NumberOrString::String(code.to_string())),
        code_description: None,
        source: Some("ggen-lsp".to_string()),
        message: "x".to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn mined_route(support: u32, success: f32) -> RepairRoute {
    RepairRoute {
        id: RouteId("mined.template-failure".into()),
        family: RepairFamily::TemplateFailure,
        steps: PartialOrder {
            nodes: vec![RepairStep {
                id: StepId("advise".into()),
                title: "mined".into(),
                edit: EditTemplate::NoOp,
            }],
            edges: vec![],
        },
        description: "mined".into(),
        provenance: Provenance::Mined {
            confidence: success,
            support,
            success_rate: success,
            first_seen: "t0".into(),
            last_seen: "t1".into(),
            source_report_hash: "h".into(),
        },
        priority: 1,
    }
}

fn registry_with(route: RepairRoute) -> RouteRegistry {
    let dir = TempDir::new().expect("tempdir");
    let path = default_pack_routes_path(dir.path());
    write_promoted(
        &path,
        &PromotedRoutes {
            version: PromotedRoutes::VERSION,
            source_log_hash: "x".into(),
            routes: vec![route],
        },
    )
    .expect("write");
    let reg = RouteRegistry::seeded().with_pack_routes(&path);
    drop(dir);
    reg
}

#[test]
fn promotable_mined_route_wins_in_all_channels() {
    let reg = registry_with(mined_route(5, 0.9)); // above thresholds
    let d = diag("E0010"); // TemplateFailure family (has a seed too)

    let editor = action_route_for(&reg, &d).map(|r| r.id.0.clone());
    let headless = route_plan_for_diagnostic(&reg, &d, "").map(|p| p.route_id.0.clone());

    assert_eq!(editor.as_deref(), Some("mined.template-failure"));
    assert_eq!(
        editor, headless,
        "editor and headless/MCP must agree on route id"
    );
}

#[test]
fn subthreshold_mined_route_loses_to_seed_in_all_channels() {
    let reg = registry_with(mined_route(1, 0.9)); // below support threshold
    let d = diag("E0010");

    let editor = action_route_for(&reg, &d).map(|r| r.id.0.clone());
    let headless = route_plan_for_diagnostic(&reg, &d, "").map(|p| p.route_id.0.clone());

    assert_eq!(
        editor.as_deref(),
        Some("template.values-inline"),
        "seed holds"
    );
    assert_eq!(editor, headless, "channels agree the seed holds");
}
