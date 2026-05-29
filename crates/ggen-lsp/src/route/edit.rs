//! Render repair-step edit templates into concrete LSP edits.
//!
//! A step's [`EditTemplate`](super::model::EditTemplate) is instantiated against
//! the diagnostic's [`RouteBindings`](super::model::RouteBindings) and the
//! document text to yield a `TextEdit`. The same renderer feeds both the editor
//! `WorkspaceEdit` and the agent `RoutePlan`, so both apply identical repairs.

use std::collections::HashMap;
use tower_lsp::lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit};

use super::model::{Anchor, EditTemplate, PartialOrder, RepairRoute, RouteBindings};
use super::plan::{DiagnosticRef, RoutePlan, RoutePlanStep};

/// Fill `{prefix}`/`{iri}`/`{symbol}` placeholders from bindings.
fn render_tmpl(tmpl: &str, b: &RouteBindings) -> String {
    let mut out = tmpl.to_string();
    if let Some(p) = &b.prefix {
        out = out.replace("{prefix}", p);
    }
    if let Some(i) = &b.iri {
        out = out.replace("{iri}", i);
    }
    if let Some(s) = &b.symbol {
        out = out.replace("{symbol}", s);
    }
    out
}

/// Compute the `TextEdit` for one step, or `None` for `NoOp`/uncomputable steps.
#[must_use]
pub fn render_edit(tmpl: &EditTemplate, b: &RouteBindings, doc: &str) -> Option<TextEdit> {
    match tmpl {
        EditTemplate::NoOp => None,
        EditTemplate::ReplaceSite { text } => {
            let range = b.site?;
            Some(TextEdit {
                range,
                new_text: render_tmpl(text, b),
            })
        }
        EditTemplate::InsertLine { anchor, text } => {
            let line = anchor_line(*anchor, doc);
            let pos = Position { line, character: 0 };
            Some(TextEdit {
                range: Range {
                    start: pos,
                    end: pos,
                },
                new_text: format!("{}\n", render_tmpl(text, b)),
            })
        }
    }
}

/// 0-based line at which an [`Anchor`] inserts.
fn anchor_line(anchor: Anchor, doc: &str) -> u32 {
    match anchor {
        Anchor::TopOfFile => 0,
        Anchor::AfterLastPrefix => {
            let mut last_prefix = None;
            for (idx, text) in doc.lines().enumerate() {
                let t = text.trim_start();
                if t.starts_with("@prefix") || t.starts_with("PREFIX") {
                    last_prefix = Some(idx);
                }
            }
            last_prefix
                .map(|i| u32::try_from(i + 1).unwrap_or(0))
                .unwrap_or(0)
        }
    }
}

/// Build a `WorkspaceEdit` applying all of a route's steps to `uri`/`doc`.
#[must_use]
pub fn workspace_edit_from_route(
    route: &RepairRoute, bindings: &RouteBindings, uri: &Url, doc: &str,
) -> WorkspaceEdit {
    let edits: Vec<TextEdit> = ordered_steps(&route.steps)
        .iter()
        .filter_map(|s| render_edit(&s.edit, bindings, doc))
        .collect();
    let mut changes = HashMap::new();
    changes.insert(uri.clone(), edits);
    WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    }
}

/// Build the editor-agnostic `RoutePlan` projection for non-editor agents.
#[must_use]
pub fn route_plan(
    route: &RepairRoute, bindings: &RouteBindings, doc: &str, target: DiagnosticRef,
) -> RoutePlan {
    let ordered_steps = ordered_steps(&route.steps)
        .iter()
        .map(|s| RoutePlanStep {
            title: s.title.clone(),
            edit: render_edit(&s.edit, bindings, doc),
        })
        .collect();
    RoutePlan {
        route_id: route.id.clone(),
        family: route.family,
        title: route.description.clone(),
        provenance: route.provenance.clone(),
        ordered_steps,
        target,
    }
}

/// Steps in topological (lawful) order; falls back to declaration order if the
/// partial order is somehow cyclic (soundness is checked at registry load).
fn ordered_steps(po: &PartialOrder) -> Vec<super::model::RepairStep> {
    match po.topo_order() {
        Some(order) => order
            .iter()
            .filter_map(|id| po.nodes.iter().find(|n| &n.id == id).cloned())
            .collect(),
        None => po.nodes.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::super::model::{
        EditTemplate, PartialOrder, Provenance, RepairFamily, RepairRoute, RepairStep, RouteId,
        StepId,
    };
    use super::*;

    fn uri() -> Url {
        Url::parse("file:///spec.ttl").expect("valid uri")
    }

    #[test]
    fn insert_line_after_last_prefix() {
        let doc = "@prefix ex: <http://example.org/> .\nex:a ex:b ex:c .\n";
        let tmpl = EditTemplate::InsertLine {
            anchor: Anchor::AfterLastPrefix,
            text: "@prefix {prefix}: <{iri}> .".to_string(),
        };
        let b = RouteBindings {
            prefix: Some("sh".into()),
            iri: Some("http://www.w3.org/ns/shacl#".into()),
            ..Default::default()
        };
        let edit = render_edit(&tmpl, &b, doc).expect("edit");
        assert_eq!(edit.range.start.line, 1);
        assert_eq!(
            edit.new_text,
            "@prefix sh: <http://www.w3.org/ns/shacl#> .\n"
        );
    }

    #[test]
    fn replace_site_token() {
        let doc = "ex:a ex:hasOwner ex:c .\n";
        let tmpl = EditTemplate::ReplaceSite {
            text: "{iri}".to_string(),
        };
        let b = RouteBindings {
            site: Some(Range {
                start: Position {
                    line: 0,
                    character: 5,
                },
                end: Position {
                    line: 0,
                    character: 16,
                },
            }),
            iri: Some("dcterms:creator".into()),
            ..Default::default()
        };
        let edit = render_edit(&tmpl, &b, doc).expect("edit");
        assert_eq!(edit.new_text, "dcterms:creator");
        assert_eq!(edit.range.start.character, 5);
    }

    #[test]
    fn workspace_edit_collects_steps() {
        let route = RepairRoute {
            id: RouteId("r".into()),
            family: RepairFamily::ParseFailure,
            steps: PartialOrder {
                nodes: vec![RepairStep {
                    id: StepId("s".into()),
                    title: "declare".into(),
                    edit: EditTemplate::InsertLine {
                        anchor: Anchor::TopOfFile,
                        text: "@prefix {prefix}: <{iri}> .".into(),
                    },
                }],
                edges: vec![],
            },
            description: "Declare prefix".into(),
            provenance: Provenance::Seeded,
            priority: 0,
        };
        let b = RouteBindings {
            prefix: Some("ex".into()),
            iri: Some("http://example.org/".into()),
            ..Default::default()
        };
        let we = workspace_edit_from_route(&route, &b, &uri(), "ex:a ex:b ex:c .\n");
        let changes = we.changes.expect("changes");
        assert_eq!(changes.get(&uri()).map(Vec::len), Some(1));
    }
}
