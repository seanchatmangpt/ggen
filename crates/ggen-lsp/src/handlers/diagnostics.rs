use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle_code_action(
    server: &GgenLanguageServer, params: CodeActionParams,
) -> Result<Option<CodeActionResponse>> {
    let uri = &params.text_document.uri;
    let doc = server.state.get_document(uri).await.unwrap_or_default();
    let registry = server.state.routes.clone();

    let mut actions: Vec<CodeActionOrCommand> = Vec::new();
    for d in &params.context.diagnostics {
        let Some(route) = crate::route::action_route_for(&registry, d) else {
            continue;
        };
        let bindings = crate::route::RouteBindings {
            site: Some(d.range),
            ..Default::default()
        };
        let edit = crate::route::workspace_edit_from_route(route, &bindings, uri, &doc);
        let has_real_edit = edit
            .changes
            .as_ref()
            .is_some_and(|c| c.values().flatten().any(|e| !e.new_text.contains('{')));
        if !has_real_edit {
            continue;
        }
        let title = route
            .steps
            .nodes
            .first()
            .map(|s| format!("Apply transition: {}", s.title))
            .unwrap_or_else(|| route.description.clone());
        let data = crate::route::envelope_for_diagnostic(&registry, d, &doc, uri.path().as_str())
            .and_then(|env| serde_json::to_value(env).ok());

        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
            title,
            kind: Some(CodeActionKind::QUICKFIX),
            diagnostics: Some(vec![d.clone()]),
            edit: Some(edit),
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data,
            documentation: None,
            ..Default::default()
        }));
    }
    Ok(if actions.is_empty() {
        None
    } else {
        Some(actions)
    })
}
