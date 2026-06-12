use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle(server: &GgenLanguageServer, params: HoverParams) -> Result<Option<Hover>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    let analyzer = server.state.get_analyzer(uri).await;
    
    // Semantic hover (e.g. SHACL shape, term kind) takes priority.
    if let Some(h) = analyzer
        .as_ref()
        .and_then(|a| a.hover_at(position.line, position.character))
    {
        return Ok(Some(h));
    }
    
    // Otherwise, if a diagnostic covers the cursor, show its OCEL-TON card.
    let (Some(analyzer), Some(content)) = (analyzer, server.state.get_document(uri).await) else {
        return Ok(None);
    };
    let registry = server.state.routes.clone();
    for d in analyzer.diagnostics() {
        if crate::server::range_contains(&d.lsp.range, position) {
            if let Some(plan) = crate::route::route_plan_for_diagnostic(&registry, &d.lsp, &content)
            {
                let view = crate::route::CompactTraceView::from_route_plan(&plan, uri.path().as_str());
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: view.to_hover_markdown(),
                    }),
                    range: Some(d.lsp.range),
                }));
            }
        }
    }
    Ok(None)
}
