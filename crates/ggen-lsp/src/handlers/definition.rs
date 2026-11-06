use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle(
    server: &GgenLanguageServer, params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    if let Some(location) = server
        .state
        .get_analyzer(uri)
        .await
        .and_then(|a| a.definition_at(position.line, position.character))
    {
        return Ok(Some(GotoDefinitionResponse::Scalar(location)));
    }

    // G4 fallback, strictly behind analyzer None: resolve the cursor moniker
    // (banner-or-ontology-declared surfaces only) to its first real on-disk
    // hit under the project root containing the file.
    let hits = crate::handlers::moniker::moniker_fallback_hits(server, uri, &position).await;
    Ok(hits.first().map(|hit| {
        GotoDefinitionResponse::Scalar(Location {
            uri: hit.uri.clone(),
            range: hit.range,
        })
    }))
}
