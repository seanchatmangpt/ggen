use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle(
    server: &GgenLanguageServer, params: ReferenceParams,
) -> Result<Option<Vec<Location>>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    if let Some(analyzer_hits) = server
        .state
        .get_analyzer(uri)
        .await
        .and_then(|a| a.references_at(position.line, position.character))
        .filter(|hits| !hits.is_empty())
    {
        return Ok(Some(analyzer_hits));
    }

    // G4 fallback, strictly behind analyzer None/empty: ALL
    // `resolve_moniker_in_tree` hits for the cursor moniker under the project
    // root containing the file.
    let hits = crate::handlers::moniker::moniker_fallback_hits(server, uri, &position).await;
    if hits.is_empty() {
        return Ok(None);
    }
    Ok(Some(
        hits.into_iter()
            .map(|hit| Location {
                uri: hit.uri,
                range: hit.range,
            })
            .collect(),
    ))
}
