use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle(
    server: &GgenLanguageServer, params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    Ok(server
        .state
        .get_analyzer(uri)
        .await
        .and_then(|a| a.completion_at(position.line, position.character)))
}
