use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle_prepare(server: &GgenLanguageServer, params: TextDocumentPositionParams) -> Result<Option<PrepareRenameResponse>> {
    let uri = &params.text_document.uri;
    let position = params.position;
    Ok(server.state.get_analyzer(uri).await
        .and_then(|a| a.prepare_rename(position))
        .map(PrepareRenameResponse::Range))
}

pub async fn handle(server: &GgenLanguageServer, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
    let uri = &params.text_document_position.text_document.uri;
    let position = params.text_document_position.position;
    let new_name = params.new_name;
    Ok(server.state.get_analyzer(uri).await.and_then(|a| a.rename_symbol(position, &new_name)))
}
