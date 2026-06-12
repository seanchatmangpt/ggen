use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle(server: &GgenLanguageServer, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    Ok(server.state.get_analyzer(uri).await
        .and_then(|a| a.definition_at(position.line, position.character))
        .map(GotoDefinitionResponse::Scalar))
}
