use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle(server: &GgenLanguageServer, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
    let uri = &params.text_document.uri;
    Ok(server.state.get_analyzer(uri).await
        .map(|a| a.document_symbols(None))
        .filter(|v| !v.is_empty())
        .map(DocumentSymbolResponse::Nested))
}
