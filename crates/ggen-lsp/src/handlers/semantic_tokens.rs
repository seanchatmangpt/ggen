use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle_full(server: &GgenLanguageServer, params: SemanticTokensParams) -> Result<Option<SemanticTokensResult>> {
    let uri = &params.text_document.uri;
    let file_type = crate::state::FileType::from_uri(uri);
    Ok(server.state.get_document(uri).await
        .and_then(|content| crate::features::semantic_tokens::semantic_tokens(file_type, &content))
        .map(SemanticTokensResult::Tokens))
}
