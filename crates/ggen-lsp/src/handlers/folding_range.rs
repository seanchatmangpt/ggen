use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle(
    server: &GgenLanguageServer, params: FoldingRangeParams,
) -> Result<Option<Vec<FoldingRange>>> {
    let uri = &params.text_document.uri;
    Ok(server
        .state
        .get_analyzer(uri)
        .await
        .and_then(|a| a.folding_ranges()))
}
