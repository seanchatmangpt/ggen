use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle_prepare(
    server: &GgenLanguageServer, params: CallHierarchyPrepareParams,
) -> Result<Option<Vec<CallHierarchyItem>>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    Ok(server
        .state
        .get_analyzer(uri)
        .await
        .and_then(|a| a.call_hierarchy_items(position)))
}
