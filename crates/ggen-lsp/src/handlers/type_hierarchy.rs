use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle_prepare(server: &GgenLanguageServer, params: TypeHierarchyPrepareParams) -> Result<Option<Vec<TypeHierarchyItem>>> {
    let uri = &params.text_document_position_params.text_document.uri;
    let position = params.text_document_position_params.position;
    Ok(server.state.get_analyzer(uri).await.and_then(|a| a.type_hierarchy_items(position)))
}
