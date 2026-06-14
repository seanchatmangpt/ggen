use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle(
    server: &GgenLanguageServer, params: InlayHintParams,
) -> Result<Option<Vec<InlayHint>>> {
    let uri = &params.text_document.uri;
    let Some(content) = server.state.get_document(uri).await else {
        return Ok(None);
    };
    let file_type = crate::state::FileType::from_uri(uri);
    Ok(crate::features::inlay_hint::inlay_hints(
        file_type,
        &content,
        params.range,
    ))
}
