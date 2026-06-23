use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

pub async fn handle_formatting(
    server: &GgenLanguageServer, params: DocumentFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = &params.text_document.uri;
    let Some(content) = server.state.get_document(uri).await else {
        return Ok(None);
    };
    let file_type = crate::state::FileType::from_uri(uri);
    Ok(crate::features::formatting::format_document(
        file_type, &content,
    ))
}

pub async fn handle_range_formatting(
    server: &GgenLanguageServer, params: DocumentRangeFormattingParams,
) -> Result<Option<Vec<TextEdit>>> {
    let uri = &params.text_document.uri;
    let Some(content) = server.state.get_document(uri).await else {
        return Ok(None);
    };
    let file_type = crate::state::FileType::from_uri(uri);
    Ok(crate::features::formatting::format_range(
        file_type,
        &content,
        params.range,
    ))
}
