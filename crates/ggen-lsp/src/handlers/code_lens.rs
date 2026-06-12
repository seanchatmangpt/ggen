use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle(server: &GgenLanguageServer, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
    let uri = &params.text_document.uri;
    let file_type = crate::state::FileType::from_uri(uri);
    let Some(content) = server.state.get_document(uri).await else {
        return Ok(None);
    };
    Ok(crate::features::code_lens::code_lenses(file_type, &content))
}
