use lsp_max::lsp_types_max::*;
use lsp_max::jsonrpc::Result;
use crate::server::GgenLanguageServer;

pub async fn handle(server: &GgenLanguageServer, params: WorkspaceSymbolParams) -> Result<Option<Vec<SymbolInformation>>> {
    let root = server.state.root.clone();
    Ok(Some(crate::features::workspace_symbol::workspace_symbols(&root, &params.query)))
}
