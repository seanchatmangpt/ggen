use serde_json::json;
use tower_lsp_max::jsonrpc::Result;
use tower_lsp_max::lsp_types::*;
use tower_lsp_max::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
}

#[tower_lsp_max::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "tower-lsp-max-pack-lsp".to_string(),
                version: Some("26.6.6".to_string()),
            }),
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        self.check_diagnostics(&uri, &content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.last() {
            self.check_diagnostics(&uri, &change.text).await;
        }
    }
}

impl Backend {
    async fn check_diagnostics(&self, uri: &Url, content: &str) {
        let mut diags = Vec::new();
        let path_str = uri.path();

        let make_diag = |code: &str, msg: &str, severity: DiagnosticSeverity| -> Diagnostic {
            let mut d = Diagnostic {
                range: Range::default(),
                severity: Some(severity),
                code: Some(NumberOrString::String(code.to_string())),
                source: Some("tower-lsp-max-pack-lsp".to_string()),
                message: msg.to_string(),
                ..Default::default()
            };
            d.data = Some(json!({ "source_id": "tower_lsp_max_pack_lsp" }));
            d
        };

        if path_str.ends_with("pack.toml") {
            if content.contains("no_sigs") {
                diags.push(make_diag(
                    "TOWER-PROJECT-OPPORTUNITY-001",
                    "Pack descriptor is missing signatures",
                    DiagnosticSeverity::INFORMATION,
                ));
            }
        }

        self.client.publish_diagnostics(uri.clone(), diags, None).await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}