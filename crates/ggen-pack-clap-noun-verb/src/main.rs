use serde_json::json;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

struct Backend {
    client: Client,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "clap-noun-verb-pack-lsp".to_string(),
                version: Some("1.0.0".to_string()),
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
                source: Some("clap-noun-verb-pack-lsp".to_string()),
                message: msg.to_string(),
                ..Default::default()
            };
            d.data = Some(json!({ "source_id": "clap_noun_verb_pack_lsp" }));
            d
        };

        if path_str.ends_with("customization-map.json") {
            if content.is_empty() || content.trim() == "{}" || content.contains("TODO") {
                diags.push(make_diag(
                    "CLAP-CUSTOMIZE-001",
                    "Required customization point incomplete",
                    DiagnosticSeverity::WARNING,
                ));
            } else {
                diags.push(make_diag(
                    "CLAP-DEBUG",
                    &format!("Content was: '{}'", content),
                    DiagnosticSeverity::WARNING,
                ));
            }
        } else {
            diags.push(make_diag(
                "CLAP-DEBUG-PATH",
                &format!("Path was: '{}'", path_str),
                DiagnosticSeverity::WARNING,
            ));
        }

        if path_str.contains("manual_parser.rs") || content.contains("pub fn parse()") {
            diags.push(make_diag(
                "CLAP-PROJECT-OPPORTUNITY-001",
                "File contains manual parser signature; could be projected using a pack",
                DiagnosticSeverity::INFORMATION,
            ));
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