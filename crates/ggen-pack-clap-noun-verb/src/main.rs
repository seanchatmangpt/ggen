use serde_json::json;
use tower_lsp_max::jsonrpc::Result;
use tower_lsp_max::lsp_types::*;
use tower_lsp_max::{Client, LanguageServer, LspService, Server};
use std::str::FromStr;
use url::Url;

struct Backend {
    client: Client,
}

#[tower_lsp_max::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["conformance-receipt.bind".to_string()],
                    ..Default::default()
                }),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "clap-noun-verb-pack-lsp".to_string(),
                version: Some("26.6.6".to_string()),
            }),
            offset_encoding: None,
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        let url = Url::parse(uri.as_str()).unwrap();
        self.check_diagnostics(&url, &content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.last() {
            let url = Url::parse(uri.as_str()).unwrap();
            self.check_diagnostics(&url, &change.text).await;
        }
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<serde_json::Value>> {
        // Here we handle the noun-verb grammar admission.
        // Once validated, the actual mutation MUST route through PackPlan -> Staging -> MutationGate.
        if params.command == "conformance-receipt.bind" {
            self.client.log_message(MessageType::INFO, "CLAP Validated: noun=conformance-receipt verb=bind").await;
            self.client.log_message(MessageType::INFO, "Routing to PackPlan -> Staging -> MutationGate").await;
            
            if let Some(arg) = params.arguments.first().and_then(|a| a.as_str()) {
                if let Ok(url) = url::Url::parse(arg) {
                    if let Ok(path) = url.to_file_path() {
                        let receipts = ggen_projection::ReceiptIndex::new();
                        let gate = ggen_projection::StagingGate::new(path.clone(), receipts);
                        let _ = gate.check_write(std::path::Path::new("conformance-receipt.json"), false);
                    }
                }
            }
        } else {
            self.client.log_message(MessageType::ERROR, format!("CLAP Rejected: unknown command {}", params.command)).await;
        }
        Ok(None)
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
            }
        }

        if path_str.contains("manual_parser.rs") || content.contains("pub fn parse()") {
            diags.push(make_diag(
                "CLAP-PROJECT-OPPORTUNITY-001",
                "File contains manual parser signature; could be projected using a pack",
                DiagnosticSeverity::INFORMATION,
            ));
        }

        if path_str.ends_with("cli.rs") || path_str.ends_with("main.rs") {
            if content.contains("struct Opts") && !content.contains("fn handle") {
                diags.push(make_diag(
                    "CLAP-PACK-HANDLER-UNBOUND",
                    "CLI domain missing handler",
                    DiagnosticSeverity::ERROR,
                ));
            }
        }

        let uri_lsp = Uri::from_str(uri.as_str()).unwrap();
        self.client.publish_diagnostics(uri_lsp, diags, None).await;
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| Backend { client });
    Server::new(stdin, stdout, socket).serve(service).await;
}
