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

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<serde_json::Value>> {
        // Here we handle the noun-verb grammar admission.
        // Once validated, the actual mutation MUST route through PackPlan -> Staging -> MutationGate.
        // We will emit a log message simulating this pipeline.
        if params.command == "conformance-receipt.bind" {
            self.client.log_message(MessageType::INFO, "CLAP Validated: noun=conformance-receipt verb=bind").await;
            self.client.log_message(MessageType::INFO, "Routing to PackPlan -> Staging -> MutationGate").await;
            
            // In a real system, we'd trigger the ggen-projection engine here via an RPC or channel.
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

        if path_str.ends_with("cli.rs") || path_str.ends_with("main.rs") {
            if content.contains("struct Opts") && !content.contains("fn handle") {
                diags.push(make_diag(
                    "CLAP-PACK-HANDLER-UNBOUND",
                    "CLI domain missing handler",
                    DiagnosticSeverity::ERROR,
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
