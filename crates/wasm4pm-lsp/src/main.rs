use serde_json::json;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};
use std::process::{Command, Stdio};
use std::io::Write;

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
                name: "wasm4pm-lsp".to_string(),
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
                source: Some("wasm4pm-lsp".to_string()),
                message: msg.to_string(),
                ..Default::default()
            };
            d.data = Some(json!({ "source_id": "wasm4pm_lsp" }));
            d
        };

        // Observe process evidence
        if path_str.ends_with(".jsonl") || path_str.ends_with(".json") || path_str.ends_with(".ocel") {
            let mut bin_path = std::env::current_exe().unwrap_or_else(|_| std::path::PathBuf::from("."));
            bin_path.pop(); // pop binary name
            let mut compat_bin = bin_path.join("gc005-wasm4pm-adapter");
            if !compat_bin.exists() && bin_path.ends_with("deps") {
                bin_path.pop(); // pop "deps"
                compat_bin = bin_path.join("gc005-wasm4pm-adapter");
            }
            
            let child_res = Command::new(&compat_bin)
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn();

            match child_res {
                Ok(mut child) => {
                    if let Some(mut stdin) = child.stdin.take() {
                        let _ = stdin.write_all(content.as_bytes());
                    }

                    if let Ok(output) = child.wait_with_output() {
                        if let Ok(res) = serde_json::from_slice::<serde_json::Value>(&output.stdout) {
                            if let Some(issues) = res.get("issues").and_then(|i| i.as_array()) {
                                for issue in issues {
                                    let code = issue.get("code").and_then(|c| c.as_str()).unwrap_or("UNKNOWN");
                                    let message = issue.get("message").and_then(|m| m.as_str()).unwrap_or("");
                                    
                                    let severity = match code {
                                        c if c.contains("INVALID") || c.contains("BROKEN") || c.contains("MISSING") || c.contains("BLOCKED") => DiagnosticSeverity::ERROR,
                                        c if c.contains("DEVIATION") || c.contains("UNKNOWN") => DiagnosticSeverity::WARNING,
                                        _ => DiagnosticSeverity::INFORMATION,
                                    };
                                    
                                    diags.push(make_diag(code, message, severity));
                                }
                            }
                        }
                    }
                }
                Err(e) => {
                    diags.push(make_diag("WASM4PM-INTERNAL-ERROR", &format!("Failed to spawn {}: {}", compat_bin.display(), e), DiagnosticSeverity::ERROR));
                }
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