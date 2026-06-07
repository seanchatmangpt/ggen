//! Playground LSP backend for manual integration testing of tower-lsp-max.
//!
//! Implements a minimal `LanguageServer` (document sync, hover, diagnostics)
//! against an in-memory rope store; useful for verifying runtime behaviour
//! without a production language server.

#![allow(clippy::mutable_key_type)]

pub mod capabilities;
pub mod handlers;

use std::sync::Arc;

use dashmap::DashMap;
use ropey::Rope;
use tower_lsp_max::jsonrpc::Result;
use tower_lsp_max::lsp_types::*;
use tower_lsp_max::{Client, LanguageServer};

/// One entry per open document.
pub struct Document {
    pub text: Rope,
    pub version: i32,
    pub language_id: String,
}

/// The playground backend.  All handler state lives here.
pub struct Backend {
    pub client: Client,
    /// Open documents indexed by URI.
    pub docs: Arc<DashMap<Uri, Document>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            docs: Arc::new(DashMap::new()),
        }
    }
}

#[tower_lsp_max::async_trait]
impl LanguageServer for Backend {
    // --- LIFECYCLE ---

    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: capabilities::server_capabilities(),
            server_info: Some(ServerInfo {
                name: "tower-lsp-max-playground".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "tower-lsp-max playground ready")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    // --- DOCUMENT SYNCHRONIZATION ---

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let rope = Rope::from_str(&params.text_document.text);
        self.docs.insert(
            params.text_document.uri.clone(),
            Document {
                text: rope,
                version: params.text_document.version,
                language_id: params.text_document.language_id,
            },
        );
        self.publish_diagnostics(params.text_document.uri).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        if let Some(mut doc) = self.docs.get_mut(&params.text_document.uri) {
            for change in params.content_changes {
                match change.range {
                    None => {
                        // Full sync
                        doc.text = Rope::from_str(&change.text);
                    }
                    Some(range) => {
                        // Incremental sync: apply range edit to rope
                        let start = rope_offset(&doc.text, range.start);
                        let end = rope_offset(&doc.text, range.end);
                        doc.text.remove(start..end);
                        doc.text.insert(start, &change.text);
                    }
                }
            }
            doc.version = params.text_document.version;
        }
        self.publish_diagnostics(params.text_document.uri).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.docs.remove(&params.text_document.uri);
        // Clear diagnostics so stale markers don't persist after file is closed
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.publish_diagnostics(params.text_document.uri).await;
    }

    // --- COMPLETIONS ---

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        Ok(handlers::completions::compute(self, uri, pos).await)
    }

    // --- HOVER ---

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        Ok(handlers::hover::compute(self, uri, pos).await)
    }

    // --- CODE ACTIONS ---

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        Ok(handlers::diagnostics::code_actions(self, uri, &params.context.diagnostics).await)
    }
}

impl Backend {
    /// Re-runs the diagnostic engine and publishes results via the client.
    pub async fn publish_diagnostics(&self, uri: Uri) {
        let diags = handlers::diagnostics::compute(self, &uri).await;
        self.client.publish_diagnostics(uri, diags, None).await;
    }
}

/// Convert an LSP Position (line/character) to a char offset in a Rope.
fn rope_offset(rope: &Rope, pos: Position) -> usize {
    if rope.len_lines() == 0 {
        return 0;
    }
    let line_idx = (pos.line as usize).min(rope.len_lines().saturating_sub(1));
    let line_start = rope.line_to_char(line_idx);
    let line_len = rope.line(line_idx).len_chars();
    let char_idx = (pos.character as usize).min(line_len);
    line_start + char_idx
}
