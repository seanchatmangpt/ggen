use lsp_max::lsp_types_max::*;
use lsp_max::{jsonrpc::Result, Client, LanguageServer};
use std::sync::Arc;

use crate::handlers;
use crate::state::ServerState;

pub struct GgenLanguageServer {
    pub(crate) state: Arc<ServerState>,
    pub(crate) client: Client,
}

impl GgenLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            state: Arc::new(ServerState::default()),
            client,
        }
    }

    /// Build (or rebuild) the analyzer for a document, store it, and publish its
    /// diagnostics — the live "refusal before execution" signal.
    async fn refresh_analyzer(&self, uri: &Url, content: &str) {
        for (target_uri, diagnostics) in self.state.analyze_and_observe(uri, content).await {
            // Task C — mirror diagnostics into lsp-max REGISTRY for the autonomic mesh.
            push_diagnostics_to_registry(&diagnostics);
            let lsp_diagnostics = diagnostics.into_iter().map(|d| d.lsp).collect();
            self.client
                .publish_diagnostics(target_uri, lsp_diagnostics, None)
                .await;
        }
    }
}

#[lsp_max::async_trait]
impl LanguageServer for GgenLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Task A — prime REGISTRY (set root_path) and MESH via lsp-max surfaces.
        #[allow(deprecated)]
        let root = params
            .root_uri
            .as_ref()
            .and_then(|u| url::Url::parse(u.as_str()).ok()?.to_file_path().ok())
            .unwrap_or_default();

        if let Ok(mut reg) = lsp_max::get_registry().lock() {
            reg.root_path = root;
        }

        lsp_max::MESH
            .get_or_init(|| std::sync::Mutex::new(lsp_max::max_runtime::AutonomicMesh::new()));

        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![
                        ":".to_string(),
                        "@".to_string(),
                        ".".to_string(),
                        "{".to_string(),
                        "[".to_string(),
                        "\"".to_string(),
                        "|".to_string(),
                    ]),
                    ..Default::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                })),
                document_symbol_provider: Some(OneOf::Left(true)),
                code_action_provider: Some(CodeActionProviderCapability::Options(
                    CodeActionOptions {
                        code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
                        resolve_provider: Some(false),
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: SemanticTokensLegend {
                                token_types: vec![
                                    SemanticTokenType::NAMESPACE,
                                    SemanticTokenType::CLASS,
                                    SemanticTokenType::PROPERTY,
                                    SemanticTokenType::VARIABLE,
                                    SemanticTokenType::KEYWORD,
                                    SemanticTokenType::STRING,
                                    SemanticTokenType::NUMBER,
                                    SemanticTokenType::COMMENT,
                                    SemanticTokenType::FUNCTION,
                                ],
                                token_modifiers: vec![],
                            },
                            range: None,
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            ..Default::default()
                        },
                    ),
                ),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "ggen-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            ..Default::default()
        })
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let content = params.text_document.text;
        self.state.set_document(uri.clone(), content.clone()).await;
        self.refresh_analyzer(&uri, &content).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.state
                .set_document(uri.clone(), change.text.clone())
                .await;
            self.refresh_analyzer(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        for (target_uri, diagnostics) in self.state.close_document(&uri).await {
            push_diagnostics_to_registry(&diagnostics);
            let lsp_diagnostics = diagnostics.into_iter().map(|d| d.lsp).collect();
            self.client
                .publish_diagnostics(target_uri, lsp_diagnostics, None)
                .await;
        }
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        handlers::completion::handle(self, params).await
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        handlers::hover::handle(self, params).await
    }

    async fn goto_definition(
        &self, params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        handlers::definition::handle(self, params).await
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        handlers::references::handle(self, params).await
    }

    async fn document_symbol(
        &self, params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        handlers::document_symbol::handle(self, params).await
    }

    async fn semantic_tokens_full(
        &self, params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        handlers::semantic_tokens::handle_full(self, params).await
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        handlers::folding_range::handle(self, params).await
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        handlers::formatting::handle_formatting(self, params).await
    }

    async fn range_formatting(
        &self, params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        handlers::formatting::handle_range_formatting(self, params).await
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        handlers::inlay_hint::handle(self, params).await
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        handlers::code_lens::handle(self, params).await
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        handlers::diagnostics::handle_code_action(self, params).await
    }

    async fn prepare_rename(
        &self, params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        handlers::rename::handle_prepare(self, params).await
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        handlers::rename::handle(self, params).await
    }

    async fn prepare_call_hierarchy(
        &self, params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        handlers::call_hierarchy::handle_prepare(self, params).await
    }

    async fn prepare_type_hierarchy(
        &self, params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        handlers::type_hierarchy::handle_prepare(self, params).await
    }

    async fn symbol(
        &self, params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        handlers::workspace_symbol::handle(self, params).await
    }
}

/// Task C helper — push GGEN-* diagnostics into the lsp-max REGISTRY so the
/// autonomic mesh can observe them. Best-effort: lock failures are silently ignored.
fn push_diagnostics_to_registry(diagnostics: &[lsp_max_protocol::MaxDiagnostic]) {
    use lsp_max_protocol::{LawAxis, MaxDiagnostic};
    if let Ok(mut reg) = lsp_max::get_registry().lock() {
        for diag in diagnostics {
            if let Some(NumberOrString::String(ref code)) = diag.lsp.code {
                let id = {
                    let mut h = blake3::Hasher::new();
                    h.update(diag.lsp.message.as_bytes());
                    format!("{}-{:.8}", code, h.finalize().to_hex())
                };
                let mut max_diag = diag.clone();
                max_diag.diagnostic_id = id.clone();
                max_diag.law_id = code.clone();
                max_diag.law_axis = LawAxis::Domain;
                max_diag.violated_invariant = diag.lsp.message.clone();
                reg.diagnostics.insert(id, max_diag);
            }
        }
    }
}

pub fn range_contains(range: &Range, position: Position) -> bool {
    let after_start = position.line > range.start.line
        || (position.line == range.start.line && position.character >= range.start.character);
    let before_end = position.line < range.end.line
        || (position.line == range.end.line && position.character <= range.end.character);
    after_start && before_end
}
