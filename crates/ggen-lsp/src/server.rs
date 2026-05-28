use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tower_lsp::{jsonrpc::Result, Client, LanguageServer};

use crate::state::ServerState;

pub struct GgenLanguageServer {
    state: Arc<ServerState>,
    client: Client,
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
        if let Some(analyzer) = crate::analyzers::build_analyzer(uri.path(), content) {
            let diagnostics = analyzer.diagnostics();
            self.state.set_analyzer(uri.clone(), analyzer).await;
            // Record the editor-flow OCEL chain (raise / route / rework-closure)
            // before publishing — applied repairs become observable to mining.
            self.state.observe_diagnostics(uri, &diagnostics).await;
            self.client
                .publish_diagnostics(uri.clone(), diagnostics, None)
                .await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for GgenLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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
                workspace_symbol_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                code_action_provider: Some(CodeActionProviderCapability::Options(
                    CodeActionOptions {
                        code_action_kinds: Some(vec![CodeActionKind::QUICKFIX]),
                        resolve_provider: Some(false),
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                    },
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
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
                            token_modifiers: vec![
                                SemanticTokenModifier::DECLARATION,
                                SemanticTokenModifier::DEFINITION,
                                SemanticTokenModifier::READONLY,
                                SemanticTokenModifier::DEPRECATED,
                            ],
                        },
                        range: Some(true),
                        full: Some(SemanticTokensFullOptions::Bool(true)),
                        ..Default::default()
                    }),
                ),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                document_range_formatting_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "ggen-lsp".to_string(),
                version: Some("26.5.21".to_string()),
            }),
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
        // FULL sync: the last change carries the entire document text.
        if let Some(change) = params.content_changes.into_iter().last() {
            self.state.set_document(uri.clone(), change.text.clone()).await;
            self.refresh_analyzer(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.state.remove_document(&params.text_document.uri).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.completion_at(position.line, position.character)))
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let analyzer = self.state.get_analyzer(uri).await;
        // Semantic hover (e.g. SHACL shape, term kind) takes priority.
        if let Some(h) = analyzer
            .as_ref()
            .and_then(|a| a.hover_at(position.line, position.character))
        {
            return Ok(Some(h));
        }
        // Otherwise, if a diagnostic covers the cursor, show its OCEL-TON card:
        // the active episode + route + next transition — the author-time receipt
        // preview. Diagnostics are recomputed here (cheap, off the publish path).
        let (Some(analyzer), Some(content)) =
            (analyzer, self.state.get_document(uri).await)
        else {
            return Ok(None);
        };
        let registry = self.state.routes.clone();
        for d in analyzer.diagnostics() {
            if range_contains(&d.range, position) {
                if let Some(plan) =
                    crate::route::route_plan_for_diagnostic(&registry, &d, &content)
                {
                    let view = crate::route::CompactTraceView::from_route_plan(&plan, uri.path());
                    return Ok(Some(Hover {
                        contents: HoverContents::Markup(MarkupContent {
                            kind: MarkupKind::Markdown,
                            value: view.to_hover_markdown(),
                        }),
                        range: Some(d.range),
                    }));
                }
            }
        }
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.definition_at(position.line, position.character))
            .map(GotoDefinitionResponse::Scalar))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.references_at(position.line, position.character)))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = &params.text_document.uri;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.document_symbols())
            .map(DocumentSymbolResponse::Nested))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.semantic_tokens())
            .map(SemanticTokensResult::Tokens))
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = &params.text_document.uri;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.folding_ranges()))
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.format_document()))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.inlay_hints()))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.code_lenses()))
    }

    /// Project repair routes as QUICKFIX code actions. A CodeAction here IS the
    /// process transition that repairs the failed transition (the diagnostic):
    /// for each in-context diagnostic, select its precomputed route and offer the
    /// concrete `WorkspaceEdit`. Only routes with a computable edit (no unfilled
    /// `{placeholder}`) are offered; advisory/NoOp routes are silent.
    async fn code_action(
        &self,
        params: CodeActionParams,
    ) -> Result<Option<CodeActionResponse>> {
        let uri = &params.text_document.uri;
        let doc = self.state.get_document(uri).await.unwrap_or_default();
        let registry = self.state.routes.clone();

        let mut actions: Vec<CodeActionOrCommand> = Vec::new();
        for d in &params.context.diagnostics {
            let Some(route) = crate::route::action_route_for(&registry, d) else {
                continue;
            };
            // Bindings computable from the diagnostic alone: its range is the site.
            let bindings = crate::route::RouteBindings {
                site: Some(d.range),
                ..Default::default()
            };
            let edit = crate::route::workspace_edit_from_route(route, &bindings, uri, &doc);
            let has_real_edit = edit
                .changes
                .as_ref()
                .is_some_and(|c| c.values().flatten().any(|e| !e.new_text.contains('{')));
            if !has_real_edit {
                continue; // unfilled template or advisory-only route
            }
            // Route-native title: a CodeAction IS the next admissible transition,
            // not a generic "quick fix".
            let title = route
                .steps
                .nodes
                .first()
                .map(|s| format!("Apply transition: {}", s.title))
                .unwrap_or_else(|| route.description.clone());
            // Carry the canonical RouteEnvelope in `data` — the SAME shape the
            // headless gate, MCP tool, and A2A bridge project for this diagnostic.
            let data = crate::route::envelope_for_diagnostic(&registry, d, &doc, uri.path())
                .and_then(|env| serde_json::to_value(env).ok());
            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                title,
                kind: Some(CodeActionKind::QUICKFIX),
                diagnostics: Some(vec![d.clone()]),
                edit: Some(edit),
                command: None,
                is_preferred: Some(true),
                disabled: None,
                data,
            }));
        }
        Ok(if actions.is_empty() {
            None
        } else {
            Some(actions)
        })
    }

    async fn prepare_rename(
        &self,
        params: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        let uri = &params.text_document.uri;
        let position = params.position;
        if self.state.get_analyzer(uri).await.is_some() {
            Ok(Some(PrepareRenameResponse::Range(Range {
                start: position,
                end: position,
            })))
        } else {
            Ok(None)
        }
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        let uri = &params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.rename_symbol(position, &new_name)))
    }

    async fn prepare_call_hierarchy(
        &self,
        params: CallHierarchyPrepareParams,
    ) -> Result<Option<Vec<CallHierarchyItem>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.call_hierarchy_items(position)))
    }

    async fn prepare_type_hierarchy(
        &self,
        params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.type_hierarchy_items(position)))
    }
}

/// True if `position` falls within `range` (inclusive of start, exclusive of end
/// line/char in the usual LSP sense; whole-line ranges with `u32::MAX` end cover
/// the line).
fn range_contains(range: &Range, position: Position) -> bool {
    let after_start = position.line > range.start.line
        || (position.line == range.start.line && position.character >= range.start.character);
    let before_end = position.line < range.end.line
        || (position.line == range.end.line && position.character <= range.end.character);
    after_start && before_end
}
