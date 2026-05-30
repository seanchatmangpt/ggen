use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tower_lsp::lsp_types::*;
use tower_lsp::{jsonrpc::Result, Client, LanguageServer};

use crate::state::{FileType, ServerState};

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
    ///
    /// For rule-referenced surfaces (`.tera`, `.rq`/`.sparql`, `ggen.toml`) the
    /// single-file diagnostics are augmented with the CROSS-FILE GGEN-TPL-001
    /// analysis: a [`ProjectIndex`](crate::project_index::ProjectIndex) is built
    /// from the nearest `ggen.toml` and run through
    /// [`detect_tpl_001`](crate::analyzers::detect_tpl_001). The edited template's
    /// own single-file diagnostics are MERGED with its GGEN-TPL-001 diagnostics
    /// and published ONCE for that URI (LSP diagnostics are replace-semantics per
    /// URI). Every publish — including the cross-file ones — flows through
    /// `observe_diagnostics` so the living OCEL loop records them.
    ///
    /// Strictly read-only: builds an index (which only reads files) and never
    /// writes any artifact.
    async fn refresh_analyzer(&self, uri: &Url, content: &str) {
        let file_type = FileType::from_path(uri.path());

        // Single-file diagnostics for the edited document (E0024 etc.).
        let mut own_diags: Vec<Diagnostic> = Vec::new();
        if let Some(analyzer) = crate::analyzers::build_analyzer(uri.path(), content) {
            own_diags = analyzer.diagnostics();
            self.state.set_analyzer(uri.clone(), analyzer).await;
        }

        // Cross-file GGEN-TPL-001: only for rule-referenced surfaces, and only
        // when we can resolve a project root with a real `ggen.toml`.
        let tpl_groups = if matches!(
            file_type,
            FileType::Tera | FileType::Sparql | FileType::Toml
        ) {
            self.detect_tpl_001_for(uri)
        } else {
            Vec::new()
        };

        // Convert the edited document's path to a Url so we can recognize when one
        // of the cross-file groups targets the very file being edited (and must be
        // merged into a single publish rather than clobbering it).
        let edited_template_url = url_from_path_str(uri.path());

        // Publish GGEN-TPL-001 for every AFFECTED template, merging the edited
        // file's own diagnostics into the same publish when it is itself the
        // template. All publishes pass through `observe_diagnostics`.
        let mut published_self = false;
        // Template URIs flagged THIS pass — used to reconcile against the previous
        // pass so cross-surface repairs clear stale diagnostics (see below).
        let mut current_flagged: HashSet<Url> = HashSet::new();
        for (template_path, tpl_diags) in tpl_groups {
            let Some(template_url) = url_from_path(&template_path) else {
                continue;
            };
            current_flagged.insert(template_url.clone());
            if edited_template_url.as_ref() == Some(&template_url) {
                // The edited file IS this template: merge once, publish once.
                let mut merged = std::mem::take(&mut own_diags);
                merged.extend(tpl_diags);
                self.publish_observed(&template_url, merged).await;
                published_self = true;
            } else {
                // A different (cross-file) template URI. Route through
                // `observe_diagnostics` (it is `pub` and takes `&uri`).
                self.publish_observed(&template_url, tpl_diags).await;
            }
        }

        // If the edited file was not itself a TPL-001-affected template, publish
        // its own single-file diagnostics (preserving the original single-file
        // flow). `.rq`/`ggen.toml` always land here for their own URI.
        if !published_self {
            self.publish_observed(uri, own_diags).await;
        }

        // STALE-CLEAR reconciliation. A cross-surface repair (e.g. adding the
        // missing variable to the SPARQL `SELECT`) makes a *different* template
        // lawful, so that template drops out of `tpl_groups` entirely and the
        // loop above never touches its URI. Without this, its squiggle would
        // linger forever and the repair would never be observed. Only runs when
        // the edited file is itself a rule surface (so we actually recomputed the
        // project graph this pass); editing an unrelated file must NOT clear
        // standing diagnostics.
        //
        // Each clear republishes the cleared template's RESIDUAL single-file
        // diagnostics (its own E0024 etc.), NOT an empty set — a template may
        // carry independent diagnostics alongside the now-repaired GGEN-TPL-001,
        // and a clear must not erase unrelated law. The single-file analyzer runs
        // with empty bindings, so the residual never contains GGEN-TPL-001. The
        // publish flows through `observe_diagnostics`, whose per-key (code|span)
        // diff drops exactly the disappeared GGEN-TPL-001 key — closing the
        // living loop (RepairApplied → GatePassed → ReceiptEmitted) — while
        // preserving every still-present key. A clear is an event, not an absence.
        if matches!(
            file_type,
            FileType::Tera | FileType::Sparql | FileType::Toml
        ) {
            for cleared in self.state.tpl_clears_for(uri, &current_flagged).await {
                let residual = self.residual_single_file_diags(&cleared).await;
                self.publish_observed(&cleared, residual).await;
            }
        }
    }

    /// Recompute a template's OWN single-file diagnostics (E0024 etc.) for use
    /// when clearing a disappeared cross-surface GGEN-TPL-001. The single-file
    /// Tera analyzer runs with empty SPARQL bindings, so the result NEVER
    /// contains GGEN-TPL-001 — publishing it through `observe_diagnostics`
    /// therefore drops only the disappeared GGEN-TPL-001 key while preserving any
    /// independent diagnostics on the same template. Content comes from the open
    /// document if available, else the file on disk. Read-only; never writes.
    async fn residual_single_file_diags(&self, uri: &Url) -> Vec<Diagnostic> {
        let content = match self.state.get_document(uri).await {
            Some(c) => c,
            None => match uri.to_file_path() {
                Ok(path) => std::fs::read_to_string(&path).unwrap_or_default(),
                Err(()) => String::new(),
            },
        };
        crate::analyzers::build_analyzer(uri.path(), &content)
            .map(|a| a.diagnostics())
            .unwrap_or_default()
    }

    /// Publish `diagnostics` for `uri` through the observed path: record the
    /// editor-flow OCEL chain via `observe_diagnostics`, then publish to the
    /// client. Centralizes the "never bypass observe_diagnostics" invariant.
    async fn publish_observed(&self, uri: &Url, diagnostics: Vec<Diagnostic>) {
        self.state.observe_diagnostics(uri, &diagnostics).await;
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }

    /// Resolve the project root for `uri`, build a `ProjectIndex`, and run the
    /// cross-surface GGEN-TPL-001 detector. Returns the per-template diagnostic
    /// groups (empty on any resolution/build failure — best-effort, never panics,
    /// never writes files).
    ///
    /// Bounded but uncached: an index is built on every change. Acceptable for the
    /// MVP; see handoff for the caching note.
    fn detect_tpl_001_for(&self, uri: &Url) -> Vec<(PathBuf, Vec<Diagnostic>)> {
        let Some(root) = self.project_root_for(uri) else {
            return Vec::new();
        };
        match crate::project_index::ProjectIndex::from_root(&root) {
            Ok(project) => crate::analyzers::detect_tpl_001(&project),
            Err(_) => Vec::new(),
        }
    }

    /// Find the project root for a document: walk up parent directories from the
    /// file path to the nearest directory containing a `ggen.toml`. Falls back to
    /// `self.state.root` when the URI is not a local file path or no manifest is
    /// found above it.
    fn project_root_for(&self, uri: &Url) -> Option<PathBuf> {
        if let Ok(file_path) = uri.to_file_path() {
            let mut dir: Option<&Path> = file_path.parent();
            while let Some(d) = dir {
                if d.join("ggen.toml").is_file() {
                    return Some(d.to_path_buf());
                }
                dir = d.parent();
            }
        }
        // Fallback: the server's configured root, if it holds a manifest.
        let fallback = self.state.root.clone();
        if fallback.join("ggen.toml").is_file() {
            Some(fallback)
        } else {
            None
        }
    }
}

/// Convert a filesystem path to a `file://` `Url`, or `None` if it is not an
/// absolute path `Url::from_file_path` accepts.
fn url_from_path(path: &Path) -> Option<Url> {
    Url::from_file_path(path).ok()
}

/// Convert a `Url::path()` string back to a `file://` `Url`. `Url::path()` yields
/// an absolute, percent-encoded path; round-tripping it lets us compare a document
/// URI against the `template_path` reported by the detector.
fn url_from_path_str(path: &str) -> Option<Url> {
    let decoded = percent_decode_path(path);
    Url::from_file_path(&decoded).ok()
}

/// Minimal percent-decoding for the `%XX` sequences `Url::path()` may contain
/// (e.g. spaces). Avoids pulling in an extra dependency for the common cases.
fn percent_decode_path(path: &str) -> String {
    let bytes = path.as_bytes();
    let mut out: Vec<u8> = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            let hi = (bytes[i + 1] as char).to_digit(16);
            let lo = (bytes[i + 2] as char).to_digit(16);
            if let (Some(hi), Some(lo)) = (hi, lo) {
                out.push((hi * 16 + lo) as u8);
                i += 3;
                continue;
            }
        }
        out.push(bytes[i]);
        i += 1;
    }
    String::from_utf8_lossy(&out).into_owned()
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
                // Re-declared because the features/ modules now genuinely deliver
                // these (advertised == delivered). semantic_tokens advertises only
                // `full` (no `range`) since only full tokenization is implemented.
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
                version: Some("26.5.28".to_string()),
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
            self.state
                .set_document(uri.clone(), change.text.clone())
                .await;
            self.refresh_analyzer(&uri, &change.text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.state.remove_document(&uri).await;
        // LSP-conformant clear: publish an EMPTY diagnostics list for this URI so
        // stale squiggles do not linger in the client after the document closes.
        // Reuse the SAME publish path `did_open`/`did_change` use (the `Client`
        // handle), so server state and editor diagnostics agree after close. Safe
        // even if the doc was never opened — publishing an empty set is idempotent
        // and never panics.
        self.client.publish_diagnostics(uri, Vec::new(), None).await;
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
        let (Some(analyzer), Some(content)) = (analyzer, self.state.get_document(uri).await) else {
            return Ok(None);
        };
        let registry = self.state.routes.clone();
        for d in analyzer.diagnostics() {
            if range_contains(&d.range, position) {
                if let Some(plan) = crate::route::route_plan_for_diagnostic(&registry, &d, &content)
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
        &self, params: GotoDefinitionParams,
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
        &self, params: DocumentSymbolParams,
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
        &self, params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = &params.text_document.uri;
        let file_type = crate::state::FileType::from_uri(uri);
        Ok(self
            .state
            .get_document(uri)
            .await
            .and_then(|content| {
                crate::features::semantic_tokens::semantic_tokens(file_type, &content)
            })
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
        let Some(content) = self.state.get_document(uri).await else {
            return Ok(None);
        };
        let file_type = crate::state::FileType::from_uri(uri);
        Ok(crate::features::formatting::format_document(
            file_type, &content,
        ))
    }

    async fn range_formatting(
        &self, params: DocumentRangeFormattingParams,
    ) -> Result<Option<Vec<TextEdit>>> {
        let uri = &params.text_document.uri;
        let Some(content) = self.state.get_document(uri).await else {
            return Ok(None);
        };
        let file_type = crate::state::FileType::from_uri(uri);
        Ok(crate::features::formatting::format_range(
            file_type,
            &content,
            params.range,
        ))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = &params.text_document.uri;
        let Some(content) = self.state.get_document(uri).await else {
            return Ok(None);
        };
        let file_type = crate::state::FileType::from_uri(uri);
        Ok(crate::features::inlay_hint::inlay_hints(
            file_type,
            &content,
            params.range,
        ))
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let uri = &params.text_document.uri;
        let file_type = crate::state::FileType::from_uri(uri);
        let Some(content) = self.state.get_document(uri).await else {
            return Ok(None);
        };
        Ok(crate::features::code_lens::code_lenses(file_type, &content))
    }

    /// Project repair routes as QUICKFIX code actions. A CodeAction here IS the
    /// process transition that repairs the failed transition (the diagnostic):
    /// for each in-context diagnostic, select its precomputed route and offer the
    /// concrete `WorkspaceEdit`. Only routes with a computable edit (no unfilled
    /// `{placeholder}`) are offered; advisory/NoOp routes are silent.
    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
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
        &self, params: TextDocumentPositionParams,
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
        &self, params: CallHierarchyPrepareParams,
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
        &self, params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        let uri = &params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        Ok(self
            .state
            .get_analyzer(uri)
            .await
            .and_then(|a| a.type_hierarchy_items(position)))
    }

    async fn symbol(
        &self, params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        let root = self.state.root.clone();
        Ok(Some(crate::features::workspace_symbol::workspace_symbols(
            &root,
            &params.query,
        )))
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
