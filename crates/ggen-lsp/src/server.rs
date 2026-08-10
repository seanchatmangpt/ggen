use lsp_max::lsp_types_max::*;
use lsp_max::{jsonrpc::Result, Client, LanguageServer};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::sync::Mutex;

use crate::handlers;
use crate::project_index::BufferOverlay;
use crate::state::ServerState;

#[derive(Default)]
struct RegistryDocumentState {
    diagnostic_ids: HashSet<String>,
    gating_ids: HashSet<String>,
}

pub struct GgenLanguageServer {
    pub(crate) state: Arc<ServerState>,
    pub(crate) client: Client,
    registry_documents: Arc<Mutex<HashMap<Url, RegistryDocumentState>>>,
    hierarchy_registrations: Arc<Mutex<Vec<Registration>>>,
    source_contract_flagged: Arc<Mutex<HashSet<Url>>>,
}

impl GgenLanguageServer {
    pub fn new(client: Client) -> Self {
        Self {
            state: Arc::new(ServerState::default()),
            client,
            registry_documents: Arc::new(Mutex::new(HashMap::new())),
            hierarchy_registrations: Arc::new(Mutex::new(Vec::new())),
            source_contract_flagged: Arc::new(Mutex::new(HashSet::new())),
        }
    }

    /// Build (or rebuild) the analyzers for a document, reconcile cross-surface
    /// source laws, store state, and publish one coherent diagnostic set per URI.
    async fn refresh_analyzer(&self, uri: &Url, content: &str) {
        let mut batches = if is_rust_source(uri) {
            Vec::new()
        } else {
            self.state.analyze_and_observe(uri, content).await
        };
        if is_source_contract_trigger(uri) {
            batches.extend(self.source_contract_publications(uri).await);
        }

        for (target_uri, diagnostics) in coalesce_publications(batches) {
            self.sync_diagnostics_to_registry(&target_uri, &diagnostics)
                .await;
            self.client
                .publish_diagnostics(
                    target_uri,
                    diagnostics
                        .into_iter()
                        .map(|diagnostic| diagnostic.lsp)
                        .collect(),
                    None,
                )
                .await;
        }
    }

    /// Recompute generated-source laws from the current project graph and
    /// open-buffer overlay. The returned publications include explicit clears
    /// for source URIs that were flagged on the previous pass.
    async fn source_contract_publications(
        &self, trigger: &Url,
    ) -> Vec<(Url, Vec<lsp_max_protocol::MaxDiagnostic>)> {
        let groups = match self.source_contract_root(trigger) {
            Some(root) => {
                let overlay = self.buffer_overlay().await;
                match crate::project_index::ProjectIndex::from_root_with_overlay(&root, &overlay) {
                    Ok(project) => crate::source_contract::detect(&project, &overlay),
                    Err(_) => Vec::new(),
                }
            }
            None => Vec::new(),
        };

        // `Url`'s `Hash`/`Eq` are derived from its string representation, not
        // from the interior-mutable `Cell` clippy flags here (a lazily-cached
        // parsed component, e.g. port) -- using it as a HashSet key is safe;
        // the false-positive is a known clippy::mutable_key_type limitation.
        #[allow(clippy::mutable_key_type)]
        let mut current = HashSet::new();
        let mut publications = Vec::new();
        for (path, diagnostics) in groups {
            let Some(uri) = url_from_path(&path) else {
                continue;
            };
            current.insert(uri.clone());
            self.state.observe_diagnostics(&uri, &diagnostics).await;
            publications.push((uri, diagnostics));
        }

        let mut flagged = self.source_contract_flagged.lock().await;
        let stale: Vec<Url> = flagged.difference(&current).cloned().collect();
        *flagged = current;
        drop(flagged);

        for uri in stale {
            self.state.observe_diagnostics(&uri, &[]).await;
            publications.push((uri, Vec::new()));
        }

        publications
    }

    async fn buffer_overlay(&self) -> BufferOverlay {
        let documents = self.state.documents.lock().await;
        documents
            .iter()
            .filter_map(|(uri, content)| {
                document_uri_to_path(uri).map(|path| (path, content.clone()))
            })
            .collect()
    }

    fn source_contract_root(&self, uri: &Url) -> Option<PathBuf> {
        if let Some(path) = document_uri_to_path(uri) {
            let mut directory = path.parent();
            while let Some(candidate) = directory {
                if candidate.join("ggen.toml").is_file() {
                    return Some(candidate.to_path_buf());
                }
                directory = candidate.parent();
            }
        }

        let fallback = lsp_max::get_registry()
            .lock()
            .ok()
            .map(|registry| registry.root_path.clone())?;
        fallback.join("ggen.toml").is_file().then_some(fallback)
    }

    /// Replace the registry diagnostics owned by one document and recompute the
    /// global Λ_CD gate from every still-open document.
    async fn sync_diagnostics_to_registry(
        &self, uri: &Url, diagnostics: &[lsp_max_protocol::MaxDiagnostic],
    ) {
        let mut next = RegistryDocumentState::default();
        let mut next_diagnostics = Vec::new();

        for diagnostic in diagnostics {
            use lsp_max_protocol::LawAxis;

            let code = crate::check::diag_code(&diagnostic.lsp);
            let id = diagnostic_id(uri, diagnostic, &code);
            let mut max_diagnostic = diagnostic.clone();
            max_diagnostic.diagnostic_id = id.clone();
            max_diagnostic.law_id = code.clone();
            max_diagnostic.law_axis = LawAxis::Domain;
            max_diagnostic.violated_invariant = diagnostic.lsp.message.clone();

            next.diagnostic_ids.insert(id.clone());
            if code.starts_with("GGEN-")
                && diagnostic.lsp.severity == Some(DiagnosticSeverity::ERROR)
            {
                next.gating_ids.insert(id.clone());
            }
            next_diagnostics.push((id, max_diagnostic));
        }

        let mut documents = self.registry_documents.lock().await;
        let previous_ids = documents
            .get(uri)
            .map(|state| state.diagnostic_ids.clone())
            .unwrap_or_default();

        if let Ok(mut registry) = lsp_max::get_registry().lock() {
            for id in previous_ids {
                registry.diagnostics.remove(&id);
            }
            for (id, diagnostic) in next_diagnostics {
                registry.diagnostics.insert(id, diagnostic);
            }
        }

        if next.diagnostic_ids.is_empty() {
            documents.remove(uri);
        } else {
            documents.insert(uri.clone(), next);
        }
        let has_gating_violations = documents.values().any(|state| !state.gating_ids.is_empty());
        drop(documents);

        write_gate(has_gating_violations);
    }

    async fn clear_registry_diagnostics(&self) {
        let mut documents = self.registry_documents.lock().await;
        if let Ok(mut registry) = lsp_max::get_registry().lock() {
            for state in documents.values() {
                for id in &state.diagnostic_ids {
                    registry.diagnostics.remove(id);
                }
            }
        }
        documents.clear();
        drop(documents);
        self.source_contract_flagged.lock().await.clear();
        write_gate(false);
    }
}

#[lsp_max::async_trait]
impl LanguageServer for GgenLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        // Prefer the modern workspaceFolders contract, then rootUri for older
        // clients, then the process cwd. The registry and gate share this root.
        #[allow(deprecated)]
        let root = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| document_uri_to_path(&folder.uri))
            .or_else(|| params.root_uri.as_ref().and_then(document_uri_to_path))
            .or_else(|| std::env::current_dir().ok())
            .unwrap_or_default();

        if let Ok(mut registry) = lsp_max::get_registry().lock() {
            registry.root_path = root;
        }
        write_gate(false);

        lsp_max::MESH
            .get_or_init(|| std::sync::Mutex::new(lsp_max::max_runtime::AutonomicMesh::new()));

        let call_hierarchy_supported =
            client_supports_capability(&params.capabilities, &["textDocument", "callHierarchy"]);
        let call_hierarchy_dynamic = client_capability_bool(
            &params.capabilities,
            &["textDocument", "callHierarchy", "dynamicRegistration"],
        );
        let type_hierarchy_dynamic = client_capability_bool(
            &params.capabilities,
            &["textDocument", "typeHierarchy", "dynamicRegistration"],
        );

        let mut registrations = self.hierarchy_registrations.lock().await;
        registrations.clear();
        if call_hierarchy_dynamic {
            registrations.push(Registration {
                id: "ggen-call-hierarchy".to_string(),
                method: "textDocument/prepareCallHierarchy".to_string(),
                register_options: Some(serde_json::json!({"documentSelector": null})),
            });
        }
        if type_hierarchy_dynamic {
            registrations.push(Registration {
                id: "ggen-type-hierarchy".to_string(),
                method: "textDocument/prepareTypeHierarchy".to_string(),
                register_options: Some(serde_json::json!({"documentSelector": null})),
            });
        }
        drop(registrations);

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
                call_hierarchy_provider: (call_hierarchy_supported && !call_hierarchy_dynamic)
                    .then_some(CallHierarchyServerCapability::Simple(true)),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "ggen-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
            ..Default::default()
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        let registrations = {
            let mut pending = self.hierarchy_registrations.lock().await;
            std::mem::take(&mut *pending)
        };
        if !registrations.is_empty() {
            if let Err(error) = self.client.register_capability(registrations).await {
                tracing::warn!(%error, "failed to register hierarchy capabilities");
            }
        }
    }

    async fn shutdown(&self) -> Result<()> {
        self.clear_registry_diagnostics().await;
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
        let mut batches = self.state.close_document(&uri).await;

        if is_source_contract_trigger(&uri) {
            // Closing a Rust buffer returns authority to disk. Recompute instead
            // of blindly clearing so a persisted violation remains visible.
            batches.extend(self.source_contract_publications(&uri).await);
        }

        for (target_uri, diagnostics) in coalesce_publications(batches) {
            self.sync_diagnostics_to_registry(&target_uri, &diagnostics)
                .await;
            self.client
                .publish_diagnostics(
                    target_uri,
                    diagnostics
                        .into_iter()
                        .map(|diagnostic| diagnostic.lsp)
                        .collect(),
                    None,
                )
                .await;
        }
    }

    async fn did_save(&self, _params: DidSaveTextDocumentParams) {
        // The authoritative text is tracked via did_open / did_change.
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

    async fn incoming_calls(
        &self, _params: CallHierarchyIncomingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyIncomingCall>>> {
        Ok(Some(Vec::new()))
    }

    async fn outgoing_calls(
        &self, _params: CallHierarchyOutgoingCallsParams,
    ) -> Result<Option<Vec<CallHierarchyOutgoingCall>>> {
        Ok(Some(Vec::new()))
    }

    async fn prepare_type_hierarchy(
        &self, params: TypeHierarchyPrepareParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        handlers::type_hierarchy::handle_prepare(self, params).await
    }

    async fn supertypes(
        &self, _params: TypeHierarchySupertypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        Ok(Some(Vec::new()))
    }

    async fn subtypes(
        &self, _params: TypeHierarchySubtypesParams,
    ) -> Result<Option<Vec<TypeHierarchyItem>>> {
        Ok(Some(Vec::new()))
    }

    async fn symbol(
        &self, params: WorkspaceSymbolParams,
    ) -> Result<Option<Vec<SymbolInformation>>> {
        handlers::workspace_symbol::handle(self, params).await
    }
}

fn client_supports_capability(capabilities: &ClientCapabilities, path: &[&str]) -> bool {
    let Ok(mut value) = serde_json::to_value(capabilities) else {
        return false;
    };
    for segment in path {
        let Some(next) = value.get(*segment).cloned() else {
            return false;
        };
        value = next;
    }
    !value.is_null()
}

fn client_capability_bool(capabilities: &ClientCapabilities, path: &[&str]) -> bool {
    let Ok(mut value) = serde_json::to_value(capabilities) else {
        return false;
    };
    for segment in path {
        let Some(next) = value.get(*segment).cloned() else {
            return false;
        };
        value = next;
    }
    value.as_bool().unwrap_or(false)
}

fn diagnostic_key(diagnostic: &lsp_max_protocol::MaxDiagnostic) -> String {
    let code = crate::check::diag_code(&diagnostic.lsp);
    let range = diagnostic.lsp.range;
    format!(
        "{}|{}:{}-{}:{}|{}",
        code,
        range.start.line,
        range.start.character,
        range.end.line,
        range.end.character,
        diagnostic.lsp.message
    )
}

#[allow(clippy::mutable_key_type)]
fn coalesce_publications(
    batches: Vec<(Url, Vec<lsp_max_protocol::MaxDiagnostic>)>,
) -> Vec<(Url, Vec<lsp_max_protocol::MaxDiagnostic>)> {
    let mut publications: Vec<(Url, Vec<lsp_max_protocol::MaxDiagnostic>)> = Vec::new();
    for (uri, diagnostics) in batches {
        if let Some((_, existing)) = publications.iter_mut().find(|(known, _)| known == &uri) {
            let mut keys: HashSet<String> = existing.iter().map(diagnostic_key).collect();
            for diagnostic in diagnostics {
                if keys.insert(diagnostic_key(&diagnostic)) {
                    existing.push(diagnostic);
                }
            }
        } else {
            publications.push((uri, diagnostics));
        }
    }
    publications
}

fn diagnostic_id(uri: &Url, diagnostic: &lsp_max_protocol::MaxDiagnostic, code: &str) -> String {
    let range = diagnostic.lsp.range;
    let mut hasher = blake3::Hasher::new();
    hasher.update(uri.as_str().as_bytes());
    hasher.update(code.as_bytes());
    hasher.update(
        format!(
            "{}:{}-{}:{}",
            range.start.line, range.start.character, range.end.line, range.end.character
        )
        .as_bytes(),
    );
    hasher.update(diagnostic.lsp.message.as_bytes());
    format!("{}-{:.16}", code, hasher.finalize().to_hex())
}

fn is_ggen_manifest(uri: &Url) -> bool {
    uri.path().as_str().ends_with("ggen.toml")
}

fn is_rust_source(uri: &Url) -> bool {
    uri.path().as_str().ends_with(".rs")
}

fn is_source_contract_trigger(uri: &Url) -> bool {
    is_ggen_manifest(uri) || is_rust_source(uri)
}

fn document_uri_to_path(uri: &Url) -> Option<PathBuf> {
    url::Url::parse(uri.as_str()).ok()?.to_file_path().ok()
}

fn url_from_path(path: &Path) -> Option<Url> {
    url::Url::from_file_path(path)
        .ok()?
        .to_string()
        .parse()
        .ok()
}

fn write_gate(has_gating_violations: bool) {
    let path = gate_file_path();
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    let _ = std::fs::write(path, if has_gating_violations { b"1" } else { b"0" });
}

/// Filesystem path for the Λ_CD gate file.
///
/// Published `lsp-max` tracks gate state in its in-memory registry rather than
/// exposing a gate-file helper, so ggen-lsp owns this path.
fn gate_file_path() -> PathBuf {
    lsp_max::get_registry()
        .lock()
        .map(|registry| registry.root_path.clone())
        .unwrap_or_default()
        .join(".ggen")
        .join("lambda_cd.gate")
}

pub fn range_contains(range: &Range, position: Position) -> bool {
    let after_start = position.line > range.start.line
        || (position.line == range.start.line && position.character >= range.start.character);
    let before_end = position.line < range.end.line
        || (position.line == range.end.line && position.character <= range.end.character);
    after_start && before_end
}

#[cfg(test)]
mod tests {
    use super::*;

    fn diagnostic(code: &str, line: u32, message: &str) -> lsp_max_protocol::MaxDiagnostic {
        let mut diagnostic = lsp_max_protocol::MaxDiagnostic::default();
        diagnostic.lsp.code = Some(NumberOrString::String(code.to_string()));
        diagnostic.lsp.severity = Some(DiagnosticSeverity::ERROR);
        diagnostic.lsp.message = message.to_string();
        diagnostic.lsp.range = Range {
            start: Position { line, character: 0 },
            end: Position { line, character: 1 },
        };
        diagnostic
    }

    #[test]
    fn hierarchy_capabilities_are_client_negotiated() {
        let unsupported = ClientCapabilities::default();
        assert!(!client_supports_capability(
            &unsupported,
            &["textDocument", "callHierarchy"]
        ));

        let supported: ClientCapabilities = serde_json::from_value(serde_json::json!({
            "textDocument": {
                "callHierarchy": {"dynamicRegistration": false},
                "typeHierarchy": {"dynamicRegistration": false}
            }
        }))
        .expect("client capabilities");
        assert!(client_supports_capability(
            &supported,
            &["textDocument", "callHierarchy"]
        ));
        assert!(client_supports_capability(
            &supported,
            &["textDocument", "typeHierarchy"]
        ));
        assert!(!client_capability_bool(
            &supported,
            &["textDocument", "callHierarchy", "dynamicRegistration"]
        ));
    }

    #[test]
    fn coalesces_same_uri_without_duplicate_diagnostics() {
        let uri: Url = "file:///workspace/ggen.toml".parse().expect("URI");
        let a = diagnostic("GGEN-RULE-001", 0, "missing rule file");
        let b = diagnostic("GGEN-OUT-001", 1, "unbound output");
        let publications = coalesce_publications(vec![
            (uri.clone(), vec![a.clone()]),
            (uri.clone(), vec![a, b]),
        ]);
        assert_eq!(publications.len(), 1);
        assert_eq!(publications[0].1.len(), 2);
    }

    #[test]
    fn diagnostic_identity_is_document_and_location_scoped() {
        let left: Url = "file:///left/ggen.toml".parse().expect("left URI");
        let right: Url = "file:///right/ggen.toml".parse().expect("right URI");
        let d0 = diagnostic("GGEN-RULE-001", 0, "missing rule file");
        let d1 = diagnostic("GGEN-RULE-001", 1, "missing rule file");

        assert_ne!(
            diagnostic_id(&left, &d0, "GGEN-RULE-001"),
            diagnostic_id(&right, &d0, "GGEN-RULE-001")
        );
        assert_ne!(
            diagnostic_id(&left, &d0, "GGEN-RULE-001"),
            diagnostic_id(&left, &d1, "GGEN-RULE-001")
        );
    }
}
