use lsp_max::ast::AutoLspAdapter;
use lsp_max::rule_pack_server::{RulePack, RulePackServer, WorkspaceIndex};
use lsp_max::{Client, LanguageServer, LspService, Server};
use lsp_max::lsp_types_max::*;
use std::path::Path;

// ── Load rule packs from `rules/` directory ───────────────────────────────────

fn load_packs() -> Vec<RulePack> {
    let rules_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("rules");
    let mut packs = Vec::new();
    let Ok(dir) = std::fs::read_dir(&rules_dir) else {
        return packs;
    };
    for entry in dir.flatten() {
        if entry.path().extension().and_then(|e| e.to_str()) != Some("toml") {
            continue;
        }
        let content = match std::fs::read_to_string(entry.path()) {
            Ok(c) => c,
            Err(_) => continue,
        };
        match toml::from_str::<RulePack>(&content) {
            Ok(pack) => packs.push(pack),
            Err(e) => tracing::warn!(path = ?entry.path(), error = %e, "skipping malformed rule pack"),
        }
    }
    packs
}

// ── Backend struct ────────────────────────────────────────────────────────────

pub struct AntiLlmCheatLspBackend {
    client: Client,
    adapter: AutoLspAdapter,
    packs: Vec<RulePack>,
    index: WorkspaceIndex,
}

impl AntiLlmCheatLspBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            adapter: AutoLspAdapter::new_default(),
            packs: load_packs(),
            index: WorkspaceIndex::new(),
        }
    }
}

impl RulePackServer for AntiLlmCheatLspBackend {
    fn rule_packs(&self) -> &[RulePack] { &self.packs }
    fn grammar(&self) -> tree_sitter::Language { tree_sitter_rust::LANGUAGE.into() }
    fn server_name(&self) -> &'static str { "anti-llm-cheat-lsp" }
    fn client(&self) -> &Client { &self.client }
    fn adapter(&self) -> &AutoLspAdapter { &self.adapter }
    fn workspace_index(&self) -> Option<&WorkspaceIndex> { Some(&self.index) }
}

#[lsp_max::async_trait]
impl LanguageServer for AntiLlmCheatLspBackend {
    async fn initialize(&self, _: InitializeParams) -> lsp_max::jsonrpc::Result<InitializeResult> {
        Ok(self.build_initialize_result())
    }
    async fn shutdown(&self) -> lsp_max::jsonrpc::Result<()> { Ok(()) }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.handle_did_open(params).await;
    }
    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.handle_did_change(params).await;
    }
    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        self.handle_did_close(params);
    }
    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        if let Some(text) = params.text {
            self.publish_findings(params.text_document.uri, &text).await;
        }
    }
    async fn diagnostic(
        &self,
        params: DocumentDiagnosticParams,
    ) -> lsp_max::jsonrpc::Result<DocumentDiagnosticReportResult> {
        Ok(self.pull_document_diagnostics(&params.text_document.uri))
    }
}