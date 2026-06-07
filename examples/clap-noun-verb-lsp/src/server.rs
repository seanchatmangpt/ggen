use clap_noun_verb_macros::verb;
use clap_noun_verb::Result;
use crate::types::CommandResult;
use tower_lsp_max::jsonrpc;
use tower_lsp_max::lsp_types::*;
use tower_lsp_max::{Client, LanguageServer, LspService, Server};
use tokio::runtime::Runtime;

#[derive(Debug)]
pub struct ClapNounVerbLsp {
    client: Client,
}

impl ClapNounVerbLsp {
    pub fn new(client: Client) -> Self {
        Self { client }
    }
}

#[tower_lsp_max::async_trait]
impl LanguageServer for ClapNounVerbLsp {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                    identifier: None,
                    inter_file_dependencies: true,
                    workspace_diagnostics: true,
                    work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
                })),
                semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        token_types: vec![
                            SemanticTokenType::new("cnvNoun"),
                            SemanticTokenType::new("cnvVerb"),
                            SemanticTokenType::new("cnvArg"),
                            SemanticTokenType::new("cnvArgTag"),
                            SemanticTokenType::new("cnvEnvVar"),
                            SemanticTokenType::new("cnvDefaultValue"),
                            SemanticTokenType::new("cnvGroupName"),
                            SemanticTokenType::new("cnvRequiresLink"),
                            SemanticTokenType::new("cnvConflictsLink"),
                            SemanticTokenType::new("cnvAlias"),
                            SemanticTokenType::new("cnvDomainCall"),
                            SemanticTokenType::new("cnvIntegrationCall"),
                            SemanticTokenType::new("cnvReceiptId"),
                            SemanticTokenType::new("cnvDiagnosticCode"),
                            SemanticTokenType::new("cnvDeprecated"),
                        ],
                        token_modifiers: vec![],
                    },
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    range: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
                })),
                inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(InlayHintOptions {
                    resolve_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
                }))),
                inline_value_provider: Some(OneOf::Right(InlineValueServerCapabilities::Options(InlineValueOptions {
                    work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
                }))),
                code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
                    resolve_provider: Some(true),
                    code_action_kinds: None,
                    work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
                })),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(true),
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                workspace_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
                moniker_provider: Some(OneOf::Left(true)),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec![],
                    work_done_progress_options: WorkDoneProgressOptions { work_done_progress: None },
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                        did_create: None,
                        will_create: None,
                        did_rename: None,
                        will_rename: None,
                        did_delete: None,
                        will_delete: None,
                    }),
                }),
                text_document_sync: Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                        include_text: Some(true),
                    })),
                    will_save: None,
                    will_save_wait_until: None,
                })),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "clap-noun-verb-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            offset_encoding: None,
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "clap-noun-verb-lsp initialized")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }
}

#[verb("serve")]
pub fn cmd_serve(stdio: bool) -> Result<CommandResult> {
    let rt = Runtime::new().unwrap();
    rt.block_on(async {
        if stdio {
            let stdin = tokio::io::stdin();
            let stdout = tokio::io::stdout();
            let (service, socket) = LspService::new(|client| ClapNounVerbLsp::new(client));
            Server::new(stdin, stdout, socket).serve(service).await;
        }
    });
    Ok(CommandResult { success: true })
}
