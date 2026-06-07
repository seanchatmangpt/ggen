use tower_lsp_max::lsp_types_max::*;

/// Build the `ServerCapabilities` advertised during `initialize`.
///
/// This "Maximal" configuration enables every single capability supported by tower-lsp-max.
pub fn server_capabilities() -> ServerCapabilities {
    ServerCapabilities {
        position_encoding: Some(PositionEncodingKind::UTF8),
        // Incremental sync: the Rope document store applies range patches in O(log n).
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::INCREMENTAL),
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                will_save: Some(true),
                will_save_wait_until: Some(true),
            },
        )),

        // Completions triggered by `.`, `:`, space, and `"` to cover all
        // tower-lsp-max completion contexts.
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(vec![
                ".".to_string(),
                ":".to_string(),
                " ".to_string(),
                "\"".to_string(),
            ]),
            all_commit_characters: None,
            work_done_progress_options: Default::default(),
            completion_item: Some(CompletionOptionsCompletionItem {
                label_details_support: Some(true),
            }),
        }),

        // Hover over method names, capability fields, and protocol types.
        hover_provider: Some(HoverProviderCapability::Simple(true)),

        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".to_string(), ",".to_string()]),
            retrigger_characters: None,
            work_done_progress_options: Default::default(),
        }),

        declaration_provider: Some(DeclarationCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
        implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
        references_provider: Some(OneOf::Left(true)),
        document_highlight_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),

        // Code actions: quickfix stubs + scaffold generation.
        code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
            code_action_kinds: Some(vec![
                CodeActionKind::QUICKFIX,
                CodeActionKind::SOURCE,
                CodeActionKind::REFACTOR,
            ]),
            resolve_provider: Some(true),
            work_done_progress_options: Default::default(),
            ..Default::default()
        })),

        code_lens_provider: Some(CodeLensOptions {
            resolve_provider: Some(true),
        }),

        document_formatting_provider: Some(OneOf::Left(true)),
        document_range_formatting_provider: Some(OneOf::Left(true)),
        document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
            first_trigger_character: "}".to_string(),
            more_trigger_character: Some(vec![";".to_string()]),
        }),

        rename_provider: Some(OneOf::Right(RenameOptions {
            prepare_provider: Some(true),
            work_done_progress_options: Default::default(),
        })),

        document_link_provider: Some(DocumentLinkOptions {
            resolve_provider: Some(true),
            work_done_progress_options: Default::default(),
        }),

        color_provider: Some(ColorProviderCapability::Simple(true)),
        folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
        selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),

        execute_command_provider: Some(ExecuteCommandOptions {
            commands: vec!["playground.test.command".to_string()],
            work_done_progress_options: Default::default(),
        }),

        call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
        // type_hierarchy_provider: Some(TypeHierarchyServerCapability::Simple(true)),
        semantic_tokens_provider: Some(SemanticTokensServerCapabilities::SemanticTokensOptions(
            SemanticTokensOptions {
                work_done_progress_options: Default::default(),
                legend: SemanticTokensLegend {
                    token_types: vec![],
                    token_modifiers: vec![],
                },
                range: Some(true),
                full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
            },
        )),

        moniker_provider: Some(OneOf::Left(true)),
        linked_editing_range_provider: Some(LinkedEditingRangeServerCapabilities::Simple(true)),
        inlay_hint_provider: Some(OneOf::Left(true)),
        inline_value_provider: Some(OneOf::Left(true)),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
            identifier: Some("playground".to_string()),
            inter_file_dependencies: true,
            workspace_diagnostics: true,
            work_done_progress_options: Default::default(),
        })),

        #[cfg(feature = "proposed")]
        inline_completion_provider: Some(OneOf::Left(true)),

        notebook_document_sync: Some(OneOf::Left(NotebookDocumentSyncOptions {
            notebook_selector: vec![NotebookSelector::ByNotebook {
                notebook: Notebook::String("jupyter".to_string()),
                cells: Some(vec![NotebookCellSelector {
                    language: "python".to_string(),
                }]),
            }],
            save: Some(true),
        })),

        workspace: Some(WorkspaceServerCapabilities {
            workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(OneOf::Left(true)),
            }),
            file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                did_create: Some(FileOperationRegistrationOptions { filters: vec![] }),
                will_create: Some(FileOperationRegistrationOptions { filters: vec![] }),
                did_rename: Some(FileOperationRegistrationOptions { filters: vec![] }),
                will_rename: Some(FileOperationRegistrationOptions { filters: vec![] }),
                did_delete: Some(FileOperationRegistrationOptions { filters: vec![] }),
                will_delete: Some(FileOperationRegistrationOptions { filters: vec![] }),
            }),
        }),

        experimental: Some(serde_json::json!({
            "maximal": true,
            "playground": true
        })),
        ..Default::default()
    }
}
