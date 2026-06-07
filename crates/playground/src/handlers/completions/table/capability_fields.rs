pub static CAPABILITY_FIELDS: &[(&str, &str, &[&str])] = &[
    (
        "text_document_sync",
        "Option<TextDocumentSyncCapability>",
        &[
            "did_open",
            "did_change",
            "did_save",
            "did_close",
            "will_save",
            "will_save_wait_until",
        ],
    ),
    (
        "notebook_document_sync",
        "Option<NotebookDocumentSyncOptionsOrNotebookDocumentSyncRegistrationOptions>",
        &[
            "did_open_notebook_document",
            "did_change_notebook_document",
            "did_save_notebook_document",
            "did_close_notebook_document",
        ],
    ),
    (
        "completion_provider",
        "Option<CompletionOptions>",
        &["completion", "completion_resolve"],
    ),
    (
        "hover_provider",
        "Option<HoverProviderCapability>",
        &["hover"],
    ),
    (
        "signature_help_provider",
        "Option<SignatureHelpOptions>",
        &["signature_help"],
    ),
    (
        "definition_provider",
        "Option<OneOf<bool, DefinitionOptions>>",
        &["goto_definition"],
    ),
    (
        "declaration_provider",
        "Option<DeclarationCapability>",
        &["goto_declaration"],
    ),
    (
        "type_definition_provider",
        "Option<TypeDefinitionProviderCapability>",
        &["goto_type_definition"],
    ),
    (
        "implementation_provider",
        "Option<ImplementationProviderCapability>",
        &["goto_implementation"],
    ),
    (
        "references_provider",
        "Option<OneOf<bool, ReferencesOptions>>",
        &["references"],
    ),
    (
        "document_highlight_provider",
        "Option<OneOf<bool, DocumentHighlightOptions>>",
        &["document_highlight"],
    ),
    (
        "document_symbol_provider",
        "Option<OneOf<bool, DocumentSymbolOptions>>",
        &["document_symbol"],
    ),
    (
        "workspace_symbol_provider",
        "Option<OneOf<bool, WorkspaceSymbolOptions>>",
        &["symbol", "symbol_resolve"],
    ),
    (
        "code_action_provider",
        "Option<CodeActionProviderCapability>",
        &["code_action", "code_action_resolve"],
    ),
    (
        "code_lens_provider",
        "Option<CodeLensOptions>",
        &["code_lens", "code_lens_resolve"],
    ),
    (
        "document_link_provider",
        "Option<DocumentLinkOptions>",
        &["document_link", "document_link_resolve"],
    ),
    (
        "color_provider",
        "Option<BooleanOrDocumentColorOptionsOrDocumentColorRegistrationOptions>",
        &["document_color", "color_presentation"],
    ),
    (
        "document_formatting_provider",
        "Option<OneOf<bool, DocumentFormattingOptions>>",
        &["formatting", "ranges_formatting"],
    ),
    (
        "document_range_formatting_provider",
        "Option<OneOf<bool, DocumentRangeFormattingOptions>>",
        &["range_formatting"],
    ),
    (
        "document_on_type_formatting_provider",
        "Option<DocumentOnTypeFormattingOptions>",
        &["on_type_formatting"],
    ),
    (
        "rename_provider",
        "Option<OneOf<bool, RenameOptions>>",
        &["rename", "prepare_rename"],
    ),
    (
        "folding_range_provider",
        "Option<FoldingRangeProviderCapability>",
        &["folding_range"],
    ),
    (
        "selection_range_provider",
        "Option<SelectionRangeProviderCapability>",
        &["selection_range"],
    ),
    (
        "execute_command_provider",
        "Option<ExecuteCommandOptions>",
        &["execute_command"],
    ),
    (
        "call_hierarchy_provider",
        "Option<CallHierarchyServerCapability>",
        &["prepare_call_hierarchy", "incoming_calls", "outgoing_calls"],
    ),
    (
        "linked_editing_range_provider",
        "Option<BooleanOrLinkedEditingRangeOptionsOrLinkedEditingRangeRegistrationOptions>",
        &["linked_editing_range"],
    ),
    (
        "semantic_tokens_provider",
        "Option<SemanticTokensServerCapabilities>",
        &[
            "semantic_tokens_full",
            "semantic_tokens_full_delta",
            "semantic_tokens_range",
        ],
    ),
    (
        "moniker_provider",
        "Option<BooleanOrMonikerOptionsOrMonikerRegistrationOptions>",
        &["moniker"],
    ),
    (
        "type_hierarchy_provider",
        "Option<OneOf<bool, TypeHierarchyOptions>>",
        &["prepare_type_hierarchy", "supertypes", "subtypes"],
    ),
    (
        "inline_value_provider",
        "Option<BooleanOrInlineValueOptionsOrInlineValueRegistrationOptions>",
        &["inline_value"],
    ),
    (
        "inlay_hint_provider",
        "Option<OneOf<bool, InlayHintServerCapabilities>>",
        &["inlay_hint", "inlay_hint_resolve"],
    ),
    (
        "diagnostic_provider",
        "Option<DiagnosticServerCapabilities>",
        &["diagnostic", "workspace_diagnostic"],
    ),
    (
        "inline_completion_provider",
        "Option<OneOf<bool, InlineCompletionOptions>>",
        &["inline_completion"],
    ),
    (
        "workspace.workspaceFolders",
        "Option<WorkspaceFoldersServerCapabilities>",
        &["did_change_workspace_folders"],
    ),
    (
        "workspace.fileOperations",
        "Option<FileOperationOptions>",
        &[
            "will_create_files",
            "did_create_files",
            "will_rename_files",
            "did_rename_files",
            "will_delete_files",
            "did_delete_files",
        ],
    ),
    (
        "text_document_content_provider",
        "Option<TextDocumentContentOptions>",
        &["text_document_content"],
    ),
];
