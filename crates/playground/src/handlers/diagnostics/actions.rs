use std::collections::HashMap;

use tower_lsp_max::lsp_types_max::*;

use crate::handlers::completions::{CAPABILITY_FIELDS, METHODS};
use crate::handlers::diagnostics::analysis::analyze_impl_block;
use crate::handlers::diagnostics::levenshtein_distance;
use crate::handlers::diagnostics::rules::{
    to_lsp_position, CAPABILITY_WITHOUT_METHOD, INVALID_RPC_NAME, METHOD_WITHOUT_CAPABILITY,
    MISSING_MANDATORY_METHOD, TYPO_IN_METHOD_NAME,
};
use crate::Backend;

/// Code action entry point called from `lib.rs`.
pub async fn code_actions(
    backend: &Backend,
    uri: &Uri,
    diagnostics: &[Diagnostic],
) -> Option<CodeActionResponse> {
    let doc = backend.docs.get(uri);
    let doc = match doc {
        Some(d) if d.language_id == "rust" => d,
        _ => return None,
    };
    let source = doc.text.to_string();
    let rope_text = doc.text.clone();
    drop(doc);

    // Try to parse AST
    let ast = syn::parse_file(&source).ok();
    let analysis = ast.as_ref().map(analyze_impl_block).unwrap_or_default();

    let mut actions: Vec<CodeActionOrCommand> = Vec::new();

    for diag in diagnostics {
        match diag.code.as_ref() {
            Some(NumberOrString::String(code)) if code == TYPO_IN_METHOD_NAME => {
                // Get the misspelled name from the document using the diagnostic range
                let start_char = rope_offset(&rope_text, diag.range.start);
                let end_char = rope_offset(&rope_text, diag.range.end);
                if start_char <= end_char && end_char <= rope_text.len_chars() {
                    let misspelled_name = rope_text.slice(start_char..end_char).to_string();
                    // Find closest match by Levenshtein distance
                    let mut min_dist = usize::MAX;
                    let mut closest_match: Option<&crate::handlers::completions::MethodEntry> =
                        None;
                    for entry in METHODS {
                        let dist = levenshtein_distance(&misspelled_name, entry.fn_name);
                        if dist < min_dist {
                            min_dist = dist;
                            closest_match = Some(entry);
                        }
                    }
                    if let Some(closest) = closest_match {
                        if min_dist <= 4 {
                            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                                title: format!("Correct typo to `{}`", closest.fn_name),
                                kind: Some(CodeActionKind::QUICKFIX),
                                diagnostics: Some(vec![diag.clone()]),
                                edit: Some(WorkspaceEdit {
                                    changes: Some(HashMap::from([(
                                        uri.clone(),
                                        vec![TextEdit {
                                            range: diag.range,
                                            new_text: closest.fn_name.to_string(),
                                        }],
                                    )])),
                                    ..Default::default()
                                }),
                                is_preferred: Some(true),
                                ..Default::default()
                            }));
                        }
                    }
                }
            }
            Some(NumberOrString::String(code)) if code == INVALID_RPC_NAME => {
                let start_char = rope_offset(&rope_text, diag.range.start);
                let end_char = rope_offset(&rope_text, diag.range.end);
                if start_char <= end_char && end_char <= rope_text.len_chars() {
                    let misspelled_name = rope_text.slice(start_char..end_char).to_string();
                    let clean_name = misspelled_name.trim_matches('"');
                    let mut min_dist = usize::MAX;
                    let mut closest_match: Option<&crate::handlers::completions::MethodEntry> =
                        None;
                    for entry in METHODS {
                        let dist = levenshtein_distance(clean_name, entry.lsp_method);
                        if dist < min_dist {
                            min_dist = dist;
                            closest_match = Some(entry);
                        }
                    }
                    if let Some(closest) = closest_match {
                        if min_dist <= 4 {
                            let new_text = if misspelled_name.starts_with('"')
                                && misspelled_name.ends_with('"')
                            {
                                format!("\"{}\"", closest.lsp_method)
                            } else {
                                closest.lsp_method.to_string()
                            };
                            actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                                title: format!("Correct RPC name to `{}`", closest.lsp_method),
                                kind: Some(CodeActionKind::QUICKFIX),
                                diagnostics: Some(vec![diag.clone()]),
                                edit: Some(WorkspaceEdit {
                                    changes: Some(HashMap::from([(
                                        uri.clone(),
                                        vec![TextEdit {
                                            range: diag.range,
                                            new_text,
                                        }],
                                    )])),
                                    ..Default::default()
                                }),
                                is_preferred: Some(true),
                                ..Default::default()
                            }));
                        }
                    }
                }
            }
            Some(NumberOrString::String(code)) if code == CAPABILITY_WITHOUT_METHOD => {
                if let Some(method_name) = extract_method_from_missing_override_msg(&diag.message) {
                    if let Some(entry) = METHODS.iter().find(|m| m.fn_name == method_name) {
                        let stub = generate_method_stub(entry);
                        let insert_pos = analysis
                            .impl_close_brace_span
                            .map(|s| to_lsp_position(s.start()))
                            .unwrap_or_else(|| find_fallback_insert_pos(&rope_text));
                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: format!("Generate stub for `{}`", entry.fn_name),
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![diag.clone()]),
                            edit: Some(workspace_edit_insert(uri, insert_pos, &stub)),
                            is_preferred: Some(true),
                            ..Default::default()
                        }));
                    }
                }
            }
            Some(NumberOrString::String(code)) if code == METHOD_WITHOUT_CAPABILITY => {
                if let Some(cap_field) = extract_capability_from_dead_code_msg(&diag.message) {
                    if let Some(&(name, ty, _)) =
                        CAPABILITY_FIELDS.iter().find(|(n, _, _)| *n == cap_field)
                    {
                        let mut action = CodeAction {
                            title: format!("Add `{name}: Some(...)` to ServerCapabilities"),
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![diag.clone()]),
                            ..Default::default()
                        };

                        if name.starts_with("workspace.") {
                            let child_field_name = if name == "workspace.workspaceFolders" {
                                "workspace_folders"
                            } else if name == "workspace.fileOperations" {
                                "file_operations"
                            } else {
                                name.strip_prefix("workspace.").unwrap()
                            };
                            let default_val = default_val_for_cap(name);

                            if let Some(workspace_brace) = analysis.workspace_open_brace_span {
                                let insert_pos = to_lsp_position(workspace_brace.end());
                                let new_text =
                                    format!("\n                {child_field_name}: {default_val},");
                                action.edit =
                                    Some(workspace_edit_insert(uri, insert_pos, &new_text));
                                action.is_preferred = Some(true);
                            } else if let Some(cap_brace) =
                                analysis.server_capabilities_open_brace_span
                            {
                                let insert_pos = to_lsp_position(cap_brace.end());
                                let new_text = format!("\n            workspace: Some(WorkspaceOptions {{\n                {child_field_name}: {default_val},\n                ..Default::default()\n            }}),");
                                action.edit =
                                    Some(workspace_edit_insert(uri, insert_pos, &new_text));
                                action.is_preferred = Some(true);
                            } else {
                                action.command = Some(Command {
                                    title: format!("Add capability declaration for `{name}`"),
                                    command: "tower-lsp-max-playground.addCapability".to_string(),
                                    arguments: Some(vec![
                                        serde_json::to_value(uri.as_str()).unwrap(),
                                        serde_json::Value::String(name.to_string()),
                                        serde_json::Value::String(ty.to_string()),
                                    ]),
                                });
                            }
                        } else {
                            if let Some(open_brace) = analysis.server_capabilities_open_brace_span {
                                let insert_pos = to_lsp_position(open_brace.end());
                                let default_val = default_val_for_cap(name);
                                let new_text = format!("\n            {name}: {default_val},");
                                action.edit =
                                    Some(workspace_edit_insert(uri, insert_pos, &new_text));
                                action.is_preferred = Some(true);
                            } else {
                                action.command = Some(Command {
                                    title: format!("Add capability declaration for `{name}`"),
                                    command: "tower-lsp-max-playground.addCapability".to_string(),
                                    arguments: Some(vec![
                                        serde_json::to_value(uri.as_str()).unwrap(),
                                        serde_json::Value::String(name.to_string()),
                                        serde_json::Value::String(ty.to_string()),
                                    ]),
                                });
                            }
                        }
                        actions.push(CodeActionOrCommand::CodeAction(action));
                    }
                }
            }
            Some(NumberOrString::String(code)) if code == MISSING_MANDATORY_METHOD => {
                let insert_pos = analysis
                    .impl_close_brace_span
                    .map(|s| to_lsp_position(s.start()))
                    .unwrap_or_else(|| find_fallback_insert_pos(&rope_text));

                // Add the specific missing method action
                if diag.message.contains("initialize") {
                    if let Some(entry) = METHODS.iter().find(|m| m.fn_name == "initialize") {
                        let stub = generate_method_stub(entry);
                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: "Generate stub for `initialize`".to_string(),
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![diag.clone()]),
                            edit: Some(workspace_edit_insert(uri, insert_pos, &stub)),
                            is_preferred: Some(true),
                            ..Default::default()
                        }));
                    }
                } else if diag.message.contains("shutdown") {
                    if let Some(entry) = METHODS.iter().find(|m| m.fn_name == "shutdown") {
                        let stub = generate_method_stub(entry);
                        actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                            title: "Generate stub for `shutdown`".to_string(),
                            kind: Some(CodeActionKind::QUICKFIX),
                            diagnostics: Some(vec![diag.clone()]),
                            edit: Some(workspace_edit_insert(uri, insert_pos, &stub)),
                            is_preferred: Some(true),
                            ..Default::default()
                        }));
                    }
                }

                // Also keep the scaffold option
                actions.push(CodeActionOrCommand::CodeAction(CodeAction {
                    title: "Generate minimal server scaffold (initialize + initialized + shutdown)"
                        .to_string(),
                    kind: Some(CodeActionKind::SOURCE),
                    diagnostics: Some(vec![diag.clone()]),
                    edit: Some(workspace_edit_insert(uri, insert_pos, MINIMAL_SCAFFOLD)),
                    is_preferred: Some(false),
                    ..Default::default()
                }));
            }
            _ => {}
        }
    }

    if actions.is_empty() {
        None
    } else {
        Some(actions)
    }
}

fn generate_method_stub(entry: &crate::handlers::completions::MethodEntry) -> String {
    if entry.fn_name == "initialize" {
        r#"
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities::default(),
            ..Default::default()
        })
    }
"#
        .to_string()
    } else if entry.fn_name == "shutdown" {
        r#"
    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
"#
        .to_string()
    } else {
        let params = if entry.params_type == "()" {
            "_: ()".to_string()
        } else {
            format!("_params: {}", entry.params_type)
        };
        let body = if entry.return_type.starts_with("Result<") {
            format!(
                "Err(tower_lsp::jsonrpc::Error::method_not_found()) // TODO: implement {}",
                entry.fn_name
            )
        } else {
            format!("// TODO: implement {}", entry.fn_name)
        };
        format!(
            "\n    async fn {}(&self, {}) -> {} {{\n        {}\n    }}\n",
            entry.fn_name, params, entry.return_type, body
        )
    }
}

fn workspace_edit_insert(uri: &Uri, pos: Position, text: &str) -> WorkspaceEdit {
    let edit = TextEdit {
        range: Range {
            start: pos,
            end: pos,
        },
        new_text: text.to_string(),
    };
    let mut changes = HashMap::new();
    changes.insert(uri.clone(), vec![edit]);
    WorkspaceEdit {
        changes: Some(changes),
        ..Default::default()
    }
}

fn extract_method_from_missing_override_msg(msg: &str) -> Option<&'static str> {
    // "`ServerCapabilities::X` is declared but `Y` is not overridden..."
    let start = msg.find("but `")? + 5;
    let end = msg[start..].find('`')? + start;
    let name = &msg[start..end];
    METHODS
        .iter()
        .find(|m| m.fn_name == name)
        .map(|m| m.fn_name)
}

fn extract_capability_from_dead_code_msg(msg: &str) -> Option<&'static str> {
    // "`Y` is overridden but `ServerCapabilities::X` is not set..."
    let marker = "ServerCapabilities::";
    let start = msg.find(marker)? + marker.len();
    let end = msg[start..].find('`')? + start;
    let name = &msg[start..end];
    CAPABILITY_FIELDS
        .iter()
        .find(|(n, _, _)| *n == name)
        .map(|(n, _, _)| *n)
}

fn default_val_for_cap(name: &str) -> &'static str {
    match name {
        "text_document_sync" => {
            "Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::INCREMENTAL))"
        }
        "completion_provider" => "Some(CompletionOptions::default())",
        "hover_provider" => "Some(HoverProviderCapability::Simple(true))",
        "definition_provider" => "Some(OneOf::Left(true))",
        "declaration_provider" => "Some(DeclarationCapability::Simple(true))",
        "type_definition_provider" => "Some(TypeDefinitionProviderCapability::Simple(true))",
        "implementation_provider" => "Some(ImplementationProviderCapability::Simple(true))",
        "references_provider" => "Some(OneOf::Left(true))",
        "document_highlight_provider" => "Some(OneOf::Left(true))",
        "document_symbol_provider" => "Some(OneOf::Left(true))",
        "workspace_symbol_provider" => "Some(OneOf::Left(true))",
        "code_action_provider" => "Some(CodeActionProviderCapability::Simple(true))",
        "code_lens_provider" => "Some(CodeLensOptions::default())",
        "document_link_provider" => "Some(DocumentLinkOptions::default())",
        "document_formatting_provider" => "Some(OneOf::Left(true))",
        "document_range_formatting_provider" => "Some(OneOf::Left(true))",
        "document_on_type_formatting_provider" => {
            "Some(DocumentOnTypeFormattingOptions::default())"
        }
        "rename_provider" => "Some(OneOf::Left(true))",
        "notebook_document_sync" => {
            "Some(NotebookDocumentSyncOptionsOrNotebookDocumentSyncRegistrationOptions::default())"
        }
        "moniker_provider" => {
            "Some(BooleanOrMonikerOptionsOrMonikerRegistrationOptions::Simple(true))"
        }
        "signature_help_provider" => "Some(SignatureHelpOptions::default())",
        "linked_editing_range_provider" => {
            "Some(BooleanOrLinkedEditingRangeOptionsOrLinkedEditingRangeRegistrationOptions::Simple(true))"
        }
        "color_provider" => {
            "Some(BooleanOrDocumentColorOptionsOrDocumentColorRegistrationOptions::Simple(true))"
        }
        "inline_value_provider" => {
            "Some(BooleanOrInlineValueOptionsOrInlineValueRegistrationOptions::Simple(true))"
        }
        "workspace.workspaceFolders" => {
            "Some(WorkspaceFoldersServerCapabilities { supported: Some(true), change_notifications: Some(OneOf::Left(true)) })"
        }
        "workspace.fileOperations" => "Some(FileOperationOptions::default())",
        _ => "Some(Default::default())",
    }
}

/// Convert an LSP Position (line/character) to a char offset in a Rope.
fn rope_offset(rope: &ropey::Rope, pos: Position) -> usize {
    if rope.len_lines() == 0 {
        return 0;
    }
    let line_idx = (pos.line as usize).min(rope.len_lines().saturating_sub(1));
    let line_start = rope.line_to_char(line_idx);
    let character = (pos.character as usize).min(rope.line(line_idx).len_chars());
    (line_start + character).min(rope.len_chars())
}

fn char_to_lsp_position(rope: &ropey::Rope, char_idx: usize) -> Position {
    if rope.len_chars() == 0 {
        return Position::new(0, 0);
    }
    let safe_idx = char_idx.min(rope.len_chars());
    let line = rope.char_to_line(safe_idx);
    let line_start_char = rope.line_to_char(line);
    let character = safe_idx.saturating_sub(line_start_char);
    Position::new(line as u32, character as u32)
}

fn find_fallback_insert_pos(text: &ropey::Rope) -> Position {
    let mut char_idx = text.len_chars();
    while char_idx > 0 {
        char_idx -= 1;
        if text.char(char_idx) == '}' {
            return char_to_lsp_position(text, char_idx);
        }
    }
    let end_line = text.len_lines().saturating_sub(1);
    let end_char = text.line(end_line).len_chars();
    Position::new(end_line as u32, end_char as u32)
}

const MINIMAL_SCAFFOLD: &str = r#"
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
"#;
