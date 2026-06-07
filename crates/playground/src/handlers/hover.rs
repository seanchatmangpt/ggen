use tower_lsp_max::lsp_types_max::*;

use super::completions::{CAPABILITY_FIELDS, METHODS};
use crate::Backend;

/// Hover entry point called from `lib.rs`.
pub async fn compute(backend: &Backend, uri: &Uri, pos: Position) -> Option<Hover> {
    let doc = backend.docs.get(uri)?;
    if doc.language_id != "rust" {
        return None;
    }
    let source = doc.text.to_string();
    drop(doc);
    let word = word_at(&source, pos)?;
    hover_for_word(&word)
}

/// Public entry point taking position + text, matching the spec signature.
pub fn get_hover(pos: Position, text: &str) -> Option<Hover> {
    let word = word_at(text, pos)?;
    hover_for_word(&word)
}

/// Look up hover documentation for a bare word token.
pub fn hover_for_word(word: &str) -> Option<Hover> {
    // Check method table first
    if let Some(entry) = METHODS.iter().find(|m| m.fn_name == word) {
        let cap_section = entry
            .capability_field
            .map(|c| {
                format!(
                    "\n\n**Capability field:** `ServerCapabilities::{c}`\n\nDeclare this in the \
                     `ServerCapabilities` returned from `initialize` to activate the handler."
                )
            })
            .unwrap_or_default();

        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "## `{}` — `{}`\n\n**Params:** `{}`  \n**Returns:** `{}`{}",
                    entry.fn_name,
                    entry.lsp_method,
                    entry.params_type,
                    entry.return_type,
                    cap_section
                ),
            }),
            range: None,
        });
    }

    // Check capability fields table
    if let Some((name, ty, methods)) = CAPABILITY_FIELDS.iter().find(|(n, _, _)| *n == word) {
        let methods_str = methods.join("`, `");
        return Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: format!(
                    "## `ServerCapabilities::{name}`\n\n**Type:** `{ty}`\n\n\
                     **Activates handler(s):** `{methods_str}`\n\nSet this field in the \
                     `ServerCapabilities` struct returned from `initialize` to tell the client \
                     you support this feature. If unset or `None`, the client will not send \
                     the corresponding requests."
                ),
            }),
            range: None,
        });
    }

    // tower-lsp-max protocol types
    hover_protocol_type(word)
}

fn hover_protocol_type(word: &str) -> Option<Hover> {
    let content = match word {
        "MaxDiagnostic" => {
            "## `MaxDiagnostic`\n\nA `tower-lsp-max` extension diagnostic. Extends the standard \
             LSP `Diagnostic` with:\n- `snapshot_id: SnapshotId` — links the diagnostic to a \
             deterministic snapshot\n- `conformance_vector: Vec<f32>` — process-mining conformance \
             scores per dimension\n- `receipt: Option<Receipt>` — cryptographic provenance receipt\n\n\
             Returned from `max_explain_diagnostic` and included in `MaxDiagnosticParams`."
        }
        "Receipt" => {
            "## `Receipt`\n\nCryptographic provenance receipt from the `tower-lsp-max` runtime. \
             Proves a specific snapshot was observed at a specific logical time. Fields:\n\
             - `snapshot_id: SnapshotId`\n\
             - `timestamp: u64` — monotonic logical clock\n\
             - `hash: [u8; 32]` — SHA-256 of snapshot content\n\nReturned from `max_receipt`."
        }
        "SnapshotId" => {
            "## `SnapshotId`\n\nOpaque identifier for a `DeterministicSnapshot` in the \
             `tower-lsp-max-runtime` mesh. Used to correlate diagnostics, receipts, and \
             conformance vectors across requests."
        }
        "LspService" => {
            "## `LspService`\n\nThe tower `Service` that wraps a `LanguageServer` implementation \
             and handles JSON-RPC routing.\n\nConstruct with:\n\
             - `LspService::new(|client| Backend { client })` — simple case\n\
             - `LspService::build(...).custom_method(...).finish()` — when adding custom \
             JSON-RPC methods beyond the standard trait"
        }
        "Client" => {
            "## `Client`\n\nHandle for making server-initiated requests and notifications to the \
             LSP client.\n\nKey methods:\n\
             - `log_message(MessageType, &str)` — `window/logMessage`\n\
             - `show_message(MessageType, &str)` — `window/showMessage`\n\
             - `publish_diagnostics(Url, Vec<Diagnostic>, version)` — `textDocument/publishDiagnostics`\n\
             - `register_capability(Vec<Registration>)` — `client/registerCapability`\n\
             - `apply_edit(WorkspaceEdit)` — `workspace/applyEdit`"
        }
        // --- Additional LSP primitive types (20+ total) ---
        "PolicyState" => {
            "## `PolicyState`\n\nRuntime policy evaluation state in the `tower-lsp-max` conformance \
             vector. Represents pass/fail/pending status of a declared proof gate at a specific snapshot."
        }
        "GateId" => {
            "## `GateId`\n\nIdentifier for a proof gate registered in the `tower-lsp-max` policy \
             layer. Passed to `max_run_gate` to trigger evaluation."
        }
        "MaxCodeAction" => {
            "## `MaxCodeAction`\n\nA `tower-lsp-max` code action from `max_repair_plan`. Includes \
             standard LSP `CodeAction` fields plus a `transaction_id` for atomic repair transactions."
        }
        "Server" => {
            "## `Server`\n\nTransport driver. Wraps an `LspService` and drives JSON-RPC framing \
             over stdio or a socket.\n\n```rust\nServer::new(stdin, stdout, socket).serve(service).await;\n```"
        }
        "LspServiceBuilder" => {
            "## `LspServiceBuilder`\n\nBuilder from `LspService::build(...)`. Chain \
             `.custom_method(name, handler)` then call `.finish()` to get `(LspService, ClientSocket)`."
        }
        "Position" => {
            "## `Position`\n\nZero-based line and UTF-16 character offset pair.\n\n\
             ```rust\nPosition { line: u32, character: u32 }\n```\n\n\
             **Note:** `character` is a UTF-16 code unit offset, not a byte offset. \
             Use `ropey` or `lsp-textdocument` for conversion."
        }
        "Range" => {
            "## `Range`\n\nHalf-open `[start, end)` span within a document.\n\n\
             ```rust\nRange { start: Position, end: Position }\n```\n\nStart inclusive; end exclusive."
        }
        "Location" => {
            "## `Location`\n\nA URI plus a `Range`. Identifies a specific span within a document.\n\n\
             ```rust\nLocation { uri: Url, range: Range }\n```"
        }
        "TextEdit" => {
            "## `TextEdit`\n\nReplace `range` with `new_text`. To insert: zero-length range. \
             To delete: empty `new_text`.\n\n```rust\nTextEdit { range: Range, new_text: String }\n```"
        }
        "WorkspaceEdit" => {
            "## `WorkspaceEdit`\n\nCollection of `TextEdit`s across multiple documents, optionally \
             with resource operations (create/rename/delete). Returned from `code_action`, `rename`, \
             `formatting`, `will_create_files`, etc."
        }
        "Diagnostic" => {
            "## `Diagnostic`\n\nAn editor annotation (error, warning, hint, info). Published via \
             `client.publish_diagnostics(...)` or returned from pull-model `textDocument/diagnostic`.\n\n\
             **Key fields:** `range`, `severity`, `code`, `source`, `message`, `related_information`"
        }
        "CompletionItem" => {
            "## `CompletionItem`\n\nA single completion suggestion.\n\n**Key fields:**\n\
             - `label` — display text\n- `kind` — icon hint (`METHOD`, `FIELD`, `SNIPPET`, etc.)\n\
             - `insert_text` / `text_edit` — inserted text\n\
             - `insert_text_format` — `PLAIN_TEXT` or `SNIPPET`\n\
             - `documentation` — markdown hover on selection\n\
             - `sort_text` — override alphabetical ordering"
        }
        "Hover" => {
            "## `Hover`\n\nResponse from `textDocument/hover`. Contains `HoverContents` (markdown or \
             plain string) and optional highlight `Range`.\n\n```rust\nHover {\n    contents: \
             HoverContents::Markup(MarkupContent { kind: MarkupKind::Markdown, value: \"...\".into() }),\n    \
             range: None,\n}\n```"
        }
        "CodeAction" => {
            "## `CodeAction`\n\nA quick-fix, refactor, or source action presented to the user.\n\n\
             **Key fields:**\n- `title` — display text\n- `kind` — `QUICKFIX`, `REFACTOR`, `SOURCE`\n\
             - `edit: Option<WorkspaceEdit>` — changes to apply\n\
             - `command: Option<Command>` — command to execute\n\
             - `is_preferred` — offer as the default fix"
        }
        "MessageType" => {
            "## `MessageType`\n\nSeverity for `window/logMessage` and `window/showMessage`.\n\n\
             **Variants:** `ERROR`, `WARNING`, `INFO`, `LOG`\n\n\
             `self.client.log_message(MessageType::INFO, \"...\").await;`"
        }
        "InitializeResult" => {
            "## `InitializeResult`\n\nResponse from `initialize`. The most important field is \
             `capabilities: ServerCapabilities` which tells the client which LSP features this \
             server supports.\n\nAlways return at minimum `ServerCapabilities::default()`."
        }
        "ServerCapabilities" => {
            "## `ServerCapabilities`\n\nDeclares every feature this server supports. The client \
             reads this at startup and only sends requests for declared capabilities.\n\n\
             **Pattern:** Start with `..Default::default()` and set only what you handle. \
             Declaring a capability without implementing the handler causes `method_not_found` errors."
        }
        "TextDocumentSyncKind" => {
            "## `TextDocumentSyncKind`\n\nControls how the client sends document content:\n\
             - `NONE` — no sync\n- `FULL` — full document on every change\n\
             - `INCREMENTAL` — range patches (efficient)\n\n\
             Use `INCREMENTAL` with a rope data structure for large files."
        }
        "InsertTextFormat" => {
            "## `InsertTextFormat`\n\nControls how `CompletionItem::insert_text` is interpreted:\n\
             - `PLAIN_TEXT` — inserted verbatim\n\
             - `SNIPPET` — supports `${1:placeholder}`, `$0` (final cursor), and tab stops"
        }
        "DiagnosticSeverity" => {
            "## `DiagnosticSeverity`\n\nSeverity level for a `Diagnostic`.\n\n\
             - `ERROR` (1) — build-breaking issue\n- `WARNING` (2) — potential problem\n\
             - `INFORMATION` (3) — informational note\n- `HINT` (4) — minor suggestion"
        }
        "MarkupContent" => {
            "## `MarkupContent`\n\nA string with an associated `MarkupKind`.\n\n\
             - `MarkupKind::PlainText` — rendered as-is\n\
             - `MarkupKind::Markdown` — rendered as GitHub Flavored Markdown\n\n\
             Used in `CompletionItem::documentation`, `Hover::contents`, and `SignatureHelp`."
        }
        "CompletionOptions" => {
            "## `CompletionOptions`\n\nDeclares this server's completion capability.\n\n\
             **Key fields:**\n- `trigger_characters` — characters that trigger completion popups\n\
             - `resolve_provider` — whether `completionItem/resolve` will be called for lazy detail\n\
             - `completion_item.label_details_support` — new in LSP 3.17"
        }
        _ => return None,
    };

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: content.to_string(),
        }),
        range: None,
    })
}

/// Extract the identifier word under the cursor position.
pub fn word_at(source: &str, pos: Position) -> Option<String> {
    let line = source.lines().nth(pos.line as usize)?;
    let char_pos = pos.character as usize;
    if char_pos > line.len() {
        return None;
    }
    let before = &line[..char_pos];
    let after = &line[char_pos..];
    let word_start = before
        .rfind(|c: char| !c.is_alphanumeric() && c != '_')
        .map(|i| i + 1)
        .unwrap_or(0);
    let word_end = char_pos
        + after
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(after.len());
    let word = &line[word_start..word_end];
    if word.is_empty() {
        None
    } else {
        Some(word.to_string())
    }
}
