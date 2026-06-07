use tower_lsp_max::lsp_types_max::*;

use crate::Backend;

mod context;
mod table;

use context::{detect_context, CompletionContext};
use table::{domain_label, domain_sort_key};
pub use table::{Domain, MethodEntry, CAPABILITY_FIELDS, METHODS};

// ---------------------------------------------------------------------------
// Public entry point
// ---------------------------------------------------------------------------

/// Completion entry point called from `lib.rs`.
pub async fn compute(backend: &Backend, uri: &Uri, pos: Position) -> Option<CompletionResponse> {
    let doc = backend.docs.get(uri)?;
    if doc.language_id != "rust" {
        return None;
    }
    let source = doc.text.to_string();
    drop(doc);

    let items = get_completions(pos, &source);
    if items.is_empty() {
        None
    } else {
        Some(CompletionResponse::Array(items))
    }
}

/// Determine completions based on cursor context. Public so tests can call it directly.
pub fn get_completions(pos: Position, text: &str) -> Vec<CompletionItem> {
    let ctx = detect_context(text, pos);
    match ctx {
        CompletionContext::ImplLanguageServerMethod { partial } => complete_ls_methods(&partial),
        CompletionContext::ServerCapabilitiesField { partial } => {
            complete_capability_fields(&partial)
        }
        CompletionContext::RpcName { partial } => complete_rpc_names(&partial),
        CompletionContext::LspServiceBuilder => complete_lsp_service_builder(),
        CompletionContext::MessageTypeVariant => complete_message_type_variants(),
        CompletionContext::Unknown => vec![],
    }
}

// Context detection logic has been moved to context.rs

// ---------------------------------------------------------------------------
// Completion builders
// ---------------------------------------------------------------------------

fn complete_ls_methods(partial: &str) -> Vec<CompletionItem> {
    let domain_order = [
        Domain::Lifecycle,
        Domain::TextSync,
        Domain::Editing,
        Domain::Navigation,
        Domain::Symbols,
        Domain::Diagnostics,
        Domain::CodeLens,
        Domain::SemanticTokens,
        Domain::Workspace,
        Domain::Window,
        Domain::Max,
    ];

    let mut items = Vec::new();
    for domain in &domain_order {
        for entry in METHODS.iter().filter(|m| m.domain == *domain).filter(|m| {
            partial.is_empty()
                || m.fn_name.starts_with(partial)
                || m.lsp_method.starts_with(partial)
        }) {
            let params_arg = if entry.params_type == "()" {
                "_: ()".to_string()
            } else {
                format!("params: {}", entry.params_type)
            };
            let stub_body = if entry.return_type.starts_with("Result<") {
                format!(
                    "Err(tower_lsp::jsonrpc::Error::method_not_found()) // TODO: implement {}",
                    entry.fn_name
                )
            } else {
                format!("// TODO: implement {}", entry.fn_name)
            };
            let stub = format!(
                "async fn {}(&self, {}) -> {} {{\n    {}\n}}",
                entry.fn_name, params_arg, entry.return_type, stub_body
            );
            let cap_note = entry
                .capability_field
                .map(|c| format!("\n\n**Requires capability:** `ServerCapabilities::{c}`"))
                .unwrap_or_default();

            // Push the item for Rust function name
            if partial.is_empty() || entry.fn_name.starts_with(partial) {
                items.push(CompletionItem {
                    label: entry.fn_name.to_string(),
                    kind: Some(CompletionItemKind::METHOD),
                    detail: Some(format!("{} — {}", entry.lsp_method, domain_label(domain))),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            "**`{}`**\n\nParams: `{}`  \nReturns: `{}`{}",
                            entry.lsp_method, entry.params_type, entry.return_type, cap_note
                        ),
                    })),
                    insert_text: Some(stub.clone()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    sort_text: Some(format!("{:02}_{}", domain_sort_key(domain), entry.fn_name)),
                    ..Default::default()
                });
            }

            // Push the item for standard LSP method name (e.g. textDocument/completion)
            if partial.is_empty() || entry.lsp_method.starts_with(partial) {
                items.push(CompletionItem {
                    label: entry.lsp_method.to_string(),
                    kind: Some(CompletionItemKind::METHOD),
                    detail: Some(format!("{} — {}", entry.fn_name, domain_label(domain))),
                    documentation: Some(Documentation::MarkupContent(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: format!(
                            "**`{}`**\n\nParams: `{}`  \nReturns: `{}`{}",
                            entry.lsp_method, entry.params_type, entry.return_type, cap_note
                        ),
                    })),
                    insert_text: Some(stub.clone()),
                    insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                    sort_text: Some(format!(
                        "{:02}_{}",
                        domain_sort_key(domain),
                        entry.lsp_method
                    )),
                    ..Default::default()
                });
            }
        }
    }
    items
}

fn complete_rpc_names(partial: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for entry in METHODS {
        let method = entry.lsp_method;
        if partial.is_empty() || method.starts_with(partial) {
            items.push(CompletionItem {
                label: method.to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: Some(format!(
                    "{} — {}",
                    entry.fn_name,
                    domain_label(&entry.domain)
                )),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        "**`{}`**\n\nRust handler: `async fn {}(&self, params: {}) -> {}`",
                        entry.lsp_method, entry.fn_name, entry.params_type, entry.return_type
                    ),
                })),
                insert_text: Some(method.to_string()),
                insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
                ..Default::default()
            });
        }
    }
    items
}

fn complete_capability_fields(partial: &str) -> Vec<CompletionItem> {
    CAPABILITY_FIELDS
        .iter()
        .filter(|(name, _, _)| partial.is_empty() || name.starts_with(partial))
        .map(|(name, ty, methods)| {
            let methods_str = methods.join("`, `");
            CompletionItem {
                label: name.to_string(),
                kind: Some(CompletionItemKind::FIELD),
                detail: Some(ty.to_string()),
                documentation: Some(Documentation::MarkupContent(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!(
                        "**`ServerCapabilities::{name}`**\n\nType: `{ty}`\n\nEnables handler(s): `{methods_str}`"
                    ),
                })),
                insert_text: Some(format!("{name}: Some(${{1:...}}),\n")),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                ..Default::default()
            }
        })
        .collect()
}

fn complete_lsp_service_builder() -> Vec<CompletionItem> {
    vec![
        CompletionItem {
            label: "custom_method".to_string(),
            kind: Some(CompletionItemKind::METHOD),
            detail: Some("Add a custom JSON-RPC method to the service".to_string()),
            documentation: Some(Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: "`.custom_method(\"namespace/method\", Backend::handler_fn)`\n\nThe handler must be `async fn(&self, params: P) -> jsonrpc::Result<R>` where `R: Serialize`.".to_string(),
            })),
            insert_text: Some("custom_method(\"${1:namespace/method}\", ${2:Backend::handler})".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        },
        CompletionItem {
            label: "finish".to_string(),
            kind: Some(CompletionItemKind::METHOD),
            detail: Some("Build the LspService".to_string()),
            insert_text: Some("finish()".to_string()),
            insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
            ..Default::default()
        },
    ]
}

fn complete_message_type_variants() -> Vec<CompletionItem> {
    [
        ("ERROR", "Error message — shown as error in client log"),
        ("WARNING", "Warning message"),
        ("INFO", "Informational message"),
        ("LOG", "Log/debug message"),
    ]
    .iter()
    .map(|(name, doc)| CompletionItem {
        label: name.to_string(),
        kind: Some(CompletionItemKind::ENUM_MEMBER),
        detail: Some(doc.to_string()),
        insert_text: Some(name.to_string()),
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        ..Default::default()
    })
    .collect()
}
