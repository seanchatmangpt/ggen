use crate::server::GgenLanguageServer;
use lsp_max::jsonrpc::Result;
use lsp_max::lsp_types_max::*;

/// `textDocument/semanticTokens/full` — R80-1 `tokens::TokensEngine` over the
/// open buffer of a Rust document. The engine is rebuilt from the workspace
/// ontology at `initialize`, and its legend is the one advertised in
/// `ServerCapabilities`, so `data` indices decode against the advertised
/// legend by construction.
pub async fn handle_full(
    server: &GgenLanguageServer, params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let uri = &params.text_document.uri;
    let Some(content) = server.state.get_document(uri).await else {
        return Ok(None);
    };
    if !uri.path().as_str().ends_with(".rs") {
        return Ok(None);
    }
    let mut engine = server.tokens_engine.lock().await;
    let tokens = engine.full(uri.as_str(), &content);
    Ok(Some(SemanticTokensResult::Tokens(to_wire_tokens(tokens))))
}

/// `textDocument/semanticTokens/full/delta` — the engine's per-document
/// resultId + relative-edit diff (LSP 3.16). An unknown `previous_result_id`
/// yields a single full-replacement edit from the engine.
pub async fn handle_full_delta(
    server: &GgenLanguageServer, params: SemanticTokensDeltaParams,
) -> Result<Option<SemanticTokensFullDeltaResult>> {
    let uri = &params.text_document.uri;
    let Some(content) = server.state.get_document(uri).await else {
        return Ok(None);
    };
    if !uri.path().as_str().ends_with(".rs") {
        return Ok(None);
    }
    let mut engine = server.tokens_engine.lock().await;
    let delta = engine.delta(uri.as_str(), &params.previous_result_id, &content);
    Ok(Some(
        SemanticTokensFullDeltaResult::TokensDelta(to_wire_delta(delta)),
    ))
}

// -- wire conversion ---------------------------------------------------------
// `tokens.rs` (R80-1) is built against `lsp_max_protocol::lsp_3_18` types; the
// server surface speaks `lsp_max::lsp_types_max`. Same LSP 3.18 wire shapes,
// converted field-by-field at the seam.

fn to_wire_tokens(t: lsp_max_protocol::lsp_3_18::SemanticTokens) -> SemanticTokens {
    SemanticTokens {
        result_id: t.result_id,
        data: ggen_lsp_data(t.data),
    }
}

/// The wire type stores structured tokens; the engine stores the flat
/// bit-packed `Vec<u32>` encoding. `tokens::decode` is the inverse of
/// `tokens::encode`, so the conversion is lossless round-trip-verified in
/// tests/tokens_delta_test.rs.
fn ggen_lsp_data(data: Vec<u32>) -> Vec<SemanticToken> {
    crate::tokens::decode(&data)
        .into_iter()
        .map(|t| SemanticToken {
            delta_line: t.line,
            delta_start: t.start,
            length: t.length,
            token_type: t.token_type,
            token_modifiers_bitset: t.modifiers,
        })
        .collect()
}

fn to_wire_delta(d: lsp_max_protocol::lsp_3_18::SemanticTokensDelta) -> SemanticTokensDelta {
    SemanticTokensDelta {
        result_id: d.result_id,
        edits: d.edits.into_iter().map(to_wire_edit).collect(),
    }
}

fn to_wire_edit(e: lsp_max_protocol::lsp_3_18::SemanticTokensEdit) -> SemanticTokensEdit {
    SemanticTokensEdit {
        start: e.start,
        delete_count: e.delete_count,
        data: e
            .data
            .map(|data| {
                crate::tokens::decode(&data)
                    .into_iter()
                    .map(|t| SemanticToken {
                        delta_line: t.line,
                        delta_start: t.start,
                    length: t.length,
                    token_type: t.token_type,
                    token_modifiers_bitset: t.modifiers,
                    })
                    .collect()
            }),
    }
}
