use tower_lsp_max::lsp_types_max::*;

use crate::handlers::completions::METHODS;
use crate::handlers::diagnostics::analysis::{analyze_impl_block, ImplAnalysis};
use crate::handlers::diagnostics::levenshtein_distance;

// Diagnostic source tag shown in the IDE
pub const SOURCE: &str = "tower-lsp-max-playground";

// Diagnostic codes
pub const CAPABILITY_WITHOUT_METHOD: &str = "TLM001";
pub const METHOD_WITHOUT_CAPABILITY: &str = "TLM002";
pub const MISSING_MANDATORY_METHOD: &str = "TLM003";
pub const MISSING_INITIALIZED_OVERRIDE: &str = "TLM004";
pub const TYPO_IN_METHOD_NAME: &str = "TLM005";
pub const INVALID_RPC_NAME: &str = "TLM006";

/// Public entry point taking raw source text. Matches the spec signature so tests can call it directly.
pub fn get_diagnostics(text: &str, _uri: &Uri) -> Vec<Diagnostic> {
    let ast = match syn::parse_file(text) {
        Ok(ast) => ast,
        Err(_) => return vec![],
    };
    let analysis = analyze_impl_block(&ast);
    build_diagnostics(analysis)
}

pub fn build_diagnostics(analysis: ImplAnalysis) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    let trait_range = analysis
        .impl_trait_span
        .map(to_lsp_range)
        .unwrap_or_default();

    // Check for typos in method names
    for method in &analysis.overridden_methods {
        let is_valid = METHODS.iter().any(|m| m.fn_name == method.name.as_str());
        if !is_valid {
            let mut min_dist = usize::MAX;
            let mut closest_match: Option<&'static str> = None;
            for entry in METHODS {
                let dist = levenshtein_distance(&method.name, entry.fn_name);
                if dist < min_dist {
                    min_dist = dist;
                    closest_match = Some(entry.fn_name);
                }
            }

            let msg = if let Some(closest) = closest_match {
                if min_dist <= 4 {
                    format!(
                        "`{}` is not a valid method of the `LanguageServer` trait. Did you mean `{}`?",
                        method.name, closest
                    )
                } else {
                    format!(
                        "`{}` is not a valid method of the `LanguageServer` trait.",
                        method.name
                    )
                }
            } else {
                format!(
                    "`{}` is not a valid method of the `LanguageServer` trait.",
                    method.name
                )
            };

            diags.push(make_diag(
                to_lsp_range(method.span),
                DiagnosticSeverity::ERROR,
                TYPO_IN_METHOD_NAME,
                &msg,
            ));
        }
    }

    // TLM003: missing mandatory initialize
    if !analysis.has_initialize {
        diags.push(make_diag(
            trait_range,
            DiagnosticSeverity::ERROR,
            MISSING_MANDATORY_METHOD,
            "`initialize` is not overridden. It is mandatory — the default returns \
             `Error::method_not_found()`. Add:\n```rust\nasync fn initialize(&self, \
             params: InitializeParams) -> Result<InitializeResult> {\n    \
             Ok(InitializeResult { capabilities: ServerCapabilities::default(), \
             ..Default::default() })\n}\n```",
        ));
    }

    // TLM003: missing mandatory shutdown
    if !analysis.has_shutdown {
        diags.push(make_diag(
            trait_range,
            DiagnosticSeverity::ERROR,
            MISSING_MANDATORY_METHOD,
            "`shutdown` is not overridden. It is mandatory. Add:\n```rust\n\
             async fn shutdown(&self) -> Result<()> { Ok(()) }\n```",
        ));
    }

    // TLM004: initialized not overridden (hint)
    if !analysis.has_initialized {
        diags.push(make_diag(
            trait_range,
            DiagnosticSeverity::HINT,
            MISSING_INITIALIZED_OVERRIDE,
            "`initialized` is not overridden. The default is a no-op. Most servers override \
             it to log a ready message or register dynamic capabilities.",
        ));
    }

    // TLM001: capability declared but handler not overridden
    for cap in &analysis.declared_capabilities {
        let required_methods = methods_for_capability(&cap.name);
        for method in &required_methods {
            if !analysis
                .overridden_methods
                .iter()
                .any(|m| m.name == *method)
            {
                let lsp_method = METHODS
                    .iter()
                    .find(|m| m.fn_name == *method)
                    .map(|m| m.lsp_method)
                    .unwrap_or("?");
                diags.push(make_diag(
                    to_lsp_range(cap.span),
                    DiagnosticSeverity::WARNING,
                    CAPABILITY_WITHOUT_METHOD,
                    &format!(
                        "`ServerCapabilities::{}` is declared but `{}` is not overridden. \
                         The client will send `{}` requests and receive `method not found` errors.",
                        cap.name, method, lsp_method
                    ),
                ));
            }
        }
    }

    // TLM002: handler overridden but capability not declared
    for method in &analysis.overridden_methods {
        if let Some(entry) = METHODS.iter().find(|m| m.fn_name == method.name.as_str()) {
            if let Some(cap_field) = entry.capability_field {
                if !analysis
                    .declared_capabilities
                    .iter()
                    .any(|c| c.name == cap_field)
                {
                    diags.push(make_diag(
                        to_lsp_range(method.span),
                        DiagnosticSeverity::WARNING,
                        METHOD_WITHOUT_CAPABILITY,
                        &format!(
                            "`{}` is overridden but `ServerCapabilities::{}` is \
                             not set. The client will never send `{}` — the override is dead code.",
                            method.name, cap_field, entry.lsp_method
                        ),
                    ));
                }
            }
        }
    }

    // TLM006: invalid RPC name
    for invalid in &analysis.invalid_rpc_names {
        diags.push(make_diag(
            to_lsp_range(invalid.span),
            DiagnosticSeverity::ERROR,
            INVALID_RPC_NAME,
            &invalid.message,
        ));
    }

    diags
}

fn methods_for_capability(field: &str) -> Vec<&'static str> {
    METHODS
        .iter()
        .filter(|m| m.capability_field == Some(field))
        .map(|m| m.fn_name)
        .collect()
}

pub fn make_diag(
    range: Range,
    severity: DiagnosticSeverity,
    code: &str,
    message: &str,
) -> Diagnostic {
    Diagnostic {
        range,
        severity: Some(severity),
        code: Some(NumberOrString::String(code.to_string())),
        source: Some(SOURCE.to_string()),
        message: message.to_string(),
        ..Default::default()
    }
}

pub fn to_lsp_position(lc: proc_macro2::LineColumn) -> Position {
    Position {
        line: (lc.line - 1) as u32,
        character: lc.column as u32,
    }
}

pub fn to_lsp_range(span: proc_macro2::Span) -> Range {
    Range {
        start: to_lsp_position(span.start()),
        end: to_lsp_position(span.end()),
    }
}
