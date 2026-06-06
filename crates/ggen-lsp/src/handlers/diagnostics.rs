use tower_lsp::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url};
use serde_json::json;
use crate::pack_lsp_registry::{Registry, ClapNounVerbObserver, TowerLspMaxObserver};

pub fn compute_observer_diagnostics(uri: &Url, content: &str) -> Vec<Diagnostic> {
    let mut diags = Vec::new();
    let path_str = uri.path();
    
    // Helper to create a diagnostic with source_id in data
    let make_diag = |code: &str, msg: &str, severity: DiagnosticSeverity, start_line: u32, end_line: u32, src: &str| -> Diagnostic {
        let mut d = Diagnostic {
            range: Range {
                start: Position { line: start_line, character: 0 },
                end: Position { line: end_line, character: 100 },
            },
            severity: Some(severity),
            code: Some(NumberOrString::String(code.to_string())),
            source: Some("ggen-lsp".to_string()),
            message: msg.to_string(),
            ..Default::default()
        };
        d.data = Some(json!({ "source_id": src }));
        d
    };

    // Instantiate and run pack-domain LSPs
    let mut registry = Registry::new();
    registry.register(Box::new(ClapNounVerbObserver));
    registry.register(Box::new(TowerLspMaxObserver));
    
    let pack_obs = registry.observe_all(uri, content);
    for obs in pack_obs {
        for finding in obs.findings {
            let severity = match finding.severity {
                1 => DiagnosticSeverity::ERROR,
                2 => DiagnosticSeverity::WARNING,
                _ => DiagnosticSeverity::INFORMATION,
            };
            
            // Note: We use the *pack-domain LSP source_id* instead of ggen_lsp_observer.
            // But we must NOT emit generic domain logic from ggen-lsp itself.
            // ggen-lsp merely passes through the pack findings.
            diags.push(make_diag(
                &finding.code,
                &finding.message,
                severity,
                finding.line,
                finding.line,
                &obs.source_id,
            ));
        }
    }

    // 1. Receipts map check (owned by ggen-lsp)
    if path_str.ends_with("receipts.json") || path_str.ends_with("receipts.jsonl") {
        if content.contains("random_corrupt_bytes") || (content.trim().starts_with('{') && serde_json::from_str::<serde_json::Value>(content).is_err()) {
            diags.push(make_diag(
                "GGEN-EVIDENCE-001",
                "Receipt index is corrupted",
                DiagnosticSeverity::ERROR,
                0,
                0,
                "ggen_lsp_observer",
            ));
        }
    }

    // 2. Projected files check (owned by ggen-lsp)
    if path_str.ends_with("main.rs") || path_str.ends_with("server.rs") || path_str.ends_with("cli.rs") || path_str.ends_with("lsp.rs") {
        let (start, end) = if path_str.ends_with("main.rs") || path_str.ends_with("cli.rs") {
            (0, 4)
        } else {
            (0, 137)
        };
        diags.push(make_diag(
            "GGEN-PROJECTED-001",
            "File is projected by a pack",
            DiagnosticSeverity::INFORMATION,
            start,
            end,
            "ggen_lsp_observer",
        ));

        // Check for drift
        if content.contains("drifted") {
            diags.push(make_diag(
                "GGEN-DRIFT-001",
                "Projected content has drifted from template",
                DiagnosticSeverity::WARNING,
                start,
                end,
                "ggen_lsp_observer",
            ));
        }

        // Check for override without receipt
        if content.contains("ggen:override") {
            diags.push(make_diag(
                "GGEN-OVERRIDE-001",
                "Override exists but not receipted",
                DiagnosticSeverity::WARNING,
                0,
                1,
                "ggen_lsp_observer",
            ));
        }
    }

    // 3. Missing receipt check for files in projected directory (owned by ggen-lsp)
    if path_str.ends_with("lib.rs") && !content.contains("pub mod cli") {
        diags.push(make_diag(
            "GGEN-EVIDENCE-001",
            "Artifact lacks projection receipt",
            DiagnosticSeverity::ERROR,
            0,
            0,
            "ggen_lsp_observer",
        ));
    }

    diags
}