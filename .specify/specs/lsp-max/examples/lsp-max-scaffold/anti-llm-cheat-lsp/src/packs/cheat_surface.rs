use lsp_max::{EvalBudget, Rule, RulePack};

/// Rule pack `cheat-surface@1.0.0` v1.0.0 — compiled from lsp.ttl.
/// Rules baked into the binary; no runtime TOML loading.
pub fn pack() -> RulePack {
    RulePack {
        id:         "cheat-surface@1.0.0".to_string(),
        version:    "1.0.0".to_string(),
        depends_on: vec![],
        rules: vec![
            Rule {
                id:            "ANTI-LLM-SURFACE-002".to_string(),
                name:          "no-wasm4pm-direct".to_string(),
                severity:      "warning".to_string(),
                pattern:       "use wasm4pm::".to_string(),
                message:       "Direct wasm4pm import — use the abstraction layer instead".to_string(),
                rationale:     "Direct wasm4pm imports bypass the admission layer".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-SURFACE-001".to_string(),
                name:          "no-tower-lsp".to_string(),
                severity:      "error".to_string(),
                pattern:       "tower-lsp".to_string(),
                message:       "Plain tower-lsp found — all LSP hosts must use lsp-max".to_string(),
                rationale:     "tower-lsp lacks the ConformanceVector, max/ protocol, and receipt infrastructure".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
        ],
    }
}