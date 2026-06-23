use lsp_max::{EvalBudget, Rule, RulePack};

/// Rule pack `cheat-tests@1.0.0` v1.0.0 — compiled from lsp.ttl.
/// Rules baked into the binary; no runtime TOML loading.
pub fn pack() -> RulePack {
    RulePack {
        id:         "cheat-tests@1.0.0".to_string(),
        version:    "1.0.0".to_string(),
        depends_on: vec![],
        rules: vec![
            Rule {
                id:            "ANTI-LLM-VERSION-001".to_string(),
                name:          "no-default-version".to_string(),
                severity:      "warning".to_string(),
                pattern:       "version = [\\x22]1\\.0\\.0[\\x22]".to_string(),
                message:       "Default template version '1.0.0' found in project configuration".to_string(),
                rationale:     "Default versions indicate a project was never properly versioned".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-TEST-001".to_string(),
                name:          "no-static-scan-as-proof".to_string(),
                severity:      "error".to_string(),
                pattern:       "static scan as route proof".to_string(),
                message:       "Static scan substituted for route proof".to_string(),
                rationale:     "Absence of bad strings does not prove mutation was safely routed".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
        ],
    }
}