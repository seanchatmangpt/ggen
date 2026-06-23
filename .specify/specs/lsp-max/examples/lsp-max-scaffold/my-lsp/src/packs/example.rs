use lsp_max::{EvalBudget, Rule, RulePack};

/// Rule pack `example@1.0.0` v1.0.0 — compiled from lsp.ttl.
/// Rules baked into the binary; no runtime TOML loading.
pub fn pack() -> RulePack {
    RulePack {
        id:         "example@1.0.0".to_string(),
        version:    "1.0.0".to_string(),
        depends_on: vec![],
        rules: vec![
            Rule {
                id:            "EXAMPLE-001".to_string(),
                name:          "no-unwrap".to_string(),
                severity:      "error".to_string(),
                pattern:       "\\.(unwrap|expect)\\(\\)".to_string(),
                message:       "Replace unwrap()/expect() with ? or map_err".to_string(),
                rationale:     "Panics abort async tasks; use Result propagation".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
        ],
    }
}