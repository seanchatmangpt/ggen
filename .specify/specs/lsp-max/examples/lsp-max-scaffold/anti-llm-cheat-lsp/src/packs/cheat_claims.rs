use lsp_max::{EvalBudget, Rule, RulePack};

/// Rule pack `cheat-claims@1.0.0` v1.0.0 — compiled from lsp.ttl.
/// Rules baked into the binary; no runtime TOML loading.
pub fn pack() -> RulePack {
    RulePack {
        id:         "cheat-claims@1.0.0".to_string(),
        version:    "1.0.0".to_string(),
        depends_on: vec![],
        rules: vec![
            Rule {
                id:            "ANTI-LLM-CLAIM-004".to_string(),
                name:          "no-successfully-proven".to_string(),
                severity:      "error".to_string(),
                pattern:       "successfully proven".to_string(),
                message:       "Forbidden proof claim: 'successfully proven'".to_string(),
                rationale:     "Proof requires a receipt chain, not a string assertion".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-CLAIM-003".to_string(),
                name:          "no-all-gaps-resolved".to_string(),
                severity:      "error".to_string(),
                pattern:       "all gaps resolved".to_string(),
                message:       "Forbidden completion claim: 'all gaps resolved'".to_string(),
                rationale:     "Gap claims require process evidence; text assertions are insufficient".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-CLAIM-002".to_string(),
                name:          "no-fully-admitted".to_string(),
                severity:      "error".to_string(),
                pattern:       "fully admitted".to_string(),
                message:       "Forbidden completion claim: 'fully admitted'".to_string(),
                rationale:     "Completion claims without process evidence violate AGENTS.md bounded-status rules".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-CLAIM-001".to_string(),
                name:          "no-victory-confirmed".to_string(),
                severity:      "error".to_string(),
                pattern:       "Victory confirmed".to_string(),
                message:       "Forbidden victory claim: 'Victory confirmed'".to_string(),
                rationale:     "Completion claims without process evidence violate AGENTS.md bounded-status rules".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
        ],
    }
}