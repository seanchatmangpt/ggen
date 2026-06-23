use lsp_max::{EvalBudget, Rule, RulePack};

/// Rule pack `cheat-receipts@1.0.0` v1.0.0 — compiled from lsp.ttl.
/// Rules baked into the binary; no runtime TOML loading.
pub fn pack() -> RulePack {
    RulePack {
        id:         "cheat-receipts@1.0.0".to_string(),
        version:    "1.0.0".to_string(),
        depends_on: vec![],
        rules: vec![
            Rule {
                id:            "ANTI-LLM-RECEIPT-003".to_string(),
                name:          "no-routing-claim".to_string(),
                severity:      "error".to_string(),
                pattern:       "Routing to PackPlan".to_string(),
                message:       "Route log treated as route execution — a print of the route does not prove execution".to_string(),
                rationale:     "Route logs are observational; execution requires a receipt".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-RECEIPT-002".to_string(),
                name:          "no-bypassed-compat".to_string(),
                severity:      "error".to_string(),
                pattern:       "[\\x22]bypassed_compat[\\x22]: true".to_string(),
                message:       "Compatibility bypass flag found in receipt or config".to_string(),
                rationale:     "bypassed_compat:true invalidates the receipt chain".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-RECEIPT-001".to_string(),
                name:          "no-test-result-as-receipt".to_string(),
                severity:      "error".to_string(),
                pattern:       "test result: ok".to_string(),
                message:       "Test stdout treated as receipt — test output is not a cryptographically signed receipt".to_string(),
                rationale:     "receipts require Ed25519 signature and BLAKE3 input/output hashes".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
        ],
    }
}