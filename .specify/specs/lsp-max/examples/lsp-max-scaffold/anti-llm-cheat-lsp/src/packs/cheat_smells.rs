use lsp_max::{EvalBudget, Rule, RulePack};

/// Rule pack `cheat-smells@1.0.0` v1.0.0 — compiled from lsp.ttl.
/// Rules baked into the binary; no runtime TOML loading.
pub fn pack() -> RulePack {
    RulePack {
        id:         "cheat-smells@1.0.0".to_string(),
        version:    "1.0.0".to_string(),
        depends_on: vec![],
        rules: vec![
            Rule {
                id:            "ANTI-LLM-OCEL-002".to_string(),
                name:          "no-ocel-trigger-002".to_string(),
                severity:      "error".to_string(),
                pattern:       "ANTI-LLM-OCEL-002-TRIGGER".to_string(),
                message:       "Receipt claim exists without OCEL object/event binding".to_string(),
                rationale:     "Receipt claims require a bound OCEL object to be admissible".to_string(),
                eval_budget:   EvalBudget::Background,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-OCEL-001".to_string(),
                name:          "no-ocel-trigger-001".to_string(),
                severity:      "error".to_string(),
                pattern:       "ANTI-LLM-OCEL-001-TRIGGER".to_string(),
                message:       "Diagnostic emitted without corresponding OCEL process event".to_string(),
                rationale:     "Every diagnostic must be traceable to an OCEL event in the process log".to_string(),
                eval_budget:   EvalBudget::Background,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-STRANGE-003".to_string(),
                name:          "no-path-leak".to_string(),
                severity:      "error".to_string(),
                pattern:       "Path was:".to_string(),
                message:       "Raw path leak found — diagnostic leaks filesystem path into message".to_string(),
                rationale:     "Leaking paths through diagnostics exposes local filesystem layout".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-STRANGE-002".to_string(),
                name:          "no-content-leak".to_string(),
                severity:      "error".to_string(),
                pattern:       "Content was:".to_string(),
                message:       "Raw content leak found — diagnostic leaks document text into message".to_string(),
                rationale:     "Leaking raw content through diagnostics exposes user source to log infrastructure".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
            Rule {
                id:            "ANTI-LLM-STRANGE-001".to_string(),
                name:          "no-clap-debug".to_string(),
                severity:      "error".to_string(),
                pattern:       "CLAP-DEBUG".to_string(),
                message:       "Debug diagnostic name found in admissible path".to_string(),
                rationale:     "CLAP-DEBUG constructs are test scaffolding and must not appear in production paths".to_string(),
                eval_budget:   EvalBudget::Sync,
                path_globs:    vec![],
                exclude_globs: vec![],
            },
        ],
    }
}