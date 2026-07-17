// Hook evaluation at the compiled hook level

use crate::term::Triple;
use crate::TripleStore;
use serde::{Deserialize, Serialize};

use super::condition::evaluate_condition;
use super::{CompiledHook, GraphDelta, HookVerdict, HookVerdictRecord};

/// Evaluates CompiledHooks against the current state and delta.
///
/// # Complexity
/// O(|H| * C) where |H| = number of hooks, C = per-condition evaluation cost
/// (typically O(|F|) where |F| = triple store size, dominated by SHACL/SPARQL)
pub fn evaluate_hooks(
    hooks: &[CompiledHook],
    post_state: &TripleStore,
    delta: &GraphDelta,
    history: &[GraphDelta],
) -> Result<Vec<HookVerdictRecord>, String> {
    let mut records = Vec::with_capacity(hooks.len());
    for hook in hooks {
        let gated = match hook.on.as_str() {
            "assert" => delta.additions.is_empty(),
            "retract" => delta.removals.is_empty(),
            _ => false,
        };
        let (verdict, diagnostics) = if gated {
            (HookVerdict::Gated, None)
        } else {
            let (fired, diag) =
                evaluate_condition(&hook.condition, post_state, delta, history, &hook.iri)?;
            let verdict = if fired {
                HookVerdict::Fired
            } else {
                HookVerdict::NotFired
            };
            (verdict, diag)
        };

        let condition_hash = hook.condition.condition_hash()?;
        records.push(HookVerdictRecord {
            hook_id: hook.id,
            hook_iri: hook.iri.clone(),
            hook_name: hook.name.clone(),
            condition_kind: hook.condition.kind().to_string(),
            condition_hash,
            verdict,
            effect: hook.effect.clone(),
            action_iri: hook.action.clone(),
            diagnostics,
            delta_hash: None,
            idempotency_key: None,
        });
    }
    Ok(records)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionOutcome {
    pub additions: Vec<Triple>,
    pub deletions: Vec<Triple>,
}
