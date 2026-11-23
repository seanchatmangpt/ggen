//! Gate-1 remediation-bound vocabulary for zcode-cli POWL/OCEL autonomic loops.
//!
//! This module is a **contract stub**: it names the bounded-refusal caps that
//! sibling `zcode-cli` enforces (`maxRemediationAttempts`, signature retry
//! budgets, write-scope conflicts). It does **not** execute the out-of-repo CLI
//! and must never be treated as evidence that zcode remediations are `ALIVE`.
//!
//! See `docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md`.

/// Default layout-court remediation budget (`ui-layout-court.ts`).
pub const DEFAULT_MAX_REMEDIATION_ATTEMPTS: u32 = 3;

/// Default per-signature retry budget (`SignatureRetryLimiter` in zcode-cli).
pub const DEFAULT_MAX_RETRIES_PER_SIGNATURE: u32 = 3;

/// Default foundry same-signature retry cap (`foundry-loop.ts`).
pub const DEFAULT_FOUNDRY_SAME_SIGNATURE_RETRIES: u32 = 2;

/// Typed refusals for remediation **bounds** (not geometry — see [`super::taffy_aot::AotRefusal`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RemediationBoundRefusal {
    /// `max_attempts == 0` or bound omitted where a positive bound is required.
    UnboundedRemediation { detail: &'static str },
    /// Attempt index exhausted the declared budget.
    RetryBudgetExhausted {
        attempts: u32,
        max_attempts: u32,
        signature: &'static str,
    },
    /// Unordered mutating write scopes overlap (maps to zcode `WRITE_SCOPE_CONFLICT`).
    WriteScopeConflict { a: &'static str, b: &'static str },
    /// Autonomic cycle index exceeded `max_cycles`.
    MaxCyclesExceeded { cycle: u32, max_cycles: u32 },
}

/// Machine-stable refusal code strings (parity with zcode OCEL / court standing).
impl RemediationBoundRefusal {
    pub fn code(&self) -> &'static str {
        match self {
            Self::UnboundedRemediation { .. } => "REFUSED_UNBOUNDED_REMEDIATION",
            Self::RetryBudgetExhausted { .. } => "RETRY_BUDGET_EXHAUSTED",
            Self::WriteScopeConflict { .. } => "WRITE_SCOPE_CONFLICT",
            Self::MaxCyclesExceeded { .. } => "REFUSED_MAX_CYCLES_EXCEEDED",
        }
    }
}

/// Admit a remediation budget. Zero (or absent-as-zero) is an explicit refuse —
/// never a silent unlimited loop.
pub fn admit_remediation_bound(max_attempts: u32) -> Result<u32, RemediationBoundRefusal> {
    if max_attempts == 0 {
        return Err(RemediationBoundRefusal::UnboundedRemediation {
            detail: "max_remediation_attempts must be >= 1; unbounded autonomic rewrite is refused",
        });
    }
    Ok(max_attempts)
}

/// Record one remediation attempt against a signature budget.
///
/// Returns `Ok(remaining)` when `attempts_used < max_attempts`, else the typed
/// exhausted refusal. Does not perform I/O or call zcode-cli.
pub fn record_remediation_attempt(
    attempts_used: u32, max_attempts: u32, signature: &'static str,
) -> Result<u32, RemediationBoundRefusal> {
    let max = admit_remediation_bound(max_attempts)?;
    if attempts_used >= max {
        return Err(RemediationBoundRefusal::RetryBudgetExhausted {
            attempts: attempts_used,
            max_attempts: max,
            signature,
        });
    }
    Ok(max - attempts_used - 1)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_layout_budget_matches_zcode_ui_layout_court() {
        assert_eq!(DEFAULT_MAX_REMEDIATION_ATTEMPTS, 3);
        assert_eq!(
            admit_remediation_bound(DEFAULT_MAX_REMEDIATION_ATTEMPTS),
            Ok(3)
        );
    }

    #[test]
    fn zero_budget_is_explicit_unbounded_refusal() {
        let err = admit_remediation_bound(0).expect_err("zero must refuse");
        assert_eq!(err.code(), "REFUSED_UNBOUNDED_REMEDIATION");
        assert!(matches!(
            err,
            RemediationBoundRefusal::UnboundedRemediation { .. }
        ));
    }

    #[test]
    fn exhausted_signature_budget_refuses_not_alive() {
        let err = record_remediation_attempt(3, 3, "layout_overflow@320")
            .expect_err("exhausted budget must refuse");
        assert_eq!(err.code(), "RETRY_BUDGET_EXHAUSTED");
        assert_eq!(
            err,
            RemediationBoundRefusal::RetryBudgetExhausted {
                attempts: 3,
                max_attempts: 3,
                signature: "layout_overflow@320",
            }
        );
    }

    #[test]
    fn write_scope_conflict_code_is_stable() {
        let err = RemediationBoundRefusal::WriteScopeConflict {
            a: "xor-branch",
            b: "loop-branch",
        };
        assert_eq!(err.code(), "WRITE_SCOPE_CONFLICT");
    }
}
