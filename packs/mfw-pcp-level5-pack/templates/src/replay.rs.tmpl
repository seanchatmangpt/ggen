//! Generated deterministic receipt-pair and standing verifier.

use crate::receipts::{CloseReceipt, OpenReceipt};
use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ReplayError {
    #[error("sequence drift")]
    SequenceDrift,
    #[error("close receipt does not reference the exact open receipt")]
    OpenReferenceDrift,
    #[error("action digest drift")]
    ActionDrift,
    #[error("bundle digest drift")]
    BundleDrift,
    #[error("effect did not succeed")]
    EffectFailed,
    #[error("postcondition was not satisfied")]
    PostconditionFailed,
    #[error("goal was not satisfied")]
    GoalNotSatisfied,
}

pub fn verify_pair(open: &OpenReceipt, open_digest: &str, close: &CloseReceipt) -> Result<(), ReplayError> {
    if open.sequence != close.sequence { return Err(ReplayError::SequenceDrift); }
    if open_digest != close.open_digest { return Err(ReplayError::OpenReferenceDrift); }
    if open.action_digest != close.action_digest { return Err(ReplayError::ActionDrift); }
    if open.bundle_digest != close.bundle_digest { return Err(ReplayError::BundleDrift); }
    Ok(())
}

pub fn close_standing(open: &OpenReceipt, open_digest: &str, close: &CloseReceipt, goal_satisfied: bool) -> Result<(), ReplayError> {
    verify_pair(open, open_digest, close)?;
    if !close.effect_succeeded { return Err(ReplayError::EffectFailed); }
    if !close.postcondition_satisfied { return Err(ReplayError::PostconditionFailed); }
    if !goal_satisfied { return Err(ReplayError::GoalNotSatisfied); }
    Ok(())
}
