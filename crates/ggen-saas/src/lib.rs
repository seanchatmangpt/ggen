//! SaaS tier management and quota enforcement for ggen

pub mod quotas;
pub mod limits;
pub mod billing;
pub mod errors;
pub mod tier;

pub use quotas::{QuotaManager, QuotaState};
pub use limits::{Limits, TierLimits};
pub use billing::{BillingCycle, UsageAccumulator};
pub use errors::SaasError;
pub use tier::{Tier, TierHierarchy};

/// Result type for SaaS operations
pub type SaasResult<T> = Result<T, SaasError>;
