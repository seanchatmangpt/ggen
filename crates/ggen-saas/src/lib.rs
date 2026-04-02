//! SaaS tier management and quota enforcement for ggen

pub mod billing;
pub mod errors;
pub mod factory_paas;
pub mod limits;
pub mod quotas;
pub mod tier;

pub use billing::{BillingCycle, UsageAccumulator};
pub use errors::SaasError;
pub use limits::{Limits, TierLimits};
pub use quotas::{QuotaManager, QuotaState};
pub use tier::{Tier, TierHierarchy};

/// Result type for SaaS operations
pub type SaasResult<T> = Result<T, SaasError>;
