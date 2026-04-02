//! SaaS error types

use thiserror::Error;

#[derive(Error, Debug)]
pub enum SaasError {
    #[error("Quota exceeded for {resource}: used {used}/{limit}")]
    QuotaExceeded {
        resource: String,
        used: u64,
        limit: u64,
    },

    #[error("Rate limit exceeded: {0} requests per minute")]
    RateLimitExceeded(u32),

    #[error("Tier not found: {0}")]
    TierNotFound(String),

    #[error("Invalid tier transition: {0}")]
    InvalidTierTransition(String),

    #[error("Billing cycle not found")]
    BillingCycleNotFound,

    #[error("Usage accumulation error: {0}")]
    UsageError(String),

    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("Database error: {0}")]
    DatabaseError(String),

    #[error("Cache error: {0}")]
    CacheError(String),

    #[error("Internal server error")]
    InternalError,
}
