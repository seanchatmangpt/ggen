//! Payment error types

use thiserror::Error;

#[derive(Error, Debug)]
pub enum PaymentError {
    #[error("Stripe API error: {0}")]
    StripeError(String),

    #[error("Customer not found")]
    CustomerNotFound,

    #[error("Payment method not found")]
    PaymentMethodNotFound,

    #[error("Subscription not found")]
    SubscriptionNotFound,

    #[error("Invoice not found")]
    InvoiceNotFound,

    #[error("Payment failed: {0}")]
    PaymentFailed(String),

    #[error("Insufficient funds")]
    InsufficientFunds,

    #[error("Card declined")]
    CardDeclined,

    #[error("Invalid amount")]
    InvalidAmount,

    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("Webhook error: {0}")]
    WebhookError(String),

    #[error("Serialization error: {0}")]
    SerializationError(String),

    #[error("Internal server error")]
    InternalError,
}
