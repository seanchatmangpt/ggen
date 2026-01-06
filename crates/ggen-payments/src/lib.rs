//! Payment processing system for ggen: Stripe integration and billing operations

pub mod stripe_client;
pub mod invoice;
pub mod subscription;
pub mod payment;
pub mod errors;
pub mod webhook;

pub use stripe_client::StripeClient;
pub use invoice::Invoice;
pub use subscription::Subscription;
pub use payment::Payment;
pub use errors::PaymentError;
pub use webhook::WebhookEvent;

/// Result type for payment operations
pub type PaymentResult<T> = Result<T, PaymentError>;
