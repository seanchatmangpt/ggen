//! Payment processing system for ggen: Stripe integration and billing operations

pub mod errors;
pub mod invoice;
pub mod payment;
pub mod stripe_client;
pub mod subscription;
pub mod webhook;

pub use errors::PaymentError;
pub use invoice::Invoice;
pub use payment::Payment;
pub use stripe_client::StripeClient;
pub use subscription::Subscription;
pub use webhook::WebhookEvent;

/// Result type for payment operations
pub type PaymentResult<T> = Result<T, PaymentError>;
