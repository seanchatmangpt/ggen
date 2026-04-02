#![warn(missing_docs)]
#![forbid(unsafe_code)]

//! # TAI GCP Service Integration
//!
//! Production-grade GCP service client wrapper layer using official Google Cloud Client Libraries.
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────┐
//! │                    TAI GCP Layer                         │
//! ├─────────────────────────────────────────────────────────┤
//! │                                                           │
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
//! │  │ Cloud Run    │  │ Pub/Sub      │  │ Firestore    │  │
//! │  │ Client       │  │ Client       │  │ Client       │  │
//! │  └──────────────┘  └──────────────┘  └──────────────┘  │
//! │          │                  │                  │        │
//! │          └──────────────────┼──────────────────┘        │
//! │                             │                           │
//! │                  ┌──────────▼──────────┐               │
//! │                  │  GCP Clients        │               │
//! │                  │  (unified wrapper)  │               │
//! │                  └──────────┬──────────┘               │
//! │                             │                           │
//! │         ┌───────────────────┼───────────────────┐      │
//! │         │                   │                   │      │
//! │    ┌────▼─────┐    ┌────────▼────────┐  ┌──────▼───┐ │
//! │    │ GCP Auth │    │ GCP Config      │  │ Retry &  │ │
//! │    │ (WI)     │    │ (SLOs, regions) │  │ Circuit  │ │
//! │    └──────────┘    └─────────────────┘  │ Breaker  │ │
//! │                                         └──────────┘ │
//! │                                                       │
//! │  ┌─────────────────────────────────────────────────┐ │
//! │  │         Error Handling & Mapping               │ │
//! │  │  (GCP codes → TAI errors, retry classification)  │ │
//! │  └─────────────────────────────────────────────────┘ │
//! │                                                       │
//! └─────────────────────────────────────────────────────────┘
//!                             │
//!                             ▼
//!        ┌──────────────────────────────────────┐
//!        │  Official Google Cloud Clients       │
//!        │  (REST API via generated stubs)      │
//!        └──────────────────────────────────────┘
//! ```
//!
//! ## Core Modules
//!
//! - [`gcp_clients`]: Unified GCP service clients (Cloud Run, Pub/Sub, Firestore, etc.)
//! - [`gcp_auth`]: Authentication (Workload Identity, ADC, token refresh)
//! - [`gcp_errors`]: GCP-specific error handling and mapping
//! - [`gcp_config`]: Configuration management (environment, metadata server, Secret Manager)
//!
//! ## Usage Example
//!
//! ```no_run
//! use tai_gcp::gcp_clients::GcpClients;
//! use tai_gcp::gcp_config::GcpConfig;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // Load configuration from environment + metadata server
//!     let config = GcpConfig::from_environment().await?;
//!
//!     // Initialize unified clients
//!     let clients = GcpClients::new(config).await?;
//!
//!     // Use clients
//!     // clients.run.invoke_service(...).await?;
//!     // clients.pubsub.publish(...).await?;
//!     // clients.firestore.write_document(...).await?;
//!
//!     Ok(())
//! }
//! ```
//!
//! ## Authentication Strategy
//!
//! The authentication layer uses a **fallback chain**:
//!
//! 1. **Workload Identity (preferred)**: GCP-native service account federation
//!    - Zero key rotation overhead
//!    - Automatic token refresh
//!    - Least privilege principle
//!
//! 2. **ADC (Application Default Credentials)**: Google's standard flow
//!    - Environment variables (`GOOGLE_APPLICATION_CREDENTIALS`)
//!    - GCP metadata server
//!    - Local development (`gcloud auth application-default login`)
//!
//! ## Error Handling
//!
//! All GCP errors are mapped to TAI error types with:
//! - **Transient vs Permanent classification**: Determines retry eligibility
//! - **Error context enrichment**: Project ID, operation, resource information
//! - **Observable logging**: All errors logged with full context
//!
//! Example:
//! ```text
//! GCP Error: Service Unavailable (HTTP 503)
//! ↓
//! Transient Classification → Eligible for retry
//! ↓
//! Context: project_id=my-project, operation=firestore.write, resource=/documents/user-123
//! ↓
//! Mapped to: TaiError::Transient(ServiceUnavailable)
//! ↓
//! Retry Backoff: exponential, max 3 attempts, jitter-enabled
//! ```
//!
//! ## Performance SLOs
//!
//! Target response times per service:
//! - **Cloud Run**: ≤ 30s (including user service execution)
//! - **Firestore**: ≤ 5s (document operations)
//! - **Pub/Sub**: ≤ 1s (publish operations)
//! - **Cloud Monitoring**: ≤ 2s (metrics write)
//! - **Cloud Scheduler**: ≤ 10s (job creation/update)
//! - **Cloud KMS**: ≤ 3s (encryption/decryption)
//! - **Cloud Logging**: ≤ 2s (log write)
//!
//! Timeouts are enforced at the client configuration level.

pub mod gcp_auth;
pub mod gcp_clients;
pub mod gcp_config;
pub mod gcp_errors;

pub use gcp_clients::GcpClients;
pub use gcp_config::GcpConfig;
pub use gcp_errors::{GcpError, GcpErrorKind};
