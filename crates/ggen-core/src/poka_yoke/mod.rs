//! Poka-Yoke (Error-Proofing) mechanisms for ggen CLI.
//!
//! This module provides 7 error-proofing mechanisms following DfLSS principles:
//! - Type-level safety (compile-time guarantees)
//! - Zero-cost abstractions (no runtime overhead)
//! - RAII pattern (automatic cleanup)
//! - Fail-fast design (early detection)
//!
//! # Mechanisms
//!
//! 1. [`AtomicFileWriter`] - Atomic file writes with rollback
//! 2. [`ValidatedPath`] - Path traversal prevention
//! 3. [`TimeoutIO`] - SLO-based timeout enforcement
//! 4. [`SanitizedInput`] - Injection prevention
//! 5. [`LockfileGuard`] - Race condition prevention
//! 6. [`NetworkRetry`] - Transient failure handling
//! 7. [`DryRunMode`] - Preview before execution

pub mod atomic_writer;
pub mod validated_path;
pub mod timeout_io;
pub mod sanitized_input;
pub mod lockfile_guard;
pub mod network_retry;
pub mod dry_run;

// Re-export core types
pub use atomic_writer::{AtomicFileWriter, Uncommitted, Committed};
pub use validated_path::ValidatedPath;
pub use timeout_io::TimeoutIO;
pub use sanitized_input::{SanitizedInput, InputType};
pub use lockfile_guard::LockfileGuard;
pub use network_retry::NetworkRetry;
pub use dry_run::{DryRunMode, Operation};

#[cfg(test)]
mod tests;
