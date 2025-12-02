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
pub mod dry_run;
pub mod lockfile_guard;
pub mod network_retry;
pub mod sanitized_input;
pub mod timeout_io;
pub mod validated_path;

// Re-export core types
pub use atomic_writer::{AtomicFileWriter, Committed, Uncommitted};
pub use dry_run::{DryRunMode, Operation};
pub use lockfile_guard::LockfileGuard;
pub use network_retry::NetworkRetry;
pub use sanitized_input::{InputType, SanitizedInput};
pub use timeout_io::TimeoutIO;
pub use validated_path::ValidatedPath;

#[cfg(test)]
mod tests;
