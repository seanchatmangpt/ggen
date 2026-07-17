//! Integration Testing
//!
//! External system integration for integration testing with external
//! dependencies, such as Testcontainers for Docker support.
//!
//! **Required Features**:
//! - `testcontainers`: Enable Docker container support (`chicago-tdd-tools = { features = ["testcontainers"] }`)
//!
//! **Usage**:
//! ```rust,ignore
//! // Enable feature in Cargo.toml:
//! // chicago-tdd-tools = { features = ["testcontainers"] }
//!
//! use chicago_tdd_tools::integration::testcontainers::*;
//! ```

#[cfg(feature = "testcontainers")]
pub mod testcontainers;

// When the `testcontainers` feature is disabled, the `testcontainers` module is absent.
// Users who try to import it will receive a compile error. Enable the feature:
//   chicago-tdd-tools = { features = ["testcontainers"] }
#[cfg(not(feature = "testcontainers"))]
mod testcontainers_placeholder {
    /// Stub module present when the `testcontainers` feature is **disabled**.
    ///
    /// The real `testcontainers` module is absent in this build. Any attempt to
    /// import `integration::testcontainers::*` will produce a compiler error such as:
    ///
    /// ```text
    /// error[E0603]: module `testcontainers` is private
    /// ```
    ///
    /// To use Docker container support, enable the feature in `Cargo.toml`:
    ///
    /// ```toml
    /// [dev-dependencies]
    /// chicago-tdd-tools = { version = "*", features = ["testcontainers"] }
    /// ```
    ///
    /// This placeholder exists solely to surface the above guidance in `rustdoc`
    /// for users who browse the API without the feature enabled.
    #[allow(dead_code)]
    pub struct FeatureGate;
}

// Re-export commonly used items
#[cfg(feature = "testcontainers")]
pub use testcontainers::*;
