//! The [`Validate`] trait for config types that can check their own invariants.

use crate::error::Error;

/// A config struct implements this to expose a self-check after loading.
///
/// Implement this on any type `T` you load with [`crate::Loader`]. The loader
/// calls `validate()` automatically when you use [`crate::Loader::load_validated`].
///
/// # Example
///
/// ```
/// use star_toml::{Validate, Error};
///
/// #[derive(serde::Deserialize)]
/// struct ServerConfig {
///     port: u16,
/// }
///
/// impl Validate for ServerConfig {
///     fn validate(&self) -> Result<(), Error> {
///         if self.port == 0 {
///             return Err(Error::validation("ServerConfig", "port must be > 0"));
///         }
///         Ok(())
///     }
/// }
/// ```
pub trait Validate {
    /// Return `Ok(())` if the config is internally consistent, or an [`Error::Validation`]
    /// describing the first violation found.
    fn validate(&self) -> Result<(), Error>;
}
