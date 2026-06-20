pub mod error;
pub mod id;
pub mod pagination;

pub use error::CoreError;
pub type Result<T, E = CoreError> = std::result::Result<T, E>;
