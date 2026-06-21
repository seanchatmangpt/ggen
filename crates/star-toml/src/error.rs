//! Error types for `star_toml`.

use std::path::PathBuf;

/// Alias so callers can write `star_toml::Result<T>`.
pub type Result<T> = std::result::Result<T, Error>;

/// Everything that can go wrong while loading, merging, or validating a TOML config.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// No file matching the requested name was found by walking parent directories.
    #[error("config file not found: {0}")]
    FileNotFound(PathBuf),

    /// OS-level I/O error (permissions, missing dir, etc.).
    #[error("I/O error reading {path}: {source}")]
    Io {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    /// The TOML text could not be parsed.
    #[error("TOML parse error in {path}: {source}")]
    Parse {
        /// The file or description ("inline string") where the error occurred.
        path: String,
        #[source]
        source: toml::de::Error,
    },

    /// A config value could not be serialized back to TOML.
    #[error("TOML serialize error: {0}")]
    Serialize(#[from] toml::ser::Error),

    /// A loaded config failed its own invariant checks (see [`crate::Validate`]).
    #[error("validation failed for {context}: {reason}")]
    Validation {
        /// Which file or config type was being validated.
        context: String,
        /// Human-readable description of the violation.
        reason: String,
    },
}

impl Error {
    /// Construct a validation error.
    pub fn validation(context: impl Into<String>, reason: impl Into<String>) -> Self {
        Self::Validation {
            context: context.into(),
            reason: reason.into(),
        }
    }

    /// Construct an I/O error with source path context.
    pub fn io(path: impl Into<PathBuf>, source: std::io::Error) -> Self {
        Self::Io {
            path: path.into(),
            source,
        }
    }

    /// Construct a parse error with file/location context.
    pub fn parse(path: impl Into<String>, source: toml::de::Error) -> Self {
        Self::Parse {
            path: path.into(),
            source,
        }
    }
}
