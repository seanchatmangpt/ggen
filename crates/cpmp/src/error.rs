use std::fmt;
use std::io;

pub type Result<T> = std::result::Result<T, MapError>;

#[derive(Debug)]
pub enum MapError {
    Io {
        path: std::path::PathBuf,
        source: io::Error,
    },
    #[cfg(feature = "sqlite")]
    Db(rusqlite::Error),
    Database(String),
    Serialization(String),
    SerdeJson(serde_json::error::Error),
    InvalidLanguage(String),
    ScanFailed(String),
    ReceiptFailed(String),
}

impl fmt::Display for MapError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MapError::Io { path, source } => write!(f, "IO error at {:?}: {}", path, source),
            #[cfg(feature = "sqlite")]
            MapError::Db(e) => write!(f, "Database error: {}", e),
            MapError::Database(s) => write!(f, "Database error: {}", s),
            MapError::Serialization(s) => write!(f, "Serialization error: {}", s),
            MapError::SerdeJson(e) => write!(f, "JSON error: {}", e),
            MapError::InvalidLanguage(s) => write!(f, "Invalid language: {}", s),
            MapError::ScanFailed(s) => write!(f, "Scan failed: {}", s),
            MapError::ReceiptFailed(s) => write!(f, "Receipt failed: {}", s),
        }
    }
}

#[cfg(feature = "sqlite")]
impl From<rusqlite::Error> for MapError {
    fn from(e: rusqlite::Error) -> Self {
        MapError::Db(e)
    }
}

impl From<serde_json::error::Error> for MapError {
    fn from(e: serde_json::error::Error) -> Self {
        MapError::SerdeJson(e)
    }
}

impl std::error::Error for MapError {}
