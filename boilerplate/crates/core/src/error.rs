use thiserror::Error;

#[derive(Debug, Error)]
pub enum CoreError {
    #[error("entity not found: {entity} id={id}")]
    NotFound { entity: &'static str, id: String },

    #[error("validation error: {0}")]
    Validation(String),

    #[error("conflict: {0}")]
    Conflict(String),

    #[error("internal error: {0}")]
    Internal(#[from] anyhow::Error),
}

impl CoreError {
    pub fn not_found(entity: &'static str, id: impl std::fmt::Display) -> Self {
        Self::NotFound {
            entity,
            id: id.to_string(),
        }
    }

    pub fn validation(msg: impl Into<String>) -> Self {
        Self::Validation(msg.into())
    }
}

impl From<String> for CoreError {
    fn from(s: String) -> Self {
        Self::Validation(s)
    }
}
