use axum::{http::StatusCode, response::IntoResponse, Json};
use serde_json::json;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ApiError {
    #[error("User not found")]
    NotFound,

    #[error("Invalid user ID")]
    InvalidId,

    #[error("Invalid name: {0}")]
    InvalidName(String),

    #[error("Invalid email: {0}")]
    InvalidEmail(String),

    #[error("Email already exists")]
    DuplicateEmail,
}

impl IntoResponse for ApiError {
    fn into_response(self) -> axum::response::Response {
        let (status, error_message) = match self {
            ApiError::NotFound => (
                StatusCode::NOT_FOUND,
                "User not found".to_string(),
            ),
            ApiError::InvalidId => (
                StatusCode::BAD_REQUEST,
                "Invalid user ID format".to_string(),
            ),
            ApiError::InvalidName(msg) => (StatusCode::BAD_REQUEST, msg),
            ApiError::InvalidEmail(msg) => (StatusCode::BAD_REQUEST, msg),
            ApiError::DuplicateEmail => (
                StatusCode::CONFLICT,
                "Email already exists".to_string(),
            ),
        };

        let body = Json(json!({
            "error": error_message,
        }));

        (status, body).into_response()
    }
}
