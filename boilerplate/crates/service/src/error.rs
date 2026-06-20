use axum::{
    http::StatusCode,
    response::{IntoResponse, Response},
    Json,
};
use bp_core::CoreError;
use serde_json::json;

/// Converts domain errors into HTTP responses.
pub struct ApiError(CoreError);

impl From<CoreError> for ApiError {
    fn from(e: CoreError) -> Self {
        Self(e)
    }
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        let (status, code, message) = match &self.0 {
            CoreError::NotFound { entity, id } => (
                StatusCode::NOT_FOUND,
                "NOT_FOUND",
                format!("{entity} {id} not found"),
            ),
            CoreError::Validation(msg) => (
                StatusCode::UNPROCESSABLE_ENTITY,
                "VALIDATION_ERROR",
                msg.clone(),
            ),
            CoreError::Conflict(msg) => (StatusCode::CONFLICT, "CONFLICT", msg.clone()),
            CoreError::Internal(_) => (
                StatusCode::INTERNAL_SERVER_ERROR,
                "INTERNAL_ERROR",
                "internal server error".to_string(),
            ),
        };

        (
            status,
            Json(json!({ "error": { "code": code, "message": message } })),
        )
            .into_response()
    }
}
