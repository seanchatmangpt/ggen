/// Error types for IDP
use actix_web::{error::ResponseError, http::StatusCode, HttpResponse};
use serde::Serialize;
use std::fmt;

#[derive(Debug, Clone, Serialize)]
pub struct ApiError {
    pub code: String,
    pub message: String,
    pub status: u16,
}

#[derive(Debug)]
pub enum IdpError {
    NotFound(String),
    Unauthorized(String),
    Forbidden(String),
    BadRequest(String),
    Conflict(String),
    InternalError(String),
    InvalidToken(String),
    SessionExpired,
    UserNotFound,
    InvalidCredentials,
    RoleNotFound,
    PermissionDenied,
    MfaRequired,
    UserAlreadyExists,
}

impl fmt::Display for IdpError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IdpError::NotFound(msg) => write!(f, "Not found: {}", msg),
            IdpError::Unauthorized(msg) => write!(f, "Unauthorized: {}", msg),
            IdpError::Forbidden(msg) => write!(f, "Forbidden: {}", msg),
            IdpError::BadRequest(msg) => write!(f, "Bad request: {}", msg),
            IdpError::Conflict(msg) => write!(f, "Conflict: {}", msg),
            IdpError::InternalError(msg) => write!(f, "Internal error: {}", msg),
            IdpError::InvalidToken(msg) => write!(f, "Invalid token: {}", msg),
            IdpError::SessionExpired => write!(f, "Session expired"),
            IdpError::UserNotFound => write!(f, "User not found"),
            IdpError::InvalidCredentials => write!(f, "Invalid credentials"),
            IdpError::RoleNotFound => write!(f, "Role not found"),
            IdpError::PermissionDenied => write!(f, "Permission denied"),
            IdpError::MfaRequired => write!(f, "MFA required"),
            IdpError::UserAlreadyExists => write!(f, "User already exists"),
        }
    }
}

impl IdpError {
    pub fn to_response(&self) -> ApiError {
        match self {
            IdpError::NotFound(msg) => ApiError {
                code: "NOT_FOUND".to_string(),
                message: msg.clone(),
                status: 404,
            },
            IdpError::Unauthorized(msg) => ApiError {
                code: "UNAUTHORIZED".to_string(),
                message: msg.clone(),
                status: 401,
            },
            IdpError::Forbidden(msg) => ApiError {
                code: "FORBIDDEN".to_string(),
                message: msg.clone(),
                status: 403,
            },
            IdpError::BadRequest(msg) => ApiError {
                code: "BAD_REQUEST".to_string(),
                message: msg.clone(),
                status: 400,
            },
            IdpError::Conflict(msg) => ApiError {
                code: "CONFLICT".to_string(),
                message: msg.clone(),
                status: 409,
            },
            IdpError::InternalError(msg) => ApiError {
                code: "INTERNAL_ERROR".to_string(),
                message: msg.clone(),
                status: 500,
            },
            IdpError::InvalidToken(_) => ApiError {
                code: "INVALID_TOKEN".to_string(),
                message: "Invalid or expired token".to_string(),
                status: 401,
            },
            IdpError::SessionExpired => ApiError {
                code: "SESSION_EXPIRED".to_string(),
                message: "Session expired".to_string(),
                status: 401,
            },
            IdpError::UserNotFound => ApiError {
                code: "USER_NOT_FOUND".to_string(),
                message: "User not found".to_string(),
                status: 404,
            },
            IdpError::InvalidCredentials => ApiError {
                code: "INVALID_CREDENTIALS".to_string(),
                message: "Invalid credentials".to_string(),
                status: 401,
            },
            IdpError::RoleNotFound => ApiError {
                code: "ROLE_NOT_FOUND".to_string(),
                message: "Role not found".to_string(),
                status: 404,
            },
            IdpError::PermissionDenied => ApiError {
                code: "PERMISSION_DENIED".to_string(),
                message: "Permission denied".to_string(),
                status: 403,
            },
            IdpError::MfaRequired => ApiError {
                code: "MFA_REQUIRED".to_string(),
                message: "MFA verification required".to_string(),
                status: 403,
            },
            IdpError::UserAlreadyExists => ApiError {
                code: "USER_EXISTS".to_string(),
                message: "User already exists".to_string(),
                status: 409,
            },
        }
    }
}

impl ResponseError for IdpError {
    fn error_response(&self) -> HttpResponse {
        let api_error = self.to_response();
        let status = StatusCode::from_u16(api_error.status).unwrap_or(StatusCode::INTERNAL_SERVER_ERROR);
        HttpResponse::build(status).json(api_error)
    }
}

impl From<String> for IdpError {
    fn from(msg: String) -> Self {
        IdpError::InternalError(msg)
    }
}

impl From<&str> for IdpError {
    fn from(msg: &str) -> Self {
        IdpError::InternalError(msg.to_string())
    }
}

pub type IdpResult<T> = Result<T, IdpError>;
