use axum::{
    extract::{Path, State},
    http::StatusCode,
    routing::get,
    Json, Router,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

mod error;
mod store;

use error::ApiError;
use store::{InMemoryUserStore, User};

type AppState = Arc<InMemoryUserStore>;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let store = Arc::new(InMemoryUserStore::new());

    let app = Router::new()
        .route("/users", get(list_users).post(create_user))
        .route("/users/:id", get(get_user).delete(delete_user))
        .with_state(store)
        .into_make_service();

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .expect("Failed to bind to 127.0.0.1:3000");

    println!("Server listening on http://127.0.0.1:3000");

    axum::serve(listener, app)
        .await
        .expect("Server error");
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct UserResponse {
    id: String,
    name: String,
    email: String,
    active: bool,
}

impl From<User> for UserResponse {
    fn from(user: User) -> Self {
        Self {
            id: user.id.to_string(),
            name: user.name,
            email: user.email,
            active: user.active,
        }
    }
}

#[derive(Debug, Deserialize)]
struct CreateUserRequest {
    name: String,
    email: String,
}

async fn list_users(State(store): State<AppState>) -> Result<Json<Vec<UserResponse>>, ApiError> {
    let users = store.list().await?;
    Ok(Json(
        users.into_iter().map(UserResponse::from).collect(),
    ))
}

async fn create_user(
    State(store): State<AppState>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<(StatusCode, Json<UserResponse>), ApiError> {
    store.validate_name(&payload.name)?;
    store.validate_email(&payload.email)?;

    let user = User {
        id: Uuid::new_v4(),
        name: payload.name,
        email: payload.email,
        active: true,
    };

    let created = store.create(user).await?;
    Ok((StatusCode::CREATED, Json(UserResponse::from(created))))
}

async fn get_user(
    State(store): State<AppState>,
    Path(id): Path<String>,
) -> Result<Json<UserResponse>, ApiError> {
    let user_id = Uuid::parse_str(&id).map_err(|_| ApiError::InvalidId)?;
    let user = store.get(user_id).await?;
    Ok(Json(UserResponse::from(user)))
}

async fn delete_user(
    State(store): State<AppState>,
    Path(id): Path<String>,
) -> Result<StatusCode, ApiError> {
    let user_id = Uuid::parse_str(&id).map_err(|_| ApiError::InvalidId)?;
    store.delete(user_id).await?;
    Ok(StatusCode::NO_CONTENT)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_user_response_from_user() {
        let user = User {
            id: Uuid::new_v4(),
            name: "John Doe".to_string(),
            email: "john@example.com".to_string(),
            active: true,
        };
        let response: UserResponse = user.into();
        assert_eq!(response.name, "John Doe");
        assert_eq!(response.email, "john@example.com");
        assert!(response.active);
    }

    #[test]
    fn test_create_user_request() {
        let req = CreateUserRequest {
            name: "Jane Doe".to_string(),
            email: "jane@example.com".to_string(),
        };
        assert_eq!(req.name, "Jane Doe");
        assert_eq!(req.email, "jane@example.com");
    }
}
