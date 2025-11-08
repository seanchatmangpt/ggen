// Generated REST API using Axum - Async/Await Pattern
// This demonstrates RDF-driven code generation for Rust

use axum::{
    extract::{Path, Query, State},
    http::{HeaderMap, StatusCode},
    response::Json,
    routing::{delete, get, patch, post, put},
    Router,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::net::TcpListener;
use tower::ServiceBuilder;
use tower_http::{
    cors::CorsLayer,
    trace::TraceLayer,
    validate_request::ValidateRequestHeaderLayer,
};

// ============================================================================
// Domain Models (Generated from RDF RequestSchema/ResponseSchema)
// ============================================================================

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct User {
    pub id: Option<i64>,
    pub username: String,
    pub email: String,
    pub created_at: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct CreateUserRequest {
    pub username: String,
    pub email: String,
}

#[derive(Debug, Deserialize)]
pub struct UpdateUserRequest {
    pub username: Option<String>,
    pub email: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct QueryParams {
    pub page: Option<u32>,
    pub limit: Option<u32>,
}

#[derive(Debug, Serialize)]
pub struct ApiResponse<T> {
    pub success: bool,
    pub data: Option<T>,
    pub error: Option<String>,
}

// ============================================================================
// Application State (Database Connection Pool)
// ============================================================================

#[derive(Clone)]
pub struct AppState {
    pub db: Arc<DatabasePool>,
}

pub struct DatabasePool {
    // In production: sqlx::PgPool or diesel::PgConnection
    // For template: simple in-memory store
    users: Arc<tokio::sync::RwLock<Vec<User>>>,
}

impl DatabasePool {
    pub fn new() -> Self {
        Self {
            users: Arc::new(tokio::sync::RwLock::new(Vec::new())),
        }
    }

    pub async fn get_users(&self) -> Vec<User> {
        self.users.read().await.clone()
    }

    pub async fn get_user_by_id(&self, id: i64) -> Option<User> {
        self.users.read().await.iter().find(|u| u.id == Some(id)).cloned()
    }

    pub async fn create_user(&self, username: String, email: String) -> User {
        let mut users = self.users.write().await;
        let id = users.len() as i64 + 1;
        let user = User {
            id: Some(id),
            username,
            email,
            created_at: Some(chrono::Utc::now().to_rfc3339()),
        };
        users.push(user.clone());
        user
    }

    pub async fn update_user(&self, id: i64, req: UpdateUserRequest) -> Option<User> {
        let mut users = self.users.write().await;
        if let Some(user) = users.iter_mut().find(|u| u.id == Some(id)) {
            if let Some(username) = req.username {
                user.username = username;
            }
            if let Some(email) = req.email {
                user.email = email;
            }
            Some(user.clone())
        } else {
            None
        }
    }

    pub async fn delete_user(&self, id: i64) -> bool {
        let mut users = self.users.write().await;
        if let Some(pos) = users.iter().position(|u| u.id == Some(id)) {
            users.remove(pos);
            true
        } else {
            false
        }
    }
}

// ============================================================================
// Middleware (Generated from rest:Middleware)
// ============================================================================

async fn auth_middleware(
    headers: HeaderMap,
    mut req: axum::extract::Request,
    next: axum::middleware::Next,
) -> Result<axum::response::Response, StatusCode> {
    // JWT validation would go here
    if let Some(auth) = headers.get("Authorization") {
        if auth.to_str().unwrap_or("").starts_with("Bearer ") {
            return Ok(next.run(req).await);
        }
    }
    Err(StatusCode::UNAUTHORIZED)
}

// ============================================================================
// Route Handlers (Generated from SPARQL Query 2)
// ============================================================================

// GET /api/users - List all users
pub async fn list_users(
    State(state): State<AppState>,
    Query(params): Query<QueryParams>,
) -> Result<Json<ApiResponse<Vec<User>>>, StatusCode> {
    let users = state.db.get_users().await;

    // Apply pagination
    let page = params.page.unwrap_or(1);
    let limit = params.limit.unwrap_or(10).min(100);
    let start = ((page - 1) * limit) as usize;
    let end = (start + limit as usize).min(users.len());

    let paginated = users[start..end].to_vec();

    Ok(Json(ApiResponse {
        success: true,
        data: Some(paginated),
        error: None,
    }))
}

// GET /api/users/:id - Get single user
pub async fn get_user(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> Result<Json<ApiResponse<User>>, StatusCode> {
    match state.db.get_user_by_id(id).await {
        Some(user) => Ok(Json(ApiResponse {
            success: true,
            data: Some(user),
            error: None,
        })),
        None => Err(StatusCode::NOT_FOUND),
    }
}

// POST /api/users - Create new user
pub async fn create_user(
    State(state): State<AppState>,
    Json(req): Json<CreateUserRequest>,
) -> Result<(StatusCode, Json<ApiResponse<User>>), StatusCode> {
    // Validation (from rest:ValidationRule)
    if req.username.is_empty() || req.email.is_empty() {
        return Err(StatusCode::BAD_REQUEST);
    }

    if !req.email.contains('@') {
        return Err(StatusCode::UNPROCESSABLE_ENTITY);
    }

    let user = state.db.create_user(req.username, req.email).await;

    Ok((
        StatusCode::CREATED,
        Json(ApiResponse {
            success: true,
            data: Some(user),
            error: None,
        }),
    ))
}

// PUT /api/users/:id - Update user
pub async fn update_user(
    State(state): State<AppState>,
    Path(id): Path<i64>,
    Json(req): Json<UpdateUserRequest>,
) -> Result<Json<ApiResponse<User>>, StatusCode> {
    match state.db.update_user(id, req).await {
        Some(user) => Ok(Json(ApiResponse {
            success: true,
            data: Some(user),
            error: None,
        })),
        None => Err(StatusCode::NOT_FOUND),
    }
}

// DELETE /api/users/:id - Delete user
pub async fn delete_user(
    State(state): State<AppState>,
    Path(id): Path<i64>,
) -> Result<StatusCode, StatusCode> {
    if state.db.delete_user(id).await {
        Ok(StatusCode::NO_CONTENT)
    } else {
        Err(StatusCode::NOT_FOUND)
    }
}

// Health check endpoint
pub async fn health_check() -> Json<serde_json::Value> {
    Json(serde_json::json!({
        "status": "healthy",
        "timestamp": chrono::Utc::now().to_rfc3339(),
    }))
}

// ============================================================================
// Router Configuration (Generated from SPARQL Query 13)
// ============================================================================

pub fn create_router(state: AppState) -> Router {
    let api_routes = Router::new()
        .route("/users", get(list_users).post(create_user))
        .route("/users/:id", get(get_user).put(update_user).delete(delete_user))
        .with_state(state);

    Router::new()
        .route("/health", get(health_check))
        .nest("/api", api_routes)
        .layer(
            ServiceBuilder::new()
                .layer(TraceLayer::new_for_http())
                .layer(CorsLayer::permissive()),
        )
}

// ============================================================================
// Main Application Entry Point
// ============================================================================

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    // Create application state
    let state = AppState {
        db: Arc::new(DatabasePool::new()),
    };

    // Build router
    let app = create_router(state);

    // Start server
    let listener = TcpListener::bind("127.0.0.1:3000").await?;
    println!("🚀 Server running on http://127.0.0.1:3000");

    axum::serve(listener, app).await?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::http::Request;
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_health_check() {
        let state = AppState {
            db: Arc::new(DatabasePool::new()),
        };
        let app = create_router(state);

        let response = app
            .oneshot(Request::builder().uri("/health").body(axum::body::Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_create_and_get_user() {
        let state = AppState {
            db: Arc::new(DatabasePool::new()),
        };
        let app = create_router(state);

        // Create user
        let create_req = serde_json::json!({
            "username": "testuser",
            "email": "test@example.com"
        });

        let response = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("POST")
                    .uri("/api/users")
                    .header("content-type", "application/json")
                    .body(axum::body::Body::from(create_req.to_string()))
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::CREATED);
    }
}
