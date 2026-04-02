use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
    Json, Router,
};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use uuid::Uuid;

mod error;
mod store;
mod mcp_tools;

use error::ApiError;
use mcp_tools::MCPTool;
use store::{InMemoryUserStore, User};

#[derive(Clone)]
pub struct AppState {
    pub store: Arc<InMemoryUserStore>,
    pub start_time: Arc<Mutex<std::time::Instant>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthStatus {
    pub status: String,
    pub timestamp: String,
    pub uptime_seconds: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemMetrics {
    pub total_users: usize,
    pub api_version: String,
    pub mcp_tools_registered: usize,
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();

    let state = AppState {
        store: Arc::new(InMemoryUserStore::new()),
        start_time: Arc::new(Mutex::new(std::time::Instant::now())),
    };

    let app = Router::new()
        // User CRUD endpoints
        .route("/users", get(list_users).post(create_user))
        .route("/users/:id", get(get_user).delete(delete_user))
        // Agent control endpoints
        .route("/health", get(health_check))
        .route("/status", get(agent_status))
        .route("/tools", get(discover_tools))
        .route("/tools/register", post(register_tool))
        .with_state(state)
        .into_make_service();

    let listener = tokio::net::TcpListener::bind("127.0.0.1:3000")
        .await
        .expect("Failed to bind to 127.0.0.1:3000");

    println!("Server listening on http://127.0.0.1:3000");
    println!("MCP Tools available at GET /tools");
    println!("Health check at GET /health");

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

async fn list_users(State(state): State<AppState>) -> Result<Json<Vec<UserResponse>>, ApiError> {
    let users = state.store.list().await?;
    Ok(Json(
        users.into_iter().map(UserResponse::from).collect(),
    ))
}

async fn create_user(
    State(state): State<AppState>,
    Json(payload): Json<CreateUserRequest>,
) -> Result<(StatusCode, Json<UserResponse>), ApiError> {
    state.store.validate_name(&payload.name)?;
    state.store.validate_email(&payload.email)?;

    let user = User {
        id: Uuid::new_v4(),
        name: payload.name,
        email: payload.email,
        active: true,
    };

    let created = state.store.create(user).await?;
    Ok((StatusCode::CREATED, Json(UserResponse::from(created))))
}

async fn get_user(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<Json<UserResponse>, ApiError> {
    let user_id = Uuid::parse_str(&id).map_err(|_| ApiError::InvalidId)?;
    let user = state.store.get(user_id).await?;
    Ok(Json(UserResponse::from(user)))
}

async fn delete_user(
    State(state): State<AppState>,
    Path(id): Path<String>,
) -> Result<StatusCode, ApiError> {
    let user_id = Uuid::parse_str(&id).map_err(|_| ApiError::InvalidId)?;
    state.store.delete(user_id).await?;
    Ok(StatusCode::NO_CONTENT)
}

// ==============================================================================
// AGENT CONTROL HANDLERS
// ==============================================================================

async fn health_check(
    State(state): State<AppState>,
) -> Result<Json<HealthStatus>, ApiError> {
    let start = state.start_time.lock()
        .map_err(|_| ApiError::Internal("Failed to acquire lock".to_string()))?;
    let uptime = start.elapsed().as_secs();

    Ok(Json(HealthStatus {
        status: "healthy".to_string(),
        timestamp: chrono::Utc::now().to_rfc3339(),
        uptime_seconds: uptime,
    }))
}

async fn agent_status(
    State(state): State<AppState>,
) -> Result<Json<SystemMetrics>, ApiError> {
    let users = state.store.list().await?;

    Ok(Json(SystemMetrics {
        total_users: users.len(),
        api_version: "2.0".to_string(),
        mcp_tools_registered: 4,
    }))
}

#[derive(Debug, Serialize)]
struct MCPToolInfo {
    name: String,
    description: String,
    input_schema: serde_json::Value,
    endpoint: String,
    method: String,
}

async fn discover_tools() -> Result<Json<Vec<MCPToolInfo>>, ApiError> {
    let tools = vec![
        MCPToolInfo {
            name: "list_users".to_string(),
            description: "Retrieve all users from the system".to_string(),
            input_schema: serde_json::json!({}),
            endpoint: "/users".to_string(),
            method: "GET".to_string(),
        },
        MCPToolInfo {
            name: "create_user".to_string(),
            description: "Create a new user in the system".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "name": { "type": "string" },
                    "email": { "type": "string" }
                },
                "required": ["name", "email"]
            }),
            endpoint: "/users".to_string(),
            method: "POST".to_string(),
        },
        MCPToolInfo {
            name: "get_user".to_string(),
            description: "Retrieve a specific user by ID".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "id": { "type": "string" }
                },
                "required": ["id"]
            }),
            endpoint: "/users/{id}".to_string(),
            method: "GET".to_string(),
        },
        MCPToolInfo {
            name: "delete_user".to_string(),
            description: "Delete a user from the system".to_string(),
            input_schema: serde_json::json!({
                "type": "object",
                "properties": {
                    "id": { "type": "string" }
                },
                "required": ["id"]
            }),
            endpoint: "/users/{id}".to_string(),
            method: "DELETE".to_string(),
        },
    ];

    Ok(Json(tools))
}

#[derive(Debug, Deserialize)]
pub struct RegisterToolRequest {
    pub name: String,
    pub description: String,
}

#[derive(Debug, Serialize)]
pub struct RegisterToolResponse {
    pub registered: bool,
    pub tool_name: String,
    pub message: String,
}

async fn register_tool(
    Json(req): Json<RegisterToolRequest>,
) -> Result<(StatusCode, Json<RegisterToolResponse>), ApiError> {
    // In a real system, this would validate and store the tool definition
    Ok((
        StatusCode::CREATED,
        Json(RegisterToolResponse {
            registered: true,
            tool_name: req.name,
            message: "Tool registered successfully".to_string(),
        }),
    ))
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
