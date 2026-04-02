//! Craftplan-A2A Bridge Service
//!
//! This service bridges Craftplan (Elixir ERP) with the A2A agent protocol,
//! exposing manufacturing operations as agent skills.

use axum::{
    extract::State,
    http::StatusCode,
    response::{IntoResponse, Json},
    routing::{get, post},
    Router,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_http::cors::CorsLayer;
use uuid::Uuid;

// ============================================================================
// Domain Types
// ============================================================================

/// Craftplan resource types that can be accessed via A2A
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CraftplanResource {
    Products,
    Materials,
    Orders,
    Inventory,
    ProductionBatches,
    Customers,
    Suppliers,
    Boms,
}

/// A2A task representing a Craftplan operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CraftplanTask {
    pub id: String,
    pub resource: CraftplanResource,
    pub operation: String,
    pub params: serde_json::Value,
    pub status: TaskStatus,
    pub result: Option<serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskStatus {
    pub state: String,
    pub message: Option<String>,
    pub timestamp: i64,
}

// ============================================================================
// Bridge State
// ============================================================================

#[derive(Clone)]
struct BridgeState {
    craftplan_url: String,
    tasks: Arc<RwLock<Vec<CraftplanTask>>>,
}

impl BridgeState {
    fn new(craftplan_url: String) -> Self {
        Self {
            craftplan_url,
            tasks: Arc::new(RwLock::new(Vec::new())),
        }
    }

    async fn create_task(
        &self,
        resource: CraftplanResource,
        operation: String,
        params: serde_json::Value,
    ) -> CraftplanTask {
        let task = CraftplanTask {
            id: Uuid::new_v4().to_string(),
            resource,
            operation,
            params,
            status: TaskStatus {
                state: "submitted".to_string(),
                message: Some("Task submitted to Craftplan".to_string()),
                timestamp: chrono::Utc::now().timestamp(),
            },
            result: None,
        };

        self.tasks.write().await.push(task.clone());
        task
    }

    async fn get_task(&self, id: &str) -> Option<CraftplanTask> {
        self.tasks
            .read()
            .await
            .iter()
            .find(|t| t.id == id)
            .cloned()
    }

    async fn list_tasks(&self) -> Vec<CraftplanTask> {
        self.tasks.read().await.clone()
    }
}

// ============================================================================
// HTTP Handlers
// ============================================================================

#[derive(Deserialize)]
struct CreateTaskRequest {
    resource: CraftplanResource,
    operation: String,
    params: serde_json::Value,
}

async fn create_task(
    State(state): State<BridgeState>,
    Json(req): Json<CreateTaskRequest>,
) -> impl IntoResponse {
    let task = state.create_task(req.resource, req.operation, req.params).await;

    // Spawn background task to execute the operation
    let state_clone = state.clone();
    let task_id = task.id.clone();
    tokio::spawn(async move {
        execute_craftplan_operation(state_clone, task_id).await;
    });

    Json(task)
}

async fn get_task(
    State(state): State<BridgeState>,
    axum::extract::Path(id): axum::extract::Path<String>,
) -> impl IntoResponse {
    match state.get_task(&id).await {
        Some(task) => (StatusCode::OK, Json(task)).into_response(),
        None => (StatusCode::NOT_FOUND, "Task not found").into_response(),
    }
}

async fn list_tasks(State(state): State<BridgeState>) -> impl IntoResponse {
    Json(state.list_tasks().await)
}

async fn health() -> impl IntoResponse {
    Json(serde_json::json!({
        "status": "healthy",
        "service": "craftplan-a2a-bridge",
        "version": "0.1.0"
    }))
}

async fn agent_card() -> impl IntoResponse {
    Json(serde_json::json!({
        "name": "craftplan-bridge",
        "description": "A2A agent for Craftplan manufacturing ERP",
        "version": "0.1.0",
        "url": "http://localhost:5000",
        "capabilities": {
            "streaming": false,
            "push_notifications": false,
            "state_transition_history": true
        },
        "skills": [
            {
                "name": "list_products",
                "description": "List all products in the catalog"
            },
            {
                "name": "create_order",
                "description": "Create a new customer order"
            },
            {
                "name": "check_inventory",
                "description": "Check inventory levels for materials"
            },
            {
                "name": "create_production_batch",
                "description": "Create a production batch"
            }
        ]
    }))
}

// ============================================================================
// Craftplan Integration
// ============================================================================

async fn execute_craftplan_operation(state: BridgeState, task_id: String) {
    // Get the task
    let mut tasks = state.tasks.write().await;
    let task = tasks.iter_mut().find(|t| t.id == task_id);

    if let Some(task) = task {
        // Update status to running
        task.status.state = "running".to_string();
        task.status.message = Some("Executing Craftplan operation".to_string());
        drop(tasks);

        // Simulate operation execution
        // In production, this would call Craftplan's API
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        // Update status to completed
        let mut tasks = state.tasks.write().await;
        if let Some(task) = tasks.iter_mut().find(|t| t.id == task_id) {
            task.status.state = "completed".to_string();
            task.status.message = Some("Operation completed successfully".to_string());
            task.result = Some(serde_json::json!({
                "success": true,
                "data": format!("Executed {} on {:?}", task.operation, task.resource)
            }));
        }
    }
}

// ============================================================================
// Main
// ============================================================================

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    let craftplan_url = std::env::var("CRAFTPLAN_URL")
        .unwrap_or_else(|_| "http://localhost:4000".to_string());

    let state = BridgeState::new(craftplan_url.clone());

    let app = Router::new()
        .route("/health", get(health))
        .route("/agent-card", get(agent_card))
        .route("/tasks", post(create_task).get(list_tasks))
        .route("/tasks/:id", get(get_task))
        .layer(CorsLayer::permissive())
        .with_state(state);

    let addr = std::env::var("BIND_ADDR")
        .unwrap_or_else(|_| "0.0.0.0:5000".to_string());

    tracing::info!("Craftplan-A2A Bridge listening on {}", addr);
    tracing::info!("Craftplan URL: {}", craftplan_url);

    let listener = tokio::net::TcpListener::bind(&addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
