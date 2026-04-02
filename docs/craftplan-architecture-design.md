# Craftplan Integration Architecture Design for A2A-RS Ecosystem

**Version**: 1.0.0
**Date**: 2026-02-04
**Author**: System Architecture Designer
**Status**: Complete Architecture Specification

---

## Executive Summary

This document provides the complete architecture design for integrating **Craftplan** (an open-source ERP for artisanal manufacturers) into the **A2A-RS ecosystem**. The integration creates a powerful AI-driven ERP system where intelligent agents can interact with business domains through a standardized protocol, enabling natural language queries, automated workflows, and real-time updates.

### Architecture Highlights

- **Hexagonal Architecture**: Clean separation between framework (a2a-rs), business logic (agents), and ERP (craftplan)
- **4-Layer Integration**: A2A-RS Core → Craftplan Adapter → Domain Agents → Client UI
- **Protocol Bridging**: A2A ↔ JSON:API (HTTP) and A2A ↔ Phoenix Channels (WebSocket)
- **Production-Ready**: JWT authentication, caching, error handling, observability
- **MCP Integration**: Agents accessible as MCP tools for LLM integration

---

## Table of Contents

1. [System Architecture Overview](#system-architecture-overview)
2. [Component Architecture](#component-architecture)
3. [Data Flow Diagrams](#data-flow-diagrams)
4. [Integration Patterns](#integration-patterns)
5. [Security Architecture](#security-architecture)
6. [API Contracts & Interfaces](#api-contracts--interfaces)
7. [Deployment Architecture](#deployment-architecture)
8. [Error Handling Strategy](#error-handling-strategy)
9. [Performance Optimization](#performance-optimization)
10. [Implementation Roadmap](#implementation-roadmap)
11. [Integration Checklist](#integration-checklist)

---

## 1. System Architecture Overview

### 1.1 High-Level Component Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                            A2A-CLIENT (Web UI)                             │
│                        Axum + Askama Templates                             │
│                          http://localhost:3000                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ Chat Interface│ │ Task Browser │ │Form Generator │ │  Dashboard   │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  └──────────────┘  │
└──────────────────────────────────────┬──────────────────────────────────────┘
                                       │ HTTP/WebSocket
                                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         A2A-AGENTS (Rust)                                 │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                     Domain Agent Layer                             │   │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐ │   │
│  │  │ Catalog  │ │  Orders  │ │Inventory │ │Production│ │   CRM    │ │   │
│  │  │  Agent   │ │  Agent   │ │  Agent   │ │  Agent   │ │  Agent   │ │   │
│  │  └────┬─────┘ └────┬─────┘ └────┬─────┘ └────┬─────┘ └────┬─────┘ │   │
│  │       │            │            │            │            │       │   │
│  │       └────────────┴────────────┴────────────┴────────────┘       │   │
│  │                              │                                     │   │
│  │           ┌──────────────────▼──────────────────┐                 │   │
│  │           │   CompositeMessageHandler           │                 │   │
│  │           │   (Intent-based Routing)            │                 │   │
│  │           └──────────────────┬──────────────────┘                 │   │
│  │                              │                                     │   │
│  │           ┌──────────────────▼──────────────────┐                 │   │
│  │           │  DefaultRequestProcessor            │                 │   │
│  │           │  (JSON-RPC + Task Management)       │                 │   │
│  │           └──────────────────┬──────────────────┘                 │   │
│  └──────────────────────────────┼─────────────────────────────────────┘   │
└───────────────────────────────────┼───────────────────────────────────────┘
                                    │
        ┌───────────────────────────┼───────────────────────────┐
        │                           │                           │
        ▼                           ▼                           ▼
┌──────────────┐          ┌──────────────┐          ┌──────────────────────┐
│   A2A-RS     │          │  A2A-MCP     │          │  CRAFTPLAN ADAPTER   │
│   CORE       │          │  BRIDGE      │          │                      │
│              │          │              │          │  - HTTP Client       │
│ - HTTP/WS    │◄────────►│Protocol      │          │  - WebSocket Client  │
│ - Auth       │          │Translation   │          │  - JSON:API Parser   │
│ - Storage    │          │              │          │  - Cache Layer       │
│ - JSON-RPC   │          │              │          │  - Error Mapping     │
└──────────────┘          └──────────────┘          └──────────┬───────────┘
                                                                     │
                                                                     ▼
                                                        ┌──────────────────────┐
                                                        │   CRAFTPLAN (Elixir) │
                                                        │                      │
                                                        │  ┌──────────────┐   │
                                                        │  │Catalog Domain │   │
                                                        │  │(Ash.Resource)│   │
                                                        │  └──────────────┘   │
                                                        │  ┌──────────────┐   │
                                                        │  │Orders Domain │   │
                                                        │  └──────────────┘   │
                                                        │  ┌──────────────┐   │
                                                        │  │Inventory Dom │   │
                                                        │  └──────────────┘   │
                                                        │  ┌──────────────┐   │
                                                        │  │Production    │   │
                                                        │  │Domain        │   │
                                                        │  └──────────────┘   │
                                                        │  ┌──────────────┐   │
                                                        │  │CRM Domain    │   │
                                                        │  └──────────────┘   │
                                                        └──────────┬───────────┘
                                                                   │
                        ┌──────────────────────────────────────────┼──────────────┐
                        │                                          │              │
                        ▼                                          ▼              ▼
              ┌──────────────────┐                    ┌──────────────────┐  ┌──────────┐
              │  PostgreSQL 16   │                    │   MinIO (S3)     │  │Phoenix   │
              │                  │                    │   File Storage   │  │PubSub    │
              └──────────────────┘                    └──────────────────┘  └──────────┘
```

### 1.2 Technology Stack Mapping

| Layer | A2A-RS Ecosystem | Craftplan | Integration Method |
|-------|------------------|-----------|-------------------|
| **Protocol** | A2A v0.3.0 (JSON-RPC 2.0) | JSON:API v1.0 | JSON:API ↔ A2A Message translation |
| **Language** | Rust 1.85+ | Elixir 1.15+ | HTTP/JSON bridge |
| **Transport** | HTTP, WebSocket | Phoenix Channels | WebSocket ↔ Phoenix.Channel |
| **Auth** | JWT, OAuth2, API Keys | AshAuthentication | Shared JWT secret |
| **Storage** | SQLx (task persistence) | AshPostgres | Shared PostgreSQL database |
| **Real-Time** | WebSocket (tokio-tungstenite) | Phoenix PubSub | PubSub ↔ WebSocket adapter |
| **UI** | Axum + Askama (SSR) | Phoenix LiveView | Separate UIs, shared backend |
| **Caching** | Moka (async LRU) | ETS table | Optional 2-layer cache |

### 1.3 Architectural Principles

#### Hexagonal Architecture (Ports & Adapters)

```
┌─────────────────────────────────────────────────────────────┐
│                     APPLICATION LAYER                       │
│  ┌─────────────────┐    ┌────────────────────────────────┐ │
│  │  JSON-RPC       │    │  HTTP/WebSocket Transports      │ │
│  │  Handlers       │    │  (Axum, tokio-tungstenite)      │ │
│  └─────────────────┘    └────────────────────────────────┘ │
└─────────────────┬───────────────────────┬───────────────────┘
                  │                       │
┌─────────────────▼───────────────────────▼───────────────────┐
│                     PORT LAYER                              │
│  ┌──────────────────┐    ┌──────────────────────────────┐  │
│  │ AsyncMessage     │    │  TaskManager                 │  │
│  │ Handler (Trait)  │    │  (Trait)                     │  │
│  └──────────────────┘    └──────────────────────────────┘  │
│  ┌──────────────────┐    ┌──────────────────────────────┐  │
│  │ StreamingHandler │    │  NotificationManager         │  │
│  │ (Trait)          │    │  (Trait)                     │  │
│  └──────────────────┘    └──────────────────────────────┘  │
└─────────────────┬───────────────────────┬───────────────────┘
                  │                       │
┌─────────────────▼───────────────────────▼───────────────────┐
│                    DOMAIN LAYER                             │
│  ┌──────────────┐ ┌──────────────┐ ┌─────────────────────┐ │
│  │   Message    │ │     Task     │ │   AgentCard         │ │
│  │   Artifact   │ │ TaskStatus   │ │   Capabilities      │ │
│  └──────────────┘ └──────────────┘ └─────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

**Key Benefits:**
- Business logic independent of frameworks
- Easy to test (mock ports)
- Swappable adapters
- Clear separation of concerns

#### Domain-Driven Design (DDD)

Craftplan business domains map directly to A2A agents:

| Craftplan Ash Domain | A2A Agent | Responsibilities | Example Operations |
|---------------------|-----------|------------------|-------------------|
| **Catalog** | `CatalogAgent` | Products, BOMs, costs | Query product, calculate BOM cost, create product |
| **Orders** | `OrdersAgent` | Orders, allocations, delivery | Create order, check status, schedule delivery |
| **Inventory** | `InventoryAgent` | Materials, lots, forecasts | Query stock, update levels, forecast demand |
| **Production** | `ProductionAgent` | Batches, consumption, completion | Create batch, allocate materials, complete batch |
| **CRM** | `CrmAgent` | Customers, suppliers, history | Lookup customer, order history, add contact |

---

## 2. Component Architecture

### 2.1 A2A-RS Core Framework

**Purpose**: Type-safe A2A protocol implementation with pluggable transports

**Module Structure:**
```
a2a-rs/
├── domain/
│   ├── core/
│   │   ├── message.rs      # Message, Part, Role
│   │   ├── task.rs         # Task, TaskStatus, TaskState
│   │   ├── artifact.rs     # Artifact, metadata
│   │   └── agent_card.rs   # AgentCard, Capabilities
│   └── protocol/
│       ├── jsonrpc.rs      # JSON-RPC 2.0 wrapper
│       └── errors.rs       # A2AError types
├── port/
│   ├── handler.rs          # AsyncMessageHandler trait
│   ├── task_manager.rs     # TaskManager trait
│   ├── streaming.rs        # StreamingHandler trait
│   └── notification.rs     # NotificationManager trait
├── adapter/
│   ├── http/
│   │   ├── client.rs       # HttpClient (reqwest)
│   │   └── server.rs       # HttpServer (axum)
│   ├── websocket/
│   │   ├── client.rs       # WebSocketClient
│   │   └── server.rs       # WebSocketServer
│   ├── auth/
│   │   ├── jwt.rs          # JwtAuthenticator
│   │   └── oauth2.rs       # OAuth2Authenticator
│   └── storage/
│       ├── memory.rs       # InMemoryTaskStorage
│       └── sqlx.rs         # SqlxTaskStorage
└── application/
    ├── processor.rs        # DefaultRequestProcessor
    └── handlers.rs         # Common handler implementations
```

**Key Types:**

```rust
// Core message types
pub struct Message {
    pub message_id: String,
    pub role: Role,
    pub parts: Vec<Part>,
    pub timestamp: DateTime<Utc>,
}

pub enum Part {
    Text(String),
    Data { data: serde_json::Value, metadata: Option<Metadata> },
    File { uri: String, metadata: Option<Metadata> },
}

pub struct Task {
    pub id: String,
    pub status: TaskStatus,
    pub artifacts: Vec<Artifact>,
    pub history: Vec<TaskStatus>,
}

// Port traits
#[async_trait]
pub trait AsyncMessageHandler: Send + Sync {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        session_id: Option<&str>,
    ) -> Result<Task, A2AError>;
}

#[async_trait]
pub trait TaskManager: Send + Sync {
    async fn create_task(&self, task: Task) -> Result<(), A2AError>;
    async fn get_task(&self, task_id: &str) -> Result<Option<Task>, A2AError>;
    async fn update_task(&self, task_id: &str, status: TaskStatus) -> Result<(), A2AError>;
}
```

**Dependencies**: None (core library)

### 2.2 Craftplan Adapter

**Purpose**: HTTP/WebSocket bridge between Rust agents and Elixir ERP

**Module Structure:**
```
craftplan-adapter/
├── src/
│   ├── lib.rs              # Public API, constants
│   ├── adapter.rs           # CraftplanAdapter (AsyncMessageHandler)
│   ├── client.rs            # CraftplanClient (HTTP/WebSocket)
│   ├── models.rs            # Domain types (Product, Order, etc.)
│   └── error.rs             # CraftplanError, conversions
├── examples/
│   └── craftplan_agent.rs   # Usage example
└── tests/
    └── integration_test.rs  # Integration tests
```

**Key Components:**

```rust
// Configuration
pub struct CraftplanConfig {
    pub base_url: String,
    pub api_key: Option<String>,
    pub jwt_secret: Option<String>,
    pub timeout: Duration,
    pub cache_ttl: Duration,
    pub max_cache_size: usize,
}

// HTTP Client
pub struct CraftplanClient {
    config: CraftplanConfig,
    http_client: reqwest::Client,
    cache: Arc<Mutex<LruCache<String, CacheEntry>>>,
}

impl CraftplanClient {
    // Catalog operations
    pub async fn get_product(&self, id: &str) -> Result<Product, CraftplanError>;
    pub async fn list_products(&self, filters: &ProductFilters) -> Result<Vec<Product>, CraftplanError>;
    pub async fn create_product(&self, params: CreateProductParams) -> Result<Product, CraftplanError>;

    // Order operations
    pub async fn create_order(&self, params: CreateOrderParams) -> Result<Order, CraftplanError>;
    pub async fn get_order(&self, id: &str) -> Result<Order, CraftplanError>;

    // Inventory operations
    pub async fn get_material(&self, id: &str) -> Result<Material, CraftplanError>;
    pub async fn update_stock(&self, params: UpdateStockParams) -> Result<Material, CraftplanError>;

    // Production operations
    pub async fn create_batch(&self, params: CreateBatchParams) -> Result<Batch, CraftplanError>;
    pub async fn get_batch(&self, id: &str) -> Result<Batch, CraftplanError>;
}

// Adapter (AsyncMessageHandler implementation)
pub struct CraftplanAdapter {
    client: Arc<CraftplanClient>,
    ai_client: Option<AiClient>,
}

#[async_trait]
impl AsyncMessageHandler for CraftplanAdapter {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        // Extract intent (using AI or pattern matching)
        let intent = self.extract_intent(message).await?;

        // Route to appropriate domain operation
        match intent {
            Intent::Catalog(op) => self.handle_catalog_operation(task_id, op).await,
            Intent::Orders(op) => self.handle_orders_operation(task_id, op).await,
            Intent::Inventory(op) => self.handle_inventory_operation(task_id, op).await,
            Intent::Production(op) => self.handle_production_operation(task_id, op).await,
            Intent::CRM(op) => self.handle_crm_operation(task_id, op).await,
        }
    }
}
```

**Domain Models:**

```rust
// Catalog domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
    pub id: String,
    pub name: String,
    pub sku: String,
    pub category: ProductCategory,
    pub cost: f64,
    pub price: f64,
    pub bom_id: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BomLineItem {
    pub material_id: String,
    pub quantity: f64,
    pub unit: String,
}

// Orders domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Order {
    pub id: String,
    pub customer_id: String,
    pub status: OrderStatus,
    pub line_items: Vec<OrderLineItem>,
    pub created_at: DateTime<Utc>,
}

// Inventory domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Material {
    pub id: String,
    pub name: String,
    pub sku: String,
    pub current_stock: f64,
    pub unit: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MaterialForecast {
    pub material_id: String,
    pub forecast_date: Date,
    pub predicted_demand: f64,
}

// Production domain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Batch {
    pub id: String,
    pub product_id: String,
    pub status: BatchStatus,
    pub quantity: f64,
    pub allocations: Vec<ProductionAllocation>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProductionAllocation {
    pub material_id: String,
    pub quantity: f64,
    pub status: AllocationStatus,
}
```

### 2.3 Domain Agents

**Purpose**: Business logic for specific Craftplan domains

**Agent Structure:**

```rust
// Agent trait implementation
pub struct CatalogAgent {
    client: Arc<CraftplanClient>,
    cache: Arc<RwLock<LruCache<String, Product>>>,
}

impl CatalogAgent {
    pub fn new(client: Arc<CraftplanClient>) -> Self {
        Self {
            client,
            cache: Arc::new(RwLock::new(LruCache::new(1000))),
        }
    }

    async fn handle_query_product(&self, task_id: &str, product_id: String) -> Result<Task, A2AError> {
        // Check cache first
        if let Some(product) = self.cache.read().await.get(&product_id) {
            return Ok(self.create_product_response(task_id, product));
        }

        // Fetch from Craftplan
        let product = self.client.get_product(&product_id).await
            .map_err(|e| A2AError::internal_error(format!("Failed to fetch product: {}", e)))?;

        // Update cache
        self.cache.write().await.put(product_id.clone(), product.clone());

        // Create response
        Ok(self.create_product_response(task_id, &product))
    }

    async fn handle_calculate_bom_cost(&self, task_id: &str, bom_id: String) -> Result<Task, A2AError> {
        // Fetch BOM components
        let product = self.client.get_product_by_bom(&bom_id).await?;

        // Calculate total cost
        let mut total_cost = 0.0;
        let mut cost_breakdown = Vec::new();

        for item in &product.bom_items {
            let material = self.client.get_material(&item.material_id).await?;
            let item_cost = material.unit_cost * item.quantity;
            total_cost += item_cost;
            cost_breakdown.push(format!("{}: {} × {} = {}", material.name, item.quantity, material.unit_cost, item_cost));
        }

        // Format response
        let response_text = format!(
            "BOM cost for {}:\n{}\n\nTotal: ${:.2}",
            product.name,
            cost_breakdown.join("\n"),
            total_cost
        );

        let task = Task::new(task_id.to_string())
            .with_status(TaskStatus {
                state: TaskState::Completed,
                timestamp: Some(chrono::Utc::now()),
                message: Some("BOM cost calculated successfully".to_string()),
                ..Default::default()
            })
            .with_artifact(Artifact {
                name: "bom_cost".to_string(),
                parts: vec![Part::Text(response_text)],
            });

        Ok(task)
    }
}

#[async_trait]
impl AsyncMessageHandler for CatalogAgent {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        // Extract intent
        let intent = self.extract_intent(message).await?;

        match intent {
            CatalogIntent::QueryProduct { id } => self.handle_query_product(task_id, id).await,
            CatalogIntent::CalculateBomCost { bom_id } => self.handle_calculate_bom_cost(task_id, bom_id).await,
            CatalogIntent::ListProducts { filters } => self.handle_list_products(task_id, filters).await,
        }
    }
}
```

**Composite Agent Pattern:**

```rust
// Combine multiple domain agents
pub struct CompositeCraftplanAgent {
    catalog: Arc<CatalogAgent>,
    orders: Arc<OrdersAgent>,
    inventory: Arc<InventoryAgent>,
    production: Arc<ProductionAgent>,
    crm: Arc<CrmAgent>,
}

impl CompositeCraftplanAgent {
    pub fn new(client: Arc<CraftplanClient>) -> Self {
        Self {
            catalog: Arc::new(CatalogAgent::new(client.clone())),
            orders: Arc::new(OrdersAgent::new(client.clone())),
            inventory: Arc::new(InventoryAgent::new(client.clone())),
            production: Arc::new(ProductionAgent::new(client.clone())),
            crm: Arc::new(CrmAgent::new(client)),
        }
    }
}

#[async_trait]
impl AsyncMessageHandler for CompositeCraftplanAgent {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        // Route to appropriate domain agent
        let domain = self.extract_domain(message).await?;

        match domain {
            Domain::Catalog => self.catalog.handle_message(task_id, message, session_id).await,
            Domain::Orders => self.orders.handle_message(task_id, message, session_id).await,
            Domain::Inventory => self.inventory.handle_message(task_id, message, session_id).await,
            Domain::Production => self.production.handle_message(task_id, message, session_id).await,
            Domain::CRM => self.crm.handle_message(task_id, message, session_id).await,
        }
    }
}
```

### 2.4 A2A-MCP Bridge

**Purpose**: Bidirectional protocol translation between A2A and MCP

**Architecture:**

```
┌────────────────────────────────────────────────────────┐
│                  A2A-MCP BRIDGE                        │
├────────────────────────────────────────────────────────┤
│  ┌──────────────────┐    ┌─────────────────────────┐  │
│  │  A2A → MCP       │    │  MCP → A2A              │  │
│  │  Adapter         │    │  Adapter                │  │
│  │                  │    │                         │  │
│  │  - Task → Tool   │    │  - Tool → Task          │  │
│  │  - Message →     │    │  - Call → Message       │  │
│  │    CallRequest   │    │  - Result → Artifact    │  │
│  │  - Artifact →    │    │                         │  │
│  │    ToolResult    │    │                         │  │
│  └──────────────────┘    └─────────────────────────┘  │
│  ┌──────────────────────────────────────────────────┐ │
│  │         State Management Layer                   │ │
│  │  - Task ↔ Tool call mapping                     │ │
│  │  - Session state synchronization                │ │
│  │  - Capability negotiation                        │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────┘
```

**Implementation:**

```rust
pub struct A2aMcpBridge {
    a2a_client: Arc<dyn AsyncA2AClient>,
    mcp_server: McpServer,
    state_store: Arc<RwLock<StateMapping>>,
}

impl A2aMcpBridge {
    // Convert A2A task to MCP tool result
    pub async fn task_to_tool_result(&self, task: &Task) -> ToolResult {
        let content = task.artifacts.iter()
            .filter_map(|artifact| {
                artifact.parts.iter().find_map(|part| match part {
                    Part::Text(text) => Some(Content::Text(text.clone())),
                    Part::Data { data, .. } => Some(Content::Json(data.clone())),
                    _ => None,
                })
            })
            .collect::<Vec<_>>();

        ToolResult {
            content,
            is_error: matches!(task.status.state, TaskState::Failed),
        }
    }

    // Convert MCP tool call to A2A message
    pub async fn tool_call_to_message(&self, call: &ToolCallRequest) -> Message {
        let parts = vec![Part::Data {
            data: serde_json::to_value(call).unwrap(),
            metadata: None,
        }];

        Message::builder()
            .role(Role::User)
            .parts(parts)
            .message_id(Uuid::new_v4().to_string())
            .build()
    }

    // Expose A2A agents as MCP tools
    pub async fn expose_agent_tools(&self) -> Result<Vec<Tool>, Error> {
        let agent_card = self.a2a_client.get_agent_card().await?;

        let tools = agent_card.capabilities.iter().map(|capability| Tool {
            name: capability.name.clone(),
            description: capability.description.clone(),
            input_schema: capability.input_schema.clone(),
        }).collect();

        Ok(tools)
    }
}
```

### 2.5 A2A Client (Web UI)

**Purpose**: Browser-based interface for agent interactions

**Tech Stack:**
- **Framework**: Axum (web server)
- **Templating**: Askama (type-safe templates)
- **Styling**: CSS (responsive design)
- **Real-time**: WebSocket updates (optional)

**Routes:**

```rust
// Route handlers
async fn index() -> Html<&'static str> {
    Html(include_str!("../templates/index.html"))
}

async fn chat_interface(State(agent_client): State<Arc<AgentClient>>) -> Result<Html<String>, AppError> {
    let agent_card = agent_client.get_agent_card().await?;
    let template = ChatTemplate { agent_card };
    Ok(Html(template.render()?))
}

async fn send_message(
    State(agent_client): State<Arc<AgentClient>>,
    Form(input): Form<MessageInput>,
) -> Result<Redirect, AppError> {
    let message = Message::user_text(input.text, Uuid::new_v4().to_string());
    let task = agent_client.send_task_message(&input.task_id, &message, None, None).await?;
    Ok(Redirect::to(&format!("/chat/{}", input.task_id)))
}

async fn get_task(
    State(agent_client): State<Arc<AgentClient>>,
    Path(task_id): Path<String>,
) -> Result<Json<Task>, AppError> {
    let task = agent_client.get_task(&task_id).await?;
    Ok(Json(task))
}

// WebSocket endpoint for real-time updates
async fn websocket_handler(
    ws: WebSocketUpgrade,
    State(agent_client): State<Arc<AgentClient>>,
    Path(task_id): Path<String>,
) -> Response {
    ws.on_upgrade(move |socket| handle_socket(socket, agent_client, task_id))
}

async fn handle_socket(
    mut socket: WebSocket,
    agent_client: Arc<AgentClient>,
    task_id: String,
) {
    // Subscribe to task updates
    let mut stream = agent_client.subscribe_to_task(&task_id).await.unwrap();

    // Forward updates to WebSocket
    while let Some(update) = stream.next().await {
        let msg = Message::Text(serde_json::to_string(&update).unwrap());
        socket.send(msg).await.unwrap();
    }
}
```

---

## 3. Data Flow Diagrams

### 3.1 Agent Message Processing Flow (HTTP)

```
┌──────────┐
│  Client  │ (User or another agent)
└─────┬────┘
      │ 1. Send message (HTTP POST /message)
      │    Body: { "jsonrpc": "2.0", "method": "message/send",
      │            "params": { "taskId": "task-1", "message": {...} } }
      ▼
┌──────────────────────────────────────────────────────────┐
│  A2A-RS HTTP Server (Axum)                                │
│  ┌────────────────────────────────────────────────────┐  │
│  │ JSON-RPC Handler                                    │  │
│  │ - Parse JSON-RPC 2.0 request                       │  │
│  │ - Extract taskId, message, sessionId               │  │
│  │ - Validate authentication (JWT)                    │  │
│  │ - Route to request processor                       │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 2. request_processor.process_message()
                       ▼
┌──────────────────────────────────────────────────────────┐
│  DefaultRequestProcessor                                 │
│  ┌────────────────────────────────────────────────────┐  │
│  │ 1. Create/get task from task_manager               │  │
│  │ 2. Update task status to "processing"              │  │
│  │ 3. Call message_handler.handle_message()           │  │
│  │ 4. Update task with result                         │  │
│  │ 5. Store updated task                              │  │
│  │ 6. Send push notification (if configured)          │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 3. AsyncMessageHandler.handle_message()
                       ▼
┌──────────────────────────────────────────────────────────┐
│  CatalogAgent (or other domain agent)                   │
│  ┌────────────────────────────────────────────────────┐  │
│  │ 1. Extract intent from message                     │  │
│  │    - Use AI client or pattern matching             │  │
│  │    - Parse structured data (if provided)           │  │
│  │ 2. Check cache for relevant data                   │  │
│  │ 3. If cache miss, call Craftplan API               │  │
│  │ 4. Transform response to A2A artifacts            │  │
│  │ 5. Create completed task with artifacts            │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 4. HTTP GET /api/products/{id}
                       │    Headers: { "Authorization": "Bearer <JWT>",
                       │              "Content-Type": "application/vnd.api+json" }
                       ▼
┌──────────────────────────────────────────────────────────┐
│  Craftplan (Phoenix + Ash)                               │
│  ┌────────────────────────────────────────────────────┐  │
│  │ - JSON:API request handler                         │  │
│  │ - Parse JWT token (AshAuthentication)             │  │
│  │ - Load user from database                          │  │
│  │ - Authorize action (Ash.Policy)                    │  │
│  │ - Execute Ash resource action (read :product)      │  │
│  │ - Query PostgreSQL                                 │  │
│  │ - Serialize to JSON:API format                     │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 5. JSON:API document (200 OK)
                       │    Body: { "data": { "type": "product",
                       │                    "id": "123",
                       │                    "attributes": {...} } }
                       ▼
┌──────────────────────────────────────────────────────────┐
│  CatalogAgent Response Processing                        │
│  ┌────────────────────────────────────────────────────┐  │
│  │ - Deserialize JSON:API to Product struct           │  │
│  │ - Update LRU cache                                 │  │
│  │ - Format natural language response                │  │
│  │ - Create Artifact with response text               │  │
│  │ - Create Task with status = "completed"            │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 6. Return Task
                       ▼
┌──────────────────────────────────────────────────────────┐
│  DefaultRequestProcessor                                 │
│  ┌────────────────────────────────────────────────────┐  │
│  │ - Update task in storage                          │  │
│  │ - Serialize task to JSON-RPC response             │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 7. HTTP Response (200 OK)
                       │    Body: { "jsonrpc": "2.0", "result": { "task": {...} } }
                       ▼
                  ┌─────────┐
                  │  Client │
                  └─────────┘
```

### 3.2 WebSocket Streaming Flow (Real-Time Updates)

```
┌──────────┐
│  Client  │
└─────┬────┘
      │ 1. WebSocket upgrade request
      │    GET /ws?taskId=task-1
      ▼
┌─────────────────────────────────────────────────────────┐
│  A2A-RS WebSocket Server                                │
│  ┌───────────────────────────────────────────────────┐  │
│  │ - Accept WebSocket connection                     │  │
│  │ - Extract taskId from query params                │  │
│  │ - Create unique subscription ID                   │  │
│  │ - Register with StreamingHandler                 │  │
│  │ - Send confirmation message                      │  │
│  └───────────────────────────────────────────────────┘  │
└───────────────────────┬─────────────────────────────────┘
                        │ 2. Subscribe to task updates
                        ▼
┌─────────────────────────────────────────────────────────┐
│  ProductionAgent (with WebSocket support)               │
│  ┌───────────────────────────────────────────────────┐  │
│  │ - Create async task for monitoring                │  │
│  │ - Poll Craftplan for batch status updates         │  │
│  │ - Convert changes to TaskStatusUpdateEvent        │  │
│  │ - Push to WebSocket stream                        │  │
│  └───────────────────────────────────────────────────┘  │
└───────────────────────┬─────────────────────────────────┘
                        │ 3. Periodic HTTP GET /api/production_batches/{id}
                        │    or Phoenix.Channel subscription
                        ▼
┌─────────────────────────────────────────────────────────┐
│  Craftplan Production Domain                            │
│  ┌───────────────────────────────────────────────────┐  │
│  │ - Ash resource: ProductionBatch                   │  │
│  │ - Actions: update_status, add_allocation,        │  │
│  │            complete_batch                          │  │
│  │ - PostgreSQL state change                         │  │
│  │ - Phoenix.PubSub broadcast (if using channels)   │  │
│  └───────────────────────────────────────────────────┘  │
└───────────────────────┬─────────────────────────────────┘
                        │ 4. Batch status update
                        │    Body: { "status": "in_progress",
                        │            "completed_quantity": 50 }
                        ▼
┌─────────────────────────────────────────────────────────┐
│  ProductionAgent Event Processing                       │
│  ┌───────────────────────────────────────────────────┐  │
│  │ - Receive update from Craftplan                   │  │
│  │ - Transform to TaskStatusUpdateEvent             │  │
│  │ - Push to WebSocket stream                       │  │
│  └───────────────────────────────────────────────────┘  │
└───────────────────────┬─────────────────────────────────┘
                        │ 5. Send WebSocket message
                        │    WS: { "type": "status_update",
                        │          "taskId": "task-1",
                        │          "state": "in_progress",
                        │          "message": "Batch is 50% complete" }
                        ▼
                  ┌─────────┐
                  │  Client │
                  └─────────┘
```

### 3.3 MCP Bridge Flow (A2A → MCP)

```
┌──────────────┐
│  LLM Client  │ (using MCP protocol, e.g., Claude Desktop)
└──────┬───────┘
       │ 1. Call MCP tool "craftplan.get_product"
       │    Request: { "id": "product-123" }
       ▼
┌────────────────────────────────────────────────────────┐
│  A2A-MCP Bridge                                        │
│  ┌──────────────────────────────────────────────────┐ │
│  │ RMCP Server (exposing A2A agents as tools)       │ │
│  │ 1. Receive MCP tool call request                 │ │
│  │ 2. Translate to A2A Message                      │ │
│  │ 3. Generate unique task ID                       │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────┬───────────────────────────────┘
                         │ 2. Send A2A message
                         │    POST /message
                         │    Body: { "jsonrpc": "2.0",
                         │           "method": "message/send",
                         │           "params": { "taskId": "task-1",
                         │                       "message": {...} } }
                         ▼
┌────────────────────────────────────────────────────────┐
│  A2A-RS HTTP Server                                   │
│  ┌──────────────────────────────────────────────────┐ │
│  │ - Process message through request processor      │ │
│  │ - Route to CatalogAgent                          │ │
│  │ - Execute business logic                         │ │
│  │ - Return completed task                          │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────┬───────────────────────────────┘
                         │ 3. Return Task
                         │    Body: { "task": { "id": "task-1",
                         │                   "status": { "state": "completed" },
                         │                   "artifacts": [...] } }
                         ▼
┌────────────────────────────────────────────────────────┐
│  A2A-MCP Bridge (Response)                            │
│  ┌──────────────────────────────────────────────────┐ │
│  │ - Convert Task to MCP ToolResult                 │ │
│  │ - Extract artifact content                       │ │
│  │ - Format as MCP response                         │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────┬───────────────────────────────┘
                         │ 4. Return tool result
                         │    Response: { "content": [{ "type": "text",
                         │                           "text": "Product details..." }],
                         │              "isError": false }
                         ▼
                  ┌──────────────┐
                  │  LLM Client  │
                  └──────────────┘
```

---

## 4. Integration Patterns

### 4.1 Pattern 1: Agent per Domain

**Rationale**: One agent per Craftplan Ash domain for clear separation of concerns

**Implementation:**

```rust
// Agent registry
pub fn create_craftplan_agents(config: &CraftplanConfig) -> Vec<Arc<dyn AsyncMessageHandler>> {
    let client = Arc::new(CraftplanClient::new(config.clone()).await.unwrap());

    vec![
        Arc::new(CatalogAgent::new(client.clone())),
        Arc::new(OrdersAgent::new(client.clone())),
        Arc::new(InventoryAgent::new(client.clone())),
        Arc::new(ProductionAgent::new(client.clone())),
        Arc::new(CrmAgent::new(client)),
    ]
}

// Server setup
let agents = create_craftplan_agents(&config);
let message_handler = CompositeMessageHandler::new(agents);
let task_manager = InMemoryTaskStorage::new();
let processor = DefaultRequestProcessor::new(
    message_handler,
    task_manager,
    notification_manager,
    agent_info,
);
```

**Benefits:**
- Clear ownership and responsibility
- Independent scaling (can run agents on different servers)
- Easy to add new domains
- Testable in isolation

### 4.2 Pattern 2: Intent-Based Message Routing

**Rationale**: Extract structured operations from natural language messages

**Implementation (AI-based):**

```rust
use genai::ChatMessage;

impl CatalogAgent {
    async fn extract_intent(&self, message: &Message) -> Result<CatalogIntent, A2AError> {
        // Extract text from message
        let text = message.parts.iter()
            .filter_map(|p| match p {
                Part::Text(t) => Some(t),
                _ => None,
            })
            .next()
            .ok_or_else(|| A2AError::validation_error("No text in message"))?;

        // Use AI to extract structured intent
        let system_prompt = r#"
You are an intent extraction system for a Craftplan ERP agent.
Extract the user's intent from their message and return JSON.

Supported intents:
- QueryProduct: Get details about a specific product
- ListProducts: List products with optional filters
- CalculateBomCost: Calculate the cost of a BOM
- CreateProduct: Create a new product

Return JSON format:
{
  "intent": "QueryProduct|ListProducts|CalculateBomCost|CreateProduct",
  "parameters": {
    "id": "...",  // for QueryProduct
    "filters": {}, // for ListProducts
    "bom_id": "...", // for CalculateBomCost
    "name": "...", "sku": "..." // for CreateProduct
  }
}
"#;

        let chat_messages = vec![
            ChatMessage::system(system_prompt),
            ChatMessage::user(text),
        ];

        let response = self.ai_client.complete(&chat_messages).await?;
        let intent_data: serde_json::Value = serde_json::from_str(&response)?;

        // Parse intent
        match intent_data["intent"].as_str() {
            Some("QueryProduct") => Ok(CatalogIntent::QueryProduct {
                id: intent_data["parameters"]["id"].as_str().unwrap().to_string(),
            }),
            Some("ListProducts") => Ok(CatalogIntent::ListProducts {
                filters: serde_json::from_value(intent_data["parameters"]["filters"].clone()).unwrap(),
            }),
            // ... other intents
            _ => Err(A2AError::validation_error("Unknown intent")),
        }
    }
}
```

**Implementation (Pattern-based fallback):**

```rust
// Fallback to regex patterns when AI is unavailable
impl CatalogAgent {
    fn extract_intent_fallback(&self, message: &Message) -> Result<CatalogIntent, A2AError> {
        let text = self.extract_text(message)?;

        // Pattern: "get product {id}"
        if let Some(captures) = Regex::new(r"(?i)get product (\w+)").unwrap().captures(&text) {
            return Ok(CatalogIntent::QueryProduct {
                id: captures.get(1).unwrap().as_str().to_string(),
            });
        }

        // Pattern: "list products"
        if text.to_lowercase().contains("list products") {
            return Ok(CatalogIntent::ListProducts {
                filters: ProductFilters::default(),
            });
        }

        // Pattern: "calculate bom cost"
        if let Some(captures) = Regex::new(r"(?i)calculate bom cost (\w+)").unwrap().captures(&text) {
            return Ok(CatalogIntent::CalculateBomCost {
                bom_id: captures.get(1).unwrap().as_str().to_string(),
            });
        }

        Err(A2AError::validation_error("Could not extract intent"))
    }
}
```

### 4.3 Pattern 3: Caching Layer

**Rationale**: Reduce load on Craftplan API and improve response times

**Implementation:**

```rust
use moka::future::Cache;

pub struct CachedCraftplanClient {
    inner: CraftplanClient,
    cache: Cache<String, CacheEntry>,
}

struct CacheEntry {
    data: serde_json::Value,
    timestamp: DateTime<Utc>,
}

impl CachedCraftplanClient {
    pub fn new(inner: CraftplanClient, ttl: Duration) -> Self {
        let cache = Cache::builder()
            .time_to_live(ttl)
            .max_capacity(1000)
            .build();

        Self { inner, cache }
    }

    pub async fn get_product(&self, id: &str) -> Result<Product, CraftplanError> {
        let cache_key = format!("product:{}", id);

        // Try cache first
        if let Some(entry) = self.cache.get(&cache_key).await {
            return Ok(serde_json::from_value(entry.data).unwrap());
        }

        // Cache miss - fetch from API
        let product = self.inner.get_product(id).await?;

        // Update cache
        self.cache.insert(
            cache_key,
            CacheEntry {
                data: serde_json::to_value(&product).unwrap(),
                timestamp: Utc::now(),
            },
        ).await;

        Ok(product)
    }

    // Invalidate cache on mutations
    pub async fn update_product(&self, id: &str, params: UpdateProductParams) -> Result<Product, CraftplanError> {
        let product = self.inner.update_product(id, params).await?;

        // Invalidate cache
        self.cache.invalidate(&format!("product:{}", id)).await;

        Ok(product)
    }
}
```

**Cache TTL Strategy:**

| Data Type | TTL | Rationale |
|-----------|-----|-----------|
| Products | 5 minutes | Rarely changes, read-heavy |
| Materials | 5 minutes | Stable reference data |
| Orders | 0 seconds (no cache) | Must be fresh |
| BOMs | 10 minutes | Changes infrequently |
| Inventory levels | 30 seconds | Changes frequently |

### 4.4 Pattern 4: Event-Driven Updates

**Rationale**: Push updates to clients instead of polling

**Implementation (Phoenix.Channel subscription):**

```rust
use tokio_tungstenite::WebSocketStream;

pub struct ProductionBatchMonitor {
    phoenix_url: String,
    ws_stream: Option<WebSocketStream<...>>,
}

impl ProductionBatchMonitor {
    pub async fn subscribe_to_batch_updates(&mut self, batch_id: &str) -> impl Stream<Item = BatchUpdate> {
        // Connect to Phoenix WebSocket
        let url = format!("{}/production_batch/websocket", self.phoenix_url);
        let (ws_stream, _) = connect_async(&url).await.unwrap();

        // Join Phoenix channel
        let join_msg = json!({
            "topic": "production_batch:{}".format(batch_id),
            "event": "phx_join",
            "payload": {},
            "ref": 1
        });
        ws_stream.send(Message::Text(join_msg.to_string())).await.unwrap();

        // Convert WebSocket messages to stream
        ws_stream.filter_map(|msg| async {
            match msg {
                Ok(Message::Text(text)) => {
                    let phoenix_msg: PhoenixMessage = serde_json::from_str(&text).ok()?;
                    match phoenix_msg.event.as_str() {
                        "batch_update" => Some(BatchUpdate::from(phoenix_msg.payload)),
                        _ => None,
                    }
                }
                _ => None,
            }
        })
    }
}
```

**Alternative (Polling with exponential backoff):**

```rust
pub struct PollingBatchMonitor {
    client: CraftplanClient,
    batch_id: String,
    interval: Duration,
}

impl Stream for PollingBatchMonitor {
    type Item = BatchUpdate;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        // Poll Craftplan API for updates
        let batch = self.client.get_batch(&self.batch_id).await.unwrap();

        // Emit update if status changed
        if batch.status != self.last_status {
            self.last_status = batch.status.clone();
            Poll::Ready(Some(BatchUpdate { batch }))
        } else {
            // Schedule next poll
            cx.waker().wake_by_time(Instant::now() + self.interval);
            Poll::Pending
        }
    }
}
```

### 4.5 Pattern 5: Composite Agents (Workflows)

**Rationale**: Orchestrate multi-domain workflows

**Example: Order Fulfillment Workflow**

```rust
pub struct OrderFulfillmentAgent {
    catalog: Arc<CatalogAgent>,
    orders: Arc<OrdersAgent>,
    inventory: Arc<InventoryAgent>,
    production: Arc<ProductionAgent>,
}

#[async_trait]
impl AsyncMessageHandler for OrderFulfillmentAgent {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        // Extract order details from message
        let order_params = self.extract_order_params(message)?;

        // Step 1: Create order
        let order = self.orders.create_order(task_id, order_params).await?;

        // Step 2: Check inventory for all line items
        let mut allocations = Vec::new();
        for item in &order.line_items {
            let material = self.inventory.get_material(&item.product_id).await?;

            if material.current_stock < item.quantity {
                // Step 3: Schedule production batch
                let batch_params = CreateBatchParams {
                    product_id: item.product_id.clone(),
                    quantity: item.quantity - material.current_stock,
                };
                let batch = self.production.create_batch(task_id, batch_params).await?;
                allocations.push(Allocation::from_batch(batch));
            } else {
                allocations.push(Allocation::from_stock(material));
            }
        }

        // Step 4: Update order with allocations
        let updated_order = self.orders.allocate_materials(task_id, order.id, allocations).await?;

        // Create completion task
        let response_text = format!(
            "Order {} created successfully.\nAllocations:\n{}",
            updated_order.id,
            allocations.iter().map(|a| format!("- {}", a)).join("\n")
        );

        let task = Task::new(task_id.to_string())
            .with_status(TaskStatus {
                state: TaskState::Completed,
                timestamp: Some(Utc::now()),
                message: Some("Order fulfilled successfully".to_string()),
                ..Default::default()
            })
            .with_artifact(Artifact {
                name: "order_fulfillment".to_string(),
                parts: vec![Part::Text(response_text)],
            });

        Ok(task)
    }
}
```

---

## 5. Security Architecture

### 5.1 Authentication Flow

```
┌──────────┐
│  Client  │
└─────┬────┘
      │ 1. Request with JWT
      │    Authorization: Bearer eyJhbGciOiJIUzI1NiIs...
      ▼
┌─────────────────────────────────────────────────────────┐
│  A2A-RS JWT Authenticator                                │
│  ┌───────────────────────────────────────────────────┐  │
│  │ 1. Extract JWT from Authorization header          │  │
│  │ 2. Verify signature with JWT_SECRET               │  │
│  │ 3. Check expiration (exp claim)                   │  │
│  │ 4. Extract user claims (sub, roles, tenant_id)    │  │
│  │ 5. Return AuthContext                             │  │
│  └───────────────────────────────────────────────────┘  │
└──────────────────────┬──────────────────────────────────┘
                       │ 2. Authenticated request
                       │    Authorization: Bearer eyJhbGciOiJIUzI1NiIs...
                       ▼
┌─────────────────────────────────────────────────────────┐
│  CraftplanClient (HTTP request to Craftplan)            │
│  ┌───────────────────────────────────────────────────┐  │
│  │ GET /api/products/123                              │  │
│  │ Host: craftplan:4000                               │  │
│  │ Authorization: Bearer eyJhbGciOiJIUzI1NiIs...      │  │
│  │ Content-Type: application/vnd.api+json             │  │
│  └───────────────────────────────────────────────────┘  │
└──────────────────────┬──────────────────────────────────┘
                       │ 3. Forwarded JWT
                       ▼
┌─────────────────────────────────────────────────────────┐
│  Craftplan AshAuthentication                            │
│  ┌───────────────────────────────────────────────────┐  │
│  │ 1. Verify JWT signature (same secret)             │  │
│  │ 2. Load user from database (by sub claim)         │  │
│  │ 3. Check authorization policies (Ash.Policy)      │  │
│  │ 4. Execute action or return 403                   │  │
│  └───────────────────────────────────────────────────┘  │
└──────────────────────┬──────────────────────────────────┘
                       │ 4. Authorized query
                       ▼
              ┌─────────────────────┐
              │  PostgreSQL         │
              │  (data access)      │
              └─────────────────────┘
```

### 5.2 JWT Token Structure

```json
{
  "header": {
    "alg": "HS256",
    "typ": "JWT"
  },
  "payload": {
    "sub": "user_123",
    "name": "John Doe",
    "email": "john@example.com",
    "roles": ["admin"],
    "iss": "craftplan",
    "aud": "a2a-agents",
    "https://craftplan.app/tenant_id": "tenant_456",
    "exp": 1735725680,
    "iat": 1735722080
  }
}
```

**Configuration:**

```bash
# Both systems share the same JWT_SECRET
# .env
JWT_SECRET=$(openssl rand -base64 32)
```

```rust
// A2A-RS configuration
let authenticator = JwtAuthenticator::new(
    std::env::var("JWT_SECRET").unwrap(),
    "a2a-agents".to_string(), // audience
    "craftplan".to_string(),  // issuer
);

// Craftplan configuration
# config/dev.exs
config :craftplan, CraftplanWeb.Auth.Guardian,
  issuer: "craftplan",
  secret_key: System.get_env("JWT_SECRET"),
  ttl: {1, :hour}
```

### 5.3 Authorization Patterns

**Role-Based Access Control (RBAC):**

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Role {
    Admin,
    Staff,
    ReadOnly,
}

pub struct AuthContext {
    pub user_id: String,
    pub tenant_id: String,
    pub roles: Vec<Role>,
}

pub fn authorize(context: &AuthContext, action: &str, resource: &str) -> bool {
    match action {
        "create" | "update" | "delete" => {
            context.roles.contains(&Role::Admin)
        }
        "read" => {
            context.roles.contains(&Role::Admin) ||
            context.roles.contains(&Role::Staff) ||
            context.roles.contains(&Role::ReadOnly)
        }
        _ => false,
    }
}
```

**Resource-Level Authorization:**

```rust
// Check tenant access
pub fn authorize_tenant_access(context: &AuthContext, resource_tenant_id: &str) -> bool {
    context.tenant_id == resource_tenant_id
}

// Check ownership
pub fn authorize_ownership(context: &AuthContext, resource_owner_id: &str) -> bool {
    context.user_id == resource_owner_id || context.roles.contains(&Role::Admin)
}
```

### 5.4 Security Best Practices

1. **Always use HTTPS in production**
2. **Rotate JWT secrets regularly**
3. **Implement rate limiting** (see Performance section)
4. **Validate all inputs** (see Error Handling section)
5. **Log all authentication attempts**
6. **Use short-lived tokens** (1 hour TTL recommended)
7. **Implement token refresh mechanism**
8. **Never log sensitive data** (tokens, passwords)

---

## 6. API Contracts & Interfaces

### 6.1 A2A → Craftplan (JSON:API)

**GET /api/products/{id}**

```http
GET /api/products/product-123 HTTP/1.1
Host: craftplan:4000
Authorization: Bearer <JWT_TOKEN>
Content-Type: application/vnd.api+json
Accept: application/vnd.api+json
```

**Response (200 OK):**

```json
{
  "data": {
    "id": "product-123",
    "type": "product",
    "attributes": {
      "name": "Artisan Sourdough Bread",
      "sku": "BREAD-001",
      "cost": 2.50,
      "price": 6.00,
      "created_at": "2026-01-15T10:00:00Z",
      "updated_at": "2026-01-15T10:00:00Z"
    },
    "relationships": {
      "bom": {
        "links": {
          "self": "/api/products/product-123/relationships/bom",
          "related": "/api/products/product-123/bom"
        }
      },
      "category": {
        "data": { "type": "product_category", "id": "cat-1" }
      }
    }
  },
  "links": {
    "self": "/api/products/product-123"
  }
}
```

**POST /api/orders**

```http
POST /api/orders HTTP/1.1
Host: craftplan:4000
Authorization: Bearer <JWT_TOKEN>
Content-Type: application/vnd.api+json
Accept: application/vnd.api+json

{
  "data": {
    "type": "order",
    "attributes": {
      "customer_id": "customer-456"
    },
    "relationships": {
      "line_items": {
        "data": [
          { "type": "order_line_item", "attributes": { "product_id": "product-123", "quantity": 10 } }
        ]
      }
    }
  }
}
```

### 6.2 Craftplan → A2A (Direct A2A Protocol)

**POST /message**

```http
POST /message HTTP/1.1
Host: a2a-agents:8080
Authorization: Bearer <JWT_TOKEN>
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "method": "message/send",
  "params": {
    "taskId": "task-789",
    "message": {
      "messageId": "msg-456",
      "role": "user",
      "parts": [
        {
          "text": "What is the cost of product BREAD-001?"
        }
      ],
      "timestamp": "2026-02-04T12:00:00Z"
    },
    "sessionId": "session-123"
  },
  "id": 1
}
```

**Response (200 OK):**

```json
{
  "jsonrpc": "2.0",
  "result": {
    "task": {
      "id": "task-789",
      "status": {
        "state": "completed",
        "timestamp": "2026-02-04T12:00:01Z",
        "message": "Product cost retrieved successfully"
      },
      "artifacts": [
        {
          "name": "product_cost",
          "parts": [
            {
              "text": "The cost of product BREAD-001 (Artisan Sourdough Bread) is $2.50. This includes flour ($1.20), water ($0.10), salt ($0.05), and yeast ($0.15), plus labor ($1.00)."
            }
          ]
        }
      ],
      "history": [
        {
          "state": "created",
          "timestamp": "2026-02-04T12:00:00Z",
          "message": "Task created"
        },
        {
          "state": "processing",
          "timestamp": "2026-02-04T12:00:00Z",
          "message": "Processing message"
        },
        {
          "state": "completed",
          "timestamp": "2026-02-04T12:00:01Z",
          "message": "Product cost retrieved successfully"
        }
      ]
    }
  },
  "id": 1
}
```

### 6.3 Message Schema Mapping

| A2A Message | Craftplan Resource | HTTP Method | JSON:API Endpoint |
|-------------|-------------------|-------------|------------------|
| `QueryProduct { id }` | `Craftplan.Catalog.Product` | GET | `/api/products/{id}` |
| `ListProducts { filters }` | `Craftplan.Catalog.Product` | GET | `/api/products?filter[name]=bread` |
| `CreateProduct { name, sku, ... }` | `Craftplan.Catalog.Product` | POST | `/api/products` |
| `CreateOrder { customer_id, items }` | `Craftplan.Orders.Order` | POST | `/api/orders` |
| `GetOrder { id }` | `Craftplan.Orders.Order` | GET | `/api/orders/{id}` |
| `CheckStock { material_id }` | `Craftplan.Inventory.Material` | GET | `/api/materials/{id}/lots` |
| `UpdateStock { material_id, quantity }` | `Craftplan.Inventory.Material` | PATCH | `/api/materials/{id}` |
| `CreateBatch { product_id, quantity }` | `Craftplan.Production.ProductionBatch` | POST | `/api/production_batches` |
| `GetBatch { id }` | `Craftplan.Production.ProductionBatch` | GET | `/api/production_batches/{id}` |
| `CompleteBatch { batch_id }` | `Craftplan.Production.ProductionBatch` | PATCH | `/api/production_batches/{id}` |
| `LookupCustomer { id }` | `Craftplan.CRM.Customer` | GET | `/api/customers/{id}` |

---

## 7. Deployment Architecture

### 7.1 Development Environment

**docker-compose.yml:**

```yaml
services:
  # Craftplan ERP
  craftplan:
    image: ghcr.io/puemos/craftplan:latest
    ports:
      - "4000:4000"
    environment:
      DATABASE_URL: "ecto://postgres:${POSTGRES_PASSWORD}@postgres/craftplan"
      JWT_SECRET: "${JWT_SECRET}"
      SECRET_KEY_BASE: "${SECRET_KEY_BASE}"
      AWS_S3_SCHEME: "http://"
      AWS_S3_HOST: minio
      AWS_ACCESS_KEY_ID: "${MINIO_ROOT_USER}"
      AWS_SECRET_ACCESS_KEY: "${MINIO_ROOT_PASSWORD}"
    depends_on:
      postgres:
        condition: service_healthy
      minio:
        condition: service_started
    restart: unless-stopped

  # A2A Agents (with Craftplan adapter)
  a2a-agents:
    build:
      context: ./a2a-rs/a2a-agents
      dockerfile: Dockerfile
    ports:
      - "8080:8080"  # HTTP
      - "8081:8081"  # WebSocket
    environment:
      CRAFTPLAN_API_URL: "http://craftplan:4000"
      JWT_SECRET: "${JWT_SECRET}"
      RUST_LOG: "info"
      DATABASE_URL: "postgresql://postgres:${POSTGRES_PASSWORD}@postgres/craftplan"
    depends_on:
      - craftplan
      - postgres
    restart: unless-stopped

  # A2A Web Client
  a2a-client:
    build:
      context: ./a2a-rs/a2a-client
      dockerfile: Dockerfile
    ports:
      - "3000:3000"
    environment:
      AGENT_URL: "http://a2a-agents:8080"
      RUST_LOG: "info"
    depends_on:
      - a2a-agents
    restart: unless-stopped

  # PostgreSQL (shared database)
  postgres:
    image: postgres:16
    environment:
      POSTGRES_DB: craftplan
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: "${POSTGRES_PASSWORD}"
      PGDATA: /var/lib/postgresql/data/pgdata
    volumes:
      - postgres_data:/var/lib/postgresql/data
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 5s
      timeout: 5s
      retries: 5
    restart: unless-stopped

  # MinIO (S3-compatible storage)
  minio:
    image: minio/minio:latest
    entrypoint: sh
    command: -c 'mkdir -p /data/craftplan && /usr/bin/minio server /data --console-address ":9001"'
    environment:
      MINIO_ROOT_USER: "${MINIO_ROOT_USER}"
      MINIO_ROOT_PASSWORD: "${MINIO_ROOT_PASSWORD}"
    ports:
      - "9000:9000"
      - "9001:9001"
    volumes:
      - minio_data:/data
    restart: unless-stopped

volumes:
  postgres_data:
  minio_data:
```

**Quick Start:**

```bash
# Generate secrets
JWT_SECRET=$(openssl rand -base64 32)
SECRET_KEY_BASE=$(openssl rand -base64 48)
POSTGRES_PASSWORD=$(openssl rand -base64 16)
MINIO_ROOT_USER=minioadmin
MINIO_ROOT_PASSWORD=$(openssl rand -base64 16)

# Create .env file
cat > .env << EOF
JWT_SECRET=${JWT_SECRET}
SECRET_KEY_BASE=${SECRET_KEY_BASE}
POSTGRES_PASSWORD=${POSTGRES_PASSWORD}
MINIO_ROOT_USER=${MINIO_ROOT_USER}
MINIO_ROOT_PASSWORD=${MINIO_ROOT_PASSWORD}
EOF

# Start all services
docker compose up -d

# Verify
curl http://localhost:4000/api/health  # Craftplan
curl http://localhost:8080/agent-card   # A2A Agents
open http://localhost:3000              # Web Client
```

### 7.2 Production Environment (Kubernetes)

**deployment.yaml:**

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: a2a-agents
spec:
  replicas: 3
  selector:
    matchLabels:
      app: a2a-agents
  template:
    metadata:
      labels:
        app: a2a-agents
    spec:
      containers:
      - name: a2a-agents
        image: ghcr.io/your-org/a2a-agents:latest
        ports:
        - containerPort: 8080
          name: http
        - containerPort: 8081
          name: websocket
        env:
        - name: CRAFTPLAN_API_URL
          value: "http://craftplan-service:4000"
        - name: JWT_SECRET
          valueFrom:
            secretKeyRef:
              name: craftplan-secrets
              key: jwt-secret
        - name: RUST_LOG
          value: "info"
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 30
        readinessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 10
---
apiVersion: v1
kind: Service
metadata:
  name: a2a-agents-service
spec:
  selector:
    app: a2a-agents
  ports:
  - name: http
    port: 8080
    targetPort: 8080
  - name: websocket
    port: 8081
    targetPort: 8081
  type: LoadBalancer
---
apiVersion: v1
kind: Secret
metadata:
  name: craftplan-secrets
type: Opaque
data:
  jwt-secret: <base64-encoded-secret>
  db-password: <base64-encoded-password>
```

### 7.3 Production Environment (Fly.io)

**fly.toml:**

```toml
app = "a2a-agents"

[build]
  dockerfile = "./a2a-rs/a2a-agents/Dockerfile"

[env]
  CRAFTPLAN_API_URL = "https://craftplan.fly.dev"
  RUST_LOG = "info"
  JWT_SECRET = "your-production-secret"

[[services]]
  http_checks = []
  internal_port = 8080
  protocol = "tcp"

  [[services.ports]]
    port = 80
    handlers = ["http"]

  [[services.ports]]
    port = 443
    handlers = ["tls", "http"]

  [services.concurrency]
    type = "connections"
    hard_limit = 250
    soft_limit = 200

[[services]]
  internal_port = 8081
  protocol = "tcp"

  [[services.ports]]
    port = 8081
    handlers = ["http"]
```

**Deploy:**

```bash
# Login to Fly.io
fly auth login

# Deploy agents
fly deploy --config a2a-agents/fly.toml

# Deploy client
fly deploy --config a2a-client/fly.toml

# Check status
fly status
```

---

## 8. Error Handling Strategy

### 8.1 Error Categories

| Category | Example | HTTP Status | A2A Error Code | Retryable |
|----------|---------|-------------|----------------|-----------|
| **Authentication** | Invalid JWT | 401 | `AUTHENTICATION_FAILED` | No |
| **Authorization** | Insufficient permissions | 403 | `AUTHORIZATION_FAILED` | No |
| **Not Found** | Product doesn't exist | 404 | `RESOURCE_NOT_FOUND` | No |
| **Validation** | Invalid input data | 422 | `VALIDATION_ERROR` | No |
| **Conflict** | Duplicate SKU | 409 | `CONFLICT` | No |
| **Rate Limit** | Too many requests | 429 | `RATE_LIMIT_EXCEEDED` | Yes |
| **Server Error** | Database connection failed | 500 | `INTERNAL_ERROR` | Yes |
| **Service Unavailable** | Craftplan API down | 503 | `SERVICE_UNAVAILABLE` | Yes |
| **Timeout** | Request took too long | 504 | `TIMEOUT` | Yes |

### 8.2 Error Response Formats

**A2A Protocol Error:**

```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32602,
    "message": "Invalid params",
    "data": {
      "field": "product_id",
      "reason": "must be a valid UUID"
    }
  },
  "id": 1
}
```

**Craftplan API Error (JSON:API):**

```json
{
  "errors": [
    {
      "title": "Invalid attribute",
      "detail": "SKU must be unique",
      "source": {
        "pointer": "/data/attributes/sku"
      },
      "status": "422"
    }
  ]
}
```

### 8.3 Error Handling Implementation

```rust
// Custom error type
#[derive(Debug, thiserror::Error)]
pub enum CraftplanError {
    #[error("Product not found: {0}")]
    ProductNotFound(String),

    #[error("Authentication failed: {0}")]
    Unauthorized(String),

    #[error("Validation error: {0}")]
    Validation(String),

    #[error("Internal error: {0}")]
    Internal(String),

    #[error("API error: {status} - {message}")]
    ApiError { status: u16, message: String },
}

// Convert to A2A error
impl From<CraftplanError> for A2AError {
    fn from(err: CraftplanError) -> Self {
        match err {
            CraftplanError::ProductNotFound(id) => {
                A2AError::resource_not_found(&format!("Product {}", id))
            }
            CraftplanError::Unauthorized(msg) => {
                A2AError::authentication_failed(&msg)
            }
            CraftplanError::Validation(msg) => {
                A2AError::validation_error(&msg)
            }
            CraftplanError::Internal(msg) => {
                A2AError::internal_error(&format!("Craftplan API error: {}", msg))
            }
            CraftplanError::ApiError { status, message } => {
                match status {
                    401 => A2AError::authentication_failed(&message),
                    403 => A2AError::authorization_failed(&message),
                    404 => A2AError::resource_not_found(&message),
                    422 => A2AError::validation_error(&message),
                    429 => A2AError::rate_limit_exceeded(&message),
                    500..=599 => A2AError::internal_error(&message),
                    _ => A2AError::internal_error(&format!("API error: {}", message)),
                }
            }
        }
    }
}
```

### 8.4 Retry Strategy

```rust
use tokio::time::{sleep, Duration};
use backoff::{ExponentialBackoff, future::retry};

pub struct RetryConfig {
    pub max_retries: u32,
    pub initial_delay: Duration,
    pub max_delay: Duration,
    pub multiplier: f64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            max_retries: 3,
            initial_delay: Duration::from_millis(100),
            max_delay: Duration::from_secs(10),
            multiplier: 2.0,
        }
    }
}

pub async fn retry_request<F, T, E>(
    config: RetryConfig,
    operation: F,
) -> Result<T, E>
where
    F: Fn() -> Pin<Box<dyn Future<Output = Result<T, E>> + Send>>,
    E: std::fmt::Display,
{
    let mut attempt = 0;
    let mut delay = config.initial_delay;

    loop {
        match operation().await {
            Ok(result) => return Ok(result),
            Err(err) if attempt < config.max_retries => {
                // Check if error is retryable
                if is_retryable_error(&err) {
                    tracing::warn!("Attempt {} failed, retrying in {:?}", attempt + 1, delay);
                    sleep(delay).await;
                    attempt += 1;
                    delay = std::cmp::min(delay * config.multiplier as u32, config.max_delay);
                } else {
                    return Err(err);
                }
            }
            Err(err) => return Err(err),
        }
    }
}

fn is_retryable_error<E>(err: &E) -> bool {
    // Check error type or message
    let err_str = format!("{}", err);
    err_str.contains("timeout") ||
    err_str.contains("connection refused") ||
    err_str.contains("503") ||
    err_str.contains("504")
}
```

---

## 9. Performance Optimization

### 9.1 Caching Strategy

**Multi-Level Caching:**

```rust
// L1: In-memory LRU cache (agent-level)
let lru_cache = Arc::new(RwLock::new(LruCache::new(1000)));

// L2: Redis cache (optional, for distributed systems)
let redis_client = redis::Client::open("redis://localhost:6379").unwrap();

pub async fn get_product_cached(
    lru_cache: Arc<RwLock<LruCache<String, Product>>>,
    redis_client: Option<redis::Client>,
    id: &str,
) -> Result<Product, CraftplanError> {
    let cache_key = format!("product:{}", id);

    // Try L1 cache first
    if let Some(product) = lru_cache.read().await.get(&cache_key) {
        return Ok(product.clone());
    }

    // Try L2 cache (Redis)
    if let Some(redis) = redis_client {
        let mut conn = redis.get_async_connection().await?;
        let cached: Option<String> = redis.get(&cache_key).await?;
        if let Some(json) = cached {
            let product: Product = serde_json::from_str(&json)?;
            // Populate L1 cache
            lru_cache.write().await.put(cache_key.clone(), product.clone());
            return Ok(product);
        }
    }

    // Cache miss - fetch from API
    let product = fetch_product_from_api(id).await?;

    // Populate caches
    lru_cache.write().await.put(cache_key.clone(), product.clone());

    if let Some(redis) = redis_client {
        let json = serde_json::to_string(&product)?;
        let mut conn = redis.get_async_connection().await?;
        redis.set_ex(&cache_key, json, 300).await?; // 5 min TTL
    }

    Ok(product)
}
```

### 9.2 Connection Pooling

```rust
// HTTP client with connection pooling
let http_client = reqwest::Client::builder()
    .pool_max_idle_per_host(10) // Keep 10 idle connections per host
    .pool_idle_timeout(Duration::from_secs(30)) // Close idle connections after 30s
    .connect_timeout(Duration::from_secs(10))
    .timeout(Duration::from_secs(30))
    .build()?;
```

### 9.3 Concurrent Requests

```rust
// Fetch multiple products in parallel
use futures::stream::{self, StreamExt};

pub async fn fetch_products_parallel(
    client: &CraftplanClient,
    product_ids: Vec<String>,
) -> Result<Vec<Product>, CraftplanError> {
    let products = stream::iter(product_ids)
        .map(|id| client.get_product(&id))
        .buffer_unordered(10) // Max 10 concurrent requests
        .collect::<Vec<_>>()
        .await;

    products.into_iter().collect()
}
```

### 9.4 Rate Limiting

```rust
use governor::{Quota, RateLimiter};

// Create rate limiter: 100 requests per second
let quota = Quota::per_second(nonzero!(100u32));
let rate_limiter = RateLimiter::direct(quota);

pub async fn rate_limited_request<F, T, E>(
    rate_limiter: &RateLimiter<...>,
    operation: F,
) -> Result<T, E>
where
    F: Future<Output = Result<T, E>>,
{
    // Wait until rate limit allows request
    rate_limiter.until_ready().await;

    // Execute operation
    operation.await
}
```

### 9.5 Performance Targets

| Metric | Target | Measurement Method |
|--------|--------|-------------------|
| **Agent Response Time** | < 500ms (p95) | Task processing duration |
| **End-to-End Latency** | < 1s (p95) | Client request → response |
| **Cache Hit Rate** | > 80% | Cache hits / total requests |
| **WebSocket Message Delay** | < 100ms | Time to push update |
| **Concurrent Users** | 100+ | Simultaneous connections |
| **Throughput** | 1000 req/min | Sustained request rate |
| **Memory per Agent** | < 100MB | Docker stats |
| **CPU per Agent** | < 50% (1 core) | Docker stats |

---

## 10. Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2) ✅ COMPLETE

**Goal**: Basic HTTP communication between A2A agents and Craftplan

**Completed:**
- [x] Set up a2a-agents project structure
- [x] Implement `CraftplanClient` with HTTP requests to JSON:API
- [x] Create `CraftplanAdapter` with basic operations
- [x] Add JWT authentication using shared secret
- [x] Deploy to local dev environment with docker-compose
- [x] Write integration tests for adapter → API communication

**Deliverables:**
- ✅ Working `CraftplanAdapter` that can query products from Craftplan
- ✅ Authentication with JWT tokens
- ✅ Integration test suite

### Phase 2: Domain Agents (Weeks 3-4) 🚧 IN PROGRESS

**Goal**: Implement agents for all major Craftplan domains

**In Progress:**
- [ ] Implement `CatalogAgent` with full CRUD operations
- [ ] Implement `OrdersAgent` with order lifecycle management
- [ ] Implement `InventoryAgent` with stock tracking
- [ ] Implement `ProductionAgent` with batch management
- [ ] Implement `CrmAgent` with customer lookup
- [ ] Add caching layer (Moka LRU cache)
- [ ] Implement AI-based intent extraction

**Planned:**
- [ ] Write comprehensive tests for all agents
- [ ] Add structured logging with tracing
- [ ] Document agent capabilities

**Deliverables:**
- 5 domain agents with full CRUD capabilities
- LRU cache for frequently accessed data
- AI-powered intent extraction

### Phase 3: WebSocket Streaming (Weeks 5-6)

**Goal**: Real-time updates for long-running operations

**Tasks:**
- [ ] Implement WebSocket server in a2a-agents
- [ ] Add Phoenix.Channel subscriptions in agents
- [ ] Stream production batch status updates
- [ ] Stream inventory level changes
- [ ] Handle connection reconnection with exponential backoff
- [ ] Add ping/pong heartbeat mechanism
- [ ] Update a2a-client to support WebSocket
- [ ] Write WebSocket integration tests

**Deliverables:**
- WebSocket endpoint at `ws://localhost:8081`
- Real-time updates for production batches
- Client subscription management

### Phase 4: MCP Bridge (Weeks 7-8)

**Goal**: Enable A2A agents to be used as MCP tools

**Tasks:**
- [ ] Implement RMCP server in a2a-mcp
- [ ] Create A2A → MCP message converter
- [ ] Create MCP → A2A message converter
- [ ] Expose agents as MCP tools
- [ ] Implement state management across protocols
- [ ] Add protocol capability negotiation
- [ ] Write cross-protocol integration tests
- [ ] Document MCP tool usage

**Deliverables:**
- Working a2a-mcp bridge
- Agents callable from MCP clients (Claude, ChatGPT)
- Documentation for MCP tool usage

### Phase 5: Advanced Features (Weeks 9-10)

**Goal**: Production-ready features

**Tasks:**
- [ ] Implement rate limiting per user/agent
- [ ] Add structured logging with tracing
- [ ] Collect metrics (Prometheus format)
- [ ] Implement distributed tracing (OpenTelemetry)
- [ ] Add health check endpoints (`/health`, `/ready`)
- [ ] Implement graceful shutdown
- [ ] Add configuration validation
- [ ] Write performance benchmarks

**Deliverables:**
- Production monitoring stack
- Rate limiting middleware
- Health check endpoints
- Performance benchmarks

### Phase 6: Client UI Enhancements (Weeks 11-12)

**Goal**: Enhanced web client for agent interactions

**Tasks:**
- [ ] Extend a2a-client with Craftplan-specific UI
- [ ] Add dynamic form generation for agent inputs
- [ ] Display agent responses with rich formatting
- [ ] Add task history and artifact browser
- [ ] Implement WebSocket for real-time updates
- [ ] Add error handling and retry UI
- [ ] Implement session management
- [ ] Write E2E tests for UI

**Deliverables:**
- Enhanced web client at `http://localhost:3000`
- Agent interaction UI
- Real-time updates in browser

---

## 11. Integration Checklist

### 11.1 Pre-Integration Checklist

**Environment Setup:**
- [ ] Rust 1.85+ installed
- [ ] Elixir 1.15+ and Erlang 27+ installed
- [ ] Docker and Docker Compose installed
- [ ] PostgreSQL 16 installed (or running in Docker)
- [ ] MinIO installed (or running in Docker)

**Accounts & Access:**
- [ ] GitHub access to `emillindfors/a2a-rs`
- [ ] GitHub access to `puemos/craftplan`
- [ ] Generated JWT secret
- [ ] Generated database password
- [ ] MinIO credentials

**Configuration:**
- [ ] `.env` file created with all secrets
- [ ] `docker-compose.yml` configured
- [ ] Network connectivity verified
- [ ] Firewall rules allow inter-container communication

### 11.2 Integration Checklist

**A2A-RS Core:**
- [ ] a2a-rs library compiles successfully
- [ ] All tests pass (`cargo test --all-features`)
- [ ] HTTP server example runs (`cargo run --example http_client_server`)
- [ ] WebSocket server example runs (`cargo run --example websocket_client_server`)

**Craftplan Adapter:**
- [ ] `craftplan-adapter` compiles successfully
- [ ] Unit tests pass (`cargo test --package craftplan-adapter`)
- [ ] Integration tests pass (`cargo test --test integration_test`)
- [ ] Can connect to Craftplan API
- [ ] JWT authentication works
- [ ] JSON:API parsing works

**Domain Agents:**
- [ ] `a2a-agents` compiles successfully
- [ ] All agent tests pass
- [ ] `CatalogAgent` works correctly
- [ ] `OrdersAgent` works correctly
- [ ] `InventoryAgent` works correctly
- [ ] `ProductionAgent` works correctly
- [ ] `CrmAgent` works correctly
- [ ] Composite agent routes messages correctly
- [ ] Caching works as expected
- [ ] Error handling is comprehensive

**A2A-MCP Bridge:**
- [ ] `a2a-mcp` compiles successfully
- [ ] MCP example runs (`cargo run --example minimal_example`)
- [ ] Craftplan MCP example runs
- [ ] A2A → MCP conversion works
- [ ] MCP → A2A conversion works
- [ ] State management works

**A2A Client:**
- [ ] `a2a-client` compiles successfully
- [ ] Server starts successfully
- [ ] Can connect to agent
- [ ] Chat interface works
- [ ] Task list works
- [ ] WebSocket updates work (if implemented)

**End-to-End Tests:**
- [ ] Full stack integration test passes
- [ ] Can send message from client → agent → Craftplan
- [ ] Response flows back correctly
- [ ] JWT authentication works end-to-end
- [ ] Error handling works end-to-end
- [ ] Performance targets met

### 11.3 Deployment Checklist

**Docker Deployment:**
- [ ] All services build successfully (`docker compose build`)
- [ ] All services start successfully (`docker compose up -d`)
- [ ] All services are healthy (`docker compose ps`)
- [ ] Logs show no errors (`docker compose logs`)
- [ ] Can access all services from browser
- [ ] Inter-service communication works

**Kubernetes Deployment (if applicable):**
- [ ] Docker images pushed to registry
- [ ] Kubernetes manifests created
- [ ] Secrets configured
- [ ] Services deployed successfully
- [ ] Pods are running and healthy
- [ ] Services are accessible via LoadBalancer
- [ ] Ingress configured (if needed)

**Fly.io Deployment (if applicable):**
- [ ] `fly.toml` configured
- [ ] App created (`fly apps create`)
- [ ] Secrets set (`fly secrets set`)
- [ ] Deployment successful (`fly deploy`)
- [ ] App is running (`fly status`)
- [ ] App is accessible via public URL

### 11.4 Monitoring & Validation Checklist

**Performance Validation:**
- [ ] Agent response time < 500ms (p95)
- [ ] End-to-end latency < 1s (p95)
- [ ] Cache hit rate > 80%
- [ ] WebSocket message delay < 100ms
- [ ] Can handle 100+ concurrent users
- [ ] Throughput > 1000 req/min

**Logging & Observability:**
- [ ] Structured logging enabled
- [ ] Log levels configured correctly
- [ ] Logs are centralized (if using ELK/Loki)
- [ ] Metrics are collected (Prometheus)
- [ ] Distributed tracing works (OpenTelemetry)
- [ ] Dashboards are configured (Grafana)

**Security Validation:**
- [ ] All endpoints require authentication
- [ ] Authorization checks work correctly
- [ ] JWT tokens expire after 1 hour
- [ ] Rate limiting is enforced
- [ ] HTTPS is used in production
- [ ] Secrets are not logged
- [ ] Input validation is comprehensive

**Error Handling Validation:**
- [ ] All error categories are handled
- [ ] Error responses are consistent
- [ ] Retries work for transient errors
- [ ] Circuit breakers trigger appropriately
- [ ] Users see helpful error messages
- [ ] Errors are logged for debugging

---

## Appendix

### A. Configuration Examples

**A2A Agents Configuration:**

```toml
# config/development.toml
[server]
http_port = 8080
ws_port = 8081
host = "0.0.0.0"

[craftplan]
api_url = "http://localhost:4000"
timeout_secs = 30
cache_ttl_secs = 300
max_cache_size = 1000

[auth]
jwt_secret = "your-secret-key"
issuer = "craftplan"
audience = "a2a-agents"

[ai]
enabled = true
provider = "ollama"  # or "openai"
model = "llama2"
base_url = "http://localhost:11434"

[cache]
enabled = true
backend = "moka"  # or "redis"
ttl_secs = 300
max_size = 1000

[logging]
level = "debug"
format = "pretty"  # or "json"
```

**Environment Variables:**

```bash
# .env
# Application
RUST_LOG=info
SERVER_HTTP_PORT=8080
SERVER_WS_PORT=8081

# Craftplan
CRAFTPLAN_API_URL=http://localhost:4000
CRAFTPLAN_TIMEOUT_SECS=30

# Authentication
JWT_SECRET=your-secret-key

# AI (optional)
AI_ENABLED=true
AI_PROVIDER=ollama
AI_MODEL=llama2
AI_BASE_URL=http://localhost:11434

# Cache
CACHE_ENABLED=true
CACHE_BACKEND=moka
CACHE_TTL_SECS=300
CACHE_MAX_SIZE=1000

# Database (optional, for task persistence)
DATABASE_URL=postgresql://user:pass@localhost/a2a
```

### B. Testing Strategy

**Unit Tests (Rust):**

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_catalog_agent_query_product() {
        let mock_client = MockCraftplanClient::new();
        let agent = CatalogAgent::new(Arc::new(mock_client));

        let message = Message::user_text(
            "What is the product BREAD-001?".to_string(),
            "msg-1".to_string()
        );

        let task = agent.handle_message("task-1", &message, None).await.unwrap();

        assert_eq!(task.status.state, TaskState::Completed);
        assert!(task.artifacts[0].parts[0].text.contains("Sourdough"));
    }
}
```

**Integration Tests:**

```rust
#[tokio::test]
#[ignore]  // Run with: cargo test -- --ignored
async fn test_craftplan_integration() {
    let config = CraftplanConfig::builder(
        std::env::var("CRAFTPLAN_API_URL").unwrap()
    ).build().unwrap();

    let client = CraftplanClient::new(config).await.unwrap();

    // Test product query
    let product = client.get_product("product-123").await.unwrap();
    assert_eq!(product.id, "product-123");

    // Test order creation
    let order = client.create_order(CreateOrderParams {
        customer_id: "customer-456".to_string(),
        line_items: vec![],
    }).await.unwrap();
    assert_eq!(order.customer_id, "customer-456");
}
```

### C. Troubleshooting Guide

**Common Issues:**

1. **"Connection refused" to Craftplan**
   ```bash
   # Check Craftplan is running
   curl http://localhost:4000/api/health

   # Check logs
   docker compose logs craftplan

   # Restart Craftplan
   docker compose restart craftplan
   ```

2. **"Invalid JWT" errors**
   ```bash
   # Verify JWT_SECRET matches
   docker compose exec a2a-agents env | grep JWT_SECRET
   docker compose exec craftplan env | grep JWT_SECRET

   # Regenerate secrets
   rm .env
   # Re-run setup
   ```

3. **High memory usage**
   ```bash
   # Check memory usage
   docker stats

   # Reduce cache size
   export CACHE_MAX_SIZE=100

   # Restart agents
   docker compose restart a2a-agents
   ```

### D. References

- [A2A Protocol Specification](https://github.com/modelcontextprotocol/rust-sdk)
- [a2a-rs Documentation](https://docs.rs/a2a-rs)
- [Ash Framework Guide](https://ash-hq.org/)
- [Craftplan Documentation](https://github.com/puemos/craftplan)
- [JSON:API Specification](https://jsonapi.org/)
- [Hexagonal Architecture](https://alistair.cockburn.us/hexagonal-architecture/)

---

**Document Version Control:**

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-04 | System Architecture Designer | Initial comprehensive architecture design |

---

**End of Document**
