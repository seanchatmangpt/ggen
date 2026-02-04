# Craftplan Integration Architecture for A2A-RS Ecosystem

**Version**: 1.0.0
**Date**: 2026-02-03
**Author**: System Architecture Designer
**Status**: Design Specification

## Executive Summary

This document defines the integration architecture for **Craftplan** (an open-source ERP for artisanal manufacturers) into the **A2A-RS ecosystem**. The integration leverages the A2A (Agent-to-Agent) protocol framework to enable intelligent agent interactions with Craftplan's business domains, creating a powerful AI-driven ERP experience.

### Key Integration Points

1. **A2A-RS Core Framework** - Protocol implementation, HTTP/WebSocket transports, authentication
2. **A2A-Agents** - Production agent implementations for Craftplan business domains
3. **A2A-MCP Bridge** - Protocol translation between A2A and Model Context Protocol
4. **A2A-Client** - Web UI for agent interactions
5. **Craftplan** - Elixir-based ERP with Ash Framework domain modeling

## Table of Contents

1. [System Overview](#system-overview)
2. [Architecture Principles](#architecture-principles)
3. [Component Architecture](#component-architecture)
4. [Data Flow](#data-flow)
5. [Integration Patterns](#integration-patterns)
6. [Security & Authentication](#security--authentication)
7. [API Contracts](#api-contracts)
8. [Deployment Architecture](#deployment-architecture)
9. [Error Handling](#error-handling)
10. [Implementation Roadmap](#implementation-roadmap)

---

## System Overview

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          A2A-CLIENT (Web UI)                            │
│                     (Axum + Askama Templates)                           │
│                 http://localhost:3000                                    │
└───────────────────────────────┬─────────────────────────────────────────┘
                                │ HTTP/WebSocket
                                ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                         A2A-AGENTS (Rust)                               │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌────────────┐ │
│  │  Catalog     │  │   Orders     │  │  Inventory   │  │ Production │ │
│  │   Agent      │  │   Agent      │  │   Agent      │  │   Agent    │ │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘  └──────┬─────┘ │
│         │                 │                 │                 │        │
│         └─────────────────┴─────────────────┴─────────────────┘        │
│                            │                                             │
│                 ┌──────────▼──────────┐                                 │
│                 │  DefaultRequest     │                                 │
│                 │  Processor          │                                 │
│                 │  (Hexagonal Ports)  │                                 │
│                 └──────────┬──────────┘                                 │
└────────────────────────────┼────────────────────────────────────────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
        ▼                    ▼                    ▼
┌───────────────┐  ┌──────────────┐  ┌─────────────────────────┐
│   A2A-RS      │  │   A2A-MCP    │  │   CRAFTPLAN (Elixir)    │
│   CORE        │  │   BRIDGE     │  │   Ash Framework +       │
│               │  │              │  │   Phoenix LiveView      │
│ - HTTP/WS     │  │ Protocol     │  │                         │
│ - Auth        │◄─┤ Translation │◄─┤ JSON:API               │
│ - Storage     │  │              │  │ GraphQL                │
│ - JSON-RPC    │  │              │  │ PostgreSQL             │
└───────────────┘  └──────────────┘  └─────────────────────────┘
                                                   │
                                                   ▼
                                      ┌────────────────────┐
                                      │  PostgreSQL 16     │
                                      │  + MinIO (S3)      │
                                      └────────────────────┘
```

### Technology Stack Mapping

| Layer | A2A-RS Ecosystem | Craftplan | Integration Point |
|-------|------------------|-----------|-------------------|
| **Protocol** | A2A Protocol v0.3.0 | JSON:API, GraphQL | JSON:API → A2A message translation |
| **Language** | Rust | Elixir | HTTP/JSON bridge |
| **Transport** | HTTP, WebSocket | Phoenix Channels | WebSocket ↔ Phoenix.Channel |
| **Auth** | JWT, OAuth2, API Keys | AshAuthentication | JWT token validation |
| **Storage** | SQLx (task persistence) | AshPostgres | Shared PostgreSQL database |
| **UI** | Axum + Askama | Phoenix LiveView | Separate UIs, shared backend |

---

## Architecture Principles

### 1. Hexagonal Architecture (Ports & Adapters)

The A2A-RS framework follows hexagonal architecture with clear separation:

```
┌─────────────────────────────────────────────────────────┐
│                   APPLICATION LAYER                     │
│  ┌─────────────────┐    ┌─────────────────────────────┐ │
│  │  JSON-RPC       │    │  HTTP/WebSocket             │ │
│  │  Handlers       │    │  Transport                  │ │
│  └─────────────────┘    └─────────────────────────────┘ │
└─────────────────┬───────────────────────┬───────────────┘
                  │                       │
┌─────────────────▼───────────────────────▼───────────────┐
│                     PORT LAYER                          │
│  ┌──────────────────┐    ┌──────────────────────────┐   │
│  │ MessageHandler   │    │  TaskManager             │   │
│  │ (Trait)          │    │  (Trait)                 │   │
│  └──────────────────┘    └──────────────────────────┘   │
│  ┌──────────────────┐    ┌──────────────────────────┐   │
│  │ NotificationMgr  │    │  StreamingHandler        │   │
│  │ (Trait)          │    │  (Trait)                 │   │
│  └──────────────────┘    └──────────────────────────┘   │
└─────────────────┬───────────────────────┬───────────────┘
                  │                       │
┌─────────────────▼───────────────────────▼───────────────┐
│                    DOMAIN LAYER                         │
│  ┌──────────────┐ ┌──────────────┐ ┌─────────────────┐  │
│  │   Message    │ │     Task     │ │   AgentCard     │  │
│  │   Artifact   │ │ TaskStatus   │ │ Capabilities    │  │
│  └──────────────┘ └──────────────┘ └─────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### 2. Domain-Driven Design (DDD)

Craftplan's business domains map directly to A2A agents:

| Craftplan Domain | Ash Resources | A2A Agent | Capabilities |
|------------------|---------------|-----------|--------------|
| **Catalog** | Product, BOM, BomComponent, LaborStep | `CatalogAgent` | Product queries, BOM rollups, cost calculations |
| **Orders** | Order, OrderItem | `OrdersAgent` | Order creation, status updates, allocations |
| **Inventory** | Material, Lot, Movement | `InventoryAgent` | Stock queries, movements, forecasting |
| **Production** | ProductionBatch, BatchAllocation | `ProductionAgent` | Batch creation, completion, consumption |
| **CRM** | Customer, Supplier | `CrmAgent` | Customer lookups, order history |

### 3. Separation of Concerns

- **Framework Logic** (a2a-rs): Protocol compliance, transport, auth
- **Business Logic** (a2a-agents): Domain-specific message handling
- **Bridge Logic** (a2a-mcp): Protocol translation
- **ERP Logic** (craftplan): Business rules, data persistence

### 4. Async-First Design

All interactions are async using Rust's `async/await` and Elixir's OTP:

```rust
// Rust side - A2A message handler
#[async_trait::async_trait]
impl AsyncMessageHandler for CatalogAgent {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        // Business logic here
    }
}
```

```elixir
# Elixir side - Craftplan domain action
defmodule Craftplan.Catalog do
  use Ash.Domain

  actions do
    define :create_product, args: [:name, :bom_id]
    define :calculate_bom_cost, args: [:bom_id]
  end
end
```

---

## Component Architecture

### A2A-RS Core Framework

**Purpose**: Type-safe A2A protocol implementation with pluggable transports

**Key Modules**:
- `domain/`: Core types (Message, Task, AgentCard, Artifact)
- `port/`: Trait definitions (MessageHandler, TaskManager, etc.)
- `adapter/`: Concrete implementations (HTTP, WebSocket, Auth)
- `application/`: JSON-RPC handlers

**Dependencies**: None (core library)

### A2A-Agents

**Purpose**: Production agent implementations for Craftplan domains

**Structure**:
```
a2a-agents/
├── src/
│   ├── catalog_agent/
│   │   ├── mod.rs
│   │   ├── handler.rs       # AsyncMessageHandler impl
│   │   ├── types.rs         # Domain-specific types
│   │   └── client.rs        # HTTP client for Craftplan API
│   ├── orders_agent/
│   ├── inventory_agent/
│   ├── production_agent/
│   └── lib.rs
└── Cargo.toml
```

**Example: CatalogAgent**

```rust
pub struct CatalogAgent {
    craftplan_client: CraftplanClient,
    cache: Arc<RwLock<LruCache<String, Product>>>,
}

#[async_trait]
impl AsyncMessageHandler for CatalogAgent {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        match message.extract_intent() {
            Intent::QueryProduct { id } => {
                self.query_product(task_id, id).await
            }
            Intent::CalculateBomCost { bom_id } => {
                self.calculate_bom_cost(task_id, bom_id).await
            }
            _ => Err(A2AError::UnknownIntent),
        }
    }
}
```

### A2A-MCP Bridge

**Purpose**: Bidirectional protocol translation between A2A and MCP

**Architecture**:

```
┌────────────────────────────────────────────────────────┐
│                  A2A-MCP BRIDGE                        │
├────────────────────────────────────────────────────────┤
│  ┌──────────────────┐    ┌─────────────────────────┐  │
│  │  A2A → MCP       │    │  MCP → A2A              │  │
│  │  Adapter         │    │  Adapter                │  │
│  └──────────────────┘    └─────────────────────────┘  │
│  ┌──────────────────────────────────────────────────┐ │
│  │         Message Conversion Layer                 │ │
│  │  - Message format translation                     │ │
│  │  - State mapping (tasks ↔ tools)                 │ │
│  │  - Capability negotiation                        │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────┘
```

**Use Cases**:
- Expose A2A agents as MCP tools for LLMs
- Use MCP tools (e.g., file I/O, web search) within A2A agents
- Enable cross-protocol agent composition

### Craftplan Integration Layer

**Purpose**: HTTP/JSON bridge between Rust agents and Elixir ERP

**Implementation Options**:

#### Option 1: Direct HTTP Client (Recommended)

Rust agents call Craftplan's existing JSON:API endpoints:

```rust
pub struct CraftplanClient {
    base_url: String,
    api_key: String,
    client: reqwest::Client,
}

impl CraftplanClient {
    pub async fn get_product(&self, id: &str) -> Result<Product, Error> {
        let response = self.client
            .get(format!("{}/api/products/{}", self.base_url, id))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/vnd.api+json")
            .send()
            .await?;

        Ok(response.json().await?)
    }
}
```

**Pros**: No changes to Craftplan, uses existing API
**Cons**: Limited to API capabilities, potential overhead

#### Option 2: Dedicated A2A Endpoint in Craftplan

Add Phoenix controller for A2A protocol:

```elixir
defmodule CraftplanWeb.A2AController do
  use CraftplanWeb, :controller

  def message(conn, %{"task_id" => task_id, "message" => message}) do
    case process_a2a_message(message) do
      {:ok, response} ->
        json(conn, %{task: task_id, response: response})
      {:error, error} ->
        conn
        |> put_status(:bad_request)
        |> json(%{error: error})
    end
  end

  defp process_a2a_message(message) do
    # Route to appropriate Ash action based on message intent
  end
end
```

**Pros**: Tighter integration, custom protocol support
**Cons**: Requires Craftplan modifications

---

## Data Flow

### 1. Agent Message Processing Flow

```
┌──────────┐
│  Client  │ (User or another agent)
└─────┬────┘
      │ 1. Send message (HTTP POST /message)
      ▼
┌──────────────────────────────────────────────────────────┐
│  A2A-RS HTTP Server                                       │
│  ┌────────────────────────────────────────────────────┐  │
│  │ JSON-RPC Handler                                    │  │
│  │ - Parse request                                     │  │
│  │ - Validate authentication                          │  │
│  │ - Route to message handler                         │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 2. AsyncMessageHandler.handle_message()
                       ▼
┌──────────────────────────────────────────────────────────┐
│  CatalogAgent (A2A-Agents)                               │
│  ┌────────────────────────────────────────────────────┐  │
│  │ 1. Extract intent from message                     │  │
│  │ 2. Check cache                                     │  │
│  │ 3. Call Craftplan API if cache miss               │  │
│  │ 4. Format response as A2A Task                     │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 3. HTTP GET /api/products/{id}
                       ▼
┌──────────────────────────────────────────────────────────┐
│  Craftplan (Phoenix + Ash)                               │
│  ┌────────────────────────────────────────────────────┐  │
│  │ - JSON:API request handler                         │  │
│  │ - Ash resource action (read :product)              │  │
│  │ - PostgreSQL query                                 │  │
│  │ - Return JSON:API response                         │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 4. JSON:API document
                       ▼
┌──────────────────────────────────────────────────────────┐
│  CatalogAgent Response Processing                        │
│  ┌────────────────────────────────────────────────────┐  │
│  │ - Deserialize JSON:API to Product struct           │  │
│  │ - Update cache                                     │  │
│  │ - Create A2A Task with result artifact             │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │ 5. Task object
                       ▼
┌──────────────────────────────────────────────────────────┐
│  A2A-RS HTTP Server Response                             │
│  ┌────────────────────────────────────────────────────┐  │
│  │ - Serialize task to JSON-RPC response              │  │
│  │ - Send HTTP 200 OK                                 │  │
│  └────────────────────────────────────────────────────┘  │
└──────────────────────┬───────────────────────────────────┘
                       │
                       ▼
                  ┌─────────┐
                  │  Client │
                  └─────────┘
```

### 2. WebSocket Streaming Flow

```
┌──────────┐
│  Client  │
└─────┬────┘
      │ 1. WebSocket upgrade request
      ▼
┌─────────────────────────────────────────────────────────┐
│  A2A-RS WebSocket Server                                │
│  ┌───────────────────────────────────────────────────┐  │
│  │ - Accept WebSocket connection                     │  │
│  │ - Create task subscription                        │  │
│  │ - Register streaming handler                      │  │
│  └───────────────────────────────────────────────────┘  │
└───────────────────────────────┬─────────────────────────┘
                                │ 2. Subscribe to task updates
                                ▼
┌─────────────────────────────────────────────────────────┐
│  ProductionAgent (Streaming Handler)                    │
│  ┌───────────────────────────────────────────────────┐  │
│  │ - Monitor production batch status                 │  │
│  │ - Poll Craftplan for updates                      │  │
│  │ - Convert to TaskStatusUpdateEvent                │  │
│  │ - Push to WebSocket stream                        │  │
│  └───────────────────────────────────────────────────┘  │
└───────────────────────────────┬─────────────────────────┘
                                │ 3. Periodic GET /api/batches/{id}
                                ▼
┌─────────────────────────────────────────────────────────┐
│  Craftplan Production Domain                            │
│  ┌───────────────────────────────────────────────────┐  │
│  │ - Ash resource: ProductionBatch                   │  │
│  │ - Actions: update_status, complete_batch          │  │
│  │ - PostgreSQL state change                         │  │
│  └───────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### 3. MCP Bridge Flow (A2A → MCP)

```
┌──────────────┐
│  LLM Client  │ (using MCP protocol)
└──────┬───────┘
       │ 1. Call MCP tool "craftplan.get_product"
       ▼
┌────────────────────────────────────────────────────────┐
│  A2A-MCP Bridge                                        │
│  ┌──────────────────────────────────────────────────┐ │
│  │ RMCP Server (exposing A2A agents as tools)       │ │
│  │ 1. Receive MCP tool call request                 │ │
│  │ 2. Translate to A2A Message                      │ │
│  │ 3. Call A2A client                               │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────┬───────────────────────────────┘
                         │ 2. Send A2A message
                         ▼
┌────────────────────────────────────────────────────────┐
│  A2A-RS HTTP Client                                    │
│  ┌──────────────────────────────────────────────────┐ │
│  │ - POST /message with A2A protocol                │ │
│  │ - Receive Task response                          │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────┬───────────────────────────────┘
                         │ 3. Return Task
                         ▼
┌────────────────────────────────────────────────────────┐
│  A2A-MCP Bridge (Response)                             │
│  ┌──────────────────────────────────────────────────┐ │
│  │ - Convert Task to MCP tool result                │ │
│  │ - Return to LLM client                           │ │
│  └──────────────────────────────────────────────────┘ │
└────────────────────────┬───────────────────────────────┘
                         │
                         ▼
                  ┌──────────────┐
                  │  LLM Client  │
                  └──────────────┘
```

---

## Integration Patterns

### Pattern 1: Agent per Domain

Each Craftplan Ash domain gets a dedicated A2A agent:

```rust
// Agent registry
pub fn create_craftplan_agents(config: &CraftplanConfig) -> Vec<Box<dyn AsyncMessageHandler>> {
    vec![
        Box::new(CatalogAgent::new(config.clone())),
        Box::new(OrdersAgent::new(config.clone())),
        Box::new(InventoryAgent::new(config.clone())),
        Box::new(ProductionAgent::new(config.clone())),
        Box::new(CrmAgent::new(config.clone())),
    ]
}
```

**Benefits**:
- Clear separation of concerns
- Independent scaling per domain
- Easy to add new capabilities

### Pattern 2: Intent-Based Message Routing

Agents parse natural language messages to extract intent:

```rust
pub enum CatalogIntent {
    QueryProduct { id: String },
    ListProducts { filters: Vec<Filter> },
    CalculateBomCost { bom_id: String },
    CreateProduct { name: String, bom: BomData },
}

impl CatalogAgent {
    fn extract_intent(&self, message: &Message) -> Result<CatalogIntent, Error> {
        // Use LLM or pattern matching to extract intent
        // Fallback to structured query format
    }
}
```

### Pattern 3: Caching Layer

Reduce load on Craftplan with intelligent caching:

```rust
pub struct CachedCraftplanClient {
    inner: CraftplanClient,
    cache: Arc<RwLock<LruCache<String, CacheEntry>>>,
    ttl: Duration,
}

impl CachedCraftplanClient {
    pub async fn get_product(&self, id: &str) -> Result<Product, Error> {
        // Check cache
        if let Some(entry) = self.cache.read().await.get(id) {
            if entry.age() < self.ttl {
                return Ok(entry.data.clone());
            }
        }

        // Cache miss - fetch from API
        let product = self.inner.get_product(id).await?;
        self.cache.write().await.put(id.to_string(), CacheEntry::new(product.clone()));
        Ok(product)
    }
}
```

### Pattern 4: Event-Driven Updates

Use Phoenix PubSub to push updates to agents:

```elixir
# Craftplan side
defmodule Craftplan.ProductionNotifier do
  @topic "production_batches"

  def broadcast_update(batch_id, changes) do
    Phoenix.PubSub.broadcast(
      Craftplan.PubSub,
      @topic,
      {:batch_update, batch_id, changes}
    )
  end
end
```

```rust
// Agent side - subscribe to updates
pub async fn subscribe_to_batch_updates(&self) -> impl Stream<Item = BatchUpdate> {
    // Connect to Phoenix channel
    // Subscribe to "production_batches" topic
    // Convert messages to BatchUpdate
}
```

### Pattern 5: Composite Agents

Combine multiple domain agents for complex workflows:

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
        // Multi-step workflow:
        // 1. Check inventory
        // 2. Allocate materials
        // 3. Schedule production batch
        // 4. Update order status
    }
}
```

---

## Security & Authentication

### Authentication Architecture

```
┌─────────────────────────────────────────────────────────┐
│  Authentication Flow                                    │
├─────────────────────────────────────────────────────────┤
│                                                          │
│  1. Client → A2A-RS Server                              │
│     "Bearer eyJhbGciOiJIUzI1NiIs..."                    │
│     │                                                    │
│     ▼                                                    │
│  2. JwtAuthenticator::validate(token)                   │
│     │                                                    │
│     ├─→ Verify signature with Craftplan public key      │
│     ├─→ Extract user claims (user_id, roles)            │
│     └─→ Return AuthContext                              │
│     │                                                    │
│     ▼                                                    │
│  3. A2A-RS → Craftplan API                              │
│     "Authorization: Bearer eyJhbGciOiJIUzI1NiIs..."      │
│     │                                                    │
│     ▼                                                    │
│  4. Craftplan AshAuthentication                          │
│     - Validate token                                     │
│     - Load user from database                           │
│     - Check authorization policies                      │
│     - Return resource or 403                            │
│                                                          │
└─────────────────────────────────────────────────────────┘
```

### Token Management

**JWT Token Structure**:

```json
{
  "sub": "user_123",
  "name": "John Doe",
  "email": "john@example.com",
  "roles": ["admin"],
  "iss": "craftplan",
  "exp": 1735725680,
  "https://craftplan.app/tenant_id": "tenant_456"
}
```

**Shared Secret**: Both systems use the same `JWT_SECRET` from environment.

### Authorization Patterns

**Role-Based Access Control (RBAC)**:

| Role | Catalog | Orders | Inventory | Production | Settings |
|------|---------|--------|-----------|------------|----------|
| Admin | ✓ | ✓ | ✓ | ✓ | ✓ |
| Staff | ✓ | ✓ | ✓ | ✓ | ✗ |
| ReadOnly | ✓ | ✓ | ✓ | ✗ | ✗ |

**Example Authorization Check**:

```rust
pub fn authorize(agent: &Agent, action: &str, resource: &str) -> bool {
    match action {
        "create" => agent.roles.contains(&Role::Admin),
        "read" => agent.roles.contains(&Role::Staff) || agent.roles.contains(&Role::ReadOnly),
        "update" => agent.roles.contains(&Role::Admin) || agent.roles.contains(&Role::Staff),
        "delete" => agent.roles.contains(&Role::Admin),
        _ => false,
    }
}
```

---

## API Contracts

### 1. A2A → Craftplan (JSON:API)

**Request Format**:

```http
GET /api/products/123 HTTP/1.1
Host: craftplan:4000
Authorization: Bearer <JWT_TOKEN>
Content-Type: application/vnd.api+json
Accept: application/vnd.api+json
```

**Response Format**:

```json
{
  "data": {
    "id": "123",
    "type": "product",
    "attributes": {
      "name": "Artisan Sourdough Bread",
      "sku": "BREAD-001",
      "cost": 2.50,
      "price": 6.00
    },
    "relationships": {
      "bom": {
        "links": {
          "self": "/api/products/123/relationships/bom",
          "related": "/api/products/123/bom"
        }
      }
    }
  },
  "links": {
    "self": "/api/products/123"
  }
}
```

### 2. Craftplan → A2A (Direct A2A Protocol)

**Request Format**:

```http
POST /message HTTP/1.1
Host: a2a-agents:8080
Authorization: Bearer <JWT_TOKEN>
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "method": "message/send",
  "params": {
    "taskId": "task-456",
    "message": {
      "role": "user",
      "parts": [
        {
          "text": "What is the cost of product BREAD-001?"
        }
      ]
    },
    "sessionId": "session-789"
  },
  "id": 1
}
```

**Response Format**:

```json
{
  "jsonrpc": "2.0",
  "result": {
    "task": {
      "id": "task-456",
      "status": {
        "state": "completed",
        "timestamp": "2026-02-03T12:00:00Z"
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
      ]
    }
  },
  "id": 1
}
```

### 3. Message Schema Mapping

| A2A Message | Craftplan Resource | Example |
|-------------|-------------------|---------|
| `QueryProduct { id }` | `Craftplan.Catalog.Product` | GET /api/products/{id} |
| `ListProducts { filters }` | `Craftplan.Catalog.Product` | GET /api/products?filter[name]=bread |
| `CreateOrder { items }` | `Craftplan.Orders.Order` | POST /api/orders |
| `CheckStock { material_id }` | `Craftplan.Inventory.Material` | GET /api/materials/{id}/lots |
| `ScheduleBatch { orders }` | `Craftplan.Production.ProductionBatch` | POST /api/production_batches |

---

## Deployment Architecture

### Development Environment

```yaml
# docker-compose.dev.yml
services:
  a2a-agents:
    build: ../a2a-rs/a2a-agents
    ports:
      - "8080:8080"  # HTTP
      - "8081:8081"  # WebSocket
    environment:
      CRAFTPLAN_API_URL: http://craftplan:4000
      JWT_SECRET: ${JWT_SECRET}
      RUST_LOG: debug
    depends_on:
      - craftplan

  a2a-client:
    build: ../a2a-rs/a2a-client
    ports:
      - "3000:3000"
    environment:
      AGENT_URL: http://a2a-agents:8080
      RUST_LOG: info

  craftplan:
    image: ghcr.io/puemos/craftplan:latest
    ports:
      - "4000:4000"
    environment:
      DATABASE_URL: ecto://postgres:postgres@postgres/craftplan
      JWT_SECRET: ${JWT_SECRET}
    depends_on:
      - postgres
      - minio

  postgres:
    image: postgres:16
    environment:
      POSTGRES_DB: craftplan
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: postgres
    volumes:
      - postgres_data:/var/lib/postgresql/data

  minio:
    image: minio/minio:latest
    command: server /data --console-address ":9001"
    ports:
      - "9000:9000"
      - "9001:9001"
    volumes:
      - minio_data:/data
```

### Production Environment

**Option 1: Docker Swarm / Kubernetes**

```yaml
# kubernetes/deployment.yaml
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
        - containerPort: 8081
        env:
        - name: CRAFTPLAN_API_URL
          value: "http://craftplan-service:4000"
        - name: JWT_SECRET
          valueFrom:
            secretKeyRef:
              name: craftplan-secrets
              key: jwt-secret
        resources:
          requests:
            memory: "256Mi"
            cpu: "250m"
          limits:
            memory: "512Mi"
            cpu: "500m"
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
```

**Option 2: Fly.io / Railway**

```toml
# fly.toml for a2a-agents
app = "a2a-agents"

[build]
  dockerfile = "../a2a-rs/a2a-agents/Dockerfile"

[env]
  CRAFTPLAN_API_URL = "https://craftplan.fly.dev"
  RUST_LOG = "info"

[[services]]
  http_checks = []
  internal_port = 8080
  protocol = "tcp"

  [[services.ports]]
    port = 80
    handlers = ["http"]

[[services]]
  internal_port = 8081
  protocol = "tcp"

  [[services.ports]]
    port = 8081
    handlers = ["http"]
```

### Network Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      Internet                           │
└───────────────────────┬─────────────────────────────────┘
                        │
        ┌───────────────┼───────────────┐
        │               │               │
        ▼               ▼               ▼
┌──────────────┐ ┌──────────────┐ ┌──────────────┐
│  Fly.io App  │ │  Fly.io App  │ │  Fly.io App  │
│  a2a-client  │ │  a2a-agents  │ │  craftplan   │
│  :3000       │ │  :8080/:8081 │ │  :4000       │
└──────┬───────┘ └──────┬───────┘ └──────┬───────┘
       │                │                │
       └────────────────┴────────────────┘
                        │
               ┌────────▼────────┐
               │  Fly.io Private │
               │   Network (6PN) │
               └─────────────────┘
```

---

## Error Handling

### Error Categories

| Category | Example | HTTP Status | A2A Error Code |
|----------|---------|-------------|----------------|
| **Authentication** | Invalid JWT | 401 | `AUTHENTICATION_FAILED` |
| **Authorization** | Insufficient permissions | 403 | `AUTHORIZATION_FAILED` |
| **Not Found** | Product doesn't exist | 404 | `RESOURCE_NOT_FOUND` |
| **Validation** | Invalid input data | 422 | `VALIDATION_ERROR` |
| **Rate Limit** | Too many requests | 429 | `RATE_LIMIT_EXCEEDED` |
| **Server Error** | Database connection failed | 500 | `INTERNAL_ERROR` |
| **Service Unavailable** | Craftplan API down | 503 | `SERVICE_UNAVAILABLE` |

### Error Response Format

**A2A Protocol Error**:

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

**Craftplan API Error**:

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

### Error Handling Strategy

```rust
// Agent-side error handling
impl From<CraftplanError> for A2AError {
    fn from(err: CraftplanError) -> Self {
        match err {
            CraftplanError::NotFound(resource) => {
                A2AError::resource_not_found(resource)
            }
            CraftplanError::Unauthorized => {
                A2AError::authentication_failed("Invalid API credentials")
            }
            CraftplanError::Validation(msg) => {
                A2AError::validation_error(msg)
            }
            CraftplanError::Internal(msg) => {
                A2AError::internal_error(format!("Craftplan API error: {}", msg))
            }
        }
    }
}
```

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2)

**Goal**: Basic HTTP communication between A2A agents and Craftplan

- [ ] Set up a2a-agents project structure
- [ ] Implement `CraftplanClient` with HTTP requests to JSON:API
- [ ] Create `CatalogAgent` with basic product queries
- [ ] Add JWT authentication using shared secret
- [ ] Deploy to local dev environment with docker-compose
- [ ] Write integration tests for agent → API communication

**Deliverables**:
- Working `CatalogAgent` that can query products from Craftplan
- Authentication with JWT tokens
- Integration test suite

### Phase 2: Multi-Domain Agents (Weeks 3-4)

**Goal**: Implement agents for all major Craftplan domains

- [ ] `OrdersAgent` - create orders, query status
- [ ] `InventoryAgent` - check stock, list lots
- [ ] `ProductionAgent` - create batches, monitor status
- [ ] `CrmAgent` - customer lookups, order history
- [ ] Add caching layer to reduce API calls
- [ ] Implement intent extraction from natural language

**Deliverables**:
- 5 domain agents with full CRUD capabilities
- Intent-based message routing
- LRU cache for frequently accessed data

### Phase 3: WebSocket Streaming (Weeks 5-6)

**Goal**: Real-time updates for long-running operations

- [ ] Implement WebSocket server in a2a-agents
- [ ] Add Phoenix.Channel subscriptions in agents
- [ ] Stream production batch status updates
- [ ] Stream inventory level changes
- [ ] Handle connection reconnection
- [ ] Add heartbeat mechanism

**Deliverables**:
- WebSocket endpoint at `ws://localhost:8081`
- Real-time updates for production batches
- Client subscription management

### Phase 4: MCP Bridge (Weeks 7-8)

**Goal**: Enable A2A agents to be used as MCP tools

- [ ] Implement RMCP server in a2a-mcp
- [ ] Create A2A → MCP message converter
- [ ] Create MCP → A2A message converter
- [ ] Expose agents as MCP tools
- [ ] Implement state management across protocols
- [ ] Add protocol capability negotiation

**Deliverables**:
- Working a2a-mcp bridge
- Agents callable from MCP clients
- Documentation for MCP tool usage

### Phase 5: Advanced Features (Weeks 9-10)

**Goal**: Production-ready features

- [ ] Rate limiting per user/agent
- [ ] Request/response logging
- [ ] Metrics collection (Prometheus)
- [ ] Distributed tracing (OpenTelemetry)
- [ ] Health check endpoints
- [ ] Graceful shutdown

**Deliverables**:
- Production monitoring stack
- Rate limiting middleware
- Health check endpoints

### Phase 6: Client UI (Weeks 11-12)

**Goal**: Enhanced web client for agent interactions

- [ ] Extend a2a-client with Craftplan-specific UI
- [ ] Add dynamic form generation for agent inputs
- [ ] Display agent responses with rich formatting
- [ ] Add task history and artifact browser
- [ ] Implement WebSocket for real-time updates

**Deliverables**:
- Enhanced web client at `http://localhost:3000`
- Agent interaction UI
- Real-time updates in browser

---

## Appendix

### A. Configuration Examples

**a2a-agents Config**:

```toml
# config/development.toml
[server]
http_port = 8080
ws_port = 8081
host = "0.0.0.0"

[craftplan]
api_url = "http://localhost:4000"
api_key = "your-api-key"
timeout_secs = 30

[auth]
jwt_secret = "your-secret-key"
issuer = "craftplan"
audience = "a2a-agents"

[cache]
max_size = 1000
ttl_secs = 300

[logging]
level = "debug"
```

### B. Testing Strategy

**Unit Tests** (Rust):

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_catalog_agent_query_product() {
        let mock_client = MockCraftplanClient::new();
        let agent = CatalogAgent::new(mock_client);

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

**Integration Tests** (Elixir):

```elixir
defmodule CraftplanWeb.A2AAgentIntegrationTest do
  use CraftplanWeb.ConnCase

  test "agent can query product via JSON:API" do
    product = insert(:product, name: "Test Product")

    conn =
      build_conn()
      |> put_req_header("authorization", "Bearer #{valid_jwt()}")
      |> get("/api/products/#{product.id}")

    assert json_response(conn, 200)["data"]["attributes"]["name"] == "Test Product"
  end
end
```

### C. Performance Considerations

**Caching Strategy**:
- Product data: Cache for 5 minutes (rarely changes)
- Inventory levels: Cache for 30 seconds (changes frequently)
- Order status: No cache (must be fresh)

**Connection Pooling**:
```rust
let client = reqwest::Client::builder()
    .pool_max_idle_per_host(10)
    .pool_idle_timeout(Duration::from_secs(30))
    .build()?;
```

**Async Concurrency**:
```rust
// Fetch multiple products in parallel
let products: Vec<_> = futures::stream::iter(product_ids)
    .map(|id| client.get_product(id))
    .buffer_unordered(10) // Max 10 concurrent requests
    .collect()
    .await;
```

### D. Troubleshooting Guide

**Common Issues**:

1. **"Connection refused" to Craftplan API**
   - Verify Craftplan is running: `curl http://localhost:4000/health`
   - Check `CRAFTPLAN_API_URL` environment variable
   - Ensure Docker containers are on same network

2. **"Invalid JWT" errors**
   - Verify `JWT_SECRET` matches between systems
   - Check token expiration: `echo <JWT> | jwt decode`
   - Ensure `iss` claim matches expected issuer

3. **WebSocket connection drops**
   - Check nginx/load balancer timeout settings
   - Implement ping/pong heartbeat
   - Add exponential backoff on reconnect

4. **High memory usage**
   - Reduce cache size in config
   - Implement cache eviction policy
   - Monitor with `docker stats`

---

## References

- [A2A Protocol Specification](https://github.com/modelcontextprotocol/rust-sdk)
- [a2a-rs Documentation](https://docs.rs/a2a-rs)
- [Ash Framework Guide](https://ash-hq.org/)
- [Craftplan Documentation](https://github.com/puemos/craftplan)
- [Hexagonal Architecture](https://alistair.cockburn.us/hexagonal-architecture/)
- [JSON:API Specification](https://jsonapi.org/)

---

**Document Version Control**:

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-03 | System Architect | Initial architecture design |

---

**End of Document**
