# Craftplan Integration Guide - Step-by-Step Setup

**Version**: 1.0.0
**Date**: 2026-02-03
**Target Audience**: Developers integrating Craftplan with A2A-RS ecosystem

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Quick Start (5 minutes)](#quick-start-5-minutes)
3. [Development Setup](#development-setup)
4. [Agent Implementation Guide](#agent-implementation-guide)
5. [Testing](#testing)
6. [Deployment](#deployment)
7. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Required Software

- **Docker** 20.10+ and **Docker Compose** 2.0+
- **Rust** 1.70+ (for agent development)
- **Elixir** 1.15+ and **Erlang** 27+ (for Craftplan customization)
- **Git** for cloning repositories

### Required Accounts & Keys

- **GitHub** access to repositories:
  - `emillindfors/a2a-rs`
  - `puemos/craftplan`
- **JWT Secret** (generate with: `openssl rand -base64 32`)

### System Requirements

- **CPU**: 2+ cores recommended
- **Memory**: 4GB+ RAM
- **Disk**: 10GB+ free space

---

## Quick Start (5 minutes)

This guide gets you running with all services locally in under 5 minutes.

### Step 1: Create Project Directory

```bash
mkdir -p ~/craftplan-a2a
cd ~/craftplan-a2a
```

### Step 2: Clone Repositories

```bash
# Clone a2a-rs ecosystem
git clone https://github.com/emillindfors/a2a-rs.git

# Clone craftplan
git clone https://github.com/puemos/craftplan.git
```

### Step 3: Create Docker Compose File

Create `docker-compose.yml`:

```yaml
services:
  # Craftplan ERP
  craftplan:
    image: ghcr.io/puemos/craftplan:latest
    ports:
      - "4000:4000"
    env_file:
      - .env
    environment:
      DATABASE_URL: "ecto://postgres:${POSTGRES_PASSWORD}@postgres/craftplan"
      AWS_S3_SCHEME: "http://"
      AWS_S3_HOST: minio
      AWS_ACCESS_KEY_ID: "${MINIO_ROOT_USER:-minioadmin}"
      AWS_SECRET_ACCESS_KEY: "${MINIO_ROOT_PASSWORD:-minioadmin}"
      AWS_S3_BUCKET: "${AWS_S3_BUCKET:-craftplan}"
      AWS_REGION: "us-east-1"
      SECRET_KEY_BASE: "${SECRET_KEY_BASE}"
      JWT_SECRET: "${JWT_SECRET}"
    depends_on:
      postgres:
        condition: service_healthy
      minio:
        condition: service_started
    restart: unless-stopped

  # PostgreSQL
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
      MINIO_ROOT_USER: "${MINIO_ROOT_USER:-minioadmin}"
      MINIO_ROOT_PASSWORD: "${MINIO_ROOT_PASSWORD:-minioadmin}"
    ports:
      - "9000:9000"
      - "9001:9001"
    volumes:
      - minio_data:/data
    restart: unless-stopped

  # A2A Agents (build from source)
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

volumes:
  postgres_data:
  minio_data:
```

### Step 4: Generate Secrets

Create `.env` file:

```bash
# Generate secrets
JWT_SECRET=$(openssl rand -base64 32)
SECRET_KEY_BASE=$(openssl rand -base64 48)
POSTGRES_PASSWORD=$(openssl rand -base64 16)

# Create .env file
cat > .env << EOF
# Authentication
JWT_SECRET=${JWT_SECRET}
SECRET_KEY_BASE=${SECRET_KEY_BASE}

# Database
POSTGRES_PASSWORD=${POSTGRES_PASSWORD}

# MinIO
MINIO_ROOT_USER=minioadmin
MINIO_ROOT_PASSWORD=$(openssl rand -base64 16)
AWS_S3_BUCKET=craftplan

# Application
PORT=4000
EOF

echo "Secrets generated successfully!"
```

### Step 5: Build and Start Services

```bash
# Start infrastructure (PostgreSQL, MinIO)
docker compose up -d postgres minio

# Wait for PostgreSQL to be ready
echo "Waiting for PostgreSQL..."
sleep 10

# Start Craftplan
docker compose up -d craftplan

# Wait for Craftplan to initialize
echo "Waiting for Craftplan to start..."
sleep 30

# Start A2A agents and client
docker compose up -d a2a-agents a2a-client

# Check all services are running
docker compose ps
```

### Step 6: Verify Setup

```bash
# Test Craftplan API
curl -s http://localhost:4000/api/health | jq .

# Test A2A agent (agent card)
curl -s http://localhost:8080/agent-card | jq .

# Test A2A web client
open http://localhost:3000
```

### Step 7: Access the Services

- **Craftplan ERP**: http://localhost:4000
  - Email: `test@test.com`
  - Password: `Aa123123123123`

- **A2A Agents API**: http://localhost:8080
  - Agent card: http://localhost:8080/agent-card
  - Health check: http://localhost:8080/health

- **A2A Web Client**: http://localhost:3000

---

## Development Setup

For active development of A2A agents or Craftplan customization.

### Option A: Develop A2A Agents (Rust)

#### 1. Install Rust Toolchain

```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# Verify installation
rustc --version
cargo --version
```

#### 2. Setup a2a-agents Project

```bash
cd a2a-rs/a2a-agents

# Install dependencies
cargo fetch

# Run tests
cargo test

# Run in development mode
cargo run --bin reimbursement_demo
```

#### 3. Local Development with Craftplan

```bash
# Start only Craftplan and dependencies
docker compose up -d postgres minio craftplan

# Run agents locally (not in Docker)
cd a2a-rs/a2a-agents
export CRAFTPLAN_API_URL="http://localhost:4000"
export JWT_SECRET="your-secret-from-.env"
cargo run --bin reimbursement_server
```

#### 4. Hot Reload Development

```bash
# Install cargo-watch for auto-reload
cargo install cargo-watch

# Run with auto-reload on file changes
cargo watch -x run --bin reimbursement_server
```

### Option B: Customize Craftplan (Elixir)

#### 1. Install Elixir/Erlang

**macOS**:
```bash
brew install elixir erlang
```

**Ubuntu/Debian**:
```bash
wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
sudo apt-get install esl-erlang elixir
```

#### 2. Setup Craftplan for Development

```bash
cd craftplan

# Install dependencies
mix deps.get

# Setup database (runs migrations)
mix ash.setup

# Install and build assets
npm install --prefix assets
mix assets.deploy

# Seed database with sample data
mix seed

# Start Phoenix server
mix phx.server
```

#### 3. Access Development Tools

- **Phoenix LiveDashboard**: http://localhost:4000/dashboard
  - View database queries
  - Monitor process memory
  - Check ETS tables

- **IEx Console** (attach to running server):
  ```bash
  # In another terminal
  iex --name cookie@127.0.0.1 --cookie secret --remsh craftplan@127.0.0.1
  ```

### Option C: Full Stack Development

Develop both Rust agents and Elixir ERP simultaneously.

#### 1. Terminal 1 - Craftplan (Elixir)

```bash
cd craftplan
docker compose up -d postgres minio
mix phx.server
# Runs on http://localhost:4000
```

#### 2. Terminal 2 - A2A Agents (Rust)

```bash
cd a2a-rs/a2a-agents
export CRAFTPLAN_API_URL="http://localhost:4000"
export JWT_SECRET="your-secret"
cargo run --bin reimbursement_server
# Runs on http://localhost:8080 (HTTP) and :8081 (WebSocket)
```

#### 3. Terminal 3 - A2A Client (Rust)

```bash
cd a2a-rs/a2a-client
export AGENT_URL="http://localhost:8080"
cargo run --bin server
# Runs on http://localhost:3000
```

#### 4. Browser

Open http://localhost:3000 to interact with agents.

---

## Agent Implementation Guide

Learn how to implement custom A2A agents for Craftplan domains.

### Agent Template

Create a new agent in `a2a-agents/src/my_agent/`:

```rust
// my_agent/mod.rs
pub mod handler;
pub mod client;
pub mod types;

pub use handler::MyAgent;
pub use client::MyAgentClient;
pub use types::*;
```

```rust
// my_agent/types.rs
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MyResource {
    pub id: String,
    pub name: String,
    pub value: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreateResourceParams {
    pub name: String,
    pub value: f64,
}
```

```rust
// my_agent/client.rs
use reqwest::Client;
use crate::types::{MyResource, CreateResourceParams};

pub struct MyAgentClient {
    base_url: String,
    api_key: String,
    client: Client,
}

impl MyAgentClient {
    pub fn new(base_url: String, api_key: String) -> Self {
        Self {
            base_url,
            api_key,
            client: Client::new(),
        }
    }

    pub async fn get_resource(&self, id: &str) -> Result<MyResource, Error> {
        let response = self.client
            .get(format!("{}/api/my-resources/{}", self.base_url, id))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/vnd.api+json")
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(Error::ApiError(response.text().await?));
        }

        // Parse JSON:API response
        let json: serde_json::Value = response.json().await?;
        Ok(parse_resource(json)?)
    }

    pub async fn create_resource(&self, params: CreateResourceParams) -> Result<MyResource, Error> {
        let body = serde_json::json!({
            "data": {
                "type": "my-resource",
                "attributes": {
                    "name": params.name,
                    "value": params.value,
                }
            }
        });

        let response = self.client
            .post(format!("{}/api/my-resources", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/vnd.api+json")
            .json(&body)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(Error::ApiError(response.text().await?));
        }

        let json: serde_json::Value = response.json().await?;
        Ok(parse_resource(json)?)
    }
}

fn parse_resource(json: serde_json::Value) -> Result<MyResource, Error> {
    // Parse JSON:API format
    let data = &json["data"];
    Ok(MyResource {
        id: data["id"].as_str().ok_or(Error::ParseError)?.to_string(),
        name: data["attributes"]["name"].as_str().ok_or(Error::ParseError)?.to_string(),
        value: data["attributes"]["value"].as_f64().ok_or(Error::ParseError)?,
    })
}
```

```rust
// my_agent/handler.rs
use a2a_rs::{
    AsyncMessageHandler, Message, Task, TaskStatus, TaskState, A2AError,
    Artifact, Part, Role,
};
use crate::client::MyAgentClient;
use crate::types::*;

pub struct MyAgent {
    client: MyAgentClient,
}

impl MyAgent {
    pub fn new(client: MyAgentClient) -> Self {
        Self { client }
    }

    async fn handle_query(&self, task_id: &str, resource_id: String) -> Result<Task, A2AError> {
        // Fetch from Craftplan
        let resource = self.client.get_resource(&resource_id).await
            .map_err(|e| A2AError::internal_error(format!("Failed to fetch resource: {}", e)))?;

        // Format response
        let response_text = format!(
            "Resource {} has name '{}' and value {}",
            resource.id, resource.name, resource.value
        );

        // Create task with artifact
        let task = Task::new(task_id.to_string())
            .with_status(TaskStatus {
                state: TaskState::Completed,
                timestamp: Some(chrono::Utc::now()),
                message: Some("Resource retrieved successfully".to_string()),
                ..Default::default()
            })
            .with_artifact(Artifact {
                name: "resource".to_string(),
                parts: vec![Part::Text(response_text)],
            });

        Ok(task)
    }

    async fn handle_create(&self, task_id: &str, params: CreateResourceParams) -> Result<Task, A2AError> {
        // Create in Craftplan
        let resource = self.client.create_resource(params).await
            .map_err(|e| A2AError::internal_error(format!("Failed to create resource: {}", e)))?;

        // Format response
        let response_text = format!(
            "Successfully created resource '{}' with ID {}",
            resource.name, resource.id
        );

        let task = Task::new(task_id.to_string())
            .with_status(TaskStatus {
                state: TaskState::Completed,
                timestamp: Some(chrono::Utc::now()),
                message: Some("Resource created successfully".to_string()),
                ..Default::default()
            })
            .with_artifact(Artifact {
                name: "created_resource".to_string(),
                parts: vec![Part::Text(response_text)],
            });

        Ok(task)
    }
}

#[async_trait::async_trait]
impl AsyncMessageHandler for MyAgent {
    async fn handle_message(
        &self,
        task_id: &str,
        message: &Message,
        _session_id: Option<&str>,
    ) -> Result<Task, A2AError> {
        // Extract intent from message
        let text = message.parts.iter()
            .filter_map(|p| match p {
                Part::Text(t) => Some(t),
                _ => None,
            })
            .next()
            .ok_or_else(|| A2AError::validation_error("No text in message"))?;

        // Simple pattern matching (use LLM in production)
        if text.to_lowercase().contains("get") || text.to_lowercase().contains("query") {
            // Extract ID from message
            let id = extract_id(text)
                .ok_or_else(|| A2AError::validation_error("Could not extract resource ID"))?;

            self.handle_query(task_id, id).await
        } else if text.to_lowercase().contains("create") {
            // Parse parameters (use structured data in production)
            let params = parse_create_params(text)
                .ok_or_else(|| A2AError::validation_error("Could not parse create parameters"))?;

            self.handle_create(task_id, params).await
        } else {
            Err(A2AError::validation_error("Unknown intent. Use 'get <id>' or 'create <name> <value>'"))
        }
    }
}

fn extract_id(text: &str) -> Option<String> {
    // Simple regex to extract ID
    use regex::Regex;
    let re = Regex::new(r"(?i)get\s+(\w+)").ok()?;
    let captures = re.captures(text)?;
    Some(captures.get(1)?.as_str().to_string())
}

fn parse_create_params(text: &str) -> Option<CreateResourceParams> {
    // Simple parser (use structured JSON in production)
    use regex::Regex;
    let re = Regex::new(r"(?i)create\s+(\w+)\s+(\d+\.?\d*)").ok()?;
    let captures = re.captures(text)?;
    Some(CreateResourceParams {
        name: captures.get(1)?.as_str().to_string(),
        value: captures.get(2)?.as_str().parse().ok()?,
    })
}
```

### Register Agent

Update `a2a-agents/src/lib.rs`:

```rust
pub mod catalog_agent;
pub mod orders_agent;
pub mod inventory_agent;
pub mod production_agent;
pub mod crm_agent;
pub mod my_agent; // Add this

use a2a_rs::*;
use std::sync::Arc;

pub fn create_all_agents(config: &Config) -> Vec<Arc<dyn AsyncMessageHandler>> {
    vec![
        Arc::new(catalog_agent::CatalogAgent::new(config.clone())),
        Arc::new(orders_agent::OrdersAgent::new(config.clone())),
        Arc::new(inventory_agent::InventoryAgent::new(config.clone())),
        Arc::new(production_agent::ProductionAgent::new(config.clone())),
        Arc::new(crm_agent::CrmAgent::new(config.clone())),
        Arc::new(my_agent::MyAgent::new(my_agent::MyAgentClient::new(
            config.craftplan_api_url.clone(),
            config.api_key.clone(),
        ))),
    ]
}
```

### Add Agent to Server

Update `a2a-agents/src/bin/my_agent_server.rs`:

```rust
use a2a_rs::*;
use a2a_agents::{Config, create_all_agents};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Load config from environment
    let config = Config::from_env()?;

    // Create all agents
    let agents = create_all_agents(&config);

    // Create request processor with all agents
    let message_handler = CompositeMessageHandler::new(agents);
    let task_manager = InMemoryTaskStorage::new();
    let notification_manager = NoopPushNotificationSender::new();
    let agent_info = SimpleAgentInfo::new(
        "craftplan-agents".to_string(),
        "1.0.0".to_string(),
    );

    let processor = DefaultRequestProcessor::new(
        message_handler,
        task_manager,
        notification_manager,
        agent_info,
    );

    // Start HTTP server
    let server = HttpServer::new(
        processor,
        "0.0.0.0:8080".to_string(),
    );

    println!("Starting A2A Agents server on http://0.0.0.0:8080");
    server.start().await?;

    Ok(())
}
```

### Test Agent

```bash
# Run agent server
cargo run --bin my_agent_server

# Test with curl
curl -X POST http://localhost:8080/message \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "method": "message/send",
    "params": {
      "taskId": "test-1",
      "message": {
        "role": "user",
        "parts": [{"text": "get resource_123"}]
      }
    },
    "id": 1
  }'
```

---

## Testing

### Unit Tests (Rust)

```bash
cd a2a-rs/a2a-agents

# Run all tests
cargo test

# Run specific test
cargo test test_catalog_agent

# Run with output
cargo test -- --nocapture

# Run tests in parallel
cargo test -- --test-threads=4
```

### Integration Tests

Create `tests/integration_test.rs`:

```rust
use a2a_rs::*;
use a2a_agents::CatalogAgent;

#[tokio::test]
async fn test_catalog_agent_integration() {
    // Skip if Craftplan not running
    let api_url = std::env::var("CRAFTPLAN_API_URL")
        .unwrap_or("http://localhost:4000".to_string());

    let client = reqwest::Client::new();
    let response = client.get(&format!("{}/api/health", api_url))
        .send()
        .await;

    if response.is_err() {
        println!("Skipping integration test: Craftplan not running");
        return;
    }

    // Create agent
    let agent = CatalogAgent::new(api_url);

    // Test product query
    let message = Message::user_text(
        "What is the product with ID product_123?".to_string(),
        "msg-1".to_string(),
    );

    let task = agent.handle_message("task-1", &message, None).await
        .expect("Failed to handle message");

    assert_eq!(task.status.state, TaskState::Completed);
    assert!(!task.artifacts.is_empty());
}
```

### Load Testing

Using **vegeta**:

```bash
# Install vegeta
brew install vegeta

# Create attack file
cat > targets.txt << EOF
POST http://localhost:8080/message
Content-Type: application/json

{
  "jsonrpc": "2.0",
  "method": "message/send",
  "params": {
    "taskId": "{{.RandomInt}}",
    "message": {
      "role": "user",
      "parts": [{"text": "list products"}]
    }
  },
  "id": 1
}
EOF

# Run load test (100 requests per second for 30 seconds)
vegeta attack -targets=targets.txt -rate=100 -duration=30s | vegeta report

# Generate report
vegeta attack -targets=targets.txt -rate=100 -duration=30s | vegeta report -type=text > report.txt
```

---

## Deployment

### Build Production Images

#### A2A Agents

Create `a2a-rs/a2a-agents/Dockerfile`:

```dockerfile
# Build stage
FROM rust:1.70 as builder

WORKDIR /app
COPY . .

# Build release binary
RUN cargo build --release --bin catalog_agent_server

# Runtime stage
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy binary from builder
COPY --from=builder /app/target/release/catalog_agent_server /app/

# Expose ports
EXPOSE 8080 8081

# Set environment variables
ENV RUST_LOG=info
ENV CRAFTPLAN_API_URL=http://craftplan:4000

# Run server
CMD ["./catalog_agent_server"]
```

Build and push:

```bash
# Build image
docker build -t your-registry/a2a-agents:latest .

# Push to registry
docker push your-registry/a2a-agents:latest
```

#### Deploy to Kubernetes

```bash
# Create namespace
kubectl create namespace craftplan-a2a

# Create secret (JWT, etc.)
kubectl create secret generic craftplan-secrets \
  --from-literal=jwt-secret=$(openssl rand -base64 32) \
  --namespace=craftplan-a2a

# Deploy agents
kubectl apply -f kubernetes/a2a-agents-deployment.yaml

# Expose service
kubectl apply -f kubernetes/a2a-agents-service.yaml

# Check deployment
kubectl get pods -n craftplan-a2a
kubectl logs -f deployment/a2a-agents -n craftplan-a2a
```

---

## Troubleshooting

### Common Issues

#### 1. "Connection refused" to Craftplan

**Symptoms**:
```
Error: Failed to connect to Craftplan API
```

**Solutions**:
```bash
# Check Craftplan is running
docker compose ps craftplan

# Check logs
docker compose logs craftplan

# Restart Craftplan
docker compose restart craftplan

# Verify network
docker network ls
docker network inspect craftplan-a2a_default
```

#### 2. "Invalid JWT" errors

**Symptoms**:
```
Error: Authentication failed: Invalid JWT
```

**Solutions**:
```bash
# Check JWT_SECRET is set
docker compose exec a2a-agents env | grep JWT_SECRET

# Regenerate secrets
rm .env
# Re-run Step 4 from Quick Start

# Verify JWT matches in both services
docker compose exec craftplan env | grep JWT_SECRET
docker compose exec a2a-agents env | grep JWT_SECRET
```

#### 3. Agent not responding

**Symptoms**:
```
curl: (7) Failed to connect to localhost:8080
```

**Solutions**:
```bash
# Check agent is running
docker compose ps a2a-agents

# Check logs
docker compose logs -f a2a-agents

# Restart agent
docker compose restart a2a-agents

# Check health endpoint
curl http://localhost:8080/health
```

#### 4. Database connection errors

**Symptoms**:
```
Error: Database connection failed
```

**Solutions**:
```bash
# Check PostgreSQL is running
docker compose ps postgres

# Check logs
docker compose logs postgres

# Restart PostgreSQL
docker compose restart postgres

# Verify connection
docker compose exec postgres psql -U postgres -c "\l"
```

### Debug Mode

Enable verbose logging:

```bash
# Stop services
docker compose down

# Enable debug logging
cat >> .env << EOF
RUST_LOG=debug
LOG_LEVEL=debug
EOF

# Restart services
docker compose up -d

# Follow logs
docker compose logs -f a2a-agents
```

### Performance Issues

**Symptoms**: Slow response times, high memory usage

**Solutions**:

```bash
# Check resource usage
docker stats

# Reduce cache size in agent config
export CACHE_MAX_SIZE=100  # default 1000

# Increase connection pool
export DB_POOL_SIZE=10  # default 5

# Profile Rust code
cargo build --release
cargo flamegraph --bin catalog_agent_server

# Analyze with flamegraph
open flamegraph.svg
```

### Getting Help

1. **Check logs**: `docker compose logs -f <service>`
2. **Enable debug mode**: Set `RUST_LOG=debug`
3. **Run health checks**: `curl http://localhost:8080/health`
4. **Review documentation**:
   - [A2A-RS Docs](https://docs.rs/a2a-rs)
   - [Craftplan Docs](https://github.com/puemos/craftplan)
5. **Open an issue**: [GitHub Issues](https://github.com/emillindfors/a2a-rs/issues)

---

## Next Steps

After completing this guide:

1. **Explore the architecture**: Read [craftplan-architecture.md](./craftplan-architecture.md)
2. **Implement custom agents**: Follow [Agent Implementation Guide](#agent-implementation-guide)
3. **Add MCP bridge**: See [a2a-mcp/README.md](../vendors/a2a-rs/a2a-mcp/README.md)
4. **Deploy to production**: Follow [Deployment](#deployment) section
5. **Contribute**: See [CONTRIBUTING.md](../vendors/a2a-rs/CONTRIBUTING.md)

---

**Document Version Control**:

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0.0 | 2026-02-03 | System Architect | Initial integration guide |

---

**End of Document**
