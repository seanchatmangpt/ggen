<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Craftplan Integration Summary](#craftplan-integration-summary)
  - [Document Deliverables](#document-deliverables)
    - [1. Architecture Document (`craftplan-architecture.md`)](#1-architecture-document-craftplan-architecturemd)
    - [2. Integration Guide (`craftplan-integration-guide.md`)](#2-integration-guide-craftplan-integration-guidemd)
  - [Architecture Overview](#architecture-overview)
  - [Integration Approach](#integration-approach)
    - [Technology Bridge](#technology-bridge)
    - [Agent-Domain Mapping](#agent-domain-mapping)
  - [Implementation Roadmap](#implementation-roadmap)
    - [Phase 1: Foundation (Weeks 1-2)](#phase-1-foundation-weeks-1-2)
    - [Phase 2: Multi-Domain Agents (Weeks 3-4)](#phase-2-multi-domain-agents-weeks-3-4)
    - [Phase 3: WebSocket Streaming (Weeks 5-6)](#phase-3-websocket-streaming-weeks-5-6)
    - [Phase 4: MCP Bridge (Weeks 7-8)](#phase-4-mcp-bridge-weeks-7-8)
    - [Phase 5: Advanced Features (Weeks 9-10)](#phase-5-advanced-features-weeks-9-10)
    - [Phase 6: Client UI (Weeks 11-12)](#phase-6-client-ui-weeks-11-12)
  - [Quick Start Commands](#quick-start-commands)
    - [Start All Services (5 minutes)](#start-all-services-5-minutes)
    - [Development Mode](#development-mode)
  - [Key Design Decisions](#key-design-decisions)
    - [1. **Hexagonal Architecture**](#1-hexagonal-architecture)
    - [2. **Agent per Domain Pattern**](#2-agent-per-domain-pattern)
    - [3. **HTTP Bridge (Not Direct Elixir-Rust FFI)**](#3-http-bridge-not-direct-elixir-rust-ffi)
    - [4. **Caching Layer in Rust Agents**](#4-caching-layer-in-rust-agents)
    - [5. **WebSocket for Real-Time Updates**](#5-websocket-for-real-time-updates)
  - [Security Considerations](#security-considerations)
    - [Authentication Flow](#authentication-flow)
    - [Key Security Measures](#key-security-measures)
  - [Performance Targets](#performance-targets)
  - [Monitoring & Observability](#monitoring--observability)
    - [Metrics to Track](#metrics-to-track)
    - [Logging Strategy](#logging-strategy)
    - [Distributed Tracing](#distributed-tracing)
  - [File Locations](#file-locations)
    - [Architecture Documents](#architecture-documents)
    - [Source Code](#source-code)
    - [Configuration Files](#configuration-files)
  - [Next Steps](#next-steps)
    - [Immediate Actions](#immediate-actions)
    - [Week 1 Priorities](#week-1-priorities)
    - [Week 2-3 Priorities](#week-2-3-priorities)
    - [Long-Term Vision](#long-term-vision)
  - [Success Criteria](#success-criteria)
    - [Phase 1 Success (Week 2)](#phase-1-success-week-2)
    - [Phase 2 Success (Week 4)](#phase-2-success-week-4)
    - [Phase 3 Success (Week 6)](#phase-3-success-week-6)
    - [Phase 4 Success (Week 8)](#phase-4-success-week-8)
    - [Phase 5 Success (Week 10)](#phase-5-success-week-10)
    - [Phase 6 Success (Week 12)](#phase-6-success-week-12)
  - [Resources](#resources)
    - [Documentation](#documentation)
    - [Code Examples](#code-examples)
    - [Community](#community)
  - [Contact & Support](#contact--support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Craftplan Integration Summary

**Date**: 2026-02-03
**Status**: Architecture Design Complete
**Next Step**: Implementation Phase 1 (Foundation)

---

## Document Deliverables

This integration project includes two comprehensive documents:

### 1. Architecture Document (`craftplan-architecture.md`)

**Purpose**: High-level system design and technical specifications

**Contents**:
- System overview with component diagrams
- Hexagonal architecture principles
- Detailed component architecture (A2A-RS, A2A-Agents, A2A-MCP, Craftplan)
- Data flow diagrams for all interaction patterns
- Integration patterns (Agent per Domain, Intent Routing, Caching, Event-Driven)
- Security & authentication architecture
- API contracts and message schemas
- Deployment architecture (Docker, Kubernetes, Fly.io)
- Error handling strategies
- 12-week implementation roadmap

**Key Diagrams**:
- High-level system architecture
- Hexagonal architecture layers
- Data flow sequences
- Authentication flow
- Network topology

### 2. Integration Guide (`craftplan-integration-guide.md`)

**Purpose**: Step-by-step setup and implementation instructions

**Contents**:
- Quick start (5-minute setup)
- Development environment setup
- Agent implementation template with full code examples
- Testing strategies (unit, integration, load)
- Deployment guides (Docker, Kubernetes)
- Troubleshooting guide
- Production deployment checklist

**Code Examples**:
- Complete agent implementation (Rust)
- Craftplan API client (Rust)
- Message handler with intent parsing
- Docker Compose configuration
- Kubernetes manifests

---

## Architecture Overview

```
┌────────────────────────────────────────────────────────────┐
│                    A2A-CLIENT (Web UI)                     │
│                   Axum + Askama Templates                  │
│                   http://localhost:3000                    │
└───────────────────────────┬────────────────────────────────┘
                            │ HTTP/WebSocket
                            ▼
┌────────────────────────────────────────────────────────────┐
│                      A2A-AGENTS (Rust)                     │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐     │
│  │ Catalog  │ │  Orders  │ │Inventory │ │Production│     │
│  │  Agent   │ │  Agent   │ │  Agent   │ │  Agent   │     │
│  └────┬─────┘ └────┬─────┘ └────┬─────┘ └────┬─────┘     │
│       │            │            │            │             │
│       └────────────┴────────────┴────────────┘             │
│                      │                                     │
│           ┌──────────▼──────────┐                          │
│           │ DefaultRequest      │                          │
│           │ Processor (Ports)   │                          │
│           └──────────┬──────────┘                          │
└──────────────────────┼────────────────────────────────────┘
                       │
        ┌──────────────┼──────────────┐
        │              │              │
        ▼              ▼              ▼
┌──────────────┐ ┌──────────┐ ┌──────────────────┐
│   A2A-RS     │ │ A2A-MCP  │ │  CRAFTPLAN       │
│   CORE       │ │ BRIDGE   │ │  (Elixir)        │
│              │ │          │ │  Ash Framework   │
│ - HTTP/WS    │ │Protocol  │ │  JSON:API        │
│ - Auth       │ │Translation│ │  PostgreSQL      │
│ - Storage    │ │          │ │                  │
└──────────────┘ └──────────┘ └──────────────────┘
                                             │
                                             ▼
                                  ┌────────────────────┐
                                  │  PostgreSQL 16     │
                                  │  + MinIO (S3)      │
                                  └────────────────────┘
```

---

## Integration Approach

### Technology Bridge

| Aspect | A2A-RS Ecosystem | Craftplan | Integration Method |
|--------|------------------|-----------|-------------------|
| **Language** | Rust | Elixir | HTTP/JSON bridge |
| **Protocol** | A2A v0.3.0 | JSON:API | Message translation |
| **Transport** | HTTP/WebSocket | Phoenix Channels | WebSocket ↔ Channel |
| **Auth** | JWT, OAuth2 | AshAuthentication | Shared JWT secret |
| **Storage** | SQLx (tasks) | AshPostgres | Shared PostgreSQL |
| **Real-time** | WebSocket | Phoenix PubSub | PubSub ↔ WebSocket |

### Agent-Domain Mapping

| Craftplan Domain | A2A Agent | Responsibilities |
|------------------|-----------|------------------|
| **Catalog** | `CatalogAgent` | Product queries, BOM rollups, cost calculations |
| **Orders** | `OrdersAgent` | Order creation, status updates, allocations |
| **Inventory** | `InventoryAgent` | Stock queries, movements, forecasting |
| **Production** | `ProductionAgent` | Batch creation, completion, consumption |
| **CRM** | `CrmAgent` | Customer lookups, order history |

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-2)
- [ ] Set up a2a-agents project
- [ ] Implement `CraftplanClient` (HTTP → JSON:API)
- [ ] Create `CatalogAgent` with product queries
- [ ] Add JWT authentication
- [ ] Docker Compose deployment
- [ ] Integration tests

**Deliverable**: Working `CatalogAgent` that queries products from Craftplan

### Phase 2: Multi-Domain Agents (Weeks 3-4)
- [ ] Implement `OrdersAgent`, `InventoryAgent`, `ProductionAgent`, `CrmAgent`
- [ ] Add caching layer (LRU cache)
- [ ] Intent extraction from natural language
- [ ] Error handling and validation

**Deliverable**: 5 domain agents with full CRUD capabilities

### Phase 3: WebSocket Streaming (Weeks 5-6)
- [ ] WebSocket server implementation
- [ ] Phoenix.Channel subscriptions
- [ ] Real-time updates (production batches, inventory)
- [ ] Connection management and heartbeat

**Deliverable**: Real-time streaming for long-running operations

### Phase 4: MCP Bridge (Weeks 7-8)
- [ ] RMCP server implementation
- [ ] A2A ↔ MCP message converters
- [ ] Expose agents as MCP tools
- [ ] State management across protocols

**Deliverable**: Agents callable from MCP clients (e.g., Claude, ChatGPT)

### Phase 5: Advanced Features (Weeks 9-10)
- [ ] Rate limiting
- [ ] Logging and metrics (Prometheus)
- [ ] Distributed tracing (OpenTelemetry)
- [ ] Health check endpoints

**Deliverable**: Production-ready monitoring and observability

### Phase 6: Client UI (Weeks 11-12)
- [ ] Enhanced web client
- [ ] Dynamic form generation
- [ ] Rich response formatting
- [ ] Task history browser

**Deliverable**: Polished UI at http://localhost:3000

---

## Quick Start Commands

### Start All Services (5 minutes)

```bash
# Clone repositories
git clone https://github.com/emillindfors/a2a-rs.git
git clone https://github.com/puemos/craftplan.git

# Create environment
cd craftplan-a2a
cp a2a-rs/docs/docker-compose.yml .
cp a2a-rs/docs/.env.example .env

# Generate secrets
JWT_SECRET=$(openssl rand -base64 32)
SECRET_KEY_BASE=$(openssl rand -base64 48)
POSTGRES_PASSWORD=$(openssl rand -base64 16)

# Update .env with generated secrets
# Then start services
docker compose up -d

# Verify
curl http://localhost:4000/api/health  # Craftplan
curl http://localhost:8080/agent-card   # A2A Agents
open http://localhost:3000              # Web Client
```

### Development Mode

```bash
# Terminal 1: Craftplan
cd craftplan
mix phx.server  # http://localhost:4000

# Terminal 2: A2A Agents
cd a2a-rs/a2a-agents
export CRAFTPLAN_API_URL="http://localhost:4000"
cargo run --bin catalog_agent_server  # http://localhost:8080

# Terminal 3: A2A Client
cd a2a-rs/a2a-client
export AGENT_URL="http://localhost:8080"
cargo run --bin server  # http://localhost:3000
```

---

## Key Design Decisions

### 1. **Hexagonal Architecture**
**Rationale**: Clean separation between framework (a2a-rs) and business logic (agents)

**Benefits**:
- Easy to test
- Swappable components
- Framework-independent business logic

### 2. **Agent per Domain Pattern**
**Rationale**: One agent per Craftplan Ash domain

**Benefits**:
- Clear ownership
- Independent scaling
- Easy to extend

### 3. **HTTP Bridge (Not Direct Elixir-Rust FFI)**
**Rationale**: Use existing JSON:API instead of native bindings

**Benefits**:
- No changes to Craftplan
- Language isolation
- Easier debugging
- Production-ready API

### 4. **Caching Layer in Rust Agents**
**Rationale**: Reduce load on Craftplan API

**Benefits**:
- Faster response times
- Reduced database queries
- Better user experience

### 5. **WebSocket for Real-Time Updates**
**Rationale**: Long-running operations need progress feedback

**Benefits**:
- Live status updates
- Better UX for batch operations
- Reduced polling overhead

---

## Security Considerations

### Authentication Flow

```
Client → A2A-RS (JWT validation)
         ↓
   A2A-RS → Craftplan (forward JWT)
         ↓
   Craftplan (AshAuthentication)
         ↓
   PostgreSQL (data access)
```

### Key Security Measures

1. **JWT Token Sharing**: Same secret used by both systems
2. **Role-Based Access Control**: Admin, Staff, ReadOnly roles
3. **API Key Encryption**: Stored encrypted at rest
4. **Rate Limiting**: Per-user request limits
5. **HTTPS Required**: Production deployments only
6. **Input Validation**: All user inputs sanitized

---

## Performance Targets

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Agent Response Time** | < 500ms (p95) | Agent processing latency |
| **End-to-End Latency** | < 1s (p95) | Client request → response |
| **Cache Hit Rate** | > 80% | Percentage of cache hits |
| **WebSocket Message Delay** | < 100ms | Real-time update delivery |
| **Concurrent Users** | 100+ | Simultaneous agent connections |
| **Throughput** | 1000 req/min | Sustained request rate |

---

## Monitoring & Observability

### Metrics to Track

- **Request rate** (per agent, per endpoint)
- **Response time** (p50, p95, p99)
- **Error rate** (4xx, 5xx)
- **Cache hit/miss ratio**
- **WebSocket connections** (active, closed)
- **Database query time**
- **Memory usage** (per service)

### Logging Strategy

```rust
// Structured logging with tracing
use tracing::{info, error, instrument};

#[instrument(skip(self))]
async fn handle_message(&self, task_id: &str, message: &Message) -> Result<Task, A2AError> {
    info!(task_id, "Handling message");

    match result {
        Ok(task) => {
            info!(task_id, artifacts = task.artifacts.len(), "Message handled successfully");
            Ok(task)
        }
        Err(e) => {
            error!(task_id, error = %e, "Failed to handle message");
            Err(e)
        }
    }
}
```

### Distributed Tracing

```rust
// OpenTelemetry integration
use opentelemetry::global;
use tracing_opentelemetry::OpenTelemetrySpanExt;

let span = tracing::info_span!("handle_message", task_id = %task_id);
span.in_scope(|| {
    // Business logic here
});
```

---

## File Locations

### Architecture Documents
```
/Users/sac/ggen/docs/
├── craftplan-architecture.md       # System design (this summary)
└── craftplan-integration-guide.md  # Setup instructions
```

### Source Code
```
/Users/sac/ggen/vendors/
├── a2a-rs/
│   ├── a2a-rs/          # Core framework
│   ├── a2a-agents/      # Agent implementations
│   ├── a2a-mcp/         # MCP bridge
│   └── a2a-client/      # Web UI
└── craftplan/           # ERP system
```

### Configuration Files
```
/Users/sac/ggen/
├── docker-compose.yml              # Development deployment
├── docker-compose.integration.yml  # Integration tests
└── vendors/
    └── .env.example                # Environment template
```

---

## Next Steps

### Immediate Actions

1. **Review Architecture**
   - Read `craftplan-architecture.md` (Sections 1-6)
   - Understand hexagonal architecture
   - Review data flow diagrams

2. **Set Up Development Environment**
   - Follow Quick Start (5 minutes)
   - Verify all services running
   - Test with sample queries

3. **Implement First Agent**
   - Follow Agent Implementation Guide
   - Create `CatalogAgent`
   - Test with Craftplan API

4. **Write Tests**
   - Unit tests for agent logic
   - Integration tests for API calls
   - Load tests for performance

### Week 1 Priorities

- [ ] Complete Phase 1 setup (development environment)
- [ ] Implement `CraftplanClient` with JSON:API calls
- [ ] Create `CatalogAgent` with product query capability
- [ ] Add JWT authentication
- [ ] Deploy to local Docker Compose
- [ ] Write integration tests

### Week 2-3 Priorities

- [ ] Implement remaining domain agents (Orders, Inventory, Production, CRM)
- [ ] Add caching layer with LRU cache
- [ ] Implement intent extraction (simple pattern matching → LLM)
- [ ] Add comprehensive error handling

### Long-Term Vision

**Goal**: Create a production-ready AI-driven ERP system where:
- Natural language queries are routed to appropriate agents
- Agents coordinate complex workflows across domains
- Real-time updates stream via WebSocket
- LLMs can interact via MCP bridge
- System scales to handle enterprise workloads

---

## Success Criteria

### Phase 1 Success (Week 2)
- ✓ `CatalogAgent` successfully queries products from Craftplan
- ✓ JWT authentication working end-to-end
- ✓ Integration tests passing
- ✓ Response time < 500ms (p95)

### Phase 2 Success (Week 4)
- ✓ All 5 domain agents implemented
- ✓ Caching layer operational (80%+ hit rate)
- ✓ Intent routing working
- ✓ Comprehensive error handling

### Phase 3 Success (Week 6)
- ✓ WebSocket server operational
- ✓ Real-time updates for production batches
- ✓ Connection management (reconnect, heartbeat)

### Phase 4 Success (Week 8)
- ✓ MCP bridge functional
- ✓ Agents callable from MCP clients
- ✓ Bidirectional protocol translation

### Phase 5 Success (Week 10)
- ✓ Rate limiting enforced
- ✓ Metrics collected (Prometheus)
- ✓ Distributed tracing (OpenTelemetry)
- ✓ Health checks operational

### Phase 6 Success (Week 12)
- ✓ Enhanced web client deployed
- ✓ Dynamic form generation
- ✓ Rich response formatting
- ✓ Production-ready documentation

---

## Resources

### Documentation
- [A2A Protocol Spec](https://github.com/modelcontextprotocol/rust-sdk)
- [a2a-rs Docs](https://docs.rs/a2a-rs)
- [Ash Framework](https://ash-hq.org/)
- [Craftplan Docs](https://github.com/puemos/craftplan)

### Code Examples
- [Reimbursement Agent](../vendors/a2a-rs/a2a-agents/src/reimbursement_agent/)
- [A2A Client](../vendors/a2a-rs/a2a-client/src/bin/server.rs)

### Community
- [A2A-RS GitHub](https://github.com/emillindfors/a2a-rs)
- [Craftplan GitHub](https://github.com/puemos/craftplan)
- [Ash Framework Discord](https://discord.gg/ash-framework)

---

## Contact & Support

**Architecture Questions**: Review `craftplan-architecture.md`
**Implementation Issues**: See `craftplan-integration-guide.md` → Troubleshooting
**Bug Reports**: [GitHub Issues](https://github.com/emillindfors/a2a-rs/issues)
**Feature Requests**: [GitHub Discussions](https://github.com/emillindfors/a2a-rs/discussions)

---

**End of Summary**

For detailed information, see:
- System Architecture: `craftplan-architecture.md`
- Setup Guide: `craftplan-integration-guide.md`
