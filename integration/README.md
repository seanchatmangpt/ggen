# GGEN Integration Stack - Craftplan + A2A-RS

## Overview

This integration stack brings together three powerful systems:

1. **Craftplan** - Elixir-based ERP for artisanal manufacturing
2. **A2A-RS** - Rust implementation of Agent-to-Agent communication protocol
3. **Craftplan-A2A Bridge** - Rust service bridging Craftplan with A2A agents

## Services Running

| Service | URL | Description |
|---------|-----|-------------|
| Craftplan | http://localhost:4000 | Manufacturing ERP |
| MinIO Console | http://localhost:9001 | S3-compatible storage |
| A2A Bridge | http://localhost:5001 | Agent protocol bridge |

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         GGEN Integration                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────────┐        ┌──────────────┐      ┌────────────┐ │
│  │   Craftplan  │◄───────│  A2A Bridge  │─────►│  A2A-RS    │ │
│  │   (Elixir)   │        │   (Rust)     │      │  Agents    │ │
│  │   Port 4000  │        │   Port 5001  │      │            │ │
│  └──────────────┘        └──────────────┘      └────────────┘ │
│         │                       │                             │
│         ▼                       ▼                             │
│  ┌──────────────┐        ┌──────────────┐                     │
│  │  PostgreSQL  │        │     MCP      │                     │
│  │   Port 5432  │        │    Bridge    │                     │
│  └──────────────┘        └──────────────┘                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Quick Start

### 1. Start All Services

```bash
# Start Craftplan
cd vendors/craftplan
docker compose up -d

# Start A2A Bridge
cd ../integration
docker compose up -d --build
```

### 2. Verify Services

```bash
# Check Craftplan
curl http://localhost:4000

# Check Bridge health
curl http://localhost:5001/health

# Get agent card
curl http://localhost:5001/agent-card
```

### 3. Access Craftplan

Open http://localhost:4000 in your browser.

Demo credentials:
- Email: `test@test.com`
- Password: `Aa123123123123`

## A2A Bridge API

### Health Check
```bash
curl http://localhost:5001/health
```

### Get Agent Card
```bash
curl http://localhost:5001/agent-card
```

### Create Task
```bash
curl -X POST http://localhost:5001/tasks \
  -H "Content-Type: application/json" \
  -d '{
    "resource": "products",
    "operation": "list",
    "params": {}
  }'
```

### List Tasks
```bash
curl http://localhost:5001/tasks
```

### Get Task Status
```bash
curl http://localhost:5001/tasks/{task_id}
```

## Available Agent Skills

The bridge exposes the following Craftplan operations as A2A agent skills:

| Skill | Description |
|-------|-------------|
| `list_products` | List all products in the catalog |
| `create_order` | Create a new customer order |
| `check_inventory` | Check inventory levels for materials |
| `create_production_batch` | Create a production batch |

## Vendor Repositories

- **Craftplan**: `vendors/craftplan/` - https://github.com/puemos/craftplan
- **A2A-RS**: `vendors/a2a-rs/` - https://github.com/EmilLindfors/a2a-rs
- **A2A-MCP**: `vendors/a2a-rs/a2a-mcp/` - MCP integration layer

## Stopping Services

```bash
# Stop bridge
cd integration && docker compose down

# Stop Craftplan
cd ../vendors/craftplan && docker compose down
```

## Next Steps

1. Implement actual Craftplan API calls in the bridge
2. Add authentication between services
3. Implement WebSocket streaming for real-time updates
4. Add MCP tool registration
5. Create example agents that use Craftplan data
