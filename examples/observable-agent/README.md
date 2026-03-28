# observable-agent — ggen Advanced Example

**Single RDF ontology → OTel Weaver + MCP server + A2A agent + Docker Compose**

This example demonstrates ggen's core promise: one declarative RDF ontology acts as the
single source of truth for an entire multi-protocol AI agent stack.  Edit
`ontology/observable-agent.ttl`, run `ggen sync`, and all five output files regenerate
consistently.

---

## Architecture

```
ontology/observable-agent.ttl   ← single source of truth
         │
         │  ggen sync (5 SPARQL queries → 5 Tera templates)
         │
         ├── generated/semconv.yaml        OTel Weaver semantic conventions
         ├── generated/mcp_server.rs       rmcp MCP server (3 tools)
         ├── generated/a2a_agent.rs        A2A agent entry point (3 skills)
         ├── generated/docker-compose.yml  3-service stack
         └── generated/agent_card.json     A2A /.well-known/agent.json card
```

### Protocol layers

| File | Protocol | Purpose |
|---|---|---|
| `semconv.yaml` | OTel Weaver | Defines span groups and attribute schemas for each capability |
| `mcp_server.rs` | MCP (rmcp) | Exposes each capability as a typed MCP tool with `#[tool]` macros |
| `a2a_agent.rs` | A2A | Registers each capability as an A2A skill; boots HTTP server |
| `docker-compose.yml` | Docker | OTel Collector + agent service wired together |
| `agent_card.json` | A2A discovery | Well-known card listing all skills for agent discovery |

---

## Quick Start

```bash
# From this directory
~/.local/bin/ggen sync
```

That's it.  All five `generated/` files are (re)written in under 50 ms.

---

## Ontology Structure

The ontology has three concept types:

**`obs:Agent`** — the agent identity (name, version, bind address, MCP struct, A2A struct).

**`obs:Capability`** — each skill/tool/span.  A capability carries predicates for all three
protocols simultaneously:
- `obs:toolName` / `obs:structName` → MCP tool and parameter struct names
- `obs:capId` / `obs:capName` / `obs:handlerName` → A2A skill registration
- `obs:spanName` → OTel span name in semconv groups

**`obs:Attribute`** — OTel span attributes attached to a capability via `obs:capabilityOf`.

**`obs:Service`** — Docker Compose services (OTel Collector, the agent itself).

---

## Generated Files

### `semconv.yaml`

OTel Weaver semantic conventions with one `span` group per capability.  Each group
lists the typed, required/recommended attributes for that operation.  Feed this to
`weaver registry check` or `weaver registry generate` to validate and emit language
bindings.

### `mcp_server.rs`

A complete `rmcp` server struct (`ObservableCodeGenServer`) with:
- One `#[derive(Deserialize, JsonSchema)]` params struct per tool
- One `#[tool(description = "...")]` async method per tool
- `ServerHandler` impl wired to the tool router

Drop this file into a Rust crate that depends on `rmcp` and it compiles immediately.

### `a2a_agent.rs`

A `tokio::main` entry point that:
- Initialises tracing via `a2a_rs::observability`
- Constructs `SimpleAgentInfo` with all three skills
- Starts a bearer-token-authenticated HTTP server

The handler logic lives in `mod handlers` (not generated — implement it once).

### `docker-compose.yml`

Two-service stack:
- `otel-collector` — receives OTLP traces from the agent
- `observable-agent` — the Rust binary

### `agent_card.json`

A2A well-known agent card served at `/.well-known/agent.json`.  Lists all skills with
their input/output modes so peer agents can discover capabilities.

---

## How Weaver + MCP + A2A Compose

```
              ┌─────────────────────────────────────┐
              │          ontology predicate          │
              ├──────────────┬──────────┬────────────┤
              │  obs:spanName│obs:toolName│obs:capId  │
              │  obs:attrName│obs:structName│obs:capName│
              └──────┬───────┴────┬─────┴─────┬──────┘
                     │            │            │
              ┌──────▼──────┐ ┌───▼────┐ ┌────▼──────┐
              │ semconv.yaml│ │mcp_    │ │a2a_agent  │
              │ (Weaver)    │ │server  │ │.rs (A2A)  │
              │             │ │.rs     │ │           │
              └─────────────┘ └────────┘ └───────────┘
                     └──────────┬─────────────┘
                                │
                         docker-compose.yml
                         agent_card.json
```

One edit to the ontology propagates atomically to all five files.  No drift between the
telemetry schema, the MCP tool definitions, and the A2A skill registrations.

---

## Adding a New Capability

1. Add a new `obs:Capability` triple block in `ontology/observable-agent.ttl` with a
   unique `obs:order` value.
2. Optionally add `obs:Attribute` triples pointing `obs:capabilityOf` at the new
   capability.
3. Run `ggen sync` — all five files regenerate with the new capability included.
4. Implement the handler method in `src/handlers.rs` (the only file you write by hand).

---

## File Layout

```
examples/observable-agent/
  ontology/
    observable-agent.ttl    source of truth
  templates/
    semconv.yaml.tera       Tera template for OTel Weaver output
    mcp_server.rs.tera      Tera template for MCP server
    a2a_agent.rs.tera       Tera template for A2A agent main
    docker-compose.yml.tera Tera template for Docker Compose
    agent_card.json.tera    Tera template for A2A agent card
  generated/               output (do not edit by hand)
    semconv.yaml
    mcp_server.rs
    a2a_agent.rs
    docker-compose.yml
    agent_card.json
  ggen.toml                 generation rules (5 rules, one per output)
  README.md                 this file
```
