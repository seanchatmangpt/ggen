# a2a-agent-definition

This example demonstrates how to define an **Agent-to-Agent (A2A) protocol** agent — including its skills — entirely in RDF Turtle, then generate a complete `a2a-rs` Rust implementation using `ggen sync`.

## What This Example Demonstrates

- Declaring an A2A agent and its skills in a single `.ttl` ontology file
- Using `ggen sync` to generate three artifacts from that single source of truth:
  - `generated/agent.rs` — the Tokio `main` entry point with `HttpServer`, `BearerTokenAuthenticator`, `SimpleAgentInfo`, and all four skills wired in
  - `generated/handlers.rs` — the `GgenAgentHandler` struct that implements every required `a2a-rs` port trait (`AsyncMessageHandler`, `AsyncTaskManager`, `AsyncNotificationManager`, `AsyncStreamingHandler`) and dispatches inbound messages to skill-specific methods
  - `generated/agent_card.json` — a standards-compliant A2A Agent Card (skills manifest) ready to serve at `/.well-known/agent.json`

## Project Structure

```
a2a-agent-definition/
  ontology/
    a2a-agent.ttl        # Source of truth — edit here, never in generated/
  templates/
    agent.rs.tera        # Tera template → generated/agent.rs
    handlers.rs.tera     # Tera template → generated/handlers.rs
    agent-card.json.tera # Tera template → generated/agent_card.json
  ggen.toml              # ggen project config (SPARQL queries + rules)
  generated/             # All output lives here — do not edit
    agent.rs
    handlers.rs
    agent_card.json
```

## Quick Start

```bash
# From this directory:
ggen sync

# Or with the full path to the binary:
~/.local/bin/ggen sync
```

To preview without writing files:

```bash
ggen sync --dry_run true
```

## The Agent Skills

The ontology defines four skills on `agent:CodeGenAgent`:

| Skill ID           | Method              | Description                                    |
|--------------------|---------------------|------------------------------------------------|
| `sync-project`     | `sync_project`      | Run ggen sync pipeline from RDF ontology       |
| `validate-ontology`| `validate_ontology` | Validate a Turtle ontology against SHACL rules |
| `generate-code`    | `generate_code`     | Generate code from an RDF ontology file        |
| `explain-ontology` | `explain_ontology`  | Explain what an RDF ontology will generate     |

## A2A Protocol Overview

The **Agent-to-Agent (A2A) protocol** is a JSON-RPC 2.0-based protocol for inter-agent communication. Key concepts:

- **Agent Card** — a JSON manifest served at `/.well-known/agent.json` that describes the agent's name, URL, and capabilities (skills). Other agents discover yours by fetching this URL.
- **Task** — the unit of work. A client sends a `tasks/send` request with a `Message`; the server processes it and returns a `Task` with a `TaskStatus` (e.g., `completed`, `working`, `failed`).
- **Skill** — a named capability the agent advertises. The agent card lists all skills with their input/output modalities. Skill dispatch (routing inbound messages to the right handler) is the agent's responsibility.
- **Message** — a structured payload containing one or more `Part` values (text, file, data). Messages carry a `role` (`user` or `agent`) and a unique `messageId`.
- **Bearer Token Auth** — the HTTP server requires an `Authorization: Bearer <token>` header. Set the token via `AGENT_TOKEN` env var (defaults to `dev-token`).

### Running the Generated Agent

Add `a2a-rs` as a dependency with features `http-server` and `tracing`, then:

```bash
AGENT_TOKEN=my-secret cargo run --bin agent
```

The server will start on `http://localhost:8080`. Clients can discover it via:

```
GET http://localhost:8080/.well-known/agent.json
```

And send tasks via JSON-RPC:

```json
{
  "jsonrpc": "2.0",
  "method": "tasks/send",
  "id": 1,
  "params": {
    "id": "task-001",
    "message": {
      "role": "user",
      "messageId": "msg-001",
      "parts": [{ "kind": "text", "text": "sync-project /path/to/project" }]
    }
  }
}
```

## Modifying the Agent

1. Edit `ontology/a2a-agent.ttl` — add/remove skills or change agent metadata
2. Run `ggen sync` to regenerate all three output files
3. Implement real skill logic in `generated/handlers.rs` (the stub methods return placeholder strings)

The generated `handlers.rs` is a working scaffold. To add real logic, copy it out of `generated/` into `src/` and implement the `sync_project`, `validate_ontology`, `generate_code`, and `explain_ontology` methods.
