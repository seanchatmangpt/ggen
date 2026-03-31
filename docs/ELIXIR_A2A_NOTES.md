# Elixir A2A Generator — Usage Guide

ggen generates Elixir A2A boilerplate from RDF agent definitions using the
[`a2a ~> 0.2`](https://hex.pm/packages/a2a) hex library (by Action Card AB).

## Prerequisites

Add to your Elixir project's `mix.exs`:

```elixir
defp deps do
  [
    {:a2a,    "~> 0.2"},
    {:plug,   "~> 1.16"},
    {:bandit, "~> 1.5"},   # HTTP server (or use cowboy)
    {:req,    "~> 0.5"}    # For A2A.Client HTTP calls
  ]
end
```

## Define agents in RDF

Create or extend your `ontology.ttl`:

```turtle
@prefix a2a: <https://ggen.dev/a2a#> .
@prefix :    <https://myapp.example.com/> .

:InvoiceAgent a a2a:Agent ;
  a2a:name        "invoice-agent" ;
  a2a:description "Handles invoice queries and approval workflows" ;
  a2a:version     "1.0" ;
  a2a:elixirApp   "MyApp" ;      # Elixir module prefix (default: MyApp)
  a2a:urlPath     "invoice" ;    # URL path segment (default: agentName)
  a2a:hasSkill [
    a a2a:Skill ;
    a2a:name        "query-invoice" ;
    a2a:description "Look up invoice status by ID"
  ] .
```

## Run ggen

```bash
ggen sync --manifest ggen.toml
```

Generated files (in `crates/elixir-a2a-generated/lib/`):

| File | Contents |
|------|----------|
| `a2a_agents.ex` | One `defmodule` per agent with `use A2A.Agent` + `handle_message/2` stub |
| `a2a_router.ex` | `Plug.Router` with one `forward` per agent |
| `a2a_supervisor.ex` | `A2A.AgentSupervisor` wrapper + ExUnit test stubs |

## Wire into Phoenix

```elixir
# lib/my_app_web/router.ex
scope "/api" do
  forward "/agents", MyAppWeb.A2ARouter
end

# lib/my_app/application.ex
children = [
  MyApp.A2ASupervisor,
  MyAppWeb.Endpoint
]
```

## Implement domain logic

Edit the `handle_message/2` stub in `a2a_agents.ex`:

```elixir
@impl A2A.Agent
def handle_message(%A2A.Message{} = message, context) do
  case A2A.Message.text(message) do
    "status " <> invoice_id ->
      {:reply, [A2A.Part.Text.new("Invoice #{invoice_id}: #{Invoices.status(invoice_id)}")]}

    _ ->
      {:input_required, [A2A.Part.Text.new("Send: status <invoice_id>")]}
  end
end
```

## A2A.Agent return values

| Return | Task transition |
|--------|----------------|
| `{:reply, [parts]}` | → `:completed` |
| `{:input_required, [parts]}` | → `:input_required` (paused) |
| `{:stream, enumerable}` | → `:working` → `:completed` |
| `{:error, reason}` | → `:failed` |

**Multi-turn:** resume with `A2A.call(agent, msg, task_id: task.id)` — the agent receives full `history` in `context`.

## Endpoints exposed by `A2A.Plug`

| Method | Path | Purpose |
|--------|------|---------|
| `GET` | `/.well-known/agent-card.json` | Agent discovery |
| `POST` | `/` | JSON-RPC 2.0 `message/send` |
| `POST` | `/` + `stream: true` | SSE streaming response |

## RDF template variables

| RDF property | Tera variable | Default |
|---|---|---|
| `a2a:name` | `agent.agentName` | — (required) |
| `a2a:description` | `agent.description` | agentName |
| `a2a:version` | `agent.version` | `"1.0"` |
| `a2a:elixirApp` | `agent.appModule` | `"MyApp"` |
| `a2a:urlPath` | `agent.path` | agentName |
| `a2a:hasSkill/a2a:name` | `agent.skills` (CSV) | `""` |

`router_module` and `supervisor_module` can be set as top-level context variables in `ggen.toml` generation rules.

## ggen.toml rules added

```toml
{ name = "elixir-a2a-agents",     ... → a2a_agents.ex     }
{ name = "elixir-a2a-router",     ... → a2a_router.ex     }
{ name = "elixir-a2a-supervisor", ... → a2a_supervisor.ex }
```

All three use `crates/ggen-core/queries/elixir-a2a/extract-agents.rq` as the SPARQL source.

## Calling remote A2A agents

```elixir
# Discover + call any A2A-compatible agent by URL
url = "https://other-service.com/agents/invoice"
task = A2A.Client.message(url, "status INV-001", method: :send)
# %A2A.Task{status: :completed, ...}
```
