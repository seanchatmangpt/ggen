# ggen Examples

All 30 examples run with `ggen sync` — verified 2026-03-28.

## Quick Start

```bash
cd examples/<name>
ggen sync        # generates files into output/ or generated/
```

## Learning Path

### Beginner
| Example | What it generates | Key concept |
|---------|------------------|-------------|
| [simple-project](simple-project/) | Cargo.toml, README, CI workflow | Minimal ggen.toml structure |
| [basic-template-generation](basic-template-generation/) | Rust modules, structs, docs | Templates + SPARQL queries |
| [rust-structs](rust-structs/) | Rust struct files | Struct-focused generation |
| [config-generator](config-generator/) | Config files | Configuration generation |

### Intermediate — API & Schema
| Example | What it generates | Key concept |
|---------|------------------|-------------|
| [openapi](openapi/) | OpenAPI 3.0, TypeScript, Zod schemas | Single ontology → 3 synchronized artifacts |
| [openapi-variants](openapi-variants/) | OpenAPI variations | Multiple output formats |
| [rest-api-advanced](rest-api-advanced/) | REST API contracts | Advanced SPARQL queries |
| [graphql-schema](graphql-schema/) | GraphQL schema | Schema-first generation |
| [grpc-service](grpc-service/) | gRPC proto definitions | Protocol buffer generation |
| [database-schema](database-schema/) | Database migration files | Schema generation |
| [validation-schemas](validation-schemas/) | JSON Schema validation | Schema validation |

### Intermediate — Application
| Example | What it generates | Key concept |
|---------|------------------|-------------|
| [cli-noun-verb](cli-noun-verb/) | OpenAPI + TypeScript + type guards | Noun-verb CLI with golden output |
| [middleware-stack](middleware-stack/) | Middleware configuration | Stack composition |
| [microservices-architecture](microservices-architecture/) | Service definitions | Multi-service generation |
| [workspace-project](workspace-project/) | Workspace README + crate manifest | Multi-crate workspace |

### Advanced
| Example | What it generates | Key concept |
|---------|------------------|-------------|
| [advanced-rust-project](advanced-rust-project/) | Rust services, endpoints, docs | Full Rust project generation |
| [comprehensive-rust-showcase](comprehensive-rust-showcase/) | Entity docs, README | Feature showcase |
| [complete-project-generation](complete-project-generation/) | Workspace Cargo.toml + crate configs | Workspace scaffolding |
| [nextjs-openapi-sqlite-shadcn-vitest](nextjs-openapi-sqlite-shadcn-vitest/) | Next.js components, API types | Full-stack generation |
| [thesis-gen](thesis-gen/) | Complete LaTeX PhD thesis (50+ pages) | Complex multi-rule pipeline |

### Specialized
| Example | What it generates | Key concept |
|---------|------------------|-------------|
| [ai-template-creation](ai-template-creation/) | AI workflow templates | AI-assisted generation |
| [ai-code-generation](ai-code-generation/) | Code gen specs | AI code generation patterns |
| [ai-microservice](ai-microservice/) | Dockerfile, README | AI microservice scaffolding |
| [factory-paas](factory-paas/) | DDD domain model (Rust entities, events, commands) | TCPS/DDD reference implementation |
| [gcp-erlang-autonomics](gcp-erlang-autonomics/) | C4 diagrams, Kubernetes manifests | Architecture visualization |
| [yawl-workflow-platform](yawl-workflow-platform/) | Workflow handlers, REST API, receipt types | YAWL workflow generation |

### Protocol Integration (Weaver · MCP · A2A)
| Example | What it generates | Key concept |
|---------|------------------|-------------|
| [weaver-semantic-conventions](weaver-semantic-conventions/) | Weaver registry YAML, Rust telemetry constants, live-check script | Define OTel semantic conventions in RDF |
| [mcp-server-definition](mcp-server-definition/) | rmcp 1.3.0 Rust server with `#[tool]` methods, Cargo.toml fragment | MCP server tools from RDF |
| [a2a-agent-definition](a2a-agent-definition/) | A2A agent entry point, skill handlers, agent-card.json | A2A skills from RDF |
| [observable-agent](observable-agent/) | Weaver YAML + MCP server + A2A agent + docker-compose from one ontology | Single RDF → three protocols |

## Common Patterns

### Query format
```toml
[[generation.rules]]
name = "my-rule"
query = { inline = """
  PREFIX ex: <https://example.org/>
  SELECT ?name ?description
  WHERE { ?item a ex:Item ; ex:name ?name ; ex:description ?description }
  ORDER BY ?name
""" }
template = { file = "templates/my-template.tera" }
output_file = "generated/{{ name }}.rs"
mode = "Overwrite"
```

### Template variable access (no `?` prefix)
```tera
{% for row in sparql_results %}
Name: {{ row["name"] }}
Description: {{ row["description"] }}
{% endfor %}
```

### Ontology section
```toml
[ontology]
source = "ontology/main.ttl"
imports = ["ontology/schema.ttl"]
base_iri = "https://example.org/ontology#"

[ontology.prefixes]
ex = "https://example.org/"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
```

## Notes

- All examples write to `generated/` or `output/`
- `thesis-gen` and `openapi` have committed generated output as reference
- `cli-noun-verb` has a `golden/` directory with expected outputs for comparison
- Run `ggen sync --audit` for a full audit trail in `.ggen/audit/`
