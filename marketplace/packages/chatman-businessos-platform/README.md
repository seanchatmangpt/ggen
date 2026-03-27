# ChatmanGPT BusinessOS Platform — ggen Package

**Fortune 500-grade code generation from ontology.**

Regenerate the entire BusinessOS platform (Go services, Docker images, Kubernetes manifests) from a single ontology source.

## Quick Start

```bash
# 1. Clone the marketplace package
ggen get chatman-businessos-platform

# 2. Run the sync pipeline
ggen sync --spec ontology/businessos.ttl --output generated/

# 3. View generated code
cat generated/services/OrderService.go
cat generated/docker/OrderService/Dockerfile
cat generated/k8s/OrderService-deployment.yaml
```

## Installation

The package is distributed via ggen marketplace registry:

```bash
# Install from registry
ggen install chatman-businessos-platform

# Or clone directly from git
git clone https://github.com/seanchatmangpt/chatmangpt.git
cd ggen/marketplace/packages/chatman-businessos-platform
ggen sync --spec ontology/businessos.ttl --output /tmp/generated-businessos/
```

## What This Package Does

### Input
- **Ontology:** `ontology/businessos.ttl` — Linked to `/Users/sac/chatmangpt/.specify/specs/020-platform-ontologies/businessos.ttl`
  - Services (OrderService, InvoiceService, ComplianceEngine, WebhookHandler)
  - Entities (Order, Invoice, Compliance Rules, Webhooks)
  - Endpoints (REST APIs, gRPC, WebSocket)
  - Dependencies (PostgreSQL, Redis, compliance frameworks)
  - Metadata (port mappings, language, framework, async flags)

### Process: The ggen Sync Pipeline

1. **Extraction (SPARQL)**
   - `queries/extract-services.rq` executes against the ontology
   - Extracts all Service definitions with metadata
   - Returns: list of `{ service, label, port, language, protocol, framework, async, dependencies, compliance_rules }`

2. **Emission (Tera Templates)**
   - `templates/service.go.tera` → generates Go service code
   - `templates/Dockerfile.tera` → generates multi-stage Docker images
   - `templates/k8s-deployment.yaml.tera` → generates Kubernetes manifests (with ConfigMap, ServiceAccount, RBAC)

3. **Output**
   - `generated/services/*.go` — Go service implementations
   - `generated/docker/**/Dockerfile` — Production-ready Docker builds
   - `generated/k8s/*-deployment.yaml` — Complete K8s setup (Deployment, Service, ConfigMap, ServiceAccount, Role, RoleBinding)

### Output

```
generated/
├── services/
│   ├── OrderService.go
│   ├── InvoiceService.go
│   ├── ComplianceEngine.go
│   └── WebhookHandler.go
├── docker/
│   ├── OrderService/
│   │   └── Dockerfile
│   ├── InvoiceService/
│   │   └── Dockerfile
│   ├── ComplianceEngine/
│   │   └── Dockerfile
│   └── WebhookHandler/
│       └── Dockerfile
└── k8s/
    ├── OrderService-deployment.yaml
    ├── InvoiceService-deployment.yaml
    ├── ComplianceEngine-deployment.yaml
    └── WebhookHandler-deployment.yaml
```

## Usage

### Basic Sync (Regenerate Everything)

```bash
ggen sync --spec ontology/businessos.ttl --output generated/
```

### Sync with Rules (Filter Services)

Generate only Go services that are in production:

```bash
ggen sync \
  --spec ontology/businessos.ttl \
  --output generated/ \
  --rules production-services
```

### Sync with Custom Output

```bash
ggen sync \
  --spec ontology/businessos.ttl \
  --output /tmp/businessos-generated/ \
  --verbose
```

### Generate Specific Templates

Only generate Docker images (skip Go services and K8s manifests):

```bash
ggen sync \
  --spec ontology/businessos.ttl \
  --output generated/ \
  --templates dockerfile
```

## Pipeline Steps

### Step 1: Load Ontology

```bash
# Parse businessos.ttl
# Validate against RDF schema
# Load prefixes: bos, schema, rdf, rdfs
```

### Step 2: Execute SPARQL Extraction

```sparql
# queries/extract-services.rq
SELECT ?service ?label ?port ?language ?protocol ?framework
WHERE {
  ?service a bos:Service .
  ?service rdfs:label ?label .
  ?service bos:port ?port .
  ...
}
```

**Output:** JSON result set with 4 services:
```json
{
  "results": {
    "bindings": [
      {
        "service": "OrderService",
        "label": "Order API",
        "port": "8001",
        "language": "Go",
        "protocol": "HTTP",
        "framework": "Gin"
      },
      ...
    ]
  }
}
```

### Step 3: Render Tera Templates

For each result binding:

1. **service.go.tera** → `services/OrderService.go`
   - Contains: struct definition, handlers, lifecycle methods, health checks
   - Pattern: `package {{ service_name }}`, `type Service struct`

2. **Dockerfile.tera** → `docker/OrderService/Dockerfile`
   - Multi-stage build (golang:1.24 → alpine:latest)
   - Optimized for production (non-root user, health checks, minimal image)

3. **k8s-deployment.yaml.tera** → `k8s/OrderService-deployment.yaml`
   - Full Kubernetes manifest (Deployment, Service, ConfigMap, RBAC)
   - Liveness/readiness probes, resource limits, security context

### Step 4: Validate Output

```bash
# Check Go syntax
go vet ./generated/services/

# Validate Dockerfile (docker lint)
hadolint generated/docker/*/Dockerfile

# Validate K8s manifests
kubectl apply -f generated/k8s/ --dry-run=client
```

## Generated Artifacts

### Go Services

**File:** `services/OrderService.go`

```go
package order_service

import "github.com/gin-gonic/gin"

// Service represents the Order API service
type Service struct {
    Port     int
    Protocol string
    Router   *gin.Engine
}

// NewOrderService creates a new Order API service
func NewOrderService(cfg Config) *Service { ... }

// Start starts the service
func (s *Service) Start() error { ... }

// Health returns service health
func (s *Service) Health(c *gin.Context) { ... }

// Ready returns readiness status
func (s *Service) Ready(c *gin.Context) { ... }
```

### Docker Images

**File:** `docker/OrderService/Dockerfile`

```dockerfile
# Multi-stage build
FROM golang:1.24-alpine as builder
  WORKDIR /app
  COPY . .
  RUN go build -o bin/order-service ./cmd/order-service/main.go

FROM alpine:latest
  COPY --from=builder /app/bin/order-service ./bin/order-service
  EXPOSE 8001
  HEALTHCHECK --interval=30s CMD curl -f http://localhost:8001/health
  ENTRYPOINT ["./bin/order-service"]
```

### Kubernetes Manifests

**File:** `k8s/OrderService-deployment.yaml`

Includes:
- **Deployment:** 3 replicas, rolling updates, anti-affinity
- **Service:** ClusterIP for internal communication
- **ConfigMap:** Service configuration
- **ServiceAccount:** For RBAC
- **Role & RoleBinding:** Fine-grained permissions
- **Probes:** startup, liveness, readiness
- **Security:** non-root user, read-only filesystem, capability dropping

## Customization

### Modify the Ontology

Edit `ontology/businessos.ttl` to add/remove services:

```turtle
:MyNewService a bos:Service ;
    rdfs:label "My New API" ;
    bos:port 8010 ;
    bos:protocol "HTTP" ;
    bos:language "Go" ;
    bos:framework "Gin" ;
    bos:dependency [ a bos:Dependency ; bos:service "PostgreSQL" ] .
```

Then regenerate:
```bash
ggen sync --spec ontology/businessos.ttl --output generated/
```

### Customize Templates

Edit template files in `templates/`:
- `service.go.tera` — Go service structure
- `Dockerfile.tera` — Docker build process
- `k8s-deployment.yaml.tera` — K8s manifests

### Add New Templates

1. Create `templates/my-template.tera`
2. Update `ggen.toml` to reference it in `[[templates.patterns]]`
3. Rerun `ggen sync`

Example: Add GraphQL schema generator:

```tera
# templates/schema.graphql.tera
type {{ service_name | pascal_case }} {
    id: ID!
    name: String!
    port: Int!
}
```

```toml
# ggen.toml
[[templates.patterns]]
name = "graphql-schema"
template = "schema.graphql.tera"
output = "generated/graphql/{{service_name}}.graphql"
```

## Configuration

See `ggen.toml`:

```toml
[project]
name = "chatman-businessos-platform"
version = "1.0.0"

[ontology]
source = "ontology/businessos.ttl"
base_uri = "https://chatmangpt.com/businessos/"

[v6.passes]
extraction = { order = 1, type = "sparql", source = "queries/extract-services.rq" }
emission = { order = 2, type = "tera", source = "templates/" }

[rules]
# Go services only
[[rules]]
name = "go-services-only"
condition = 'language == "Go"'
templates = ["service.go.tera", "Dockerfile.tera"]

# Production services (get K8s manifests)
[[rules]]
name = "production-services"
condition = 'service_tier == "production"'
templates = ["k8s-deployment.yaml.tera"]
```

## SPARQL Query Reference

### Extract Services

```sparql
PREFIX bos: <https://chatmangpt.com/businessos#>
SELECT ?service ?label ?port ?language
WHERE {
  ?service a bos:Service .
  ?service rdfs:label ?label .
  ?service bos:port ?port .
  ?service bos:language ?language .
}
```

### Extract Entities

See `queries/extract-entities.rq` for data model extraction.

### Extract Endpoints

See `queries/extract-endpoints.rq` for REST/gRPC endpoint extraction.

## Troubleshooting

### Error: `ontology/businessos.ttl not found`

The ontology is symlinked to the actual BusinessOS ontology. Ensure the link is valid:

```bash
ls -la ontology/businessos.ttl
# Should show: ontology/businessos.ttl -> /Users/sac/chatmangpt/.specify/specs/020-platform-ontologies/businessos.ttl
```

### Error: SPARQL query returns no results

1. Verify the ontology has Service definitions:
   ```bash
   grep "a bos:Service" ontology/businessos.ttl
   ```

2. Check the SPARQL syntax:
   ```bash
   # Validate with Apache Jena or similar
   sparql --query queries/extract-services.rq --data ontology/businessos.ttl
   ```

### Generated Go code has syntax errors

Check that template variables are populated:
- `{{ service_name }}` should be lowercase snake_case
- `{{ port }}` should be an integer
- `{{ language }}` should be "Go", "Elixir", etc.

Run with `--verbose`:
```bash
ggen sync --spec ontology/businessos.ttl --output generated/ --verbose
```

## Integration with CI/CD

### GitHub Actions

```yaml
name: Regenerate BusinessOS Platform
on: [push, pull_request]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo install ggen
      - run: ggen sync --spec ontology/businessos.ttl --output generated/
      - run: go vet ./generated/services/
      - run: kubectl apply -f generated/k8s/ --dry-run=client
      - uses: git-commit-action@v1
        with:
          message: "chore: regenerate BusinessOS platform"
          pattern: "generated/**"
```

## Contributing

1. Fork the marketplace package
2. Modify ontology or templates
3. Run `ggen sync` to test
4. Submit PR to https://github.com/seanchatmangpt/chatmangpt

## License

MIT — See LICENSE file

## Support

- Documentation: https://chatmangpt.com/docs
- Issues: https://github.com/seanchatmangpt/chatmangpt/issues
- Email: info@chatmangpt.com

---

**Generated by ggen from BusinessOS ontology.**
**Last updated: 2026-03-26**
