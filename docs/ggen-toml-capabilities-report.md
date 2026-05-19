# ggen.toml Configuration Capability Report
## From Entry-Level to Fortune 5 Enterprise

**Version:** 26.5.4
**Date:** 2026-03-31
**Status:** Comprehensive Reference

---

## Executive Summary

`ggen.toml` is the single source of truth for ggen (specification-driven code generation). This report documents **all available configuration options** across the spectrum from simple starter projects to Fortune 5 enterprise deployments.

**Key Insight:** ggen.toml scales from 3 lines to 500+ lines, with each tier adding orthogonal capabilities. No tier requires capabilities from previous tiers.

---

## Tier Spectrum

| Tier | Lines | Use Case | Key Capabilities |
|------|-------|----------|------------------|
| **T0: Minimal** | 3-10 | Hello World | `project.*` only |
| **T1: Starter** | 10-50 | Personal Projects | + ontology, generation rules |
| **T2: Professional** | 50-150 | Small Teams | + AI, templates, validation |
| **T3: Team** | 150-300 | Medium Teams | + RDF/SPARQL, lifecycle, security |
| **T4: Organization** | 300-500 | Departments | + MCP, A2A, performance, logging |
| **T5: Enterprise** | 500-1000+ | Corporations | + Multi-env, marketplace, monitoring |

---

## T0: Minimal (3-10 lines)

**Target:** First-time users, learning, demos

**Required:** `project.name`, `project.version`

```toml
[project]
name = "hello-world"
version = "0.1.0"
```

**Optional additions:**
```toml
[project]
name = "hello-world"
version = "0.1.0"
description = "My first ggen project"
authors = ["Developer Name"]
license = "MIT"
repository = "https://github.com/user/repo"
```

**Use Case:** Generate a single file from hardcoded template, no ontology required.

---

## T1: Starter (10-50 lines)

**Target:** Personal projects, proof-of-concepts, hackathons

**New Capabilities:**
- Ontology-driven generation
- SPARQL extraction
- Template rendering
- Multiple output files

```toml
[project]
name = "my-api"
version = "0.1.0"
description = "REST API for personal blog"

[ontology]
source = "ontology.ttl"
base_iri = "https://example.org/my-api#"

[ontology.prefixes]
ex = "https://example.org/my-api#"
rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"

[generation]
output_dir = "src"

[[generation.rules]]
name = "main-rs"
query = { inline = """
PREFIX ex: <https://example.org/my-api#>
SELECT ?project_name WHERE {
  ?project a ex:Project ; ex:name ?project_name .
} LIMIT 1
""" }
template = { file = "templates/main.rs.tera" }
output_file = "src/main.rs"
mode = "Overwrite"

[[generation.rules]]
name = "cargo-toml"
query = { inline = """
PREFIX ex: <https://example.org/my-api#>
SELECT ?name ?version WHERE {
  ?project a ex:Project ; ex:name ?name ; ex:version ?version .
} LIMIT 1
""" }
template = { file = "templates/Cargo.toml.tera" }
output_file = "Cargo.toml"
mode = "Overwrite"
```

**Key Features:**
- Inline SPARQL queries (no separate `.rq` files needed)
- File-based templates
- Multiple generation rules
- Basic mode control (`Overwrite`, `Skip`, `Append`)

---

## T2: Professional (50-150 lines)

**Target:** Small teams, production services, SaaS MVPs

**New Capabilities:**
- AI-assisted code generation
- Template directories
- RDF/SPARQL configuration
- Lifecycle hooks
- Security controls

```toml
[project]
name = "production-api"
version = "1.0.0"
description = "Production REST API"
authors = ["Team <dev@company.com>"]
license = "MIT"

# === AI Configuration ===
[ai]
provider = "anthropic"
model = "claude-sonnet-4-20250514"
temperature = 0.3
max_tokens = 4000
timeout = 60

[ai.prompts]
system = "You are a senior Rust engineer. Generate production-ready code with comprehensive error handling."

[ai.validation]
enabled = true
quality_threshold = 0.85
max_iterations = 3

# === Templates ===
[templates]
directory = "templates"
output_directory = "src/generated"
backup_enabled = true
idempotent = true

# === RDF Configuration ===
[rdf]
base_uri = "https://company.com/production-api#"
default_format = "turtle"
cache_queries = true
store_path = ".ggen/rdf-store"

[rdf.prefixes]
company = "https://company.com/production-api#"
schema = "http://schema.org/"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
owl = "http://www.w3.org/2002/07/owl#"

# === SPARQL Configuration ===
[sparql]
timeout = 30
max_results = 5000
cache_enabled = true
cache_ttl = 3600

# === Generation Rules ===
[generation]
output_dir = "src"
enable_llm = true
llm_provider = "groq"
llm_model = "llama-3.3-70b-versatile"

[[generation.rules]]
name = "api-endpoints"
query = { file = "queries/extract-endpoints.rq" }
template = { file = "templates/api.rs.tera" }
output_file = "src/api/endpoints.rs"
mode = "Overwrite"

[[generation.rules]]
name = "database-schema"
query = { file = "queries/extract-schema.rq" }
template = { file = "templates/schema.sql.tera" }
output_file = "migrations/schema.sql"
mode = "Overwrite"

# === Lifecycle Hooks ===
[lifecycle]
enabled = true
config_file = "make.toml"
cache_directory = ".ggen/cache"

[lifecycle.phases.pre_generate]
scripts = ["scripts/validate-ontology.sh"]

[lifecycle.phases.post_generate]
scripts = ["scripts/format-code.sh", "scripts/run-tests.sh"]

# === Security ===
[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true
validate_paths = true
require_confirmation = true

# === Performance ===
[performance]
parallel_execution = true
max_workers = 4
cache_size = "500MB"

# === Logging ===
[logging]
level = "info"
format = "pretty"
file = "logs/ggen.log"
rotation = "daily"
```

**Key Features:**
- **AI Integration**: Multiple providers (Anthropic, Groq, Ollama, OpenAI)
- **Template Management**: Directory-based, backup, idempotent generation
- **RDF/SPARQL**: Persistent store, query caching, configurable prefixes
- **Lifecycle**: Pre/post hooks, cache management, state tracking
- **Security**: Path validation, sandboxing, confirmation prompts
- **Performance**: Parallel workers, memory limits, template caching
- **Logging**: Structured output, file rotation, log levels

---

## T3: Team (150-300 lines)

**Target:** Medium teams, microservices, multi-environment deployments

**New Capabilities:**
- MCP (Model Context Protocol) servers
- A2A (Agent-to-Agent) messaging
- Environment-specific overrides
- Build configuration
- Test configuration
- Feature flags

```toml
# === Project Metadata ===
[project]
name = "microservice-platform"
version = "2.0.0"
description = "Multi-service platform with AI agents"
authors = ["Platform Team <platform@company.com>"]
license = "MIT"
repository = "https://github.com/company/platform"

# === AI Configuration ===
[ai]
provider = "anthropic"
model = "claude-sonnet-4-20250514"
temperature = 0.5
max_tokens = 8000
timeout = 120

[ai.prompts]
system = "You are a platform architect. Generate microservice code with proper observability, tracing, and resilience patterns."

[ai.validation]
enabled = true
quality_threshold = 0.9
max_iterations = 5

# === Templates ===
[templates]
directory = "templates"
output_directory = "src/generated"
backup_enabled = true
idempotent = true

[templates.rust]
style = "core-team"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"

[templates.web]
framework = "axum"
database = "postgresql"
cache = "redis"
monitoring = "prometheus"

# === RDF/SPARQL ===
[rdf]
base_uri = "https://platform.company.com#"
default_format = "turtle"
cache_queries = true

[rdf.prefixes]
platform = "https://platform.company.com#"
mcp = "https://platform.company.com/mcp#"
a2a = "https://platform.company.com/a2a#"
schema = "http://schema.org/"

[sparql]
timeout = 60
max_results = 10000
cache_enabled = true
cache_ttl = 7200

# === Lifecycle ===
[lifecycle]
enabled = true
config_file = ".ggen/lifecycle.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases.pre_generate]
scripts = [
    "scripts/validate-ontology.sh",
    "scripts/check-dependencies.sh"
]

[lifecycle.phases.post_generate]
scripts = [
    "scripts/format-code.sh",
    "scripts/run-tests.sh",
    "scripts/generate-docs.sh"
]

# === Security ===
[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true
validate_paths = true
require_confirmation = false
audit_operations = true
backup_before_write = true

# === Performance ===
[performance]
parallel_execution = true
max_workers = 8
cache_size = "1GB"
enable_profiling = false
memory_limit_mb = 2048

# === Logging ===
[logging]
level = "info"
format = "json"
file = "logs/ggen.log"
rotation = "size"
max_file_size_mb = 100

# === MCP Configuration ===
[mcp]
enabled = true
name = "platform-mcp-server"
version = "1.0.0"
tool_timeout_ms = 30000
max_concurrent_requests = 100

[mcp.transport]
transport_type = "http"
host = "0.0.0.0"
port = 8080
request_timeout_seconds = 30

[mcp.transport.tls]
enabled = true
cert_path = "/etc/certs/server.pem"
key_path = "/etc/certs/server-key.pem"

[mcp.tools]
discovery_path = "tools/"
require_registration = true
validate_signatures = true
allowed_prefixes = ["platform", "database", "cache"]

[mcp.discovery]
enabled = true
method = "hybrid"
registry_url = "https://marketplace.company.com/mcp"
cache_ttl_seconds = 3600

# === A2A Configuration ===
[a2a]
enabled = true
agent_id = "platform-coordinator"
agent_name = "PlatformCoordinator"
agent_type = "coordinator"

[a2a.transport]
transport_type = "http"
bind_address = "0.0.0.0"
port = 9090
timeout_ms = 5000
max_connections = 50

[a2a.transport.retry]
max_attempts = 3
initial_delay_ms = 100
max_delay_ms = 5000
exponential_backoff = true

[a2a.messaging]
queue_size = 1000
message_ttl_seconds = 300
persistence_enabled = true
persistence_path = ".ggen/a2a/messages"
signing_enabled = true
signature_algorithm = "ed25519"

[a2a.orchestration]
mode = "hierarchical"
coordinator_address = "http://coordinator:9090"
heartbeat_interval_seconds = 30
agent_timeout_seconds = 120
consensus_enabled = false

[a2a.capabilities]
= ["query_ontology", "generate_code", "validate_tests", "deploy_service"]

# === Build Configuration ===
[build]
target = "release"
features = ["tls", "tracing", "metrics"]
profile = "optimized"
parallel_jobs = 8

# === Test Configuration ===
[test]
framework = "cargo-nextest"
parallel = true
timeout_seconds = 300
coverage_enabled = true
coverage_threshold = 80

# === Feature Flags ===
[features]
ai_generation = true
sparql_queries = true
lifecycle_management = true
template_validation = true
graph_analysis = true
documentation_generation = true
code_validation = true
test_generation = true
mcp_server = true
a2a_messaging = true
observability = true

# === Environment Overrides ===
[env.development]
"ai.model" = "claude-3-haiku-20240307"
"ai.temperature" = 0.9
"logging.level" = "debug"
"mcp.transport.port" = "3000"

[env.staging]
"ai.temperature" = 0.5
"logging.level" = "info"
"performance.max_workers" = 4
"mcp.transport.tls.enabled" = false

[env.production]
"ai.temperature" = 0.3
"logging.level" = "warn"
"performance.max_workers" = 16
"a2a.orchestration.consensus_enabled" = true
```

**Key Features:**
- **MCP Servers**: HTTP/WebSocket/stdio transport, TLS, tool discovery, ZAI integration
- **A2A Agents**: Multi-agent orchestration, message persistence, signing, consensus
- **Environments**: Development, staging, production overrides
- **Build**: Target profiles, feature flags, parallel compilation
- **Testing**: Multiple frameworks, coverage thresholds, timeouts
- **Features**: Granular feature flags for conditional compilation

---

## T4: Organization (300-500 lines)

**Target:** Departments, large teams, enterprise platforms

**New Capabilities:**
- Marketplace integration
- Package metadata
- Multi-repository coordination
- Advanced security policies
- Observability stacks
- CI/CD integration

```toml
# === Project Metadata ===
[project]
name = "enterprise-platform"
version = "3.0.0"
description = "Enterprise-grade platform with full observability"
authors = ["Platform Team <platform@company.com>"]
license = "MIT"
repository = "https://github.com/company/platform"

# === Package Metadata (Marketplace) ===
[package]
name = "enterprise-platform"
version = "3.0.0"
description = "Enterprise platform with MCP and A2A support"
authors = ["Platform Team"]
license = "MIT"
repository = "https://github.com/company/platform"
keywords = ["mcp", "a2a", "microservices", "observability"]
categories = ["platform", "enterprise", "ai"]

[package.metadata]
enterprise_tier = "premium"
support_level = "24x7"
compliance = ["SOC2", "HIPAA", "GDPR"]
sla = "99.99"

# === AI Configuration (Multi-Provider) ===
[ai]
provider = "anthropic"
model = "claude-sonnet-4-20250514"
temperature = 0.5
max_tokens = 8000
timeout = 120

[ai.prompts]
system = """You are an enterprise architect. Generate code that:
- Follows company coding standards (https://handbook.company.com/standards)
- Includes comprehensive tracing (OpenTelemetry)
- Implements circuit breakers and retries
- Uses structured logging (serde_json)
- Follows security best practices (OWASP Top 10)"""

[ai.validation]
enabled = true
quality_threshold = 0.95
max_iterations = 5

# === Templates (Multi-Language) ===
[templates]
directory = "templates"
output_directory = "src/generated"
backup_enabled = true
idempotent = true

[templates.rust]
style = "enterprise"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"
documentation = "full"

[templates.web]
framework = "axum"
database = "postgresql"
cache = "redis"
monitoring = ["prometheus", "jaeger"]
authentication = "oauth2"
rate_limiting = true

[templates.golang]
style = "standard"
logging = "zap"
tracing = "opentelemetry"
framework = "gin"

[templates.typescript]
framework = "nextjs"
runtime = "nodejs"
testing = "jest"
monitoring = "datadog"

# === RDF/SPARQL ===
[rdf]
base_uri = "https://platform.company.com#"
default_format = "turtle"
cache_queries = true
store_path = ".ggen/rdf-store"

[rdf.prefixes]
platform = "https://platform.company.com#"
mcp = "https://platform.company.com/mcp#"
a2a = "https://platform.company.com/a2a#"
enterprise = "https://company.com/enterprise#"
schema = "http://schema.org/"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
owl = "http://www.w3.org/2002/07/owl#"
xsd = "http://www.w3.org/2001/XMLSchema#"

[sparql]
timeout = 120
max_results = 50000
cache_enabled = true
cache_ttl = 86400

# === Lifecycle (Multi-Stage) ===
[lifecycle]
enabled = true
config_file = ".ggen/lifecycle.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases.pre_generate]
scripts = [
    "scripts/validate-ontology.sh",
    "scripts/check-dependencies.sh",
    "scripts/security-scan.sh",
    "scripts/compliance-check.sh"
]

[lifecycle.phases.generate]
scripts = [
    "scripts/generate-proto.sh",
    "scripts/generate-openapi.sh",
    "scripts/generate-clients.sh"
]

[lifecycle.phases.post_generate]
scripts = [
    "scripts/format-code.sh",
    "scripts/run-linter.sh",
    "scripts/run-tests.sh",
    "scripts/generate-docs.sh",
    "scripts/security-scan.sh",
    "scripts/upload-metrics.sh"
]

# === Security (Enterprise) ===
[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true
validate_paths = true
require_confirmation = false
audit_operations = true
backup_before_write = true

[security.allowed_domains]
domains = [
    "company.com",
    "github.com",
    "registry.npmjs.org",
    "crates.io"
]
max_file_size = 104857600  # 100MB
validate_ssl = true

[security.encryption]
enabled = true
algorithm = "aes-256-gcm"
key_path = ".ggen/keys/encryption.key"

[security.compliance]
soc2_enabled = true
hipaa_enabled = true
gdpr_enabled = true
audit_logging = true
retention_days = 2555  # 7 years

# === Performance (Enterprise) ===
[performance]
parallel_execution = true
max_workers = 32
cache_size = "4GB"
enable_profiling = true
memory_limit_mb = 8192

[performance.profiling]
cpu_sampling = true
memory_allocation = true
io_tracking = true
network_tracing = true

# === Logging (Structured) ===
[logging]
level = "info"
format = "json"
file = "logs/ggen.log"
rotation = "size"
max_file_size_mb = 100
max_files = 30

[logging.outputs]
stderr = true
file = true
elasticsearch = true
datadog = true

[logging.otel]
enabled = true
endpoint = "http://otel-collector:4317"
headers = { "X-Custom-Header" = "value" }

# === MCP (Enterprise) ===
[mcp]
enabled = true
name = "enterprise-mcp"
version = "2.0.0"
tool_timeout_ms = 60000
max_concurrent_requests = 1000

[mcp.transport]
transport_type = "http"
host = "0.0.0.0"
port = 8080
request_timeout_seconds = 60

[mcp.transport.tls]
enabled = true
cert_path = "/etc/certs/server.pem"
key_path = "/etc/certs/server-key.pem"
ca_path = "/etc/certs/ca.pem"
min_version = "TLSv1.2"
cipher_suites = ["TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"]

[mcp.tools]
discovery_path = "tools/"
require_registration = true
validate_signatures = true
allowed_prefixes = ["platform", "database", "cache", "auth", "billing"]
rate_limit_per_minute = 1000

[mcp.zai]
enabled = true
provider_url = "https://zai.company.com"
model = "zai-enterprise"
cache_enabled = true
cache_ttl_seconds = 3600

[mcp.discovery]
enabled = true
method = "remote"
registry_url = "https://marketplace.company.com/mcp"
cache_ttl_seconds = 7200
health_check_interval_seconds = 60

# === A2A (Enterprise) ===
[a2a]
enabled = true
agent_id = "enterprise-coordinator"
agent_name = "EnterpriseCoordinator"
agent_type = "coordinator"

[a2a.transport]
transport_type = "http"
bind_address = "0.0.0.0"
port = 9090
timeout_ms = 10000
max_connections = 200

[a2a.transport.retry]
max_attempts = 5
initial_delay_ms = 100
max_delay_ms = 30000
exponential_backoff = true

[a2a.messaging]
queue_size = 10000
message_ttl_seconds = 3600
persistence_enabled = true
persistence_path = "/var/lib/ggen/a2a/messages"
signing_enabled = true
signature_algorithm = "ed25519"
encryption_enabled = true

[a2a.orchestration]
mode = "hierarchical"
coordinator_address = "http://coordinator:9090"
heartbeat_interval_seconds = 30
agent_timeout_seconds = 300
consensus_enabled = true
consensus_algorithm = "raft"

[a2a.capabilities]
= [
    "query_ontology",
    "generate_code",
    "validate_tests",
    "deploy_service",
    "monitor_services",
    "scale_services",
    "incident_response",
    "compliance_reporting"
]

# === Build (Multi-Target) ===
[build]
target = "release"
features = ["tls", "tracing", "metrics", "jaeger", "kafka"]
profile = "enterprise"
parallel_jobs = 16

[build.docker]
enabled = true
base_image = "rust:1.81-slim"
multi_stage = true
squashfs = true

[build.ci]
enabled = true
platform = ["linux/amd64", "linux/arm64"]

# === Test (Enterprise) ===
[test]
framework = "cargo-nextest"
parallel = true
timeout_seconds = 600
coverage_enabled = true
coverage_threshold = 85

[test.integration]
framework = "integrit"
parallel = true
database = "postgresql"
redis = "redis"

[test.e2e]
framework = "cucumber"
browser = "chrome"
parallel = false

# === Marketplace ===
[marketplace]
registry_url = "https://marketplace.company.com"
cache_packages = true
verify_signatures = true
auto_update = true

# === Observability ===
[observability]
enabled = true

[observability.metrics]
prometheus = true
port = 9090
path = "/metrics"
histograms = true

[observability.tracing]
jaeger = true
endpoint = "http://jaeger:14268/api/v2/spans"
sampling_rate = 0.1

[observability.logging]
elasticsearch = true
endpoint = "http://elasticsearch:9200"
index_prefix = "ggen-"

[observability.alerting]
pagerduty = true
slack = true
email = ["platform-ops@company.com"]

# === Environment Overrides (Multi-Env) ===
[env.development]
"ai.model" = "claude-3-haiku-20240307"
"ai.temperature" = 0.9
"logging.level" = "debug"
"mcp.transport.port" = "3000"
"a2a.transport.port" = "3001"
"observability.tracing.sampling_rate" = 1.0

[env.staging]
"ai.temperature" = 0.5
"logging.level" = "info"
"performance.max_workers" = 8
"mcp.transport.tls.enabled" = true
"a2a.orchestration.consensus_enabled" = false

[env.production]
"ai.temperature" = 0.3
"logging.level" = "warn"
"performance.max_workers" = 32
"a2a.orchestration.consensus_enabled" = true
"observability.tracing.sampling_rate" = 0.01
"security.compliance.audit_logging" = true

# === CI/CD Integration ===
[ci.github_actions]
enabled = true
workflows_path = ".github/workflows"
trigger_on_push = ["main", "develop"]
trigger_on_pr = true

[ci.github_actions.jobs]
lint = true
test = true
build = true
security_scan = true
compliance_check = true
deploy_staging = true

[ci.github_actions.secrets]
CODEOWNERS_TOKEN = "${{ secrets.GITHUB_TOKEN }}"
SLACK_WEBHOOK = "${{ secrets.SLACK_WEBHOOK }}"
PAGERDUTY_TOKEN = "${{ secrets.PAGERDUTY_TOKEN }}"
```

**Key Features:**
- **Multi-Language Templates**: Rust, Go, TypeScript, Python, Java
- **Enterprise Security**: SOC2/HIPAA/GDPR compliance, encryption, audit logging
- **Advanced Observability**: Prometheus, Jaeger, Elasticsearch, PagerDuty
- **Multi-Target Builds**: Docker, multi-architecture, optimized profiles
- **Comprehensive Testing**: Unit, integration, E2E, coverage thresholds
- **Marketplace Integration**: Package discovery, signature verification, auto-update
- **CI/CD**: GitHub Actions, workflow generation, secret management

---

## T5: Fortune 5 (500-1000+ lines)

**Target:** Global corporations, multi-region deployments, regulated industries

**New Capabilities:**
- Multi-region deployment
- Disaster recovery
- Advanced compliance frameworks
- Custom marketplace publishing
- Federated MCP/A2A networks
- Real-time monitoring dashboards
- Custom extension points

```toml
# === T5: Fortune 5 Enterprise Configuration ===
# This is a comprehensive example showing ALL available options

# ============================================================================
# PROJECT METADATA
# ============================================================================
[project]
name = "global-platform"
version = "5.0.0"
description = "Global multi-region platform with Fortune 5 scale"
authors = ["Enterprise Platform Team <platform@fortune5.com>"]
license = "MIT"
repository = "https://github.com/fortune5/global-platform"

# ============================================================================
# PACKAGE METADATA (Marketplace Publishing)
# ============================================================================
[package]
name = "global-platform"
version = "5.0.0"
description = "Global enterprise platform with full MCP/A2A support"
authors = ["Enterprise Platform Team"]
license = "MIT"
repository = "https://github.com/fortune5/global-platform"
keywords = ["mcp", "a2a", "microservices", "observability", "enterprise"]
categories = ["platform", "enterprise", "ai", "multi-region"]

[package.metadata]
enterprise_tier = "fortune5"
support_level = "24x7x365"
compliance = ["SOC2", "HIPAA", "GDPR", "PCI-DSS", "ISO27001", "FedRAMP"]
sla = "99.999"
max_rpo = 5  # minutes
max_rto = 15  # minutes
disaster_recovery = true
geo_distribution = true

# ============================================================================
# AI CONFIGURATION (Multi-Provider with Fallback)
# ============================================================================
[ai]
# Primary provider
provider = "anthropic"
model = "claude-sonnet-4-20250514"
temperature = 0.3
max_tokens = 16000
timeout = 180

# Fallback providers (in order)
fallback_providers = ["openai", "groq"]

[ai.prompts]
system = """You are a Fortune 5 enterprise architect. Generate code that:
- Follows Fortune 5 coding standards (https://handbook.fortune5.com/standards)
- Implements zero-trust security principles
- Includes comprehensive observability (metrics, traces, logs)
- Implements circuit breakers, retries, and timeouts
- Uses structured logging (serde_json)
- Follows OWASP, NIST, and CIS security benchmarks
- Implements graceful degradation (fallback, cache-aside, circuit-breaker)
- Includes comprehensive documentation and examples
- Optimizes for cost (token usage) and performance (latency)"""

[ai.prompts.user_prefix]
temperature_profiles = """
- High precision (0.1-0.3): Security-critical code, cryptography, authentication
- Standard (0.4-0.6): Business logic, APIs, services
- High creativity (0.7-0.9): Brainstorming, prototyping, exploration
"""

[ai.validation]
enabled = true
quality_threshold = 0.98
max_iterations = 5
require_human_review = true
reviewer_email = "architecture@fortune5.com"

[ai.generation]
auto_improve = true
include_tests = true
include_docs = true
include_examples = true
include_benchmarks = true
include_openapi = true
include_graphql = true

# ============================================================================
# TEMPLATES (Multi-Language, Multi-Framework)
# ============================================================================
[templates]
directory = "templates"
output_directory = "src/generated"
backup_enabled = true
idempotent = true
compression = "zstd"
encryption_enabled = true

[templates.rust]
style = "enterprise"
error_handling = "thiserror"
logging = "tracing"
async_runtime = "tokio"
testing = "comprehensive"
documentation = "full"
benchmarks = "criterion"
profiling = "flamegraph"

[templates.rust.dependencies]
tokio = { version = "1.40", features = ["full", "tracing"] }
axum = { version = "0.7", features = ["tracing", "multipart"] }
sqlx = { version = "0.8", features = ["postgres", "runtime-tokio", "tls"] }
tracing = { version = "0.1", features = ["log", "flare"] }
tracing-opentelemetry = { version = "0.24", features = ["jaeger", "otlp"] }
opentelemetry = { version = "0.24", features = ["trace", "metrics", "logs"] }
prometheus = { version = "0.13", features = ["process"] }
tower = { version = "0.5", features = ["full"] }
tower-http = { version = "0.5", features = ["trace", "cors", "compression"] }

[templates.web]
framework = "axum"
database = "postgresql"
cache = "redis"
monitoring = ["prometheus", "jaeger", "tempo"]
authentication = "oauth2"
authorization = "opa"
rate_limiting = true
circuit_breaker = true
request_id = true

[templates.golang]
style = "standard"
logging = "zap"
tracing = "opentelemetry"
framework = "gin"
database = "postgres"
cache = "redis"

[templates.typescript]
framework = "nextjs"
runtime = "nodejs"
testing = "jest"
monitoring = "datadog"
state_management = "zustand"

[templates.java]
framework = "spring-boot"
version = "3.2"
build_tool = "gradle"
database = "postgresql"
cache = "redis"

[templates.python]
framework = "fastapi"
version = "3.12"
async_runtime = "asyncio"
testing = "pytest"
monitoring = "prometheus"

# ============================================================================
# RDF/SPARQL (Distributed Triple Store)
# ============================================================================
[rdf]
base_uri = "https://platform.fortune5.com#"
default_format = "turtle"
cache_queries = true
store_path = ".ggen/rdf-store"

[rdf.prefixes]
platform = "https://platform.fortune5.com#"
mcp = "https://platform.fortune5.com/mcp#"
a2a = "https://platform.fortune5.com/a2a#"
enterprise = "https://fortune5.com/enterprise#"
schema = "http://schema.org/"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"
owl = "http://www.w3.org/2002/07/owl#"
xsd = "http://www.w3.org/2001/XMLSchema#"
foaf = "http://xmlns.com/foaf/0.1/"
dct = "http://purl.org/dc/terms/"
skos = "http://www.w3.org/2004/02/skos/core#"

[rdf.distributed]
enabled = true
cluster = "rdf-cluster.fortune5.com"
port = 7200
tls = true
replication_factor = 3
consistency = "quorum"

[sparql]
timeout = 300
max_results = 100000
cache_enabled = true
cache_ttl = 86400

[sparql.federation]
enabled = true
endpoints = [
    "https://sparql.us-east.fortune5.com",
    "https://sparql.eu-west.fortune5.com",
    "https://sparql.ap-south.fortune5.com"
]
timeout_per_endpoint = 30
merge_strategy = "union"

# ============================================================================
# LIFECYCLE (Multi-Stage with Rollback)
# ============================================================================
[lifecycle]
enabled = true
config_file = ".ggen/lifecycle.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases.init]
scripts = [
    "scripts/init/validate-environment.sh",
    "scripts/init/setup-dependencies.sh",
    "scripts/init/verify-permissions.sh"
]

[lifecycle.phases.pre_validate]
scripts = [
    "scripts/validate-ontology.sh",
    "scripts/validate-templates.sh",
    "scripts/check-dependencies.sh",
    "scripts/security-scan.sh",
    "scripts/compliance-check.sh",
    "scripts/license-check.sh"
]

[lifecycle.phases.pre_generate]
scripts = [
    "scripts/backup-existing.sh",
    "scripts/verify-no-conflicts.sh",
    "scripts/notify-team.sh"
]

[lifecycle.phases.generate]
scripts = [
    "scripts/generate-proto.sh",
    "scripts/generate-openapi.sh",
    "scripts/generate-clients.sh",
    "scripts/generate-docs.sh"
]

[lifecycle.phases.post_generate]
scripts = [
    "scripts/format-code.sh",
    "scripts/run-linter.sh",
    "scripts/run-tests.sh",
    "scripts/security-scan.sh",
    "scripts/coverage-check.sh",
    "scripts/upload-metrics.sh",
    "scripts/notify-team.sh"
]

[lifecycle.phases.pre_deploy]
scripts = [
    "scripts/deploy/verify-staging.sh",
    "scripts/deploy/run-integration-tests.sh",
    "scripts/deploy/security-scan.sh",
    "scripts/deploy/compliance-check.sh",
    "scripts/deploy/approval-check.sh"
]

[lifecycle.phases.deploy]
scripts = [
    "scripts/deploy/blue-green.sh",
    "scripts/deploy/canary.sh",
    "scripts/deploy/rollback-ready.sh"
]

[lifecycle.phases.post_deploy]
scripts = [
    "scripts/deploy/smoke-tests.sh",
    "scripts/deploy/monitoring-check.sh",
    "scripts/deploy/notify-success.sh"
]

[lifecycle.phases.rollback]
scripts = [
    "scripts/deploy/rollback.sh",
    "scripts/deploy/restore-backup.sh",
    "scripts/deploy/notify-rollback.sh"
]

[lifecycle.rollbacks]
enabled = true
automatic_on_failure = true
max_rollback_attempts = 3
rollback_timeout_seconds = 300

# ============================================================================
# SECURITY (Zero Trust, Compliance)
# ============================================================================
[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true
validate_paths = true
require_confirmation = false
audit_operations = true
backup_before_write = true

[security.allowed_domains]
domains = [
    "fortune5.com",
    "github.com",
    "gitlab.fortune5.com",
    "registry.npmjs.org",
    "crates.io",
    "pkg.go.dev"
]
max_file_size = 104857600  # 100MB
validate_ssl = true

[security.encryption]
enabled = true
algorithm = "aes-256-gcm"
key_path = ".ggen/keys/encryption.key"
key_rotation_days = 90

[security.zero_trust]
enabled = true
verify_all_requests = true
require_mtls = true
allowed_certificates = [".fortune5.com"]
session_timeout_seconds = 3600

[security.compliance]
soc2_enabled = true
soc2_report_url = "https://compliance.fortune5.com/soc2"
hipaa_enabled = true
hipaa_report_url = "https://compliance.fortune5.com/hipaa"
gdpr_enabled = true
gdpr_dpo_email = "dpo@fortune5.com"
pci_dss_enabled = true
pci_dss_level = 1
iso27001_enabled = true
iso27001_cert_id = "ISO/IEC 27001:2022"
fedramp_enabled = true
fedramp_agency = "GSA"
fedramp_authorization_boundary = "FORTUNE5-HIGH"

[security.audit]
enabled = true
log_all_operations = true
log_format = "CEL"
retention_days = 2555  # 7 years
audit_trail_signing = true
immutable_audit_trail = true

# ============================================================================
# PERFORMANCE (Multi-Region, Auto-Scaling)
# ============================================================================
[performance]
parallel_execution = true
max_workers = 64
cache_size = "8GB"
enable_profiling = false
memory_limit_mb = 16384

[performance.auto_scaling]
enabled = true
min_workers = 4
max_workers = 128
scale_up_threshold = 0.7
scale_down_threshold = 0.3
scale_up_cooldown_seconds = 300
scale_down_cooldown_seconds = 600

[performance.profiling]
cpu_sampling = true
memory_allocation = true
io_tracking = true
network_tracing = true
flamegraphs = true
heap_profiles = true

[performance.caching]
enabled = true
layers = ["l1", "l2", "l3"]
default_ttl = 3600
max_size_bytes = 8000000000  # 8GB
eviction_policy = "lru"

# ============================================================================
# LOGGING (Structured, Multi-Output)
# ============================================================================
[logging]
level = "info"
format = "json"
file = "logs/ggen.log"
rotation = "size"
max_file_size_mb = 100
max_files = 30

[logging.outputs]
stderr = true
file = true
elasticsearch = true
datadog = true
s3 = true
cloudwatch = true

[logging.elasticsearch]
enabled = true
endpoint = "http://elasticsearch.fortune5.com:9200"
index_prefix = "ggen-"
username = "ggen-logger"
password_env = "ES_PASSWORD"
tls = true

[logging.datadog]
enabled = true
site = "datadog.fortune5.com"
api_key_env = "DD_API_KEY"
hostname = "ggen-platform"
tags = ["environment:production", "tier:enterprise"]

[logging.s3]
enabled = true
bucket = "ggen-logs-fortune5"
prefix = "production/"
region = "us-east-1"
retention_days = 90

[logging.otel]
enabled = true
endpoint = "http://otel-collector.fortune5.com:4317"
headers = { "X-Custom-Header" = "value" }
batch_export_max_export_batch_size = 512
batch_export_max_export_timeout_millis = 30000

# ============================================================================
# MCP (Global, Multi-Region)
# ============================================================================
[mcp]
enabled = true
name = "fortune5-mcp"
version = "5.0.0"
tool_timeout_ms = 120000
max_concurrent_requests = 10000

[mcp.transport]
transport_type = "http"
host = "0.0.0.0"
port = 8080
request_timeout_seconds = 60

[mcp.transport.tls]
enabled = true
cert_path = "/etc/certs/server.pem"
key_path = "/etc/certs/server-key.pem"
ca_path = "/etc/certs/ca.pem"
min_version = "TLSv1.3"
cipher_suites = ["TLS_AES_256_GCM_SHA384", "TLS_CHACHA20_POLY1305_SHA256"]
client_auth = "require"

[mcp.transport.multi_region]
enabled = true
regions = ["us-east-1", "eu-west-1", "ap-south-1"]
dns_round_robin = true
health_check_interval_seconds = 30
failover_timeout_seconds = 10

[mcp.tools]
discovery_path = "tools/"
require_registration = true
validate_signatures = true
allowed_prefixes = ["platform", "database", "cache", "auth", "billing", "monitoring"]
rate_limit_per_minute = 10000
tool_timeout_seconds = 300

[mcp.zai]
enabled = true
provider_url = "https://zai.fortune5.com"
model = "zai-fortune5-enterprise"
cache_enabled = true
cache_ttl_seconds = 3600
max_concurrent_requests = 100

[mcp.discovery]
enabled = true
method = "federated"
registry_urls = [
    "https://marketplace.fortune5.com/mcp",
    "https://marketplace.eu.fortune5.com/mcp",
    "https://marketplace.ap.fortune5.com/mcp"
]
cache_ttl_seconds = 3600
health_check_interval_seconds = 60
sync_interval_seconds = 300

[mcp.monitoring]
enabled = true
prometheus_port = 9090
jaeger_endpoint = "http://jaeger.fortune5.com:14268/api/v2/spans"
alerting = ["pagerduty", "slack", "email"]

# ============================================================================
# A2A (Global Federation, Consensus)
# ============================================================================
[a2a]
enabled = true
agent_id = "fortune5-coordinator"
agent_name = "Fortune5Coordinator"
agent_type = "coordinator"

[a2a.transport]
transport_type = "http"
bind_address = "0.0.0.0"
port = 9090
timeout_ms = 30000
max_connections = 1000

[a2a.transport.retry]
max_attempts = 5
initial_delay_ms = 100
max_delay_ms = 60000
exponential_backoff = true
jitter = true

[a2a.transport.multi_region]
enabled = true
regions = ["us-east-1", "eu-west-1", "ap-south-1"]
replication_enabled = true
consistency = "eventual"

[a2a.messaging]
queue_size = 100000
message_ttl_seconds = 86400
persistence_enabled = true
persistence_path = "/var/lib/ggen/a2a/messages"
signing_enabled = true
signature_algorithm = "ed25519"
encryption_enabled = true
encryption_algorithm = "aes-256-gcm"

[a2a.orchestration]
mode = "hierarchical"
coordinator_address = "http://coordinator.fortune5.com:9090"
heartbeat_interval_seconds = 30
agent_timeout_seconds = 600
consensus_enabled = true
consensus_algorithm = "raft"
raft_snapshot_interval = 1000
raft_election_timeout = 300

[a2a.capabilities]
= [
    "query_ontology",
    "generate_code",
    "validate_tests",
    "deploy_service",
    "monitor_services",
    "scale_services",
    "incident_response",
    "compliance_reporting",
    "cost_optimization",
    "performance_tuning",
    "security_audit",
    "disaster_recovery"
]

[a2a.federation]
enabled = true
domains = ["us.fortune5.com", "eu.fortune5.com", "ap.fortune5.com"]
trust_store_enabled = true
cross_domain_signing = true
allowed_domains = [".fortune5.com"]

# ============================================================================
# BUILD (Multi-Target, Multi-Platform)
# ============================================================================
[build]
target = "release"
features = ["tls", "tracing", "metrics", "jaeger", "kafka", "postgres"]
profile = "enterprise"
parallel_jobs = 32
strip = true
lto = true
codegen_units = 256

[build.docker]
enabled = true
base_image = "rust:1.81-slim"
multi_stage = true
squashfs = true
target_platform = ["linux/amd64", "linux/arm64"]

[build.ci]
enabled = true
platform = ["linux/amd64", "linux/arm64"]
cache_enabled = true
cache_url = "https://cache.fortune5.com"

[build.optimization]
profile_guided_optimization = true
link_time_optimization = true
inter_procedural_optimization = true
devirtualization = "full"
incremental = true

# ============================================================================
# TEST (Comprehensive Suite)
# ============================================================================
[test]
framework = "cargo-nextest"
parallel = true
timeout_seconds = 900
coverage_enabled = true
coverage_threshold = 90

[test.integration]
framework = "integrit"
parallel = true
database = "postgresql"
redis = "redis"
kafka = true

[test.e2e]
framework = "cucumber"
browser = "chrome"
parallel = false
screenshot_on_failure = true
video_recording = true

[test.performance]
framework = "criterion"
baseline = "benchmarks/baseline.json"
regression_threshold = 0.05
measurement_time = 60
sample_size = 100

[test.property]
framework = "proptest"
cases = 1000
max_failures = 10
shrinking_time = 60

[test.fuzzing]
framework = "libfuzzer"
max_duration_seconds = 300
max_input_size = 1048576
fork_server = true

# ============================================================================
# MARKETPLACE (Publishing)
# ============================================================================
[marketplace]
registry_url = "https://marketplace.fortune5.com"
cache_packages = true
verify_signatures = true
auto_update = false

[marketplace.publishing]
enabled = true
verify_before_publish = true
run_tests = true
generate_docs = true
create_github_release = true
sign_packages = true
changelog_generation = true

[marketplace.promotion]
featured = false
sponsored = false
tags = ["enterprise", "production-ready", "fortune5"]

# ============================================================================
# OBSERVABILITY (Full Stack)
# ============================================================================
[observability]
enabled = true

[observability.metrics]
prometheus = true
port = 9090
path = "/metrics"
histograms = true
summary = true
native = true

[observability.tracing]
jaeger = true
endpoint = "http://jaeger.fortune5.com:14268/api/v2/spans"
sampling_rate = 0.1
sampling_strategy = "probabilistic"

[observability.logging]
elasticsearch = true
endpoint = "http://elasticsearch.fortune5.com:9200"
index_prefix = "ggen-"

[observability.alerting]
pagerduty = true
slack = true
email = ["platform-ops@fortune5.com", "architecture@fortune5.com"]
opsgenie = true
victorops = true

[observability.dashboards]
grafana = "https://grafana.fortune5.com/d/global-platform"
sentry = "https://sentry.fortune5.com/d/global-platform"
datadog = "https://app.datadoghq.com/dashboard/global-platform"

# ============================================================================
# ENVIRONMENT OVERRITES (Multi-Region, Multi-Stage)
# ============================================================================
[env.development]
"ai.model" = "claude-3-haiku-20240307"
"ai.temperature" = 0.9
"logging.level" = "debug"
"mcp.transport.port" = "3000"
"a2a.transport.port" = "3001"
"observability.tracing.sampling_rate" = 1.0
"performance.max_workers" = 8
"test.timeout_seconds" = 300

[env.staging]
"ai.temperature" = 0.5
"logging.level" = "info"
"performance.max_workers" = 16
"mcp.transport.tls.enabled" = true
"a2a.orchestration.consensus_enabled" = false
"observability.tracing.sampling_rate" = 0.5
"security.compliance.audit_logging" = false

[env.production]
"ai.temperature" = 0.3
"logging.level" = "warn"
"performance.max_workers" = 64
"a2a.orchestration.consensus_enabled" = true
"observability.tracing.sampling_rate" = 0.01
"security.compliance.audit_logging" = true
"performance.auto_scaling.enabled" = true
"performance.auto_scaling.max_workers" = 128

[env.us-east-1]
"rdf.distributed.cluster" = "rdf-cluster.us-east-1.fortune5.com"
"mcp.transport.port" = 8080
"a2a.transport.port" = 9090
"observability.logging.elasticsearch.endpoint" = "http://elasticsearch.us-east-1.fortune5.com:9200"

[env.eu-west-1]
"rdf.distributed.cluster" = "rdf-cluster.eu-west-1.fortune5.com"
"mcp.transport.port" = 8080
"a2a.transport.port" = 9090
"observability.logging.elasticsearch.endpoint" = "http://elasticsearch.eu-west-1.fortune5.com:9200"

[env.ap-south-1]
"rdf.distributed.cluster" = "rdf-cluster.ap-south-1.fortune5.com"
"mcp.transport.port" = 8080
"a2a.transport.port" = 9090
"observability.logging.elasticsearch.endpoint" = "http://elasticsearch.ap-south-1.fortune5.com:9200"

# ============================================================================
# CI/CD INTEGRATION
# ============================================================================
[ci.github_actions]
enabled = true
workflows_path = ".github/workflows"
trigger_on_push = ["main", "develop"]
trigger_on_pr = true
trigger_on_schedule = [{ cron = "0 0 * * *" }]

[ci.github_actions.jobs]
lint = true
test = true
test_integration = true
test_e2e = true
build = true
build_docker = true
security_scan = true
compliance_check = true
performance_test = true
deploy_staging = true
deploy_production = true

[ci.github_actions.secrets]
CODEOWNERS_TOKEN = "${{ secrets.GITHUB_TOKEN }}"
SLACK_WEBHOOK = "${{ secrets.SLACK_WEBHOOK }}"
PAGERDUTY_TOKEN = "${{ secrets.PAGERDUTY_TOKEN }}"
OPSGENIE_TOKEN = "${{ secrets.OPSGENIE_TOKEN }}"
DATADOG_API_KEY = "${{ secrets.DATADOG_API_KEY }}"
S3_ACCESS_KEY = "${{ secrets.S3_ACCESS_KEY }}"
S3_SECRET_KEY = "${{ secrets.S3_SECRET_KEY }}"

[ci.gitlab_ci]
enabled = true
stages = ["validate", "test", "build", "deploy", "monitor"]
trigger_on_push = ["main", "develop"]
trigger_on_mr = true

[ci.circleci]
enabled = true
orchestration = "parallel"
workflows = ["test", "build", "deploy"]

# ============================================================================
# DISASTER RECOVERY
# ============================================================================
[disaster_recovery]
enabled = true

[disaster_recovery.backup]
enabled = true
schedule = "0 2 * * *"  # Daily at 2 AM
retention_days = 90
storage = "s3://ggen-backups-fortune5"
encryption = true
encryption_key_env = "BACKUP_ENCRYPTION_KEY"

[disaster_recovery.replication]
enabled = true
regions = ["us-east-1", "eu-west-1", "ap-south-1"]
replication_factor = 3
consistency = "strong"

[disaster_recovery.failover]
enabled = true
health_check_interval_seconds = 30
failover_timeout_seconds = 10
automatic_failover = true
manual_override = false

[disaster_recovery.dr]
enabled = true
primary_region = "us-east-1"
dr_region = "eu-west-1
rpo_seconds = 300
rto_seconds = 900
data_replication = "synchronous"

# ============================================================================
# CUSTOM EXTENSIONS
# ============================================================================
[extensions]
custom_validators = ["scripts/validators/custom.sh"]
custom_templates = ["templates/custom/"]
custom_generators = ["scripts/generators/custom.py"]
custom_metrics = ["scripts/metrics/custom.sh"]

[extensions.webhooks]
pre_generate = ["https://webhooks.fortune5.com/pre-generate"]
post_generate = ["https://webhooks.fortune5.com/post-generate"]
on_failure = ["https://webhooks.fortune5.com/failure"]

[extensions.plugins]
load_path = [".ggen/plugins"]
enabled_plugins = ["cost-analyzer", "security-scanner", "performance-profiler"]
```

**Key Features:**
- **Multi-Region**: US, EU, AP deployments with failover
- **Disaster Recovery**: Automated backup, replication, DR orchestration
- **Advanced Compliance**: SOC2, HIPAA, GDPR, PCI-DSS, ISO27001, FedRAMP
- **Full Observability**: Prometheus, Jaeger, Elasticsearch, Datadog, Grafana
- **CI/CD**: GitHub Actions, GitLab CI, CircleCI with secrets management
- **Marketplace Publishing**: Package verification, signing, promotion
- **Custom Extensions**: Validators, generators, metrics, webhooks, plugins

---

## Complete Configuration Reference

### Section: `[project]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `name` | string | ✅ | - | Project identifier |
| `version` | string | ✅ | - | SemVer version |
| `description` | string | ❌ | - | Human-readable description |
| `authors` | array[string] | ❌ | - | List of authors |
| `license` | string | ❌ | - | SPDX license identifier |
| `repository` | string | ❌ | - | Git repository URL |

### Section: `[ai]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `provider` | string | ✅ | - | AI provider (anthropic, openai, groq, ollama, zai) |
| `model` | string | ✅ | - | Model name |
| `temperature` | float | ❌ | 0.7 | Sampling temperature (0.0-1.0) |
| `max_tokens` | integer | ❌ | 2000 | Max tokens per request |
| `timeout` | integer | ❌ | 30 | Request timeout (seconds) |
| `prompts` | object | ❌ | - | System/user prompt configuration |
| `validation` | object | ❌ | - | Quality validation settings |

### Section: `[templates]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `directory` | string | ❌ | "templates" | Template source directory |
| `output_directory` | string | ❌ | "src/generated" | Generated files output |
| `backup_enabled` | boolean | ❌ | false | Backup before overwriting |
| `idempotent` | boolean | ❌ | false | Skip unchanged files |

### Section: `[rdf]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `base_uri` | string | ❌ | - | Base URI for RDF entities |
| `default_format` | string | ❌ | "turtle" | RDF serialization format |
| `cache_queries` | boolean | ❌ | true | Cache SPARQL queries |
| `store_path` | string | ❌ | - | RDF store persistence path |
| `prefixes` | object | ❌ | - | RDF namespace prefixes |

### Section: `[mcp]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `enabled` | boolean | ❌ | false | Enable MCP server |
| `name` | string | ❌ | - | Server name |
| `version` | string | ❌ | - | Server version |
| `tool_timeout_ms` | integer | ❌ | 30000 | Tool execution timeout |
| `max_concurrent_requests` | integer | ❌ | 100 | Max concurrent requests |
| `transport` | object | ❌ | - | Transport configuration |
| `tools` | object | ❌ | - | Tool registry |
| `zai` | object | ❌ | - | ZAI integration |
| `discovery` | object | ❌ | - | Server discovery |

### Section: `[a2a]`

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `enabled` | boolean | ❌ | false | Enable A2A agent |
| `agent_id` | string | ❌ | - | Unique agent identifier |
| `agent_name` | string | ❌ | - | Human-readable name |
| `agent_type` | string | ❌ | - | Agent type |
| `transport` | object | ❌ | - | Transport configuration |
| `messaging` | object | ❌ | - | Message handling |
| `orchestration` | object | ❌ | - | Orchestration settings |
| `capabilities` | array[string] | ❌ | - | Agent capabilities |

---

## Configuration Best Practices

### 1. Start Small, Scale Up
Begin with T0/T1, add tiers as complexity grows. Each tier is orthogonal.

### 2. Environment-Specific Overrides
Use `[env.*]` sections for development/staging/production differences.

### 3. Security First
Always enable security protections in production. Use `[security]` controls.

### 4. Observability from Day One
Enable logging and metrics early. Add tracing as scale increases.

### 5. Version Control All Configuration
Commit `ggen.toml` to version control. Track changes over time.

### 6. Validate Early
Use `[lifecycle.phases.pre_validate]` to catch configuration errors.

### 7. Document Custom Sections
Use comments to explain non-standard configuration.

---

## Migration Path: T0 → T5

### From T0 to T1
1. Add `[ontology]` section
2. Define first `[[generation.rules]]`
3. Create initial templates

### From T1 to T2
1. Add `[ai]` section with provider
2. Add `[templates]` directory configuration
3. Add `[lifecycle]` hooks
4. Add `[security]` controls

### From T2 to T3
1. Add `[mcp]` server configuration
2. Add `[a2a]` agent configuration
3. Add `[env.*]` environment overrides
4. Add `[build]` and `[test]` sections

### From T3 to T4
1. Add `[package]` marketplace metadata
2. Add `[observability]` full stack
3. Add `[ci.*]` CI/CD integration
4. Add `[performance]` optimization

### From T4 to T5
1. Add `[security.compliance.*]` frameworks
2. Add `[disaster_recovery]` configuration
3. Add multi-region `[env.*]` sections
4. Add `[extensions]` for custom behavior

---

## Appendix: Complete Type Definitions

See `/crates/ggen-config/src/schema.rs` for full Rust struct definitions.

---

**Document Version:** 1.0
**Last Updated:** 2026-03-31
**Maintained By:** Platform Team <platform@fortune5.com>
