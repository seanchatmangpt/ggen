# Template Integration Examples

Complete examples showing template system integration with CLI, marketplace, and lifecycle.

## Example 1: Rust Microservice from Marketplace

```bash
# Search for Rust microservice templates
ggen market search "rust microservice axum"

# Output:
# 📦 Found 3 template packages:
#   1. rust-axum-microservice (⭐ 4.8, 1.2k downloads)
#      Production-ready Axum microservice with PostgreSQL
#   2. rust-actix-service (⭐ 4.5, 890 downloads)
#      Actix-web microservice template
#   3. rust-minimal-service (⭐ 4.2, 650 downloads)
#      Minimal microservice template

# Install the template
ggen market add "rust-axum-microservice"

# Generate service
ggen template generate-tree \
  --template rust-axum-microservice:full-service.yaml \
  --output ./user-service \
  --var service_name=user-service \
  --var port=8080 \
  --var db_name=users \
  --var auth_enabled=true

# Output:
# 📦 Generating file tree from template: rust-axum-microservice:full-service.yaml
# 📝 Generating files...
# ✅ Successfully generated 23 files
# 📂 Output directory: ./user-service
# 📊 RDF metadata: ./user-service/.ggen/metadata.ttl

# Navigate and build
cd user-service
ggen lifecycle run build
ggen lifecycle run test
```

## Example 2: Multi-Service Platform

```bash
#!/bin/bash
# generate-platform.sh

# Define services
SERVICES=(
  "user-service:8080:users"
  "order-service:8081:orders"
  "payment-service:8082:payments"
  "notification-service:8083:notifications"
)

# Generate each service
for service_def in "${SERVICES[@]}"; do
  IFS=':' read -r name port db <<< "$service_def"

  echo "Generating $name..."
  ggen template generate-tree \
    --template rust-axum-microservice:service.yaml \
    --output "./services/$name" \
    --var service_name="$name" \
    --var port="$port" \
    --var db_name="$db" \
    --var metrics_enabled=true \
    --var tracing_enabled=true
done

# Generate API gateway
ggen template generate-tree \
  --template api-gateway:nginx.yaml \
  --output ./gateway \
  --var services="user,order,payment,notification" \
  --var ports="8080,8081,8082,8083"

# Generate infrastructure
ggen template generate-tree \
  --template docker-compose:platform.yaml \
  --output . \
  --var services="user,order,payment,notification,gateway"

echo "✅ Platform generated successfully!"
echo "📂 Structure:"
tree -L 2 .
```

## Example 3: Lifecycle Integration

Create `make.toml`:

```toml
[project]
name = "my-platform"
version = "1.0.0"

[project.variables]
environment = "${ENV:-development}"
service_count = "4"

[[phases]]
name = "scaffold"
description = "Generate initial project structure"
commands = [
    '''
    ggen template generate-tree \
      --template platform-base:scaffold.yaml \
      --var project_name=${PROJECT_NAME} \
      --var environment=${environment}
    '''
]

[[phases]]
name = "gen-services"
description = "Generate microservices"
depends_on = ["scaffold"]
commands = [
    '''
    for service in user order payment notification; do
      ggen template generate-tree \
        --template microservice:service.yaml \
        --output ./services/$service \
        --var service_name=$service
    done
    '''
]

[[phases]]
name = "gen-infra"
description = "Generate infrastructure code"
depends_on = ["gen-services"]
commands = [
    "ggen template generate-tree --template docker:compose.yaml --output .",
    "ggen template generate-tree --template k8s:manifests.yaml --output ./k8s"
]

[[phases]]
name = "validate"
description = "Validate generated code"
depends_on = ["gen-infra"]
commands = [
    "ggen lifecycle validate --env ${environment}",
    "docker-compose config --quiet",
    "kubectl --dry-run=client apply -f k8s/"
]

[[phases]]
name = "build"
description = "Build all services"
depends_on = ["validate"]
parallel = true
commands = [
    "cd services/user-service && cargo build --release",
    "cd services/order-service && cargo build --release",
    "cd services/payment-service && cargo build --release",
    "cd services/notification-service && cargo build --release"
]

[[phases]]
name = "test"
description = "Run tests"
depends_on = ["build"]
parallel = true
commands = [
    "cd services/user-service && cargo test",
    "cd services/order-service && cargo test",
    "cd services/payment-service && cargo test",
    "cd services/notification-service && cargo test"
]

[[phases]]
name = "deploy"
description = "Deploy to environment"
depends_on = ["test"]
commands = [
    '''
    if [ "${environment}" = "production" ]; then
      kubectl apply -f k8s/
    else
      docker-compose up -d
    fi
    '''
]

[phases.hooks]
after_scaffold = [
    "git init",
    "git add .",
    "git commit -m 'Initial commit from template'"
]
after_gen_services = [
    "cargo fmt --all",
    "cargo clippy --all -- -D warnings"
]
```

Run the workflow:

```bash
# Set environment
export PROJECT_NAME="my-platform"
export ENV="development"

# Run complete pipeline
ggen lifecycle run deploy

# Or run specific phases
ggen lifecycle run scaffold
ggen lifecycle run gen-services
ggen lifecycle run gen-infra
```

## Example 4: Interactive Template Creation

```bash
# Create custom template interactively
ggen template generate-tree \
  --template custom-service:interactive.yaml \
  --output ./my-custom-service \
  --interactive

# Prompts:
# 🔧 Interactive variable collection
# Press Enter to use default values (if available)
#
#   service_name: my-auth-service
#   port [8080]: 9000
#   database_type (postgres/mysql/mongodb) [postgres]: postgres
#   auth_provider (jwt/oauth2/saml) [jwt]: oauth2
#   enable_metrics (true/false) [true]: true
#   enable_tracing (true/false) [true]: true
#   log_level (debug/info/warn/error) [info]: debug
#
# 📝 Generating files...
# ✅ Successfully generated 31 files
```

## Example 5: Template with Post-Generation Hooks

Template file `service-with-hooks.yaml`:

```yaml
name: "service-with-automation"
description: "Service with automated setup"

variables:
  - name: service_name
    required: true
  - name: port
    default: "8080"
  - name: db_enabled
    default: "true"

nodes:
  - name: "{{service_name}}"
    type: directory
    children:
      - name: "src"
        type: directory
        children:
          - name: "main.rs"
            type: file
            content: |
              // {{service_name}}
              fn main() {
                  println!("Starting on port {{port}}");
              }
      - name: "Cargo.toml"
        type: file
        content: |
          [package]
          name = "{{service_name}}"
          version = "0.1.0"

# Post-generation hooks
post_hooks:
  - "cargo fmt"
  - "cargo clippy --fix --allow-dirty --allow-staged"
  - "cargo build"
  - "cargo test"
  - "git init"
  - "git add ."
  - "git commit -m 'Initial commit: {{service_name}}'"
  - |
    cat > README.md <<EOF
    # {{service_name}}

    Generated on $(date)
    Port: {{port}}
    Database: {{db_enabled}}
    EOF
```

Generate with hooks:

```bash
ggen template generate-tree \
  --template service-with-hooks.yaml \
  --output ./automated-service \
  --var service_name=my-service \
  --var port=9000

# Hooks automatically:
# ✓ Format code
# ✓ Run clippy
# ✓ Build project
# ✓ Run tests
# ✓ Initialize git
# ✓ Create initial commit
# ✓ Generate README
```

## Example 6: Template Search and Discovery

```bash
# Search with filters
ggen market search "microservice" \
  --category web-service \
  --framework axum \
  --min-rating 4.0

# Browse by category
ggen market categories

# List popular templates
ggen market templates --popular --limit 10

# List recently updated
ggen market templates --recent --limit 10

# Get template details
ggen market show rust-axum-microservice

# Output:
# 📦 rust-axum-microservice
# Version: 2.1.0
# Category: microservice
# Rating: ⭐ 4.8 (342 ratings)
# Downloads: 1,247
#
# Description:
# Production-ready Axum microservice template with:
# - PostgreSQL integration
# - JWT authentication
# - OpenTelemetry tracing
# - Metrics (Prometheus)
# - Health checks
# - Docker support
#
# Templates:
#   - full-service.yaml (Complete service)
#   - minimal-service.yaml (Minimal setup)
#   - api-only.yaml (API without database)
#
# Variables:
#   - service_name (required)
#   - port (default: 8080)
#   - db_name (required if db_enabled=true)
#   - auth_enabled (default: true)
#
# Examples:
#   ggen template generate-tree \
#     --template rust-axum-microservice:full-service.yaml \
#     --var service_name=my-service
```

## Example 7: Configuration-Driven Generation

Create `.ggen/template-config.toml`:

```toml
[search_paths]
paths = [
    "templates",
    ".ggen/templates",
    "~/.ggen/global-templates"
]

[default_variables]
author = "Platform Team"
license = "MIT"
rust_edition = "2021"
company = "Acme Corp"

[generation]
auto_format = true
run_hooks = true
validate_before_gen = true

[marketplace]
enabled = true
auto_update = true
trusted_sources = [
    "ggen-official",
    "community-verified",
    "acme-internal"
]
```

Generate using defaults:

```bash
# Variables from config are automatically applied
ggen template generate-tree \
  --template microservice:service.yaml \
  --var service_name=my-service
  # author, license, company automatically filled in
```

## Example 8: CI/CD Integration

`.github/workflows/generate-and-deploy.yml`:

```yaml
name: Generate and Deploy

on:
  workflow_dispatch:
    inputs:
      service_name:
        description: 'Service name'
        required: true
      port:
        description: 'Port number'
        required: true
        default: '8080'

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: cargo install ggen

      - name: Install template
        run: ggen market add rust-microservice-template

      - name: Generate service
        run: |
          ggen template generate-tree \
            --template rust-microservice-template:service.yaml \
            --output ./services/${{ github.event.inputs.service_name }} \
            --var service_name=${{ github.event.inputs.service_name }} \
            --var port=${{ github.event.inputs.port }}

      - name: Validate
        run: |
          cd services/${{ github.event.inputs.service_name }}
          ggen lifecycle validate

      - name: Build
        run: |
          cd services/${{ github.event.inputs.service_name }}
          ggen lifecycle run build

      - name: Test
        run: |
          cd services/${{ github.event.inputs.service_name }}
          ggen lifecycle run test

      - name: Deploy
        run: |
          cd services/${{ github.event.inputs.service_name }}
          ggen lifecycle run deploy --env production
```

## Example 9: Custom Template Repository

```bash
# Set up custom template repository
mkdir -p ~/.ggen/custom-templates

# Create custom template
cat > ~/.ggen/custom-templates/company-service.yaml <<EOF
name: "company-standard-service"
description: "Company standard microservice"

variables:
  - name: service_name
    required: true
  - name: team
    required: true
  - name: port
    default: "8080"

nodes:
  - name: "{{service_name}}"
    type: directory
    children:
      - name: "src"
        type: directory
        children:
          - name: "main.rs"
            type: file
            content: |
              // {{service_name}}
              // Team: {{team}}
              // Port: {{port}}

              fn main() {
                  println!("Company Standard Service");
              }
EOF

# Use custom template
ggen template generate-tree \
  --template ~/.ggen/custom-templates/company-service.yaml \
  --output ./my-service \
  --var service_name=auth-service \
  --var team=platform
```

## Summary

The template integration provides:

1. **CLI Integration**: `ggen template generate-tree` command
2. **Marketplace Integration**: Search, install, and use template packages
3. **Lifecycle Integration**: Templates as part of build/deploy pipelines
4. **Configuration**: Centralized template settings
5. **Automation**: Post-generation hooks and validation
6. **RDF Metadata**: Automatic tracking of generated files

For more information:
- [Template Creation Guide](./template-creation-guide.md)
- [Lifecycle Documentation](./lifecycle.md)
- [Marketplace Guide](./marketplace.md)
