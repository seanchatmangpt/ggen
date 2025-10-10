# Recipe: Wiring a Module with Idempotent Injection

**Time:** 25 minutes
**Difficulty:** Advanced
**Patterns:** Idempotent generation, dependency injection, module wiring, graph relationships

## What You'll Build

A complete module dependency injection system that:
- Generates module initialization code from graph relationships
- Automatically wires dependencies without circular references
- Handles lifecycle management (startup, shutdown, health checks)
- Regenerates cleanly without breaking existing wiring
- Supports adding/removing modules without manual configuration

## Prerequisites

- Completed [API Endpoint Generator](./api_endpoint_generator.md)
- Understanding of dependency injection patterns
- Familiarity with async Rust (or your chosen language)

## The Problem

As applications grow:
- Modules depend on each other (database → cache → auth → API)
- Initialization order matters (database before API)
- Configuration gets duplicated and inconsistent
- Adding a new module means updating multiple files
- Circular dependencies are easy to create accidentally

Let's solve this by modeling modules and their dependencies as a graph, then generating all wiring code.

## The Recipe

### Step 1: Define Module Dependency Graph

Create `data/modules.ttl`:

```turtle
@prefix mod: <http://example.org/modules#> .
@prefix dep: <http://example.org/dependencies#> .

# Base infrastructure modules
mod:config
  a mod:Module ;
  mod:name "config" ;
  mod:type "Configuration" ;
  mod:init_priority 1 ;
  mod:description "Application configuration" ;
  mod:provides [
    mod:interface "ConfigProvider" ;
    mod:method "get_database_url" ;
    mod:method "get_redis_url" ;
    mod:method "get_jwt_secret"
  ] ;
  mod:lifecycle [
    mod:startup "load_from_env" ;
    mod:health_check "validate_required_fields"
  ] .

mod:database
  a mod:Module ;
  mod:name "database" ;
  mod:type "Database" ;
  mod:init_priority 2 ;
  mod:description "PostgreSQL connection pool" ;
  mod:depends_on mod:config ;
  mod:provides [
    mod:interface "DatabasePool" ;
    mod:method "query" ;
    mod:method "execute" ;
    mod:method "transaction"
  ] ;
  mod:lifecycle [
    mod:startup "connect_pool" ;
    mod:shutdown "close_connections" ;
    mod:health_check "ping_database"
  ] .

mod:cache
  a mod:Module ;
  mod:name "cache" ;
  mod:type "Cache" ;
  mod:init_priority 3 ;
  mod:description "Redis cache layer" ;
  mod:depends_on mod:config ;
  mod:provides [
    mod:interface "CacheProvider" ;
    mod:method "get" ;
    mod:method "set" ;
    mod:method "delete" ;
    mod:method "exists"
  ] ;
  mod:lifecycle [
    mod:startup "connect_redis" ;
    mod:shutdown "flush_and_disconnect" ;
    mod:health_check "ping_redis"
  ] .

# Application services
mod:auth
  a mod:Module ;
  mod:name "auth" ;
  mod:type "AuthService" ;
  mod:init_priority 4 ;
  mod:description "Authentication and authorization" ;
  mod:depends_on mod:database ;
  mod:depends_on mod:cache ;
  mod:depends_on mod:config ;
  mod:provides [
    mod:interface "AuthProvider" ;
    mod:method "authenticate" ;
    mod:method "authorize" ;
    mod:method "generate_token" ;
    mod:method "validate_token"
  ] ;
  mod:lifecycle [
    mod:startup "initialize_jwt_keys" ;
    mod:health_check "verify_signing_keys"
  ] .

mod:user_service
  a mod:Module ;
  mod:name "user_service" ;
  mod:type "UserService" ;
  mod:init_priority 5 ;
  mod:description "User management service" ;
  mod:depends_on mod:database ;
  mod:depends_on mod:cache ;
  mod:depends_on mod:auth ;
  mod:provides [
    mod:interface "UserProvider" ;
    mod:method "create_user" ;
    mod:method "get_user" ;
    mod:method "update_user" ;
    mod:method "delete_user"
  ] ;
  mod:lifecycle [
    mod:startup "warm_user_cache" ;
    mod:health_check "verify_cache_sync"
  ] .

# API layer
mod:api
  a mod:Module ;
  mod:name "api" ;
  mod:type "ApiServer" ;
  mod:init_priority 6 ;
  mod:description "REST API server" ;
  mod:depends_on mod:auth ;
  mod:depends_on mod:user_service ;
  mod:depends_on mod:config ;
  mod:provides [
    mod:interface "HttpServer" ;
    mod:method "start" ;
    mod:method "stop"
  ] ;
  mod:lifecycle [
    mod:startup "bind_routes" ;
    mod:shutdown "graceful_shutdown" ;
    mod:health_check "check_listener"
  ] .

# Monitoring
mod:metrics
  a mod:Module ;
  mod:name "metrics" ;
  mod:type "MetricsCollector" ;
  mod:init_priority 7 ;
  mod:description "Prometheus metrics" ;
  mod:depends_on mod:api ;
  mod:provides [
    mod:interface "MetricsProvider" ;
    mod:method "record_metric" ;
    mod:method "export_metrics"
  ] ;
  mod:lifecycle [
    mod:startup "register_metrics" ;
    mod:health_check "verify_prometheus_endpoint"
  ] .
```

### Step 2: Create Module Initialization Template

Create `templates/module_init.tmpl`:

```handlebars
//! Module Initialization and Dependency Injection
//!
//! Generated: {{timestamp}}
//! DO NOT EDIT: This file is fully regenerated
//!
//! Initialization order (by priority):
{{#each modules}}
//!   {{init_priority}}. {{name}} - {{description}}
{{/each}}

use std::sync::Arc;
use anyhow::Result;
use tokio::sync::RwLock;
use tracing::{info, warn, error};

// Module imports
{{#each modules}}
use crate::modules::{{name}}::{{type}};
{{/each}}

/// Application state container
#[derive(Clone)]
pub struct AppState {
{{#each modules}}
    pub {{name}}: Arc<{{type}}>,
{{/each}}
}

impl AppState {
    /// Initialize all modules in dependency order
    pub async fn new() -> Result<Self> {
        info!("🚀 Initializing application modules...");

{{#each modules_sorted_by_priority}}
        // Initialize: {{name}} (priority {{init_priority}})
        info!("  📦 Initializing {{name}}...");
        let {{name}} = {{type}}::new(
{{#each dependencies}}
            {{this}}.clone(),
{{/each}}
        ).await?;

{{#if startup}}
        // Startup hook: {{startup}}
        {{name}}.{{startup}}().await?;
        info!("  ✅ {{name}} initialized");
{{/if}}

{{/each}}

        Ok(Self {
{{#each modules}}
            {{name}}: Arc::new({{name}}),
{{/each}}
        })
    }

    /// Shutdown all modules in reverse dependency order
    pub async fn shutdown(&self) -> Result<()> {
        info!("🛑 Shutting down application modules...");

{{#each modules_sorted_by_priority_reverse}}
{{#if shutdown}}
        // Shutdown: {{name}}
        info!("  Shutting down {{name}}...");
        self.{{name}}.{{shutdown}}().await?;
{{/if}}
{{/each}}

        info!("✅ All modules shut down successfully");
        Ok(())
    }

    /// Run health checks on all modules
    pub async fn health_check(&self) -> Result<HealthStatus> {
        let mut status = HealthStatus::new();

{{#each modules}}
{{#if health_check}}
        // Health check: {{name}}
        match self.{{name}}.{{health_check}}().await {
            Ok(_) => status.add_healthy("{{name}}"),
            Err(e) => status.add_unhealthy("{{name}}", e),
        }
{{/if}}
{{/each}}

        Ok(status)
    }
}

#[derive(Debug)]
pub struct HealthStatus {
    pub healthy: Vec<String>,
    pub unhealthy: Vec<(String, String)>,
}

impl HealthStatus {
    fn new() -> Self {
        Self {
            healthy: Vec::new(),
            unhealthy: Vec::new(),
        }
    }

    fn add_healthy(&mut self, module: &str) {
        self.healthy.push(module.to_string());
    }

    fn add_unhealthy(&mut self, module: &str, error: anyhow::Error) {
        self.unhealthy.push((module.to_string(), error.to_string()));
    }

    pub fn is_healthy(&self) -> bool {
        self.unhealthy.is_empty()
    }
}
```

### Step 3: Create Module Interface Template

Create `templates/module_interface.tmpl`:

```handlebars
{{#each modules}}
//! {{description}}
//! Generated: {{timestamp}}

use async_trait::async_trait;
use std::sync::Arc;
use anyhow::Result;

{{#if provides}}
#[async_trait]
pub trait {{interface}} {
{{#each methods}}
    async fn {{this}}(&self) -> Result<()>;
{{/each}}
}
{{/if}}

pub struct {{type}} {
{{#each dependencies}}
    {{this}}: Arc<{{lookup ../modules this 'type'}}>,
{{/each}}
    // <<<FREEZE_START:{{name}}_fields>>>
    // Add custom fields here
    // <<<FREEZE_END:{{name}}_fields>>>
}

impl {{type}} {
    pub async fn new(
{{#each dependencies}}
        {{this}}: Arc<{{lookup ../modules this 'type'}}>,
{{/each}}
    ) -> Result<Self> {
        Ok(Self {
{{#each dependencies}}
            {{this}},
{{/each}}
            // <<<FREEZE_START:{{name}}_init>>>
            // Initialize custom fields
            // <<<FREEZE_END:{{name}}_init>>>
        })
    }

{{#if startup}}
    // <<<FREEZE_START:{{name}}_startup>>>
    pub async fn {{startup}}(&self) -> Result<()> {
        // TODO: Implement startup logic
        Ok(())
    }
    // <<<FREEZE_END:{{name}}_startup>>>
{{/if}}

{{#if shutdown}}
    // <<<FREEZE_START:{{name}}_shutdown>>>
    pub async fn {{shutdown}}(&self) -> Result<()> {
        // TODO: Implement shutdown logic
        Ok(())
    }
    // <<<FREEZE_END:{{name}}_shutdown>>>
{{/if}}

{{#if health_check}}
    // <<<FREEZE_START:{{name}}_health_check>>>
    pub async fn {{health_check}}(&self) -> Result<()> {
        // TODO: Implement health check
        Ok(())
    }
    // <<<FREEZE_END:{{name}}_health_check>>>
{{/if}}
}

{{#if provides}}
#[async_trait]
impl {{interface}} for {{type}} {
{{#each methods}}
    // <<<FREEZE_START:{{../name}}_{{this}}>>>
    async fn {{this}}(&self) -> Result<()> {
        // TODO: Implement {{this}}
        Ok(())
    }
    // <<<FREEZE_END:{{../name}}_{{this}}>>>
{{/each}}
}
{{/if}}

{{/each}}
```

### Step 4: Create Dependency Graph Validator

Create `templates/dependency_validator.tmpl`:

```handlebars
//! Dependency Graph Validation
//! Generated: {{timestamp}}

use std::collections::{HashMap, HashSet};

pub struct DependencyGraph {
    modules: HashMap<String, Vec<String>>,
}

impl DependencyGraph {
    pub fn new() -> Self {
        let mut modules = HashMap::new();

{{#each modules}}
        modules.insert(
            "{{name}}".to_string(),
            vec![
{{#each dependencies}}
                "{{this}}".to_string(),
{{/each}}
            ]
        );
{{/each}}

        Self { modules }
    }

    /// Detect circular dependencies
    pub fn validate(&self) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        for (module, _) in &self.modules {
            let mut visited = HashSet::new();
            let mut path = Vec::new();

            if let Some(cycle) = self.find_cycle(module, &mut visited, &mut path) {
                errors.push(format!("Circular dependency detected: {}", cycle.join(" -> ")));
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn find_cycle(
        &self,
        module: &str,
        visited: &mut HashSet<String>,
        path: &mut Vec<String>,
    ) -> Option<Vec<String>> {
        if path.contains(&module.to_string()) {
            let cycle_start = path.iter().position(|m| m == module).unwrap();
            let mut cycle = path[cycle_start..].to_vec();
            cycle.push(module.to_string());
            return Some(cycle);
        }

        if visited.contains(module) {
            return None;
        }

        visited.insert(module.to_string());
        path.push(module.to_string());

        if let Some(deps) = self.modules.get(module) {
            for dep in deps {
                if let Some(cycle) = self.find_cycle(dep, visited, path) {
                    return Some(cycle);
                }
            }
        }

        path.pop();
        None
    }

    /// Get initialization order (topological sort)
    pub fn init_order(&self) -> Vec<String> {
        let mut result = Vec::new();
        let mut visited = HashSet::new();

        for module in self.modules.keys() {
            self.visit(module, &mut visited, &mut result);
        }

        result
    }

    fn visit(&self, module: &str, visited: &mut HashSet<String>, result: &mut Vec<String>) {
        if visited.contains(module) {
            return;
        }

        visited.insert(module.to_string());

        if let Some(deps) = self.modules.get(module) {
            for dep in deps {
                self.visit(dep, visited, result);
            }
        }

        result.push(module.to_string());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_circular_dependencies() {
        let graph = DependencyGraph::new();
        assert!(graph.validate().is_ok(), "Dependency graph has circular dependencies");
    }

    #[test]
    fn test_initialization_order() {
        let graph = DependencyGraph::new();
        let order = graph.init_order();

        // Verify dependencies come before dependents
        let positions: HashMap<_, _> = order.iter()
            .enumerate()
            .map(|(i, m)| (m.clone(), i))
            .collect();

        for (module, deps) in &graph.modules {
            let module_pos = positions[module];
            for dep in deps {
                let dep_pos = positions[dep];
                assert!(
                    dep_pos < module_pos,
                    "Dependency {} should come before {} in init order",
                    dep,
                    module
                );
            }
        }
    }
}
```

### Step 5: Generate All Module Code

Create `scripts/generate_modules.sh`:

```bash
#!/bin/bash
set -e

DATA="data/modules.ttl"

echo "🔍 Validating dependency graph..."
ggen exec \
  --template templates/dependency_validator.tmpl \
  --data "$DATA" \
  --output src/dependency_graph.rs

# Run validation
cargo test --test dependency_graph || {
    echo "❌ Circular dependencies detected!"
    exit 1
}

echo "✅ Dependency graph valid"
echo ""

echo "🏗️  Generating module initialization..."
ggen exec \
  --template templates/module_init.tmpl \
  --data "$DATA" \
  --output src/app_state.rs

echo "📦 Generating module interfaces..."
ggen exec \
  --template templates/module_interface.tmpl \
  --data "$DATA" \
  --output src/modules/mod.rs \
  --freeze

echo "✅ Module generation complete!"
```

Run it:

```bash
chmod +x scripts/generate_modules.sh
./scripts/generate_modules.sh
```

### Step 6: Implement Module Logic

Edit `src/modules/mod.rs` and implement custom logic in freeze blocks:

```rust
// Find the database module's startup freeze block
// <<<FREEZE_START:database_startup>>>
pub async fn connect_pool(&self) -> Result<()> {
    use sqlx::postgres::PgPoolOptions;

    let db_url = self.config.get_database_url()?;

    let pool = PgPoolOptions::new()
        .max_connections(10)
        .connect(&db_url)
        .await?;

    // Store pool in self (add field in fields freeze block)
    *self.pool.write().await = Some(pool);

    info!("✅ Database pool connected");
    Ok(())
}
// <<<FREEZE_END:database_startup>>>

// <<<FREEZE_START:database_health_check>>>
pub async fn ping_database(&self) -> Result<()> {
    let pool = self.pool.read().await;
    let pool = pool.as_ref().ok_or_else(|| anyhow!("Pool not initialized"))?;

    sqlx::query("SELECT 1")
        .execute(pool)
        .await?;

    Ok(())
}
// <<<FREEZE_END:database_health_check>>>
```

### Step 7: Use AppState in Your Application

Create `src/main.rs`:

```rust
mod app_state;
mod modules;
mod dependency_graph;

use app_state::AppState;
use tracing_subscriber;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt::init();

    // Initialize all modules in dependency order
    let app_state = AppState::new().await?;

    // Run health checks
    let health = app_state.health_check().await?;
    if !health.is_healthy() {
        eprintln!("❌ Health check failed:");
        for (module, error) in health.unhealthy {
            eprintln!("  - {}: {}", module, error);
        }
        return Err(anyhow::anyhow!("Unhealthy modules detected"));
    }

    println!("✅ All modules healthy");

    // Start API server
    app_state.api.start().await?;

    // Wait for shutdown signal
    tokio::signal::ctrl_c().await?;

    // Graceful shutdown in reverse order
    app_state.shutdown().await?;

    Ok(())
}
```

### Step 8: Add a New Module and Regenerate

Add to `data/modules.ttl`:

```turtle
mod:email
  a mod:Module ;
  mod:name "email" ;
  mod:type "EmailService" ;
  mod:init_priority 4 ;
  mod:description "Email sending service" ;
  mod:depends_on mod:config ;
  mod:provides [
    mod:interface "EmailProvider" ;
    mod:method "send_email" ;
    mod:method "send_template"
  ] ;
  mod:lifecycle [
    mod:startup "connect_smtp" ;
    mod:shutdown "close_smtp_connection" ;
    mod:health_check "verify_smtp_connection"
  ] .

# Update user_service to depend on email
mod:user_service
  mod:depends_on mod:email .
```

Regenerate:

```bash
./scripts/generate_modules.sh
```

**Result:**
- New `EmailService` module added
- Initialization order automatically adjusted
- `UserService` now receives `email` dependency
- All existing implementations preserved
- Dependency graph validation passes

## What's Happening?

### Dependency Resolution

1. **Graph modeling**: Modules and dependencies are explicit in RDF
2. **Topological sort**: GGen queries the graph to determine initialization order
3. **Priority override**: `init_priority` can override dependency order if needed
4. **Cycle detection**: Generated validator prevents circular dependencies

### Idempotent Wiring

The same graph generates:
- Module initialization code (fully regenerated)
- Module interfaces (with freeze blocks for implementations)
- Dependency validators (fully regenerated)
- Health check orchestration (fully regenerated)

Adding/removing modules or dependencies regenerates cleanly without breaking existing code.

### The SPARQL Query (Auto-generated)

```sparql
SELECT ?name ?type ?init_priority ?description
       (GROUP_CONCAT(DISTINCT ?dep; separator=",") as ?dependencies)
       ?startup ?shutdown ?health_check
WHERE {
  ?module a mod:Module ;
          mod:name ?name ;
          mod:type ?type ;
          mod:init_priority ?init_priority ;
          mod:description ?description .

  OPTIONAL { ?module mod:depends_on ?dep }
  OPTIONAL { ?module mod:lifecycle/mod:startup ?startup }
  OPTIONAL { ?module mod:lifecycle/mod:shutdown ?shutdown }
  OPTIONAL { ?module mod:lifecycle/mod:health_check ?health_check }
}
GROUP BY ?name ?type ?init_priority ?description ?startup ?shutdown ?health_check
ORDER BY ?init_priority
```

## Advanced Patterns

### 1. Feature Flags

```turtle
mod:analytics
  a mod:Module ;
  mod:name "analytics" ;
  mod:enabled_by_feature "analytics" ;
  mod:depends_on mod:config .
```

Template:

```handlebars
{{#if enabled_by_feature}}
#[cfg(feature = "{{enabled_by_feature}}")]
{{/if}}
pub struct {{type}} {
    // ...
}
```

### 2. Environment-Specific Modules

```turtle
mod:dev_tools
  a mod:Module ;
  mod:name "dev_tools" ;
  mod:environment "development" ;
  mod:init_priority 10 .
```

Template:

```handlebars
{{#if_env environment "development"}}
    let {{name}} = {{type}}::new().await?;
{{/if_env}}
```

### 3. Lazy Initialization

```turtle
mod:expensive_service
  mod:init_strategy "lazy" ;
  mod:init_on_first_use true .
```

Generate lazy wrappers:

```handlebars
{{#if init_strategy_lazy}}
pub struct {{type}}Lazy {
    inner: Arc<RwLock<Option<{{type}}>>>,
}

impl {{type}}Lazy {
    pub async fn get_or_init(&self) -> Result<Arc<{{type}}>> {
        // Initialize on first access
    }
}
{{/if}}
```

## Testing the Module System

### Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_module_initialization_order() {
        let app_state = AppState::new().await.unwrap();

        // Verify dependencies are initialized before dependents
        // (database should be initialized before user_service)
        assert!(app_state.database.is_connected().await);
        assert!(app_state.user_service.is_ready().await);
    }

    #[tokio::test]
    async fn test_health_checks() {
        let app_state = AppState::new().await.unwrap();
        let health = app_state.health_check().await.unwrap();

        assert!(health.is_healthy(), "Health check failed: {:?}", health.unhealthy);
    }

    #[tokio::test]
    async fn test_graceful_shutdown() {
        let app_state = AppState::new().await.unwrap();

        // Should shutdown in reverse order without panics
        app_state.shutdown().await.unwrap();
    }
}
```

### Integration Tests

```rust
#[tokio::test]
async fn test_full_application_lifecycle() {
    // Initialize
    let app_state = AppState::new().await.unwrap();

    // Use modules
    let user = app_state.user_service
        .create_user("test@example.com", "password")
        .await
        .unwrap();

    // Verify dependencies work
    assert!(app_state.cache.exists(&format!("user:{}", user.id)).await.unwrap());

    // Shutdown
    app_state.shutdown().await.unwrap();
}
```

## Troubleshooting

### Circular Dependency Detected

**Problem:** Validation test fails with circular dependency error

**Solution:** Examine the cycle and break it:

```bash
# Run validation to see the cycle
cargo test dependency_graph -- --nocapture

# Output: Circular dependency detected: auth -> user_service -> auth
```

Fix by introducing an interface:

```turtle
# Instead of direct dependency, use interface
mod:auth
  mod:depends_on mod:user_repository_interface .

mod:user_service
  mod:implements mod:user_repository_interface .
```

### Modules Initialize in Wrong Order

**Problem:** Module tries to use dependency before it's ready

**Solution:** Check priorities and dependencies:

```turtle
# Ensure dependent has higher priority
mod:database
  mod:init_priority 2 .

mod:user_service
  mod:init_priority 5 ;  # Higher = later
  mod:depends_on mod:database .
```

### Module Not Found After Regeneration

**Problem:** Compilation error: module not found

**Solution:** Verify module file structure matches names:

```bash
# Generated modules should match graph names
src/modules/
  ├── config.rs      # mod:config
  ├── database.rs    # mod:database
  ├── auth.rs        # mod:auth
  └── mod.rs         # Generated module declarations
```

## Performance Considerations

### Parallel Initialization

Modify template for parallel init where possible:

```handlebars
// Modules with same priority can initialize in parallel
let (config, metrics) = tokio::join!(
    {{type}}::new(),
    MetricsCollector::new(),
);
```

### Connection Pooling

```turtle
mod:database
  mod:config_param [
    mod:param "max_connections" ;
    mod:default "10"
  ] .
```

### Lazy Loading

Only initialize heavy modules when needed:

```rust
// <<<FREEZE_START:expensive_module_init>>>
pub async fn init_if_needed(&self) -> Result<()> {
    if self.inner.read().await.is_none() {
        let instance = HeavyModule::new().await?;
        *self.inner.write().await = Some(instance);
    }
    Ok(())
}
// <<<FREEZE_END:expensive_module_init>>>
```

## Next Steps

- **Configuration management**: Generate config loaders from schema ([Knowledge Hooks](./docs_sync_hook.md))
- **Service discovery**: Add module registry for dynamic service location
- **Distributed systems**: Extend to microservices with RPC generation
- **Monitoring**: Add distributed tracing hooks to all module boundaries

## Related Patterns

- [Dependency Injection](../patterns/dependency_injection.md)
- [Idempotent Generation](../patterns/idempotent_generation.md)
- [Graph Relationships](../patterns/graph_relationships.md)
- [Lifecycle Management](../patterns/lifecycle_patterns.md)

---

**Success checkpoint:** You should have:
1. ✅ Modules initializing in correct dependency order
2. ✅ No circular dependencies (validated)
3. ✅ Health checks running on all modules
4. ✅ Graceful shutdown in reverse order
5. ✅ New modules integrate seamlessly
6. ✅ Custom implementations preserved on regeneration

If modules aren't initializing, check the dependency graph validator output and verify your `init_priority` values follow dependency order.
