# Pattern 016: Hybrid Files (Once Tags)

## Intent
Generate file sections exactly once while allowing other sections to regenerate freely, creating hybrid files that mix immutable initialization with mutable updates.

## Motivation
Some code sections should only be generated during initial scaffolding (like configuration defaults, initial migrations, or seed data), while other sections need continuous regeneration (like API routes or model definitions). Once tags enable this hybrid approach.

## Applicability
- Database migrations that should never rerun
- Initial configuration files that users customize
- Seed data generation that shouldn't duplicate
- License headers or legal notices
- One-time setup scripts

## Structure

```handlebars
{{!-- ggen:once:start:initial_config --}}
// Generated once, never updated
const DEFAULT_CONFIG = { ... };
{{!-- ggen:once:end:initial_config --}}

// This section regenerates normally
export function getCurrentConfig() { ... }
```

## Implementation

### Basic Once Block

**File: `config_template.tmpl`**
```handlebars
---
output: "config/app_config.rs"
---
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

{{!-- ggen:once:start:initial_defaults --}}
// Initial configuration defaults (generated once)
// Customize these values for your deployment
const DEFAULT_HOST: &str = "127.0.0.1";
const DEFAULT_PORT: u16 = 8080;
const DEFAULT_LOG_LEVEL: &str = "info";
const DEFAULT_DATABASE_URL: &str = "postgres://localhost/myapp";
{{!-- ggen:once:end:initial_defaults --}}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppConfig {
    pub host: String,
    pub port: u16,
    pub log_level: String,
    pub database_url: String,
    {{#each additional_fields}}
    pub {{name}}: {{type}},
    {{/each}}
}

impl Default for AppConfig {
    fn default() -> Self {
        Self {
            host: DEFAULT_HOST.to_string(),
            port: DEFAULT_PORT,
            log_level: DEFAULT_LOG_LEVEL.to_string(),
            database_url: DEFAULT_DATABASE_URL.to_string(),
            {{#each additional_fields}}
            {{name}}: {{default_value}},
            {{/each}}
        }
    }
}

// These methods regenerate when template changes
impl AppConfig {
    pub fn from_env() -> Result<Self, ConfigError> {
        // Updated implementation
        Ok(Self::default())
    }

    pub fn validate(&self) -> Result<(), ConfigError> {
        // Updated validation logic
        Ok(())
    }
}
```

### First Generation

**File: `config/app_config.rs`**
```rust
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

// ggen:once:start:initial_defaults
// Initial configuration defaults (generated once)
// Customize these values for your deployment
const DEFAULT_HOST: &str = "127.0.0.1";
const DEFAULT_PORT: u16 = 8080;
const DEFAULT_LOG_LEVEL: &str = "info";
const DEFAULT_DATABASE_URL: &str = "postgres://localhost/myapp";
// ggen:once:end:initial_defaults

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppConfig {
    pub host: String,
    pub port: u16,
    pub log_level: String,
    pub database_url: String,
}

impl Default for AppConfig {
    fn default() -> Self {
        Self {
            host: DEFAULT_HOST.to_string(),
            port: DEFAULT_PORT,
            log_level: DEFAULT_LOG_LEVEL.to_string(),
            database_url: DEFAULT_DATABASE_URL.to_string(),
        }
    }
}

impl AppConfig {
    pub fn from_env() -> Result<Self, ConfigError> {
        Ok(Self::default())
    }

    pub fn validate(&self) -> Result<(), ConfigError> {
        Ok(())
    }
}
```

### User Customizes Once Block

User modifies the initial defaults:

```rust
// ggen:once:start:initial_defaults
// Production configuration defaults
const DEFAULT_HOST: &str = "0.0.0.0";  // Changed for production
const DEFAULT_PORT: u16 = 3000;         // Changed to standard HTTP
const DEFAULT_LOG_LEVEL: &str = "warn"; // Changed to reduce noise
const DEFAULT_DATABASE_URL: &str = "postgres://prod-db.example.com/myapp"; // Production DB
// ggen:once:end:initial_defaults
```

### Template Updated with New Field

Developer adds `max_connections` field to template:

```handlebars
pub struct AppConfig {
    pub host: String,
    pub port: u16,
    pub log_level: String,
    pub database_url: String,
    pub max_connections: usize,  // NEW FIELD
}

impl Default for AppConfig {
    fn default() -> Self {
        Self {
            host: DEFAULT_HOST.to_string(),
            port: DEFAULT_PORT,
            log_level: DEFAULT_LOG_LEVEL.to_string(),
            database_url: DEFAULT_DATABASE_URL.to_string(),
            max_connections: 100,  // NEW FIELD
        }
    }
}
```

### After Regeneration

User's custom defaults are **preserved**, new field is **added**:

```rust
// ggen:once:start:initial_defaults
// Production configuration defaults (USER'S CUSTOMIZATIONS PRESERVED)
const DEFAULT_HOST: &str = "0.0.0.0";
const DEFAULT_PORT: u16 = 3000;
const DEFAULT_LOG_LEVEL: &str = "warn";
const DEFAULT_DATABASE_URL: &str = "postgres://prod-db.example.com/myapp";
// ggen:once:end:initial_defaults

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AppConfig {
    pub host: String,
    pub port: u16,
    pub log_level: String,
    pub database_url: String,
    pub max_connections: usize,  // ADDED BY REGENERATION
}

impl Default for AppConfig {
    fn default() -> Self {
        Self {
            host: DEFAULT_HOST.to_string(),
            port: DEFAULT_PORT,
            log_level: DEFAULT_LOG_LEVEL.to_string(),
            database_url: DEFAULT_DATABASE_URL.to_string(),
            max_connections: 100,  // ADDED BY REGENERATION
        }
    }
}
```

## Advanced Example: Database Migrations

**File: `migration_template.tmpl`**
```handlebars
---
output: "migrations/{{timestamp}}_{{name}}.sql"
ggen:
  timestamp: "{{now | date: '%Y%m%d%H%M%S'}}"
---
{{!-- ggen:once:start:migration_header --}}
-- Migration: {{name}}
-- Generated: {{timestamp}}
-- Author: {{git_user_name}}
-- DO NOT MODIFY THIS MIGRATION AFTER DEPLOYMENT
{{!-- ggen:once:end:migration_header --}}

{{!-- ggen:once:start:up_migration --}}
-- Up Migration
BEGIN;

{{#each tables}}
CREATE TABLE IF NOT EXISTS {{name}} (
    id BIGSERIAL PRIMARY KEY,
{{#each columns}}
    {{name}} {{type}}{{#if not_null}} NOT NULL{{/if}}{{#if unique}} UNIQUE{{/if}},
{{/each}}
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

{{#each indexes}}
CREATE INDEX IF NOT EXISTS idx_{{../name}}_{{name}} ON {{../name}}({{columns}});
{{/each}}
{{/each}}

COMMIT;
{{!-- ggen:once:end:up_migration --}}

{{!-- ggen:once:start:down_migration --}}
-- Down Migration
BEGIN;

{{#each tables}}
DROP TABLE IF EXISTS {{name}} CASCADE;
{{/each}}

COMMIT;
{{!-- ggen:once:end:down_migration --}}

{{!-- ggen:once:start:seed_data --}}
-- Seed Data (optional, remove if not needed)
BEGIN;

{{#each tables}}
{{#if seed_data}}
INSERT INTO {{name}} ({{seed_columns}}) VALUES
{{#each seed_rows}}
    ({{this}}){{#unless @last}},{{/unless}}
{{/each}};
{{/if}}
{{/each}}

COMMIT;
{{!-- ggen:once:end:seed_data --}}
```

**File: `migration_data.ttl`**
```turtle
@prefix mig: <http://migrations.example.org/> .

mig:CreateUserTable a mig:Migration ;
    mig:name "create_users" ;
    mig:hasTable mig:UsersTable .

mig:UsersTable a mig:Table ;
    mig:name "users" ;
    mig:hasColumn mig:UserName, mig:UserEmail ;
    mig:hasIndex mig:UserEmailIndex .

mig:UserName a mig:Column ;
    mig:name "name" ;
    mig:type "VARCHAR(255)" ;
    mig:notNull true .

mig:UserEmail a mig:Column ;
    mig:name "email" ;
    mig:type "VARCHAR(255)" ;
    mig:notNull true ;
    mig:unique true .

mig:UserEmailIndex a mig:Index ;
    mig:name "email" ;
    mig:columns "email" .
```

**Generated migration:**
```sql
-- ggen:once:start:migration_header
-- Migration: create_users
-- Generated: 20250109120000
-- Author: developer
-- DO NOT MODIFY THIS MIGRATION AFTER DEPLOYMENT
-- ggen:once:end:migration_header

-- ggen:once:start:up_migration
-- Up Migration
BEGIN;

CREATE TABLE IF NOT EXISTS users (
    id BIGSERIAL PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);

COMMIT;
-- ggen:once:end:up_migration

-- ggen:once:start:down_migration
-- Down Migration
BEGIN;

DROP TABLE IF EXISTS users CASCADE;

COMMIT;
-- ggen:once:end:down_migration

-- ggen:once:start:seed_data
-- Seed Data (optional, remove if not needed)
BEGIN;

COMMIT;
-- ggen:once:end:seed_data
```

**After deployment**, user can remove seed block but migration stays frozen.

## Combining Once and Freeze Blocks

**File: `hybrid_template.tmpl`**
```handlebars
{{!-- ggen:once:start:license --}}
/*
 * Copyright (c) 2025 YourCompany
 * Licensed under MIT License
 * Generated: {{now}}
 */
{{!-- ggen:once:end:license --}}

{{!-- ggen:freeze:start:custom_imports --}}
// Add custom imports here
{{!-- ggen:freeze:end:custom_imports --}}

// Auto-generated exports (regenerate freely)
{{#each modules}}
export { {{name}} } from './{{name}}';
{{/each}}

{{!-- ggen:freeze:start:custom_exports --}}
// Add custom exports here
{{!-- ggen:freeze:end:custom_exports --}}

{{!-- ggen:once:start:default_config --}}
export const DEFAULT_OPTIONS = {
    timeout: 30000,
    retries: 3,
    backoff: 'exponential',
};
{{!-- ggen:once:end:default_config --}}

// Auto-generated utility functions
{{#each utilities}}
export function {{name}}({{params}}) {
    {{implementation}}
}
{{/each}}
```

## Comparison Table

| Feature | Once Block | Freeze Block |
|---------|-----------|--------------|
| **Regeneration** | Never regenerates | Never regenerates |
| **Purpose** | Initial scaffolding | Ongoing customization |
| **Use Case** | Defaults, migrations | Business logic, custom methods |
| **User Expectation** | Set once, customize freely | Add code during development |
| **Template Changes** | Content ignored after first gen | Content ignored always |

## Best Practices

1. **Use once blocks for:**
   - Initial configuration values
   - One-time migrations
   - License headers
   - Seed data
   - First-run setup

2. **Use freeze blocks for:**
   - Business logic
   - Custom methods
   - Test implementations
   - Complex validations
   - Integration code

3. **Naming conventions:**
```handlebars
{{!-- ggen:once:start:initial_setup --}}
{{!-- ggen:once:start:license_header --}}
{{!-- ggen:once:start:migration_up --}}
{{!-- ggen:once:start:seed_data --}}
{{!-- ggen:once:start:default_config --}}
```

4. **Documentation:**
   - Always comment why something is once-only
   - Explain customization expectations
   - Warn about regeneration behavior

## Anti-Patterns

❌ **Using once for frequently changing content:**
```handlebars
{{!-- ggen:once:start:api_routes --}}
// These routes change often, should NOT be once-only
{{!-- ggen:once:end:api_routes --}}
```

❌ **No once blocks for migrations:**
Migrations without once blocks can accidentally duplicate.

❌ **Mixing concerns in one block:**
```handlebars
{{!-- ggen:once:start:everything --}}
// License, config, AND migration logic - TOO BROAD
{{!-- ggen:once:end:everything --}}
```

## Benefits

1. **Safe Initialization**: Generate defaults without overwriting customizations
2. **Migration Safety**: Never duplicate database migrations
3. **User Freedom**: Users can modify once-generated content freely
4. **Template Evolution**: Update templates without breaking existing deployments
5. **Clear Intent**: Explicit markers show what's immutable

## Related Patterns

- **015: Immutability First** - Freeze blocks for ongoing customization
- **014: Fan-Out Projection** - Generate many files with once protection
- **091: Idempotent Injection** - Update without duplication

## Known Uses

- Database migration systems (Flyway, Liquibase)
- Rails migrations and seeds
- Django initial data fixtures
- Infrastructure-as-code init scripts
- License header generation

## See Also

- GGen Once Tag Documentation
- Migration Best Practices
- Configuration Management Patterns
