# Pattern 091: Idempotent Injection

## Intent
Safely inject code into specific sections of generated files without duplication, enabling incremental updates to existing outputs.

## Motivation
Generated files often need updates (new methods, imports, fields) without complete regeneration. Idempotent injection allows targeted updates to specific file sections while preserving manual customizations and preventing duplicate code.

## Applicability
- Adding new methods to generated classes
- Updating import statements without duplication
- Injecting new configuration entries
- Appending database migrations
- Incremental API endpoint additions

## Structure

```yaml
---
ggen:injection:
  target: "src/services/user_service.rs"
  anchor: "// INJECT_METHODS_HERE"
  strategy: "append-once"
  idempotency_key: "method:{{method_name}}"
---
```

## Implementation

### Basic Idempotent Injection

**File: `existing_service.rs`** (already exists)
```rust
use std::sync::Arc;

pub struct UserService {
    repository: Arc<dyn UserRepository>,
}

impl UserService {
    pub fn new(repository: Arc<dyn UserRepository>) -> Self {
        Self { repository }
    }

    pub async fn get_user(&self, id: u64) -> Result<User, Error> {
        self.repository.find_by_id(id).await
    }

    // INJECT_METHODS_HERE
}
```

**File: `add_method_template.tmpl`**
```handlebars
---
ggen:injection:
  target: "src/services/user_service.rs"
  anchor: "// INJECT_METHODS_HERE"
  strategy: "append-once"
  idempotency_key: "method:{{method_name}}"
---
    pub async fn {{method_name}}(&self{{#each params}}, {{name}}: {{type}}{{/each}}) -> Result<{{return_type}}, Error> {
        {{implementation}}
    }
```

**File: `methods_to_add.ttl`**
```turtle
@prefix meth: <http://methods.example.org/> .

meth:FindByEmail a meth:Method ;
    meth:name "find_by_email" ;
    meth:returnType "User" ;
    meth:hasParam meth:EmailParam ;
    meth:implementation "self.repository.find_by_email(email).await" .

meth:EmailParam a meth:Parameter ;
    meth:name "email" ;
    meth:type "&str" .

meth:ListActive a meth:Method ;
    meth:name "list_active_users" ;
    meth:returnType "Vec<User>" ;
    meth:implementation "self.repository.find_where(|u| u.active).await" .

meth:UpdateEmail a meth:Method ;
    meth:name "update_user_email" ;
    meth:returnType "User" ;
    meth:hasParam meth:IdParam, meth:NewEmailParam ;
    meth:implementation """let mut user = self.get_user(id).await?;
        user.email = new_email.to_string();
        self.repository.update(user).await""" .

meth:IdParam a meth:Parameter ;
    meth:name "id" ;
    meth:type "u64" .

meth:NewEmailParam a meth:Parameter ;
    meth:name "new_email" ;
    meth:type "&str" .
```

**First injection:**
```bash
ggen inject add_method_template.tmpl --graph methods_to_add.ttl
```

**After first injection:**
```rust
pub struct UserService {
    repository: Arc<dyn UserRepository>,
}

impl UserService {
    pub fn new(repository: Arc<dyn UserRepository>) -> Self {
        Self { repository }
    }

    pub async fn get_user(&self, id: u64) -> Result<User, Error> {
        self.repository.find_by_id(id).await
    }

    // INJECT_METHODS_HERE
    pub async fn find_by_email(&self, email: &str) -> Result<User, Error> {
        self.repository.find_by_email(email).await
    }

    pub async fn list_active_users(&self) -> Result<Vec<User>, Error> {
        self.repository.find_where(|u| u.active).await
    }

    pub async fn update_user_email(&self, id: u64, new_email: &str) -> Result<User, Error> {
        let mut user = self.get_user(id).await?;
        user.email = new_email.to_string();
        self.repository.update(user).await
    }
}
```

**Second injection (same graph, same template):**
```bash
# Running again - NO DUPLICATES added
ggen inject add_method_template.tmpl --graph methods_to_add.ttl
# Output: 0 new injections (3 already present, skipped)
```

File remains unchanged - idempotency guaranteed by unique keys.

### Advanced Example: Import Management

**File: `existing_module.rs`**
```rust
// INJECT_IMPORTS_HERE

pub struct MyModule {
    // ...
}
```

**File: `import_injector.tmpl`**
```handlebars
---
ggen:injection:
  target: "{{target_file}}"
  anchor: "// INJECT_IMPORTS_HERE"
  strategy: "prepend-once"
  idempotency_key: "import:{{import_path}}"
  sort: true  # Auto-sort imports alphabetically
---
use {{import_path}};
```

**File: `imports_graph.ttl`**
```turtle
@prefix imp: <http://imports.example.org/> .

imp:Import1 a imp:Import ;
    imp:path "std::collections::HashMap" ;
    imp:targetFile "src/module.rs" .

imp:Import2 a imp:Import ;
    imp:path "serde::{Serialize, Deserialize}" ;
    imp:targetFile "src/module.rs" .

imp:Import3 a imp:Import ;
    imp:path "tokio::sync::RwLock" ;
    imp:targetFile "src/module.rs" .

imp:Import4 a imp:Import ;
    imp:path "std::collections::HashMap" ;  # Duplicate!
    imp:targetFile "src/module.rs" .
```

**After injection:**
```rust
// INJECT_IMPORTS_HERE
use serde::{Serialize, Deserialize};
use std::collections::HashMap;
use tokio::sync::RwLock;

pub struct MyModule {
    // ...
}
```

Note: `std::collections::HashMap` appears only once (idempotent), imports are sorted.

## Multi-Anchor Injection

**File: `service_with_anchors.rs`**
```rust
// INJECT_IMPORTS_HERE

pub struct OrderService {
    // INJECT_FIELDS_HERE
}

impl OrderService {
    pub fn new() -> Self {
        Self {
            // INJECT_FIELD_INIT_HERE
        }
    }

    // INJECT_METHODS_HERE
}

// INJECT_TESTS_HERE
```

**File: `multi_anchor_template.tmpl`**
```handlebars
---
ggen:injection:
  - target: "src/services/order_service.rs"
    anchor: "// INJECT_IMPORTS_HERE"
    strategy: "prepend-once"
    idempotency_key: "import:{{import}}"
    content: "use {{import}};"

  - target: "src/services/order_service.rs"
    anchor: "// INJECT_FIELDS_HERE"
    strategy: "append-once"
    idempotency_key: "field:{{field_name}}"
    content: "    {{field_name}}: {{field_type}},"

  - target: "src/services/order_service.rs"
    anchor: "// INJECT_FIELD_INIT_HERE"
    strategy: "append-once"
    idempotency_key: "init:{{field_name}}"
    content: "            {{field_name}}: {{field_init}},"

  - target: "src/services/order_service.rs"
    anchor: "// INJECT_METHODS_HERE"
    strategy: "append-once"
    idempotency_key: "method:{{method_name}}"
    content: |
      pub fn {{method_name}}(&self) -> {{return_type}} {
          {{implementation}}
      }
---
```

**File: `order_components.ttl`**
```turtle
@prefix comp: <http://components.example.org/> .

comp:PaymentGateway a comp:Component ;
    comp:import "crate::payment::PaymentGateway" ;
    comp:fieldName "payment" ;
    comp:fieldType "Arc<PaymentGateway>" ;
    comp:fieldInit "Arc::new(PaymentGateway::new())" ;
    comp:hasMethod comp:ProcessPayment .

comp:ProcessPayment a comp:Method ;
    comp:name "process_payment" ;
    comp:returnType "Result<Payment, Error>" ;
    comp:implementation "self.payment.process().await" .

comp:NotificationService a comp:Component ;
    comp:import "crate::notification::NotificationService" ;
    comp:fieldName "notifications" ;
    comp:fieldType "NotificationService" ;
    comp:fieldInit "NotificationService::new()" ;
    comp:hasMethod comp:SendNotification .

comp:SendNotification a comp:Method ;
    comp:name "send_order_notification" ;
    comp:returnType "Result<(), Error>" ;
    comp:implementation "self.notifications.send('Order placed').await" .
```

**After injection:**
```rust
// INJECT_IMPORTS_HERE
use crate::notification::NotificationService;
use crate::payment::PaymentGateway;

pub struct OrderService {
    // INJECT_FIELDS_HERE
    notifications: NotificationService,
    payment: Arc<PaymentGateway>,
}

impl OrderService {
    pub fn new() -> Self {
        Self {
            // INJECT_FIELD_INIT_HERE
            notifications: NotificationService::new(),
            payment: Arc::new(PaymentGateway::new()),
        }
    }

    // INJECT_METHODS_HERE
    pub fn process_payment(&self) -> Result<Payment, Error> {
        self.payment.process().await
    }

    pub fn send_order_notification(&self) -> Result<(), Error> {
        self.notifications.send('Order placed').await
    }
}

// INJECT_TESTS_HERE
```

## Injection Strategies

| Strategy | Behavior | Use Case |
|----------|----------|----------|
| `append-once` | Add after anchor, skip if key exists | Adding methods, fields |
| `prepend-once` | Add before anchor, skip if key exists | Imports, headers |
| `replace-once` | Replace anchor, skip if key exists | Configuration values |
| `append-always` | Always add after anchor | Logging, events |
| `prepend-always` | Always add before anchor | Rarely used |

## Idempotency Key Patterns

```yaml
# Method injection
idempotency_key: "method:{{method_name}}"

# Import injection
idempotency_key: "import:{{import_path | hash}}"

# Field injection
idempotency_key: "field:{{struct_name}}:{{field_name}}"

# Configuration injection
idempotency_key: "config:{{section}}:{{key}}"

# Test injection
idempotency_key: "test:{{test_name}}"

# Endpoint injection
idempotency_key: "endpoint:{{method}}:{{path}}"
```

## Database Migration Example

**File: `schema.sql`** (existing)
```sql
-- INJECT_TABLES_HERE

-- INJECT_INDEXES_HERE

-- INJECT_CONSTRAINTS_HERE
```

**File: `migration_injector.tmpl`**
```handlebars
---
ggen:injection:
  - target: "migrations/schema.sql"
    anchor: "-- INJECT_TABLES_HERE"
    strategy: "append-once"
    idempotency_key: "table:{{table_name}}"
    content: |
      CREATE TABLE IF NOT EXISTS {{table_name}} (
      {{#each columns}}
          {{name}} {{type}}{{#if constraints}} {{constraints}}{{/if}}{{#unless @last}},{{/unless}}
      {{/each}}
      );

  - target: "migrations/schema.sql"
    anchor: "-- INJECT_INDEXES_HERE"
    strategy: "append-once"
    idempotency_key: "index:{{table_name}}:{{index_name}}"
    content: "CREATE INDEX IF NOT EXISTS {{index_name}} ON {{table_name}}({{columns}});"
---
```

**After multiple injection runs:**
```sql
-- INJECT_TABLES_HERE
CREATE TABLE IF NOT EXISTS users (
    id BIGSERIAL PRIMARY KEY,
    email VARCHAR(255) UNIQUE NOT NULL,
    created_at TIMESTAMP DEFAULT NOW()
);

CREATE TABLE IF NOT EXISTS products (
    id BIGSERIAL PRIMARY KEY,
    title VARCHAR(255) NOT NULL,
    price DECIMAL(10, 2) NOT NULL
);

-- INJECT_INDEXES_HERE
CREATE INDEX IF NOT EXISTS idx_users_email ON users(email);
CREATE INDEX IF NOT EXISTS idx_products_title ON products(title);

-- INJECT_CONSTRAINTS_HERE
```

## Benefits

1. **No Duplicates**: Idempotency keys prevent repeated injections
2. **Safe Updates**: Add to existing files without breaking changes
3. **Incremental Development**: Build up files gradually
4. **Coordination**: Multiple templates can inject into same file
5. **Traceability**: Keys track what was injected and when

## Best Practices

1. **Unique keys**: Always use unique, stable idempotency keys
2. **Named anchors**: Use descriptive anchor comments
3. **Sorted output**: Enable sorting for imports/similar content
4. **Validation**: Check injections don't break syntax
5. **Anchor placement**: Place anchors in logical, stable locations

## Anti-Patterns

❌ **No idempotency key:**
```yaml
strategy: "append-once"
# Missing idempotency_key - will append every time!
```

❌ **Non-unique keys:**
```yaml
idempotency_key: "import"  # Same for all imports!
```

❌ **Anchors in generated code:**
```rust
{{#each items}}
// INJECT_HERE  # Don't put anchors in loops!
{{/each}}
```

❌ **Complex content in YAML:**
```yaml
content: "def foo():\n    bar()\n    baz()"  # Use template content instead
```

## Related Patterns

- **015: Immutability First** - Combine freeze blocks with injection
- **016: Hybrid Files** - Use once blocks with incremental injection
- **014: Fan-Out Projection** - Inject into many generated files

## Known Uses

- Adding REST endpoints to existing APIs
- Incremental database migrations
- Managing imports across modules
- Configuration file updates
- Plugin registration systems

## See Also

- GGen Injection Documentation
- Idempotency in Code Generation
- Safe File Updates
- Anchor-Based Templating
