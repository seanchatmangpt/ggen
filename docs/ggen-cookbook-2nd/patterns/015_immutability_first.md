# Pattern 015: Immutability First (Freeze Blocks)

## Intent
Protect critical sections of generated files from regeneration while allowing safe updates to other sections.

## Motivation
Generated code often needs manual customization. Without protection, regenerating templates overwrites manual changes. Freeze blocks mark sections as immutable, preventing accidental data loss while enabling continuous generation.

## Applicability
- Protecting manually written business logic in generated files
- Preserving custom configurations in scaffolded projects
- Maintaining user modifications in auto-generated documentation
- Safeguarding test implementations while updating boilerplate

## Structure

```handlebars
{{!-- ggen:freeze:start:implementation --}}
// This section is protected from regeneration
func CustomLogic() {
    // Your custom code here
}
{{!-- ggen:freeze:end:implementation --}}
```

## Implementation

### Basic Freeze Block

**File: `service_template.tmpl`**
```handlebars
---
output: "src/services/{{name}}_service.rs"
---
use std::sync::Arc;

pub struct {{name}}Service {
    repository: Arc<dyn Repository>,
}

impl {{name}}Service {
    pub fn new(repository: Arc<dyn Repository>) -> Self {
        Self { repository }
    }

    {{!-- ggen:freeze:start:custom_methods --}}
    // Add your custom methods here
    // This block will NOT be overwritten on regeneration
    {{!-- ggen:freeze:end:custom_methods --}}

    // Auto-generated CRUD methods (safe to regenerate)
    pub async fn get_by_id(&self, id: u64) -> Result<{{name}}, Error> {
        self.repository.find_by_id(id).await
    }

    pub async fn create(&self, data: Create{{name}}) -> Result<{{name}}, Error> {
        self.repository.insert(data).await
    }

    pub async fn update(&self, id: u64, data: Update{{name}}) -> Result<{{name}}, Error> {
        self.repository.update(id, data).await
    }

    pub async fn delete(&self, id: u64) -> Result<bool, Error> {
        self.repository.delete(id).await
    }
}

{{!-- ggen:freeze:start:additional_impls --}}
// Custom trait implementations go here
{{!-- ggen:freeze:end:additional_impls --}}

#[cfg(test)]
mod tests {
    use super::*;

    {{!-- ggen:freeze:start:custom_tests --}}
    // Your custom tests here
    {{!-- ggen:freeze:end:custom_tests --}}

    #[test]
    fn test_service_creation() {
        // Auto-generated test
        assert!(true);
    }
}
```

### First Generation Output

**File: `src/services/user_service.rs`**
```rust
use std::sync::Arc;

pub struct UserService {
    repository: Arc<dyn Repository>,
}

impl UserService {
    pub fn new(repository: Arc<dyn Repository>) -> Self {
        Self { repository }
    }

    // ggen:freeze:start:custom_methods
    // Add your custom methods here
    // This block will NOT be overwritten on regeneration
    // ggen:freeze:end:custom_methods

    // Auto-generated CRUD methods (safe to regenerate)
    pub async fn get_by_id(&self, id: u64) -> Result<User, Error> {
        self.repository.find_by_id(id).await
    }

    pub async fn create(&self, data: CreateUser) -> Result<User, Error> {
        self.repository.insert(data).await
    }

    pub async fn update(&self, id: u64, data: UpdateUser) -> Result<User, Error> {
        self.repository.update(id, data).await
    }

    pub async fn delete(&self, id: u64) -> Result<bool, Error> {
        self.repository.delete(id).await
    }
}

// ggen:freeze:start:additional_impls
// Custom trait implementations go here
// ggen:freeze:end:additional_impls

#[cfg(test)]
mod tests {
    use super::*;

    // ggen:freeze:start:custom_tests
    // Your custom tests here
    // ggen:freeze:end:custom_tests

    #[test]
    fn test_service_creation() {
        // Auto-generated test
        assert!(true);
    }
}
```

### After Manual Customization

Developer adds custom code inside freeze blocks:

```rust
use std::sync::Arc;

pub struct UserService {
    repository: Arc<dyn Repository>,
}

impl UserService {
    pub fn new(repository: Arc<dyn Repository>) -> Self {
        Self { repository }
    }

    // ggen:freeze:start:custom_methods
    pub async fn find_by_email(&self, email: &str) -> Result<User, Error> {
        self.repository.find_one(|u| u.email == email).await
    }

    pub async fn activate_user(&self, id: u64) -> Result<User, Error> {
        let mut user = self.get_by_id(id).await?;
        user.active = true;
        self.repository.update(id, user).await
    }
    // ggen:freeze:end:custom_methods

    // Auto-generated CRUD methods (safe to regenerate)
    pub async fn get_by_id(&self, id: u64) -> Result<User, Error> {
        self.repository.find_by_id(id).await
    }

    pub async fn create(&self, data: CreateUser) -> Result<User, Error> {
        self.repository.insert(data).await
    }

    pub async fn update(&self, id: u64, data: UpdateUser) -> Result<User, Error> {
        self.repository.update(id, data).await
    }

    pub async fn delete(&self, id: u64) -> Result<bool, Error> {
        self.repository.delete(id).await
    }
}

// ggen:freeze:start:additional_impls
impl Clone for UserService {
    fn clone(&self) -> Self {
        Self {
            repository: Arc::clone(&self.repository),
        }
    }
}
// ggen:freeze:end:additional_impls

#[cfg(test)]
mod tests {
    use super::*;

    // ggen:freeze:start:custom_tests
    #[tokio::test]
    async fn test_find_by_email() {
        let service = setup_test_service().await;
        let user = service.find_by_email("test@example.com").await.unwrap();
        assert_eq!(user.email, "test@example.com");
    }

    #[tokio::test]
    async fn test_activate_user() {
        let service = setup_test_service().await;
        let user = service.activate_user(1).await.unwrap();
        assert!(user.active);
    }
    // ggen:freeze:end:custom_tests

    #[test]
    fn test_service_creation() {
        // Auto-generated test
        assert!(true);
    }
}
```

### Regeneration with Template Update

Developer updates template to add new CRUD method:

```handlebars
    pub async fn list_all(&self) -> Result<Vec<{{name}}>, Error> {
        self.repository.find_all().await
    }
```

**After regeneration**, custom code is preserved:

```rust
// Custom methods PRESERVED
pub async fn find_by_email(&self, email: &str) -> Result<User, Error> { ... }
pub async fn activate_user(&self, id: u64) -> Result<User, Error> { ... }

// NEW auto-generated method added
pub async fn list_all(&self) -> Result<Vec<User>, Error> {
    self.repository.find_all().await
}

// Other auto-generated methods updated if template changed
pub async fn get_by_id(&self, id: u64) -> Result<User, Error> { ... }
```

## Advanced Example: Multi-Level Freeze

**File: `api_controller.tmpl`**
```handlebars
---
output: "src/controllers/{{name}}_controller.rs"
---
use axum::{
    extract::{Path, Json, State},
    http::StatusCode,
};
use crate::services::{{name}}_service::{{name}}Service;

{{!-- ggen:freeze:start:imports --}}
// Add custom imports here
{{!-- ggen:freeze:end:imports --}}

pub struct {{name}}Controller {
    service: {{name}}Service,
}

impl {{name}}Controller {
    pub fn new(service: {{name}}Service) -> Self {
        Self { service }
    }

    {{!-- ggen:freeze:start:pre_hooks --}}
    // Add request preprocessing hooks here
    {{!-- ggen:freeze:end:pre_hooks --}}

    pub async fn list(
        State(controller): State<{{name}}Controller>,
    ) -> Result<Json<Vec<{{name}}>>, StatusCode> {
        {{!-- ggen:freeze:start:list_logic --}}
        controller.service.list_all().await
            .map(Json)
            .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
        {{!-- ggen:freeze:end:list_logic --}}
    }

    pub async fn get(
        State(controller): State<{{name}}Controller>,
        Path(id): Path<u64>,
    ) -> Result<Json<{{name}}>, StatusCode> {
        {{!-- ggen:freeze:start:get_logic --}}
        controller.service.get_by_id(id).await
            .map(Json)
            .map_err(|_| StatusCode::NOT_FOUND)
        {{!-- ggen:freeze:end:get_logic --}}
    }

    {{!-- ggen:freeze:start:custom_endpoints --}}
    // Add custom endpoint handlers here
    {{!-- ggen:freeze:end:custom_endpoints --}}

    {{!-- ggen:freeze:start:post_hooks --}}
    // Add response postprocessing hooks here
    {{!-- ggen:freeze:end:post_hooks --}}
}
```

**After customization:**
```rust
// ggen:freeze:start:imports
use crate::middleware::metrics::track_request;
use crate::validation::Validator;
// ggen:freeze:end:imports

// ggen:freeze:start:pre_hooks
async fn validate_request<T: Validator>(data: &T) -> Result<(), StatusCode> {
    data.validate()
        .map_err(|_| StatusCode::BAD_REQUEST)
}
// ggen:freeze:end:pre_hooks

pub async fn list(
    State(controller): State<UserController>,
) -> Result<Json<Vec<User>>, StatusCode> {
    // ggen:freeze:start:list_logic
    track_request("user_list").await;

    controller.service.list_all().await
        .map(Json)
        .map_err(|e| {
            log::error!("Failed to list users: {:?}", e);
            StatusCode::INTERNAL_SERVER_ERROR
        })
    // ggen:freeze:end:list_logic
}

// ggen:freeze:start:custom_endpoints
pub async fn search(
    State(controller): State<UserController>,
    Query(params): Query<SearchParams>,
) -> Result<Json<Vec<User>>, StatusCode> {
    controller.service.search(params).await
        .map(Json)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}

pub async fn bulk_update(
    State(controller): State<UserController>,
    Json(updates): Json<Vec<UserUpdate>>,
) -> Result<StatusCode, StatusCode> {
    validate_request(&updates).await?;
    controller.service.bulk_update(updates).await
        .map(|_| StatusCode::OK)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}
// ggen:freeze:end:custom_endpoints
```

## Freeze Block Naming Conventions

```handlebars
{{!-- ggen:freeze:start:imports --}}        // Package/module imports
{{!-- ggen:freeze:start:types --}}          // Type definitions
{{!-- ggen:freeze:start:constants --}}      // Constants and configs
{{!-- ggen:freeze:start:pre_hooks --}}      // Before-operation hooks
{{!-- ggen:freeze:start:post_hooks --}}     // After-operation hooks
{{!-- ggen:freeze:start:custom_methods --}} // Custom method implementations
{{!-- ggen:freeze:start:custom_tests --}}   // Custom test cases
{{!-- ggen:freeze:start:helpers --}}        // Helper functions
{{!-- ggen:freeze:start:validation --}}     // Validation logic
```

## Best Practices

1. **Name blocks descriptively**: Use semantic names that indicate purpose
2. **Place strategically**: Position freeze blocks where customization is likely
3. **Keep blocks focused**: One concern per freeze block
4. **Document intent**: Add comments explaining what goes in each block
5. **Version freeze markers**: Include version info for tracking

## Anti-Patterns

❌ **Freezing everything:**
```handlebars
{{!-- ggen:freeze:start:entire_file --}}
// Entire file content
{{!-- ggen:freeze:end:entire_file --}}
```
This defeats the purpose of generation.

❌ **No freeze blocks:**
Generated files without freeze blocks cannot be safely customized.

❌ **Overlapping freeze blocks:**
```handlebars
{{!-- ggen:freeze:start:section1 --}}
    {{!-- ggen:freeze:start:section2 --}}
    {{!-- ggen:freeze:end:section2 --}}
{{!-- ggen:freeze:end:section1 --}}
```
Avoid nesting freeze blocks.

## Benefits

1. **Safe Regeneration**: Update templates without losing custom code
2. **Clear Boundaries**: Explicit zones for generated vs. custom code
3. **Collaboration**: Multiple developers can work on same generated files
4. **Incremental Development**: Add features gradually without losing work
5. **Refactoring Safety**: Update templates knowing customizations are safe

## Related Patterns

- **014: Fan-Out Projection** - Generate many files with freeze protection
- **016: Hybrid Files** - Combine freeze blocks with once-only generation
- **091: Idempotent Injection** - Update specific sections safely

## Known Uses

- Django migrations with custom SQL
- Rails scaffolding with custom validations
- GraphQL schema stitching with custom resolvers
- API client generation with custom methods

## See Also

- GGen Freeze Block Documentation
- Template Regeneration Guide
- Custom Code Preservation Strategies
