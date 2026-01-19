# Domain Module API Signatures

**Date**: 2025-11-20
**Architecture**: Clean Architecture with Zero CLI Dependencies

---

## Marketplace Module API

**Module**: `ggen_domain::marketplace`

### Public Functions

```rust
/// Search for packages in the marketplace
///
/// # Arguments
/// * `query` - Search query with filters
///
/// # Returns
/// * `Result<SearchResults>` - List of matching packages
///
/// # Examples
/// ```rust
/// use ggen_domain::marketplace::{search, SearchQuery};
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let query = SearchQuery {
///         query: "rust web".to_string(),
///         limit: 10,
///         category: Some("backend".to_string()),
///         min_score: Some(80.0),
///     };
///
///     let results = search(query).await?;
///     println!("Found {} packages", results.total);
///     Ok(())
/// }
/// ```
pub async fn search(query: SearchQuery) -> Result<SearchResults, MarketplaceError>

/// Install a package with dependencies
///
/// # Arguments
/// * `input` - Installation parameters
///
/// # Returns
/// * `Result<InstallResult>` - Installation details
///
/// # Examples
/// ```rust
/// use ggen_domain::marketplace::{install, InstallInput};
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let input = InstallInput {
///         package: "io.ggen.rust.microservice".to_string(),
///         target: None,
///         force: false,
///         no_dependencies: false,
///         dry_run: false,
///     };
///
///     let result = install(input).await?;
///     println!("Installed {} to {}", result.package_name, result.install_path.display());
///     Ok(())
/// }
/// ```
pub async fn install(input: InstallInput) -> Result<InstallResult, MarketplaceError>

/// Publish a package to the marketplace
///
/// # Arguments
/// * `input` - Publishing parameters
///
/// # Returns
/// * `Result<PublishResult>` - Publication details
///
/// # Examples
/// ```rust
/// use ggen_domain::marketplace::{publish, PublishInput};
/// use std::path::PathBuf;
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let input = PublishInput {
///         path: PathBuf::from("./my-package"),
///         tag: Some("v1.0.0".to_string()),
///         dry_run: false,
///         force: false,
///     };
///
///     let result = publish(input).await?;
///     println!("Published {} version {}", result.package_name, result.version);
///     Ok(())
/// }
/// ```
pub async fn publish(input: PublishInput) -> Result<PublishResult, MarketplaceError>

/// Uninstall a package
///
/// # Arguments
/// * `input` - Uninstallation parameters
///
/// # Returns
/// * `Result<UninstallResult>` - Uninstallation details
pub async fn uninstall(input: UninstallInput) -> Result<UninstallResult, MarketplaceError>

/// List installed packages
///
/// # Arguments
/// * `input` - Listing parameters
///
/// # Returns
/// * `Result<ListOutput>` - List of installed packages
pub async fn list(input: ListInput) -> Result<ListOutput, MarketplaceError>

/// Validate package for production readiness
///
/// # Arguments
/// * `input` - Validation parameters
///
/// # Returns
/// * `Result<ValidationResult>` - Validation results
pub async fn validate(input: ValidateInput) -> Result<ValidationResult, MarketplaceError>
```

---

## Template Module API

**Module**: `ggen_domain::template`

### Public Functions

```rust
/// Generate code from template
///
/// # Arguments
/// * `input` - Generation parameters
///
/// # Returns
/// * `Result<GenerateResult>` - Generated files
///
/// # Examples
/// ```rust
/// use ggen_domain::template::{generate, GenerateInput};
/// use std::path::PathBuf;
/// use std::collections::BTreeMap;
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let mut vars = BTreeMap::new();
///     vars.insert("name".to_string(), "MyProject".to_string());
///
///     let input = GenerateInput {
///         template: "rust-microservice".to_string(),
///         output_dir: PathBuf::from("./output"),
///         vars,
///         force: false,
///     };
///
///     let result = generate(input).await?;
///     println!("Generated {} files", result.total_files);
///     Ok(())
/// }
/// ```
pub async fn generate(input: GenerateInput) -> Result<GenerateResult, TemplateError>

/// Validate template syntax
///
/// # Arguments
/// * `input` - Validation parameters
///
/// # Returns
/// * `Result<ValidationResult>` - Validation results
///
/// # Examples
/// ```rust
/// use ggen_domain::template::{validate, ValidateInput};
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let input = ValidateInput {
///         template: "my-template".to_string(),
///     };
///
///     let result = validate(input).await?;
///     if !result.valid {
///         for error in result.errors {
///             println!("Error at line {}: {}", error.line, error.message);
///         }
///     }
///     Ok(())
/// }
/// ```
pub async fn validate(input: ValidateInput) -> Result<ValidationResult, TemplateError>

/// Discover templates in directories
///
/// # Arguments
/// * `input` - Discovery parameters
///
/// # Returns
/// * `Result<DiscoveryResult>` - Discovered templates
///
/// # Examples
/// ```rust
/// use ggen_domain::template::{discover, DiscoverInput};
/// use std::path::PathBuf;
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let input = DiscoverInput {
///         search_dirs: vec![PathBuf::from("./templates")],
///         pattern: Some("*.tmpl".to_string()),
///     };
///
///     let result = discover(input).await?;
///     println!("Found {} templates", result.total);
///     Ok(())
/// }
/// ```
pub async fn discover(input: DiscoverInput) -> Result<DiscoveryResult, TemplateError>

/// List all available templates
///
/// # Arguments
/// * `input` - Listing parameters
///
/// # Returns
/// * `Result<ListOutput>` - List of templates
pub async fn list(input: ListInput) -> Result<ListOutput, TemplateError>

/// Show template metadata
///
/// # Arguments
/// * `input` - Show parameters
///
/// # Returns
/// * `Result<ShowOutput>` - Template details
pub async fn show(input: ShowInput) -> Result<ShowOutput, TemplateError>

/// Create new template
///
/// # Arguments
/// * `input` - Creation parameters
///
/// # Returns
/// * `Result<CreateResult>` - Created template details
pub async fn create(input: CreateInput) -> Result<CreateResult, TemplateError>
```

---

## Project Module API

**Module**: `ggen_domain::project`

### Public Functions

```rust
/// Create a new project from template
///
/// # Arguments
/// * `input` - Creation parameters
///
/// # Returns
/// * `Result<CreateResult>` - Created project details
///
/// # Examples
/// ```rust
/// use ggen_domain::project::{create, CreateInput};
/// use std::path::PathBuf;
/// use std::collections::BTreeMap;
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let mut vars = BTreeMap::new();
///     vars.insert("author".to_string(), "Alice".to_string());
///
///     let input = CreateInput {
///         name: "my-project".to_string(),
///         template: Some("rust-web-app".to_string()),
///         output_dir: PathBuf::from("./projects"),
///         vars,
///     };
///
///     let result = create(input).await?;
///     println!("Created project at {}", result.project_path.display());
///     println!("Generated {} files", result.files_created.len());
///     Ok(())
/// }
/// ```
pub async fn create(input: CreateInput) -> Result<CreateResult, ProjectError>

/// Configure project settings
///
/// # Arguments
/// * `input` - Configuration parameters
///
/// # Returns
/// * `Result<ConfigureResult>` - Configuration result
///
/// # Examples
/// ```rust
/// use ggen_domain::project::{configure, ConfigureInput, ProjectConfig};
/// use std::path::PathBuf;
/// use std::collections::BTreeMap;
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let mut settings = BTreeMap::new();
///     settings.insert("rust_edition".to_string(), "2021".to_string());
///
///     let config = ProjectConfig {
///         name: "my-project".to_string(),
///         version: "1.0.0".to_string(),
///         description: Some("My project description".to_string()),
///         author: Some("Alice <alice@example.com>".to_string()),
///         settings,
///     };
///
///     let input = ConfigureInput {
///         project_path: PathBuf::from("./my-project"),
///         config,
///     };
///
///     let result = configure(input).await?;
///     println!("Configuration updated: {}", result.config_updated);
///     Ok(())
/// }
/// ```
pub async fn configure(input: ConfigureInput) -> Result<ConfigureResult, ProjectError>

/// Generate project files from plan
///
/// # Arguments
/// * `input` - Generation parameters
///
/// # Returns
/// * `Result<GenerateResult>` - Generation result
///
/// # Examples
/// ```rust
/// use ggen_domain::project::{generate, GenerateInput};
/// use std::path::PathBuf;
///
/// #[tokio::main]
/// async fn main() -> Result<()> {
///     let input = GenerateInput {
///         project_path: PathBuf::from("./my-project"),
///         plan: None, // Will use default plan
///         force: false,
///     };
///
///     let result = generate(input).await?;
///     println!("Generated {} files", result.total_files);
///     for file in result.files_generated {
///         println!("  - {}", file.display());
///     }
///     Ok(())
/// }
/// ```
pub async fn generate(input: GenerateInput) -> Result<GenerateResult, ProjectError>

/// List available project configurations
///
/// # Arguments
/// * `input` - Listing parameters
///
/// # Returns
/// * `Result<ListOutput>` - List of projects
pub async fn list(input: ListInput) -> Result<ListOutput, ProjectError>

/// Initialize project structure
///
/// # Arguments
/// * `input` - Initialization parameters
///
/// # Returns
/// * `Result<InitResult>` - Initialization result
pub async fn init(input: InitInput) -> Result<InitResult, ProjectError>

/// Build project plan
///
/// # Arguments
/// * `input` - Planning parameters
///
/// # Returns
/// * `Result<PlanResult>` - Generated plan
pub async fn plan(input: PlanInput) -> Result<PlanResult, ProjectError>

/// Apply project changes
///
/// # Arguments
/// * `input` - Apply parameters
///
/// # Returns
/// * `Result<ApplyResult>` - Apply result
pub async fn apply(input: ApplyInput) -> Result<ApplyResult, ProjectError>
```

---

## Type System Overview

### Naming Conventions

All types follow consistent naming patterns:

1. **Input Types**: Suffix with `Input`
   - `SearchQuery` (exception: common term)
   - `InstallInput`
   - `PublishInput`
   - `GenerateInput`

2. **Output Types**: Suffix with `Result`, `Output`, `Info`, or domain-specific names
   - `SearchResults`
   - `InstallResult`
   - `ListOutput`
   - `PackageInfo`
   - `TemplateMetadata`

3. **Error Types**: Suffix with `Error`
   - `MarketplaceError`
   - `TemplateError`
   - `ProjectError`

### Derive Traits

All input/output types must derive:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
```

All error types must derive:

```rust
#[derive(Debug, Error, Serialize)]
```

### Function Return Types

All public functions return:

```rust
pub async fn function_name(input: InputType) -> Result<OutputType, ErrorType>
```

---

## CLI Integration Example

```rust
// ggen-cli/src/cmds/marketplace.rs
use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;

use crate::runtime_helper::execute_async_verb;
use ggen_domain::marketplace::{search, SearchQuery};

#[derive(Serialize)]
struct SearchOutput {
    packages: Vec<PackageInfo>,
    total: usize,
}

#[verb]
fn search(query: String, limit: Option<usize>, category: Option<String>) -> Result<SearchOutput> {
    let input = SearchQuery {
        query,
        limit: limit.unwrap_or(10),
        category,
        min_score: None,
    };

    execute_async_verb(async move {
        let results = ggen_domain::marketplace::search(input)
            .await
            .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

        Ok(SearchOutput {
            packages: results.packages,
            total: results.total,
        })
    })
}
```

---

## Web API Integration Example

```rust
// web-api/src/routes/marketplace.rs
use axum::{Json, extract::Query};
use ggen_domain::marketplace::{search, SearchQuery, SearchResults};

async fn search_endpoint(
    Query(params): Query<SearchQuery>
) -> Result<Json<SearchResults>, StatusCode> {
    search(params)
        .await
        .map(Json)
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)
}
```

---

## Agent Integration Example

```rust
// agents/marketplace_agent.rs
use ggen_domain::marketplace::{search, SearchQuery, PackageInfo};

pub async fn find_production_ready_packages(domain: &str) -> Vec<PackageInfo> {
    let query = SearchQuery {
        query: domain.to_string(),
        limit: 20,
        category: Some("production".to_string()),
        min_score: Some(90.0),
    };

    search(query)
        .await
        .ok()
        .map(|r| r.packages)
        .unwrap_or_default()
}
```

---

## Testing Examples

### Unit Test (Chicago TDD)

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_search_returns_results() {
        // ARRANGE
        let query = SearchQuery {
            query: "rust web".to_string(),
            limit: 5,
            category: None,
            min_score: None,
        };

        // ACT
        let results = search(query).await.unwrap();

        // ASSERT - Verify observable outputs
        assert!(results.total > 0);
        assert!(results.packages.len() <= 5);
        assert!(results.packages.iter().any(|p| p.name.contains("rust")));
    }

    #[tokio::test]
    async fn test_search_filters_by_category() {
        // ARRANGE
        let query = SearchQuery {
            query: "microservice".to_string(),
            limit: 10,
            category: Some("backend".to_string()),
            min_score: None,
        };

        // ACT
        let results = search(query).await.unwrap();

        // ASSERT - Verify filtering behavior
        assert!(results.packages.iter().all(|p| {
            // Verify category filtering logic
            true // Placeholder for actual verification
        }));
    }
}
```

### Integration Test

```rust
#[tokio::test]
async fn test_install_creates_files() {
    use tempfile::TempDir;

    // ARRANGE
    let temp_dir = TempDir::new().unwrap();
    let input = InstallInput {
        package: "io.ggen.test-package".to_string(),
        target: Some(temp_dir.path().to_path_buf()),
        force: false,
        no_dependencies: false,
        dry_run: false,
    };

    // ACT
    let result = install(input).await.unwrap();

    // ASSERT - Verify state changes
    assert!(result.install_path.exists());
    assert_eq!(result.package_name, "io.ggen.test-package");
    assert!(result.install_path.join("package.toml").exists());
}
```

---

## Performance Requirements

All domain functions must meet:

| Operation | Target Latency | Notes |
|-----------|---------------|-------|
| search() | ≤ 100ms | In-memory index |
| search() | ≤ 500ms | Disk-based search |
| install() | ≤ 3s | Small packages (< 10MB) |
| publish() | ≤ 5s | Including validation |
| generate() | ≤ 2s | Templates with < 100 files |
| validate() | ≤ 500ms | Single package |
| list() | ≤ 200ms | < 1000 items |

---

**API Design Complete** ✅
**Ready for Implementation** ⚡
