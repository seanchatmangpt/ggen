//! Business logic separation for CLI commands
//!
//! This module provides functionality for separating CLI layer (thin wrapper)
//! from business logic (domain implementation) to enable easier maintenance
//! and testing. This follows the 2026 best practice of keeping CLI layers thin
//! and business logic in a separate domain layer.
//!
//! ## Features
//!
//! - **CLI Wrapper Generation**: Creates thin synchronous wrappers for async business logic
//! - **Domain Skeleton Generation**: Generates async business logic templates
//! - **Non-Destructive**: Never overwrites existing business logic files
//! - **Frozen Sections**: Uses frozen tags to protect user code
//! - **Type Safety**: Generates type-safe argument structures
//! - **Test Templates**: Includes test boilerplate for business logic
//!
//! ## Architecture
//!
//! The separation creates two layers:
//!
//! 1. **CLI Layer** (`cli/`): Thin synchronous wrappers that:
//!    - Parse CLI arguments using clap
//!    - Create a Tokio runtime
//!    - Call async business logic
//!    - Handle errors and output
//!
//! 2. **Domain Layer** (`domain/`): Async business logic that:
//!    - Contains actual implementation
//!    - Is never overwritten once created
//!    - Can be freely modified by users
//!    - Includes frozen sections for regeneration safety
//!
//! ## Examples
//!
//! ### Generating Separated Files
//!
//! ```rust,no_run
//! use ggen_core::templates::business_logic::BusinessLogicSeparator;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let cli_path = Path::new("cli/create_project.rs");
//! let domain_path = Path::new("domain/create_project.rs");
//!
//! BusinessLogicSeparator::generate_separated_files(
//!     cli_path,
//!     domain_path,
//!     "create",
//!     "project",
//!     false, // Don't overwrite existing domain file
//! )?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Generated CLI Wrapper
//!
//! The CLI wrapper will look like:
//!
//! ```rust,no_run
//! //! CLI wrapper for create project
//! use ggen_utils::error::Result;
//! use clap::Args;
//! use crate::domain::create_project;
//!
//! #[derive(Debug, Args)]
//! pub struct CreateProjectArgs {
//!     // CLI arguments here
//! }
//!
//! #[create]
//! pub fn create_project(args: CreateProjectArgs) -> Result<()> {
//!     tokio::runtime::Runtime::new()?.block_on(async {
//!         create_project(args).await
//!     })
//! }
//! ```
//!
//! ### Generated Domain Logic
//!
//! The domain logic will look like:
//!
//! ```rust,no_run
//! //! Business logic for create project
//! use ggen_utils::error::Result;
//!
//! #[derive(Debug)]
//! pub struct CreateProjectArgs {
//!     // Business logic arguments
//! }
//!
//! pub async fn create_project(args: CreateProjectArgs) -> Result<()> {
//!     {% frozen id="implementation" %}
//!     // User's implementation here
//!     {% endfrozen %}
//! }
//! ```

use ggen_utils::error::Result;
use std::fs;
use std::path::Path;

/// Generator for separated business logic files
///
/// Generates code that separates CLI layer (thin wrapper) from business logic
/// (domain implementation), following 2026 best practices for maintainable CLI projects.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::templates::business_logic::BusinessLogicSeparator;
/// use std::path::Path;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let cli_path = Path::new("cli/create_project.rs");
/// let domain_path = Path::new("domain/create_project.rs");
///
/// BusinessLogicSeparator::generate_separated_files(
///     cli_path,
///     domain_path,
///     "create",
///     "project",
///     false,
/// )?;
/// # Ok(())
/// # }
/// ```
pub struct BusinessLogicSeparator;

impl BusinessLogicSeparator {
    /// Generate CLI wrapper file (sync wrapper calling async business logic)
    ///
    /// Creates a thin CLI layer with `#[verb]` attribute that delegates to
    /// async business logic in the domain layer.
    ///
    /// # Arguments
    /// * `verb` - The CLI verb (e.g., "create", "delete", "list")
    /// * `noun` - The CLI noun (e.g., "project", "user", "task")
    /// * `module_path` - Optional module path for business logic (defaults to "domain")
    pub fn generate_cli_wrapper(verb: &str, noun: &str, module_path: Option<&str>) -> String {
        let module = module_path.unwrap_or("domain");
        let struct_name = Self::to_pascal_case(&format!("{}-{}", verb, noun));
        let function_name = Self::to_snake_case(&format!("{}_{}", verb, noun));

        format!(
            r#"//! CLI wrapper for {} {}
//!
//! This is a thin synchronous wrapper that delegates to async business logic.

use ggen_utils::error::Result;
use clap::Args;
use crate::{}::{};

/// CLI arguments for {} {}
#[derive(Debug, Args)]
pub struct {}Args {{
    // FUTURE: Add CLI arguments here
}}

/// {} {} command (sync wrapper)
#[{}]
pub fn {}(args: {}Args) -> Result<()> {{
    // Delegate to async business logic
    tokio::runtime::Runtime::new()?.block_on(async {{
        {}(args).await
    }})
}}
"#,
            verb,
            noun,
            module,
            function_name,
            verb,
            noun,
            struct_name,
            verb,
            noun,
            verb,
            function_name,
            struct_name,
            function_name
        )
    }

    /// Generate business logic skeleton (only if doesn't exist)
    ///
    /// Creates an async business logic template in the domain layer with
    /// frozen sections to protect user code. This file is NEVER overwritten
    /// once it exists, allowing users to freely modify business logic.
    ///
    /// # Arguments
    ///
    /// * `verb` - The CLI verb (e.g., "create", "delete", "list")
    /// * `noun` - The CLI noun (e.g., "project", "user", "task")
    ///
    /// # Returns
    ///
    /// Generated Rust code as a string containing the async business logic skeleton
    /// with frozen sections for safe regeneration.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::templates::business_logic::BusinessLogicSeparator;
    ///
    /// let code = BusinessLogicSeparator::generate_domain_skeleton("delete", "user");
    /// assert!(code.contains("pub async fn delete_user"));
    /// assert!(code.contains("{% frozen"));
    /// assert!(code.contains("#[tokio::test]"));
    /// ```
    pub fn generate_domain_skeleton(verb: &str, noun: &str) -> String {
        let function_name = Self::to_snake_case(&format!("{}_{}", verb, noun));
        let struct_name = Self::to_pascal_case(&format!("{}-{}", verb, noun));

        let template = format!(
            r#"//! Business logic for {} {}
//!
//! This module contains the async business logic implementation.
//! Modify this file freely - it will never be regenerated.

use ggen_utils::error::Result;

/// Arguments for {} {} operation
#[derive(Debug)]
pub struct {}Args {{
    // FUTURE: Add business logic arguments here
}}

/// {} {} business logic (async)
pub async fn {}(args: {}Args) -> Result<()> {{
    // Default implementation - replace with your logic
    // TODO: Implement actual business logic here
    Ok(())
}}

#[cfg(test)]
mod tests {{
    use super::*;

    #[tokio::test]
    async fn test_{}() {{
        let args = {}Args {{}};
        let result = {}(args).await;
        assert!(result.is_ok());
    }}
}}
"#,
            verb,
            noun,
            verb,
            noun,
            struct_name,
            verb,
            noun,
            function_name,
            struct_name,
            function_name,
            struct_name,
            function_name
        );

        // Add frozen tags after format to avoid format string conflicts
        template
            .replace(
                &format!("/// {} {} business logic (async)", verb, noun),
                &format!("/// {} {} business logic (async)\n///\n/// {{% frozen id=\"business_logic\" %}}\n/// FUTURE: Implement business logic here\n/// {{% endfrozen %}}", verb, noun)
            )
            .replace(
                "// Default implementation - replace with your logic",
                "{{% frozen id=\"implementation\" %}}\n    // Default implementation - replace with your logic"
            )
            .replace(
                &format!("println!(\"Executing {} {} with args: {{{{:?}}}}\", args);\n    Ok(())", verb, noun),
                &format!("println!(\"Executing {} {} with args: {{{{:?}}}}\", args);\n    Ok(())\n    {{% endfrozen %}}", verb, noun)
            )
    }

    /// Check if business logic file exists
    ///
    /// Returns `true` if the business logic file already exists and should
    /// NOT be overwritten. This prevents accidental loss of user code.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the business logic file to check
    ///
    /// # Returns
    ///
    /// `true` if the file exists, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::templates::business_logic::BusinessLogicSeparator;
    /// use std::path::Path;
    ///
    /// let exists = BusinessLogicSeparator::business_logic_exists(Path::new("domain/create.rs"));
    /// if !exists {
    ///     // Safe to generate new file
    /// }
    /// ```
    pub fn business_logic_exists(path: &Path) -> bool {
        path.exists()
    }

    /// Generate both CLI wrapper and domain logic files
    ///
    /// Generates the complete separation: a CLI wrapper file and a domain logic file.
    /// The CLI wrapper is always generated (it's thin and safe to regenerate).
    /// The domain logic file is only generated if it doesn't exist, unless `force_domain`
    /// is `true` (which is dangerous and should be used with caution).
    ///
    /// # Arguments
    ///
    /// * `cli_path` - Path for CLI wrapper file (always generated)
    /// * `domain_path` - Path for business logic file (only if missing or forced)
    /// * `verb` - The CLI verb (e.g., "create", "delete", "list")
    /// * `noun` - The CLI noun (e.g., "project", "user", "task")
    /// * `force_domain` - Force overwrite of domain file (dangerous! Use with caution)
    ///
    /// # Errors
    ///
    /// Returns an error if file system operations fail.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::templates::business_logic::BusinessLogicSeparator;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// BusinessLogicSeparator::generate_separated_files(
    ///     Path::new("cli/create_project.rs"),
    ///     Path::new("domain/create_project.rs"),
    ///     "create",
    ///     "project",
    ///     false, // Don't overwrite existing domain file
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn generate_separated_files(
        cli_path: &Path, domain_path: &Path, verb: &str, noun: &str, force_domain: bool,
    ) -> Result<()> {
        // Always generate CLI wrapper (it's thin and can be regenerated)
        let cli_content = Self::generate_cli_wrapper(verb, noun, None);
        if let Some(parent) = cli_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(cli_path, cli_content)?;

        // Only generate domain skeleton if it doesn't exist or force is true
        if force_domain || !Self::business_logic_exists(domain_path) {
            let domain_content = Self::generate_domain_skeleton(verb, noun);
            if let Some(parent) = domain_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(domain_path, domain_content)?;
        }

        Ok(())
    }

    /// Convert string to PascalCase
    fn to_pascal_case(s: &str) -> String {
        s.split(|c: char| !c.is_alphanumeric())
            .filter(|s| !s.is_empty())
            .map(|word| {
                let mut chars = word.chars();
                match chars.next() {
                    None => String::new(),
                    Some(first) => first.to_uppercase().chain(chars).collect(),
                }
            })
            .collect()
    }

    /// Convert string to snake_case
    fn to_snake_case(s: &str) -> String {
        s.split(|c: char| !c.is_alphanumeric())
            .filter(|s| !s.is_empty())
            .map(|s| s.to_lowercase())
            .collect::<Vec<_>>()
            .join("_")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_generate_cli_wrapper() {
        let cli_code = BusinessLogicSeparator::generate_cli_wrapper("create", "project", None);

        assert!(cli_code.contains("CreateProjectArgs"));
        assert!(cli_code.contains("#[create]"));
        assert!(cli_code.contains("pub fn create_project"));
        assert!(cli_code.contains("use crate::domain::create_project"));
    }

    #[test]
    fn test_generate_domain_skeleton() {
        let domain_code = BusinessLogicSeparator::generate_domain_skeleton("delete", "user");

        assert!(domain_code.contains("DeleteUserArgs"));
        assert!(domain_code.contains("pub async fn delete_user"));
        assert!(domain_code.contains("{% frozen id=\"business_logic\" %}"));
        assert!(domain_code.contains("#[tokio::test]"));
    }

    #[test]
    fn test_business_logic_exists() {
        let temp_dir = TempDir::new().unwrap();
        let existing_file = temp_dir.path().join("existing.rs");
        let nonexistent_file = temp_dir.path().join("nonexistent.rs");

        fs::write(&existing_file, "content").unwrap();

        assert!(BusinessLogicSeparator::business_logic_exists(
            &existing_file
        ));
        assert!(!BusinessLogicSeparator::business_logic_exists(
            &nonexistent_file
        ));
    }

    #[test]
    fn test_generate_separated_files() {
        let temp_dir = TempDir::new().unwrap();
        let cli_path = temp_dir.path().join("cli/list_task.rs");
        let domain_path = temp_dir.path().join("domain/list_task.rs");

        let result = BusinessLogicSeparator::generate_separated_files(
            &cli_path,
            &domain_path,
            "list",
            "task",
            false,
        );

        assert!(result.is_ok());
        assert!(cli_path.exists());
        assert!(domain_path.exists());

        let cli_content = fs::read_to_string(&cli_path).unwrap();
        assert!(cli_content.contains("ListTaskArgs"));

        let domain_content = fs::read_to_string(&domain_path).unwrap();
        assert!(domain_content.contains("pub async fn list_task"));
    }

    #[test]
    fn test_no_overwrite_existing_domain() {
        let temp_dir = TempDir::new().unwrap();
        let cli_path = temp_dir.path().join("cli/update_file.rs");
        let domain_path = temp_dir.path().join("domain/update_file.rs");

        // Create existing domain file with custom content
        fs::create_dir_all(domain_path.parent().unwrap()).unwrap();
        fs::write(&domain_path, "// My custom implementation").unwrap();

        let result = BusinessLogicSeparator::generate_separated_files(
            &cli_path,
            &domain_path,
            "update",
            "file",
            false, // force_domain = false
        );

        assert!(result.is_ok());

        // Domain file should NOT be overwritten
        let domain_content = fs::read_to_string(&domain_path).unwrap();
        assert!(domain_content.contains("// My custom implementation"));
        assert!(!domain_content.contains("pub async fn update_file"));
    }

    #[test]
    fn test_force_overwrite_domain() {
        let temp_dir = TempDir::new().unwrap();
        let cli_path = temp_dir.path().join("cli/update_file.rs");
        let domain_path = temp_dir.path().join("domain/update_file.rs");

        // Create existing domain file
        fs::create_dir_all(domain_path.parent().unwrap()).unwrap();
        fs::write(&domain_path, "// Old implementation").unwrap();

        let result = BusinessLogicSeparator::generate_separated_files(
            &cli_path,
            &domain_path,
            "update",
            "file",
            true, // force_domain = true
        );

        assert!(result.is_ok());

        // Domain file SHOULD be overwritten
        let domain_content = fs::read_to_string(&domain_path).unwrap();
        assert!(!domain_content.contains("// Old implementation"));
        assert!(domain_content.contains("pub async fn update_file"));
    }

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(
            BusinessLogicSeparator::to_pascal_case("create-project"),
            "CreateProject"
        );
        assert_eq!(
            BusinessLogicSeparator::to_pascal_case("delete_user"),
            "DeleteUser"
        );
        assert_eq!(
            BusinessLogicSeparator::to_pascal_case("list-all-tasks"),
            "ListAllTasks"
        );
    }

    #[test]
    fn test_to_snake_case() {
        assert_eq!(
            BusinessLogicSeparator::to_snake_case("CreateProject"),
            "createproject"
        );
        assert_eq!(
            BusinessLogicSeparator::to_snake_case("delete-user"),
            "delete_user"
        );
        assert_eq!(
            BusinessLogicSeparator::to_snake_case("list-all-tasks"),
            "list_all_tasks"
        );
    }
}
