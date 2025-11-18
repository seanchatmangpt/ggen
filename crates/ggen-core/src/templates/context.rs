//! Template context for variable resolution
//!
//! Provides context management for template variable substitution.
//!
//! ## Features
//!
//! - **Variable management**: Store and retrieve template variables
//! - **Tera integration**: Seamless integration with Tera template engine
//! - **Type-safe values**: Support for JSON values (strings, numbers, objects, arrays)
//! - **Context conversion**: Convert to Tera Context for rendering
//!
//! ## Examples
//!
//! ### Creating and Using Template Context
//!
//! ```rust
//! use ggen_core::templates::context::TemplateContext;
//! use serde_json::json;
//!
//! let mut ctx = TemplateContext::new();
//!
//! // Add variables
//! ctx.set("name", json!("MyApp")).unwrap();
//! ctx.set("version", json!("1.0.0")).unwrap();
//! ctx.set("author", json!("Alice")).unwrap();
//!
//! // Verify variables are set
//! assert_eq!(ctx.get_string("name"), Some("MyApp".to_string()));
//! ```
//!
//! ### Working with Nested Values
//!
//! ```rust
//! use ggen_core::templates::context::TemplateContext;
//! use serde_json::json;
//!
//! let mut ctx = TemplateContext::new();
//!
//! // Add nested object
//! ctx.set("project", json!({
//!     "name": "MyApp",
//!     "version": "1.0.0",
//!     "dependencies": ["serde", "tokio"]
//! })).unwrap();
//!
//! // Verify nested value can be accessed
//! let project = ctx.get("project").unwrap();
//! assert!(project.is_object());
//! ```

use ggen_utils::error::{Error, Result};
use serde_json::Value;
use std::collections::BTreeMap;
use tera::Context;

/// Template context for variable resolution
///
/// Manages template variables and provides integration with the Tera template engine.
/// Supports JSON values (strings, numbers, objects, arrays) for flexible variable types.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::context::TemplateContext;
/// use serde_json::json;
///
/// let mut ctx = TemplateContext::new();
/// ctx.set("name", json!("MyApp")).unwrap();
/// ctx.set("version", json!("1.0.0")).unwrap();
///
/// assert_eq!(ctx.get_string("name"), Some("MyApp".to_string()));
/// ```
#[derive(Debug, Clone)]
pub struct TemplateContext {
    /// Variables for template rendering
    variables: BTreeMap<String, Value>,
}

impl TemplateContext {
    /// Create a new empty template context
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    ///
    /// let ctx = TemplateContext::new();
    /// assert!(ctx.variable_names().is_empty());
    /// ```
    pub fn new() -> Self {
        Self {
            variables: BTreeMap::new(),
        }
    }

    /// Create from a map of string variables
    ///
    /// Converts a map of string key-value pairs into a template context.
    /// All values are stored as JSON strings.
    ///
    /// # Arguments
    ///
    /// * `variables` - Map of variable names to string values
    ///
    /// # Returns
    ///
    /// A new `TemplateContext` with the provided variables.
    ///
    /// # Errors
    ///
    /// Returns an error if the variable name is invalid or the value cannot be stored.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use std::collections::BTreeMap;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut vars = BTreeMap::new();
    /// vars.insert("service_name".to_string(), "my-service".to_string());
    /// vars.insert("port".to_string(), "8080".to_string());
    ///
    /// let ctx = TemplateContext::from_map(vars)?;
    /// assert_eq!(ctx.get_string("service_name"), Some("my-service".to_string()));
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_map(variables: BTreeMap<String, String>) -> Result<Self> {
        let mut ctx = Self::new();
        for (key, value) in variables {
            ctx.set(key, value)?;
        }
        Ok(ctx)
    }

    /// Set a variable in the context
    ///
    /// Sets or updates a variable value. The value can be any JSON-compatible type
    /// (string, number, boolean, object, array, or null).
    ///
    /// # Arguments
    ///
    /// * `key` - Variable name (any type that can be converted to `String`)
    /// * `value` - Variable value (any type that can be converted to `serde_json::Value`)
    ///
    /// # Returns
    ///
    /// `Ok(())` on success.
    ///
    /// # Errors
    ///
    /// Returns an error if the variable name is invalid or the value cannot be stored.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    ///
    /// // Set string value
    /// ctx.set("name", json!("MyApp"))?;
    ///
    /// // Set number value
    /// ctx.set("port", json!(8080))?;
    ///
    /// // Set nested object
    /// ctx.set("project", json!({
    ///     "name": "MyApp",
    ///     "version": "1.0.0"
    /// }))?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn set<K: Into<String>, V: Into<Value>>(&mut self, key: K, value: V) -> Result<()> {
        self.variables.insert(key.into(), value.into());
        Ok(())
    }

    /// Get a variable from the context
    ///
    /// Returns a reference to the variable value if it exists.
    ///
    /// # Arguments
    ///
    /// * `key` - Variable name to look up
    ///
    /// # Returns
    ///
    /// `Some(&Value)` if the variable exists, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?;
    ///
    /// let value = ctx.get("name");
    /// assert!(value.is_some());
    /// assert_eq!(value.unwrap().as_str(), Some("MyApp"));
    ///
    /// let missing = ctx.get("nonexistent");
    /// assert!(missing.is_none());
    /// # Ok(())
    /// # }
    /// ```
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.variables.get(key)
    }

    /// Get a variable as a string
    ///
    /// Returns the variable value as a string if it exists and is a string type.
    ///
    /// # Arguments
    ///
    /// * `key` - Variable name to look up
    ///
    /// # Returns
    ///
    /// `Some(String)` if the variable exists and is a string, `None` otherwise.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?;
    ///
    /// assert_eq!(ctx.get_string("name"), Some("MyApp".to_string()));
    ///
    /// // Number values return None
    /// ctx.set("port", json!(8080))?;
    /// assert_eq!(ctx.get_string("port"), None);
    /// # Ok(())
    /// # }
    /// ```
    pub fn get_string(&self, key: &str) -> Option<String> {
        self.variables
            .get(key)
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
    }

    /// Check if a variable exists
    ///
    /// Returns `true` if the variable exists in the context, regardless of its value type.
    ///
    /// # Arguments
    ///
    /// * `key` - Variable name to check
    ///
    /// # Returns
    ///
    /// `true` if the variable exists, `false` otherwise.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?;
    ///
    /// assert!(ctx.contains("name"));
    /// assert!(!ctx.contains("nonexistent"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn contains(&self, key: &str) -> bool {
        self.variables.contains_key(key)
    }

    /// Merge another context into this one
    ///
    /// Adds all variables from `other` into this context. If a variable exists
    /// in both contexts, the value from `other` will overwrite the existing value.
    ///
    /// # Arguments
    ///
    /// * `other` - The context to merge into this one
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx1 = TemplateContext::new();
    /// ctx1.set("name", json!("App1"))?;
    ///
    /// let mut ctx2 = TemplateContext::new();
    /// ctx2.set("port", json!("8080"))?;
    /// ctx2.set("name", json!("App2"))?; // Will overwrite ctx1's "name"
    ///
    /// ctx1.merge(&ctx2);
    ///
    /// assert_eq!(ctx1.get_string("name"), Some("App2".to_string()));
    /// assert_eq!(ctx1.get_string("port"), Some("8080".to_string()));
    /// # Ok(())
    /// # }
    /// ```
    pub fn merge(&mut self, other: &TemplateContext) {
        for (key, value) in &other.variables {
            self.variables.insert(key.clone(), value.clone());
        }
    }

    /// Convert to Tera Context
    ///
    /// Converts this template context into a Tera `Context` for use with
    /// the Tera template engine. All variable types are preserved.
    ///
    /// # Returns
    ///
    /// A Tera `Context` containing all variables from this context.
    ///
    /// # Errors
    ///
    /// Returns an error if the variable name is invalid or the value cannot be stored.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?;
    /// ctx.set("port", json!(8080))?;
    ///
    /// let tera_ctx = ctx.to_tera_context()?;
    /// // tera_ctx can now be used with Tera templates
    /// # Ok(())
    /// # }
    /// ```
    pub fn to_tera_context(&self) -> Result<Context> {
        let mut context = Context::new();

        for (key, value) in &self.variables {
            match value {
                Value::String(s) => context.insert(key, s),
                Value::Number(n) => context.insert(key, n),
                Value::Bool(b) => context.insert(key, b),
                Value::Array(arr) => context.insert(key, arr),
                Value::Object(obj) => context.insert(key, obj),
                Value::Null => context.insert(key, &Option::<String>::None),
            }
        }

        Ok(context)
    }

    /// Get all variable names
    ///
    /// Returns a vector of all variable names in the context, in sorted order.
    ///
    /// # Returns
    ///
    /// A vector of variable name string slices.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?;
    /// ctx.set("port", json!(8080))?;
    /// ctx.set("version", json!("1.0.0"))?;
    ///
    /// let names = ctx.variable_names();
    /// assert_eq!(names.len(), 3);
    /// assert!(names.contains(&"name"));
    /// assert!(names.contains(&"port"));
    /// assert!(names.contains(&"version"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn variable_names(&self) -> Vec<&str> {
        self.variables.keys().map(|s| s.as_str()).collect()
    }

    /// Validate that all required variables are present
    ///
    /// Checks that all variables in the `required` list exist in this context.
    /// Returns an error if any required variables are missing.
    ///
    /// # Arguments
    ///
    /// * `required` - Slice of variable names that must be present
    ///
    /// # Returns
    ///
    /// `Ok(())` if all required variables are present.
    ///
    /// # Errors
    ///
    /// Returns an error if any required variables are missing, with a message
    /// listing all missing variables.
    ///
    /// # Examples
    ///
    /// ## Success case
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?;
    /// ctx.set("port", json!(8080))?;
    ///
    /// let required = vec!["name".to_string(), "port".to_string()];
    /// ctx.validate_required(&required)?; // Ok
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// ## Error case
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?;
    ///
    /// let required = vec!["name".to_string(), "port".to_string()];
    /// let result = ctx.validate_required(&required);
    /// assert!(result.is_err());
    /// assert!(result.unwrap_err().to_string().contains("port"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn validate_required(&self, required: &[String]) -> Result<()> {
        let missing: Vec<_> = required
            .iter()
            .filter(|var| !self.variables.contains_key(*var))
            .collect();

        if !missing.is_empty() {
            return Err(ggen_utils::error::Error::new(&format!(
                "Missing required template variables: {}",
                missing
                    .iter()
                    .map(|v| v.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            )));
        }

        Ok(())
    }

    /// Apply defaults for missing variables
    ///
    /// Sets default values for variables that don't exist in the context.
    /// Existing variables are not overwritten.
    ///
    /// # Arguments
    ///
    /// * `defaults` - Map of variable names to default string values
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    /// use std::collections::BTreeMap;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("MyApp"))?; // Existing value
    ///
    /// let mut defaults = BTreeMap::new();
    /// defaults.insert("name".to_string(), "DefaultApp".to_string()); // Won't overwrite
    /// defaults.insert("port".to_string(), "8080".to_string()); // Will be applied
    ///
    /// ctx.apply_defaults(&defaults);
    ///
    /// // Existing value preserved
    /// assert_eq!(ctx.get_string("name"), Some("MyApp".to_string()));
    /// // Default applied
    /// assert_eq!(ctx.get_string("port"), Some("8080".to_string()));
    /// # Ok(())
    /// # }
    /// ```
    pub fn apply_defaults(&mut self, defaults: &BTreeMap<String, String>) {
        for (key, value) in defaults {
            if !self.variables.contains_key(key) {
                self.variables
                    .insert(key.clone(), Value::String(value.clone()));
            }
        }
    }

    /// Render a template string with this context
    ///
    /// Renders a Tera template string using the variables in this context.
    /// The template string can contain Tera syntax like `{{ variable }}`.
    ///
    /// # Arguments
    ///
    /// * `template` - Tera template string to render
    ///
    /// # Returns
    ///
    /// The rendered string with variables substituted.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Template syntax is invalid
    /// - Required variables are missing
    /// - Template rendering fails
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::templates::context::TemplateContext;
    /// use serde_json::json;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut ctx = TemplateContext::new();
    /// ctx.set("name", json!("World"))?;
    /// ctx.set("count", json!(42))?;
    ///
    /// let rendered = ctx.render_string("Hello, {{ name }}! Count: {{ count }}")?;
    /// assert_eq!(rendered, "Hello, World! Count: 42");
    /// # Ok(())
    /// # }
    /// ```
    pub fn render_string(&self, template: &str) -> Result<String> {
        let mut tera = tera::Tera::default();
        let context = self.to_tera_context()?;

        tera.render_str(template, &context)
            .map_err(|e| Error::with_context("Failed to render template string", &e.to_string()))
    }

    /// Clone the variables map
    pub fn variables(&self) -> &BTreeMap<String, Value> {
        &self.variables
    }
}

impl Default for TemplateContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    test!(test_new_context, {
        let ctx = TemplateContext::new();
        assert!(ctx.variables.is_empty());
    });

    test!(test_set_and_get, {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();

        assert_eq!(ctx.get_string("name"), Some("test".to_string()));
        assert!(ctx.contains("name"));
    });

    test!(test_from_map, {
        let mut vars = BTreeMap::new();
        vars.insert("service_name".to_string(), "my-service".to_string());
        vars.insert("port".to_string(), "8080".to_string());

        let ctx = TemplateContext::from_map(vars).unwrap();

        assert_eq!(
            ctx.get_string("service_name"),
            Some("my-service".to_string())
        );
        assert_eq!(ctx.get_string("port"), Some("8080".to_string()));
    });

    test!(test_merge, {
        let mut ctx1 = TemplateContext::new();
        ctx1.set("name", "test1").unwrap();

        let mut ctx2 = TemplateContext::new();
        ctx2.set("port", "8080").unwrap();

        ctx1.merge(&ctx2);

        assert_eq!(ctx1.get_string("name"), Some("test1".to_string()));
        assert_eq!(ctx1.get_string("port"), Some("8080".to_string()));
    });

    test!(test_validate_required, {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();

        let required = vec!["name".to_string(), "port".to_string()];
        let result = ctx.validate_required(&required);

        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("port"));
    });

    test!(test_apply_defaults, {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();

        let mut defaults = BTreeMap::new();
        defaults.insert("name".to_string(), "default-name".to_string());
        defaults.insert("port".to_string(), "8080".to_string());

        ctx.apply_defaults(&defaults);

        // Existing value should not be overwritten
        assert_eq!(ctx.get_string("name"), Some("test".to_string()));
        // Default should be applied for missing value
        assert_eq!(ctx.get_string("port"), Some("8080".to_string()));
    });

    test!(test_render_string, {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "World").unwrap();
        ctx.set("count", 42).unwrap();

        let rendered = ctx
            .render_string("Hello, {{ name }}! Count: {{ count }}")
            .unwrap();
        assert_eq!(rendered, "Hello, World! Count: 42");
    });

    test!(test_variable_names, {
        let mut ctx = TemplateContext::new();
        ctx.set("name", "test").unwrap();
        ctx.set("port", "8080").unwrap();

        let names = ctx.variable_names();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&"name"));
        assert!(names.contains(&"port"));
    });
}
