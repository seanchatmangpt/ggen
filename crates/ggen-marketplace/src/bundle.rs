//! Bundle types for ergonomic pack aliases.
//!
//! CISO requirement: Bundles are only aliases. Atomic packs are canonical.
//! All bundle expansion must be deterministic and shown before compile.

use crate::atomic::AtomicPackId;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Bundle ID (e.g., "mcp-rust", "a2a-rust-axum").
pub type BundleId = String;

/// Bundle — ergonomic alias that expands to atomic packs.
///
/// CISO requirement: The canonical model must be atomic packs.
/// Composite packs (bundles) may exist only as governed aliases.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Bundle {
    /// Bundle identifier (e.g., "mcp-rust")
    pub id: BundleId,

    /// Human-readable name
    pub name: String,

    /// Atomic packs this bundle expands to (deterministic)
    pub atomic_packs: Vec<AtomicPackId>,

    /// Description
    pub description: String,

    /// Runtime selection (if bundle specifies one)
    ///
    /// CISO requirement: Runtime must be explicit unless bundle encodes one.
    pub explicit_runtime: Option<String>,

    /// Version requirement for this bundle
    pub version_requirement: Option<String>,
}

impl Bundle {
    /// Create a new bundle.
    #[must_use]
    pub fn new(
        id: BundleId,
        name: String,
        atomic_packs: Vec<AtomicPackId>,
        description: String,
    ) -> Self {
        Self {
            id,
            name,
            atomic_packs,
            description,
            explicit_runtime: None,
            version_requirement: None,
        }
    }

    /// Create a bundle with explicit runtime.
    #[must_use]
    pub fn with_runtime(
        id: BundleId,
        name: String,
        atomic_packs: Vec<AtomicPackId>,
        description: String,
        runtime: String,
    ) -> Self {
        Self {
            id,
            name,
            atomic_packs,
            description,
            explicit_runtime: Some(runtime),
            version_requirement: None,
        }
    }

    /// Expand this bundle to its atomic packs (deterministic).
    ///
    /// This is the key operation: all bundles are just aliases for atomic packs.
    #[must_use]
    pub fn expand(&self) -> Vec<AtomicPackId> {
        self.atomic_packs.clone()
    }

    /// Check if this bundle has an explicit runtime.
    #[must_use]
    pub fn has_explicit_runtime(&self) -> bool {
        self.explicit_runtime.is_some()
    }

    /// Get the runtime for this bundle.
    ///
    /// Returns None if no explicit runtime is set.
    #[must_use]
    pub fn runtime(&self) -> Option<&str> {
        self.explicit_runtime.as_deref()
    }

    /// Validate this bundle (check for duplicate atomic packs, etc.).
    ///
    /// Returns an error description if validation fails, None if valid.
    #[must_use]
    pub fn validate(&self) -> Option<String> {
        // Check for duplicate atomic packs
        let mut seen = HashSet::new();
        for pack in &self.atomic_packs {
            if !seen.insert(pack) {
                return Some(format!(
                    "Bundle {} contains duplicate atomic pack: {:?}",
                    self.id, pack
                ));
            }
        }

        // Check that bundle has at least one atomic pack
        if self.atomic_packs.is_empty() {
            return Some(format!("Bundle {} has no atomic packs", self.id));
        }

        None
    }

    /// Get a human-readable expansion of this bundle.
    ///
    /// Example output:
    /// ```text
    /// mcp-rust expands to:
    /// - surface-mcp
    /// - projection-rust
    /// - profile-mcp-server
    /// ```
    #[must_use]
    pub fn expansion_text(&self) -> String {
        let mut text = format!("{} expands to:\n", self.id);
        for pack in &self.atomic_packs {
            text.push_str(&format!("  - {}\n", pack));
        }
        if let Some(runtime) = &self.explicit_runtime {
            text.push_str(&format!("Runtime: {}\n", runtime));
        }
        text
    }
}

/// Common bundle definitions.
///
/// These are the canonical bundles that most users will interact with.
pub struct Bundles;

impl Bundles {
    /// mcp-rust: MCP surface with Rust projection.
    ///
    /// Expands to: surface-mcp, projection-rust, profile-mcp-server
    #[must_use]
    pub fn mcp_rust() -> Bundle {
        Bundle::new(
            "mcp-rust".to_string(),
            "MCP with Rust".to_string(),
            vec![
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::SurfaceMcp,
                    "mcp".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ProjectionRust,
                    "rust".to_string(),
                ),
            ],
            "MCP surface with Rust projection".to_string(),
        )
    }

    /// mcp-rust-stdio: MCP with Rust, stdio runtime.
    ///
    /// Explicit runtime: stdio
    #[must_use]
    pub fn mcp_rust_stdio() -> Bundle {
        Bundle::with_runtime(
            "mcp-rust-stdio".to_string(),
            "MCP with Rust (stdio)".to_string(),
            vec![
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::SurfaceMcp,
                    "mcp".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ProjectionRust,
                    "rust".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::RuntimeStdio,
                    "stdio".to_string(),
                ),
            ],
            "MCP surface with Rust projection and stdio runtime".to_string(),
            "stdio".to_string(),
        )
    }

    /// mcp-rust-axum: MCP with Rust, Axum HTTP runtime.
    ///
    /// Explicit runtime: axum
    #[must_use]
    pub fn mcp_rust_axum() -> Bundle {
        Bundle::with_runtime(
            "mcp-rust-axum".to_string(),
            "MCP with Rust (Axum)".to_string(),
            vec![
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::SurfaceMcp,
                    "mcp".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ProjectionRust,
                    "rust".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::RuntimeAxum,
                    "axum".to_string(),
                ),
            ],
            "MCP surface with Rust projection and Axum HTTP runtime".to_string(),
            "axum".to_string(),
        )
    }

    /// a2a-rust: A2A surface with Rust projection.
    ///
    /// Expands to: surface-a2a, projection-rust
    #[must_use]
    pub fn a2a_rust() -> Bundle {
        Bundle::new(
            "a2a-rust".to_string(),
            "A2A with Rust".to_string(),
            vec![
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::SurfaceA2a,
                    "a2a".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ProjectionRust,
                    "rust".to_string(),
                ),
            ],
            "A2A surface with Rust projection".to_string(),
        )
    }

    /// openapi-rust: OpenAPI contract with Rust projection.
    ///
    /// Note: CISO decided OpenAPI is a contract surface, not language feature.
    /// This is a convenience alias only.
    #[must_use]
    pub fn openapi_rust() -> Bundle {
        Bundle::new(
            "openapi-rust".to_string(),
            "OpenAPI with Rust".to_string(),
            vec![
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ContractOpenapi,
                    "openapi".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ProjectionRust,
                    "rust".to_string(),
                ),
            ],
            "OpenAPI contract with Rust projection".to_string(),
        )
    }

    /// graphql-typescript: GraphQL contract with TypeScript projection.
    #[must_use]
    pub fn graphql_typescript() -> Bundle {
        Bundle::new(
            "graphql-typescript".to_string(),
            "GraphQL with TypeScript".to_string(),
            vec![
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ContractGraphql,
                    "graphql".to_string(),
                ),
                AtomicPackId::new(
                    crate::atomic::AtomicPackClass::ProjectionTypescript,
                    "typescript".to_string(),
                ),
            ],
            "GraphQL contract with TypeScript projection".to_string(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bundle_expansion() {
        let bundle = Bundles::mcp_rust();
        let expanded = bundle.expand();

        assert_eq!(expanded.len(), 2);
        assert_eq!(expanded[0].class, crate::atomic::AtomicPackClass::SurfaceMcp);
        assert_eq!(expanded[1].class, crate::atomic::AtomicPackClass::ProjectionRust);
    }

    #[test]
    fn test_bundle_with_runtime() {
        let bundle = Bundles::mcp_rust_axum();

        assert!(bundle.has_explicit_runtime());
        assert_eq!(bundle.runtime(), Some("axum"));
        assert_eq!(bundle.expand().len(), 3); // Includes runtime
    }

    #[test]
    fn test_bundle_validation_no_duplicates() {
        let bundle = Bundles::mcp_rust();
        assert!(bundle.validate().is_none());
    }

    #[test]
    fn test_bundle_validation_duplicates() {
        let dup_pack = AtomicPackId::new(
            crate::atomic::AtomicPackClass::SurfaceMcp,
            "mcp".to_string(),
        );

        let bundle = Bundle::new(
            "test-dup".to_string(),
            "Test".to_string(),
            vec![dup_pack.clone(), dup_pack],
            "Test".to_string(),
        );

        assert!(bundle.validate().is_some());
        assert!(bundle.validate().unwrap().contains("duplicate"));
    }

    #[test]
    fn test_bundle_validation_empty() {
        let bundle = Bundle::new(
            "test-empty".to_string(),
            "Test".to_string(),
            vec![],
            "Test".to_string(),
        );

        assert!(bundle.validate().is_some());
        assert!(bundle.validate().unwrap().contains("no atomic packs"));
    }

    #[test]
    fn test_expansion_text() {
        let bundle = Bundles::mcp_rust_axum();
        let text = bundle.expansion_text();

        assert!(text.contains("mcp-rust-axum expands to:"));
        assert!(text.contains("surface-mcp"));
        assert!(text.contains("projection-rust"));
        assert!(text.contains("runtime-axum"));
        assert!(text.contains("Runtime: axum"));
    }
}
