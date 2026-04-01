//! Atomic pack taxonomy for governed marketplace.
//!
//! Implements Fortune 5 CISO requirements for atomic pack classification:
//! - Surface/contract before projection
//! - Atomic packs are canonical
//! - Bundles are only aliases

use serde::{Deserialize, Serialize};
use std::fmt;
use std::str::FromStr;

/// Atomic pack class (the "what" of the pack).
///
/// Surface and contract types come first (CISO requirement: enterprise-visible
/// interface before implementation language).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AtomicPackClass {
    // ===== SURFACES (enterprise-visible interfaces) =====

    /// MCP (Model Context Protocol) surface
    #[serde(rename = "surface-mcp")]
    SurfaceMcp,

    /// A2A (Agent-to-Agent) surface
    #[serde(rename = "surface-a2a")]
    SurfaceA2a,

    // ===== CONTRACTS (external API contracts) =====

    /// OpenAPI contract surface
    #[serde(rename = "contract-openapi")]
    ContractOpenapi,

    /// GraphQL contract surface
    #[serde(rename = "contract-graphql")]
    ContractGraphql,

    // ===== PROJECTIONS (implementation languages) =====

    /// Rust projection
    #[serde(rename = "projection-rust")]
    ProjectionRust,

    /// TypeScript projection
    #[serde(rename = "projection-typescript")]
    ProjectionTypescript,

    /// Python projection
    #[serde(rename = "projection-python")]
    ProjectionPython,

    /// Java 26 projection
    #[serde(rename = "projection-java")]
    ProjectionJava,

    /// Go projection
    #[serde(rename = "projection-go")]
    ProjectionGo,

    // ===== RUNTIMES (deployment models) =====

    /// stdio runtime
    #[serde(rename = "runtime-stdio")]
    RuntimeStdio,

    /// Axum HTTP server runtime
    #[serde(rename = "runtime-axum")]
    RuntimeAxum,

    /// Actix Web HTTP server runtime
    #[serde(rename = "runtime-actix")]
    RuntimeActix,

    /// Embedded library runtime
    #[serde(rename = "runtime-embedded")]
    RuntimeEmbedded,

    /// Standalone binary runtime
    #[serde(rename = "runtime-standalone")]
    RuntimeStandalone,

    // ===== POLICY (governance rules) =====

    /// No defaults policy (forbid all inferred capabilities)
    #[serde(rename = "policy-no-defaults")]
    PolicyNoDefaults,

    /// Strict validation policy
    #[serde(rename = "policy-strict")]
    PolicyStrict,

    // ===== VALIDATORS (quality gates) =====

    /// Protocol-visible values validator
    #[serde(rename = "validator-protocol-visible-values")]
    ValidatorProtocolVisibleValues,

    /// SHACL validator
    #[serde(rename = "validator-shacl")]
    ValidatorShacl,

    // ===== RECEIPTS (proof formats) =====

    /// Enterprise-signed receipts
    #[serde(rename = "receipt-enterprise-signed")]
    ReceiptEnterpriseSigned,

    /// Chained receipts (hash-linked)
    #[serde(rename = "receipt-chained")]
    ReceiptChained,

    // ===== CONSEQUENCES (migration/upgrade behavior) =====

    /// Semver migration consequence
    #[serde(rename = "consequence-semver-migration")]
    ConsequenceSemverMigration,

    /// Breaking change consequence
    #[serde(rename = "consequence-breaking-change")]
    ConsequenceBreakingChange,

    // ===== CORE (foundation infrastructure) =====

    /// Core ontology
    #[serde(rename = "core-ontology")]
    CoreOntology,

    /// Core hooks
    #[serde(rename = "core-hooks")]
    CoreHooks,

    /// Core receipts
    #[serde(rename = "core-receipts")]
    CoreReceipts,

    /// Core versioning
    #[serde(rename = "core-versioning")]
    CoreVersioning,

    /// Core validation
    #[serde(rename = "core-validation")]
    CoreValidation,

    /// Core policy
    #[serde(rename = "core-policy")]
    CorePolicy,
}

impl AtomicPackClass {
    /// Get the category of this atomic pack class.
    #[must_use]
    pub const fn category(&self) -> AtomicPackCategory {
        match self {
            Self::SurfaceMcp | Self::SurfaceA2a => AtomicPackCategory::Surface,
            Self::ContractOpenapi | Self::ContractGraphql => AtomicPackCategory::Contract,
            Self::ProjectionRust
            | Self::ProjectionTypescript
            | Self::ProjectionPython
            | Self::ProjectionJava
            | Self::ProjectionGo => AtomicPackCategory::Projection,
            Self::RuntimeStdio
            | Self::RuntimeAxum
            | Self::RuntimeActix
            | Self::RuntimeEmbedded
            | Self::RuntimeStandalone => AtomicPackCategory::Runtime,
            Self::PolicyNoDefaults | Self::PolicyStrict => AtomicPackCategory::Policy,
            Self::ValidatorProtocolVisibleValues | Self::ValidatorShacl => {
                AtomicPackCategory::Validator
            }
            Self::ReceiptEnterpriseSigned | Self::ReceiptChained => AtomicPackCategory::Receipt,
            Self::ConsequenceSemverMigration | Self::ConsequenceBreakingChange => {
                AtomicPackCategory::Consequence
            }
            Self::CoreOntology
            | Self::CoreHooks
            | Self::CoreReceipts
            | Self::CoreVersioning
            | Self::CoreValidation
            | Self::CorePolicy => AtomicPackCategory::Core,
        }
    }

    /// Check if this is a foundation (core) pack.
    #[must_use]
    pub const fn is_foundation(&self) -> bool {
        matches!(self.category(), AtomicPackCategory::Core)
    }

    /// Check if this is a surface/contract pack (enterprise-visible interface).
    #[must_use]
    pub const fn is_interface(&self) -> bool {
        matches!(
            self.category(),
            AtomicPackCategory::Surface | AtomicPackCategory::Contract
        )
    }
}

/// Atomic pack category groupings.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AtomicPackCategory {
    Surface,
    Contract,
    Projection,
    Runtime,
    Policy,
    Validator,
    Receipt,
    Consequence,
    Core,
}

/// Atomic pack identifier (e.g., "surface-mcp", "projection-rust", "runtime-axum").
///
/// The canonical semantic unit. All bundles expand to these.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct AtomicPackId {
    /// The class of the atomic pack
    pub class: AtomicPackClass,

    /// Specific name (e.g., for surface-mcp, name is "mcp")
    pub name: String,
}

impl AtomicPackId {
    /// Create a new atomic pack ID.
    #[must_use]
    pub fn new(class: AtomicPackClass, name: String) -> Self {
        Self { class, name }
    }

    /// Create from string (e.g., "surface-mcp").
    ///
    /// Returns None if the string is not a valid atomic pack ID.
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        let (class_str, name) = s.split_once('-')?;
        let class = match class_str {
            "surface" => match name {
                "mcp" => Some(AtomicPackClass::SurfaceMcp),
                "a2a" => Some(AtomicPackClass::SurfaceA2a),
                _ => return None,
            },
            "contract" => match name {
                "openapi" => Some(AtomicPackClass::ContractOpenapi),
                "graphql" => Some(AtomicPackClass::ContractGraphql),
                _ => return None,
            },
            "projection" => match name {
                "rust" => Some(AtomicPackClass::ProjectionRust),
                "typescript" | "ts" => Some(AtomicPackClass::ProjectionTypescript),
                "python" => Some(AtomicPackClass::ProjectionPython),
                "java" => Some(AtomicPackClass::ProjectionJava),
                "go" => Some(AtomicPackClass::ProjectionGo),
                _ => return None,
            },
            "runtime" => match name {
                "stdio" => Some(AtomicPackClass::RuntimeStdio),
                "axum" => Some(AtomicPackClass::RuntimeAxum),
                "actix" => Some(AtomicPackClass::RuntimeActix),
                "embedded" => Some(AtomicPackClass::RuntimeEmbedded),
                "standalone" => Some(AtomicPackClass::RuntimeStandalone),
                _ => return None,
            },
            "policy" => match name {
                "no-defaults" => Some(AtomicPackClass::PolicyNoDefaults),
                "strict" => Some(AtomicPackClass::PolicyStrict),
                _ => return None,
            },
            "validator" => match name {
                "protocol-visible-values" => Some(AtomicPackClass::ValidatorProtocolVisibleValues),
                "shacl" => Some(AtomicPackClass::ValidatorShacl),
                _ => return None,
            },
            "receipt" => match name {
                "enterprise-signed" => Some(AtomicPackClass::ReceiptEnterpriseSigned),
                "chained" => Some(AtomicPackClass::ReceiptChained),
                _ => return None,
            },
            "consequence" => match name {
                "semver-migration" => Some(AtomicPackClass::ConsequenceSemverMigration),
                "breaking-change" => Some(AtomicPackClass::ConsequenceBreakingChange),
                _ => return None,
            },
            "core" => match name {
                "ontology" => Some(AtomicPackClass::CoreOntology),
                "hooks" => Some(AtomicPackClass::CoreHooks),
                "receipts" => Some(AtomicPackClass::CoreReceipts),
                "versioning" => Some(AtomicPackClass::CoreVersioning),
                "validation" => Some(AtomicPackClass::CoreValidation),
                "policy" => Some(AtomicPackClass::CorePolicy),
                _ => return None,
            },
            _ => return None,
        };

        class.map(|class| Self {
            class,
            name: name.to_string(),
        })
    }

    /// Get the string representation of this atomic pack ID.
    #[must_use]
    pub fn as_str(&self) -> String {
        let class_str = match self.class {
            AtomicPackClass::SurfaceMcp => "surface-mcp",
            AtomicPackClass::SurfaceA2a => "surface-a2a",
            AtomicPackClass::ContractOpenapi => "contract-openapi",
            AtomicPackClass::ContractGraphql => "contract-graphql",
            AtomicPackClass::ProjectionRust => "projection-rust",
            AtomicPackClass::ProjectionTypescript => "projection-typescript",
            AtomicPackClass::ProjectionPython => "projection-python",
            AtomicPackClass::ProjectionJava => "projection-java",
            AtomicPackClass::ProjectionGo => "projection-go",
            AtomicPackClass::RuntimeStdio => "runtime-stdio",
            AtomicPackClass::RuntimeAxum => "runtime-axum",
            AtomicPackClass::RuntimeActix => "runtime-actix",
            AtomicPackClass::RuntimeEmbedded => "runtime-embedded",
            AtomicPackClass::RuntimeStandalone => "runtime-standalone",
            AtomicPackClass::PolicyNoDefaults => "policy-no-defaults",
            AtomicPackClass::PolicyStrict => "policy-strict",
            AtomicPackClass::ValidatorProtocolVisibleValues => "validator-protocol-visible-values",
            AtomicPackClass::ValidatorShacl => "validator-shacl",
            AtomicPackClass::ReceiptEnterpriseSigned => "receipt-enterprise-signed",
            AtomicPackClass::ReceiptChained => "receipt-chained",
            AtomicPackClass::ConsequenceSemverMigration => "consequence-semver-migration",
            AtomicPackClass::ConsequenceBreakingChange => "consequence-breaking-change",
            AtomicPackClass::CoreOntology => "core-ontology",
            AtomicPackClass::CoreHooks => "core-hooks",
            AtomicPackClass::CoreReceipts => "core-receipts",
            AtomicPackClass::CoreVersioning => "core-versioning",
            AtomicPackClass::CoreValidation => "core-validation",
            AtomicPackClass::CorePolicy => "core-policy",
        };
        format!("{}-{}", class_str, self.name)
    }
}

impl fmt::Display for AtomicPackId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl FromStr for AtomicPackId {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::from_str(s).ok_or_else(|| format!("Invalid atomic pack ID: {}", s))
    }
}

/// Foundation packs that own shared ontology.
///
/// CISO requirement: Shared ontology must be owned by foundation packs,
/// not duplicated across feature packs.
pub fn foundation_packs() -> Vec<AtomicPackId> {
    vec![
        AtomicPackId::new(AtomicPackClass::CoreOntology, "ontology".to_string()),
        AtomicPackId::new(AtomicPackClass::CoreHooks, "hooks".to_string()),
        AtomicPackId::new(AtomicPackClass::CoreReceipts, "receipts".to_string()),
        AtomicPackId::new(AtomicPackClass::CoreVersioning, "versioning".to_string()),
        AtomicPackId::new(AtomicPackClass::CoreValidation, "validation".to_string()),
        AtomicPackId::new(AtomicPackClass::CorePolicy, "policy".to_string()),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_atomic_pack_id_from_str() {
        let mcp_surface = AtomicPackId::from_str("surface-mcp").unwrap();
        assert_eq!(mcp_surface.class, AtomicPackClass::SurfaceMcp);
        assert_eq!(mcp_surface.name, "mcp");

        let rust_projection = AtomicPackId::from_str("projection-rust").unwrap();
        assert_eq!(rust_projection.class, AtomicPackClass::ProjectionRust);
        assert_eq!(rust_projection.name, "rust");

        let axum_runtime = AtomicPackId::from_str("runtime-axum").unwrap();
        assert_eq!(axum_runtime.class, AtomicPackClass::RuntimeAxum);
        assert_eq!(axum_runtime.name, "axum");
    }

    #[test]
    fn test_atomic_pack_id_display() {
        let pack = AtomicPackId::new(AtomicPackClass::SurfaceMcp, "mcp".to_string());
        assert_eq!(pack.to_string(), "surface-mcp-mcp");

        let pack = AtomicPackId::new(AtomicPackClass::ProjectionRust, "rust".to_string());
        assert_eq!(pack.to_string(), "projection-rust-rust");
    }

    #[test]
    fn test_surface_packs_are_interfaces() {
        assert!(AtomicPackClass::SurfaceMcp.is_interface());
        assert!(AtomicPackClass::SurfaceA2a.is_interface());
        assert!(AtomicPackClass::ContractOpenapi.is_interface());
        assert!(!AtomicPackClass::ProjectionRust.is_interface());
        assert!(!AtomicPackClass::RuntimeAxum.is_interface());
    }

    #[test]
    fn test_foundation_packs() {
        assert!(AtomicPackClass::CoreOntology.is_foundation());
        assert!(AtomicPackClass::CoreHooks.is_foundation());
        assert!(!AtomicPackClass::SurfaceMcp.is_foundation());
        assert!(!AtomicPackClass::ProjectionRust.is_foundation());
    }

    #[test]
    fn test_foundation_packs_const() {
        let packs = foundation_packs();
        assert_eq!(packs.len(), 6);
        assert!(packs.iter().all(|p| p.class.is_foundation()));
    }

    #[test]
    fn test_invalid_pack_id() {
        assert!(AtomicPackId::from_str("invalid-pack").is_none());
        assert!(AtomicPackId::from_str("surface-invalid").is_none());
        assert!(AtomicPackId::from_str("projection-elixir").is_none()); // elixir not supported
    }
}
