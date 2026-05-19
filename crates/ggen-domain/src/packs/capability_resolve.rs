//! Config-driven capability resolution.
//!
//! Replaces hardcoded match arms with resolution derived from:
//! 1. A capability config file (`.ggen/capabilities.toml`)
//! 2. Fallback: pack metadata tags (scan `capability:<name>` tags)
//!
//! This is the authoritative source for `capability enable` pack resolution.

use crate::packs::metadata::{get_packs_dir, list_packs};
use crate::packs::types::Pack;
use ggen_core::utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// A capability definition mapping surfaces/projections to pack IDs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapabilityDefinition {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: String,
    /// Pack IDs required when this capability is enabled
    pub atomic_packs: Vec<String>,
}

/// Capability registry loaded from config or derived from pack metadata.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct CapabilityRegistry {
    pub capabilities: HashMap<String, CapabilityDefinition>,
}

impl CapabilityRegistry {
    /// Load from a config file if it exists, otherwise derive from pack metadata.
    pub fn load() -> Result<Self> {
        let config_path = PathBuf::from(".ggen/capabilities.toml");
        if config_path.exists() {
            Self::from_config(&config_path)
        } else {
            Self::from_pack_metadata()
        }
    }

    /// Load from explicit config file.
    fn from_config(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        let registry: CapabilityRegistry = toml::from_str(&content)
            .map_err(|e| Error::new(&format!("Failed to parse capabilities config: {}", e)))?;
        Ok(registry)
    }

    /// Derive capabilities from pack metadata tags.
    ///
    /// Scans all available packs and extracts `capability:<name>` tags
    /// to build the capability registry dynamically.
    fn from_pack_metadata() -> Result<Self> {
        let packs = list_packs(None)?;
        let mut capabilities: HashMap<String, CapabilityDefinition> = HashMap::new();

        for pack in &packs {
            for tag in &pack.tags {
                if let Some(cap_name) = tag.strip_prefix("capability:") {
                    let entry = capabilities.entry(cap_name.to_string()).or_insert_with(|| {
                        CapabilityDefinition {
                            id: cap_name.to_string(),
                            name: cap_name_to_title(cap_name),
                            description: format!("{} capability", cap_name),
                            category: infer_category(cap_name),
                            atomic_packs: vec![],
                        }
                    });
                    entry.atomic_packs.push(pack.id.clone());
                }
            }
        }

        // If no capability tags found, use built-in defaults (same as the old hardcoded matches)
        if capabilities.is_empty() {
            capabilities.insert(
                "mcp".to_string(),
                CapabilityDefinition {
                    id: "mcp".to_string(),
                    name: "Model Context Protocol".to_string(),
                    description: "MCP server and tools".to_string(),
                    category: "surface".to_string(),
                    atomic_packs: vec!["mcp-rust".to_string()],
                },
            );
            capabilities.insert(
                "a2a".to_string(),
                CapabilityDefinition {
                    id: "a2a".to_string(),
                    name: "Agent-to-Agent".to_string(),
                    description: "A2A protocol and messaging".to_string(),
                    category: "surface".to_string(),
                    atomic_packs: vec!["a2a-rust".to_string()],
                },
            );
            capabilities.insert(
                "openapi".to_string(),
                CapabilityDefinition {
                    id: "openapi".to_string(),
                    name: "OpenAPI Contract".to_string(),
                    description: "OpenAPI specification and validation".to_string(),
                    category: "contract".to_string(),
                    atomic_packs: vec!["openapi-rust".to_string()],
                },
            );
        }

        Ok(CapabilityRegistry { capabilities })
    }

    /// Resolve a capability to its atomic pack IDs.
    ///
    /// Returns Err if the capability is unknown.
    /// Returns Ok with empty vec if capability exists but has no packs.
    pub fn resolve(&self, surface: &str) -> Result<Vec<String>> {
        self.capabilities
            .get(surface)
            .map(|c| c.atomic_packs.clone())
            .ok_or_else(|| Error::new(&format!("Unknown capability: '{}'", surface)))
    }

    /// List all registered capabilities.
    pub fn list(&self) -> Vec<&CapabilityDefinition> {
        let mut defs: Vec<_> = self.capabilities.values().collect();
        defs.sort_by_key(|d| d.id.clone());
        defs
    }

    /// Get a single capability definition.
    pub fn get(&self, id: &str) -> Option<&CapabilityDefinition> {
        self.capabilities.get(id)
    }
}

fn cap_name_to_title(name: &str) -> String {
    name.split('-').map(|w| {
        let mut c = w.chars();
        match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        }
    }).collect::<Vec<_>>()
    .join(" ")
}

fn infer_category(name: &str) -> String {
    match name {
        n if n.starts_with("surface") => "surface".to_string(),
        n if n.starts_with("projection") => "projection".to_string(),
        n if n.starts_with("contract") => "contract".to_string(),
        _ => "general".to_string(),
    }
}

/// Validate that all resolved packs exist in the pack metadata store.
///
/// Returns Err with the list of missing packs if any are not found.
pub fn validate_packs_exist(pack_ids: &[String]) -> Result<()> {
    let packs_dir = get_packs_dir()?;
    let mut missing = vec![];

    for pack_id in pack_ids {
        let toml_path = packs_dir.join(format!("{}.toml", pack_id));
        if !toml_path.exists() {
            missing.push(pack_id.clone());
        }
    }

    if missing.is_empty() {
        Ok(())
    } else {
        Err(Error::new(&format!(
            "Packs not found: {}",
            missing.join(", ")
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_resolve_known_capability() {
        let registry = CapabilityRegistry::from_pack_metadata().unwrap();
        let packs = registry.resolve("mcp").unwrap();
        assert!(!packs.is_empty());
    }

    #[test]
    fn test_resolve_unknown_capability_fails() {
        let registry = CapabilityRegistry::from_pack_metadata().unwrap();
        let result = registry.resolve("nonexistent-surface");
        assert!(result.is_err());
    }

    #[test]
    fn test_list_returns_sorted_capabilities() {
        let registry = CapabilityRegistry::from_pack_metadata().unwrap();
        let list = registry.list();
        assert!(!list.is_empty());
        // Verify sorted
        for window in list.windows(2) {
            assert!(window[0].id <= window[1].id);
        }
    }

    #[test]
    fn test_cap_name_to_title() {
        assert_eq!(cap_name_to_title("mcp"), "Mcp");
        assert_eq!(cap_name_to_title("model-context-protocol"), "Model Context Protocol");
    }

    #[test]
    fn test_infer_category() {
        assert_eq!(infer_category("surface-mcp"), "surface");
        assert_eq!(infer_category("projection-rust"), "projection");
        assert_eq!(infer_category("contract-openapi"), "contract");
        assert_eq!(infer_category("custom-thing"), "general");
    }
}
