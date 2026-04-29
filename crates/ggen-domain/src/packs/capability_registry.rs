//! Capability Registry: maps (surface, projection, runtime) → real pack IDs
//!
//! This module provides the authoritative mapping from capability descriptors
//! (surface + optional projection + optional runtime) to concrete pack IDs
//! that exist in the marketplace.
//!
//! The registry also validates that each resolved pack actually exists on disk
//! by calling `load_pack_metadata` for each resolved ID.

use crate::packs::metadata::load_pack_metadata;

/// Resolve a capability triple (surface, projection, runtime) to a list of
/// real marketplace pack IDs.
///
/// Returns `Ok(Vec<String>)` containing the resolved pack IDs, or
/// `Err(String)` if the surface is not recognised.
///
/// Pack existence is validated via `load_pack_metadata`.  A pack that is
/// listed in the registry but absent from the local marketplace directory
/// will be reported as a warning in the returned `Ok` value — the function
/// does **not** hard-fail on missing packs so that the CLI can still present
/// install commands to the user.
pub fn resolve_capability_to_packs(
    surface: &str, projection: Option<&str>, runtime: Option<&str>,
) -> Result<Vec<String>, String> {
    // ── Canonical mapping ────────────────────────────────────────────────────
    let pack_ids: Vec<&str> = match surface {
        // MCP surface: all projection/runtime combinations map to mcp-rust
        "mcp" => vec!["mcp-rust"],

        // SOC2 compliance
        "compliance-soc2" => vec!["enterprise-backend"],

        // Web surface
        "web" => match projection {
            Some("fullstack") | Some("full-stack") => vec!["web-fullstack"],
            _ => vec!["web-fullstack"],
        },

        // DevOps surface
        "devops" => vec!["devops-automation"],

        // Data science surface
        "data-science" => vec!["data-science-toolkit"],

        // Startup surface
        "startup" => vec!["startup-essentials"],

        // Enterprise backend (direct)
        "enterprise" | "enterprise-backend" => vec!["enterprise-backend"],

        // Unknown surface
        _ => {
            return Err(format!(
                "Unknown capability surface '{}'. \
                 Available surfaces: mcp, compliance-soc2, web, devops, \
                 data-science, startup, enterprise",
                surface
            ));
        }
    };

    // ── Validate packs exist ─────────────────────────────────────────────────
    // We attempt to load each pack's metadata.  Missing packs are warned about
    // but do not cause the function to fail — the caller is expected to surface
    // install instructions to the user.
    let mut resolved: Vec<String> = Vec::with_capacity(pack_ids.len());

    for &pack_id in &pack_ids {
        match load_pack_metadata(pack_id) {
            Ok(_) => {
                tracing::debug!(pack_id = pack_id, "capability_registry: pack validated");
            }
            Err(e) => {
                // Pack not found locally — still include it so the CLI can
                // emit `ggen packs add <pack_id>` install commands.
                tracing::warn!(
                    pack_id = pack_id,
                    error = %e,
                    "capability_registry: pack not found locally (not yet installed)"
                );
            }
        }
        resolved.push(pack_id.to_string());
    }

    let _ = runtime; // runtime parameter reserved for future routing
    Ok(resolved)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcp_resolves_to_mcp_rust() {
        let result = resolve_capability_to_packs("mcp", Some("rust"), Some("axum"));
        // Result may be Ok or contain pack ids regardless of local filesystem
        assert!(result.is_ok());
        let packs = result.unwrap();
        assert_eq!(packs, vec!["mcp-rust"]);
    }

    #[test]
    fn test_mcp_projection_none_resolves_to_mcp_rust() {
        let packs = resolve_capability_to_packs("mcp", None, None).unwrap();
        assert_eq!(packs, vec!["mcp-rust"]);
    }

    #[test]
    fn test_compliance_soc2_resolves_to_enterprise_backend() {
        let packs = resolve_capability_to_packs("compliance-soc2", None, None).unwrap();
        assert_eq!(packs, vec!["enterprise-backend"]);
    }

    #[test]
    fn test_web_fullstack_resolves() {
        let packs = resolve_capability_to_packs("web", Some("fullstack"), None).unwrap();
        assert_eq!(packs, vec!["web-fullstack"]);
    }

    #[test]
    fn test_devops_resolves() {
        let packs = resolve_capability_to_packs("devops", None, None).unwrap();
        assert_eq!(packs, vec!["devops-automation"]);
    }

    #[test]
    fn test_data_science_resolves() {
        let packs = resolve_capability_to_packs("data-science", None, None).unwrap();
        assert_eq!(packs, vec!["data-science-toolkit"]);
    }

    #[test]
    fn test_startup_resolves() {
        let packs = resolve_capability_to_packs("startup", None, None).unwrap();
        assert_eq!(packs, vec!["startup-essentials"]);
    }

    #[test]
    fn test_unknown_surface_returns_error() {
        let result = resolve_capability_to_packs("unknown-surface", None, None);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("unknown-surface"));
    }
}
