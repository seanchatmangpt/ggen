//! Capability registry — resolves capabilities to packs via config-driven approach.
//!
//! This module delegates to `capability_resolve::CapabilityRegistry` and adds
//! hard-fail validation on missing packs.

use crate::packs::capability_resolve::{validate_packs_exist, CapabilityRegistry};
use ggen_core::utils::error::{Error, Result};

/// Resolve a capability to its atomic pack IDs.
///
/// Delegates to `CapabilityRegistry::resolve()` which reads from
//! `.ggen/capabilities.toml` or derives from pack metadata.
///
/// Returns Err if the capability is unknown.
pub async fn resolve_capability_to_packs(
    surface: &str,
    _projection: Option<&str>,
    _runtime: Option<&str>,
) -> Result<Vec<String>> {
    let registry = CapabilityRegistry::load()?;
    let packs = registry.resolve(surface)?;
    Ok(packs)
}

/// Resolve and validate that all required packs exist.
///
/// Returns Err if the capability is unknown OR any required pack is missing.
pub async fn resolve_and_validate_packs(
    surface: &str,
    projection: Option<&str>,
    runtime: Option<&str>,
) -> Result<Vec<String>> {
    let packs = resolve_capability_to_packs(surface, projection, runtime).await?;
    validate_packs_exist(&packs)?;
    Ok(packs)
}
