//! Promoted-route artifact + the conformance-gated promotion policy.
//!
//! The miner writes [`PromotedRoutes`] to `.agent-admissibility/powl/repair-routes.json`;
//! the registry loads it and decides precedence with [`is_promotable`] — the one
//! place the van der Aalst rule lives: a mined route only beats the seed when the
//! log *proves* it (enough support AND measured lifecycle conformance).

use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use super::model::{Provenance, RepairRoute};

/// Promotion thresholds (the explicit noise filter). A mined route supersedes a
/// seed only when both are met.
pub mod promotion {
    /// Minimum supporting episodes before a mined route may be preferred.
    pub const MIN_SUPPORT: u32 = 3;
    /// Minimum measured success_rate (lawful closure fraction) to prefer a mined route.
    pub const MIN_SUCCESS: f32 = 0.6;
}

/// On-disk promoted-route set, written by `ggen lsp mine`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PromotedRoutes {
    /// Schema version; the loader rejects an unknown major.
    pub version: u8,
    /// BLAKE3 of the OCEL log these routes were mined from (provenance binding).
    pub source_log_hash: String,
    /// The promoted routes (each carries `Provenance::Mined`).
    pub routes: Vec<RepairRoute>,
}

impl PromotedRoutes {
    /// Current artifact schema version.
    pub const VERSION: u8 = 1;
}

/// Canonical path of the promoted-route artifact under a project root.
#[must_use]
pub fn default_pack_routes_path(root: &Path) -> PathBuf {
    root.join(".agent-admissibility")
        .join("powl")
        .join("repair-routes.json")
}

/// The conformance-gated precedence decision. A mined route may supersede the
/// family's seed **iff** it has enough support AND its measured success_rate
/// (lawful-closure fraction) clears the threshold. Seeds (and unknown provenance)
/// are never "promotable over" anything — they are the safe default.
#[must_use]
pub fn is_promotable(provenance: &Provenance) -> bool {
    match provenance {
        Provenance::Mined {
            support,
            success_rate,
            ..
        } => *support >= promotion::MIN_SUPPORT && *success_rate >= promotion::MIN_SUCCESS,
        Provenance::Seeded => false,
    }
}

/// Read + version-check the promoted-route artifact. Returns `None` if absent,
/// unreadable, malformed, or an unknown major version (fail-closed → seeds hold).
#[must_use]
pub fn load_promoted(path: &Path) -> Option<PromotedRoutes> {
    let content = std::fs::read_to_string(path).ok()?;
    let parsed: PromotedRoutes = serde_json::from_str(&content).ok()?;
    (parsed.version == PromotedRoutes::VERSION).then_some(parsed)
}

/// Atomically write the artifact (tmp + rename) so readers never see a partial file.
///
/// # Errors
/// Returns an I/O error if the directory/file cannot be created or written.
pub fn write_promoted(path: &Path, routes: &PromotedRoutes) -> std::io::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let json = serde_json::to_string_pretty(routes)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
    let tmp = path.with_extension("json.tmp");
    std::fs::write(&tmp, json)?;
    std::fs::rename(&tmp, path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::route::model::{PartialOrder, RepairFamily, RepairRoute, RouteId};

    fn mined(support: u32, success: f32) -> Provenance {
        Provenance::Mined {
            confidence: 0.9,
            support,
            success_rate: success,
            first_seen: "2026-05-28T00:00:00+00:00".into(),
            last_seen: "2026-05-28T01:00:00+00:00".into(),
            source_report_hash: "abc".into(),
        }
    }

    #[test]
    fn promotable_only_above_both_thresholds() {
        assert!(
            is_promotable(&mined(5, 0.8)),
            "high support + success → promotable"
        );
        assert!(
            !is_promotable(&mined(1, 0.9)),
            "low support → not promotable"
        );
        assert!(
            !is_promotable(&mined(10, 0.2)),
            "low success → not promotable"
        );
        assert!(
            !is_promotable(&Provenance::Seeded),
            "seed is never 'promotable over'"
        );
    }

    #[test]
    fn artifact_roundtrips_and_version_checks() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = default_pack_routes_path(dir.path());
        let routes = PromotedRoutes {
            version: PromotedRoutes::VERSION,
            source_log_hash: "deadbeef".into(),
            routes: vec![RepairRoute {
                id: RouteId("mined.x".into()),
                family: RepairFamily::TemplateFailure,
                steps: PartialOrder {
                    nodes: vec![],
                    edges: vec![],
                },
                description: "mined".into(),
                provenance: mined(4, 0.7),
                priority: 0,
            }],
        };
        write_promoted(&path, &routes).expect("write");
        let loaded = load_promoted(&path).expect("load");
        assert_eq!(loaded.routes.len(), 1);
        assert_eq!(loaded.source_log_hash, "deadbeef");
    }

    #[test]
    fn unknown_version_is_rejected() {
        let dir = tempfile::TempDir::new().expect("tempdir");
        let path = default_pack_routes_path(dir.path());
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).expect("mkdir");
        }
        std::fs::write(&path, r#"{"version":99,"source_log_hash":"x","routes":[]}"#).expect("w");
        assert!(
            load_promoted(&path).is_none(),
            "unknown major → fail-closed"
        );
    }
}
