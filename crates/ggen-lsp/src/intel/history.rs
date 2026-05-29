//! Promotion history ledger — `.ggen/ocel/promotion-history.jsonl`.
//!
//! Append-only record of every route's promotion status per mine cycle. Without
//! it the system cannot distinguish *route got better* / *route was replaced* /
//! *route disappeared* / *route failed later*. Backs `promotion_survival_rate`,
//! `promotion_churn`, and `seed_displacement_rate`.

use std::fs::{create_dir_all, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

/// Lifecycle status of a promoted route across mining cycles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RouteStatus {
    /// Promotable this cycle (support + success above threshold).
    Active,
    /// Was active, no longer promotable this cycle.
    Demoted,
    /// Replaced by a different route for the family.
    Superseded,
    /// Held back pending more evidence (sub-threshold but present).
    Quarantined,
}

/// One route's promotion record for one mine cycle.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RoutePromotionRecord {
    pub route_id: String,
    pub route_source: String, // "seed" | "mined" | "manual"
    pub diagnostic_code: String,
    pub family: String,
    pub support: u32,
    pub success_rate: f32,
    pub source_log_hash: String,
    pub routes_hash: String,
    pub promotion_receipt: Option<String>,
    /// RFC-3339 cycle timestamp.
    pub promoted_at: String,
    pub supersedes: Option<String>,
    pub status: RouteStatus,
}

/// Append-only promotion-history ledger.
#[derive(Debug, Clone)]
pub struct PromotionHistory {
    path: PathBuf,
}

/// Canonical path of the ledger under a project root.
#[must_use]
pub fn default_history_path(root: &Path) -> PathBuf {
    root.join(".ggen")
        .join("ocel")
        .join("promotion-history.jsonl")
}

impl PromotionHistory {
    #[must_use]
    pub fn at_root(root: &Path) -> Self {
        Self {
            path: default_history_path(root),
        }
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Append one cycle's records (NDJSON, one record per line).
    ///
    /// # Errors
    /// Returns an I/O error if the directory/file cannot be created or written.
    pub fn append(&self, records: &[RoutePromotionRecord]) -> io::Result<()> {
        if records.is_empty() {
            return Ok(());
        }
        if let Some(parent) = self.path.parent() {
            create_dir_all(parent)?;
        }
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)?;
        let mut buf = String::new();
        for r in records {
            match serde_json::to_string(r) {
                Ok(line) => {
                    buf.push_str(&line);
                    buf.push('\n');
                }
                Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidData, e)),
            }
        }
        file.write_all(buf.as_bytes())?;
        file.flush()
    }

    /// Read all records (skips corrupt lines).
    #[must_use]
    pub fn read(&self) -> Vec<RoutePromotionRecord> {
        let content = std::fs::read_to_string(&self.path).unwrap_or_default();
        content
            .lines()
            .filter(|l| !l.trim().is_empty())
            .filter_map(|l| serde_json::from_str::<RoutePromotionRecord>(l).ok())
            .collect()
    }

    /// The most recent record per route_id (latest cycle state).
    #[must_use]
    pub fn latest_by_route(&self) -> std::collections::BTreeMap<String, RoutePromotionRecord> {
        let mut map = std::collections::BTreeMap::new();
        for r in self.read() {
            map.insert(r.route_id.clone(), r); // later lines overwrite → latest wins
        }
        map
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn rec(id: &str, status: RouteStatus) -> RoutePromotionRecord {
        RoutePromotionRecord {
            route_id: id.into(),
            route_source: "mined".into(),
            diagnostic_code: "E0024".into(),
            family: "TemplateFailure".into(),
            support: 5,
            success_rate: 1.0,
            source_log_hash: "src".into(),
            routes_hash: "rt".into(),
            promotion_receipt: Some("rcpt".into()),
            promoted_at: "2026-05-28T00:00:00+00:00".into(),
            supersedes: None,
            status,
        }
    }

    #[test]
    fn append_then_latest_reflects_most_recent_status() {
        let dir = TempDir::new().expect("tempdir");
        let h = PromotionHistory::at_root(dir.path());
        h.append(&[rec("mined.template-failure", RouteStatus::Active)])
            .expect("cycle1");
        h.append(&[rec("mined.template-failure", RouteStatus::Demoted)])
            .expect("cycle2");
        assert_eq!(h.read().len(), 2, "both cycles recorded (append-only)");
        let latest = h.latest_by_route();
        assert_eq!(
            latest.get("mined.template-failure").map(|r| r.status),
            Some(RouteStatus::Demoted),
            "latest cycle status wins"
        );
    }
}
