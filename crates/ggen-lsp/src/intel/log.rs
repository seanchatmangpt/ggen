//! Append-only NDJSON event store at `.ggen/ocel/agent-edit-events.ocel.jsonl`.
//!
//! One serialized `OcelEvent` per line. Append-only + per-line flush is the only
//! crash-safe shape (rewriting a whole `OcelLog` each event would lose data and
//! be O(n)). The reader folds lines back into an `OcelLog`, reconstructing the
//! object table from each event's refs and skipping any truncated trailing line.

use std::collections::BTreeMap;
use std::fs::{create_dir_all, OpenOptions};
use std::io::{self, Write};
use std::path::{Path, PathBuf};

use ggen_graph::ocel::{OcelEvent, OcelLog, OcelObject};

/// Append-only OCEL event log.
#[derive(Debug, Clone)]
pub struct IntelLog {
    path: PathBuf,
}

/// Canonical path of the event log under a project root.
#[must_use]
pub fn default_path(root: &Path) -> PathBuf {
    root.join(".ggen")
        .join("ocel")
        .join("agent-edit-events.ocel.jsonl")
}

impl IntelLog {
    /// Open (logically) the log at `path`.
    #[must_use]
    pub fn new(path: PathBuf) -> Self {
        Self { path }
    }

    /// Open the canonical log under a project `root`.
    #[must_use]
    pub fn at_root(root: &Path) -> Self {
        Self::new(default_path(root))
    }

    /// The backing file path.
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Append events as NDJSON (one JSON object per line). Best-effort but
    /// crash-safe: each line is written+flushed; partial lines are skipped on read.
    ///
    /// # Errors
    /// Returns an I/O error if the directory or file cannot be created/written.
    pub fn append(&self, events: &[OcelEvent]) -> io::Result<()> {
        if events.is_empty() {
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
        for ev in events {
            match serde_json::to_string(ev) {
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

    /// Read the log and fold it into an `OcelLog`, reconstructing the object
    /// table from event refs. Unparseable (e.g. truncated) lines are skipped.
    #[must_use]
    pub fn read(&self) -> OcelLog {
        let content = std::fs::read_to_string(&self.path).unwrap_or_default();
        let mut events = Vec::new();
        let mut objects: BTreeMap<String, OcelObject> = BTreeMap::new();
        for line in content.lines() {
            if line.trim().is_empty() {
                continue;
            }
            let Ok(ev) = serde_json::from_str::<OcelEvent>(line) else {
                continue; // skip truncated/corrupt trailing line
            };
            for r in &ev.objects {
                objects.entry(r.id.clone()).or_insert_with(|| OcelObject {
                    id: r.id.clone(),
                    r#type: r.r#type.clone(),
                    attributes: std::collections::HashMap::new(),
                });
            }
            events.push(ev);
        }
        OcelLog {
            objects: objects.into_values().collect(),
            events,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::intel::events::diagnostic_raised;
    use tempfile::TempDir;

    #[test]
    fn append_then_read_roundtrips() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        log.append(&[
            diagnostic_raised("a.ttl", "E0010", "error", "0:0-0:1", "r", 1),
            diagnostic_raised("a.ttl", "E0011", "warning", "0:0-0:1", "r", 2),
        ])
        .expect("append");

        let folded = log.read();
        assert_eq!(folded.events.len(), 2);
        // Objects reconstructed: file a.ttl + two diagnostic codes.
        assert!(folded.objects.iter().any(|o| o.id == "a.ttl"));
        assert!(folded.objects.iter().any(|o| o.id == "E0010"));
        assert!(folded.objects.iter().any(|o| o.id == "E0011"));
    }

    #[test]
    fn append_is_additive_across_calls() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        log.append(&[diagnostic_raised(
            "a.ttl", "E0010", "error", "0:0-0:1", "r", 1,
        )])
        .expect("append1");
        log.append(&[diagnostic_raised(
            "b.ttl", "E0024", "error", "0:0-0:1", "r", 2,
        )])
        .expect("append2");
        assert_eq!(log.read().events.len(), 2);
    }

    #[test]
    fn truncated_trailing_line_is_skipped() {
        let dir = TempDir::new().expect("tempdir");
        let log = IntelLog::at_root(dir.path());
        log.append(&[diagnostic_raised(
            "a.ttl", "E0010", "error", "0:0-0:1", "r", 1,
        )])
        .expect("append");
        // Simulate a crash mid-write by appending a partial line.
        let mut f = OpenOptions::new()
            .append(true)
            .open(log.path())
            .expect("open");
        f.write_all(b"{\"id\":\"trunc").expect("partial write");
        assert_eq!(log.read().events.len(), 1, "partial line skipped");
    }
}
