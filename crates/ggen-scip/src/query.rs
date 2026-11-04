//! Query layer: load `.ggen/scip.json` once (memory-mapped), build
//! symbol -> occurrence maps, answer `def` / `refs` with zero re-parse.
//!
//! NOTE: this file was stubbed by the R80-04 lane as a compile-freeze SLA
//! unblock (their stub message on disk confirms it); the owning lane
//! (r80-03) supersedes the stub with the real implementation below.

use crate::{Index, Occurrence, ROLE_DEFINITION};
use std::collections::HashMap;
use std::path::Path;

/// A loaded index handle. Parse-once: the JSON is memory-mapped,
/// deserialized once, and indexed into hash maps for O(1) symbol lookup.
pub struct Query {
    index: Index,
    /// wire symbol -> definition occurrence (first definition wins).
    defs: HashMap<String, Occurrence>,
    /// wire symbol -> reference occurrences.
    refs: HashMap<String, Vec<Occurrence>>,
}

impl Query {
    /// Load and index the JSON at `path`. The file is memory-mapped for the
    /// deserialization pass; nothing is re-parsed afterward.
    pub fn open(path: &Path) -> std::io::Result<Self> {
        let file = std::fs::File::open(path)?;
        let mmap = unsafe { memmap2::Mmap::map(&file)? };
        let text = std::str::from_utf8(&mmap)
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
        let index: Index = serde_json::from_str(text)?;
        Ok(Self::from_index(index))
    }

    /// Open `<root>/.ggen/scip.json`.
    pub fn open_root(root: &Path) -> std::io::Result<Self> {
        Self::open(&root.join(crate::emit::OUTPUT_REL))
    }

    /// Build a handle from an already-parsed index.
    pub fn from_index(index: Index) -> Self {
        let mut defs = HashMap::new();
        let mut refs: HashMap<String, Vec<Occurrence>> = HashMap::new();
        for doc in &index.documents {
            for occ in &doc.occurrences {
                if occ.roles & ROLE_DEFINITION != 0 {
                    defs.entry(occ.symbol.clone()).or_insert_with(|| occ.clone());
                } else {
                    refs.entry(occ.symbol.clone()).or_default().push(occ.clone());
                }
            }
        }
        Self { index, defs, refs }
    }

    /// Definition occurrence for `wire`, if indexed.
    pub fn def(&self, wire: &str) -> Option<&Occurrence> {
        self.defs.get(wire)
    }

    /// All reference occurrences for `wire`.
    pub fn refs(&self, wire: &str) -> &[Occurrence] {
        self.refs.get(wire).map(Vec::as_slice).unwrap_or(&[])
    }

    /// Underlying index.
    pub fn index(&self) -> &Index {
        &self.index
    }

    /// Number of distinct defined symbols.
    pub fn symbol_count(&self) -> usize {
        self.defs.len()
    }
}
