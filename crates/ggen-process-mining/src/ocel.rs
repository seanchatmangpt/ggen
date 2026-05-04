//! OCEL (Object-Centric Event Log) format support.
//!
//! This module provides parsing functionality for the OCEL 2.0 format,
//! utilizing pictl-types for high-performance representation.

pub use pictl_types::ocel::{OCELEvent as OcelEvent, OCELObject as OcelObject, OCEL as OcelLog};

/// OCEL event log parser stub for pictl integration.
pub struct OcelParser;

impl OcelParser {
    /// Create a new parser
    pub fn new() -> Self {
        Self
    }
}

impl Default for OcelParser {
    fn default() -> Self {
        Self::new()
    }
}
