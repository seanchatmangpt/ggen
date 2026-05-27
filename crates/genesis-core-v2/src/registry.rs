//! Pattern registry implementation
//!
//! Provides pattern discovery and management

use crate::Pattern;
use dashmap::DashMap;
use std::sync::Arc;

/// Thread-safe pattern registry using DashMap
pub struct PatternRegistry {
    patterns: DashMap<u32, Arc<dyn Pattern>>,
}

impl PatternRegistry {
    pub fn new() -> Self {
        Self {
            patterns: DashMap::new(),
        }
    }

    pub fn register(&self, id: u32, pattern: Arc<dyn Pattern>) {
        self.patterns.insert(id, pattern);
    }

    pub fn get(&self, id: u32) -> Option<Arc<dyn Pattern>> {
        self.patterns.get(&id).map(|entry| Arc::clone(&entry))
    }

    pub fn exists(&self, id: u32) -> bool {
        self.patterns.contains_key(&id)
    }

    pub fn count(&self) -> usize {
        self.patterns.len()
    }
}

impl Default for PatternRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_basic() {
        let registry = PatternRegistry::new();
        assert_eq!(registry.count(), 0);
    }
}
