//! Reusable agent patterns and templates

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Agent pattern definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentPattern {
    /// Pattern name
    pub name: String,

    /// Pattern description
    pub description: String,

    /// Module chain definition
    pub modules: Vec<String>,

    /// Pattern metadata
    pub metadata: HashMap<String, String>,
}

impl AgentPattern {
    /// Create a new agent pattern
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            modules: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add a module to the pattern
    pub fn add_module(&mut self, module: impl Into<String>) {
        self.modules.push(module.into());
    }

    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
}

/// Builder for agent patterns
pub struct PatternBuilder {
    pattern: AgentPattern,
}

impl PatternBuilder {
    /// Create a new pattern builder
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            pattern: AgentPattern::new(name, description),
        }
    }

    /// Add a module
    pub fn with_module(mut self, module: impl Into<String>) -> Self {
        self.pattern.add_module(module);
        self
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.pattern.set_metadata(key, value);
        self
    }

    /// Build the pattern
    pub fn build(self) -> AgentPattern {
        self.pattern
    }
}

/// Library of predefined patterns
pub struct PatternLibrary {
    patterns: HashMap<String, AgentPattern>,
}

impl PatternLibrary {
    /// Create a new pattern library
    pub fn new() -> Self {
        Self {
            patterns: HashMap::new(),
        }
    }

    /// Register a pattern
    pub fn register(&mut self, pattern: AgentPattern) {
        self.patterns.insert(pattern.name.clone(), pattern);
    }

    /// Get a pattern by name
    pub fn get(&self, name: &str) -> Option<&AgentPattern> {
        self.patterns.get(name)
    }

    /// List all pattern names
    pub fn list(&self) -> Vec<&str> {
        self.patterns.keys().map(|s| s.as_str()).collect()
    }
}

impl Default for PatternLibrary {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_builder() {
        let pattern = PatternBuilder::new("test", "A test pattern")
            .with_module("Predictor")
            .with_module("ChainOfThought")
            .with_metadata("version", "1.0")
            .build();

        assert_eq!(pattern.name, "test");
        assert_eq!(pattern.modules.len(), 2);
        assert_eq!(pattern.metadata.get("version"), Some(&"1.0".to_string()));
    }

    #[test]
    fn test_pattern_library() {
        let mut library = PatternLibrary::new();
        let pattern = AgentPattern::new("test", "A test pattern");
        library.register(pattern);

        assert!(library.get("test").is_some());
        assert_eq!(library.list().len(), 1);
    }
}
