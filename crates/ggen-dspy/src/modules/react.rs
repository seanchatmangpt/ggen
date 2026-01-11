//! ReAct module - Reasoning + Acting pattern

use crate::{Module, ModuleOutput, Result};

/// ReAct (Reasoning + Acting) module
pub struct ReAct {
    name: String,
}

impl ReAct {
    /// Create a new ReAct module
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

#[async_trait::async_trait]
impl Module for ReAct {
    async fn forward(&self, _inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // TODO: Implement ReAct loop
        Ok(ModuleOutput::new())
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Alias for ReAct
pub type ReactAgent = ReAct;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_react_creation() {
        let react = ReAct::new("ReAct");
        assert_eq!(react.name(), "ReAct");
    }
}
