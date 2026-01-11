//! Chain of Thought module - adds reasoning step before final answer

use crate::{Module, ModuleOutput, Result};

/// Chain of Thought module
pub struct ChainOfThought {
    name: String,
}

impl ChainOfThought {
    /// Create a new Chain of Thought module
    pub fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

#[async_trait::async_trait]
impl Module for ChainOfThought {
    async fn forward(&self, _inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // TODO: Implement CoT reasoning
        Ok(ModuleOutput::new())
    }

    fn name(&self) -> &str {
        &self.name
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cot_creation() {
        let cot = ChainOfThought::new("CoT");
        assert_eq!(cot.name(), "CoT");
    }
}
