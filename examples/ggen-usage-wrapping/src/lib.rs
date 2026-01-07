use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeGenerator {
    pub name: String,
    pub version: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WrapperConfig {
    pub enabled: bool,
    pub generators: Vec<CodeGenerator>,
}

impl Default for WrapperConfig {
    fn default() -> Self {
        Self::new()
    }
}

impl WrapperConfig {
    pub fn new() -> Self {
        Self {
            enabled: true,
            generators: Vec::new(),
        }
    }

    pub fn add_generator(&mut self, generator: CodeGenerator) {
        self.generators.push(generator);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_creation() {
        let config = WrapperConfig::new();
        assert!(config.enabled);
        assert_eq!(config.generators.len(), 0);
    }

    #[test]
    fn test_add_generator() {
        let mut config = WrapperConfig::new();
        config.add_generator(CodeGenerator {
            name: "rest-api".to_string(),
            version: "1.0.0".to_string(),
        });
        assert_eq!(config.generators.len(), 1);
    }
}
