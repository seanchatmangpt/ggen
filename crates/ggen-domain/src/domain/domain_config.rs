//! Domain configuration module

// Placeholder until Tera templates are properly generated
pub struct DomainConfig {
    pub name: String,
}

impl Default for DomainConfig {
    fn default() -> Self {
        Self {
            name: "development".to_string(),
        }
    }
}

impl DomainConfig {
    pub fn validate(&self) -> Result<(), String> {
        if self.name.is_empty() {
            return Err("Name cannot be empty".to_string());
        }
        Ok(())
    }
}
