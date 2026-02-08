//! Domain validation module

use super::DomainConfig;

pub struct DomainValidator {
    pub config: DomainConfig,
}

impl DomainValidator {
    pub fn new(config: DomainConfig) -> Self {
        Self { config }
    }

    pub fn validate(&self) -> Result<(), String> {
        self.config.validate()
    }
}
