use crate::error::{Result, TransportError};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use url::Url;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Origin {
    pub scheme: String,
    pub host: String,
    pub port: Option<u16>,
}

impl Origin {
    pub fn from_url(url: &str) -> Result<Self> {
        let parsed = Url::parse(url).map_err(|e| {
            TransportError::OriginValidationFailed(format!("Invalid URL: {}", e))
        })?;

        Ok(Self {
            scheme: parsed.scheme().to_string(),
            host: parsed
                .host_str()
                .ok_or_else(|| {
                    TransportError::OriginValidationFailed("Missing host".to_string())
                })?
                .to_string(),
            port: parsed.port(),
        })
    }

    pub fn to_string(&self) -> String {
        match self.port {
            Some(port) => format!("{}://{}:{}", self.scheme, self.host, port),
            None => format!("{}://{}", self.scheme, self.host),
        }
    }

    pub fn matches(&self, other: &Origin) -> bool {
        self.scheme == other.scheme
            && self.host == other.host
            && self.port == other.port
    }
}

#[derive(Debug, Clone)]
pub struct OriginValidator {
    allowed_origins: HashSet<String>,
    allow_all: bool,
}

impl OriginValidator {
    pub fn new(allowed_origins: Vec<String>) -> Self {
        Self {
            allowed_origins: allowed_origins.into_iter().collect(),
            allow_all: false,
        }
    }

    pub fn allow_all() -> Self {
        Self {
            allowed_origins: HashSet::new(),
            allow_all: true,
        }
    }

    pub fn validate(&self, origin: &Origin) -> Result<()> {
        if self.allow_all {
            return Ok(());
        }

        let origin_str = origin.to_string();
        if self.allowed_origins.contains(&origin_str) {
            Ok(())
        } else {
            Err(TransportError::OriginValidationFailed(format!(
                "Origin not allowed: {}",
                origin_str
            )))
        }
    }

    pub fn add_origin(&mut self, origin: String) {
        self.allowed_origins.insert(origin);
    }

    pub fn remove_origin(&mut self, origin: &str) {
        self.allowed_origins.remove(origin);
    }

    pub fn is_allowed(&self, origin: &Origin) -> bool {
        self.allow_all || self.allowed_origins.contains(&origin.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_origin_from_url() {
        let origin = Origin::from_url("https://example.com:8080").unwrap();
        assert_eq!(origin.scheme, "https");
        assert_eq!(origin.host, "example.com");
        assert_eq!(origin.port, Some(8080));
    }

    #[test]
    fn test_origin_matches() {
        let origin1 = Origin::from_url("https://example.com:8080").unwrap();
        let origin2 = Origin::from_url("https://example.com:8080").unwrap();
        let origin3 = Origin::from_url("https://example.com:9090").unwrap();

        assert!(origin1.matches(&origin2));
        assert!(!origin1.matches(&origin3));
    }

    #[test]
    fn test_validator_allow_all() {
        let validator = OriginValidator::allow_all();
        let origin = Origin::from_url("https://example.com").unwrap();
        assert!(validator.validate(&origin).is_ok());
    }

    #[test]
    fn test_validator_allowed_origins() {
        let validator = OriginValidator::new(vec!["https://example.com".to_string()]);
        let origin1 = Origin::from_url("https://example.com").unwrap();
        let origin2 = Origin::from_url("https://blocked.com").unwrap();

        assert!(validator.validate(&origin1).is_ok());
        assert!(validator.validate(&origin2).is_err());
    }
}
