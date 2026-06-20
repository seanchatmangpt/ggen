use bp_core::{error::CoreError, Result};

/// A non-empty, trimmed name string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Name(String);

impl Name {
    pub fn new(raw: impl Into<String>) -> Result<Self> {
        let s = raw.into().trim().to_owned();
        if s.is_empty() {
            return Err(CoreError::validation("name must not be empty"));
        }
        if s.len() > 255 {
            return Err(CoreError::validation("name must be ≤ 255 characters"));
        }
        Ok(Self(s))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_new_empty_string_returns_err() {
        let result = Name::new("");
        assert!(result.is_err(), "Name::new(\"\") must return Err");
    }

    #[test]
    fn name_new_whitespace_only_returns_err() {
        let result = Name::new("   ");
        assert!(result.is_err(), "Name::new with only whitespace must return Err");
    }

    #[test]
    fn name_new_valid_string_returns_ok() {
        let result = Name::new("Alice");
        assert!(result.is_ok(), "Name::new(\"Alice\") must return Ok");
        assert_eq!(result.unwrap().as_str(), "Alice");
    }

    #[test]
    fn name_new_trims_surrounding_whitespace() {
        let name = Name::new("  Bob  ").unwrap();
        assert_eq!(name.as_str(), "Bob");
    }

    #[test]
    fn name_new_too_long_returns_err() {
        let long = "x".repeat(256);
        assert!(Name::new(long).is_err(), "Name exceeding 255 chars must return Err");
    }
}
