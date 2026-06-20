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
