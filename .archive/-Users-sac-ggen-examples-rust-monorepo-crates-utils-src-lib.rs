//! Utility functions for the example project

use anyhow::Result;

/// Calculate hash of input (simple example)
pub fn simple_hash(input: &str) -> u64 {
    input.bytes().map(|b| b as u64).sum()
}

/// Validate input string
pub fn validate_input(input: &str) -> Result<()> {
    if input.is_empty() {
        anyhow::bail!("Input cannot be empty");
    }
    if input.len() > 1000 {
        anyhow::bail!("Input too long (max 1000 chars)");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_hash() {
        assert_eq!(simple_hash("hello"), 532);
    }

    #[test]
    fn test_validate_input_empty() {
        assert!(validate_input("").is_err());
    }

    #[test]
    fn test_validate_input_too_long() {
        let long_str = "a".repeat(1001);
        assert!(validate_input(&long_str).is_err());
    }

    #[test]
    fn test_validate_input_valid() {
        assert!(validate_input("hello").is_ok());
    }
}
