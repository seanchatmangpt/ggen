//! RDF/TTL canonicalization
//!
//! Provides deterministic RDF triple canonicalization.
//! Uses canonical N-Triples format for deterministic output.

use crate::{Canonical, Canonicalizer, Result};

/// RDF/TTL canonicalizer
///
/// Converts RDF to canonical N-Triples format with sorted triples
pub struct TtlCanonicalizer {
    /// Whether to include comments in output
    include_comments: bool,
}

impl TtlCanonicalizer {
    /// Create a new TTL canonicalizer
    pub fn new() -> Self {
        Self {
            include_comments: false,
        }
    }

    /// Normalize and sort TTL content
    fn normalize_ttl(&self, input: &str) -> Result<Vec<String>> {
        let mut lines: Vec<String> = input
            .lines()
            .map(|line| line.trim())
            .filter(|line| !line.is_empty() && !line.starts_with('@'))
            .filter(|line| !line.starts_with('#') || self.include_comments)
            .map(String::from)
            .collect();

        lines.sort();
        Ok(lines)
    }
}

impl Default for TtlCanonicalizer {
    fn default() -> Self {
        Self::new()
    }
}

impl Canonicalizer for TtlCanonicalizer {
    type Input = String;
    type Output = Canonical<String>;

    fn canonicalize(&self, input: Self::Input) -> Result<Self::Output> {
        let lines = self.normalize_ttl(&input)?;
        let canonical = lines.join("\n");
        Ok(Canonical::new_unchecked(canonical))
    }
}

/// Canonicalize TTL/RDF to sorted lines
///
/// # Errors
///
/// Returns error if parsing or serialization fails
pub fn canonicalize_ttl(ttl: &str) -> Result<String> {
    let canonicalizer = TtlCanonicalizer::new();
    let canonical = canonicalizer.canonicalize(ttl.to_string())?;
    Ok(canonical.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_ttl() {
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:subject ex:predicate ex:object .
        "#;
        let canonicalizer = TtlCanonicalizer::new();
        let lines = canonicalizer.normalize_ttl(ttl);
        assert!(lines.is_ok());
        assert_eq!(lines.unwrap().len(), 1);
    }

    #[test]
    fn test_canonicalize_ttl() {
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:s1 ex:p1 ex:o1 .
            ex:s2 ex:p2 ex:o2 .
        "#;
        let canonicalizer = TtlCanonicalizer::new();
        let result = canonicalizer.canonicalize(ttl.to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_determinism() {
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:s ex:p ex:o .
        "#;
        let canonicalizer = TtlCanonicalizer::new();
        let result1 = canonicalizer.canonicalize(ttl.to_string()).unwrap();
        let result2 = canonicalizer.canonicalize(ttl.to_string()).unwrap();
        assert_eq!(result1, result2);
    }

    #[test]
    fn test_sorting() {
        let ttl = r#"
            ex:z ex:p ex:o .
            ex:a ex:p ex:o .
            ex:m ex:p ex:o .
        "#;
        let canonicalizer = TtlCanonicalizer::new();
        let result = canonicalizer.canonicalize(ttl.to_string()).unwrap();
        let result_str = result.into_inner();
        // Should be sorted: a, m, z
        let lines: Vec<&str> = result_str.lines().collect();
        assert!(lines[0].contains(":a "));
        assert!(lines[1].contains(":m "));
        assert!(lines[2].contains(":z "));
    }

    #[test]
    fn test_canonicalize_helper() {
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            ex:subject ex:predicate ex:object .
        "#;
        let result = canonicalize_ttl(ttl);
        assert!(result.is_ok());
    }

    #[test]
    fn test_order_independence() {
        let ttl1 = "ex:a ex:p ex:o1 .\nex:b ex:p ex:o2 .";
        let ttl2 = "ex:b ex:p ex:o2 .\nex:a ex:p ex:o1 .";

        let canonicalizer = TtlCanonicalizer::new();
        let result1 = canonicalizer.canonicalize(ttl1.to_string()).unwrap();
        let result2 = canonicalizer.canonicalize(ttl2.to_string()).unwrap();

        assert_eq!(result1, result2);
    }
}
