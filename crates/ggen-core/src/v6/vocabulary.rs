//! Vocabulary: Governance framework for RDF namespaces
//!
//! Implements the vocabulary governance rule: "Existing ontology first".
//! Custom terms are disallowed unless a proof-of-insufficiency exists.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};

/// An allowed vocabulary namespace
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AllowedVocabulary {
    /// The IRI namespace (e.g., "http://www.w3.org/2000/01/rdf-schema#")
    pub namespace: String,

    /// Standard prefix (e.g., "rdfs")
    pub prefix: String,

    /// Whether using this vocabulary requires a proof receipt
    pub requires_proof: bool,

    /// Human-readable description
    pub description: Option<String>,
}

impl AllowedVocabulary {
    /// Create a new allowed vocabulary
    pub fn new(namespace: impl Into<String>, prefix: impl Into<String>) -> Self {
        Self {
            namespace: namespace.into(),
            prefix: prefix.into(),
            requires_proof: false,
            description: None,
        }
    }

    /// Set whether proof is required
    pub fn with_requires_proof(mut self, requires: bool) -> Self {
        self.requires_proof = requires;
        self
    }

    /// Set description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }
}

/// A forbidden vocabulary namespace
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForbiddenVocabulary {
    /// The IRI namespace
    pub namespace: String,

    /// Reason for forbidding
    pub reason: String,
}

impl ForbiddenVocabulary {
    /// Create a new forbidden vocabulary
    pub fn new(namespace: impl Into<String>, reason: impl Into<String>) -> Self {
        Self {
            namespace: namespace.into(),
            reason: reason.into(),
        }
    }
}

/// Vocabulary violation detected during validation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VocabularyViolation {
    /// The unknown namespace
    pub namespace: String,

    /// Whether it's explicitly forbidden
    pub is_forbidden: bool,

    /// Human-readable message
    pub message: String,
}

/// Registry of allowed and forbidden vocabularies
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VocabularyRegistry {
    /// Allowed vocabularies keyed by namespace
    pub allowed: BTreeMap<String, AllowedVocabulary>,

    /// Forbidden vocabularies keyed by namespace
    pub forbidden: BTreeMap<String, ForbiddenVocabulary>,

    /// Proof receipts for custom vocabularies
    pub proof_receipts: BTreeMap<String, String>,
}

impl VocabularyRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a registry with standard W3C vocabularies
    pub fn with_standard_vocabularies() -> Self {
        let mut registry = Self::new();

        // RDF core vocabularies
        registry.add_allowed(
            AllowedVocabulary::new("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf")
                .with_description("RDF syntax vocabulary"),
        );

        registry.add_allowed(
            AllowedVocabulary::new("http://www.w3.org/2000/01/rdf-schema#", "rdfs")
                .with_description("RDF Schema vocabulary"),
        );

        registry.add_allowed(
            AllowedVocabulary::new("http://www.w3.org/2002/07/owl#", "owl")
                .with_description("Web Ontology Language"),
        );

        registry.add_allowed(
            AllowedVocabulary::new("http://www.w3.org/2001/XMLSchema#", "xsd")
                .with_description("XML Schema datatypes"),
        );

        registry.add_allowed(
            AllowedVocabulary::new("http://www.w3.org/ns/shacl#", "shacl")
                .with_description("Shapes Constraint Language"),
        );

        // Common external vocabularies
        registry.add_allowed(
            AllowedVocabulary::new("https://schema.org/", "schema")
                .with_description("Schema.org vocabulary"),
        );

        registry.add_allowed(
            AllowedVocabulary::new("http://purl.org/dc/terms/", "dcterms")
                .with_description("Dublin Core terms"),
        );

        registry.add_allowed(
            AllowedVocabulary::new("http://xmlns.com/foaf/0.1/", "foaf")
                .with_description("Friend of a Friend vocabulary"),
        );

        // ggen vocabularies
        registry.add_allowed(
            AllowedVocabulary::new("http://ggen.dev/v6#", "gv6")
                .with_description("ggen v6 ontology"),
        );

        registry.add_allowed(
            AllowedVocabulary::new("http://ggen.ai/ontology/meta#", "meta")
                .with_description("ggen meta-ontology"),
        );

        registry
    }

    /// Add an allowed vocabulary
    pub fn add_allowed(&mut self, vocab: AllowedVocabulary) {
        self.allowed.insert(vocab.namespace.clone(), vocab);
    }

    /// Add a forbidden vocabulary
    pub fn add_forbidden(&mut self, vocab: ForbiddenVocabulary) {
        self.forbidden.insert(vocab.namespace.clone(), vocab);
    }

    /// Register a proof receipt for a custom vocabulary
    pub fn add_proof_receipt(
        &mut self, namespace: impl Into<String>, receipt_id: impl Into<String>,
    ) {
        self.proof_receipts
            .insert(namespace.into(), receipt_id.into());
    }

    /// Check if a namespace is allowed
    pub fn is_allowed(&self, namespace: &str) -> bool {
        // Check if it matches any allowed vocabulary (by prefix)
        for allowed_ns in self.allowed.keys() {
            if namespace.starts_with(allowed_ns) {
                return true;
            }
        }

        // Check if it has a proof receipt
        for proof_ns in self.proof_receipts.keys() {
            if namespace.starts_with(proof_ns) {
                return true;
            }
        }

        false
    }

    /// Check if a namespace is explicitly forbidden
    pub fn is_forbidden(&self, namespace: &str) -> bool {
        for forbidden_ns in self.forbidden.keys() {
            if namespace.starts_with(forbidden_ns) {
                return true;
            }
        }
        false
    }

    /// Validate a set of namespaces used in an ontology
    ///
    /// # Arguments
    /// * `namespaces` - Set of namespace IRIs used
    ///
    /// # Returns
    /// * `Ok(())` - All namespaces are allowed
    /// * `Err(Error)` - Contains list of violations
    pub fn validate_namespaces(&self, namespaces: &BTreeSet<String>) -> Result<()> {
        let mut violations = Vec::new();

        for namespace in namespaces {
            if self.is_forbidden(namespace) {
                if let Some(forbidden) = self.forbidden.get(namespace) {
                    violations.push(VocabularyViolation {
                        namespace: namespace.clone(),
                        is_forbidden: true,
                        message: format!(
                            "Forbidden namespace: {} - {}",
                            namespace, forbidden.reason
                        ),
                    });
                }
            } else if !self.is_allowed(namespace) {
                violations.push(VocabularyViolation {
                    namespace: namespace.clone(),
                    is_forbidden: false,
                    message: format!(
                        "Unknown namespace '{}'. Add to allowed list or provide proof receipt.",
                        namespace
                    ),
                });
            }
        }

        if violations.is_empty() {
            Ok(())
        } else {
            let messages: Vec<_> = violations.iter().map(|v| v.message.clone()).collect();
            Err(Error::new(&format!(
                "Vocabulary violations:\n{}",
                messages.join("\n")
            )))
        }
    }

    /// Extract namespaces from RDF content
    ///
    /// # Arguments
    /// * `content` - Turtle RDF content
    ///
    /// # Returns
    /// Set of namespace IRIs found
    pub fn extract_namespaces(content: &str) -> BTreeSet<String> {
        let mut namespaces = BTreeSet::new();

        // Extract from @prefix declarations
        for line in content.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("@prefix") {
                // Extract namespace from: @prefix foo: <http://example.org/> .
                if let Some(start) = trimmed.find('<') {
                    if let Some(end) = trimmed.find('>') {
                        let ns = &trimmed[start + 1..end];
                        namespaces.insert(ns.to_string());
                    }
                }
            }
        }

        // Extract from IRI references (simple heuristic)
        let iri_pattern = regex::Regex::new(r"<([^>]+)>").unwrap();
        for cap in iri_pattern.captures_iter(content) {
            if let Some(iri) = cap.get(1) {
                let iri_str = iri.as_str();
                // Extract namespace (up to last # or /)
                if let Some(idx) = iri_str.rfind('#').or_else(|| iri_str.rfind('/')) {
                    let ns = &iri_str[..=idx];
                    namespaces.insert(ns.to_string());
                }
            }
        }

        namespaces
    }

    /// Get prefix for a namespace
    pub fn get_prefix(&self, namespace: &str) -> Option<&str> {
        for (ns, vocab) in &self.allowed {
            if namespace.starts_with(ns) {
                return Some(&vocab.prefix);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_standard_vocabularies() {
        let registry = VocabularyRegistry::with_standard_vocabularies();

        assert!(registry.is_allowed("http://www.w3.org/2000/01/rdf-schema#Class"));
        assert!(registry.is_allowed("http://www.w3.org/2002/07/owl#Thing"));
        assert!(registry.is_allowed("https://schema.org/Person"));
        assert!(registry.is_allowed("http://ggen.dev/v6#Pass"));
    }

    #[test]
    fn test_registry_unknown_namespace() {
        let registry = VocabularyRegistry::with_standard_vocabularies();

        assert!(!registry.is_allowed("http://unknown.example.org/Foo"));
    }

    #[test]
    fn test_registry_forbidden_namespace() {
        let mut registry = VocabularyRegistry::with_standard_vocabularies();
        registry.add_forbidden(ForbiddenVocabulary::new(
            "http://evil.example.org/",
            "Known malicious vocabulary",
        ));

        assert!(registry.is_forbidden("http://evil.example.org/Malware"));
    }

    #[test]
    fn test_registry_proof_receipt() {
        let mut registry = VocabularyRegistry::with_standard_vocabularies();

        // Initially not allowed
        assert!(!registry.is_allowed("http://custom.example.org/MyClass"));

        // Add proof receipt
        registry.add_proof_receipt("http://custom.example.org/", "receipt-12345");

        // Now allowed
        assert!(registry.is_allowed("http://custom.example.org/MyClass"));
    }

    #[test]
    fn test_extract_namespaces() {
        let content = r#"
            @prefix ex: <http://example.org/> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

            ex:alice rdf:type ex:Person .
        "#;

        let namespaces = VocabularyRegistry::extract_namespaces(content);

        assert!(namespaces.contains("http://example.org/"));
        assert!(namespaces.contains("http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    }

    #[test]
    fn test_validate_namespaces() {
        let registry = VocabularyRegistry::with_standard_vocabularies();

        let mut valid_ns = BTreeSet::new();
        valid_ns.insert("http://www.w3.org/2000/01/rdf-schema#".to_string());
        valid_ns.insert("https://schema.org/".to_string());

        assert!(registry.validate_namespaces(&valid_ns).is_ok());

        let mut invalid_ns = BTreeSet::new();
        invalid_ns.insert("http://unknown.example.org/".to_string());

        assert!(registry.validate_namespaces(&invalid_ns).is_err());
    }
}
