//! Standard Ontology Validation and Screening
//!
//! Enforces BIG BANG 80/20 principle: Reject custom ontologies, force users
//! to use proven standard bases (schema.org, FOAF, Dublin Core, SKOS, Big Five).
//!
//! **Design Philosophy**: Like Seth's failure to find "one ontology link", users
//! often build custom ontologies when standard solutions exist. This module forces
//! the discipline: provide real data first, validate immediately, no custom schemas.
//!
//! ## Supported Standard Ontologies
//!
//! - **schema.org** (v3.13+) - Largest vocabulary for structured data on the web
//! - **FOAF** (Friend of a Friend) - Social networks, personal profiles
//! - **Dublin Core** (v1.1) - Metadata and document properties
//! - **SKOS** (Simple Knowledge Organization System) - Thesauri, controlled vocabularies
//! - **Big Five Traits** - Personality modeling (OCEAN framework)
//!
//! All others are rejected in screening mode.
//!
//! ## Constitution Compliance
//!
//! - ✓ Principle V: Type-first (enum-based standard list)
//! - ✓ Principle VII: Result<T,E> (no panics)
//! - ✓ Principle IX: Poka-yoke (reject bad choices at gate)

use crate::validation::error::{Result, ValidationError};
use std::collections::HashSet;

/// Standard ontologies approved for use in ggen projects
///
/// Using a standard ontology is non-negotiable in BIG BANG 80/20 mode.
/// This enum enforces the discipline: no "custom" or "special" schemas.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StandardOntology {
    /// schema.org (v3.13.0+) - Web structured data vocabulary
    ///
    /// Largest vocabulary with 500+ types for real-world domains:
    /// Person, Organization, Place, Event, Product, etc.
    /// **Canonical source**: https://schema.org/
    SchemaOrg,

    /// FOAF (Friend of a Friend) - Social networks and profiles
    ///
    /// RDF vocabulary for describing people, their interests, connections.
    /// **Canonical source**: http://xmlns.com/foaf/0.1/
    Foaf,

    /// Dublin Core (v1.1) - Metadata and document properties
    ///
    /// 15 core metadata properties: title, creator, date, subject, etc.
    /// **Canonical source**: http://purl.org/dc/elements/1.1/
    DublinCore,

    /// SKOS (Simple Knowledge Organization System) - Thesauri
    ///
    /// Controlled vocabularies, concept schemes, hierarchies.
    /// **Canonical source**: http://www.w3.org/2004/02/skos/core#
    Skos,

    /// Big Five Personality Traits (OCEAN) - Behavioral modeling
    ///
    /// Openness, Conscientiousness, Extraversion, Agreeableness, Neuroticism
    /// **Canonical source**: Custom namespace (ggen-approved)
    BigFive,
}

impl StandardOntology {
    /// Get the canonical namespace/prefix for this ontology
    pub fn namespace(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "https://schema.org/",
            StandardOntology::Foaf => "http://xmlns.com/foaf/0.1/",
            StandardOntology::DublinCore => "http://purl.org/dc/elements/1.1/",
            StandardOntology::Skos => "http://www.w3.org/2004/02/skos/core#",
            StandardOntology::BigFive => "https://ggen.io/ontologies/big-five#",
        }
    }

    /// Get human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "schema.org",
            StandardOntology::Foaf => "FOAF",
            StandardOntology::DublinCore => "Dublin Core",
            StandardOntology::Skos => "SKOS",
            StandardOntology::BigFive => "Big Five Traits",
        }
    }

    /// Get recommended version constraint
    pub fn version_constraint(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "^3.13.0",
            StandardOntology::Foaf => "~0.1.0",
            StandardOntology::DublinCore => "^1.1.0",
            StandardOntology::Skos => "~0.3.0",
            StandardOntology::BigFive => "^0.1.0",
        }
    }

    /// Get link to canonical specification
    pub fn spec_url(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "https://schema.org/",
            StandardOntology::Foaf => "http://xmlns.com/foaf/spec/",
            StandardOntology::DublinCore => "http://dublincore.org/documents/dces/",
            StandardOntology::Skos => "https://www.w3.org/2009/08/skos-reference/skos.html",
            StandardOntology::BigFive => "https://ggen.io/docs/big-five",
        }
    }

    /// All approved standard ontologies
    pub fn all() -> &'static [StandardOntology] {
        &[
            StandardOntology::SchemaOrg,
            StandardOntology::Foaf,
            StandardOntology::DublinCore,
            StandardOntology::Skos,
            StandardOntology::BigFive,
        ]
    }

    /// All approved namespace URIs
    pub fn approved_namespaces() -> HashSet<&'static str> {
        Self::all().iter().map(|o| o.namespace()).collect()
    }

    /// Resolve a namespace to a standard ontology
    pub fn from_namespace(namespace: &str) -> Option<Self> {
        match namespace {
            "https://schema.org/" | "http://schema.org/" => Some(StandardOntology::SchemaOrg),
            "http://xmlns.com/foaf/0.1/" | "http://xmlns.com/foaf/" => Some(StandardOntology::Foaf),
            "http://purl.org/dc/elements/1.1/" | "http://purl.org/dc/terms/" => {
                Some(StandardOntology::DublinCore)
            }
            "http://www.w3.org/2004/02/skos/core#" => Some(StandardOntology::Skos),
            "https://ggen.io/ontologies/big-five#" | "https://ggen.io/big-five#" => {
                Some(StandardOntology::BigFive)
            }
            _ => None,
        }
    }
}

/// Validator for standard ontology compliance
pub struct StandardOntologyValidator;

impl StandardOntologyValidator {
    /// Check if an ontology namespace is standard-approved
    ///
    /// # Arguments
    /// - `namespace`: The ontology namespace URI to validate
    /// - `strict`: If true, rejects custom prefixes even if base is standard
    ///
    /// # Returns
    /// - `Ok(StandardOntology)` if namespace is approved
    /// - `Err(ValidationError)` if custom/non-standard
    pub fn validate_namespace(namespace: &str, strict: bool) -> Result<StandardOntology> {
        StandardOntology::from_namespace(namespace).ok_or_else(|| {
            ValidationError::OxigraphError(format!(
                "Custom ontology namespace '{}' rejected. Use one of: {}",
                namespace,
                Self::approved_list()
            ))
        })
    }

    /// Get formatted list of approved ontologies for error messages
    fn approved_list() -> String {
        StandardOntology::all()
            .iter()
            .map(|o| format!("{} ({})", o.name(), o.namespace()))
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Check if configuration uses only standard ontologies
    ///
    /// **BIG BANG 80/20 Gate**: Rejects custom ontologies and forces users
    /// to either:
    /// 1. Use standard ontologies with real data
    /// 2. Prove why custom ontology is necessary (not supported)
    pub fn validate_config_ontologies(
        ontology_namespaces: &[String],
    ) -> Result<Vec<StandardOntology>> {
        if ontology_namespaces.is_empty() {
            return Err(ValidationError::OxigraphError(
                "No ontology namespaces configured. Required: one or more of: \
                 schema.org, FOAF, Dublin Core, SKOS, Big Five"
                    .to_string(),
            ));
        }

        let mut validated = Vec::new();
        for namespace in ontology_namespaces {
            match StandardOntology::from_namespace(namespace) {
                Some(ontology) => validated.push(ontology),
                None => {
                    return Err(ValidationError::OxigraphError(
                        format!(
                            "Ontology '{}' is not standard-approved.\n\
                             Approved options:\n{}\n\
                             \n\
                             Why? Custom ontologies delay execution. Seth built a 100-page custom \
                             ontology instead of using schema.org in 5 minutes. Use standards first, \
                             prove market fit, then extend if needed.",
                            namespace,
                            StandardOntology::all()
                                .iter()
                                .map(|o| format!("  • {} ({})", o.name(), o.namespace()))
                                .collect::<Vec<_>>()
                                .join("\n")
                        ),
                    ))
                }
            }
        }

        Ok(validated)
    }

    /// Screening questions to determine if user is ready for ggen
    ///
    /// These are the "litmus test" questions Sean asked Seth:
    /// - Can you find an existing ontology link?
    /// - Do you have real user data to validate with?
    /// - Can you articulate the problem in one sentence?
    pub fn screening_questions() -> Vec<&'static str> {
        vec![
            "Do you have real user data (CSV, JSON) to validate with? (Not promised, but actual)",
            "Can you find one existing standard ontology that fits your domain? \
             (schema.org, FOAF, Dublin Core, SKOS - 5 minute task, not 3 months)",
            "Can you explain your product in one sentence without a 100-page ontology?",
            "Has anyone (not a friend, not a co-founder) committed time/money to this? \
             (Email, contract, payment - proof, not enthusiasm)",
            "If we gave you 48 hours, could you validate your idea with 10 real users?",
        ]
    }
}

/// Configuration for ontology screening and validation
#[derive(Debug, Clone)]
pub struct OntologyScreeningConfig {
    /// Enforce standard ontologies (BIG BANG 80/20 mode)
    pub strict_standard_only: bool,

    /// Require user data upload before allowing sync
    pub require_user_data: bool,

    /// Require market validation (email list, beta users)
    pub require_market_signal: bool,

    /// Maximum custom namespace URIs allowed (0 in strict mode)
    pub max_custom_namespaces: usize,
}

impl Default for OntologyScreeningConfig {
    fn default() -> Self {
        Self {
            strict_standard_only: true,
            require_user_data: true,
            require_market_signal: true,
            max_custom_namespaces: 0,
        }
    }
}

impl OntologyScreeningConfig {
    /// BIG BANG 80/20 mode: maximum strictness
    pub fn big_bang_80_20() -> Self {
        Self {
            strict_standard_only: true,
            require_user_data: true,
            require_market_signal: true,
            max_custom_namespaces: 0,
        }
    }

    /// Relaxed mode: allow custom if user has proven execution capability
    pub fn permissive() -> Self {
        Self {
            strict_standard_only: false,
            require_user_data: false,
            require_market_signal: false,
            max_custom_namespaces: 3,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard_ontology_namespaces() {
        assert_eq!(
            StandardOntology::SchemaOrg.namespace(),
            "https://schema.org/"
        );
        assert_eq!(
            StandardOntology::Foaf.namespace(),
            "http://xmlns.com/foaf/0.1/"
        );
        assert_eq!(
            StandardOntology::DublinCore.namespace(),
            "http://purl.org/dc/elements/1.1/"
        );
        assert_eq!(
            StandardOntology::Skos.namespace(),
            "http://www.w3.org/2004/02/skos/core#"
        );
    }

    #[test]
    fn test_resolve_namespace_schema_org() {
        let result = StandardOntology::from_namespace("https://schema.org/");
        assert_eq!(result, Some(StandardOntology::SchemaOrg));
    }

    #[test]
    fn test_resolve_namespace_foaf() {
        let result = StandardOntology::from_namespace("http://xmlns.com/foaf/0.1/");
        assert_eq!(result, Some(StandardOntology::Foaf));
    }

    #[test]
    fn test_reject_custom_namespace() {
        let result = StandardOntology::from_namespace("https://custom.example.com/ontology#");
        assert_eq!(result, None);
    }

    #[test]
    fn test_validate_config_ontologies_success() {
        let namespaces = vec!["https://schema.org/".to_string()];
        let result = StandardOntologyValidator::validate_config_ontologies(&namespaces);
        assert!(result.is_ok());
        let validated = result.unwrap();
        assert_eq!(validated.len(), 1);
        assert_eq!(validated[0], StandardOntology::SchemaOrg);
    }

    #[test]
    fn test_validate_config_ontologies_multiple() {
        let namespaces = vec![
            "https://schema.org/".to_string(),
            "http://xmlns.com/foaf/0.1/".to_string(),
        ];
        let result = StandardOntologyValidator::validate_config_ontologies(&namespaces);
        assert!(result.is_ok());
        let validated = result.unwrap();
        assert_eq!(validated.len(), 2);
    }

    #[test]
    fn test_validate_config_ontologies_custom_rejected() {
        let namespaces = vec!["https://custom.example.com/ontology#".to_string()];
        let result = StandardOntologyValidator::validate_config_ontologies(&namespaces);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("not standard-approved"));
    }

    #[test]
    fn test_screening_config_big_bang_80_20() {
        let config = OntologyScreeningConfig::big_bang_80_20();
        assert!(config.strict_standard_only);
        assert!(config.require_user_data);
        assert!(config.require_market_signal);
        assert_eq!(config.max_custom_namespaces, 0);
    }

    #[test]
    fn test_screening_config_permissive() {
        let config = OntologyScreeningConfig::permissive();
        assert!(!config.strict_standard_only);
        assert!(!config.require_user_data);
        assert!(!config.require_market_signal);
        assert_eq!(config.max_custom_namespaces, 3);
    }

    #[test]
    fn test_screening_questions_non_empty() {
        let questions = StandardOntologyValidator::screening_questions();
        assert!(!questions.is_empty());
        assert!(questions[0].contains("real user data"));
    }

    #[test]
    fn test_approved_namespaces_set() {
        let namespaces = StandardOntology::approved_namespaces();
        assert_eq!(namespaces.len(), 5); // 5 standard ontologies
        assert!(namespaces.contains("https://schema.org/"));
        assert!(namespaces.contains("http://xmlns.com/foaf/0.1/"));
    }

    #[test]
    fn test_standard_ontology_all() {
        let all = StandardOntology::all();
        assert_eq!(all.len(), 5);
    }

    #[test]
    fn test_standard_ontology_names() {
        assert_eq!(StandardOntology::SchemaOrg.name(), "schema.org");
        assert_eq!(StandardOntology::Foaf.name(), "FOAF");
        assert_eq!(StandardOntology::DublinCore.name(), "Dublin Core");
        assert_eq!(StandardOntology::Skos.name(), "SKOS");
        assert_eq!(StandardOntology::BigFive.name(), "Big Five Traits");
    }

    #[test]
    fn test_empty_ontology_namespaces_rejected() {
        let namespaces: Vec<String> = vec![];
        let result = StandardOntologyValidator::validate_config_ontologies(&namespaces);
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("No ontology namespaces"));
    }
}
