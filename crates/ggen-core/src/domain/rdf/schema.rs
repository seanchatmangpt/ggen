//! Ggen RDF schema and ontology definitions (v2 domain layer)
//!
//! This module provides programmatic access to the Ggen ontology.
//! Refactored from v1 to use v2 error handling patterns.

use crate::utils::error::Result;

/// Ggen ontology namespace
pub const GGEN_NAMESPACE: &str = "https://ggen.io/marketplace/";

/// RDF syntax namespace
pub const RDF_NAMESPACE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
/// RDFS schema namespace
pub const RDFS_NAMESPACE: &str = "http://www.w3.org/2000/01/rdf-schema#";
/// XSD datatypes namespace
pub const XSD_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema#";
/// OWL ontology namespace
pub const OWL_NAMESPACE: &str = "http://www.w3.org/2002/07/owl#";

/// Ggen ontology class and property URIs
pub struct GgenOntology;

impl GgenOntology {
    /// Template class URI.
    pub fn template() -> String {
        format!("{}Template", GGEN_NAMESPACE)
    }

    /// File class URI.
    pub fn file() -> String {
        format!("{}File", GGEN_NAMESPACE)
    }

    /// Variable class URI.
    pub fn variable() -> String {
        format!("{}Variable", GGEN_NAMESPACE)
    }

    /// Directory class URI.
    pub fn directory() -> String {
        format!("{}Directory", GGEN_NAMESPACE)
    }

    /// Artifact class URI.
    pub fn artifact() -> String {
        format!("{}Artifact", GGEN_NAMESPACE)
    }

    /// Dependency class URI.
    pub fn dependency() -> String {
        format!("{}Dependency", GGEN_NAMESPACE)
    }

    /// FileFormat class URI.
    pub fn file_format() -> String {
        format!("{}FileFormat", GGEN_NAMESPACE)
    }

    /// Generates file property URI.
    pub fn generates_file() -> String {
        format!("{}generatesFile", GGEN_NAMESPACE)
    }

    /// Generates directory property URI.
    pub fn generates_directory() -> String {
        format!("{}generatesDirectory", GGEN_NAMESPACE)
    }

    /// Has variable property URI.
    pub fn has_variable() -> String {
        format!("{}hasVariable", GGEN_NAMESPACE)
    }

    /// Requires variable property URI.
    pub fn requires_variable() -> String {
        format!("{}requiresVariable", GGEN_NAMESPACE)
    }

    /// Template name property URI.
    pub fn template_name() -> String {
        format!("{}templateName", GGEN_NAMESPACE)
    }

    /// Template version property URI.
    pub fn template_version() -> String {
        format!("{}templateVersion", GGEN_NAMESPACE)
    }

    /// Template description property URI.
    pub fn template_description() -> String {
        format!("{}templateDescription", GGEN_NAMESPACE)
    }

    /// Template author property URI.
    pub fn template_author() -> String {
        format!("{}templateAuthor", GGEN_NAMESPACE)
    }

    /// Created at property URI.
    pub fn created_at() -> String {
        format!("{}createdAt", GGEN_NAMESPACE)
    }

    /// Updated at property URI.
    pub fn updated_at() -> String {
        format!("{}updatedAt", GGEN_NAMESPACE)
    }

    /// Variable name property URI.
    pub fn variable_name() -> String {
        format!("{}variableName", GGEN_NAMESPACE)
    }

    /// Variable type property URI.
    pub fn variable_type() -> String {
        format!("{}variableType", GGEN_NAMESPACE)
    }

    /// Variable default property URI.
    pub fn variable_default() -> String {
        format!("{}variableDefault", GGEN_NAMESPACE)
    }

    /// Variable description property URI.
    pub fn variable_description() -> String {
        format!("{}variableDescription", GGEN_NAMESPACE)
    }

    /// Is required property URI.
    pub fn is_required() -> String {
        format!("{}isRequired", GGEN_NAMESPACE)
    }

    /// File path property URI.
    pub fn file_path() -> String {
        format!("{}filePath", GGEN_NAMESPACE)
    }

    /// File extension property URI.
    pub fn file_extension() -> String {
        format!("{}fileExtension", GGEN_NAMESPACE)
    }

    /// File size property URI.
    pub fn file_size() -> String {
        format!("{}fileSize", GGEN_NAMESPACE)
    }

    /// Depends on property URI.
    pub fn depends_on() -> String {
        format!("{}dependsOn", GGEN_NAMESPACE)
    }

    /// Extends property URI.
    pub fn extends() -> String {
        format!("{}extends", GGEN_NAMESPACE)
    }

    /// Includes property URI.
    pub fn includes() -> String {
        format!("{}includes", GGEN_NAMESPACE)
    }

    /// Overrides property URI.
    pub fn overrides() -> String {
        format!("{}overrides", GGEN_NAMESPACE)
    }

    /// Category property URI.
    pub fn category() -> String {
        format!("{}category", GGEN_NAMESPACE)
    }

    /// Tag property URI.
    pub fn tag() -> String {
        format!("{}tag", GGEN_NAMESPACE)
    }

    /// Test coverage property URI.
    pub fn test_coverage() -> String {
        format!("{}testCoverage", GGEN_NAMESPACE)
    }

    /// Stability property URI.
    pub fn stability() -> String {
        format!("{}stability", GGEN_NAMESPACE)
    }

    /// Usage count property URI.
    pub fn usage_count() -> String {
        format!("{}usageCount", GGEN_NAMESPACE)
    }

    /// RDF type property URI.
    pub fn rdf_type() -> String {
        format!("{}type", RDF_NAMESPACE)
    }

    /// RDFS label property URI.
    pub fn rdfs_label() -> String {
        format!("{}label", RDFS_NAMESPACE)
    }

    /// RDFS comment property URI.
    pub fn rdfs_comment() -> String {
        format!("{}comment", RDFS_NAMESPACE)
    }
}

/// Load the Ggen schema as Turtle string
pub fn load_schema() -> Result<String> {
    Ok(include_str!("schema.ttl").to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ontology_uris() {
        assert_eq!(
            GgenOntology::template(),
            "https://ggen.io/marketplace/Template"
        );
        assert_eq!(
            GgenOntology::generates_file(),
            "https://ggen.io/marketplace/generatesFile"
        );
        assert_eq!(
            GgenOntology::template_name(),
            "https://ggen.io/marketplace/templateName"
        );
        assert_eq!(
            GgenOntology::rdf_type(),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
        );
    }

    #[test]
    fn test_load_schema() {
        let schema = load_schema().expect("Failed to load schema");
        assert!(schema.contains("@prefix ggen:"));
        assert!(schema.contains("ggen:Template a rdfs:Class"));
        assert!(schema.contains("ggen:generatesFile a rdf:Property"));
    }

    #[test]
    fn test_namespace_constants() {
        assert_eq!(GGEN_NAMESPACE, "https://ggen.io/marketplace/");
        assert_eq!(RDF_NAMESPACE, "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        assert_eq!(RDFS_NAMESPACE, "http://www.w3.org/2000/01/rdf-schema#");
    }
}
