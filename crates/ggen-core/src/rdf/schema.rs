//! Ggen RDF schema and ontology definitions
//!
//! This module provides programmatic access to the Ggen ontology,
//! making it easy to construct RDF triples with proper namespacing.

use anyhow::Result;

/// Ggen ontology namespace
pub const GGEN_NAMESPACE: &str = "http://ggen.dev/ontology#";

/// Standard RDF namespaces
pub const RDF_NAMESPACE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
pub const RDFS_NAMESPACE: &str = "http://www.w3.org/2000/01/rdf-schema#";
pub const XSD_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema#";
pub const OWL_NAMESPACE: &str = "http://www.w3.org/2002/07/owl#";

/// Ggen ontology class and property URIs
pub struct GgenOntology;

impl GgenOntology {
    // Core Classes
    pub fn template() -> String {
        format!("{}Template", GGEN_NAMESPACE)
    }

    pub fn file() -> String {
        format!("{}File", GGEN_NAMESPACE)
    }

    pub fn variable() -> String {
        format!("{}Variable", GGEN_NAMESPACE)
    }

    pub fn directory() -> String {
        format!("{}Directory", GGEN_NAMESPACE)
    }

    pub fn artifact() -> String {
        format!("{}Artifact", GGEN_NAMESPACE)
    }

    pub fn dependency() -> String {
        format!("{}Dependency", GGEN_NAMESPACE)
    }

    pub fn file_format() -> String {
        format!("{}FileFormat", GGEN_NAMESPACE)
    }

    // Generation Properties
    pub fn generates_file() -> String {
        format!("{}generatesFile", GGEN_NAMESPACE)
    }

    pub fn generates_directory() -> String {
        format!("{}generatesDirectory", GGEN_NAMESPACE)
    }

    pub fn has_variable() -> String {
        format!("{}hasVariable", GGEN_NAMESPACE)
    }

    pub fn requires_variable() -> String {
        format!("{}requiresVariable", GGEN_NAMESPACE)
    }

    // Template Metadata Properties
    pub fn template_name() -> String {
        format!("{}templateName", GGEN_NAMESPACE)
    }

    pub fn template_version() -> String {
        format!("{}templateVersion", GGEN_NAMESPACE)
    }

    pub fn template_description() -> String {
        format!("{}templateDescription", GGEN_NAMESPACE)
    }

    pub fn template_author() -> String {
        format!("{}templateAuthor", GGEN_NAMESPACE)
    }

    pub fn created_at() -> String {
        format!("{}createdAt", GGEN_NAMESPACE)
    }

    pub fn updated_at() -> String {
        format!("{}updatedAt", GGEN_NAMESPACE)
    }

    // Variable Properties
    pub fn variable_name() -> String {
        format!("{}variableName", GGEN_NAMESPACE)
    }

    pub fn variable_type() -> String {
        format!("{}variableType", GGEN_NAMESPACE)
    }

    pub fn variable_default() -> String {
        format!("{}variableDefault", GGEN_NAMESPACE)
    }

    pub fn variable_description() -> String {
        format!("{}variableDescription", GGEN_NAMESPACE)
    }

    pub fn is_required() -> String {
        format!("{}isRequired", GGEN_NAMESPACE)
    }

    // File Properties
    pub fn file_path() -> String {
        format!("{}filePath", GGEN_NAMESPACE)
    }

    pub fn file_extension() -> String {
        format!("{}fileExtension", GGEN_NAMESPACE)
    }

    pub fn file_size() -> String {
        format!("{}fileSize", GGEN_NAMESPACE)
    }

    // Relationship Properties
    pub fn depends_on() -> String {
        format!("{}dependsOn", GGEN_NAMESPACE)
    }

    pub fn extends() -> String {
        format!("{}extends", GGEN_NAMESPACE)
    }

    pub fn includes() -> String {
        format!("{}includes", GGEN_NAMESPACE)
    }

    pub fn overrides() -> String {
        format!("{}overrides", GGEN_NAMESPACE)
    }

    // Categorization Properties
    pub fn category() -> String {
        format!("{}category", GGEN_NAMESPACE)
    }

    pub fn tag() -> String {
        format!("{}tag", GGEN_NAMESPACE)
    }

    // Quality Properties
    pub fn test_coverage() -> String {
        format!("{}testCoverage", GGEN_NAMESPACE)
    }

    pub fn stability() -> String {
        format!("{}stability", GGEN_NAMESPACE)
    }

    pub fn usage_count() -> String {
        format!("{}usageCount", GGEN_NAMESPACE)
    }

    // Standard RDF properties
    pub fn rdf_type() -> String {
        format!("{}type", RDF_NAMESPACE)
    }

    pub fn rdfs_label() -> String {
        format!("{}label", RDFS_NAMESPACE)
    }

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
            "http://ggen.dev/ontology#Template"
        );
        assert_eq!(
            GgenOntology::generates_file(),
            "http://ggen.dev/ontology#generatesFile"
        );
        assert_eq!(
            GgenOntology::template_name(),
            "http://ggen.dev/ontology#templateName"
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
        assert_eq!(GGEN_NAMESPACE, "http://ggen.dev/ontology#");
        assert_eq!(
            RDF_NAMESPACE,
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        );
        assert_eq!(RDFS_NAMESPACE, "http://www.w3.org/2000/01/rdf-schema#");
    }
}
