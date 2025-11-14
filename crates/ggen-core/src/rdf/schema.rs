//! Ggen RDF schema and ontology definitions
//!
//! This module provides programmatic access to the Ggen ontology,
//! making it easy to construct RDF triples with proper namespacing.
//!
//! ## Features
//!
//! - **Namespace constants**: Pre-defined URIs for Ggen, RDF, RDFS, XSD, and OWL
//! - **Ontology builder**: Type-safe methods for constructing ontology triples
//! - **Standard compliance**: Uses W3C standard namespaces
//!
//! ## Examples
//!
//! ### Using Namespace Constants
//!
//! ```rust,no_run
//! use ggen_core::rdf::schema::{GGEN_NAMESPACE, RDF_NAMESPACE, RDFS_NAMESPACE};
//!
//! // Construct URIs with proper namespacing
//! let template_uri = format!("{}Template", GGEN_NAMESPACE);
//! let class_uri = format!("{}Class", RDFS_NAMESPACE);
//! let type_uri = format!("{}type", RDF_NAMESPACE);
//! ```
//!
//! ### Building Ontology Triples
//!
//! ```rust
//! use ggen_core::rdf::schema::GgenOntology;
//!
//! # fn main() {
//! // Create a template class URI
//! let template_class = GgenOntology::template();
//!
//! // Create a variable class URI
//! let variable_class = GgenOntology::variable();
//!
//! // Use in RDF construction
//! println!("Template class: {}", template_class);
//! println!("Variable class: {}", variable_class);
//! # }
//! ```

use ggen_utils::error::Result;

/// Ggen ontology namespace
pub const GGEN_NAMESPACE: &str = "http://ggen.dev/ontology#";

/// Standard RDF namespaces
pub const RDF_NAMESPACE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
pub const RDFS_NAMESPACE: &str = "http://www.w3.org/2000/01/rdf-schema#";
pub const XSD_NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema#";
pub const OWL_NAMESPACE: &str = "http://www.w3.org/2002/07/owl#";

/// Ggen ontology class and property URIs
///
/// **Kaizen improvement**: All public methods include runnable doctest examples
/// for consistency and developer experience. Pattern: Use `# fn main() { ... }`
/// wrapper with assertions to verify URI generation.
pub struct GgenOntology;

impl GgenOntology {
    /// Get the URI for the Template class.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::template();
    /// assert!(uri.contains("Template"));
    /// # }
    /// ```
    pub fn template() -> String {
        format!("{}Template", GGEN_NAMESPACE)
    }

    /// Get the URI for the File class.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::file();
    /// assert!(uri.contains("File"));
    /// # }
    /// ```
    pub fn file() -> String {
        format!("{}File", GGEN_NAMESPACE)
    }

    /// Get the URI for the Variable class.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::variable();
    /// assert!(uri.contains("Variable"));
    /// # }
    /// ```
    pub fn variable() -> String {
        format!("{}Variable", GGEN_NAMESPACE)
    }

    /// Get the URI for the Directory class.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::directory();
    /// assert!(uri.contains("Directory"));
    /// # }
    /// ```
    pub fn directory() -> String {
        format!("{}Directory", GGEN_NAMESPACE)
    }

    /// Get the URI for the Artifact class.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::artifact();
    /// assert!(uri.contains("Artifact"));
    /// # }
    /// ```
    pub fn artifact() -> String {
        format!("{}Artifact", GGEN_NAMESPACE)
    }

    /// Get the URI for the Dependency class.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::dependency();
    /// assert!(uri.contains("Dependency"));
    /// # }
    /// ```
    pub fn dependency() -> String {
        format!("{}Dependency", GGEN_NAMESPACE)
    }

    /// Get the URI for the FileFormat class.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::file_format();
    /// assert!(uri.contains("FileFormat"));
    /// # }
    /// ```
    pub fn file_format() -> String {
        format!("{}FileFormat", GGEN_NAMESPACE)
    }

    // Generation Properties
    /// Get the URI for the generatesFile property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::generates_file();
    /// assert!(uri.contains("generatesFile"));
    /// # }
    /// ```
    pub fn generates_file() -> String {
        format!("{}generatesFile", GGEN_NAMESPACE)
    }

    /// Get the URI for the generatesDirectory property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::generates_directory();
    /// assert!(uri.contains("generatesDirectory"));
    /// # }
    /// ```
    pub fn generates_directory() -> String {
        format!("{}generatesDirectory", GGEN_NAMESPACE)
    }

    /// Get the URI for the hasVariable property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::has_variable();
    /// assert!(uri.contains("hasVariable"));
    /// # }
    /// ```
    pub fn has_variable() -> String {
        format!("{}hasVariable", GGEN_NAMESPACE)
    }

    /// Get the URI for the requiresVariable property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::requires_variable();
    /// assert!(uri.contains("requiresVariable"));
    /// # }
    /// ```
    pub fn requires_variable() -> String {
        format!("{}requiresVariable", GGEN_NAMESPACE)
    }

    // Template Metadata Properties
    /// Get the URI for the templateName property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::template_name();
    /// assert!(uri.contains("templateName"));
    /// # }
    /// ```
    pub fn template_name() -> String {
        format!("{}templateName", GGEN_NAMESPACE)
    }

    /// Get the URI for the templateVersion property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::template_version();
    /// assert!(uri.contains("templateVersion"));
    /// # }
    /// ```
    pub fn template_version() -> String {
        format!("{}templateVersion", GGEN_NAMESPACE)
    }

    /// Get the URI for the templateDescription property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::template_description();
    /// assert!(uri.contains("templateDescription"));
    /// # }
    /// ```
    pub fn template_description() -> String {
        format!("{}templateDescription", GGEN_NAMESPACE)
    }

    /// Get the URI for the templateAuthor property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::template_author();
    /// assert!(uri.contains("templateAuthor"));
    /// # }
    /// ```
    pub fn template_author() -> String {
        format!("{}templateAuthor", GGEN_NAMESPACE)
    }

    /// Get the URI for the createdAt property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::created_at();
    /// assert!(uri.contains("createdAt"));
    /// # }
    /// ```
    pub fn created_at() -> String {
        format!("{}createdAt", GGEN_NAMESPACE)
    }

    /// Get the URI for the updatedAt property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::updated_at();
    /// assert!(uri.contains("updatedAt"));
    /// # }
    /// ```
    pub fn updated_at() -> String {
        format!("{}updatedAt", GGEN_NAMESPACE)
    }

    // Variable Properties
    /// Get the URI for the variableName property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::variable_name();
    /// assert!(uri.contains("variableName"));
    /// # }
    /// ```
    pub fn variable_name() -> String {
        format!("{}variableName", GGEN_NAMESPACE)
    }

    /// Get the URI for the variableType property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::variable_type();
    /// assert!(uri.contains("variableType"));
    /// # }
    /// ```
    pub fn variable_type() -> String {
        format!("{}variableType", GGEN_NAMESPACE)
    }

    /// Get the URI for the variableDefault property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::variable_default();
    /// assert!(uri.contains("variableDefault"));
    /// # }
    /// ```
    pub fn variable_default() -> String {
        format!("{}variableDefault", GGEN_NAMESPACE)
    }

    /// Get the URI for the variableDescription property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::variable_description();
    /// assert!(uri.contains("variableDescription"));
    /// # }
    /// ```
    pub fn variable_description() -> String {
        format!("{}variableDescription", GGEN_NAMESPACE)
    }

    /// Get the URI for the isRequired property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::is_required();
    /// assert!(uri.contains("isRequired"));
    /// # }
    /// ```
    pub fn is_required() -> String {
        format!("{}isRequired", GGEN_NAMESPACE)
    }

    // File Properties
    /// Get the URI for the filePath property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::file_path();
    /// assert!(uri.contains("filePath"));
    /// # }
    /// ```
    pub fn file_path() -> String {
        format!("{}filePath", GGEN_NAMESPACE)
    }

    /// Get the URI for the fileExtension property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::file_extension();
    /// assert!(uri.contains("fileExtension"));
    /// # }
    /// ```
    pub fn file_extension() -> String {
        format!("{}fileExtension", GGEN_NAMESPACE)
    }

    /// Get the URI for the fileSize property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::file_size();
    /// assert!(uri.contains("fileSize"));
    /// # }
    /// ```
    pub fn file_size() -> String {
        format!("{}fileSize", GGEN_NAMESPACE)
    }

    // Relationship Properties
    /// Get the URI for the dependsOn property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::depends_on();
    /// assert!(uri.contains("dependsOn"));
    /// # }
    /// ```
    pub fn depends_on() -> String {
        format!("{}dependsOn", GGEN_NAMESPACE)
    }

    /// Get the URI for the extends property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::extends();
    /// assert!(uri.contains("extends"));
    /// # }
    /// ```
    pub fn extends() -> String {
        format!("{}extends", GGEN_NAMESPACE)
    }

    /// Get the URI for the includes property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::includes();
    /// assert!(uri.contains("includes"));
    /// # }
    /// ```
    pub fn includes() -> String {
        format!("{}includes", GGEN_NAMESPACE)
    }

    /// Get the URI for the overrides property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::overrides();
    /// assert!(uri.contains("overrides"));
    /// # }
    /// ```
    pub fn overrides() -> String {
        format!("{}overrides", GGEN_NAMESPACE)
    }

    // Categorization Properties
    /// Get the URI for the category property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::category();
    /// assert!(uri.contains("category"));
    /// # }
    /// ```
    pub fn category() -> String {
        format!("{}category", GGEN_NAMESPACE)
    }

    /// Get the URI for the tag property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::tag();
    /// assert!(uri.contains("tag"));
    /// # }
    /// ```
    pub fn tag() -> String {
        format!("{}tag", GGEN_NAMESPACE)
    }

    // Quality Properties
    /// Get the URI for the testCoverage property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::test_coverage();
    /// assert!(uri.contains("testCoverage"));
    /// # }
    /// ```
    pub fn test_coverage() -> String {
        format!("{}testCoverage", GGEN_NAMESPACE)
    }

    /// Get the URI for the stability property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::stability();
    /// assert!(uri.contains("stability"));
    /// # }
    /// ```
    pub fn stability() -> String {
        format!("{}stability", GGEN_NAMESPACE)
    }

    /// Get the URI for the usageCount property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::usage_count();
    /// assert!(uri.contains("usageCount"));
    /// # }
    /// ```
    pub fn usage_count() -> String {
        format!("{}usageCount", GGEN_NAMESPACE)
    }

    // Standard RDF properties
    /// Get the URI for the RDF type property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::rdf_type();
    /// assert!(uri.contains("type"));
    /// # }
    /// ```
    pub fn rdf_type() -> String {
        format!("{}type", RDF_NAMESPACE)
    }

    /// Get the URI for the RDFS label property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::rdfs_label();
    /// assert!(uri.contains("label"));
    /// # }
    /// ```
    pub fn rdfs_label() -> String {
        format!("{}label", RDFS_NAMESPACE)
    }

    /// Get the URI for the RDFS comment property.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::schema::GgenOntology;
    ///
    /// # fn main() {
    /// let uri = GgenOntology::rdfs_comment();
    /// assert!(uri.contains("comment"));
    /// # }
    /// ```
    pub fn rdfs_comment() -> String {
        format!("{}comment", RDFS_NAMESPACE)
    }
}

/// Load the Ggen schema as Turtle string
///
/// Returns the complete Ggen ontology schema in Turtle format.
///
/// # Examples
///
/// ```rust
/// use ggen_core::rdf::schema::load_schema;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let schema = load_schema()?;
/// assert!(schema.contains("@prefix ggen:"));
/// assert!(schema.contains("ggen:Template"));
/// # Ok(())
/// # }
/// ```
pub fn load_schema() -> Result<String> {
    Ok(include_str!("schema.ttl").to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};

    test!(test_ontology_uris, {
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
    });

    test!(test_load_schema, {
        let schema = load_schema().expect("Failed to load schema");
        assert!(schema.contains("@prefix ggen:"));
        assert!(schema.contains("ggen:Template a rdfs:Class"));
        assert!(schema.contains("ggen:generatesFile a rdf:Property"));
    });

    test!(test_namespace_constants, {
        assert_eq!(GGEN_NAMESPACE, "http://ggen.dev/ontology#");
        assert_eq!(RDF_NAMESPACE, "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
        assert_eq!(RDFS_NAMESPACE, "http://www.w3.org/2000/01/rdf-schema#");
    });
}
