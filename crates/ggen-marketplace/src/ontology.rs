//! RDF Ontology for ggen marketplace
//!
//! Defines the semantic model for packages, versions, dependencies, and metadata
//! in RDF format. All marketplace data is stored as RDF triples in oxigraph.

/// Legacy namespace constant for backward compatibility
pub const GGEN_NAMESPACE: &str = "http://ggen.dev/ontology#";

/// GgenOntology struct for backward compatibility
pub struct GgenOntology;

impl GgenOntology {
    /// Create a new GgenOntology instance
    pub fn new() -> Self {
        Self
    }

    /// Get namespace URI
    pub fn namespace(&self) -> &'static str {
        GGEN_NAMESPACE
    }

    /// Get template URI
    pub fn template() -> String {
        format!("{}Template", GGEN_NAMESPACE)
    }

    /// Get template version URI
    pub fn template_version() -> String {
        format!("{}templateVersion", GGEN_NAMESPACE)
    }

    /// Get depends_on URI
    pub fn depends_on() -> String {
        format!("{}dependsOn", GGEN_NAMESPACE)
    }
}

impl Default for GgenOntology {
    fn default() -> Self {
        Self::new()
    }
}

/// Core namespaces for marketplace RDF
pub struct Namespaces;

impl Namespaces {
    /// ggen marketplace namespace
    pub const GGEN: &'static str = "https://ggen.io/marketplace/";

    /// RDF namespace
    pub const RDF: &'static str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

    /// RDFS namespace
    pub const RDFS: &'static str = "http://www.w3.org/2000/01/rdf-schema#";

    /// Dublin Core metadata
    pub const DC: &'static str = "http://purl.org/dc/elements/1.1/";

    /// FOAF (Friend of a Friend)
    pub const FOAF: &'static str = "http://xmlns.com/foaf/0.1/";

    /// SKOS (Simple Knowledge Organization System)
    pub const SKOS: &'static str = "http://www.w3.org/2004/02/skos/core#";

    /// XSD datatypes
    pub const XSD: &'static str = "http://www.w3.org/2001/XMLSchema#";
}

/// RDF Classes for marketplace
pub struct Classes;

impl Classes {
    /// Create a class URI
    pub fn uri(name: &str) -> String {
        format!("{}classes/{}", Namespaces::GGEN, name)
    }

    /// Package class
    pub fn package() -> String {
        Self::uri("Package")
    }

    /// PackageVersion class
    pub fn package_version() -> String {
        Self::uri("PackageVersion")
    }

    /// Author class
    pub fn author() -> String {
        Self::uri("Author")
    }

    /// Dependency class
    pub fn dependency() -> String {
        Self::uri("Dependency")
    }

    /// License class
    pub fn license() -> String {
        Self::uri("License")
    }

    /// Repository class
    pub fn repository() -> String {
        Self::uri("Repository")
    }

    /// ValidationResult class
    pub fn validation_result() -> String {
        Self::uri("ValidationResult")
    }
}

/// RDF Properties for marketplace
pub struct Properties;

impl Properties {
    /// Create a property URI
    pub fn uri(name: &str) -> String {
        format!("{}properties/{}", Namespaces::GGEN, name)
    }

    /// Package ID property
    pub fn package_id() -> String {
        Self::uri("packageId")
    }

    /// Package name property
    pub fn name() -> String {
        Self::uri("name")
    }

    /// Package description
    pub fn description() -> String {
        Self::uri("description")
    }

    /// Package version
    pub fn version() -> String {
        Self::uri("version")
    }

    /// Has version relationship
    pub fn has_version() -> String {
        Self::uri("hasVersion")
    }

    /// Has dependency relationship
    pub fn has_dependency() -> String {
        Self::uri("hasDependency")
    }

    /// Has author relationship
    pub fn has_author() -> String {
        Self::uri("hasAuthor")
    }

    /// Author name (using FOAF)
    pub fn author_name() -> String {
        format!("{}name", Namespaces::FOAF)
    }

    /// License property
    pub fn license() -> String {
        Self::uri("license")
    }

    /// Repository URL
    pub fn repository_url() -> String {
        Self::uri("repositoryUrl")
    }

    /// Homepage URL
    pub fn homepage_url() -> String {
        Self::uri("homepageUrl")
    }

    /// Keywords (SKOS)
    pub fn keywords() -> String {
        format!("{}keywords", Namespaces::GGEN)
    }

    /// Quality score
    pub fn quality_score() -> String {
        Self::uri("qualityScore")
    }

    /// Download count
    pub fn downloads() -> String {
        Self::uri("downloads")
    }

    /// Created timestamp
    pub fn created_at() -> String {
        Self::uri("createdAt")
    }

    /// Updated timestamp
    pub fn updated_at() -> String {
        Self::uri("updatedAt")
    }

    /// Checksum (SHA-256)
    pub fn checksum() -> String {
        Self::uri("checksum")
    }

    /// Signature (Ed25519)
    pub fn signature() -> String {
        Self::uri("signature")
    }

    /// Public key
    pub fn public_key() -> String {
        Self::uri("publicKey")
    }
}

/// Named node URIs for common concepts
pub struct Uris;

impl Uris {
    /// Create a resource URI
    pub fn resource(name: &str) -> String {
        format!("{}resources/{}", Namespaces::GGEN, name)
    }

    /// RDF type
    pub fn rdf_type() -> String {
        format!("{}type", Namespaces::RDF)
    }

    /// Literal type
    pub fn literal_type() -> String {
        format!("{}Literal", Namespaces::RDFS)
    }

    /// String type
    pub fn string_type() -> String {
        format!("{}string", Namespaces::XSD)
    }

    /// Integer type
    pub fn integer_type() -> String {
        format!("{}integer", Namespaces::XSD)
    }

    /// Date type
    pub fn date_type() -> String {
        format!("{}dateTime", Namespaces::XSD)
    }

    /// Boolean type
    pub fn boolean_type() -> String {
        format!("{}boolean", Namespaces::XSD)
    }
}

/// SPARQL Query templates for common operations
pub struct Queries;

impl Queries {
    /// Query to find all packages
    pub fn all_packages() -> String {
        format!(
            r"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
            }}
            ",
            Namespaces::RDF,
            Classes::package()
        )
    }

    /// Query to search packages by name
    pub fn search_by_name(name: &str) -> String {
        format!(
            r#"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?pkgName .
                FILTER(CONTAINS(LCASE(str(?pkgName)), LCASE("{}")))
            }}
            "#,
            Namespaces::RDF,
            Classes::package(),
            Properties::name(),
            name
        )
    }

    /// Query to search packages by description
    pub fn search_by_description(text: &str) -> String {
        format!(
            r#"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?desc .
                FILTER(CONTAINS(LCASE(str(?desc)), LCASE("{}")))
            }}
            "#,
            Namespaces::RDF,
            Classes::package(),
            Properties::description(),
            text
        )
    }

    /// Query to get package versions
    pub fn package_versions(package_id: &str) -> String {
        format!(
            r"
            SELECT ?version WHERE {{
                <{}packages/{}> <{}> ?version .
            }}
            ORDER BY DESC(?version)
            ",
            Namespaces::GGEN,
            package_id,
            Properties::has_version()
        )
    }

    /// Query to get package dependencies
    pub fn package_dependencies(package_id: &str, version: &str) -> String {
        format!(
            r"
            SELECT ?dep_package ?dep_version WHERE {{
                <{}packages/{}/versions/{}> <{}> ?dep_node .
                ?dep_node <{}> ?dep_package .
                ?dep_node <{}> ?dep_version .
            }}
            ",
            Namespaces::GGEN,
            package_id,
            version,
            Properties::has_dependency(),
            Properties::package_id(),
            Properties::version()
        )
    }

    /// Query to find packages by quality score
    pub fn packages_by_quality(min_score: u32) -> String {
        format!(
            r"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?score .
                FILTER(?score >= {})
            }}
            ORDER BY DESC(?score)
            ",
            Namespaces::RDF,
            Classes::package(),
            Properties::quality_score(),
            min_score
        )
    }

    /// Query to find packages by category/keywords
    pub fn packages_by_keyword(keyword: &str) -> String {
        format!(
            r#"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?kw .
                FILTER(CONTAINS(LCASE(str(?kw)), LCASE("{}")))
            }}
            "#,
            Namespaces::RDF,
            Classes::package(),
            Properties::keywords(),
            keyword
        )
    }

    /// Query to get all authors
    pub fn all_authors() -> String {
        format!(
            r"
            SELECT DISTINCT ?author WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?author .
            }}
            ",
            Namespaces::RDF,
            Classes::package(),
            Properties::has_author()
        )
    }

    /// Query to find packages by author
    pub fn packages_by_author(author: &str) -> String {
        format!(
            r#"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?author .
                ?author <{}> ?authorName .
                FILTER(CONTAINS(LCASE(str(?authorName)), LCASE("{}")))
            }}
            "#,
            Namespaces::RDF,
            Classes::package(),
            Properties::has_author(),
            Properties::author_name(),
            author
        )
    }

    /// Query trending packages (by downloads)
    pub fn trending_packages(limit: usize) -> String {
        format!(
            r"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?downloads .
            }}
            ORDER BY DESC(?downloads)
            LIMIT {}
            ",
            Namespaces::RDF,
            Classes::package(),
            Properties::downloads(),
            limit
        )
    }

    /// Query recent packages
    pub fn recent_packages(limit: usize) -> String {
        format!(
            r"
            SELECT ?package WHERE {{
                ?package <{}type> <{}> .
                ?package <{}> ?created .
            }}
            ORDER BY DESC(?created)
            LIMIT {}
            ",
            Namespaces::RDF,
            Classes::package(),
            Properties::created_at(),
            limit
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_class_uris() {
        assert!(Classes::package().contains("Package"));
        assert!(Classes::package_version().contains("PackageVersion"));
        assert!(Classes::dependency().contains("Dependency"));
    }

    #[test]
    fn test_property_uris() {
        assert!(Properties::name().contains("name"));
        assert!(Properties::has_version().contains("hasVersion"));
        assert!(Properties::quality_score().contains("qualityScore"));
    }

    #[test]
    fn test_query_generation() {
        let all_pkg = Queries::all_packages();
        assert!(all_pkg.contains("SELECT"));
        assert!(all_pkg.contains("Package"));

        let search = Queries::search_by_name("test");
        assert!(search.contains("test"));
        assert!(search.contains("LCASE"));
    }
}
