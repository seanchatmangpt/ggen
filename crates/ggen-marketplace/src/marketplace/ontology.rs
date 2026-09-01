//! RDF Ontology for ggen marketplace
//!
//! Defines the semantic model for packages, versions, dependencies, and metadata
//! in RDF format. All marketplace data is stored as RDF triples in oxigraph.

#![allow(clippy::doc_markdown)]
#![allow(clippy::must_use_candidate)]

/// Canonical marketplace namespace (v1).
///
/// This is THE single source of truth for the ggen marketplace RDF namespace.
/// All code — including the enum-based ontology in
/// [`crate::marketplace::rdf::ontology`] — must reference this constant rather
/// than declaring its own literal. The `Classes`/`Properties` helpers below
/// and the enum-based `Class`/`Property` URIs are kept byte-identical so that a
/// triple inserted by one path is findable by a SPARQL query built by the
/// other. Divergent local property names (e.g. routing `packageName` to
/// `dc:title`) cause silent SPARQL data loss (see P0-03).
pub const MARKETPLACE_NS: &str = "https://ggen.io/marketplace/";

/// Core namespaces for marketplace RDF
pub struct Namespaces;

impl Namespaces {
    /// ggen marketplace namespace (use MARKETPLACE_NS constant instead)
    pub const GGEN: &'static str = MARKETPLACE_NS;

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
    #[must_use]
    pub fn uri(name: &str) -> String {
        format!("{}{}", Namespaces::GGEN, name)
    }

    /// Package class
    #[must_use]
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
        format!("{}{}", Namespaces::GGEN, name)
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

    /// Category (primary category/domain of a package) -- the predicate. The
    /// object of this triple is always a category *node* URI minted by
    /// [`Self::category_node_uri`], never a bare string literal, so it can be
    /// the subject of SKOS `related`/`broader`/`narrower` edges elsewhere in
    /// the graph (see [`Queries::related_by_category`]).
    pub fn category() -> String {
        Self::uri("category")
    }

    /// Mint the category-node URI for one category slug (e.g. `"web"` ->
    /// `https://ggen.io/marketplace/categories/web`). This is the single
    /// source of truth for that URI shape -- both the RDF write path
    /// (`RdfMapper::package_to_rdf`) and the SKOS-expansion query
    /// ([`Queries::related_by_category`]) must mint the identical URI for the
    /// same slug, or a package's real `category` triple and a query seeded
    /// from that same slug silently fail to join.
    pub fn category_node_uri(category_slug: &str) -> String {
        format!("{}categories/{}", Namespaces::GGEN, category_slug)
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

    /// Trust tier classification
    pub fn trust_tier() -> String {
        Self::uri("trustTier")
    }

    /// Registry type (ggen, crates.io, npm, pypi, etc.)
    pub fn registry_type() -> String {
        Self::uri("registryType")
    }
}

/// Named node URIs for common concepts
pub struct Uris;

impl Uris {
    /// Create a resource URI
    pub fn resource(name: &str) -> String {
        format!("{}{}", Namespaces::GGEN, name)
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

    /// Query to find other packages related by keyword overlap.
    ///
    /// For a given set of keyword literal strings, finds OTHER packages
    /// sharing at least one of those keywords via the existing `keywords`
    /// predicate, groups by package, and counts the number of shared
    /// keywords as `?overlap`. Pure graph-relation ranking over the RDF
    /// store -- no vectors, no embeddings, no external HTTP calls.
    pub fn related_by_keyword_overlap(package_keywords: &[String], limit: usize) -> String {
        let values = package_keywords
            .iter()
            .map(|kw| format!("\"{}\"", kw.replace('\\', "\\\\").replace('"', "\\\"")))
            .collect::<Vec<_>>()
            .join(" ");

        format!(
            r"
            SELECT ?package (COUNT(DISTINCT ?kw) AS ?overlap) WHERE {{
                ?package <{rdf_type}> <{package_class}> .
                ?package <{keywords}> ?kw .
                VALUES ?kw {{ {values} }}
            }}
            GROUP BY ?package
            ORDER BY DESC(?overlap)
            LIMIT {limit}
            ",
            rdf_type = format!("{}type", Namespaces::RDF),
            package_class = Classes::package(),
            keywords = Properties::keywords(),
            values = values,
            limit = limit
        )
    }

    /// Query to find packages related by category via SKOS relations.
    ///
    /// Expands from a seed category through `skos:related|skos:broader|skos:narrower`
    /// property paths to related categories, then finds packages in any of
    /// those categories. A real SPARQL property-path query, not
    /// application-code graph walking.
    ///
    /// `category` is a category *slug* (e.g. `"web"`), minted to the same
    /// category-node URI [`Properties::category_node_uri`] uses on the write
    /// path, not a raw literal -- category slugs are not attacker-controlled
    /// free text in this crate's own callers, but are still escaped the same
    /// way `related_by_keyword_overlap`'s VALUES literals are, since the slug
    /// is embedded directly into the query text.
    pub fn related_by_category(category: &str, limit: usize) -> String {
        let escaped = category
            .replace('\\', "\\\\")
            .replace('>', "\\>")
            .replace('"', "\\\"");
        format!(
            r#"
            SELECT DISTINCT ?package WHERE {{
                <{seed_category}> (<{skos}related>|<{skos}broader>|<{skos}narrower>)* ?relatedCategory .
                ?package <{rdf_type}> <{package_class}> .
                ?package <{category_prop}> ?relatedCategory .
            }}
            LIMIT {limit}
            "#,
            seed_category = Properties::category_node_uri(&escaped),
            skos = Namespaces::SKOS,
            rdf_type = format!("{}type", Namespaces::RDF),
            package_class = Classes::package(),
            category_prop = Properties::category(),
            limit = limit
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

    #[test]
    fn test_data_properties_under_canonical_namespace() {
        // The data-bearing package properties (the ones written by the
        // production insert path and read by the query builders) must all
        // resolve under MARKETPLACE_NS. This locks the single-source-of-truth
        // invariant that P0-03 violated.
        for uri in [
            Properties::package_id(),
            Properties::name(),
            Properties::description(),
            Properties::license(),
            Properties::repository_url(),
            Properties::homepage_url(),
            Properties::downloads(),
            Properties::created_at(),
            Properties::updated_at(),
            Properties::has_version(),
            Properties::has_dependency(),
            Properties::has_author(),
        ] {
            assert!(
                uri.starts_with(MARKETPLACE_NS),
                "data property {uri} must live under MARKETPLACE_NS"
            );
        }

        // Classes likewise.
        assert!(Classes::package().starts_with(MARKETPLACE_NS));
        assert!(Classes::package_version().starts_with(MARKETPLACE_NS));

        // Author NAME stays FOAF (genuine standard vocab, consistent both
        // sides); the author LINK is canonical.
        assert_eq!(Properties::author_name(), "http://xmlns.com/foaf/0.1/name");
        assert!(Properties::has_author().starts_with(MARKETPLACE_NS));
    }
}
