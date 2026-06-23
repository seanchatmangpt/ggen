//! Comprehensive SPARQL Query Operations
//!
//! This module provides type-safe SPARQL queries for all marketplace operations:
//! - Package search and discovery
//! - Installation lookups
//! - Validation queries
//! - Analytics and reporting
//! - State machine transitions
//!
//! All queries use the POKA YOKE type system for compile-time safety.

#![allow(clippy::missing_errors_doc)]
#![allow(clippy::needless_borrows_for_generic_args)]

use super::ontology::{namespaces, Property};
use super::poka_yoke::{typestate, PokaYokeError, SparqlQuery};
use serde::{Deserialize, Serialize};

/// Search parameters for package queries
#[derive(Debug, Clone, Default)]
pub struct SearchParams {
    pub query: Option<String>,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub author: Option<String>,
    pub min_rating: Option<f64>,
    pub limit: Option<usize>,
    pub offset: Option<usize>,
}

/// Package search result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageSearchResult {
    pub package_id: String,
    pub name: String,
    pub description: Option<String>,
    pub version: String,
    pub author: Option<String>,
    pub rating: Option<f64>,
    pub download_count: Option<i64>,
    pub published_at: Option<String>,
}

/// Installation record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstallationRecord {
    pub package_id: String,
    pub version: String,
    pub installed_at: String,
    pub status: String,
    pub install_path: String,
}

/// Validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationRecord {
    pub package_id: String,
    pub version: String,
    pub validated_at: String,
    pub status: String,
    pub violations: Vec<String>,
}

/// Dependency information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyInfo {
    /// The ID of the package that has dependencies
    pub package_id: String,
    /// The ID of the dependency package
    pub dependency_id: String,
    /// The version constraint for the dependency
    pub dependency_version: String,
    /// The type of dependency (e.g., "runtime", "dev", "build")
    pub dependency_type: String,
    /// Whether this dependency is optional
    pub is_optional: bool,
}

/// SPARQL query builder for marketplace operations
pub struct MarketplaceQueries;

impl MarketplaceQueries {
    /// Search for packages matching criteria
    pub fn search_packages(
        params: &SearchParams,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        let mut query = SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .prefix("foaf", namespaces::FOAF)
            .prefix("xsd", namespaces::XSD)
            .select(&[
                "?package",
                "?name",
                "?description",
                "?version",
                "?author",
                "?rating",
                "?downloads",
                "?published",
            ])
            .where_pattern("?package a ggen:Package")
            // Property::PackageName now resolves to ggen:name, matching the
            // canonical insert side. The version node carries ggen:version
            // (not ggen:versionNumber) per rdf_mapper::package_to_rdf.
            .where_triple("?package", Property::PackageName, "?name")
            .where_pattern("?package ggen:hasVersion ?versionNode")
            .where_pattern("?versionNode ggen:version ?version");

        // Optional fields — all predicates match what rdf_mapper inserts
        // under MARKETPLACE_NS (description, hasAuthor + foaf:name, downloads,
        // createdAt). ?rating is left as ggen:rating: it is a distinct concept
        // from ggen:qualityScore and is simply absent for unrated packages.
        query = query.where_pattern("OPTIONAL { ?package ggen:description ?description }");
        query = query.where_pattern(
            "OPTIONAL { ?package ggen:hasAuthor ?authorNode . ?authorNode foaf:name ?author }",
        );
        query = query.where_pattern("OPTIONAL { ?package ggen:rating ?rating }");
        query = query.where_pattern("OPTIONAL { ?package ggen:downloads ?downloads }");
        query = query.where_pattern("OPTIONAL { ?package ggen:createdAt ?published }");

        // Apply filters
        if let Some(search_query) = &params.query {
            let filter = format!(
                "regex(?name, \"{search_query}\", \"i\") || regex(?description, \"{search_query}\", \"i\")"
            );
            query = query.filter(filter);
        }

        if let Some(category) = &params.category {
            query = query
                .where_pattern("?package ggen:hasCategory ?cat")
                .filter(format!("?cat = \"{category}\""));
        }

        if !params.tags.is_empty() {
            for (idx, tag) in params.tags.iter().enumerate() {
                query = query
                    .where_pattern(&format!("?package ggen:hasTag ?tag{idx}"))
                    .filter(format!("?tag{idx} = \"{tag}\""));
            }
        }

        if let Some(min_rating) = params.min_rating {
            query = query.filter(format!("?rating >= {min_rating}"));
        }

        // Ordering and pagination
        query = query.order_by("DESC(?downloads)");

        if let Some(limit) = params.limit {
            query = query.limit(limit);
        }

        if let Some(offset) = params.offset {
            query = query.offset(offset);
        }

        query.validate()
    }

    /// Get package details by ID
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_package_details(
        package_id: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .prefix("foaf", namespaces::FOAF)
            .select(&[
                "?name",
                "?description",
                "?homepage",
                "?repository",
                "?readme",
                "?license",
                "?author",
            ])
            // Property URIs MUST match the canonical insert side
            // (`rdf_mapper::package_to_rdf`), which writes ggen:name,
            // ggen:description, ggen:license, ggen:homepageUrl,
            // ggen:repositoryUrl, ggen:hasAuthor (+ foaf:name on the author
            // node) under MARKETPLACE_NS. Previously these used dc:/foaf:
            // terms that no inserted triple carried, yielding empty results.
            .where_pattern(&format!("<{package_id}> a ggen:Package"))
            .where_pattern(&format!("<{package_id}> ggen:name ?name"))
            .where_pattern(&format!(
                "OPTIONAL {{ <{package_id}> ggen:description ?description }}"
            ))
            .where_pattern(&format!(
                "OPTIONAL {{ <{package_id}> ggen:homepageUrl ?homepage }}"
            ))
            .where_pattern(&format!(
                "OPTIONAL {{ <{package_id}> ggen:repositoryUrl ?repository }}"
            ))
            .where_pattern(&format!(
                "OPTIONAL {{ <{package_id}> ggen:readme ?readme }}"
            ))
            .where_pattern(&format!(
                "OPTIONAL {{ <{package_id}> ggen:license ?license }}"
            ))
            .where_pattern(&format!(
                "OPTIONAL {{ <{package_id}> ggen:hasAuthor ?authorNode . ?authorNode foaf:name ?author }}"
            ))
            .validate()
    }

    /// Get all versions of a package
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_package_versions(
        package_id: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .select(&[
                "?version",
                "?versionNumber",
                "?published",
                "?deprecated",
                "?yanked",
            ])
            // Version nodes carry ggen:version (the string) and ggen:releasedAt
            // (the publish date) per rdf_mapper. Previously this queried
            // ggen:versionNumber + dc:created, neither of which is inserted.
            .where_pattern(&format!("<{package_id}> ggen:hasVersion ?version"))
            .where_pattern("?version ggen:version ?versionNumber")
            .where_pattern("OPTIONAL { ?version ggen:releasedAt ?published }")
            .where_pattern("OPTIONAL { ?version ggen:deprecated ?deprecated }")
            .where_pattern("OPTIONAL { ?version ggen:yanked ?yanked }")
            .order_by("DESC(?published)")
            .validate()
    }

    /// Get dependencies for a package version
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_dependencies(
        package_id: &str, version: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .select(&[
                "?depPackage",
                "?depName",
                "?depVersion",
                "?depType",
                "?optional",
            ])
            .where_pattern(&format!("<{package_id}> ggen:hasVersion ?versionNode"))
            .where_pattern(&format!("?versionNode ggen:version \"{version}\""))
            .where_pattern("?versionNode ggen:hasDependency ?dep")
            .where_pattern("?dep ggen:dependsOn ?depPackage")
            // ggen:name matches the canonical insert (was dc:title).
            .where_pattern("?depPackage ggen:name ?depName")
            .where_pattern("?dep ggen:dependencyVersion ?depVersion")
            .where_pattern("?dep ggen:dependencyType ?depType")
            .where_pattern("OPTIONAL { ?dep ggen:isOptional ?optional }")
            .validate()
    }

    /// Check if package is installed
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_installation_status(
        package_id: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("prov", namespaces::PROV)
            .select(&["?status", "?version", "?installedAt", "?path"])
            .where_pattern(&format!("<{package_id}> ggen:hasInstallation ?install"))
            .where_pattern("?install a ggen:InstallationState")
            .where_pattern("?install ggen:installationStatus ?status")
            .where_pattern("?install ggen:versionNumber ?version")
            .where_pattern("?install prov:atTime ?installedAt")
            .where_pattern("OPTIONAL { ?install ggen:installPath ?path }")
            .order_by("DESC(?installedAt)")
            .limit(1)
            .validate()
    }

    /// Record package installation
    ///
    /// # Errors
    ///
    /// This function never returns an error
    #[must_use]
    pub fn insert_installation(
        package_id: &str, version: &str, status: &str, path: &str,
    ) -> String {
        let install_id = format!("{}#install-{}", package_id, chrono::Utc::now().timestamp());
        format!(
            r#"PREFIX ggen: <{}>
PREFIX prov: <{}>
PREFIX xsd: <{}>

INSERT DATA {{
    <{install_id}> a ggen:InstallationState ;
        ggen:forPackage <{package_id}> ;
        ggen:versionNumber "{version}" ;
        ggen:installationStatus "{status}" ;
        ggen:installPath "{path}" ;
        prov:atTime "{timestamp}"^^xsd:dateTime .

    <{package_id}> ggen:hasInstallation <{install_id}> .
}}"#,
            namespaces::GGEN,
            namespaces::PROV,
            namespaces::XSD,
            install_id = install_id,
            package_id = package_id,
            version = version,
            status = status,
            path = path,
            timestamp = chrono::Utc::now().to_rfc3339(),
        )
    }

    /// Get validation results for a package version
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_validation_results(
        package_id: &str, version: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("prov", namespaces::PROV)
            .select(&["?result", "?status", "?validatedAt", "?violations"])
            .where_pattern(&format!("<{package_id}> ggen:hasVersion ?versionNode"))
            // Version-node string predicate is ggen:version (canonical insert).
            .where_pattern(&format!("?versionNode ggen:version \"{version}\""))
            .where_pattern("?versionNode ggen:hasValidation ?result")
            .where_pattern("?result a ggen:ValidationResult")
            .where_pattern("?result ggen:validationStatus ?status")
            .where_pattern("?result prov:atTime ?validatedAt")
            .where_pattern("OPTIONAL { ?result ggen:violations ?violations }")
            .order_by("DESC(?validatedAt)")
            .limit(1)
            .validate()
    }

    /// Insert validation result
    ///
    /// # Errors
    ///
    /// This function never returns an error
    #[must_use]
    pub fn insert_validation_result(
        package_id: &str, version: &str, status: &str, violations: &[String],
    ) -> String {
        let result_id = format!("{package_id}#validation-{}", chrono::Utc::now().timestamp());
        let violations_ttl = if violations.is_empty() {
            String::new()
        } else {
            let violations_str = violations
                .iter()
                .map(|v| format!("\"{}\"", v.replace('"', "\\\"")))
                .collect::<Vec<_>>()
                .join(", ");
            format!("        ggen:violations ( {violations_str} ) ;")
        };

        format!(
            r#"PREFIX ggen: <{}>
PREFIX prov: <{}>
PREFIX xsd: <{}>

INSERT DATA {{
    <{result_id}> a ggen:ValidationResult ;
        ggen:forPackage <{package_id}> ;
        ggen:forVersion "{version}" ;
        ggen:validationStatus "{status}" ;
{violations_ttl}
        prov:atTime "{timestamp}"^^xsd:dateTime .

    <{package_id}> ggen:hasValidation <{result_id}> .
}}"#,
            namespaces::GGEN,
            namespaces::PROV,
            namespaces::XSD,
            result_id = result_id,
            package_id = package_id,
            version = version,
            status = status,
            violations_ttl = violations_ttl,
            timestamp = chrono::Utc::now().to_rfc3339(),
        )
    }

    /// Get package statistics
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_package_statistics() -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .select(&[
                "(COUNT(DISTINCT ?package) AS ?totalPackages)",
                "(SUM(?downloads) AS ?totalDownloads)",
                "(AVG(?rating) AS ?avgRating)",
                "(COUNT(DISTINCT ?author) AS ?totalAuthors)",
            ])
            // All predicates use the ggen: prefix (MARKETPLACE_NS) to match the
            // canonical insert side. Previously this referenced foaf:maker
            // without declaring the foaf prefix (parse failure) and
            // ggen:downloadCount (never inserted).
            .where_pattern("?package a ggen:Package")
            .where_pattern("OPTIONAL { ?package ggen:downloads ?downloads }")
            .where_pattern("OPTIONAL { ?package ggen:rating ?rating }")
            .where_pattern("OPTIONAL { ?package ggen:hasAuthor ?author }")
            .validate()
    }

    /// Get most popular packages
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_popular_packages(
        limit: usize,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .select(&["?package", "?name", "?downloads", "?rating"])
            .where_pattern("?package a ggen:Package")
            .where_triple("?package", Property::PackageName, "?name")
            // ggen:downloads matches the canonical insert (was ggen:downloadCount).
            .where_pattern("?package ggen:downloads ?downloads")
            .where_pattern("OPTIONAL { ?package ggen:rating ?rating }")
            .order_by("DESC(?downloads)")
            .limit(limit)
            .validate()
    }

    /// Get recently published packages
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_recent_packages(
        limit: usize,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .select(&["?package", "?name", "?version", "?published"])
            .where_pattern("?package a ggen:Package")
            .where_triple("?package", Property::PackageName, "?name")
            .where_pattern("?package ggen:hasVersion ?versionNode")
            // Version string is ggen:version; the package publish date is
            // ggen:createdAt on the package node (canonical insert).
            .where_pattern("?versionNode ggen:version ?version")
            .where_pattern("?package ggen:createdAt ?published")
            .order_by("DESC(?published)")
            .limit(limit)
            .validate()
    }

    /// Get packages by category
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_packages_by_category(
        category: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .select(&["?package", "?name", "?description"])
            .where_pattern("?package a ggen:Package")
            .where_triple("?package", Property::PackageName, "?name")
            .where_pattern("?package ggen:hasCategory ?cat")
            .where_pattern(&format!("?cat rdfs:label \"{category}\""))
            .where_pattern("OPTIONAL { ?package ggen:description ?description }")
            .validate()
    }

    /// Get packages by author
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_packages_by_author(
        author: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .prefix("foaf", namespaces::FOAF)
            .select(&["?package", "?name", "?description"])
            .where_pattern("?package a ggen:Package")
            .where_triple("?package", Property::PackageName, "?name")
            // Author link is ggen:hasAuthor (canonical insert); the author
            // node's name is foaf:name (genuine standard vocab, used on both
            // sides). Was foaf:maker, which no inserted triple carries.
            .where_pattern("?package ggen:hasAuthor ?authorNode")
            .where_pattern(&format!("?authorNode foaf:name \"{author}\""))
            .where_pattern("OPTIONAL { ?package ggen:description ?description }")
            .validate()
    }

    /// Check for circular dependencies
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn detect_circular_dependencies(
        package_id: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .select(&["?dep1", "?dep2", "?dep3"])
            .where_pattern(&format!(
                "<{package_id}> ggen:hasVersion/ggen:hasDependency/ggen:dependsOn ?dep1"
            ))
            .where_pattern("?dep1 ggen:hasVersion/ggen:hasDependency/ggen:dependsOn ?dep2")
            .where_pattern("?dep2 ggen:hasVersion/ggen:hasDependency/ggen:dependsOn ?dep3")
            .where_pattern(&format!("FILTER (?dep3 = <{package_id}>)"))
            .validate()
    }

    /// Update package download count
    ///
    /// # Errors
    ///
    /// This function never returns an error
    #[must_use]
    pub fn increment_download_count(package_id: &str) -> String {
        format!(
            r"PREFIX ggen: <{}>

DELETE {{
    <{package_id}> ggen:downloads ?oldCount .
}}
INSERT {{
    <{package_id}> ggen:downloads ?newCount .
}}
WHERE {{
    <{package_id}> ggen:downloads ?oldCount .
    BIND(?oldCount + 1 AS ?newCount)
}}",
            namespaces::GGEN,
            package_id = package_id,
        )
    }

    /// Update package rating
    ///
    /// # Errors
    ///
    /// This function never returns an error
    #[must_use]
    pub fn update_package_rating(package_id: &str, new_rating: f64) -> String {
        format!(
            r#"PREFIX ggen: <{}>
PREFIX xsd: <{}>

DELETE {{
    <{package_id}> ggen:rating ?oldRating .
}}
INSERT {{
    <{package_id}> ggen:rating "{new_rating}"^^xsd:decimal .
}}
WHERE {{
    OPTIONAL {{ <{package_id}> ggen:rating ?oldRating }}
}}"#,
            namespaces::GGEN,
            namespaces::XSD,
            package_id = package_id,
            new_rating = new_rating,
        )
    }

    /// Mark version as deprecated
    ///
    /// # Errors
    ///
    /// This function never returns an error
    #[must_use]
    pub fn deprecate_version(package_id: &str, version: &str) -> String {
        format!(
            r#"PREFIX ggen: <{}>
PREFIX xsd: <{}>

INSERT {{
    ?versionNode ggen:deprecated "true"^^xsd:boolean .
}}
WHERE {{
    <{package_id}> ggen:hasVersion ?versionNode .
    ?versionNode ggen:version "{version}" .
}}"#,
            namespaces::GGEN,
            namespaces::XSD,
            package_id = package_id,
            version = version,
        )
    }

    /// Yank a version
    ///
    /// # Errors
    ///
    /// This function never returns an error
    #[must_use]
    pub fn yank_version(package_id: &str, version: &str) -> String {
        format!(
            r#"PREFIX ggen: <{}>
PREFIX xsd: <{}>

INSERT {{
    ?versionNode ggen:yanked "true"^^xsd:boolean .
}}
WHERE {{
    <{package_id}> ggen:hasVersion ?versionNode .
    ?versionNode ggen:version "{version}" .
}}"#,
            namespaces::GGEN,
            namespaces::XSD,
            package_id = package_id,
            version = version,
        )
    }

    /// Get dependency tree (recursive)
    ///
    /// # Errors
    ///
    /// Returns [`PokaYokeError`] if query validation fails
    pub fn get_dependency_tree(
        package_id: &str, _version: &str, max_depth: usize,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        // Use property paths for recursive dependencies
        let depth_path = (1..=max_depth)
            .map(|_| "ggen:hasVersion/ggen:hasDependency/ggen:dependsOn")
            .collect::<Vec<_>>()
            .join("/");

        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .select(&["?dep", "?depName", "?depVersion"])
            .where_pattern(&format!("<{package_id}> {depth_path} ?dep"))
            // ggen:name + ggen:version match the canonical insert (were
            // dc:title + ggen:versionNumber).
            .where_pattern("?dep ggen:name ?depName")
            .where_pattern("?dep ggen:hasVersion/ggen:version ?depVersion")
            .validate()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_packages_basic() {
        let params = SearchParams {
            query: Some("react".to_string()),
            limit: Some(10),
            ..Default::default()
        };

        let query = MarketplaceQueries::search_packages(&params);
        assert!(query.is_ok());

        let query_str = query.unwrap().to_string();
        assert!(query_str.contains("SELECT"));
        assert!(query_str.contains("?name"));
        assert!(query_str.contains("LIMIT 10"));
    }

    #[test]
    fn test_get_package_details() {
        let query = MarketplaceQueries::get_package_details("http://ggen.dev/packages/test");
        assert!(query.is_ok());
    }

    #[test]
    fn test_insert_installation() {
        let sparql = MarketplaceQueries::insert_installation(
            "http://ggen.dev/packages/test",
            "1.0.0",
            "installed",
            "/path/to/install",
        );
        assert!(sparql.contains("INSERT DATA"));
        assert!(sparql.contains("InstallationState"));
    }

    #[test]
    fn test_get_dependencies() {
        let query = MarketplaceQueries::get_dependencies("http://ggen.dev/packages/test", "1.0.0");
        assert!(query.is_ok());
    }

    #[test]
    fn test_increment_download_count() {
        let sparql = MarketplaceQueries::increment_download_count("http://ggen.dev/packages/test");
        assert!(sparql.contains("DELETE"));
        assert!(sparql.contains("INSERT"));
        // Uses ggen:downloads (canonical insert predicate), not the legacy
        // ggen:downloadCount which no inserted triple carried.
        assert!(sparql.contains("ggen:downloads"));
    }
}
