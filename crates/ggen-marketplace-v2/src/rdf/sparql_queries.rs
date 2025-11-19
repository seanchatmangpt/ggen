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
            .where_triple("?package", Property::PackageName, "?name")
            .where_pattern("?package ggen:hasVersion ?versionNode")
            .where_pattern("?versionNode ggen:versionNumber ?version");

        // Optional fields
        query = query.where_pattern("OPTIONAL { ?package dc:description ?description }");
        query = query.where_pattern(
            "OPTIONAL { ?package foaf:maker ?authorNode . ?authorNode foaf:name ?author }",
        );
        query = query.where_pattern("OPTIONAL { ?package ggen:rating ?rating }");
        query = query.where_pattern("OPTIONAL { ?package ggen:downloadCount ?downloads }");
        query = query.where_pattern("OPTIONAL { ?versionNode dc:created ?published }");

        // Apply filters
        if let Some(search_query) = &params.query {
            let filter = format!(
                "regex(?name, \"{}\", \"i\") || regex(?description, \"{}\", \"i\")",
                search_query, search_query
            );
            query = query.filter(filter);
        }

        if let Some(category) = &params.category {
            query = query
                .where_pattern("?package ggen:hasCategory ?cat")
                .filter(format!("?cat = \"{}\"", category));
        }

        if !params.tags.is_empty() {
            for tag in &params.tags {
                query = query
                    .where_pattern(&format!("?package ggen:hasTag ?tag{}", tag))
                    .filter(format!("?tag{} = \"{}\"", tag, tag));
            }
        }

        if let Some(min_rating) = params.min_rating {
            query = query.filter(format!("?rating >= {}", min_rating));
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
            .where_pattern(&format!("<{}> a ggen:Package", package_id))
            .where_pattern(&format!("<{}> dc:title ?name", package_id))
            .where_pattern("OPTIONAL { ?package dc:description ?description }")
            .where_pattern("OPTIONAL { ?package foaf:homepage ?homepage }")
            .where_pattern("OPTIONAL { ?package ggen:repository ?repository }")
            .where_pattern("OPTIONAL { ?package ggen:readme ?readme }")
            .where_pattern("OPTIONAL { ?package dc:license ?license }")
            .where_pattern(
                "OPTIONAL { ?package foaf:maker ?authorNode . ?authorNode foaf:name ?author }",
            )
            .validate()
    }

    /// Get all versions of a package
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
            .where_pattern(&format!("<{}> ggen:hasVersion ?version", package_id))
            .where_pattern("?version ggen:versionNumber ?versionNumber")
            .where_pattern("?version dc:created ?published")
            .where_pattern("OPTIONAL { ?version ggen:deprecated ?deprecated }")
            .where_pattern("OPTIONAL { ?version ggen:yanked ?yanked }")
            .order_by("DESC(?published)")
            .validate()
    }

    /// Get dependencies for a package version
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
            .where_pattern(&format!("<{}> ggen:hasVersion ?versionNode", package_id))
            .where_pattern(&format!("?versionNode ggen:versionNumber \"{}\"", version))
            .where_pattern("?versionNode ggen:hasDependency ?dep")
            .where_pattern("?dep ggen:dependsOn ?depPackage")
            .where_pattern("?depPackage dc:title ?depName")
            .where_pattern("?dep ggen:dependencyVersion ?depVersion")
            .where_pattern("?dep ggen:dependencyType ?depType")
            .where_pattern("OPTIONAL { ?dep ggen:isOptional ?optional }")
            .validate()
    }

    /// Check if package is installed
    pub fn get_installation_status(
        package_id: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("prov", namespaces::PROV)
            .select(&["?status", "?version", "?installedAt", "?path"])
            .where_pattern(&format!("<{}> ggen:hasInstallation ?install", package_id))
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
    pub fn get_validation_results(
        package_id: &str, version: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("prov", namespaces::PROV)
            .select(&["?result", "?status", "?validatedAt", "?violations"])
            .where_pattern(&format!("<{}> ggen:hasVersion ?versionNode", package_id))
            .where_pattern(&format!("?versionNode ggen:versionNumber \"{}\"", version))
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
    pub fn insert_validation_result(
        package_id: &str, version: &str, status: &str, violations: &[String],
    ) -> String {
        let result_id = format!(
            "{}#validation-{}",
            package_id,
            chrono::Utc::now().timestamp()
        );
        let violations_ttl = if violations.is_empty() {
            String::new()
        } else {
            let violations_str = violations
                .iter()
                .map(|v| format!("\"{}\"", v.replace('"', "\\\"")))
                .collect::<Vec<_>>()
                .join(", ");
            format!("        ggen:violations ( {} ) ;", violations_str)
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
    pub fn get_package_statistics() -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .select(&[
                "(COUNT(DISTINCT ?package) AS ?totalPackages)",
                "(SUM(?downloads) AS ?totalDownloads)",
                "(AVG(?rating) AS ?avgRating)",
                "(COUNT(DISTINCT ?author) AS ?totalAuthors)",
            ])
            .where_pattern("?package a ggen:Package")
            .where_pattern("OPTIONAL { ?package ggen:downloadCount ?downloads }")
            .where_pattern("OPTIONAL { ?package ggen:rating ?rating }")
            .where_pattern("OPTIONAL { ?package foaf:maker ?author }")
            .validate()
    }

    /// Get most popular packages
    pub fn get_popular_packages(
        limit: usize,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .prefix("dc", namespaces::DC)
            .select(&["?package", "?name", "?downloads", "?rating"])
            .where_pattern("?package a ggen:Package")
            .where_triple("?package", Property::PackageName, "?name")
            .where_pattern("?package ggen:downloadCount ?downloads")
            .where_pattern("OPTIONAL { ?package ggen:rating ?rating }")
            .order_by("DESC(?downloads)")
            .limit(limit)
            .validate()
    }

    /// Get recently published packages
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
            .where_pattern("?versionNode ggen:versionNumber ?version")
            .where_pattern("?versionNode dc:created ?published")
            .order_by("DESC(?published)")
            .limit(limit)
            .validate()
    }

    /// Get packages by category
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
            .where_pattern(&format!("?cat rdfs:label \"{}\"", category))
            .where_pattern("OPTIONAL { ?package dc:description ?description }")
            .validate()
    }

    /// Get packages by author
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
            .where_pattern("?package foaf:maker ?authorNode")
            .where_pattern(&format!("?authorNode foaf:name \"{}\"", author))
            .where_pattern("OPTIONAL { ?package dc:description ?description }")
            .validate()
    }

    /// Check for circular dependencies
    pub fn detect_circular_dependencies(
        package_id: &str,
    ) -> Result<SparqlQuery<typestate::Validated>, PokaYokeError> {
        SparqlQuery::new()
            .prefix("ggen", namespaces::GGEN)
            .select(&["?dep1", "?dep2", "?dep3"])
            .where_pattern(&format!(
                "<{}> ggen:hasVersion/ggen:hasDependency/ggen:dependsOn ?dep1",
                package_id
            ))
            .where_pattern("?dep1 ggen:hasVersion/ggen:hasDependency/ggen:dependsOn ?dep2")
            .where_pattern("?dep2 ggen:hasVersion/ggen:hasDependency/ggen:dependsOn ?dep3")
            .where_pattern(&format!("FILTER (?dep3 = <{}>)", package_id))
            .validate()
    }

    /// Update package download count
    pub fn increment_download_count(package_id: &str) -> String {
        format!(
            r#"PREFIX ggen: <{}>

DELETE {{
    <{package_id}> ggen:downloadCount ?oldCount .
}}
INSERT {{
    <{package_id}> ggen:downloadCount ?newCount .
}}
WHERE {{
    <{package_id}> ggen:downloadCount ?oldCount .
    BIND(?oldCount + 1 AS ?newCount)
}}"#,
            namespaces::GGEN,
            package_id = package_id,
        )
    }

    /// Update package rating
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
    pub fn deprecate_version(package_id: &str, version: &str) -> String {
        format!(
            r#"PREFIX ggen: <{}>
PREFIX xsd: <{}>

INSERT {{
    ?versionNode ggen:deprecated "true"^^xsd:boolean .
}}
WHERE {{
    <{package_id}> ggen:hasVersion ?versionNode .
    ?versionNode ggen:versionNumber "{version}" .
}}"#,
            namespaces::GGEN,
            namespaces::XSD,
            package_id = package_id,
            version = version,
        )
    }

    /// Yank a version
    pub fn yank_version(package_id: &str, version: &str) -> String {
        format!(
            r#"PREFIX ggen: <{}>
PREFIX xsd: <{}>

INSERT {{
    ?versionNode ggen:yanked "true"^^xsd:boolean .
}}
WHERE {{
    <{package_id}> ggen:hasVersion ?versionNode .
    ?versionNode ggen:versionNumber "{version}" .
}}"#,
            namespaces::GGEN,
            namespaces::XSD,
            package_id = package_id,
            version = version,
        )
    }

    /// Get dependency tree (recursive)
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
            .where_pattern(&format!("<{}> {} ?dep", package_id, depth_path))
            .where_pattern("?dep dc:title ?depName")
            .where_pattern("?dep ggen:hasVersion/ggen:versionNumber ?depVersion")
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
        assert!(sparql.contains("downloadCount"));
    }
}
