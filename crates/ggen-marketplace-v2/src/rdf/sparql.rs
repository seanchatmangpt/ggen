//! SPARQL Query Layer with Type-Safe Builders
//!
//! All marketplace operations are executed as SPARQL queries.
//! No direct object manipulation or JSON/SQL fallbacks.

use crate::error::{Error, Result};
use crate::poka_yoke::{
    AuthorEmail, Category, Checksum, Keyword, LicenseId, PackageId, PackageVersion,
    QualityScore, RepositoryUrl,
};
use oxigraph::model::{GraphNameRef, Quad, Subject, Term};
use oxigraph::sparql::{QueryResults, QuerySolution};
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::str::FromStr;

use super::ontology::{Ontology, UriBuilder, MARKETPLACE_NS};

/// SPARQL executor with connection pooling and caching
pub struct SparqlExecutor {
    store: Store,
    uri_builder: UriBuilder,
}

impl SparqlExecutor {
    /// Create a new SPARQL executor with an oxigraph store
    pub fn new(store: Store) -> Self {
        Self {
            store,
            uri_builder: UriBuilder::default(),
        }
    }

    /// Execute a SELECT query
    pub fn query(&self, sparql: &str) -> Result<QueryResults> {
        self.store
            .query(sparql)
            .map_err(|e| Error::SparqlError {
                query: sparql.to_string(),
                reason: e.to_string(),
            })
    }

    /// Execute an UPDATE query (INSERT/DELETE)
    pub fn update(&self, sparql: &str) -> Result<()> {
        self.store
            .update(sparql)
            .map_err(|e| Error::SparqlError {
                query: sparql.to_string(),
                reason: e.to_string(),
            })
    }

    /// Insert a quad into the store
    pub fn insert_quad(&self, quad: Quad) -> Result<()> {
        self.store.insert(&quad).map_err(|e| Error::RdfStoreError {
            operation: "insert_quad".to_string(),
            reason: e.to_string(),
        })
    }

    /// Check if the store contains a quad
    pub fn contains(&self, quad: &Quad) -> Result<bool> {
        self.store
            .contains(quad)
            .map_err(|e| Error::RdfStoreError {
                operation: "contains".to_string(),
                reason: e.to_string(),
            })
    }

    /// Get the underlying store
    pub fn store(&self) -> &Store {
        &self.store
    }

    /// Get the URI builder
    pub fn uri_builder(&self) -> &UriBuilder {
        &self.uri_builder
    }
}

/// Type-safe SPARQL query builder
pub struct SparqlQueryBuilder {
    prefixes: String,
    select_vars: Vec<String>,
    where_clauses: Vec<String>,
    filters: Vec<String>,
    order_by: Option<String>,
    limit: Option<usize>,
    offset: Option<usize>,
}

impl SparqlQueryBuilder {
    /// Create a new query builder with default prefixes
    pub fn new() -> Self {
        Self {
            prefixes: Ontology::prefixes(),
            select_vars: Vec::new(),
            where_clauses: Vec::new(),
            filters: Vec::new(),
            order_by: None,
            limit: None,
            offset: None,
        }
    }

    /// Select specific variables
    pub fn select(mut self, vars: &[&str]) -> Self {
        self.select_vars = vars.iter().map(|v| v.to_string()).collect();
        self
    }

    /// Add a WHERE clause pattern
    pub fn where_clause(mut self, clause: impl Into<String>) -> Self {
        self.where_clauses.push(clause.into());
        self
    }

    /// Add a triple pattern to WHERE clause
    pub fn where_triple(mut self, subject: &str, predicate: &str, object: &str) -> Self {
        self.where_clauses
            .push(format!("{} {} {} .", subject, predicate, object));
        self
    }

    /// Add a FILTER clause
    pub fn filter(mut self, condition: impl Into<String>) -> Self {
        self.filters.push(condition.into());
        self
    }

    /// Add ORDER BY clause
    pub fn order_by(mut self, var: impl Into<String>) -> Self {
        self.order_by = Some(var.into());
        self
    }

    /// Add LIMIT clause
    pub fn limit(mut self, limit: usize) -> Self {
        self.limit = Some(limit);
        self
    }

    /// Add OFFSET clause
    pub fn offset(mut self, offset: usize) -> Self {
        self.offset = Some(offset);
        self
    }

    /// Build the SPARQL query string
    pub fn build(self) -> String {
        let mut query = self.prefixes;

        // SELECT
        query.push_str("SELECT ");
        if self.select_vars.is_empty() {
            query.push_str("*");
        } else {
            query.push_str(&self.select_vars.join(" "));
        }
        query.push_str("\nWHERE {\n");

        // WHERE clauses
        for clause in &self.where_clauses {
            query.push_str("  ");
            query.push_str(clause);
            query.push('\n');
        }

        // FILTER clauses
        for filter in &self.filters {
            query.push_str("  FILTER (");
            query.push_str(filter);
            query.push_str(")\n");
        }

        query.push_str("}\n");

        // ORDER BY
        if let Some(order) = &self.order_by {
            query.push_str("ORDER BY ");
            query.push_str(order);
            query.push('\n');
        }

        // LIMIT
        if let Some(limit) = self.limit {
            query.push_str(&format!("LIMIT {}\n", limit));
        }

        // OFFSET
        if let Some(offset) = self.offset {
            query.push_str(&format!("OFFSET {}\n", offset));
        }

        query
    }
}

impl Default for SparqlQueryBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Named SPARQL queries for all marketplace operations
pub struct SparqlQuery;

impl SparqlQuery {
    /// Search packages by keyword
    pub fn search_packages(keyword: &str, limit: usize) -> String {
        SparqlQueryBuilder::new()
            .select(&["?package", "?name", "?description", "?quality"])
            .where_triple("?package", "rdf:type", "mp:Package")
            .where_triple("?package", "mp:hasName", "?name")
            .where_triple("?package", "mp:hasDescription", "?description")
            .where_clause("OPTIONAL { ?package mp:hasKeyword ?keyword }")
            .where_clause("OPTIONAL { ?package mp:hasQualityScore ?quality }")
            .filter(format!(
                "CONTAINS(LCASE(?name), \"{}\") || CONTAINS(LCASE(?description), \"{}\") || CONTAINS(LCASE(?keyword), \"{}\")",
                keyword.to_lowercase(),
                keyword.to_lowercase(),
                keyword.to_lowercase()
            ))
            .order_by("DESC(?quality)")
            .limit(limit)
            .build()
    }

    /// Get package by ID
    pub fn get_package(package_id: &PackageId) -> String {
        let pkg_uri = format!("<{}>", UriBuilder::default().package(package_id.as_str()));
        SparqlQueryBuilder::new()
            .select(&[
                "?name",
                "?description",
                "?version",
                "?license",
                "?quality",
                "?created",
                "?updated",
            ])
            .where_triple(&pkg_uri, "mp:hasName", "?name")
            .where_triple(&pkg_uri, "mp:hasDescription", "?description")
            .where_triple(&pkg_uri, "mp:hasVersion", "?version")
            .where_triple(&pkg_uri, "mp:hasLicense", "?license")
            .where_clause(format!(
                "OPTIONAL {{ {} mp:hasQualityScore ?quality }}",
                pkg_uri
            ))
            .where_clause(format!(
                "OPTIONAL {{ {} mp:hasCreatedAt ?created }}",
                pkg_uri
            ))
            .where_clause(format!(
                "OPTIONAL {{ {} mp:hasUpdatedAt ?updated }}",
                pkg_uri
            ))
            .build()
    }

    /// List all packages with optional filters
    pub fn list_packages(
        category: Option<&str>,
        min_quality: Option<u32>,
        limit: usize,
        offset: usize,
    ) -> String {
        let mut builder = SparqlQueryBuilder::new()
            .select(&["?package", "?name", "?version", "?quality"])
            .where_triple("?package", "rdf:type", "mp:Package")
            .where_triple("?package", "mp:hasName", "?name")
            .where_triple("?package", "mp:hasVersion", "?version")
            .where_clause("OPTIONAL { ?package mp:hasQualityScore ?quality }");

        if let Some(cat) = category {
            builder = builder
                .where_triple("?package", "mp:hasCategory", &format!("\"{}\"", cat))
        }

        if let Some(min_q) = min_quality {
            builder = builder.filter(format!("?quality >= {}", min_q));
        }

        builder
            .order_by("DESC(?quality)")
            .limit(limit)
            .offset(offset)
            .build()
    }

    /// Get package dependencies
    pub fn get_dependencies(package_id: &PackageId, version: &PackageVersion) -> String {
        let ver_uri = format!(
            "<{}>",
            UriBuilder::default().version(package_id.as_str(), version.as_str())
        );
        SparqlQueryBuilder::new()
            .select(&["?depId", "?depVersion", "?isOptional"])
            .where_triple(&ver_uri, "mp:hasDependency", "?dep")
            .where_triple("?dep", "mp:dependsOn", "?depId")
            .where_triple("?dep", "mp:versionRequirement", "?depVersion")
            .where_clause("OPTIONAL { ?dep mp:isOptional ?isOptional }")
            .build()
    }

    /// Check package state
    pub fn get_package_state(package_id: &PackageId) -> String {
        let pkg_uri = format!("<{}>", UriBuilder::default().package(package_id.as_str()));
        SparqlQueryBuilder::new()
            .select(&["?state"])
            .where_triple(&pkg_uri, "mp:hasState", "?state")
            .build()
    }

    /// Get package maturity metrics
    pub fn get_maturity_metrics(package_id: &PackageId) -> String {
        let pkg_uri = format!("<{}>", UriBuilder::default().package(package_id.as_str()));
        SparqlQueryBuilder::new()
            .select(&[
                "?quality",
                "?downloads",
                "?versions",
                "?lastUpdate",
                "?testCoverage",
            ])
            .where_clause(format!("{} mp:hasQualityScore ?quality", pkg_uri))
            .where_clause(format!("{} mp:hasDownloadCount ?downloads", pkg_uri))
            .where_clause(format!(
                "{{ SELECT (COUNT(?v) as ?versions) WHERE {{ ?v mp:belongsToPackage {} }} }}",
                pkg_uri
            ))
            .where_clause(format!(
                "OPTIONAL {{ {} mp:hasUpdatedAt ?lastUpdate }}",
                pkg_uri
            ))
            .where_clause(format!(
                "OPTIONAL {{ {} mp:hasTestCoverage ?testCoverage }}",
                pkg_uri
            ))
            .build()
    }

    /// Validate package integrity
    pub fn validate_package(package_id: &PackageId) -> String {
        let pkg_uri = format!("<{}>", UriBuilder::default().package(package_id.as_str()));
        SparqlQueryBuilder::new()
            .select(&["?hasName", "?hasVersion", "?hasLicense", "?hasAuthor"])
            .where_clause(format!(
                "BIND(EXISTS {{ {} mp:hasName ?name }} as ?hasName)",
                pkg_uri
            ))
            .where_clause(format!(
                "BIND(EXISTS {{ {} mp:hasVersion ?version }} as ?hasVersion)",
                pkg_uri
            ))
            .where_clause(format!(
                "BIND(EXISTS {{ {} mp:hasLicense ?license }} as ?hasLicense)",
                pkg_uri
            ))
            .where_clause(format!(
                "BIND(EXISTS {{ {} mp:hasAuthor ?author }} as ?hasAuthor)",
                pkg_uri
            ))
            .build()
    }

    /// Insert package (SPARQL UPDATE)
    pub fn insert_package(
        package_id: &PackageId,
        name: &str,
        description: &str,
        version: &PackageVersion,
        license: &LicenseId,
    ) -> String {
        let pkg_uri = UriBuilder::default().package(package_id.as_str());
        let now = chrono::Utc::now().to_rfc3339();

        format!(
            r#"{}
INSERT DATA {{
  <{pkg_uri}> rdf:type mp:Package ;
    mp:hasId "{}" ;
    mp:hasName "{}" ;
    mp:hasDescription "{}" ;
    mp:hasVersion "{}" ;
    mp:hasLicense "{}" ;
    mp:hasState mp:Draft ;
    mp:hasCreatedAt "{}"^^xsd:dateTime ;
    mp:hasUpdatedAt "{}"^^xsd:dateTime .
}}
"#,
            Ontology::prefixes(),
            package_id.as_str().replace('"', "\\\""),
            name.replace('"', "\\\""),
            description.replace('"', "\\\""),
            version.as_str().replace('"', "\\\""),
            license.as_str().replace('"', "\\\""),
            now,
            now
        )
    }

    /// Update package state (SPARQL UPDATE)
    pub fn update_package_state(package_id: &PackageId, new_state: &str) -> String {
        let pkg_uri = UriBuilder::default().package(package_id.as_str());
        let now = chrono::Utc::now().to_rfc3339();

        format!(
            r#"{}
DELETE {{
  <{pkg_uri}> mp:hasState ?oldState .
}}
INSERT {{
  <{pkg_uri}> mp:hasState mp:{new_state} ;
    mp:hasUpdatedAt "{now}"^^xsd:dateTime .
}}
WHERE {{
  <{pkg_uri}> mp:hasState ?oldState .
}}
"#,
            Ontology::prefixes(),
        )
    }

    /// Add package metadata (authors, keywords, categories)
    pub fn add_package_metadata(
        package_id: &PackageId,
        authors: &[AuthorEmail],
        keywords: &[Keyword],
        categories: &[Category],
    ) -> String {
        let pkg_uri = UriBuilder::default().package(package_id.as_str());
        let mut triples = Vec::new();

        for author in authors {
            let author_uri = UriBuilder::default().author(author.as_str());
            triples.push(format!(
                "  <{pkg_uri}> mp:hasAuthor <{author_uri}> .\n  <{author_uri}> mp:hasEmail \"{}\" .",
                author.as_str().replace('"', "\\\"")
            ));
        }

        for keyword in keywords {
            triples.push(format!(
                "  <{pkg_uri}> mp:hasKeyword \"{}\" .",
                keyword.as_str().replace('"', "\\\"")
            ));
        }

        for category in categories {
            triples.push(format!(
                "  <{pkg_uri}> mp:hasCategory \"{}\" .",
                category.as_str().replace('"', "\\\"")
            ));
        }

        format!(
            r#"{}
INSERT DATA {{
{}
}}
"#,
            Ontology::prefixes(),
            triples.join("\n")
        )
    }

    /// Dashboard query - get aggregate statistics
    pub fn dashboard_stats() -> String {
        SparqlQueryBuilder::new()
            .select(&[
                "(COUNT(DISTINCT ?package) as ?totalPackages)",
                "(AVG(?quality) as ?avgQuality)",
                "(SUM(?downloads) as ?totalDownloads)",
            ])
            .where_triple("?package", "rdf:type", "mp:Package")
            .where_clause("OPTIONAL { ?package mp:hasQualityScore ?quality }")
            .where_clause("OPTIONAL { ?package mp:hasDownloadCount ?downloads }")
            .build()
    }
}

/// Result parser for SPARQL queries
pub struct SparqlResultParser;

impl SparqlResultParser {
    /// Parse a single solution binding
    pub fn get_string(solution: &QuerySolution, var: &str) -> Option<String> {
        solution.get(var).and_then(|term| {
            if let Term::Literal(lit) = term {
                Some(lit.value().to_string())
            } else {
                None
            }
        })
    }

    /// Parse an integer binding
    pub fn get_int(solution: &QuerySolution, var: &str) -> Option<i64> {
        Self::get_string(solution, var)
            .and_then(|s| s.parse::<i64>().ok())
    }

    /// Parse a boolean binding
    pub fn get_bool(solution: &QuerySolution, var: &str) -> Option<bool> {
        solution.get(var).and_then(|term| {
            if let Term::Literal(lit) = term {
                lit.value().parse::<bool>().ok()
            } else {
                None
            }
        })
    }

    /// Parse a URI binding
    pub fn get_uri(solution: &QuerySolution, var: &str) -> Option<String> {
        solution.get(var).map(|term| term.to_string())
    }
}

/// Package query result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageQueryResult {
    pub id: PackageId,
    pub name: String,
    pub description: String,
    pub version: PackageVersion,
    pub license: LicenseId,
    pub quality_score: Option<QualityScore>,
    pub created_at: Option<String>,
    pub updated_at: Option<String>,
}

/// Search result with relevance score
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SearchQueryResult {
    pub package: PackageQueryResult,
    pub relevance: f64,
}

/// Dependency query result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencyQueryResult {
    pub package_id: PackageId,
    pub version_requirement: String,
    pub is_optional: bool,
}

/// Maturity metrics from query
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MaturityMetrics {
    pub quality_score: Option<QualityScore>,
    pub download_count: u64,
    pub version_count: usize,
    pub last_update: Option<String>,
    pub test_coverage: Option<f64>,
}

/// Dashboard statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DashboardStats {
    pub total_packages: usize,
    pub average_quality: f64,
    pub total_downloads: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_packages_query() {
        let query = SparqlQuery::search_packages("rust", 10);
        assert!(query.contains("SELECT"));
        assert!(query.contains("FILTER"));
        assert!(query.contains("LIMIT 10"));
    }

    #[test]
    fn test_get_package_query() {
        let pkg_id = PackageId::new("test-package").unwrap();
        let query = SparqlQuery::get_package(&pkg_id);
        assert!(query.contains("SELECT"));
        assert!(query.contains("test-package"));
    }

    #[test]
    fn test_list_packages_query() {
        let query = SparqlQuery::list_packages(Some("web"), Some(80), 20, 0);
        assert!(query.contains("FILTER"));
        assert!(query.contains("web"));
        assert!(query.contains("LIMIT 20"));
    }

    #[test]
    fn test_insert_package_query() {
        let pkg_id = PackageId::new("new-package").unwrap();
        let version = PackageVersion::new("1.0.0").unwrap();
        let license = LicenseId::mit();
        let query = SparqlQuery::insert_package(&pkg_id, "New Package", "A new package", &version, &license);
        assert!(query.contains("INSERT DATA"));
        assert!(query.contains("new-package"));
        assert!(query.contains("New Package"));
    }

    #[test]
    fn test_update_state_query() {
        let pkg_id = PackageId::new("test-package").unwrap();
        let query = SparqlQuery::update_package_state(&pkg_id, "Published");
        assert!(query.contains("DELETE"));
        assert!(query.contains("INSERT"));
        assert!(query.contains("Published"));
    }

    #[test]
    fn test_query_builder() {
        let query = SparqlQueryBuilder::new()
            .select(&["?package", "?name"])
            .where_triple("?package", "rdf:type", "mp:Package")
            .where_triple("?package", "mp:hasName", "?name")
            .filter("?quality > 80")
            .order_by("DESC(?quality)")
            .limit(10)
            .build();

        assert!(query.contains("SELECT ?package ?name"));
        assert!(query.contains("WHERE {"));
        assert!(query.contains("FILTER (?quality > 80)"));
        assert!(query.contains("ORDER BY DESC(?quality)"));
        assert!(query.contains("LIMIT 10"));
    }

    #[test]
    fn test_dashboard_stats_query() {
        let query = SparqlQuery::dashboard_stats();
        assert!(query.contains("COUNT"));
        assert!(query.contains("AVG"));
        assert!(query.contains("SUM"));
    }
}
