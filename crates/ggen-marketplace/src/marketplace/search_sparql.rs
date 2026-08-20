//! SPARQL-based semantic search engine
//!
//! Implements intelligent package discovery using SPARQL queries against
//! the RDF knowledge graph. Enables semantic search, filtering, and ranking.

#![allow(clippy::missing_errors_doc)]
#![allow(clippy::uninlined_format_args)]
#![allow(clippy::double_must_use)]

use crate::marketplace::error::Result;
use crate::marketplace::ontology::Queries;
use oxigraph::store::Store;
use std::sync::Arc;
use tracing::debug;

/// SPARQL-powered search engine
///
/// Uses semantic queries to find packages based on:
/// - Full-text search (name, description, keywords)
/// - Quality filtering
/// - Author filtering
/// - Trending/recent packages
/// - Dependency relationships
pub struct SparqlSearchEngine {
    /// RDF triplestore
    store: Arc<Store>,
}

impl SparqlSearchEngine {
    /// Create a new SPARQL search engine
    #[must_use]
    pub fn new(store: Arc<Store>) -> Self {
        Self { store }
    }

    /// Search packages by name (lexical/substring search)
    ///
    /// Plain SPARQL `CONTAINS(LCASE(...))` substring matching against the
    /// package name literal -- not semantic/embedding-based search.
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn search_by_name(&self, name: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_name(name);
        self.execute_query(&query)
    }

    /// Search packages by description content (lexical/substring search)
    ///
    /// Plain SPARQL `CONTAINS(LCASE(...))` substring matching against the
    /// package description literal -- not semantic/embedding-based search.
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn search_by_description(&self, text: &str) -> Result<Vec<String>> {
        let query = Queries::search_by_description(text);
        self.execute_query(&query)
    }

    /// Search packages by keyword/category (lexical/substring search)
    ///
    /// Plain SPARQL `CONTAINS(LCASE(...))` substring matching against the
    /// package keyword literal -- not semantic/embedding-based search.
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn search_by_keyword(&self, keyword: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_keyword(keyword);
        self.execute_query(&query)
    }

    /// Find packages related by real SPARQL-computed keyword overlap
    /// (genuinely semantic in the RDF sense).
    ///
    /// Runs `Queries::related_by_keyword_overlap` against the RDF store: for
    /// the given set of keyword strings, finds other packages sharing at
    /// least one keyword, groups by package, and ranks by
    /// `COUNT(DISTINCT ?kw)` (shared-keyword overlap) descending -- computed
    /// entirely inside SPARQL via the graph's own relational structure, not
    /// lexical substring matching and not embeddings/vector similarity.
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn search_semantic(
        &self, package_id_or_keywords: &[String], limit: usize,
    ) -> Result<Vec<String>> {
        let query = Queries::related_by_keyword_overlap(package_id_or_keywords, limit);
        debug!(
            keyword_count = package_id_or_keywords.len(),
            limit, "search_semantic: executing related_by_keyword_overlap SPARQL query"
        );
        let results = self.execute_query(&query)?;
        debug!(
            result_count = results.len(),
            "search_semantic: SPARQL keyword-overlap query returned results"
        );
        Ok(results)
    }

    /// Find packages related by SKOS category-taxonomy expansion (genuinely
    /// semantic in the RDF sense).
    ///
    /// Runs `Queries::related_by_category` against the RDF store: starting
    /// from one category slug, expands transitively through
    /// `skos:related|skos:broader|skos:narrower` edges between category
    /// nodes, then returns every package whose `ggen:category` triple points
    /// at any category reached by that expansion -- real SPARQL 1.1
    /// property-path graph traversal, not application-code BFS/DFS and not
    /// embeddings/vector similarity.
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn search_related_by_category(&self, category: &str, limit: usize) -> Result<Vec<String>> {
        let query = Queries::related_by_category(category, limit);
        debug!(
            category,
            limit, "search_related_by_category: executing SKOS-expansion SPARQL query"
        );
        let results = self.execute_query(&query)?;
        debug!(
            result_count = results.len(),
            "search_related_by_category: SPARQL SKOS-expansion query returned results"
        );
        Ok(results)
    }

    /// Find packages by author
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn search_by_author(&self, author: &str) -> Result<Vec<String>> {
        let query = Queries::packages_by_author(author);
        self.execute_query(&query)
    }

    /// Get trending packages (sorted by downloads)
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn trending_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::trending_packages(limit);
        self.execute_query(&query)
    }

    /// Get recent packages (newly added)
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn recent_packages(&self, limit: usize) -> Result<Vec<String>> {
        let query = Queries::recent_packages(limit);
        self.execute_query(&query)
    }

    /// Find high-quality packages (quality score >= threshold)
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn search_by_quality(&self, min_score: u32) -> Result<Vec<String>> {
        let query = Queries::packages_by_quality(min_score);
        self.execute_query(&query)
    }

    /// Get all packages
    ///
    /// # Errors
    ///
    /// * `Error::SparqlError` - When the SPARQL query syntax is invalid
    /// * `Error::SearchError` - When querying the RDF store fails
    #[must_use]
    pub fn all_packages(&self) -> Result<Vec<String>> {
        let query = Queries::all_packages();
        self.execute_query(&query)
    }

    /// Execute a SPARQL query and extract results
    fn execute_query(&self, query: &str) -> Result<Vec<String>> {
        let results = self.store.query(query).map_err(|e| {
            crate::marketplace::error::Error::SearchError(format!("SPARQL query failed: {}", e))
        })?;

        let mut packages = Vec::new();

        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                match solution {
                    Ok(solution) => {
                        for (_, term) in solution.iter() {
                            if let oxigraph::model::Term::NamedNode(node) = term {
                                packages.push(node.as_str().to_string());
                            }
                        }
                    }
                    Err(e) => {
                        debug!("Error processing SPARQL solution: {}", e);
                    }
                }
            }
        }

        Ok(packages)
    }
}

/// Search filters for advanced queries
#[derive(Clone, Debug)]
pub struct SearchFilters {
    /// Minimum quality score (0-100)
    pub min_quality: Option<u32>,

    /// Author filter
    pub author: Option<String>,

    /// Category/keyword filter
    pub keyword: Option<String>,

    /// Maximum results
    pub limit: usize,
}

impl SearchFilters {
    /// Create empty filters
    #[must_use]
    pub fn new() -> Self {
        Self {
            min_quality: None,
            author: None,
            keyword: None,
            limit: 50,
        }
    }

    /// Set quality filter
    #[must_use]
    pub fn with_quality(mut self, min_score: u32) -> Self {
        self.min_quality = Some(min_score);
        self
    }

    /// Set author filter
    #[must_use]
    pub fn with_author(mut self, author: impl Into<String>) -> Self {
        self.author = Some(author.into());
        self
    }

    /// Set keyword filter
    #[must_use]
    pub fn with_keyword(mut self, keyword: impl Into<String>) -> Self {
        self.keyword = Some(keyword.into());
        self
    }

    /// Set result limit
    #[must_use]
    pub fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }
}

impl Default for SearchFilters {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_search_filters() {
        let filters = SearchFilters::new()
            .with_quality(80)
            .with_author("Alice")
            .with_keyword("database")
            .with_limit(100);

        assert_eq!(filters.min_quality, Some(80));
        assert_eq!(filters.author, Some("Alice".to_string()));
        assert_eq!(filters.keyword, Some("database".to_string()));
        assert_eq!(filters.limit, 100);
    }

    // --- Chicago-TDD tests for SPARQL-based semantic search ---
    //
    // Real oxigraph::store::Store (in-memory), real inserted triples via
    // real SPARQL UPDATE (INSERT DATA), real SPARQL SELECT queries executed
    // by `SparqlSearchEngine::search_semantic`. No mocks, no stubbed
    // results -- assertions are on the real ordered `Vec<String>` returned
    // by executing `Queries::related_by_keyword_overlap` against the store.

    use crate::marketplace::ontology::{Classes, Namespaces, Properties};
    use oxigraph::model::{GraphNameRef, Literal, NamedNode, QuadRef};

    /// Insert one real `ggen:Package` individual with real
    /// `rdf:type`/`ggen:name`/`ggen:keywords` triples directly into the
    /// store (same real-insert path the marketplace's own `registry_rdf.rs`
    /// uses -- `Store::insert`, not a mocked writer).
    fn insert_package(store: &Store, id: &str, name: &str, keywords: &[&str]) {
        let package_iri = format!("{}package/{}", Namespaces::GGEN, id);
        let package_node = NamedNode::new(&package_iri).expect("valid package IRI");
        let rdf_type = NamedNode::new(format!("{}type", Namespaces::RDF)).expect("valid rdf:type IRI");
        let package_class = NamedNode::new(Classes::package()).expect("valid Package class IRI");
        let name_prop = NamedNode::new(Properties::name()).expect("valid name property IRI");
        let keywords_prop =
            NamedNode::new(Properties::keywords()).expect("valid keywords property IRI");

        store
            .insert(QuadRef::new(
                &package_node,
                &rdf_type,
                &package_class,
                GraphNameRef::DefaultGraph,
            ))
            .expect("real triple insert must succeed against the in-memory store");
        store
            .insert(QuadRef::new(
                &package_node,
                &name_prop,
                Literal::new_simple_literal(name).as_ref(),
                GraphNameRef::DefaultGraph,
            ))
            .expect("real triple insert must succeed against the in-memory store");
        for kw in keywords {
            store
                .insert(QuadRef::new(
                    &package_node,
                    &keywords_prop,
                    Literal::new_simple_literal(*kw).as_ref(),
                    GraphNameRef::DefaultGraph,
                ))
                .expect("real triple insert must succeed against the in-memory store");
        }
    }

    /// Build a fresh in-memory oxigraph Store with four real packages:
    /// - pkg-a: keywords ["database", "async", "rust"]
    /// - pkg-b: keywords ["database", "async", "web"]   (2 keywords shared with pkg-a)
    /// - pkg-c: keywords ["frontend", "css"]             (0 keywords shared with pkg-a)
    /// - pkg-d: keywords ["database"]                    (1 keyword shared with pkg-a)
    fn build_store_with_packages() -> Arc<Store> {
        let store = Store::new().expect("real in-memory oxigraph Store must construct");
        insert_package(&store, "pkg-a", "Package A", &["database", "async", "rust"]);
        insert_package(&store, "pkg-b", "Package B", &["database", "async", "web"]);
        insert_package(&store, "pkg-c", "Package C", &["frontend", "css"]);
        insert_package(&store, "pkg-d", "Package D", &["database"]);
        Arc::new(store)
    }

    #[test]
    fn test_search_semantic_ranks_higher_keyword_overlap_above_no_overlap() {
        let store = build_store_with_packages();
        let engine = SparqlSearchEngine::new(store);

        // Seed the query from pkg-a's own keywords.
        let query_keywords = vec![
            "database".to_string(),
            "async".to_string(),
            "rust".to_string(),
        ];

        let results = engine
            .search_semantic(&query_keywords, 10)
            .expect("real SPARQL SELECT over the populated store must succeed");

        let pkg_b_iri = format!("{}package/pkg-b", Namespaces::GGEN);
        let pkg_c_iri = format!("{}package/pkg-c", Namespaces::GGEN);

        let pos_b = results
            .iter()
            .position(|p| p == &pkg_b_iri)
            .expect("pkg-b (2 shared keywords: database, async) must appear in results");
        let pos_c = results.iter().position(|p| p == &pkg_c_iri);

        // pkg-c shares zero keywords with the query, so it must not even
        // appear in the SPARQL VALUES-joined result set at all.
        assert!(
            pos_c.is_none(),
            "pkg-c shares no keywords with the query and must be absent from results, got {:?}",
            results
        );

        // pkg-a itself (self-match, 3/3 keywords) should rank first.
        let pkg_a_iri = format!("{}package/pkg-a", Namespaces::GGEN);
        let pos_a = results
            .iter()
            .position(|p| p == &pkg_a_iri)
            .expect("pkg-a must appear in results (self-match on all 3 keywords)");
        assert_eq!(
            pos_a, 0,
            "pkg-a (overlap=3) must rank first via real COUNT(DISTINCT ?kw) DESC ordering, got {:?}",
            results
        );

        // pkg-b (overlap=2) must rank above pkg-d (overlap=1).
        let pkg_d_iri = format!("{}package/pkg-d", Namespaces::GGEN);
        let pos_d = results
            .iter()
            .position(|p| p == &pkg_d_iri)
            .expect("pkg-d (1 shared keyword: database) must appear in results");
        assert!(
            pos_b < pos_d,
            "pkg-b (overlap=2) must rank above pkg-d (overlap=1) via real SPARQL ORDER BY DESC(?overlap), got {:?}",
            results
        );
    }

    #[test]
    fn test_search_semantic_excludes_packages_with_zero_keyword_overlap() {
        let store = build_store_with_packages();
        let engine = SparqlSearchEngine::new(store);

        // Query using only pkg-c's own keywords -- pkg-a/b/d share none of these.
        let query_keywords = vec!["frontend".to_string(), "css".to_string()];

        let results = engine
            .search_semantic(&query_keywords, 10)
            .expect("real SPARQL SELECT over the populated store must succeed");

        let pkg_c_iri = format!("{}package/pkg-c", Namespaces::GGEN);
        assert_eq!(
            results,
            vec![pkg_c_iri],
            "only pkg-c overlaps with [\"frontend\", \"css\"]; real SPARQL VALUES join must exclude a, b, d entirely"
        );
    }

    #[test]
    fn test_search_semantic_respects_limit() {
        let store = build_store_with_packages();
        let engine = SparqlSearchEngine::new(store);

        let query_keywords = vec!["database".to_string()];

        // pkg-a, pkg-b, pkg-d all share "database" (3 matches); cap to 2.
        let results = engine
            .search_semantic(&query_keywords, 2)
            .expect("real SPARQL SELECT with LIMIT must succeed");

        assert_eq!(
            results.len(),
            2,
            "real SPARQL LIMIT 2 must cap the result set to 2 rows, got {:?}",
            results
        );
    }

    // --- Chicago-TDD tests for SPARQL-based SKOS category-relation search ---
    //
    // Real oxigraph::store::Store, real inserted skos:broader/skos:related
    // edges between real category-node URIs, real ggen:category triples on
    // real packages, real SPARQL property-path traversal executed by
    // `SparqlSearchEngine::search_related_by_category`.

    /// Link one package to one category node (minted via
    /// `Properties::category_node_uri`, matching the real write path in
    /// `RdfMapper::package_to_rdf`).
    fn insert_package_category(store: &Store, package_id: &str, category_slug: &str) {
        let package_iri = format!("{}package/{}", Namespaces::GGEN, package_id);
        let package_node = NamedNode::new(&package_iri).expect("valid package IRI");
        let category_prop = NamedNode::new(Properties::category()).expect("valid category IRI");
        let category_node = NamedNode::new(Properties::category_node_uri(category_slug))
            .expect("valid category node IRI");

        store
            .insert(QuadRef::new(
                &package_node,
                &category_prop,
                &category_node,
                GraphNameRef::DefaultGraph,
            ))
            .expect("real triple insert must succeed against the in-memory store");
    }

    /// Insert one real `skos:broader` (or `skos:related`/`skos:narrower`)
    /// edge between two category-node URIs.
    fn insert_category_edge(store: &Store, from_slug: &str, skos_predicate: &str, to_slug: &str) {
        let from_node = NamedNode::new(Properties::category_node_uri(from_slug))
            .expect("valid category node IRI");
        let pred = NamedNode::new(format!("{}{}", Namespaces::SKOS, skos_predicate))
            .expect("valid skos predicate IRI");
        let to_node =
            NamedNode::new(Properties::category_node_uri(to_slug)).expect("valid category node IRI");

        store
            .insert(QuadRef::new(&from_node, &pred, &to_node, GraphNameRef::DefaultGraph))
            .expect("real triple insert must succeed against the in-memory store");
    }

    /// Also give each package a real `rdf:type ggen:Package` triple, since
    /// `related_by_category`'s query joins on it -- reuses the same
    /// `insert_package` helper's type-insertion shape without its keywords.
    fn insert_package_type(store: &Store, package_id: &str, name: &str) {
        insert_package(store, package_id, name, &[]);
    }

    #[test]
    fn test_search_related_by_category_expands_through_skos_broader() {
        let store = Store::new().expect("real in-memory oxigraph Store must construct");

        // Taxonomy: "web" --broader--> "backend" --broader--> "infra"
        insert_category_edge(&store, "web", "broader", "backend");
        insert_category_edge(&store, "backend", "broader", "infra");

        // pkg-web is directly in "web"; pkg-infra is two hops away via broader.
        insert_package_type(&store, "pkg-web", "Web Package");
        insert_package_category(&store, "pkg-web", "web");

        insert_package_type(&store, "pkg-infra", "Infra Package");
        insert_package_category(&store, "pkg-infra", "infra");

        // pkg-unrelated has a category with no SKOS edge to "web" at all.
        insert_package_type(&store, "pkg-unrelated", "Unrelated Package");
        insert_package_category(&store, "pkg-unrelated", "mobile");

        let engine = SparqlSearchEngine::new(Arc::new(store));
        let results = engine
            .search_related_by_category("web", 10)
            .expect("real SKOS property-path SPARQL query must succeed");

        let pkg_web_iri = format!("{}package/pkg-web", Namespaces::GGEN);
        let pkg_infra_iri = format!("{}package/pkg-infra", Namespaces::GGEN);
        let pkg_unrelated_iri = format!("{}package/pkg-unrelated", Namespaces::GGEN);

        assert!(
            results.contains(&pkg_web_iri),
            "pkg-web is directly in the seed category, must appear, got {:?}",
            results
        );
        assert!(
            results.contains(&pkg_infra_iri),
            "pkg-infra is 2 skos:broader hops from the seed category, must appear via real \
             property-path (skos:related|skos:broader|skos:narrower)* traversal, got {:?}",
            results
        );
        assert!(
            !results.contains(&pkg_unrelated_iri),
            "pkg-unrelated's category has no SKOS edge to the seed category at all, must be \
             absent, got {:?}",
            results
        );
    }

    #[test]
    fn test_search_related_by_category_no_taxonomy_edges_returns_only_direct_matches() {
        let store = Store::new().expect("real in-memory oxigraph Store must construct");

        // No skos:* edges inserted at all -- the property path's zero-hop
        // case (`*` includes the empty path) must still match packages
        // directly in the seed category.
        insert_package_type(&store, "pkg-direct", "Direct Package");
        insert_package_category(&store, "pkg-direct", "web");

        let engine = SparqlSearchEngine::new(Arc::new(store));
        let results = engine
            .search_related_by_category("web", 10)
            .expect("real SKOS property-path SPARQL query must succeed even with zero edges");

        let pkg_direct_iri = format!("{}package/pkg-direct", Namespaces::GGEN);
        assert_eq!(
            results,
            vec![pkg_direct_iri],
            "with no SKOS edges, only the direct category match must appear, got {:?}",
            results
        );
    }

    #[test]
    fn test_search_related_by_category_unknown_category_returns_empty() {
        let store = Store::new().expect("real in-memory oxigraph Store must construct");
        insert_package_type(&store, "pkg-web", "Web Package");
        insert_package_category(&store, "pkg-web", "web");

        let engine = SparqlSearchEngine::new(Arc::new(store));
        let results = engine
            .search_related_by_category("does-not-exist", 10)
            .expect("real SPARQL query over an unmatched seed category must still succeed");

        assert!(
            results.is_empty(),
            "a category slug with no node in the graph must yield zero results, got {:?}",
            results
        );
    }

    #[test]
    fn test_search_semantic_empty_store_returns_empty() {
        let store = Arc::new(Store::new().expect("real in-memory oxigraph Store must construct"));
        let engine = SparqlSearchEngine::new(store);

        let results = engine
            .search_semantic(&["database".to_string()], 10)
            .expect("real SPARQL SELECT over an empty store must still succeed, just with no rows");

        assert!(
            results.is_empty(),
            "an empty store has no packages, so real query execution must return zero results, got {:?}",
            results
        );
    }
}

