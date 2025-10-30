use crate::search::{IndexStats, SearchEngine};
use crate::types::{Facet as CustomFacet, Package, ScoredPackage, SearchQuery, SearchResults};
use anyhow::{Context, Result};
use async_trait::async_trait;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use tantivy::collector::{Count, TopDocs};
use tantivy::query::{BooleanQuery, FuzzyTermQuery, Occur, Query, QueryParser, TermQuery};
use tantivy::schema::*;
use tantivy::{Index, IndexReader, IndexWriter, ReloadPolicy, TantivyDocument};
use tokio::sync::RwLock;

/// Tantivy-based search engine with full-text search and faceted filtering
pub struct TantivySearchEngine {
    index: Index,
    reader: IndexReader,
    writer: Arc<RwLock<IndexWriter>>,
    schema: Schema,
    fields: SchemaFields,
}

#[derive(Clone)]
struct SchemaFields {
    id: Field,
    name: Field,
    description: Field,
    version: Field,
    category: Field,
    language: Field,
    license: Field,
    tags: Field,
    downloads: Field,
    rating: Field,
    created_at: Field,
    updated_at: Field,
    author: Field,
    repository_url: Field,
}

impl TantivySearchEngine {
    /// Create a new search engine with the given index directory
    pub fn new<P: AsRef<Path>>(index_path: P) -> Result<Self> {
        let schema = Self::build_schema();
        let fields = Self::extract_fields(&schema);

        // Create or open index
        let index = if index_path.as_ref().exists() {
            Index::open_in_dir(index_path)?
        } else {
            std::fs::create_dir_all(&index_path)?;
            Index::create_in_dir(index_path, schema.clone())?
        };

        // Create reader with auto-reload
        let reader = index
            .reader_builder()
            .reload_policy(ReloadPolicy::OnCommitWithDelay)
            .try_into()?;

        // Create writer with 50MB buffer
        let writer = index.writer(50_000_000)?;

        Ok(Self {
            index,
            reader,
            writer: Arc::new(RwLock::new(writer)),
            schema,
            fields,
        })
    }

    /// Build the search schema
    fn build_schema() -> Schema {
        let mut schema_builder = Schema::builder();

        // ID field (stored, not indexed)
        schema_builder.add_text_field("id", STORED);

        // Name field (stored, indexed, tokenized with position for phrase queries)
        let text_options = TextOptions::default()
            .set_indexing_options(
                TextFieldIndexing::default()
                    .set_tokenizer("en_stem")
                    .set_index_option(IndexRecordOption::WithFreqsAndPositions),
            )
            .set_stored();
        schema_builder.add_text_field("name", text_options.clone());

        // Description field (indexed, tokenized)
        schema_builder.add_text_field("description", text_options);

        // Version (stored)
        schema_builder.add_text_field("version", STORED);

        // Category (faceted)
        let facet_options = TextOptions::default()
            .set_indexing_options(
                TextFieldIndexing::default()
                    .set_tokenizer("raw")
                    .set_index_option(IndexRecordOption::Basic),
            )
            .set_stored();
        schema_builder.add_text_field("category", facet_options.clone());

        // Language (faceted)
        schema_builder.add_text_field("language", facet_options.clone());

        // License (faceted)
        schema_builder.add_text_field("license", facet_options.clone());

        // Tags (indexed, tokenized)
        let tags_options = TextOptions::default()
            .set_indexing_options(
                TextFieldIndexing::default()
                    .set_tokenizer("raw")
                    .set_index_option(IndexRecordOption::WithFreqs),
            )
            .set_stored();
        schema_builder.add_text_field("tags", tags_options);

        // Downloads (u64, sortable)
        schema_builder.add_u64_field("downloads", FAST | STORED);

        // Rating (f64, sortable)
        schema_builder.add_f64_field("rating", FAST | STORED);

        // Timestamps
        schema_builder.add_date_field("created_at", FAST | STORED);
        schema_builder.add_date_field("updated_at", FAST | STORED);

        // Author
        schema_builder.add_text_field("author", STORED);

        // Repository URL
        schema_builder.add_text_field("repository_url", STORED);

        schema_builder.build()
    }

    /// Extract fields from schema
    fn extract_fields(schema: &Schema) -> SchemaFields {
        SchemaFields {
            id: schema.get_field("id").expect("id field"),
            name: schema.get_field("name").expect("name field"),
            description: schema.get_field("description").expect("description field"),
            version: schema.get_field("version").expect("version field"),
            category: schema.get_field("category").expect("category field"),
            language: schema.get_field("language").expect("language field"),
            license: schema.get_field("license").expect("license field"),
            tags: schema.get_field("tags").expect("tags field"),
            downloads: schema.get_field("downloads").expect("downloads field"),
            rating: schema.get_field("rating").expect("rating field"),
            created_at: schema.get_field("created_at").expect("created_at field"),
            updated_at: schema.get_field("updated_at").expect("updated_at field"),
            author: schema.get_field("author").expect("author field"),
            repository_url: schema.get_field("repository_url").expect("repository_url field"),
        }
    }

    /// Convert package to Tantivy document
    fn package_to_doc(&self, package: &Package) -> TantivyDocument {
        let mut doc = TantivyDocument::default();

        doc.add_text(self.fields.id, &package.id);
        doc.add_text(self.fields.name, &package.name);
        doc.add_text(self.fields.description, &package.description);
        doc.add_text(self.fields.version, &package.version);
        doc.add_text(self.fields.category, &package.category);
        doc.add_text(self.fields.language, &package.language);
        doc.add_text(self.fields.license, &package.license);

        for tag in &package.tags {
            doc.add_text(self.fields.tags, tag);
        }

        doc.add_u64(self.fields.downloads, package.downloads);
        doc.add_f64(self.fields.rating, package.rating as f64);
        doc.add_date(self.fields.created_at, tantivy::DateTime::from_timestamp_secs(package.created_at.timestamp()));
        doc.add_date(self.fields.updated_at, tantivy::DateTime::from_timestamp_secs(package.updated_at.timestamp()));
        doc.add_text(self.fields.author, &package.author);

        if let Some(url) = &package.repository_url {
            doc.add_text(self.fields.repository_url, url);
        }

        doc
    }

    /// Convert Tantivy document to Package
    fn doc_to_package(&self, doc: &TantivyDocument) -> Result<Package> {
        let id = doc
            .get_first(self.fields.id)
            .and_then(|v| v.as_str())
            .context("Missing id")?
            .to_string();

        let name = doc
            .get_first(self.fields.name)
            .and_then(|v| v.as_str())
            .context("Missing name")?
            .to_string();

        let description = doc
            .get_first(self.fields.description)
            .and_then(|v| v.as_str())
            .context("Missing description")?
            .to_string();

        let version = doc
            .get_first(self.fields.version)
            .and_then(|v| v.as_str())
            .context("Missing version")?
            .to_string();

        let category = doc
            .get_first(self.fields.category)
            .and_then(|v| v.as_str())
            .context("Missing category")?
            .to_string();

        let language = doc
            .get_first(self.fields.language)
            .and_then(|v| v.as_str())
            .context("Missing language")?
            .to_string();

        let license = doc
            .get_first(self.fields.license)
            .and_then(|v| v.as_str())
            .context("Missing license")?
            .to_string();

        let tags: Vec<String> = doc
            .get_all(self.fields.tags)
            .filter_map(|v| v.as_str().map(|s| s.to_string()))
            .collect();

        let downloads = doc
            .get_first(self.fields.downloads)
            .and_then(|v| v.as_u64())
            .context("Missing downloads")?;

        let rating = doc
            .get_first(self.fields.rating)
            .and_then(|v| v.as_f64())
            .context("Missing rating")? as f32;

        let created_at = doc
            .get_first(self.fields.created_at)
            .and_then(|v| v.as_datetime())
            .map(|dt| chrono::DateTime::from_timestamp(dt.into_timestamp_secs(), 0).unwrap_or_default())
            .context("Missing created_at")?;

        let updated_at = doc
            .get_first(self.fields.updated_at)
            .and_then(|v| v.as_datetime())
            .map(|dt| chrono::DateTime::from_timestamp(dt.into_timestamp_secs(), 0).unwrap_or_default())
            .context("Missing updated_at")?;

        let author = doc
            .get_first(self.fields.author)
            .and_then(|v| v.as_str())
            .context("Missing author")?
            .to_string();

        let repository_url = doc
            .get_first(self.fields.repository_url)
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        Ok(Package {
            id,
            name,
            description,
            version,
            category,
            language,
            license,
            tags,
            downloads,
            rating,
            created_at,
            updated_at,
            author,
            repository_url,
        })
    }

    /// Build query from SearchQuery
    fn build_query(&self, search_query: &SearchQuery) -> Result<Box<dyn Query>> {
        let searcher = self.reader.searcher();

        // Parse main text query
        let query_parser = QueryParser::for_index(
            &self.index,
            vec![self.fields.name, self.fields.description, self.fields.tags],
        );

        let mut subqueries: Vec<(Occur, Box<dyn Query>)> = Vec::new();

        // Add main query
        if !search_query.query.is_empty() {
            if search_query.fuzzy {
                // Fuzzy matching for typo tolerance
                let terms: Vec<_> = search_query.query.split_whitespace().collect();
                for term in terms {
                    // Try name field with fuzzy matching
                    let name_term = tantivy::Term::from_field_text(self.fields.name, term);
                    let fuzzy_query = FuzzyTermQuery::new(name_term, 2, true);
                    subqueries.push((Occur::Should, Box::new(fuzzy_query)));
                }
            } else {
                let parsed_query = query_parser.parse_query(&search_query.query)?;
                subqueries.push((Occur::Must, parsed_query));
            }
        }

        // Add filter queries
        let filters = &search_query.filters;

        for category in &filters.categories {
            let term = tantivy::Term::from_field_text(self.fields.category, category);
            subqueries.push((Occur::Must, Box::new(TermQuery::new(term, IndexRecordOption::Basic))));
        }

        for language in &filters.languages {
            let term = tantivy::Term::from_field_text(self.fields.language, language);
            subqueries.push((Occur::Must, Box::new(TermQuery::new(term, IndexRecordOption::Basic))));
        }

        for license in &filters.licenses {
            let term = tantivy::Term::from_field_text(self.fields.license, license);
            subqueries.push((Occur::Must, Box::new(TermQuery::new(term, IndexRecordOption::Basic))));
        }

        Ok(Box::new(BooleanQuery::new(subqueries)))
    }
}

#[async_trait]
impl SearchEngine for TantivySearchEngine {
    async fn search(&self, query: &SearchQuery) -> Result<SearchResults> {
        let start = std::time::Instant::now();
        let searcher = self.reader.searcher();

        // Build query
        let tantivy_query = self.build_query(query)?;

        // Calculate total results
        let count_collector = Count;
        let total = searcher.search(&tantivy_query, &count_collector)?;

        // Collect top docs based on sorting
        let limit = query.limit.min(1000); // Cap at 1000 for safety
        let collector = TopDocs::with_limit(limit).and_offset(query.offset);

        let top_docs = searcher.search(&tantivy_query, &collector)?;

        // Convert to scored packages
        let mut packages = Vec::new();
        for (score, doc_address) in top_docs {
            let retrieved_doc = searcher.doc(doc_address)?;
            if let Ok(package) = self.doc_to_package(&retrieved_doc) {
                // Filter by min_score
                if score >= query.min_score {
                    packages.push(ScoredPackage {
                        package,
                        score,
                        highlights: HashMap::new(), // TODO: Implement highlighting
                    });
                }
            }
        }

        // Collect facets
        let mut facets = HashMap::new();

        // Category facets
        let category_counts = self.collect_facet_counts(&searcher, &tantivy_query, self.fields.category)?;
        facets.insert("category".to_string(), category_counts);

        // Language facets
        let language_counts = self.collect_facet_counts(&searcher, &tantivy_query, self.fields.language)?;
        facets.insert("language".to_string(), language_counts);

        // License facets
        let license_counts = self.collect_facet_counts(&searcher, &tantivy_query, self.fields.license)?;
        facets.insert("license".to_string(), license_counts);

        let query_time_ms = start.elapsed().as_millis() as u64;

        Ok(SearchResults {
            packages,
            total,
            facets,
            query_time_ms,
        })
    }

    async fn index(&self, package: &Package) -> Result<()> {
        let doc = self.package_to_doc(package);
        let mut writer = self.writer.write().await;
        writer.add_document(doc)?;
        Ok(())
    }

    async fn bulk_index(&self, packages: Vec<Package>) -> Result<()> {
        let mut writer = self.writer.write().await;
        for package in packages {
            let doc = self.package_to_doc(&package);
            writer.add_document(doc)?;
        }
        Ok(())
    }

    async fn remove(&self, package_id: &str) -> Result<()> {
        let term = tantivy::Term::from_field_text(self.fields.id, package_id);
        let mut writer = self.writer.write().await;
        writer.delete_term(term);
        Ok(())
    }

    async fn update(&self, package: &Package) -> Result<()> {
        // Remove old document and add new one
        self.remove(&package.id).await?;
        self.index(package).await?;
        Ok(())
    }

    async fn commit(&self) -> Result<()> {
        let mut writer = self.writer.write().await;
        writer.commit()?;
        Ok(())
    }

    async fn stats(&self) -> Result<IndexStats> {
        let searcher = self.reader.searcher();
        let segment_metas = searcher.segment_readers();

        let total_documents: usize = segment_metas.iter().map(|s| s.num_docs() as usize).sum();

        // Approximate index size
        let index_size_bytes = 0; // TODO: Calculate actual size

        Ok(IndexStats {
            total_documents,
            index_size_bytes,
            last_updated: chrono::Utc::now(),
        })
    }
}

impl TantivySearchEngine {
    /// Collect facet counts for a field
    fn collect_facet_counts(
        &self,
        searcher: &tantivy::Searcher,
        query: &dyn Query,
        field: Field,
    ) -> Result<Vec<Facet>> {
        // Simple facet collection by grouping documents
        let top_docs = searcher.search(query, &TopDocs::with_limit(10000))?;
        let mut counts: HashMap<String, usize> = HashMap::new();

        for (_score, doc_address) in top_docs {
            let doc: TantivyDocument = searcher.doc(doc_address)?;
            if let Some(value) = doc.get_first(field).and_then(|v| v.as_str()) {
                *counts.entry(value.to_string()).or_insert(0) += 1;
            }
        }

        let mut facets: Vec<CustomFacet> = counts
            .into_iter()
            .map(|(value, count)| CustomFacet { value, count })
            .collect();

        // Sort by count descending using custom sort
        facets.sort_by(|a, b| b.count.cmp(&a.count));

        Ok(facets)
    }
}
