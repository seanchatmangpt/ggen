//! Helper methods for TemplateMetadataStore
//!
//! This module provides helper methods for querying and managing template metadata
//! stored in RDF format. It extends the `TemplateMetadataStore` with convenience
//! methods for querying full metadata, relationships, and variables.
//!
//! ## Features
//!
//! - **Full Metadata Queries**: Query complete template metadata in a single operation
//! - **Relationship Discovery**: Find related templates and dependencies
//! - **Variable Extraction**: Extract template variables from RDF metadata
//! - **SPARQL Integration**: Uses SPARQL queries to extract structured metadata
//!
//! ## Examples
//!
//! ### Querying Full Metadata
//!
//! ```text
//! use mcpp_core::rdf::TemplateMetadataStore;
//!
//! # fn main() -> mcpp_utils::error::Result<()> {
//! let store = TemplateMetadataStore::new()?;
//! let metadata = store.query_full_metadata("http://example.org/template1")?;
//!
//! println!("Template: {}", metadata.name);
//! println!("Version: {:?}", metadata.version);
//! # Ok(())
//! # }
//! ```

use super::template_metadata::{TemplateMetadata, TemplateMetadataStore};
use mcpp_utils::error::Result;

impl TemplateMetadataStore {
    /// Query full metadata by executing multiple SPARQL queries
    pub(super) fn query_full_metadata(&self, template_id: &str) -> Result<TemplateMetadata> {
        // Query basic fields
        let query = format!(
            r#"
            PREFIX mcpp: <http://mcpp.dev/ontology#>
            SELECT ?name ?version ?description ?author ?category ?stability ?coverage ?usage
            WHERE {{
                <{template_id}> a mcpp:Template ;
                    mcpp:templateName ?name .
                OPTIONAL {{ <{template_id}> mcpp:templateVersion ?version }}
                OPTIONAL {{ <{template_id}> mcpp:templateDescription ?description }}
                OPTIONAL {{ <{template_id}> mcpp:templateAuthor ?author }}
                OPTIONAL {{ <{template_id}> mcpp:category ?category }}
                OPTIONAL {{ <{template_id}> mcpp:stability ?stability }}
                OPTIONAL {{ <{template_id}> mcpp:testCoverage ?coverage }}
                OPTIONAL {{ <{template_id}> mcpp:usageCount ?usage }}
            }}
            "#,
            template_id = template_id
        );

        let results = self.query(&query)?;
        let mut metadata = TemplateMetadata::new(template_id.to_string(), String::new());

        if let Some(row) = results.first() {
            metadata.name = row
                .get("name")
                .map(|s| s.trim_matches('"').to_string())
                .unwrap_or_default();
            metadata.version = row.get("version").map(|s| s.trim_matches('"').to_string());
            metadata.description = row
                .get("description")
                .map(|s| s.trim_matches('"').to_string());
            metadata.author = row.get("author").map(|s| s.trim_matches('"').to_string());
            metadata.category = row.get("category").map(|s| s.trim_matches('"').to_string());
            metadata.stability = row
                .get("stability")
                .map(|s| s.trim_matches('"').to_string());

            if let Some(coverage_str) = row.get("coverage") {
                metadata.test_coverage = coverage_str.trim_matches('"').parse().ok();
            }
            if let Some(usage_str) = row.get("usage") {
                metadata.usage_count = usage_str.trim_matches('"').parse().ok();
            }
        }

        // Query tags
        let tags_query = format!(
            r#"
            PREFIX mcpp: <http://mcpp.dev/ontology#>
            SELECT ?tag
            WHERE {{
                <{template_id}> mcpp:tag ?tag .
            }}
            "#,
            template_id = template_id
        );

        let tags_results = self.query(&tags_query)?;
        metadata.tags = tags_results
            .iter()
            .filter_map(|row| row.get("tag").map(|s| s.trim_matches('"').to_string()))
            .collect();

        Ok(metadata)
    }
}
