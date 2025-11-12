// Helper methods for TemplateMetadataStore
use super::template_metadata::{TemplateMetadata, TemplateMetadataStore};
use anyhow::Result;

impl TemplateMetadataStore {
    /// Query full metadata by executing multiple SPARQL queries
    pub(super) fn query_full_metadata(&self, template_id: &str) -> Result<TemplateMetadata> {
        // Query basic fields
        let query = format!(
            r#"
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT ?name ?version ?description ?author ?category ?stability ?coverage ?usage
            WHERE {{
                <{template_id}> a ggen:Template ;
                    ggen:templateName ?name .
                OPTIONAL {{ <{template_id}> ggen:templateVersion ?version }}
                OPTIONAL {{ <{template_id}> ggen:templateDescription ?description }}
                OPTIONAL {{ <{template_id}> ggen:templateAuthor ?author }}
                OPTIONAL {{ <{template_id}> ggen:category ?category }}
                OPTIONAL {{ <{template_id}> ggen:stability ?stability }}
                OPTIONAL {{ <{template_id}> ggen:testCoverage ?coverage }}
                OPTIONAL {{ <{template_id}> ggen:usageCount ?usage }}
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
            PREFIX ggen: <http://ggen.dev/ontology#>
            SELECT ?tag
            WHERE {{
                <{template_id}> ggen:tag ?tag .
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
