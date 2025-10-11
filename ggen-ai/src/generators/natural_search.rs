//! AI-powered natural language package search

use crate::client::LlmClient;
use crate::error::Result;
use crate::error_utils::{no_valid_content_error, parse_failure_error, ErrorContext};
use crate::prompts::PromptTemplateLoader;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;

/// AI-powered natural language search result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NaturalSearchResult {
    pub query: String,
    pub interpretation: String,
    pub search_params: HashMap<String, Value>,
    pub results: Vec<PackageResult>,
    pub suggestions: Vec<String>,
    pub confidence: f32,
}

/// Package search result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageResult {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: Option<String>,
    pub tags: Vec<String>,
    pub relevance_score: f32,
    pub ai_reasoning: Option<String>,
}

/// Natural language package search generator
#[derive(Debug)]
pub struct NaturalSearchGenerator {
    client: Arc<dyn LlmClient>,
    template_loader: PromptTemplateLoader,
}

impl NaturalSearchGenerator {
    /// Create a new natural search generator
    pub fn new(client: Arc<dyn LlmClient>) -> Result<Self> {
        let template_loader = PromptTemplateLoader::new()?;
        Ok(Self {
            client,
            template_loader,
        })
    }

    /// Interpret natural language query and generate search results
    pub async fn search(&mut self, query: &str) -> Result<NaturalSearchResult> {
        // Build prompt using template loader (dogfooding!)
        let prompt = self.template_loader.render_natural_search(query)?;

        // Call LLM to interpret query
        let response = self.client.complete(&prompt).await?;

        // Parse LLM response into structured data
        self.parse_search_response(query, &response.content)
    }

    /// Parse LLM response into structured search result
    fn parse_search_response(&self, query: &str, response: &str) -> Result<NaturalSearchResult> {
        // Extract JSON from response (may be in code blocks)
        let json_str = self.extract_json(response)?;

        // Parse JSON
        let parsed: Value = serde_json::from_str(&json_str).map_err(|e| {
            return parse_failure_error::<Value>(
                "LLM response as JSON",
                &e.to_string(),
                &json_str,
                ErrorContext::Validation,
            )
            .unwrap_err();
        })?;

        // Extract fields with fallbacks
        let interpretation = parsed
            .get("interpretation")
            .and_then(|v| v.as_str())
            .unwrap_or("Analyzing query for relevant packages")
            .to_string();

        let confidence = parsed
            .get("confidence")
            .and_then(|v| v.as_f64())
            .unwrap_or(0.7) as f32;

        // Extract keywords and build search params
        let mut search_params = HashMap::new();
        if let Some(keywords) = parsed.get("keywords").and_then(|v| v.as_array()) {
            search_params.insert(
                "keywords".to_string(),
                Value::Array(keywords.clone()),
            );
        }
        if let Some(category) = parsed.get("category").and_then(|v| v.as_str()) {
            search_params.insert("category".to_string(), Value::String(category.to_string()));
        }
        search_params.insert("search_terms".to_string(), Value::String(query.to_string()));
        search_params.insert("sort".to_string(), Value::String("relevance".to_string()));

        // Parse package results
        let results = if let Some(packages) = parsed.get("packages").and_then(|v| v.as_array()) {
            packages
                .iter()
                .filter_map(|p| self.parse_package_result(p))
                .collect()
        } else {
            // Fallback: generate default result if LLM didn't provide packages
            vec![PackageResult {
                id: "io.ggen.template.basic".to_string(),
                name: "Basic Project Template".to_string(),
                description: "Starter template based on your query".to_string(),
                category: Some("template".to_string()),
                tags: vec!["template".to_string(), "starter".to_string()],
                relevance_score: 0.6,
                ai_reasoning: Some("General-purpose template that might match your needs".to_string()),
            }]
        };

        // Extract suggestions
        let suggestions = parsed
            .get("suggestions")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_else(|| vec![
                "Try: 'authentication system'".to_string(),
                "Try: 'web API framework'".to_string(),
                "Try: 'CLI application'".to_string(),
            ]);

        Ok(NaturalSearchResult {
            query: query.to_string(),
            interpretation,
            search_params,
            results,
            suggestions,
            confidence,
        })
    }

    /// Extract JSON from potentially markdown-formatted response
    fn extract_json(&self, response: &str) -> Result<String> {
        // Try to find JSON in code blocks using parsing_utils
        if let Some(json) = crate::parsing_utils::extract_code_block(response, "json") {
            return Ok(json);
        }

        // Try to find any code block with JSON content
        if let Some(content) = crate::parsing_utils::extract_any_code_block(response) {
            if content.trim().starts_with('{') {
                return Ok(content);
            }
        }

        // Try to find JSON object directly
        if let Some(start) = response.find('{') {
            if let Some(end) = response.rfind('}') {
                if end > start {
                    return Ok(response[start..=end].to_string());
                }
            }
        }

        no_valid_content_error("JSON object", response, ErrorContext::Validation)
    }

    /// Parse a single package result from JSON
    fn parse_package_result(&self, package: &Value) -> Option<PackageResult> {
        Some(PackageResult {
            id: package.get("id")?.as_str()?.to_string(),
            name: package.get("name")?.as_str()?.to_string(),
            description: package.get("description")?.as_str()?.to_string(),
            category: package.get("category").and_then(|v| v.as_str()).map(|s| s.to_string()),
            tags: package
                .get("tags")
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|v| v.as_str().map(|s| s.to_string()))
                        .collect()
                })
                .unwrap_or_default(),
            relevance_score: package
                .get("relevance_score")
                .and_then(|v| v.as_f64())
                .unwrap_or(0.5) as f32,
            ai_reasoning: package
                .get("ai_reasoning")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string()),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::{
        create_natural_search_generator_with_response, create_natural_search_test_generator,
    };

    #[tokio::test]
    async fn test_natural_search_generation() {
        let mock_response = r#"```json
{
  "interpretation": "User is looking for authentication packages",
  "keywords": ["authentication", "auth", "user"],
  "category": "security",
  "confidence": 0.9,
  "packages": [
    {
      "id": "io.ggen.auth.user",
      "name": "User Authentication System",
      "description": "Complete user authentication with JWT",
      "category": "security",
      "tags": ["authentication", "jwt", "user"],
      "relevance_score": 0.95,
      "ai_reasoning": "Perfect match for authentication needs"
    }
  ],
  "suggestions": ["Try: 'OAuth2 authentication'", "Try: 'JWT tokens'"]
}
```"#;

        let mut generator =
            create_natural_search_generator_with_response(mock_response).unwrap();

        let result = generator
            .search("I need user authentication")
            .await
            .unwrap();

        assert_eq!(result.query, "I need user authentication");
        assert!(result.interpretation.contains("authentication"));
        assert!(!result.results.is_empty());
        assert!(result.confidence > 0.0);
    }

    #[tokio::test]
    async fn test_json_extraction() {
        let generator = create_natural_search_test_generator();

        // Test with code block
        let json_str = generator
            .extract_json("```json\n{\"test\": true}\n```")
            .unwrap();
        assert!(json_str.contains("test"));

        // Test with direct JSON
        let json_str = generator.extract_json("{\"test\": true}").unwrap();
        assert!(json_str.contains("test"));
    }

    #[tokio::test]
    async fn test_json_extraction_edge_cases() {
        let generator = create_natural_search_test_generator();

        // Test with unmarked code block (should still extract JSON object)
        let json_str = generator
            .extract_json("```\n{\"test\": true}\n```")
            .unwrap();
        assert!(json_str.contains("test"));

        // Test with JSON embedded in text
        let json_str = generator
            .extract_json("Here is the result: {\"test\": true} and more text")
            .unwrap();
        assert!(json_str.contains("test"));

        // Test with nested JSON
        let json_str = generator
            .extract_json("{\"outer\": {\"inner\": \"value\"}}")
            .unwrap();
        assert!(json_str.contains("outer"));
        assert!(json_str.contains("inner"));

        // Test with multiline JSON
        let multiline = r#"{
            "interpretation": "test",
            "confidence": 0.8
        }"#;
        let json_str = generator.extract_json(multiline).unwrap();
        assert!(json_str.contains("interpretation"));
        assert!(json_str.contains("confidence"));
    }

    #[tokio::test]
    async fn test_json_extraction_failures() {
        let generator = create_natural_search_test_generator();

        // Test with no JSON (should fail)
        let result = generator.extract_json("No JSON here");
        assert!(result.is_err());

        // Test with incomplete JSON (should still extract)
        let result = generator.extract_json("Some text before {\"incomplete\": ");
        assert!(result.is_err());

        // Test with empty string
        let result = generator.extract_json("");
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_search_with_minimal_response() {
        let mock_response = r#"{
            "interpretation": "Basic search"
        }"#;

        let mut generator =
            create_natural_search_generator_with_response(mock_response).unwrap();

        // Should handle minimal response with fallback values
        let result = generator.search("test query").await.unwrap();

        assert_eq!(result.query, "test query");
        assert_eq!(result.interpretation, "Basic search");
        assert!(result.confidence > 0.0); // Should have default confidence
        assert!(!result.suggestions.is_empty()); // Should have fallback suggestions
    }

    #[tokio::test]
    async fn test_search_with_invalid_json() {
        let mock_response = "Not valid JSON at all!";

        let mut generator =
            create_natural_search_generator_with_response(mock_response).unwrap();

        // Should return error for completely invalid response
        let result = generator.search("test query").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_package_result_parsing() {
        let generator = create_natural_search_test_generator();

        // Valid package
        let valid_package = serde_json::json!({
            "id": "test.package",
            "name": "Test Package",
            "description": "A test package",
            "category": "testing",
            "tags": ["test", "demo"],
            "relevance_score": 0.95,
            "ai_reasoning": "Perfect match"
        });

        let result = generator.parse_package_result(&valid_package);
        assert!(result.is_some());
        let pkg = result.unwrap();
        assert_eq!(pkg.id, "test.package");
        assert_eq!(pkg.relevance_score, 0.95);
        assert_eq!(pkg.tags.len(), 2);

        // Package with missing optional fields
        let minimal_package = serde_json::json!({
            "id": "minimal.package",
            "name": "Minimal Package",
            "description": "Minimal test package"
        });

        let result = generator.parse_package_result(&minimal_package);
        assert!(result.is_some());
        let pkg = result.unwrap();
        assert_eq!(pkg.id, "minimal.package");
        assert!(pkg.category.is_none());
        assert!(pkg.tags.is_empty());
        assert_eq!(pkg.relevance_score, 0.5); // Default value

        // Invalid package (missing required fields)
        let invalid_package = serde_json::json!({
            "name": "No ID Package"
        });

        let result = generator.parse_package_result(&invalid_package);
        assert!(result.is_none());
    }
}
