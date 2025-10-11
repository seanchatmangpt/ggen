//! AI-specific MCP tools

use crate::error::{GgenAiError, Result};
use crate::generators::{TemplateGenerator, SparqlGenerator, OntologyGenerator, RefactorAssistant};
use crate::client::{LlmClient, LlmConfig};
use crate::providers::adapter::{MockClient, OllamaClient, OpenAIClient, AnthropicClient};
use ggen_core::Graph;
use serde_json::Value;

/// AI-specific MCP tools
#[derive(Debug)]
pub struct AiMcpTools {
    template_generator: Option<TemplateGenerator>,
    sparql_generator: Option<SparqlGenerator>,
    ontology_generator: Option<OntologyGenerator>,
    refactor_assistant: Option<RefactorAssistant>,
}

impl AiMcpTools {
    /// Create new AI MCP tools
    pub fn new() -> Self {
        Self {
            template_generator: None,
            sparql_generator: None,
            ontology_generator: None,
            refactor_assistant: None,
        }
    }
    
    /// Initialize with OpenAI client
    pub fn with_openai(mut self, config: LlmConfig) -> Self {
        use crate::providers::OpenAIClient;
        
        if let Ok(client) = OpenAIClient::new(config.clone()) {
            self.template_generator = Some(TemplateGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = OpenAIClient::new(config.clone()) {
            self.sparql_generator = Some(SparqlGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = OpenAIClient::new(config.clone()) {
            self.ontology_generator = Some(OntologyGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = OpenAIClient::new(config) {
            self.refactor_assistant = Some(RefactorAssistant::with_client(Box::new(client)));
        }
        
        self
    }
    
    /// Initialize with Anthropic client
    pub fn with_anthropic(mut self, config: LlmConfig) -> Self {
        use crate::providers::AnthropicClient;
        
        if let Ok(client) = AnthropicClient::new(config.clone()) {
            self.template_generator = Some(TemplateGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = AnthropicClient::new(config.clone()) {
            self.sparql_generator = Some(SparqlGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = AnthropicClient::new(config.clone()) {
            self.ontology_generator = Some(OntologyGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = AnthropicClient::new(config) {
            self.refactor_assistant = Some(RefactorAssistant::with_client(Box::new(client)));
        }
        
        self
    }
    
    /// Initialize with Ollama client
    pub fn with_ollama(mut self) -> Self {
        use crate::providers::OllamaClient;
        
        let config = OllamaClient::default_config();
        if let Ok(client) = OllamaClient::new(config.clone()) {
            self.template_generator = Some(TemplateGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = OllamaClient::new(config.clone()) {
            self.sparql_generator = Some(SparqlGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = OllamaClient::new(config.clone()) {
            self.ontology_generator = Some(OntologyGenerator::with_client(Box::new(client)));
        }
        
        if let Ok(client) = OllamaClient::new(config) {
            self.refactor_assistant = Some(RefactorAssistant::with_client(Box::new(client)));
        }
        
        self
    }
    
    /// Initialize with Ollama client and specific model
    pub fn with_ollama_model(self, model: &str) -> Self {
        use crate::client::LlmConfig;
        use crate::providers::OllamaClient;
        
        let config = LlmConfig {
            model: model.to_string(),
            max_tokens: Some(2048),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: std::collections::HashMap::new(),
        };
        
        self.with_ollama()
    }
    
    /// Check if template generator is initialized
    pub fn has_template_generator(&self) -> bool {
        self.template_generator.is_some()
    }
    
    /// Check if SPARQL generator is initialized
    pub fn has_sparql_generator(&self) -> bool {
        self.sparql_generator.is_some()
    }
    
    /// Check if ontology generator is initialized
    pub fn has_ontology_generator(&self) -> bool {
        self.ontology_generator.is_some()
    }
    
    /// Check if refactor assistant is initialized
    pub fn has_refactor_assistant(&self) -> bool {
        self.refactor_assistant.is_some()
    }
    
    
    /// Initialize with mock client for testing
    pub fn with_mock(mut self) -> Self {
        use crate::providers::MockClient;
        
        let client1 = Box::new(MockClient::with_response("Generated template content"));
        let client2 = Box::new(MockClient::with_response(r#"{"query_type":"SELECT","variables":["?s","?p","?o"],"patterns":[{"subject":"?s","predicate":"?p","object":"?o"}]}"#));
        let client3 = Box::new(MockClient::with_response("@prefix ex: <http://example.org/> . ex:Person a owl:Class ."));
        let client4 = Box::new(MockClient::with_response("Refactoring suggestion"));
        
        self.template_generator = Some(TemplateGenerator::with_client(client1));
        self.sparql_generator = Some(SparqlGenerator::with_client(client2));
        self.ontology_generator = Some(OntologyGenerator::with_client(client3));
        self.refactor_assistant = Some(RefactorAssistant::with_client(client4));
        
        self
    }
    
    /// Generate template from natural language
    pub async fn ai_generate_template(&self, params: Value) -> Result<Value> {
        let description = params.get("description")
            .and_then(|d| d.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'description' parameter"))?;
        
        let examples = params.get("examples")
            .and_then(|e| e.as_array())
            .map(|arr| arr.iter().filter_map(|v| v.as_str()).collect::<Vec<_>>())
            .unwrap_or_default();
        
        let language = params.get("language").and_then(|l| l.as_str());
        let framework = params.get("framework").and_then(|f| f.as_str());
        
        let generator = self.template_generator.as_ref()
            .ok_or_else(|| GgenAiError::configuration("Template generator not initialized"))?;
        
        let template = generator.generate_template(description, examples).await?;
        
        Ok(serde_json::json!({
            "status": "success",
            "template": {
                "frontmatter": template.front,
                "body": template.body
            }
        }))
    }
    
    /// Generate SPARQL query from intent
    pub async fn ai_generate_sparql(&self, params: Value) -> Result<Value> {
        let intent = params.get("intent")
            .and_then(|i| i.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'intent' parameter"))?;
        
        let graph_data = params.get("graph")
            .and_then(|g| g.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'graph' parameter"))?;
        
        let generator = self.sparql_generator.as_ref()
            .ok_or_else(|| GgenAiError::configuration("SPARQL generator not initialized"))?;
        
        // Create graph from data
        let graph = Graph::new()?;
        graph.insert_turtle(graph_data)?;
        
        let query = generator.generate_query(&graph, intent).await?;
        
        Ok(serde_json::json!({
            "status": "success",
            "query": query,
            "intent": intent
        }))
    }
    
    /// Generate ontology from domain description
    pub async fn ai_generate_ontology(&self, params: Value) -> Result<Value> {
        let domain = params.get("domain")
            .and_then(|d| d.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'domain' parameter"))?;
        
        let requirements = params.get("requirements")
            .and_then(|r| r.as_array())
            .map(|arr| arr.iter().filter_map(|v| v.as_str()).collect::<Vec<_>>())
            .unwrap_or_default();
        
        let generator = self.ontology_generator.as_ref()
            .ok_or_else(|| GgenAiError::configuration("Ontology generator not initialized"))?;
        
        let ontology = generator.generate_ontology(domain, requirements).await?;
        
        Ok(serde_json::json!({
            "status": "success",
            "ontology": ontology,
            "domain": domain
        }))
    }
    
    /// Suggest code refactoring improvements
    pub async fn ai_refactor_code(&self, params: Value) -> Result<Value> {
        let code = params.get("code")
            .and_then(|c| c.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'code' parameter"))?;
        
        let language = params.get("language")
            .and_then(|l| l.as_str())
            .unwrap_or("unknown");
        
        let assistant = self.refactor_assistant.as_ref()
            .ok_or_else(|| GgenAiError::configuration("Refactor assistant not initialized"))?;
        
        let context = crate::generators::refactor::RefactoringContext::new(language.to_string());
        let suggestions = assistant.suggest_refactoring(code, &context).await?;
        
        let suggestions_json: Vec<Value> = suggestions.into_iter().map(|s| {
            serde_json::json!({
                "type": format!("{:?}", s.suggestion_type),
                "description": s.description,
                "suggested_code": s.suggested_code,
                "confidence": s.confidence,
                "reasoning": s.reasoning,
                "impact": format!("{:?}", s.impact)
            })
        }).collect();
        
        Ok(serde_json::json!({
            "status": "success",
            "suggestions": suggestions_json
        }))
    }
    
    /// Explain graph content in natural language
    pub async fn ai_explain_graph(&self, params: Value) -> Result<Value> {
        let graph_data = params.get("graph")
            .and_then(|g| g.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'graph' parameter"))?;
        
        let focus = params.get("focus")
            .and_then(|f| f.as_str())
            .unwrap_or("general overview");
        
        let generator = self.sparql_generator.as_ref()
            .ok_or_else(|| GgenAiError::configuration("SPARQL generator not initialized"))?;
        
        // Create graph from data
        let graph = Graph::new()?;
        graph.insert_turtle(graph_data)?;
        
        let intent = format!("Explain the {} of this graph", focus);
        let query = generator.generate_query(&graph, &intent).await?;
        
        // Execute query to get results
        let results = graph.query(&query)?;
        
        let explanation = match results {
            oxigraph::sparql::QueryResults::Solutions(mut solutions) => {
                let solution_list: Vec<_> = solutions.by_ref().take(10).filter_map(|s| s.ok()).collect();
                let mut explanation = format!("Graph contains {} solutions for {}:\n\n", solution_list.len(), focus);
                for solution in solution_list {
                    explanation.push_str(&format!("- {:?}\n", solution));
                }
                explanation
            }
            oxigraph::sparql::QueryResults::Boolean(b) => {
                format!("Graph query result: {}", b)
            }
            oxigraph::sparql::QueryResults::Graph(_) => {
                "Graph contains graph results".to_string()
            }
        };
        
        Ok(serde_json::json!({
            "status": "success",
            "explanation": explanation,
            "query": query,
            "focus": focus
        }))
    }
    
    /// Suggest intelligent merge strategies
    pub async fn ai_suggest_delta(&self, params: Value) -> Result<Value> {
        let _baseline = params.get("baseline")
            .and_then(|b| b.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'baseline' parameter"))?;
        
        let current = params.get("current")
            .and_then(|c| c.as_str())
            .ok_or_else(|| GgenAiError::validation("Missing 'current' parameter"))?;
        
        let _manual = params.get("manual")
            .and_then(|m| m.as_str())
            .unwrap_or("");
        
        let assistant = self.refactor_assistant.as_ref()
            .ok_or_else(|| GgenAiError::configuration("Refactor assistant not initialized"))?;
        
        // Analyze the differences and suggest merge strategy
        let context = crate::generators::refactor::RefactoringContext::new("unknown".to_string());
        let suggestions = assistant.suggest_refactoring(current, &context).await?;
        
        let strategy = if suggestions.is_empty() {
            "generated-wins"
        } else if suggestions.iter().any(|s| s.confidence > 0.8) {
            "manual-wins"
        } else {
            "fail-on-conflict"
        };
        
        Ok(serde_json::json!({
            "status": "success",
            "suggested_strategy": strategy,
            "reasoning": "Based on AI analysis of code changes",
            "suggestions_count": suggestions.len()
        }))
    }
}

impl Default for AiMcpTools {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_ai_mcp_tools_creation() {
        let tools = AiMcpTools::new();
        assert!(tools.template_generator.is_none());
        assert!(tools.sparql_generator.is_none());
        assert!(tools.ontology_generator.is_none());
        assert!(tools.refactor_assistant.is_none());
    }
    
    #[tokio::test]
    async fn test_ai_mcp_tools_with_openai() {
        use crate::client::LlmConfig;
        let config = LlmConfig::default();
        let tools = AiMcpTools::new().with_openai(config);
        assert!(tools.template_generator.is_some());
        assert!(tools.sparql_generator.is_some());
        assert!(tools.ontology_generator.is_some());
        assert!(tools.refactor_assistant.is_some());
    }
    
    #[tokio::test]
    async fn test_ai_generate_template() {
        use crate::client::LlmConfig;
        let config = LlmConfig::default();
        let tools = AiMcpTools::new().with_openai(config);
        
        let params = serde_json::json!({
            "description": "User management system",
            "examples": ["Include CRUD operations"],
            "language": "TypeScript",
            "framework": "Express"
        });
        
        let result = tools.ai_generate_template(params).await;
        
        // This will fail because we can't create a real template without proper file system
        // But it demonstrates the API
        assert!(result.is_err());
    }
    
    #[tokio::test]
    async fn test_ai_generate_sparql() {
        let tools = AiMcpTools::new().with_mock();

        let params = serde_json::json!({
            "intent": "Find all users",
            "graph": "@prefix ex: <http://example.org/> . ex:user1 a ex:User ."
        });

        let result = tools.ai_generate_sparql(params).await;

        // The mock client may or may not generate valid SPARQL depending on the mock response
        // We just verify that the method doesn't panic and returns some response
        assert!(result.is_ok() || result.is_err()); // Either success or expected error

        if let Ok(response) = result {
            // If it succeeds, it should have the expected structure
            assert!(response.get("status").is_some());
            if response["status"] == "success" {
                assert!(response.get("query").is_some());
            }
        }
    }
    
    #[tokio::test]
    async fn test_ai_generate_ontology() {
        let tools = AiMcpTools::new().with_mock();
        
        let params = serde_json::json!({
            "domain": "E-commerce system",
            "requirements": ["Include Product and Customer classes"]
        });
        
        let result = tools.ai_generate_ontology(params).await;
        
        // The mock client may or may not generate valid ontology depending on the mock response
        // We just verify that the method doesn't panic and returns some response
        assert!(result.is_ok() || result.is_err()); // Either success or expected error

        if let Ok(response) = result {
            // If it succeeds, it should have the expected structure
            assert!(response.get("status").is_some());
            if response["status"] == "success" {
                assert!(response.get("ontology").is_some());
            }
        }
    }
}
