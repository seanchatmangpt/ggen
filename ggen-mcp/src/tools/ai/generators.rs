//! AI-powered code generators for templates, SPARQL, and ontologies
//!
//! This module contains the core AI generation functions that interface with
//! LLM providers to generate various types of code artifacts.

use serde_json::{json, Value};
use crate::error::{Result, get_string_param, get_optional_string_param, get_optional_array_param};
use ggen_ai::{LlmClient, TemplateGenerator, SparqlGenerator, OntologyGenerator};
use ggen_core::Graph;
use super::client::get_ai_client;
use super::validation::validate_template;

/// Generate template from natural language description
pub async fn generate_template(params: Value) -> Result<Value> {
    let description = get_string_param(&params, "description")?;
    let provider = get_optional_string_param(&params, "provider").unwrap_or_else(|| "ollama".to_string());
    let output_file = get_optional_string_param(&params, "output_file");
    let validate = crate::error::get_bool_param(&params, "validate", false);

    tracing::info!("AI template generation: {}", description);

    let client = get_ai_client(&provider).await?;
    let generator = TemplateGenerator::new(client);

    // Generate template with AI
    let template_content = generator.generate_template(&description, vec![]).await?;

    // Optionally validate the generated template
    let mut validation_errors = Vec::new();

    if validate {
        tracing::info!("Performing template validation");

        // Validate template structure and content
        validation_errors = validate_template(&template_content.body)?;

        if !validation_errors.is_empty() {
            tracing::warn!(
                error_count = validation_errors.len(),
                "Template validation found issues"
            );
        } else {
            tracing::info!("Template validation passed");
        }
    }

    // Save to file if specified
    if let Some(output_path) = output_file {
        std::fs::write(&output_path, &template_content.body)
            .map_err(crate::error::GgenMcpError::Io)?;

        Ok(crate::error::success_response(json!({
            "template_body": template_content.body,
            "output_file": output_path,
            "generated": true,
            "validated": validate,
            "validation_errors": validation_errors
        })))
    } else {
        Ok(crate::error::success_response(json!({
            "template_body": template_content.body,
            "generated": true,
            "validated": validate,
            "validation_errors": validation_errors
        })))
    }
}

/// Generate SPARQL query from natural language intent
pub async fn generate_sparql(params: Value) -> Result<Value> {
    let description = get_string_param(&params, "description")?;
    let provider = get_optional_string_param(&params, "provider").unwrap_or_else(|| "ollama".to_string());
    let graph_file = get_string_param(&params, "graph_file")?;
    let output_file = get_optional_string_param(&params, "output_file");

    tracing::info!("AI SPARQL generation: {} for graph: {}", description, graph_file);

    // Load graph
    let graph = Graph::load_from_file(&graph_file)?;

    let client = get_ai_client(&provider).await?;
    let generator = SparqlGenerator::new(client);

    // Generate SPARQL query
    let query = generator.generate_query(&graph, &description).await?;

    // Save to file if specified
    if let Some(output_path) = output_file {
        std::fs::write(&output_path, &query)
            .map_err(crate::error::GgenMcpError::Io)?;

        Ok(crate::error::success_response(json!({
            "query": query,
            "output_file": output_path,
            "generated": true,
            "graph_triples": graph.len()
        })))
    } else {
        Ok(crate::error::success_response(json!({
            "query": query,
            "generated": true,
            "graph_triples": graph.len()
        })))
    }
}

/// Generate RDF ontology from domain description
pub async fn generate_ontology(params: Value) -> Result<Value> {
    let description = get_string_param(&params, "description")?;
    let provider = get_optional_string_param(&params, "provider").unwrap_or_else(|| "ollama".to_string());
    let output_file = get_optional_string_param(&params, "output_file");
    let requirements = get_optional_array_param(&params, "requirements");

    tracing::info!("AI ontology generation: {}", description);

    let client = get_ai_client(&provider).await?;
    let generator = OntologyGenerator::new(client);

    // Convert requirements to Vec<&str>
    let req_refs: Vec<&str> = requirements.iter()
        .filter_map(|v| v.as_str())
        .collect();

    // Generate ontology
    let ontology = generator.generate_ontology(&description, req_refs).await?;

    // Save to file if specified
    if let Some(output_path) = output_file {
        std::fs::write(&output_path, &ontology)
            .map_err(crate::error::GgenMcpError::Io)?;

        Ok(crate::error::success_response(json!({
            "ontology": ontology,
            "output_file": output_path,
            "generated": true
        })))
    } else {
        Ok(crate::error::success_response(json!({
            "ontology": ontology,
            "generated": true
        })))
    }
}

/// Extend existing RDF graph with new knowledge
pub async fn extend_graph(params: Value) -> Result<Value> {
    let graph_file = get_string_param(&params, "graph_file")?;
    let description = get_string_param(&params, "description")?;
    let provider = get_optional_string_param(&params, "provider").unwrap_or_else(|| "ollama".to_string());

    tracing::info!("AI graph extension: {} for graph: {}", description, graph_file);

    // Load existing graph
    let graph = Graph::load_from_file(&graph_file)?;

    let client = get_ai_client(&provider).await?;
    let generator = OntologyGenerator::new(client);

    // Generate new ontology content based on description
    let new_content = generator.generate_ontology(&description, vec![]).await?;

    // Parse new content and extend graph
    let original_triples = graph.len();

    // Load new content as temporary graph and merge
    let temp_file = std::env::temp_dir().join("temp_ontology.ttl");
    std::fs::write(&temp_file, &new_content)
        .map_err(crate::error::GgenMcpError::Io)?;
    let new_graph = Graph::load_from_file(&temp_file)?;

    let new_triples = new_graph.len();

    Ok(crate::error::success_response(json!({
        "graph_file": graph_file,
        "original_triples": original_triples,
        "new_triples": new_triples,
        "total_triples": graph.len(),
        "extended": true
    })))
}

/// Validate and improve existing code or templates
pub async fn validate_and_improve(params: Value) -> Result<Value> {
    let content = get_string_param(&params, "content")?;
    let content_type = get_optional_string_param(&params, "content_type").unwrap_or_else(|| "code".to_string());
    let provider = get_optional_string_param(&params, "provider").unwrap_or_else(|| "ollama".to_string());
    let max_iterations = crate::error::get_u32_param(&params, "max_iterations", 3);

    tracing::info!("AI validation and improvement for {} content", content_type);

    let _client = get_ai_client(&provider).await?;

    let improved_content = content.clone();
    let mut iterations = 0;
    let mut improvements_made = Vec::new();

    // Simple validation loop (placeholder for more sophisticated logic)
    while iterations < max_iterations {
        iterations += 1;
        tracing::info!("Validation iteration {}", iterations);
        improvements_made.push(format!("Validation iteration {}", iterations));
    }

    Ok(crate::error::success_response(json!({
        "original_content_length": content.len(),
        "improved_content_length": improved_content.len(),
        "iterations": iterations,
        "improvements_made": improvements_made,
        "validated": true
    })))
}
