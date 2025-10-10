//! Prompt templates for SPARQL query generation

use crate::error::Result;

/// Builder for SPARQL query generation prompts
pub struct SparqlPromptBuilder {
    intent: String,
    graph_schema: Option<String>,
    prefixes: Vec<(String, String)>,
    examples: Vec<String>,
    constraints: Vec<String>,
    output_format: Option<String>,
}

impl SparqlPromptBuilder {
    /// Create a new SPARQL prompt builder
    pub fn new(intent: String) -> Self {
        Self {
            intent,
            graph_schema: None,
            prefixes: Vec::new(),
            examples: Vec::new(),
            constraints: Vec::new(),
            output_format: None,
        }
    }
    
    /// Add graph schema information
    pub fn with_schema(mut self, schema: String) -> Self {
        self.graph_schema = Some(schema);
        self
    }
    
    /// Add prefixes
    pub fn with_prefixes(mut self, prefixes: Vec<(String, String)>) -> Self {
        self.prefixes = prefixes;
        self
    }
    
    /// Add examples
    pub fn with_examples(mut self, examples: Vec<String>) -> Self {
        self.examples = examples;
        self
    }
    
    /// Add constraints
    pub fn with_constraints(mut self, constraints: Vec<String>) -> Self {
        self.constraints = constraints;
        self
    }
    
    /// Set output format
    pub fn with_output_format(mut self, format: String) -> Self {
        self.output_format = Some(format);
        self
    }
    
    /// Build the final prompt
    pub fn build(self) -> Result<String> {
        let mut prompt = String::new();
        
        // System prompt
        prompt.push_str("You are an expert SPARQL query generator. ");
        prompt.push_str("Generate accurate, efficient SPARQL queries based on natural language intent. ");
        prompt.push_str("Ensure queries are syntactically correct and follow SPARQL best practices.\n\n");
        
        // Intent section
        prompt.push_str("## Query Intent\n");
        prompt.push_str(&self.intent);
        prompt.push_str("\n\n");
        
        // Schema section
        if let Some(schema) = &self.graph_schema {
            prompt.push_str("## Graph Schema\n");
            prompt.push_str(schema);
            prompt.push_str("\n\n");
        }
        
        // Prefixes section
        if !self.prefixes.is_empty() {
            prompt.push_str("## Available Prefixes\n");
            for (prefix, uri) in &self.prefixes {
                prompt.push_str(&format!("PREFIX {}: <{}>\n", prefix, uri));
            }
            prompt.push_str("\n");
        }
        
        // Constraints section
        if !self.constraints.is_empty() {
            prompt.push_str("## Constraints\n");
            for constraint in &self.constraints {
                prompt.push_str(&format!("- {}\n", constraint));
            }
            prompt.push_str("\n");
        }
        
        // Examples section
        if !self.examples.is_empty() {
            prompt.push_str("## Example Queries\n");
            for (i, example) in self.examples.iter().enumerate() {
                prompt.push_str(&format!("{}. {}\n", i + 1, example));
            }
            prompt.push_str("\n");
        }
        
        // Output format instructions
        if let Some(format) = &self.output_format {
            prompt.push_str(&format!("## Output Format\n{}\n\n", format));
        }
        
        // Query generation instructions
        prompt.push_str("## Query Generation Rules\n");
        prompt.push_str("1. Use appropriate SPARQL syntax\n");
        prompt.push_str("2. Include necessary PREFIX declarations\n");
        prompt.push_str("3. Use efficient query patterns\n");
        prompt.push_str("4. Add appropriate FILTER clauses when needed\n");
        prompt.push_str("5. Use proper variable naming conventions\n");
        prompt.push_str("6. Include LIMIT clauses for large result sets\n");
        prompt.push_str("7. Use OPTIONAL for optional data\n");
        prompt.push_str("8. Use UNION for alternative patterns\n\n");
        
        // Common patterns
        prompt.push_str("## Common SPARQL Patterns\n");
        prompt.push_str("```sparql\n");
        prompt.push_str("# Basic SELECT query\n");
        prompt.push_str("SELECT ?s ?p ?o WHERE {\n");
        prompt.push_str("  ?s ?p ?o .\n");
        prompt.push_str("}\n\n");
        prompt.push_str("# Query with FILTER\n");
        prompt.push_str("SELECT ?s ?p ?o WHERE {\n");
        prompt.push_str("  ?s ?p ?o .\n");
        prompt.push_str("  FILTER(?o > 100)\n");
        prompt.push_str("}\n\n");
        prompt.push_str("# Query with OPTIONAL\n");
        prompt.push_str("SELECT ?s ?p ?o ?optional WHERE {\n");
        prompt.push_str("  ?s ?p ?o .\n");
        prompt.push_str("  OPTIONAL { ?s ex:optional ?optional }\n");
        prompt.push_str("}\n\n");
        prompt.push_str("# Query with UNION\n");
        prompt.push_str("SELECT ?s ?p ?o WHERE {\n");
        prompt.push_str("  { ?s ex:type1 ?o }\n");
        prompt.push_str("  UNION\n");
        prompt.push_str("  { ?s ex:type2 ?o }\n");
        prompt.push_str("}\n");
        prompt.push_str("```\n\n");
        
        // Output instructions
        prompt.push_str("Generate the SPARQL query now:\n\n");
        
        Ok(prompt)
    }
}

/// Pre-built prompt templates for common SPARQL use cases
pub struct SparqlPrompts;

impl SparqlPrompts {
    /// Generate a query to find all instances of a class
    pub fn find_instances(class_uri: &str, prefixes: Vec<(String, String)>) -> Result<String> {
        SparqlPromptBuilder::new(format!("Find all instances of the class {}", class_uri))
            .with_prefixes(prefixes)
            .with_constraints(vec![
                "Return subject and object values".to_string(),
                "Include rdf:type information".to_string(),
            ])
            .with_examples(vec![
                "SELECT ?instance WHERE { ?instance a ex:Person }".to_string(),
                "SELECT ?instance ?type WHERE { ?instance a ?type }".to_string(),
            ])
            .build()
    }
    
    /// Generate a query to find properties of a resource
    pub fn find_properties(resource_uri: &str, prefixes: Vec<(String, String)>) -> Result<String> {
        SparqlPromptBuilder::new(format!("Find all properties of the resource {}", resource_uri))
            .with_prefixes(prefixes)
            .with_constraints(vec![
                "Return predicate and object values".to_string(),
                "Include property types".to_string(),
            ])
            .with_examples(vec![
                "SELECT ?property ?value WHERE { ex:resource ?property ?value }".to_string(),
                "SELECT ?property ?value ?type WHERE { ex:resource ?property ?value . ?property a ?type }".to_string(),
            ])
            .build()
    }
    
    /// Generate a query to find relationships between resources
    pub fn find_relationships(subject_uri: &str, object_uri: &str, prefixes: Vec<(String, String)>) -> Result<String> {
        SparqlPromptBuilder::new(format!("Find relationships between {} and {}", subject_uri, object_uri))
            .with_prefixes(prefixes)
            .with_constraints(vec![
                "Return all possible relationship paths".to_string(),
                "Include intermediate resources".to_string(),
            ])
            .with_examples(vec![
                "SELECT ?path WHERE { ex:subject (ex:relation)* ex:object }".to_string(),
                "SELECT ?intermediate WHERE { ex:subject ex:relation ?intermediate . ?intermediate ex:relation ex:object }".to_string(),
            ])
            .build()
    }
    
    /// Generate a query to find resources by property value
    pub fn find_by_property_value(property_uri: &str, value: &str, prefixes: Vec<(String, String)>) -> Result<String> {
        SparqlPromptBuilder::new(format!("Find resources where {} equals '{}'", property_uri, value))
            .with_prefixes(prefixes)
            .with_constraints(vec![
                "Return subject resources".to_string(),
                "Handle both literal and URI values".to_string(),
            ])
            .with_examples(vec![
                "SELECT ?resource WHERE { ?resource ex:property \"value\" }".to_string(),
                "SELECT ?resource WHERE { ?resource ex:property ?value . FILTER(?value = \"value\") }".to_string(),
            ])
            .build()
    }
    
    /// Generate a query to find resources with missing properties
    pub fn find_missing_properties(class_uri: &str, property_uri: &str, prefixes: Vec<(String, String)>) -> Result<String> {
        SparqlPromptBuilder::new(format!("Find instances of {} that don't have {}", class_uri, property_uri))
            .with_prefixes(prefixes)
            .with_constraints(vec![
                "Use NOT EXISTS or MINUS".to_string(),
                "Return subject resources".to_string(),
            ])
            .with_examples(vec![
                "SELECT ?resource WHERE { ?resource a ex:Class . NOT EXISTS { ?resource ex:property ?value } }".to_string(),
                "SELECT ?resource WHERE { ?resource a ex:Class . MINUS { ?resource ex:property ?value } }".to_string(),
            ])
            .build()
    }
    
    /// Generate a query to find the most connected resources
    pub fn find_most_connected(prefixes: Vec<(String, String)>) -> Result<String> {
        SparqlPromptBuilder::new("Find resources with the most connections".to_string())
            .with_prefixes(prefixes)
            .with_constraints(vec![
                "Count outgoing and incoming connections".to_string(),
                "Order by connection count".to_string(),
                "Include LIMIT clause".to_string(),
            ])
            .with_examples(vec![
                "SELECT ?resource (COUNT(?connection) AS ?count) WHERE { ?resource ?p ?connection } GROUP BY ?resource ORDER BY DESC(?count) LIMIT 10".to_string(),
            ])
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_sparql_prompt_builder() {
        let prompt = SparqlPromptBuilder::new("Find all users".to_string())
            .with_prefixes(vec![("ex".to_string(), "http://example.org/".to_string())])
            .with_constraints(vec!["Return user names".to_string()])
            .with_examples(vec!["SELECT ?user WHERE { ?user a ex:User }".to_string()])
            .build()
            .expect("Failed to build SPARQL prompt");

        assert!(prompt.contains("Find all users"));
        assert!(prompt.contains("ex: <http://example.org/>"));
        assert!(prompt.contains("Return user names"));
        assert!(prompt.contains("SELECT ?user WHERE"));
    }
    
    #[test]
    fn test_find_instances_prompt() {
        let prompt = SparqlPrompts::find_instances(
            "ex:Person",
            vec![("ex".to_string(), "http://example.org/".to_string())]
        ).expect("Failed to create find instances prompt");

        assert!(prompt.contains("ex:Person"));
        assert!(prompt.contains("instances"));
    }
    
    #[test]
    fn test_find_properties_prompt() {
        let prompt = SparqlPrompts::find_properties(
            "ex:resource",
            vec![("ex".to_string(), "http://example.org/".to_string())]
        ).expect("Failed to create find properties prompt");

        assert!(prompt.contains("ex:resource"));
        assert!(prompt.contains("properties"));
    }
}
