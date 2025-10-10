//! Prompt templates for template generation

use crate::error::{GgenAiError, Result};

/// Builder for template generation prompts
pub struct TemplatePromptBuilder {
    description: String,
    examples: Vec<String>,
    requirements: Vec<String>,
    language: Option<String>,
    framework: Option<String>,
    output_format: Option<String>,
}

impl TemplatePromptBuilder {
    /// Create a new template prompt builder
    pub fn new(description: String) -> Self {
        Self {
            description,
            examples: Vec::new(),
            requirements: Vec::new(),
            language: None,
            framework: None,
            output_format: None,
        }
    }
    
    /// Add examples to the prompt
    pub fn with_examples(mut self, examples: Vec<String>) -> Self {
        self.examples = examples;
        self
    }
    
    /// Add requirements to the prompt
    pub fn with_requirements(mut self, requirements: Vec<String>) -> Self {
        self.requirements = requirements;
        self
    }
    
    /// Set the target language
    pub fn with_language(mut self, language: String) -> Self {
        self.language = Some(language);
        self
    }
    
    /// Set the target framework
    pub fn with_framework(mut self, framework: String) -> Self {
        self.framework = Some(framework);
        self
    }
    
    /// Set the output format
    pub fn with_output_format(mut self, format: String) -> Self {
        self.output_format = Some(format);
        self
    }
    
    /// Build the final prompt
    pub fn build(self) -> Result<String> {
        let mut prompt = String::new();
        
        // System prompt
        prompt.push_str("You are an expert code generator that creates ggen templates. ");
        prompt.push_str("ggen templates use YAML frontmatter followed by Tera template syntax. ");
        prompt.push_str("Generate a complete, valid ggen template based on the requirements.\n\n");
        
        // Requirements section
        prompt.push_str("## Requirements\n");
        prompt.push_str(&self.description);
        prompt.push_str("\n\n");
        
        if !self.requirements.is_empty() {
            prompt.push_str("Additional requirements:\n");
            for req in &self.requirements {
                prompt.push_str(&format!("- {}\n", req));
            }
            prompt.push_str("\n");
        }
        
        // Context section
        if let Some(language) = &self.language {
            prompt.push_str(&format!("Target language: {}\n", language));
        }
        
        if let Some(framework) = &self.framework {
            prompt.push_str(&format!("Target framework: {}\n", framework));
        }
        
        if let Some(format) = &self.output_format {
            prompt.push_str(&format!("Output format: {}\n", format));
        }
        
        prompt.push_str("\n");
        
        // Examples section
        if !self.examples.is_empty() {
            prompt.push_str("## Examples\n");
            for (i, example) in self.examples.iter().enumerate() {
                prompt.push_str(&format!("{}. {}\n", i + 1, example));
            }
            prompt.push_str("\n");
        }
        
        // Template format instructions
        prompt.push_str("## Template Format\n");
        prompt.push_str("Generate a ggen template with the following structure:\n\n");
        prompt.push_str("```yaml\n");
        prompt.push_str("---\n");
        prompt.push_str("to: \"path/to/output/file\"\n");
        prompt.push_str("vars:\n");
        prompt.push_str("  - name: \"variable_name\"\n");
        prompt.push_str("    type: \"string\"\n");
        prompt.push_str("    description: \"Variable description\"\n");
        prompt.push_str("rdf:\n");
        prompt.push_str("  - \"path/to/rdf/file.ttl\"\n");
        prompt.push_str("sparql:\n");
        prompt.push_str("  query_name: \"SELECT ?s ?p ?o WHERE { ?s ?p ?o }\"\n");
        prompt.push_str("---\n");
        prompt.push_str("```\n\n");
        prompt.push_str("{{% for item in sparql.query_name %}}\n");
        prompt.push_str("// Generated code here\n");
        prompt.push_str("{{% endfor %}}\n\n");
        
        // Validation instructions
        prompt.push_str("## Validation\n");
        prompt.push_str("Ensure the template:\n");
        prompt.push_str("- Has valid YAML frontmatter\n");
        prompt.push_str("- Uses proper Tera syntax\n");
        prompt.push_str("- Includes appropriate RDF/SPARQL queries\n");
        prompt.push_str("- Generates valid code for the target language\n");
        prompt.push_str("- Follows best practices for the target framework\n\n");
        
        // Output instructions
        prompt.push_str("Generate the complete ggen template now:\n\n");
        
        Ok(prompt)
    }
}

/// Pre-built prompt templates for common use cases
pub struct TemplatePrompts;

impl TemplatePrompts {
    /// Generate a REST API controller template
    pub fn rest_api_controller(description: &str, language: &str, framework: &str) -> Result<String> {
        TemplatePromptBuilder::new(description.to_string())
            .with_language(language.to_string())
            .with_framework(framework.to_string())
            .with_requirements(vec![
                "Include CRUD operations".to_string(),
                "Add input validation".to_string(),
                "Include error handling".to_string(),
                "Add documentation comments".to_string(),
            ])
            .with_examples(vec![
                "GET /api/users - List all users".to_string(),
                "POST /api/users - Create new user".to_string(),
                "PUT /api/users/:id - Update user".to_string(),
                "DELETE /api/users/:id - Delete user".to_string(),
            ])
            .build()
    }
    
    /// Generate a data model template
    pub fn data_model(description: &str, language: &str) -> Result<String> {
        TemplatePromptBuilder::new(description.to_string())
            .with_language(language.to_string())
            .with_requirements(vec![
                "Include all necessary fields".to_string(),
                "Add type definitions".to_string(),
                "Include validation rules".to_string(),
                "Add serialization support".to_string(),
            ])
            .with_examples(vec![
                "User model with id, name, email fields".to_string(),
                "Product model with id, name, price, category fields".to_string(),
            ])
            .build()
    }
    
    /// Generate a configuration file template
    pub fn config_file(description: &str, format: &str) -> Result<String> {
        TemplatePromptBuilder::new(description.to_string())
            .with_output_format(format.to_string())
            .with_requirements(vec![
                "Include all necessary settings".to_string(),
                "Add default values".to_string(),
                "Include validation rules".to_string(),
                "Add documentation comments".to_string(),
            ])
            .with_examples(vec![
                "Database connection settings".to_string(),
                "API endpoint configurations".to_string(),
                "Logging and monitoring settings".to_string(),
            ])
            .build()
    }
    
    /// Generate a test file template
    pub fn test_file(description: &str, language: &str, framework: &str) -> Result<String> {
        TemplatePromptBuilder::new(description.to_string())
            .with_language(language.to_string())
            .with_framework(framework.to_string())
            .with_requirements(vec![
                "Include unit tests".to_string(),
                "Add integration tests".to_string(),
                "Include test fixtures".to_string(),
                "Add test utilities".to_string(),
            ])
            .with_examples(vec![
                "Test CRUD operations".to_string(),
                "Test validation logic".to_string(),
                "Test error handling".to_string(),
            ])
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_template_prompt_builder() {
        let prompt = TemplatePromptBuilder::new("Generate a user management system".to_string())
            .with_language("TypeScript".to_string())
            .with_framework("Express".to_string())
            .with_requirements(vec!["Include authentication".to_string()])
            .with_examples(vec!["User registration".to_string()])
            .build()
            .unwrap();
        
        assert!(prompt.contains("user management system"));
        assert!(prompt.contains("TypeScript"));
        assert!(prompt.contains("Express"));
        assert!(prompt.contains("authentication"));
        assert!(prompt.contains("User registration"));
    }
    
    #[test]
    fn test_rest_api_controller_prompt() {
        let prompt = TemplatePrompts::rest_api_controller(
            "User management API",
            "TypeScript",
            "Express"
        ).unwrap();
        
        assert!(prompt.contains("User management API"));
        assert!(prompt.contains("TypeScript"));
        assert!(prompt.contains("Express"));
        assert!(prompt.contains("CRUD operations"));
    }
    
    #[test]
    fn test_data_model_prompt() {
        let prompt = TemplatePrompts::data_model("User entity", "Rust").unwrap();
        
        assert!(prompt.contains("User entity"));
        assert!(prompt.contains("Rust"));
        assert!(prompt.contains("type definitions"));
    }
}
