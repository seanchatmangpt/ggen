//! Prompt templates for code generation

use crate::error::Result;

/// Builder for code generation prompts
pub struct CodePromptBuilder {
    description: String,
    language: String,
    framework: Option<String>,
    requirements: Vec<String>,
    examples: Vec<String>,
    patterns: Vec<String>,
    output_format: Option<String>,
}

impl CodePromptBuilder {
    /// Create a new code prompt builder
    pub fn new(description: String, language: String) -> Self {
        Self {
            description,
            language,
            framework: None,
            requirements: Vec::new(),
            examples: Vec::new(),
            patterns: Vec::new(),
            output_format: None,
        }
    }
    
    /// Set the framework
    pub fn with_framework(mut self, framework: String) -> Self {
        self.framework = Some(framework);
        self
    }
    
    /// Add requirements
    pub fn with_requirements(mut self, requirements: Vec<String>) -> Self {
        self.requirements = requirements;
        self
    }
    
    /// Add examples
    pub fn with_examples(mut self, examples: Vec<String>) -> Self {
        self.examples = examples;
        self
    }
    
    /// Add patterns
    pub fn with_patterns(mut self, patterns: Vec<String>) -> Self {
        self.patterns = patterns;
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
        prompt.push_str("You are an expert code generator. ");
        prompt.push_str("Generate clean, efficient, and well-documented code that follows best practices. ");
        prompt.push_str("Ensure the code is production-ready and includes proper error handling.\n\n");
        
        // Description section
        prompt.push_str("## Code Description\n");
        prompt.push_str(&self.description);
        prompt.push_str("\n\n");
        
        // Language and framework section
        prompt.push_str("## Target Environment\n");
        prompt.push_str(&format!("Language: {}\n", self.language));
        
        if let Some(framework) = &self.framework {
            prompt.push_str(&format!("Framework: {}\n", framework));
        }
        
        if let Some(format) = &self.output_format {
            prompt.push_str(&format!("Output format: {}\n", format));
        }
        
        prompt.push_str("\n");
        
        // Requirements section
        if !self.requirements.is_empty() {
            prompt.push_str("## Requirements\n");
            for req in &self.requirements {
                prompt.push_str(&format!("- {}\n", req));
            }
            prompt.push_str("\n");
        }
        
        // Examples section
        if !self.examples.is_empty() {
            prompt.push_str("## Examples\n");
            for (i, example) in self.examples.iter().enumerate() {
                prompt.push_str(&format!("{}. {}\n", i + 1, example));
            }
            prompt.push_str("\n");
        }
        
        // Patterns section
        if !self.patterns.is_empty() {
            prompt.push_str("## Code Patterns\n");
            for pattern in &self.patterns {
                prompt.push_str(&format!("- {}\n", pattern));
            }
            prompt.push_str("\n");
        }
        
        // Language-specific instructions
        self.add_language_specific_instructions(&mut prompt)?;
        
        // Best practices
        prompt.push_str("## Best Practices\n");
        prompt.push_str("1. Use meaningful variable and function names\n");
        prompt.push_str("2. Add comprehensive documentation\n");
        prompt.push_str("3. Include proper error handling\n");
        prompt.push_str("4. Follow language-specific conventions\n");
        prompt.push_str("5. Add type annotations where applicable\n");
        prompt.push_str("6. Include input validation\n");
        prompt.push_str("7. Use appropriate design patterns\n");
        prompt.push_str("8. Ensure code is testable\n\n");
        
        // Output instructions
        prompt.push_str("Generate the complete code implementation now:\n\n");
        
        Ok(prompt)
    }
    
    fn add_language_specific_instructions(&self, prompt: &mut String) -> Result<()> {
        match self.language.to_lowercase().as_str() {
            "rust" => {
                prompt.push_str("## Rust-Specific Guidelines\n");
                prompt.push_str("1. Use `Result<T, E>` for error handling\n");
                prompt.push_str("2. Prefer `&str` over `String` for parameters\n");
                prompt.push_str("3. Use `Option<T>` for nullable values\n");
                prompt.push_str("4. Implement `Display` and `Debug` traits\n");
                prompt.push_str("5. Use `serde` for serialization\n");
                prompt.push_str("6. Follow Rust naming conventions\n");
                prompt.push_str("7. Use `clippy` recommendations\n");
                prompt.push_str("8. Add `#[derive(...)]` attributes as needed\n\n");
            }
            "typescript" => {
                prompt.push_str("## TypeScript-Specific Guidelines\n");
                prompt.push_str("1. Use strict type checking\n");
                prompt.push_str("2. Define interfaces for complex objects\n");
                prompt.push_str("3. Use `async/await` for promises\n");
                prompt.push_str("4. Add JSDoc comments\n");
                prompt.push_str("5. Use `const` and `let` appropriately\n");
                prompt.push_str("6. Follow ESLint rules\n");
                prompt.push_str("7. Use proper module exports\n");
                prompt.push_str("8. Handle errors with try-catch\n\n");
            }
            "python" => {
                prompt.push_str("## Python-Specific Guidelines\n");
                prompt.push_str("1. Follow PEP 8 style guide\n");
                prompt.push_str("2. Use type hints\n");
                prompt.push_str("3. Add docstrings for functions and classes\n");
                prompt.push_str("4. Use `pathlib` for file operations\n");
                prompt.push_str("5. Handle exceptions appropriately\n");
                prompt.push_str("6. Use `dataclasses` for data structures\n");
                prompt.push_str("7. Follow naming conventions\n");
                prompt.push_str("8. Use `logging` instead of `print`\n\n");
            }
            "go" => {
                prompt.push_str("## Go-Specific Guidelines\n");
                prompt.push_str("1. Follow Go naming conventions\n");
                prompt.push_str("2. Use `error` interface for error handling\n");
                prompt.push_str("3. Add godoc comments\n");
                prompt.push_str("4. Use `context.Context` for cancellation\n");
                prompt.push_str("5. Prefer composition over inheritance\n");
                prompt.push_str("6. Use `gofmt` for formatting\n");
                prompt.push_str("7. Handle errors explicitly\n");
                prompt.push_str("8. Use interfaces for abstraction\n\n");
            }
            "java" => {
                prompt.push_str("## Java-Specific Guidelines\n");
                prompt.push_str("1. Follow Java naming conventions\n");
                prompt.push_str("2. Use `Optional<T>` for nullable values\n");
                prompt.push_str("3. Add JavaDoc comments\n");
                prompt.push_str("4. Use `Stream` API for collections\n");
                prompt.push_str("5. Handle exceptions properly\n");
                prompt.push_str("6. Use `final` for immutable variables\n");
                prompt.push_str("7. Follow SOLID principles\n");
                prompt.push_str("8. Use appropriate design patterns\n\n");
            }
            _ => {
                prompt.push_str("## General Guidelines\n");
                prompt.push_str("1. Follow language-specific conventions\n");
                prompt.push_str("2. Use appropriate data structures\n");
                prompt.push_str("3. Add meaningful comments\n");
                prompt.push_str("4. Handle errors gracefully\n");
                prompt.push_str("5. Write testable code\n");
                prompt.push_str("6. Use consistent formatting\n");
                prompt.push_str("7. Optimize for readability\n");
                prompt.push_str("8. Follow security best practices\n\n");
            }
        }
        
        Ok(())
    }
}

/// Pre-built prompt templates for common code generation use cases
pub struct CodePrompts;

impl CodePrompts {
    /// Generate a REST API controller
    pub fn rest_controller(description: &str, language: &str, framework: &str) -> Result<String> {
        CodePromptBuilder::new(description.to_string(), language.to_string())
            .with_framework(framework.to_string())
            .with_requirements(vec![
                "Implement CRUD operations".to_string(),
                "Add input validation".to_string(),
                "Include error handling".to_string(),
                "Add logging".to_string(),
                "Include authentication".to_string(),
            ])
            .with_examples(vec![
                "GET /api/users - List all users".to_string(),
                "POST /api/users - Create new user".to_string(),
                "PUT /api/users/:id - Update user".to_string(),
                "DELETE /api/users/:id - Delete user".to_string(),
            ])
            .with_patterns(vec![
                "Use dependency injection".to_string(),
                "Implement repository pattern".to_string(),
                "Add middleware for common functionality".to_string(),
            ])
            .build()
    }
    
    /// Generate a data model
    pub fn data_model(description: &str, language: &str) -> Result<String> {
        CodePromptBuilder::new(description.to_string(), language.to_string())
            .with_requirements(vec![
                "Include all necessary fields".to_string(),
                "Add validation rules".to_string(),
                "Include serialization support".to_string(),
                "Add builder pattern".to_string(),
            ])
            .with_examples(vec![
                "User model with id, name, email".to_string(),
                "Product model with id, name, price, category".to_string(),
            ])
            .with_patterns(vec![
                "Use immutable objects".to_string(),
                "Implement equals and hashCode".to_string(),
                "Add toString method".to_string(),
            ])
            .build()
    }
    
    /// Generate a service class
    pub fn service_class(description: &str, language: &str) -> Result<String> {
        CodePromptBuilder::new(description.to_string(), language.to_string())
            .with_requirements(vec![
                "Implement business logic".to_string(),
                "Add transaction support".to_string(),
                "Include error handling".to_string(),
                "Add logging".to_string(),
            ])
            .with_examples(vec![
                "UserService with CRUD operations".to_string(),
                "EmailService with sending capabilities".to_string(),
            ])
            .with_patterns(vec![
                "Use dependency injection".to_string(),
                "Implement interface segregation".to_string(),
                "Add caching where appropriate".to_string(),
            ])
            .build()
    }
    
    /// Generate a test file
    pub fn test_file(description: &str, language: &str, framework: &str) -> Result<String> {
        CodePromptBuilder::new(description.to_string(), language.to_string())
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
            .with_patterns(vec![
                "Use AAA pattern (Arrange, Act, Assert)".to_string(),
                "Mock external dependencies".to_string(),
                "Test edge cases".to_string(),
            ])
            .build()
    }
    
    /// Generate a configuration class
    pub fn config_class(description: &str, language: &str) -> Result<String> {
        CodePromptBuilder::new(description.to_string(), language.to_string())
            .with_requirements(vec![
                "Load from environment variables".to_string(),
                "Add default values".to_string(),
                "Include validation".to_string(),
                "Support hot reloading".to_string(),
            ])
            .with_examples(vec![
                "Database configuration".to_string(),
                "API endpoint configuration".to_string(),
                "Logging configuration".to_string(),
            ])
            .with_patterns(vec![
                "Use builder pattern".to_string(),
                "Implement singleton pattern".to_string(),
                "Add configuration validation".to_string(),
            ])
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_code_prompt_builder() {
        let prompt = CodePromptBuilder::new("User management system".to_string(), "Rust".to_string())
            .with_framework("Actix".to_string())
            .with_requirements(vec!["Include authentication".to_string()])
            .with_examples(vec!["User registration".to_string()])
            .build()
            .expect("Failed to build code prompt");

        assert!(prompt.contains("User management system"));
        assert!(prompt.contains("Rust"));
        assert!(prompt.contains("Actix"));
        assert!(prompt.contains("authentication"));
        assert!(prompt.contains("User registration"));
    }
    
    #[test]
    fn test_rest_controller_prompt() {
        let prompt = CodePrompts::rest_controller(
            "User API",
            "TypeScript",
            "Express"
        ).expect("Failed to create REST controller prompt");

        assert!(prompt.contains("User API"));
        assert!(prompt.contains("TypeScript"));
        assert!(prompt.contains("Express"));
        assert!(prompt.contains("CRUD operations"));
    }
    
    #[test]
    fn test_data_model_prompt() {
        let prompt = CodePrompts::data_model("User entity", "Rust")
            .expect("Failed to create data model prompt");

        assert!(prompt.contains("User entity"));
        assert!(prompt.contains("Rust"));
        assert!(prompt.contains("validation rules"));
    }
}

