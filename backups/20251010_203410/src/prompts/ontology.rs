//! Ontology generation prompts

use serde::{Deserialize, Serialize};
use crate::error::Result;

/// Builder for ontology generation prompts
#[derive(Debug, Clone)]
pub struct OntologyPromptBuilder {
    domain: String,
    requirements: Vec<String>,
    examples: Vec<String>,
    format: Option<String>,
}

impl OntologyPromptBuilder {
    /// Create a new ontology prompt builder
    pub fn new(domain: String) -> Self {
        Self {
            domain,
            requirements: Vec::new(),
            examples: Vec::new(),
            format: None,
        }
    }

    /// Set the domain description
    pub fn with_domain(mut self, domain: String) -> Self {
        self.domain = domain;
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

    /// Add a single requirement
    pub fn add_requirement(mut self, requirement: String) -> Self {
        self.requirements.push(requirement);
        self
    }

    /// Add a single example
    pub fn add_example(mut self, example: String) -> Self {
        self.examples.push(example);
        self
    }

    /// Set the output format
    pub fn with_format(mut self, format: Option<String>) -> Self {
        self.format = format;
        self
    }

    /// Build the prompt
    pub fn build(self) -> Result<String> {
        let mut prompt = String::new();
        
        prompt.push_str("Generate an RDF/OWL ontology for the following domain:\n\n");
        prompt.push_str(&format!("Domain: {}\n\n", self.domain));
        
        if !self.requirements.is_empty() {
            prompt.push_str("Requirements:\n");
            for req in &self.requirements {
                prompt.push_str(&format!("- {}\n", req));
            }
            prompt.push('\n');
        }
        
        if !self.examples.is_empty() {
            prompt.push_str("Examples:\n");
            for example in &self.examples {
                prompt.push_str(&format!("- {}\n", example));
            }
            prompt.push('\n');
        }
        
        if let Some(format) = self.format {
            prompt.push_str(&format!("Output format: {}\n\n", format));
        }
        
        prompt.push_str("Please generate a complete RDF/OWL ontology in Turtle format with appropriate classes, properties, and relationships.");
        
        Ok(prompt)
    }
}

/// Predefined ontology prompts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyPrompts {
    pub domain: String,
    pub requirements: Vec<String>,
    pub examples: Vec<String>,
    pub format: String,
}

impl OntologyPrompts {
    /// Create a new ontology prompts struct
    pub fn new(domain: String) -> Self {
        Self {
            domain,
            requirements: Vec::new(),
            examples: Vec::new(),
            format: "turtle".to_string(),
        }
    }

    /// Add a requirement
    pub fn add_requirement(mut self, requirement: String) -> Self {
        self.requirements.push(requirement);
        self
    }

    /// Add an example
    pub fn add_example(mut self, example: String) -> Self {
        self.examples.push(example);
        self
    }

    /// Set the format
    pub fn with_format(mut self, format: String) -> Self {
        self.format = format;
        self
    }
}