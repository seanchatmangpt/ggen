//! Ontology generation prompts

use crate::error::Result;
use serde::{Deserialize, Serialize};

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

        // Add explicit Turtle syntax requirements and example
        prompt.push_str("CRITICAL: You must generate valid Turtle syntax. Every triple must have subject-predicate-object.\n\n");
        prompt.push_str("Example of valid Turtle ontology:\n");
        prompt.push_str("```turtle\n");
        prompt.push_str("@prefix : <http://example.org/domain#> .\n");
        prompt.push_str("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
        prompt.push_str("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n");
        prompt.push_str("@prefix owl: <http://www.w3.org/2002/07/owl#> .\n");
        prompt.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
        prompt.push_str("\n");
        prompt.push_str("# Class declarations\n");
        prompt.push_str(":Product a rdfs:Class ;\n");
        prompt.push_str("    rdfs:label \"Product\" ;\n");
        prompt.push_str("    rdfs:comment \"A product in the domain\" .\n");
        prompt.push_str("\n");
        prompt.push_str(":Order a rdfs:Class ;\n");
        prompt.push_str("    rdfs:label \"Order\" ;\n");
        prompt.push_str("    rdfs:comment \"An order\" .\n");
        prompt.push_str("\n");
        prompt.push_str("# Property declarations\n");
        prompt.push_str(":hasName a rdf:Property ;\n");
        prompt.push_str("    rdfs:domain :Product ;\n");
        prompt.push_str("    rdfs:range rdfs:Literal .\n");
        prompt.push_str("```\n\n");
        prompt.push_str("IMPORTANT RULES:\n");
        prompt.push_str("1. Every triple must have subject-predicate-object (e.g., ':Product a rdfs:Class .')\n");
        prompt.push_str(
            "2. Class declarations use 'a rdfs:Class' (e.g., ':Product a rdfs:Class .')\n",
        );
        prompt.push_str(
            "3. Property declarations use 'a rdf:Property' (e.g., ':hasName a rdf:Property .')\n",
        );
        prompt.push_str("4. All triples must end with a period (.)\n");
        prompt.push_str("5. Use semicolons (;) to continue triples with the same subject\n");
        prompt.push_str(
            "6. CRITICAL: Declare ALL prefixes you will use with @prefix BEFORE using them\n",
        );
        prompt.push_str("   - If you use rdf:, declare @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n");
        prompt.push_str("   - If you use rdfs:, declare @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n");
        prompt.push_str(
            "   - If you use owl:, declare @prefix owl: <http://www.w3.org/2002/07/owl#> .\n",
        );
        prompt.push_str(
            "   - If you use xsd:, declare @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n",
        );
        prompt.push_str("   - NEVER use a prefix without declaring it first\n");
        prompt.push_str("7. NEVER write just a subject followed by a period (e.g., ':Product .' is INVALID)\n\n");
        prompt.push_str("Generate ONLY valid Turtle syntax. Output the complete ontology wrapped in ```turtle code blocks.");

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
