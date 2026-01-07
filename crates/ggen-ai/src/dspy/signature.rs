//! DSPy Signature - Type-safe specification of module interface

use crate::dspy::field::{InputField, OutputField};
use serde::{Deserialize, Serialize};

/// Signature: Type-safe specification of a module's interface
///
/// Analogous to DSPy's Signature class, but fully typed in Rust.
/// Defines the contract between inputs and outputs for a module.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Signature {
    /// Module name (should be a valid Rust type name)
    pub name: String,

    /// Module description
    pub description: String,

    /// Input fields (user-provided data)
    pub inputs: Vec<InputField>,

    /// Output fields (model-generated data)
    pub outputs: Vec<OutputField>,

    /// Optional instructions for the LLM
    pub instructions: Option<String>,
}

impl Signature {
    /// Create a new signature
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            inputs: Vec::new(),
            outputs: Vec::new(),
            instructions: None,
        }
    }

    /// Add an input field
    pub fn with_input(mut self, input: InputField) -> Self {
        self.inputs.push(input);
        self
    }

    /// Add multiple input fields
    pub fn with_inputs(mut self, inputs: impl IntoIterator<Item = InputField>) -> Self {
        self.inputs.extend(inputs);
        self
    }

    /// Add an output field
    pub fn with_output(mut self, output: OutputField) -> Self {
        self.outputs.push(output);
        self
    }

    /// Add multiple output fields
    pub fn with_outputs(mut self, outputs: impl IntoIterator<Item = OutputField>) -> Self {
        self.outputs.extend(outputs);
        self
    }

    /// Set custom instructions for the LLM
    pub fn with_instructions(mut self, instructions: impl Into<String>) -> Self {
        self.instructions = Some(instructions.into());
        self
    }

    /// Get all input field names
    pub fn input_names(&self) -> Vec<&str> {
        self.inputs.iter().map(|f| f.name()).collect()
    }

    /// Get all output field names
    pub fn output_names(&self) -> Vec<&str> {
        self.outputs.iter().map(|f| f.name()).collect()
    }

    /// Get input field by name
    pub fn get_input(&self, name: &str) -> Option<&InputField> {
        self.inputs.iter().find(|f| f.name() == name)
    }

    /// Get output field by name
    pub fn get_output(&self, name: &str) -> Option<&OutputField> {
        self.outputs.iter().find(|f| f.name() == name)
    }

    /// Generate Rust struct code representing this signature
    pub fn as_rust_struct(&self) -> String {
        let inputs_doc = self.inputs.iter()
            .map(|f| format!("    /// {} ({})", f.desc(), f.type_annotation()))
            .collect::<Vec<_>>()
            .join("\n");

        let inputs_fields = self.inputs.iter()
            .map(|f| format!("    pub {}: {},", f.name(), f.type_annotation()))
            .collect::<Vec<_>>()
            .join("\n");

        let outputs_doc = self.outputs.iter()
            .map(|f| format!("    /// {} ({})", f.desc(), f.type_annotation()))
            .collect::<Vec<_>>()
            .join("\n");

        let outputs_fields = self.outputs.iter()
            .map(|f| format!("    pub {}: {},", f.name(), f.type_annotation()))
            .collect::<Vec<_>>()
            .join("\n");

        let instructions_comment = self.instructions.as_ref()
            .map(|inst| format!("/// Instructions: {}\n", inst))
            .unwrap_or_default();

        format!(
            r#"{}/// {}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {}_Inputs {{
{}
{}
}}

{}/// {}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {}_Outputs {{
{}
{}
}}
"#,
            instructions_comment,
            self.description,
            self.name,
            inputs_doc,
            inputs_fields,
            self.inputs.iter()
                .find_map(|f| f.metadata.prefix.as_ref().map(|p| format!("/// Input prefix: {}\n", p)))
                .unwrap_or_default(),
            "Output from module",
            self.name,
            outputs_doc,
            outputs_fields
        )
    }

    /// Get signature schema as JSON-compatible structure
    pub fn schema(&self) -> SignatureSchema {
        SignatureSchema {
            name: self.name.clone(),
            description: self.description.clone(),
            inputs: self.inputs.iter()
                .map(|f| FieldSchema {
                    name: f.name().to_string(),
                    description: f.desc().to_string(),
                    type_annotation: f.type_annotation().to_string(),
                })
                .collect(),
            outputs: self.outputs.iter()
                .map(|f| FieldSchema {
                    name: f.name().to_string(),
                    description: f.desc().to_string(),
                    type_annotation: f.type_annotation().to_string(),
                })
                .collect(),
        }
    }
}

/// Schema representation for a Signature (JSON-serializable)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SignatureSchema {
    pub name: String,
    pub description: String,
    pub inputs: Vec<FieldSchema>,
    pub outputs: Vec<FieldSchema>,
}

/// Schema for a single field
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldSchema {
    pub name: String,
    pub description: String,
    pub type_annotation: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_signature_creation() {
        let sig = Signature::new("Summarize", "Summarize text")
            .with_input(InputField::new("text", "Text to summarize", "String"))
            .with_output(OutputField::new("summary", "Summarized text", "String"));

        assert_eq!(sig.name, "Summarize");
        assert_eq!(sig.inputs.len(), 1);
        assert_eq!(sig.outputs.len(), 1);
    }

    #[test]
    fn test_field_lookups() {
        let sig = Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"));

        assert!(sig.get_input("question").is_some());
        assert!(sig.get_output("answer").is_some());
        assert!(sig.get_input("nonexistent").is_none());
    }

    #[test]
    fn test_field_names() {
        let sig = Signature::new("Test", "Test signature")
            .with_input(InputField::new("input1", "First input", "String"))
            .with_input(InputField::new("input2", "Second input", "i32"))
            .with_output(OutputField::new("output1", "First output", "String"));

        assert_eq!(sig.input_names(), vec!["input1", "input2"]);
        assert_eq!(sig.output_names(), vec!["output1"]);
    }

    #[test]
    fn test_rust_struct_generation() {
        let sig = Signature::new("Echo", "Echo the input")
            .with_input(InputField::new("text", "Text to echo", "String"))
            .with_output(OutputField::new("echo", "Echoed text", "String"));

        let code = sig.as_rust_struct();
        assert!(code.contains("pub struct Echo_Inputs"));
        assert!(code.contains("pub struct Echo_Outputs"));
        assert!(code.contains("pub text: String"));
        assert!(code.contains("pub echo: String"));
    }
}
