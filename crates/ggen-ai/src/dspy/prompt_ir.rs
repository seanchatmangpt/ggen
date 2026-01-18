//! Prompt Intermediate Representation (Prompt-IR)
//!
//! This module makes prompts first-class data structures rather than strings.
//! The key insight: you have Fields, Constraints, Prefixes, Types, and Semantics,
//! but you're still collapsing all of that into a string.
//!
//! This abstraction makes rendering a backend concern.
//!
//! The PromptAtom enum captures the semantic units of a prompt,
//! enabling programmatic manipulation before rendering.

use crate::dspy::field::{FieldConstraints, FieldMetadata};
use crate::dspy::constraint::ConstraintSet;
use crate::dspy::signature::Signature;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::fmt;

/// Atomic unit of prompt structure
///
/// These are the semantic building blocks that compose into prompts.
/// Each variant captures a distinct concern that contributes to the final prompt.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PromptAtom {
    /// System-level context or instructions
    SystemContext(String),

    /// Task description
    TaskDescription(String),

    /// Custom instructions
    Instructions(String),

    /// An input field with its value
    Input {
        metadata: FieldMetadata,
        value: Value,
        constraints: Option<FieldConstraints>,
    },

    /// An output field specification (what we want the model to produce)
    Output {
        metadata: FieldMetadata,
        constraints: Option<FieldConstraints>,
    },

    /// Constraint specification (can be attached to input or output)
    Constraint {
        field_name: String,
        constraint_set: ConstraintSet,
    },

    /// Few-shot example (input-output pair)
    Example {
        inputs: Vec<(String, Value)>,
        outputs: Vec<(String, Value)>,
    },

    /// Raw text (for backwards compatibility or escape hatch)
    Raw(String),

    /// Separator between sections
    Separator,

    /// Format specification for output
    OutputFormat(OutputFormat),
}

/// Output format specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum OutputFormat {
    /// Plain text, field: value format
    Text,

    /// JSON object
    Json,

    /// JSON with schema
    JsonSchema(String),

    /// YAML
    Yaml,

    /// Markdown with field headers
    Markdown,

    /// Custom format with template
    Custom(String),
}

impl Default for OutputFormat {
    fn default() -> Self {
        OutputFormat::Text
    }
}

/// Prompt Intermediate Representation
///
/// A structured representation of a prompt that can be:
/// - Composed programmatically
/// - Inspected and modified
/// - Rendered to different formats for different models
/// - Optimized before rendering
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PromptIR {
    /// Sequence of prompt atoms in order
    atoms: Vec<PromptAtom>,

    /// Preferred output format
    output_format: OutputFormat,

    /// Metadata about the prompt
    metadata: PromptMetadata,
}

/// Metadata about the prompt IR
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PromptMetadata {
    /// Signature name this prompt is for
    pub signature_name: Option<String>,

    /// Estimated token count (if computed)
    pub estimated_tokens: Option<usize>,

    /// Whether constraints are embedded in the prompt
    pub constraints_embedded: bool,

    /// Whether examples are included
    pub has_examples: bool,
}

impl PromptIR {
    /// Create a new empty prompt IR
    pub fn new() -> Self {
        Self::default()
    }

    /// Build from a Signature and input values
    pub fn from_signature(signature: &Signature, inputs: &[(String, Value)]) -> Self {
        let mut ir = Self::new();

        ir.metadata.signature_name = Some(signature.name.clone());

        // Add task description
        ir.atoms.push(PromptAtom::TaskDescription(signature.description.clone()));

        // Add instructions if present
        if let Some(ref instructions) = signature.instructions {
            ir.atoms.push(PromptAtom::Instructions(instructions.clone()));
        }

        ir.atoms.push(PromptAtom::Separator);

        // Add input fields with values
        for input_field in &signature.inputs {
            if let Some((_, value)) = inputs.iter().find(|(name, _)| name == input_field.name()) {
                ir.atoms.push(PromptAtom::Input {
                    metadata: input_field.metadata.clone(),
                    value: value.clone(),
                    constraints: if input_field.constraints.has_constraints() {
                        Some(input_field.constraints.clone())
                    } else {
                        None
                    },
                });
            }
        }

        ir.atoms.push(PromptAtom::Separator);

        // Add output field specifications
        for output_field in &signature.outputs {
            ir.atoms.push(PromptAtom::Output {
                metadata: output_field.metadata.clone(),
                constraints: if output_field.constraints.has_constraints() {
                    Some(output_field.constraints.clone())
                } else {
                    None
                },
            });

            // If output has constraints, add explicit constraint atom
            if output_field.constraints.has_constraints() {
                ir.metadata.constraints_embedded = true;
                ir.atoms.push(PromptAtom::Constraint {
                    field_name: output_field.name().to_string(),
                    constraint_set: ConstraintSet::from_field_constraints(&output_field.constraints),
                });
            }
        }

        ir
    }

    /// Add an atom to the prompt
    pub fn push(&mut self, atom: PromptAtom) {
        self.atoms.push(atom);
    }

    /// Add a system context
    pub fn with_system_context(mut self, context: impl Into<String>) -> Self {
        self.atoms.insert(0, PromptAtom::SystemContext(context.into()));
        self
    }

    /// Add an example
    pub fn with_example(mut self, inputs: Vec<(String, Value)>, outputs: Vec<(String, Value)>) -> Self {
        self.metadata.has_examples = true;
        self.atoms.push(PromptAtom::Example { inputs, outputs });
        self
    }

    /// Set output format
    pub fn with_output_format(mut self, format: OutputFormat) -> Self {
        self.output_format = format.clone();
        self.atoms.push(PromptAtom::OutputFormat(format));
        self
    }

    /// Get all atoms
    pub fn atoms(&self) -> &[PromptAtom] {
        &self.atoms
    }

    /// Get mutable access to atoms
    pub fn atoms_mut(&mut self) -> &mut Vec<PromptAtom> {
        &mut self.atoms
    }

    /// Get output format
    pub fn output_format(&self) -> &OutputFormat {
        &self.output_format
    }

    /// Get metadata
    pub fn metadata(&self) -> &PromptMetadata {
        &self.metadata
    }

    /// Count the number of atoms
    pub fn len(&self) -> usize {
        self.atoms.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.atoms.is_empty()
    }

    /// Filter atoms by type
    pub fn inputs(&self) -> Vec<&PromptAtom> {
        self.atoms.iter().filter(|a| matches!(a, PromptAtom::Input { .. })).collect()
    }

    /// Filter to get output specifications
    pub fn outputs(&self) -> Vec<&PromptAtom> {
        self.atoms.iter().filter(|a| matches!(a, PromptAtom::Output { .. })).collect()
    }

    /// Filter to get constraints
    pub fn constraints(&self) -> Vec<&PromptAtom> {
        self.atoms.iter().filter(|a| matches!(a, PromptAtom::Constraint { .. })).collect()
    }
}

// ============================================================================
// Rendering Backends
// ============================================================================

/// Render configuration
#[derive(Debug, Clone, Default)]
pub struct RenderConfig {
    /// Include constraint descriptions in prompt
    pub include_constraints: bool,

    /// Include type annotations
    pub include_types: bool,

    /// Use prefixes for fields
    pub use_prefixes: bool,

    /// Max length for constraint descriptions (0 = unlimited)
    pub max_constraint_desc_len: usize,
}

impl RenderConfig {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_constraints(mut self) -> Self {
        self.include_constraints = true;
        self
    }

    pub fn with_types(mut self) -> Self {
        self.include_types = true;
        self
    }

    pub fn with_prefixes(mut self) -> Self {
        self.use_prefixes = true;
        self
    }
}

/// Trait for prompt rendering backends
pub trait PromptRenderer {
    /// Render the IR to a string
    fn render(&self, ir: &PromptIR, config: &RenderConfig) -> String;
}

/// Default text renderer
pub struct TextRenderer;

impl PromptRenderer for TextRenderer {
    fn render(&self, ir: &PromptIR, config: &RenderConfig) -> String {
        let mut output = String::new();

        for atom in &ir.atoms {
            match atom {
                PromptAtom::SystemContext(ctx) => {
                    output.push_str(ctx);
                    output.push_str("\n\n");
                }

                PromptAtom::TaskDescription(desc) => {
                    output.push_str(desc);
                    output.push_str("\n\n");
                }

                PromptAtom::Instructions(inst) => {
                    output.push_str("Instructions: ");
                    output.push_str(inst);
                    output.push_str("\n\n");
                }

                PromptAtom::Input { metadata, value, constraints } => {
                    let prefix = if config.use_prefixes {
                        metadata.prefix.as_deref().unwrap_or("")
                    } else {
                        ""
                    };

                    let type_suffix = if config.include_types {
                        format!(" ({})", metadata.type_annotation)
                    } else {
                        String::new()
                    };

                    output.push_str(&format!(
                        "{}{}{}: {}\n",
                        prefix,
                        metadata.name,
                        type_suffix,
                        format_value(value)
                    ));

                    if config.include_constraints {
                        if let Some(c) = constraints {
                            let desc = describe_constraints(c, config.max_constraint_desc_len);
                            if !desc.is_empty() {
                                output.push_str(&format!("  [constraints: {}]\n", desc));
                            }
                        }
                    }
                }

                PromptAtom::Output { metadata, constraints } => {
                    let type_suffix = if config.include_types {
                        format!(" ({})", metadata.type_annotation)
                    } else {
                        String::new()
                    };

                    output.push_str(&format!("{}{}: ", metadata.name, type_suffix));

                    if config.include_constraints {
                        if let Some(c) = constraints {
                            let desc = describe_constraints(c, config.max_constraint_desc_len);
                            if !desc.is_empty() {
                                output.push_str(&format!("[{}] ", desc));
                            }
                        }
                    }

                    output.push('\n');
                }

                PromptAtom::Constraint { field_name, constraint_set } => {
                    if config.include_constraints && !constraint_set.is_empty() {
                        output.push_str(&format!(
                            "Constraints for {}: {}\n",
                            field_name,
                            describe_constraint_set(constraint_set)
                        ));
                    }
                }

                PromptAtom::Example { inputs, outputs } => {
                    output.push_str("\n--- Example ---\n");
                    output.push_str("Input:\n");
                    for (name, value) in inputs {
                        output.push_str(&format!("  {}: {}\n", name, format_value(value)));
                    }
                    output.push_str("Output:\n");
                    for (name, value) in outputs {
                        output.push_str(&format!("  {}: {}\n", name, format_value(value)));
                    }
                    output.push_str("--- End Example ---\n\n");
                }

                PromptAtom::Raw(text) => {
                    output.push_str(text);
                }

                PromptAtom::Separator => {
                    output.push('\n');
                }

                PromptAtom::OutputFormat(format) => {
                    match format {
                        OutputFormat::Json => {
                            output.push_str("\nRespond with valid JSON.\n");
                        }
                        OutputFormat::JsonSchema(schema) => {
                            output.push_str(&format!("\nRespond with JSON matching schema:\n{}\n", schema));
                        }
                        OutputFormat::Yaml => {
                            output.push_str("\nRespond in YAML format.\n");
                        }
                        OutputFormat::Markdown => {
                            output.push_str("\nFormat your response in markdown.\n");
                        }
                        OutputFormat::Custom(template) => {
                            output.push_str(&format!("\nFormat: {}\n", template));
                        }
                        OutputFormat::Text => {
                            // Default, no special instruction
                        }
                    }
                }
            }
        }

        output
    }
}

/// JSON-focused renderer (for models with native JSON support)
pub struct JsonRenderer;

impl PromptRenderer for JsonRenderer {
    fn render(&self, ir: &PromptIR, _config: &RenderConfig) -> String {
        let mut output = String::new();

        // Extract task description
        for atom in &ir.atoms {
            if let PromptAtom::TaskDescription(desc) = atom {
                output.push_str(desc);
                output.push_str("\n\n");
                break;
            }
        }

        // Instructions
        for atom in &ir.atoms {
            if let PromptAtom::Instructions(inst) = atom {
                output.push_str(inst);
                output.push_str("\n\n");
                break;
            }
        }

        // Input as JSON
        output.push_str("Input:\n");
        let mut input_obj = serde_json::Map::new();
        for atom in &ir.atoms {
            if let PromptAtom::Input { metadata, value, .. } = atom {
                input_obj.insert(metadata.name.clone(), value.clone());
            }
        }
        output.push_str(&serde_json::to_string_pretty(&Value::Object(input_obj)).unwrap_or_default());
        output.push_str("\n\n");

        // Output specification
        output.push_str("Respond with a JSON object containing:\n");
        for atom in &ir.atoms {
            if let PromptAtom::Output { metadata, .. } = atom {
                output.push_str(&format!("- \"{}\": {} ({})\n", metadata.name, metadata.desc, metadata.type_annotation));
            }
        }

        output
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Format a JSON value for prompt rendering
fn format_value(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Array(arr) => serde_json::to_string(arr).unwrap_or_default(),
        Value::Object(obj) => serde_json::to_string(obj).unwrap_or_default(),
        Value::Null => "null".to_string(),
    }
}

/// Describe constraints in human-readable form
fn describe_constraints(constraints: &FieldConstraints, max_len: usize) -> String {
    let mut parts = Vec::new();

    if constraints.required {
        parts.push("required".to_string());
    }

    if let Some(min) = constraints.min_length {
        parts.push(format!("min {} chars", min));
    }

    if let Some(max) = constraints.max_length {
        parts.push(format!("max {} chars", max));
    }

    if let Some(min) = constraints.min_items {
        parts.push(format!("min {} items", min));
    }

    if let Some(max) = constraints.max_items {
        parts.push(format!("max {} items", max));
    }

    if let Some(ref values) = constraints.enum_values {
        let values_str = values.join(", ");
        parts.push(format!("one of: {}", values_str));
    }

    if let Some(ref pattern) = constraints.pattern {
        parts.push(format!("pattern: {}", pattern));
    }

    let result = parts.join(", ");

    if max_len > 0 && result.len() > max_len {
        format!("{}...", &result[..max_len])
    } else {
        result
    }
}

/// Describe a ConstraintSet
fn describe_constraint_set(set: &ConstraintSet) -> String {
    let mut parts: Vec<String> = set.all.iter().map(describe_single_constraint).collect();

    for group in &set.any {
        let group_desc: Vec<String> = group.iter().map(describe_single_constraint).collect();
        if !group_desc.is_empty() {
            parts.push(format!("one of [{}]", group_desc.join(" OR ")));
        }
    }

    parts.join(", ")
}

/// Describe a single constraint
fn describe_single_constraint(c: &crate::dspy::constraint::Constraint) -> String {
    use crate::dspy::constraint::Constraint;
    match c {
        Constraint::Required => "required".to_string(),
        Constraint::MinLength(n) => format!("min {} chars", n),
        Constraint::MaxLength(n) => format!("max {} chars", n),
        Constraint::Pattern(p) => format!("matches /{}/", p),
        Constraint::OneOf(v) => format!("one of [{}]", v.join(", ")),
        Constraint::MinItems(n) => format!("min {} items", n),
        Constraint::MaxItems(n) => format!("max {} items", n),
        Constraint::TypeIs(t) => format!("type: {}", t.name()),
        Constraint::MinValue(v) => format!(">= {}", v),
        Constraint::MaxValue(v) => format!("<= {}", v),
        Constraint::SemanticType(s) => format!("semantic: {}", s),
        Constraint::Datatype(d) => format!("datatype: {}", d),
        Constraint::Custom(name) => format!("custom: {}", name),
    }
}

impl fmt::Display for PromptIR {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let renderer = TextRenderer;
        let config = RenderConfig::new().with_prefixes();
        write!(f, "{}", renderer.render(self, &config))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::{InputField, OutputField};
    use serde_json::json;

    #[test]
    fn test_prompt_ir_creation() {
        let mut ir = PromptIR::new();
        ir.push(PromptAtom::TaskDescription("Answer questions".to_string()));
        ir.push(PromptAtom::Input {
            metadata: FieldMetadata::new("question", "The question", "String"),
            value: json!("What is Rust?"),
            constraints: None,
        });

        assert_eq!(ir.len(), 2);
        assert!(!ir.is_empty());
    }

    #[test]
    fn test_prompt_ir_from_signature() {
        let sig = Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"));

        let inputs = vec![("question".to_string(), json!("What is Rust?"))];
        let ir = PromptIR::from_signature(&sig, &inputs);

        assert!(!ir.is_empty());
        assert_eq!(ir.inputs().len(), 1);
        assert_eq!(ir.outputs().len(), 1);
    }

    #[test]
    fn test_text_renderer() {
        let mut ir = PromptIR::new();
        ir.push(PromptAtom::TaskDescription("Test task".to_string()));
        ir.push(PromptAtom::Input {
            metadata: FieldMetadata::new("input", "Test input", "String"),
            value: json!("test value"),
            constraints: None,
        });
        ir.push(PromptAtom::Output {
            metadata: FieldMetadata::new("output", "Test output", "String"),
            constraints: None,
        });

        let renderer = TextRenderer;
        let config = RenderConfig::new();
        let output = renderer.render(&ir, &config);

        assert!(output.contains("Test task"));
        assert!(output.contains("input:"));
        assert!(output.contains("output:"));
    }

    #[test]
    fn test_renderer_with_constraints() {
        let mut ir = PromptIR::new();
        ir.push(PromptAtom::Output {
            metadata: FieldMetadata::new("status", "Status", "String"),
            constraints: Some(
                FieldConstraints::new()
                    .required(true)
                    .enum_values(vec!["active".to_string(), "inactive".to_string()])
            ),
        });

        let renderer = TextRenderer;
        let config = RenderConfig::new().with_constraints();
        let output = renderer.render(&ir, &config);

        assert!(output.contains("required"));
        assert!(output.contains("one of:"));
    }

    #[test]
    fn test_json_renderer() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("a", "Input A", "String"))
            .with_output(OutputField::new("b", "Output B", "String"));

        let inputs = vec![("a".to_string(), json!("value"))];
        let ir = PromptIR::from_signature(&sig, &inputs);

        let renderer = JsonRenderer;
        let config = RenderConfig::new();
        let output = renderer.render(&ir, &config);

        assert!(output.contains("\"a\""));
        assert!(output.contains("JSON object"));
    }

    #[test]
    fn test_with_example() {
        let ir = PromptIR::new()
            .with_example(
                vec![("input".to_string(), json!("example input"))],
                vec![("output".to_string(), json!("example output"))],
            );

        assert!(ir.metadata().has_examples);
        assert_eq!(ir.len(), 1);
    }

    #[test]
    fn test_output_format() {
        let ir = PromptIR::new()
            .with_output_format(OutputFormat::Json);

        assert_eq!(ir.output_format(), &OutputFormat::Json);

        let renderer = TextRenderer;
        let config = RenderConfig::new();
        let output = renderer.render(&ir, &config);

        assert!(output.contains("JSON"));
    }

    #[test]
    fn test_describe_constraints() {
        let constraints = FieldConstraints::new()
            .required(true)
            .min_length(5)
            .max_length(100);

        let desc = describe_constraints(&constraints, 0);
        assert!(desc.contains("required"));
        assert!(desc.contains("min 5 chars"));
        assert!(desc.contains("max 100 chars"));
    }
}
