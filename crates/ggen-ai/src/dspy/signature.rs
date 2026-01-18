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

    /// Create a validator for this signature
    ///
    /// Returns a `SignatureValidator` that can validate JSON input against
    /// the signature's input field constraints.
    ///
    /// # Example
    /// ```ignore
    /// use serde_json::json;
    /// use ggen_ai::dspy::{Signature, InputField};
    ///
    /// let sig = Signature::new("QA", "Question answering")
    ///     .with_input(InputField::new("question", "Question", "String"));
    ///
    /// let validator = sig.validator();
    /// let input = json!({"question": "What is Rust?"});
    /// assert!(validator.validate(&input).is_ok());
    /// ```
    pub fn validator(&self) -> crate::dspy::SignatureValidator {
        crate::dspy::SignatureValidator::new(self.clone())
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

    /// Generate JSON Schema from Signature constraints
    ///
    /// Creates a valid JSON Schema that describes the input fields with their types,
    /// constraints, and descriptions. The schema follows JSON Schema Draft 7 format.
    ///
    /// # Returns
    /// A `serde_json::Value` containing the JSON Schema object with:
    /// - "type": "object"
    /// - "properties": object mapping field names to their schemas
    /// - "required": array of required field names
    /// - "description": signature description
    ///
    /// # Example
    /// ```ignore
    /// let schema = signature.as_json_schema();
    /// // {
    /// //   "type": "object",
    /// //   "properties": {...},
    /// //   "required": [...],
    /// //   "description": "..."
    /// // }
    /// ```
    pub fn as_json_schema(&self) -> serde_json::Value {
        use serde_json::{json, Value};

        let mut properties = serde_json::Map::new();
        let mut required_fields = Vec::new();

        // Process input fields
        for field in &self.inputs {
            let field_schema = Self::field_to_json_schema(field.type_annotation(), &field.constraints);
            let mut field_obj = field_schema;

            // Add description if present
            if !field.desc().is_empty() {
                if let Value::Object(ref mut obj) = field_obj {
                    obj.insert("description".to_string(), Value::String(field.desc().to_string()));
                }
            }

            properties.insert(field.name().to_string(), field_obj);

            // Mark required if no default
            if field.constraints.required {
                required_fields.push(field.name().to_string());
            }
        }

        // Build the schema
        let mut schema = json!({
            "type": "object",
            "properties": Value::Object(properties),
        });

        // Add required array if not empty
        if !required_fields.is_empty() {
            if let Some(obj) = schema.as_object_mut() {
                obj.insert("required".to_string(), json!(required_fields));
            }
        }

        // Add description
        if !self.description.is_empty() {
            if let Some(obj) = schema.as_object_mut() {
                obj.insert("description".to_string(), Value::String(self.description.clone()));
            }
        }

        schema
    }

    /// Generate JSON Schema for output fields
    ///
    /// Creates a valid JSON Schema that describes the output fields with their types,
    /// constraints, and descriptions. This is useful for structured generation where
    /// LLMs need to produce outputs matching a specific schema.
    ///
    /// # Returns
    /// A `serde_json::Value` containing the JSON Schema object with:
    /// - "type": "object"
    /// - "properties": object mapping output field names to their schemas
    /// - "required": array of required output field names
    /// - "description": description of outputs
    ///
    /// # Example
    /// ```ignore
    /// let output_schema = signature.outputs_as_json_schema();
    /// // {
    /// //   "type": "object",
    /// //   "properties": {"answer": {...}, "confidence": {...}},
    /// //   "required": ["answer"],
    /// //   "description": "Output from QA module"
    /// // }
    /// ```
    pub fn outputs_as_json_schema(&self) -> serde_json::Value {
        use serde_json::{json, Value};

        let mut properties = serde_json::Map::new();
        let mut required_fields = Vec::new();

        // Process output fields
        for field in &self.outputs {
            let field_schema = Self::field_to_json_schema(field.type_annotation(), &field.constraints);
            let mut field_obj = field_schema;

            // Add description if present
            if !field.desc().is_empty() {
                if let Value::Object(ref mut obj) = field_obj {
                    obj.insert("description".to_string(), Value::String(field.desc().to_string()));
                }
            }

            properties.insert(field.name().to_string(), field_obj);

            // Mark required if no default
            if field.constraints.required {
                required_fields.push(field.name().to_string());
            }
        }

        // Build the schema
        let mut schema = json!({
            "type": "object",
            "properties": Value::Object(properties),
        });

        // Add required array if not empty
        if !required_fields.is_empty() {
            if let Some(obj) = schema.as_object_mut() {
                obj.insert("required".to_string(), json!(required_fields));
            }
        }

        // Add description
        if let Some(obj) = schema.as_object_mut() {
            obj.insert("description".to_string(), Value::String(format!("Output from {}", self.name)));
        }

        schema
    }

    /// Generate full JSON Schema including both inputs and outputs
    ///
    /// Creates a comprehensive JSON Schema with separate "input" and "output" definitions.
    /// This is useful for documenting the complete module interface.
    ///
    /// # Returns
    /// A `serde_json::Value` containing:
    /// - "name": module name
    /// - "description": module description
    /// - "input": JSON Schema for input fields
    /// - "output": JSON Schema for output fields
    ///
    /// # Example
    /// ```ignore
    /// let full_schema = signature.as_full_json_schema();
    /// // {
    /// //   "name": "QA",
    /// //   "description": "Question answering module",
    /// //   "input": { "type": "object", "properties": {...} },
    /// //   "output": { "type": "object", "properties": {...} }
    /// // }
    /// ```
    pub fn as_full_json_schema(&self) -> serde_json::Value {
        use serde_json::json;

        json!({
            "name": self.name,
            "description": self.description,
            "input": self.as_json_schema(),
            "output": self.outputs_as_json_schema(),
        })
    }

    /// Convert a Rust type annotation to JSON Schema type definition
    ///
    /// Handles basic types, vectors, and applies constraints.
    ///
    /// Type mappings:
    /// - "String" → {"type": "string"}
    /// - "str" → {"type": "string"}
    /// - "i32", "i64", "u32", "u64", "isize", "usize" → {"type": "integer"}
    /// - "f32", "f64" → {"type": "number"}
    /// - "bool" → {"type": "boolean"}
    /// - "Vec<T>" → {"type": "array", "items": {...}}
    fn field_to_json_schema(type_annotation: &str, constraints: &crate::dspy::field::FieldConstraints) -> serde_json::Value {
        use serde_json::json;

        let type_annotation = type_annotation.trim();
        let mut schema = Self::parse_type_to_schema(type_annotation);

        // Apply constraints
        if let Some(obj) = schema.as_object_mut() {
            if let Some(min_length) = constraints.min_length {
                obj.insert("minLength".to_string(), json!(min_length));
            }

            if let Some(max_length) = constraints.max_length {
                obj.insert("maxLength".to_string(), json!(max_length));
            }

            if let Some(min_items) = constraints.min_items {
                obj.insert("minItems".to_string(), json!(min_items));
            }

            if let Some(max_items) = constraints.max_items {
                obj.insert("maxItems".to_string(), json!(max_items));
            }

            if let Some(ref pattern) = constraints.pattern {
                obj.insert("pattern".to_string(), json!(pattern));
            }

            if let Some(ref enum_values) = constraints.enum_values {
                obj.insert("enum".to_string(), json!(enum_values));
            }
        }

        schema
    }

    /// Parse a Rust type annotation string into JSON Schema type definition
    fn parse_type_to_schema(type_annotation: &str) -> serde_json::Value {
        use serde_json::json;

        let type_annotation = type_annotation.trim();

        // Handle Vec<T> types
        if type_annotation.starts_with("Vec<") && type_annotation.ends_with('>') {
            let inner_type = &type_annotation[4..type_annotation.len() - 1];
            let items_schema = Self::parse_type_to_schema(inner_type);
            return json!({
                "type": "array",
                "items": items_schema
            });
        }

        // Handle Option<T> types - treat as nullable
        if type_annotation.starts_with("Option<") && type_annotation.ends_with('>') {
            let inner_type = &type_annotation[7..type_annotation.len() - 1];
            return Self::parse_type_to_schema(inner_type);
        }

        // Basic type mappings
        match type_annotation {
            "String" | "str" | "&str" => json!({ "type": "string" }),
            "i32" | "i64" | "i16" | "i8" | "u32" | "u64" | "u16" | "u8" | "isize" | "usize" | "int" => {
                json!({ "type": "integer" })
            }
            "f32" | "f64" | "float" | "double" => json!({ "type": "number" }),
            "bool" | "boolean" => json!({ "type": "boolean" }),
            // Default to string for unknown types
            _ => json!({ "type": "string" }),
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
    use crate::dspy::field::FieldConstraints;

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

    // JSON Schema generation tests

    #[test]
    fn test_simple_string_type_mapping() {
        let schema = Signature::parse_type_to_schema("String");
        assert_eq!(schema["type"], "string");
    }

    #[test]
    fn test_integer_type_mapping() {
        let schema_i32 = Signature::parse_type_to_schema("i32");
        assert_eq!(schema_i32["type"], "integer");

        let schema_i64 = Signature::parse_type_to_schema("i64");
        assert_eq!(schema_i64["type"], "integer");

        let schema_u32 = Signature::parse_type_to_schema("u32");
        assert_eq!(schema_u32["type"], "integer");
    }

    #[test]
    fn test_float_type_mapping() {
        let schema_f32 = Signature::parse_type_to_schema("f32");
        assert_eq!(schema_f32["type"], "number");

        let schema_f64 = Signature::parse_type_to_schema("f64");
        assert_eq!(schema_f64["type"], "number");
    }

    #[test]
    fn test_bool_type_mapping() {
        let schema = Signature::parse_type_to_schema("bool");
        assert_eq!(schema["type"], "boolean");
    }

    #[test]
    fn test_vec_string_type_mapping() {
        let schema = Signature::parse_type_to_schema("Vec<String>");
        assert_eq!(schema["type"], "array");
        assert_eq!(schema["items"]["type"], "string");
    }

    #[test]
    fn test_vec_integer_type_mapping() {
        let schema = Signature::parse_type_to_schema("Vec<i32>");
        assert_eq!(schema["type"], "array");
        assert_eq!(schema["items"]["type"], "integer");
    }

    #[test]
    fn test_vec_nested_type_mapping() {
        let schema = Signature::parse_type_to_schema("Vec<Vec<String>>");
        assert_eq!(schema["type"], "array");
        assert_eq!(schema["items"]["type"], "array");
        assert_eq!(schema["items"]["items"]["type"], "string");
    }

    #[test]
    fn test_option_type_unwrapping() {
        let schema = Signature::parse_type_to_schema("Option<String>");
        assert_eq!(schema["type"], "string");
    }

    #[test]
    fn test_option_vec_type() {
        let schema = Signature::parse_type_to_schema("Option<Vec<i32>>");
        assert_eq!(schema["type"], "array");
        assert_eq!(schema["items"]["type"], "integer");
    }

    #[test]
    fn test_str_reference_type_mapping() {
        let schema = Signature::parse_type_to_schema("&str");
        assert_eq!(schema["type"], "string");
    }

    #[test]
    fn test_unknown_type_defaults_to_string() {
        let schema = Signature::parse_type_to_schema("CustomType");
        assert_eq!(schema["type"], "string");
    }

    #[test]
    fn test_constraints_min_length() {
        let constraints = FieldConstraints::new().min_length(5);
        let schema = Signature::field_to_json_schema("String", &constraints);
        assert_eq!(schema["type"], "string");
        assert_eq!(schema["minLength"], 5);
    }

    #[test]
    fn test_constraints_max_length() {
        let constraints = FieldConstraints::new().max_length(100);
        let schema = Signature::field_to_json_schema("String", &constraints);
        assert_eq!(schema["type"], "string");
        assert_eq!(schema["maxLength"], 100);
    }

    #[test]
    fn test_constraints_min_and_max_length() {
        let constraints = FieldConstraints::new()
            .min_length(5)
            .max_length(100);
        let schema = Signature::field_to_json_schema("String", &constraints);
        assert_eq!(schema["minLength"], 5);
        assert_eq!(schema["maxLength"], 100);
    }

    #[test]
    fn test_constraints_pattern() {
        let constraints = FieldConstraints::new()
            .pattern("^[a-zA-Z]+$");
        let schema = Signature::field_to_json_schema("String", &constraints);
        assert_eq!(schema["pattern"], "^[a-zA-Z]+$");
    }

    #[test]
    fn test_constraints_enum_values() {
        let constraints = FieldConstraints::new()
            .enum_values(vec!["finops".to_string(), "banking".to_string(), "insurance".to_string()]);
        let schema = Signature::field_to_json_schema("String", &constraints);
        let enum_vals = schema["enum"].as_array().unwrap();
        assert_eq!(enum_vals.len(), 3);
        assert_eq!(enum_vals[0], "finops");
        assert_eq!(enum_vals[1], "banking");
        assert_eq!(enum_vals[2], "insurance");
    }

    #[test]
    fn test_constraints_min_items() {
        let constraints = FieldConstraints::new().min_items(1);
        let schema = Signature::field_to_json_schema("Vec<String>", &constraints);
        assert_eq!(schema["type"], "array");
        assert_eq!(schema["minItems"], 1);
    }

    #[test]
    fn test_constraints_max_items() {
        let constraints = FieldConstraints::new().max_items(10);
        let schema = Signature::field_to_json_schema("Vec<String>", &constraints);
        assert_eq!(schema["type"], "array");
        assert_eq!(schema["maxItems"], 10);
    }

    #[test]
    fn test_constraints_min_and_max_items() {
        let constraints = FieldConstraints::new()
            .min_items(1)
            .max_items(10);
        let schema = Signature::field_to_json_schema("Vec<i32>", &constraints);
        assert_eq!(schema["minItems"], 1);
        assert_eq!(schema["maxItems"], 10);
    }

    #[test]
    fn test_json_schema_basic_single_field() {
        let sig = Signature::new("Test", "Test signature")
            .with_input(InputField::new("name", "User name", "String"));

        let schema = sig.as_json_schema();

        assert_eq!(schema["type"], "object");
        assert_eq!(schema["description"], "Test signature");
        assert_eq!(schema["properties"]["name"]["type"], "string");
        assert_eq!(schema["properties"]["name"]["description"], "User name");
    }

    #[test]
    fn test_json_schema_multiple_fields() {
        let sig = Signature::new("ComplexTest", "Complex test")
            .with_input(InputField::new("text", "Input text", "String"))
            .with_input(InputField::new("count", "Count value", "i32"));

        let schema = sig.as_json_schema();

        assert_eq!(schema["properties"]["text"]["type"], "string");
        assert_eq!(schema["properties"]["count"]["type"], "integer");
    }

    #[test]
    fn test_json_schema_required_fields() {
        let mut field1 = InputField::new("required_field", "A required field", "String");
        field1.constraints = FieldConstraints::new().required(true);

        let mut field2 = InputField::new("optional_field", "An optional field", "String");
        field2.constraints = FieldConstraints::new().required(false);

        let sig = Signature::new("RequiredTest", "Test required fields")
            .with_input(field1)
            .with_input(field2);

        let schema = sig.as_json_schema();
        let required = schema["required"].as_array().unwrap();

        assert_eq!(required.len(), 1);
        assert_eq!(required[0], "required_field");
    }

    #[test]
    fn test_json_schema_with_enum_constraint() {
        let mut field = InputField::new("industry_domain", "Financial domain", "String");
        field.constraints = FieldConstraints::new()
            .enum_values(vec!["finops".to_string(), "banking".to_string(), "insurance".to_string()]);

        let sig = Signature::new("DomainSelector", "Select financial domain")
            .with_input(field);

        let schema = sig.as_json_schema();
        let enum_vals = schema["properties"]["industry_domain"]["enum"].as_array().unwrap();

        assert_eq!(enum_vals.len(), 3);
        assert_eq!(enum_vals[0], "finops");
        assert_eq!(enum_vals[1], "banking");
        assert_eq!(enum_vals[2], "insurance");
    }

    #[test]
    fn test_json_schema_with_array_constraints() {
        let mut field = InputField::new("ontology_selections", "Selected ontologies", "Vec<String>");
        field.constraints = FieldConstraints::new()
            .min_items(1)
            .max_items(10);

        let sig = Signature::new("OntologySelector", "Select ontologies")
            .with_input(field);

        let schema = sig.as_json_schema();
        let ontology_field = &schema["properties"]["ontology_selections"];

        assert_eq!(ontology_field["type"], "array");
        assert_eq!(ontology_field["items"]["type"], "string");
        assert_eq!(ontology_field["minItems"], 1);
        assert_eq!(ontology_field["maxItems"], 10);
    }

    #[test]
    fn test_json_schema_complex_example() {
        let mut domain_field = InputField::new("industry_domain", "Financial domain", "String");
        domain_field.constraints = FieldConstraints::new()
            .enum_values(vec!["finops".to_string(), "banking".to_string(), "insurance".to_string()]);

        let mut ontology_field = InputField::new("ontology_selections", "Selected ontologies", "Vec<String>");
        ontology_field.constraints = FieldConstraints::new()
            .min_items(1)
            .max_items(10);

        let sig = Signature::new("ConfigSelector", "Select financial configuration")
            .with_input(domain_field)
            .with_input(ontology_field);

        let schema = sig.as_json_schema();

        // Verify structure
        assert_eq!(schema["type"], "object");
        assert_eq!(schema["description"], "Select financial configuration");

        // Verify properties
        assert!(schema["properties"]["industry_domain"].is_object());
        assert!(schema["properties"]["ontology_selections"].is_object());

        // Verify enum field
        let enum_vals = schema["properties"]["industry_domain"]["enum"].as_array().unwrap();
        assert_eq!(enum_vals.len(), 3);

        // Verify array field
        assert_eq!(schema["properties"]["ontology_selections"]["type"], "array");
        assert_eq!(schema["properties"]["ontology_selections"]["minItems"], 1);
        assert_eq!(schema["properties"]["ontology_selections"]["maxItems"], 10);
    }

    #[test]
    fn test_json_schema_is_valid_json() {
        let sig = Signature::new("Test", "Test")
            .with_input(InputField::new("field", "desc", "String"));

        let schema = sig.as_json_schema();
        let json_str = serde_json::to_string(&schema).expect("Should serialize to JSON");

        // Should be able to parse back
        let parsed: serde_json::Value = serde_json::from_str(&json_str)
            .expect("Should deserialize from JSON");

        assert_eq!(parsed["type"], "object");
    }

    #[test]
    fn test_json_schema_no_inputs() {
        let sig = Signature::new("NoInputs", "Signature with no inputs");

        let schema = sig.as_json_schema();

        assert_eq!(schema["type"], "object");
        assert_eq!(schema["description"], "Signature with no inputs");
        assert_eq!(schema["properties"].as_object().unwrap().len(), 0);
        assert!(!schema.get("required").is_some() || schema["required"].as_array().unwrap().is_empty());
    }

    #[test]
    fn test_json_schema_with_all_string_constraints() {
        let mut field = InputField::new("constrained_text", "Constrained text field", "String");
        field.constraints = FieldConstraints::new()
            .min_length(5)
            .max_length(100)
            .pattern("^[a-zA-Z0-9_]*$");

        let sig = Signature::new("ConstrainedText", "Text with constraints")
            .with_input(field);

        let schema = sig.as_json_schema();
        let text_field = &schema["properties"]["constrained_text"];

        assert_eq!(text_field["type"], "string");
        assert_eq!(text_field["minLength"], 5);
        assert_eq!(text_field["maxLength"], 100);
        assert_eq!(text_field["pattern"], "^[a-zA-Z0-9_]*$");
    }

    #[test]
    fn test_json_schema_multiple_types_mixed() {
        let sig = Signature::new("MixedTypes", "Signature with mixed types")
            .with_input(InputField::new("text", "String field", "String"))
            .with_input(InputField::new("count", "Integer field", "i32"))
            .with_input(InputField::new("ratio", "Float field", "f64"))
            .with_input(InputField::new("enabled", "Boolean field", "bool"))
            .with_input(InputField::new("items", "Array field", "Vec<String>"));

        let schema = sig.as_json_schema();

        assert_eq!(schema["properties"]["text"]["type"], "string");
        assert_eq!(schema["properties"]["count"]["type"], "integer");
        assert_eq!(schema["properties"]["ratio"]["type"], "number");
        assert_eq!(schema["properties"]["enabled"]["type"], "boolean");
        assert_eq!(schema["properties"]["items"]["type"], "array");
        assert_eq!(schema["properties"]["items"]["items"]["type"], "string");
    }

    #[test]
    fn test_json_schema_preserves_order_fields() {
        let sig = Signature::new("OrderTest", "Test field order")
            .with_input(InputField::new("first", "First field", "String"))
            .with_input(InputField::new("second", "Second field", "String"))
            .with_input(InputField::new("third", "Third field", "String"));

        let schema = sig.as_json_schema();
        let properties = schema["properties"].as_object().unwrap();

        assert!(properties.contains_key("first"));
        assert!(properties.contains_key("second"));
        assert!(properties.contains_key("third"));
    }

    // ===== Multi-Output Field Support Tests =====

    #[test]
    fn test_single_output_field_json_schema() {
        // Arrange
        let sig = Signature::new("QA", "Question answering")
            .with_input(InputField::new("question", "The question", "String"))
            .with_output(OutputField::new("answer", "The answer", "String"));

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        assert_eq!(output_schema["type"], "object");
        assert_eq!(output_schema["properties"]["answer"]["type"], "string");
        assert_eq!(output_schema["properties"]["answer"]["description"], "The answer");
        assert_eq!(output_schema["description"], "Output from QA");
    }

    #[test]
    fn test_multiple_output_fields_json_schema() {
        // Arrange
        let sig = Signature::new("Reasoning", "Multi-step reasoning")
            .with_input(InputField::new("question", "Question to answer", "String"))
            .with_output(OutputField::new("rationale", "Step-by-step reasoning", "String"))
            .with_output(OutputField::new("answer", "Final answer", "String"))
            .with_output(OutputField::new("confidence", "Confidence score", "f64"));

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        assert_eq!(output_schema["type"], "object");
        assert_eq!(output_schema["properties"]["rationale"]["type"], "string");
        assert_eq!(output_schema["properties"]["answer"]["type"], "string");
        assert_eq!(output_schema["properties"]["confidence"]["type"], "number");
    }

    #[test]
    fn test_output_fields_with_constraints() {
        // Arrange
        let mut answer_field = OutputField::new("answer", "Generated answer", "String");
        answer_field.constraints = FieldConstraints::new()
            .required(true)
            .min_length(10)
            .max_length(500);

        let mut tags_field = OutputField::new("tags", "Generated tags", "Vec<String>");
        tags_field.constraints = FieldConstraints::new()
            .min_items(1)
            .max_items(5);

        let sig = Signature::new("ContentGenerator", "Generate content with tags")
            .with_input(InputField::new("topic", "Content topic", "String"))
            .with_output(answer_field)
            .with_output(tags_field);

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        assert_eq!(output_schema["properties"]["answer"]["minLength"], 10);
        assert_eq!(output_schema["properties"]["answer"]["maxLength"], 500);
        assert_eq!(output_schema["properties"]["tags"]["minItems"], 1);
        assert_eq!(output_schema["properties"]["tags"]["maxItems"], 5);

        // Verify required fields
        let required = output_schema["required"].as_array().unwrap();
        assert_eq!(required.len(), 1);
        assert!(required.contains(&serde_json::json!("answer")));
    }

    #[test]
    fn test_output_fields_with_enum_constraint() {
        // Arrange
        let mut classification_field = OutputField::new("category", "Document category", "String");
        classification_field.constraints = FieldConstraints::new()
            .required(true)
            .enum_values(vec![
                "technical".to_string(),
                "business".to_string(),
                "legal".to_string(),
            ]);

        let sig = Signature::new("Classifier", "Classify documents")
            .with_input(InputField::new("text", "Document text", "String"))
            .with_output(classification_field);

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        let enum_vals = output_schema["properties"]["category"]["enum"].as_array().unwrap();
        assert_eq!(enum_vals.len(), 3);
        assert_eq!(enum_vals[0], "technical");
        assert_eq!(enum_vals[1], "business");
        assert_eq!(enum_vals[2], "legal");
    }

    #[test]
    fn test_full_json_schema_with_inputs_and_outputs() {
        // Arrange
        let sig = Signature::new("Translation", "Translate text between languages")
            .with_input(InputField::new("text", "Text to translate", "String"))
            .with_input(InputField::new("target_lang", "Target language", "String"))
            .with_output(OutputField::new("translated_text", "Translated text", "String"))
            .with_output(OutputField::new("detected_lang", "Detected source language", "String"));

        // Act
        let full_schema = sig.as_full_json_schema();

        // Assert
        assert_eq!(full_schema["name"], "Translation");
        assert_eq!(full_schema["description"], "Translate text between languages");

        // Verify input schema
        assert_eq!(full_schema["input"]["type"], "object");
        assert!(full_schema["input"]["properties"]["text"].is_object());
        assert!(full_schema["input"]["properties"]["target_lang"].is_object());

        // Verify output schema
        assert_eq!(full_schema["output"]["type"], "object");
        assert!(full_schema["output"]["properties"]["translated_text"].is_object());
        assert!(full_schema["output"]["properties"]["detected_lang"].is_object());
    }

    #[test]
    fn test_backward_compatibility_as_json_schema_only_inputs() {
        // Arrange: Create signature with both inputs and outputs
        let sig = Signature::new("BackwardCompat", "Test backward compatibility")
            .with_input(InputField::new("input1", "First input", "String"))
            .with_input(InputField::new("input2", "Second input", "i32"))
            .with_output(OutputField::new("output1", "First output", "String"));

        // Act: Call the original as_json_schema method
        let input_schema = sig.as_json_schema();

        // Assert: Only input fields are in the schema (backward compatible)
        assert_eq!(input_schema["type"], "object");
        assert!(input_schema["properties"]["input1"].is_object());
        assert!(input_schema["properties"]["input2"].is_object());
        assert!(input_schema["properties"].get("output1").is_none());
    }

    #[test]
    fn test_outputs_as_json_schema_with_no_outputs() {
        // Arrange: Signature with no outputs
        let sig = Signature::new("NoOutputs", "Module with no outputs")
            .with_input(InputField::new("data", "Input data", "String"));

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert: Empty but valid schema
        assert_eq!(output_schema["type"], "object");
        assert_eq!(output_schema["properties"].as_object().unwrap().len(), 0);
    }

    #[test]
    fn test_outputs_with_complex_types() {
        // Arrange
        let sig = Signature::new("ComplexOutputs", "Module with complex output types")
            .with_input(InputField::new("query", "Search query", "String"))
            .with_output(OutputField::new("results", "Search results", "Vec<String>"))
            .with_output(OutputField::new("scores", "Relevance scores", "Vec<f64>"))
            .with_output(OutputField::new("metadata", "Result metadata", "Vec<Vec<String>>"));

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        assert_eq!(output_schema["properties"]["results"]["type"], "array");
        assert_eq!(output_schema["properties"]["results"]["items"]["type"], "string");

        assert_eq!(output_schema["properties"]["scores"]["type"], "array");
        assert_eq!(output_schema["properties"]["scores"]["items"]["type"], "number");

        assert_eq!(output_schema["properties"]["metadata"]["type"], "array");
        assert_eq!(output_schema["properties"]["metadata"]["items"]["type"], "array");
        assert_eq!(output_schema["properties"]["metadata"]["items"]["items"]["type"], "string");
    }

    #[test]
    fn test_output_schema_serialization() {
        // Arrange
        let mut answer_field = OutputField::new("answer", "Generated answer", "String");
        answer_field.constraints = FieldConstraints::new()
            .required(true)
            .min_length(5);

        let sig = Signature::new("Generator", "Content generator")
            .with_output(answer_field);

        // Act
        let output_schema = sig.outputs_as_json_schema();
        let json_str = serde_json::to_string(&output_schema).expect("Should serialize");
        let parsed: serde_json::Value = serde_json::from_str(&json_str).expect("Should deserialize");

        // Assert: Round trip preserves structure
        assert_eq!(output_schema["type"], parsed["type"]);
        assert_eq!(output_schema["properties"]["answer"]["minLength"], parsed["properties"]["answer"]["minLength"]);
    }

    #[test]
    fn test_full_schema_serialization() {
        // Arrange
        let sig = Signature::new("FullModule", "Complete module")
            .with_input(InputField::new("input", "Input field", "String"))
            .with_output(OutputField::new("output", "Output field", "String"));

        // Act
        let full_schema = sig.as_full_json_schema();
        let json_str = serde_json::to_string_pretty(&full_schema).expect("Should serialize");

        // Assert: Contains all expected keys
        assert!(json_str.contains("\"name\""));
        assert!(json_str.contains("\"description\""));
        assert!(json_str.contains("\"input\""));
        assert!(json_str.contains("\"output\""));
    }

    #[test]
    fn test_chain_of_thought_pattern_multiple_outputs() {
        // Arrange: Chain-of-thought pattern with rationale and answer
        let mut rationale_field = OutputField::new("rationale", "Step-by-step reasoning", "String");
        rationale_field.constraints = FieldConstraints::new()
            .required(true)
            .min_length(20);

        let mut answer_field = OutputField::new("answer", "Final answer", "String");
        answer_field.constraints = FieldConstraints::new()
            .required(true)
            .min_length(1);

        let sig = Signature::new("ChainOfThought", "Answer with reasoning")
            .with_input(InputField::new("question", "Question to answer", "String"))
            .with_output(rationale_field)
            .with_output(answer_field);

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        let required = output_schema["required"].as_array().unwrap();
        assert_eq!(required.len(), 2);
        assert!(required.contains(&serde_json::json!("rationale")));
        assert!(required.contains(&serde_json::json!("answer")));

        assert_eq!(output_schema["properties"]["rationale"]["minLength"], 20);
        assert_eq!(output_schema["properties"]["answer"]["minLength"], 1);
    }

    #[test]
    fn test_output_field_names_retrieval() {
        // Arrange
        let sig = Signature::new("MultiOutput", "Multiple outputs")
            .with_output(OutputField::new("result1", "First result", "String"))
            .with_output(OutputField::new("result2", "Second result", "String"))
            .with_output(OutputField::new("result3", "Third result", "i32"));

        // Act
        let output_names = sig.output_names();

        // Assert
        assert_eq!(output_names.len(), 3);
        assert_eq!(output_names, vec!["result1", "result2", "result3"]);
    }

    #[test]
    fn test_output_field_lookup() {
        // Arrange
        let sig = Signature::new("Lookup", "Test field lookup")
            .with_output(OutputField::new("answer", "The answer", "String"))
            .with_output(OutputField::new("confidence", "Confidence score", "f64"));

        // Act & Assert
        assert!(sig.get_output("answer").is_some());
        assert!(sig.get_output("confidence").is_some());
        assert!(sig.get_output("nonexistent").is_none());

        let answer_field = sig.get_output("answer").unwrap();
        assert_eq!(answer_field.name(), "answer");
        assert_eq!(answer_field.desc(), "The answer");
    }

    #[test]
    fn test_rust_struct_generation_multiple_outputs() {
        // Arrange
        let sig = Signature::new("MultiStruct", "Test struct generation")
            .with_input(InputField::new("input", "Input data", "String"))
            .with_output(OutputField::new("output1", "First output", "String"))
            .with_output(OutputField::new("output2", "Second output", "i32"));

        // Act
        let code = sig.as_rust_struct();

        // Assert: Both outputs appear in the generated struct
        assert!(code.contains("pub output1: String"));
        assert!(code.contains("pub output2: i32"));
        assert!(code.contains("MultiStruct_Outputs"));
    }

    #[test]
    fn test_output_schema_with_pattern_constraint() {
        // Arrange
        let mut uuid_field = OutputField::new("uuid", "Generated UUID", "String");
        uuid_field.constraints = FieldConstraints::new()
            .pattern("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$");

        let sig = Signature::new("UUIDGenerator", "Generate UUIDs")
            .with_output(uuid_field);

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        assert!(output_schema["properties"]["uuid"]["pattern"].is_string());
        let pattern = output_schema["properties"]["uuid"]["pattern"].as_str().unwrap();
        assert!(pattern.contains("^[0-9a-f]{8}"));
    }

    #[test]
    fn test_mixed_required_and_optional_outputs() {
        // Arrange
        let mut required_output = OutputField::new("answer", "Required answer", "String");
        required_output.constraints = FieldConstraints::new().required(true);

        let mut optional_output = OutputField::new("explanation", "Optional explanation", "String");
        optional_output.constraints = FieldConstraints::new().required(false);

        let sig = Signature::new("MixedOutputs", "Mixed required/optional outputs")
            .with_output(required_output)
            .with_output(optional_output);

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        let required = output_schema["required"].as_array().unwrap();
        assert_eq!(required.len(), 1);
        assert!(required.contains(&serde_json::json!("answer")));
        assert!(!required.contains(&serde_json::json!("explanation")));
    }

    #[test]
    fn test_output_schema_preserves_descriptions() {
        // Arrange
        let sig = Signature::new("Descriptions", "Test description preservation")
            .with_output(OutputField::new("result", "A detailed result description", "String"));

        // Act
        let output_schema = sig.outputs_as_json_schema();

        // Assert
        assert_eq!(
            output_schema["properties"]["result"]["description"],
            "A detailed result description"
        );
    }
}
