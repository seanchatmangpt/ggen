//! Schema Parser for A2A Skills
//!
//! Parses schema definitions from a simple syntax into structured representations.
//! Syntax: `TypeName { field: type, optional_field?: type }`
//!
//! This parser uses PEST (Parsing Expression Grammar Tool) for robust parsing.
//! See grammar.pest for the formal grammar definition.

use pest::Parser;
use pest_derive::Parser;
use serde::{Deserialize, Serialize};
use std::fmt;

#[derive(Parser)]
#[grammar = "schema/grammar.pest"]
struct SchemaParserInternal;

/// Schema definition for an A2A skill input/output type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Schema {
    pub name: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    pub fields: Vec<Field>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub nested_schemas: Vec<Schema>,
}

impl fmt::Display for Schema {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {{ ", self.name)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, " }}")
    }
}

/// Field definition within a schema
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Field {
    pub name: String,
    pub field_type: SchemaType,
    pub optional: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.optional {
            write!(f, "{}?: {}", self.name, self.field_type)
        } else {
            write!(f, "{}: {}", self.name, self.field_type)
        }
    }
}

/// Schema type definitions
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "lowercase")]
pub enum SchemaType {
    String,
    Integer,
    Float,
    Boolean,
    Any,
    Reference(String),
    Array(Box<SchemaType>),
    Object(Vec<Field>),
}

impl fmt::Display for SchemaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SchemaType::String => write!(f, "string"),
            SchemaType::Integer => write!(f, "integer"),
            SchemaType::Float => write!(f, "float"),
            SchemaType::Boolean => write!(f, "boolean"),
            SchemaType::Any => write!(f, "any"),
            SchemaType::Reference(name) => write!(f, "{}", name),
            SchemaType::Array(inner) => write!(f, "{}[]", inner),
            SchemaType::Object(fields) => {
                write!(f, "{{ ")?;
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", field)?;
                }
                write!(f, " }}")
            }
        }
    }
}

impl SchemaType {
    pub fn as_str(&self) -> &'static str {
        match self {
            SchemaType::String => "string",
            SchemaType::Integer => "integer",
            SchemaType::Float => "float",
            SchemaType::Boolean => "boolean",
            SchemaType::Any => "any",
            SchemaType::Reference(_) => "reference",
            SchemaType::Array(_) => "array",
            SchemaType::Object(_) => "object",
        }
    }
}

/// Schema parser that converts string definitions into Schema structs
pub struct SchemaParser;

impl SchemaParser {
    /// Parse a schema string into a Schema struct
    ///
    /// # Examples
    ///
    /// ```
    /// use ggen_core::schema::SchemaParser;
    ///
    /// let schema = SchemaParser::parse("Request { field: string }").unwrap();
    /// assert_eq!(schema.name, "Request");
    /// assert_eq!(schema.fields.len(), 1);
    /// ```
    pub fn parse(input: &str) -> Result<Schema, String> {
        let pairs = SchemaParserInternal::parse(Rule::schema, input)
            .map_err(|e| format!("Parse error: {}", e))?;

        let mut schema = None;

        for pair in pairs {
            match pair.as_rule() {
                Rule::schema => {
                    schema = Some(Self::build_schema(pair)?);
                }
                Rule::EOI => {
                    // End of input - nothing to do
                }
                _ => {}
            }
        }

        schema.ok_or_else(|| "No schema found".to_string())
    }

    fn build_schema(pair: pest::iterators::Pair<Rule>) -> Result<Schema, String> {
        let mut name = String::new();
        let mut fields = Vec::new();

        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    name = inner_pair.as_str().trim().to_string();
                }
                Rule::fields => {
                    // fields contains: "{", field_list?, "}"
                    // We need to go into field_list to get the actual fields
                    for nested_pair in inner_pair.into_inner() {
                        if nested_pair.as_rule() == Rule::field_list {
                            for field_pair in nested_pair.into_inner() {
                                if field_pair.as_rule() == Rule::field {
                                    fields.push(Self::build_field(field_pair)?);
                                }
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(Schema {
            name,
            description: None,
            fields,
            nested_schemas: vec![],
        })
    }

    fn build_field(pair: pest::iterators::Pair<Rule>) -> Result<Field, String> {
        let mut name = String::new();
        let mut field_type = SchemaType::Any;
        let mut optional = false;

        // Iterate through the tokens in order
        for inner_pair in pair.into_inner() {
            match inner_pair.as_rule() {
                Rule::identifier => {
                    name = inner_pair.as_str().trim().to_string();
                }
                Rule::type_ => {
                    field_type = Self::build_type(inner_pair)?;
                }
                Rule::optional_marker => {
                    optional = true;
                }
                _ => {
                    // Ignore other tokens (like ":" which is not a named rule)
                }
            }
        }

        Ok(Field {
            name,
            field_type,
            optional,
            description: None,
        })
    }

    fn build_type(pair: pest::iterators::Pair<Rule>) -> Result<SchemaType, String> {
        let inner = pair
            .into_inner()
            .next()
            .ok_or_else(|| "Empty type".to_string())?;

        match inner.as_rule() {
            Rule::primitive_type => {
                let type_str = inner.as_str().to_lowercase();
                match type_str.as_str() {
                    "string" => Ok(SchemaType::String),
                    "integer" => Ok(SchemaType::Integer),
                    "boolean" => Ok(SchemaType::Boolean),
                    "float" => Ok(SchemaType::Float),
                    "any" => Ok(SchemaType::Any),
                    _ => Err(format!("Unknown primitive type: {}", type_str)),
                }
            }
            Rule::named_type => {
                let name = inner.as_str().to_string();
                Ok(SchemaType::Reference(name))
            }
            Rule::array_type => {
                // array_type now directly contains the base type
                let mut inner_pairs = inner.into_inner();
                let base_type_pair = inner_pairs
                    .next()
                    .ok_or_else(|| "Array type missing inner type".to_string())?;

                // The base type could be primitive or named
                let base_type = match base_type_pair.as_rule() {
                    Rule::primitive_type => {
                        let type_str = base_type_pair.as_str().to_lowercase();
                        match type_str.as_str() {
                            "string" => SchemaType::String,
                            "integer" => SchemaType::Integer,
                            "boolean" => SchemaType::Boolean,
                            "float" => SchemaType::Float,
                            "any" => SchemaType::Any,
                            _ => {
                                return Err(format!(
                                    "Unknown primitive type in array: {}",
                                    type_str
                                ))
                            }
                        }
                    }
                    Rule::named_type => {
                        let name = base_type_pair.as_str().to_string();
                        SchemaType::Reference(name)
                    }
                    _ => {
                        return Err(format!(
                            "Unexpected array base type rule: {:?}",
                            base_type_pair.as_rule()
                        ))
                    }
                };

                Ok(SchemaType::Array(Box::new(base_type)))
            }
            _ => Err(format!("Unknown type rule: {:?}", inner.as_rule())),
        }
    }

    /// Parse a JSON Schema into a Schema struct (stub — not yet implemented)
    pub fn from_json_schema(_json: &str) -> Result<Schema, String> {
        Err("JSON Schema parsing not yet implemented".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_schema() {
        let schema = SchemaParser::parse("Request { field: string }").unwrap();
        assert_eq!(schema.name, "Request");
        assert_eq!(schema.fields.len(), 1);
        assert_eq!(schema.fields[0].name, "field");
        assert!(!schema.fields[0].optional);
        assert_eq!(schema.fields[0].field_type, SchemaType::String);
    }

    #[test]
    fn test_parse_optional_field() {
        let schema = SchemaParser::parse("Request { field?: string }").unwrap();
        assert!(schema.fields[0].optional);
    }

    #[test]
    fn test_parse_example_file_read_request() {
        let schema =
            SchemaParser::parse("FileReadRequest { path: string, offset?: integer }").unwrap();
        assert_eq!(schema.name, "FileReadRequest");
        assert_eq!(schema.fields.len(), 2);
        assert_eq!(schema.fields[0].name, "path");
        assert_eq!(schema.fields[1].name, "offset");
        assert!(schema.fields[1].optional);
    }

    #[test]
    fn test_parse_array_type() {
        let schema = SchemaParser::parse("Request { items: string[] }").unwrap();
        assert_eq!(
            schema.fields[0].field_type,
            SchemaType::Array(Box::new(SchemaType::String))
        );
    }

    #[test]
    fn test_parse_named_type() {
        let schema = SchemaParser::parse("Request { custom: MyType }").unwrap();
        assert_eq!(
            schema.fields[0].field_type,
            SchemaType::Reference("MyType".to_string())
        );
    }

    #[test]
    fn test_parse_multiple_fields() {
        let schema =
            SchemaParser::parse("Request { name: string, age: integer, active?: boolean }")
                .unwrap();
        assert_eq!(schema.fields.len(), 3);
        assert_eq!(schema.fields[0].name, "name");
        assert_eq!(schema.fields[1].name, "age");
        assert_eq!(schema.fields[2].name, "active");
        assert!(schema.fields[2].optional);
    }

    #[test]
    fn test_parse_error_missing_brace() {
        let result = SchemaParser::parse("Request { field: string");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_empty_name() {
        let result = SchemaParser::parse("{ field: string }");
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_error_invalid_type() {
        // The parser accepts any identifier as a named type reference
        // Validation of whether the type exists happens later
        let result = SchemaParser::parse("Request { field: CustomType }");
        assert!(result.is_ok()); // Named types are valid syntax

        // Test actual parse error - missing colon
        let result = SchemaParser::parse("Request { field string }");
        assert!(result.is_err());
    }

    #[test]
    fn test_schema_display() {
        let schema = SchemaParser::parse("Request { name: string, age?: integer }").unwrap();
        let display = format!("{}", schema);
        assert!(display.contains("Request {"));
        assert!(display.contains("name: string"));
        assert!(display.contains("age?: integer"));
    }

    #[test]
    fn test_field_type_display() {
        assert_eq!(format!("{}", SchemaType::String), "string");
        assert_eq!(format!("{}", SchemaType::Integer), "integer");
        assert_eq!(
            format!("{}", SchemaType::Array(Box::new(SchemaType::String))),
            "string[]"
        );
        assert_eq!(
            format!("{}", SchemaType::Reference("MyType".to_string())),
            "MyType"
        );
    }
}
