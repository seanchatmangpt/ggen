//! TypeScript code generation from OntologySchema
//!
//! This module generates TypeScript interfaces, Zod validation schemas, and utility types
//! from an extracted OntologySchema. It provides complete type definitions for use in
//! frontend applications, API clients, and type-safe code generation.

use crate::ontology::{Cardinality, OntologySchema, PropertyRange};
use std::fmt::Write;

/// TypeScript code generator
pub struct TypeScriptGenerator;

impl TypeScriptGenerator {
    /// Generate TypeScript interfaces for all classes in the schema
    pub fn generate_interfaces(schema: &OntologySchema) -> Result<String, String> {
        let mut output = String::new();

        // Add file header
        writeln!(
            output,
            "/**\n * Auto-generated TypeScript types from ontology: {}\n * Generated from: {}\n */\n",
            schema.label, schema.namespace
        )
        .map_err(|e| e.to_string())?;

        // Generate interfaces for each class
        for ont_class in &schema.classes {
            writeln!(
                output,
                "/**\n * {}\n */",
                ont_class.description.as_ref().unwrap_or(&String::new())
            )
            .map_err(|e| e.to_string())?;

            writeln!(output, "export interface {} {{\n", ont_class.name)
                .map_err(|e| e.to_string())?;

            // Add properties
            let class_props = schema.properties_for_class(&ont_class.uri);
            for prop in class_props {
                let ts_type = prop.range.to_typescript_type();
                let optional =
                    matches!(prop.cardinality, Cardinality::ZeroOrOne | Cardinality::Many);

                writeln!(
                    output,
                    "  /** {} */\n  {}{}: {};",
                    prop.description.as_ref().unwrap_or(&String::new()),
                    prop.name,
                    if optional { "?" } else { "" },
                    ts_type
                )
                .map_err(|e| e.to_string())?;
                writeln!(output).map_err(|e| e.to_string())?;
            }

            writeln!(output, "}}\n").map_err(|e| e.to_string())?;
        }

        Ok(output)
    }

    /// Generate Zod validation schemas for runtime validation
    pub fn generate_zod_schemas(schema: &OntologySchema) -> Result<String, String> {
        let mut output = String::new();

        // Add imports
        writeln!(
            output,
            "import {{ z }} from 'zod';\n\n/**\n * Zod validation schemas from ontology: {}\n */\n",
            schema.label
        )
        .map_err(|e| e.to_string())?;

        // Generate schemas for each class
        for ont_class in &schema.classes {
            writeln!(
                output,
                "export const {}Schema = z.object({{\n",
                ont_class.name
            )
            .map_err(|e| e.to_string())?;

            let class_props = schema.properties_for_class(&ont_class.uri);
            for prop in class_props {
                let zod_type = Self::property_to_zod(&prop.range, &prop.cardinality);
                writeln!(output, "  {}: {},", prop.name, zod_type).map_err(|e| e.to_string())?;
            }

            writeln!(output, "}});\n\n").map_err(|e| e.to_string())?;
        }

        Ok(output)
    }

    /// Generate utility types (Create, Update, Partial versions)
    pub fn generate_utility_types(schema: &OntologySchema) -> Result<String, String> {
        let mut output = String::new();

        writeln!(
            output,
            "/**\n * Utility types derived from base interfaces\n */\n"
        )
        .map_err(|e| e.to_string())?;

        for ont_class in &schema.classes {
            // Create type (all properties, none optional)
            writeln!(
                output,
                "export type {}Create = Omit<{}, 'id' | 'createdAt' | 'updatedAt'>;\n",
                ont_class.name, ont_class.name
            )
            .map_err(|e| e.to_string())?;

            // Update type (all properties optional)
            writeln!(
                output,
                "export type {}Update = Partial<{}Create>;\n",
                ont_class.name, ont_class.name
            )
            .map_err(|e| e.to_string())?;

            // Response type (includes id and timestamps)
            writeln!(
                output,
                "export type {}Response = {} & {{ id: string; createdAt: Date; updatedAt: Date }};\n",
                ont_class.name, ont_class.name
            )
            .map_err(|e| e.to_string())?;
        }

        Ok(output)
    }

    /// Convert a PropertyRange to Zod validation code
    fn property_to_zod(range: &PropertyRange, cardinality: &Cardinality) -> String {
        let base = match range {
            PropertyRange::String => "z.string()",
            PropertyRange::Integer => "z.number().int()",
            PropertyRange::Float => "z.number()",
            PropertyRange::Boolean => "z.boolean()",
            PropertyRange::DateTime => "z.date()",
            PropertyRange::Date => "z.string().date()",
            PropertyRange::Time => "z.string().time()",
            PropertyRange::Reference(class_uri) => {
                let class_name = Self::extract_class_name(class_uri);
                return format!("z.lazy(() => {}Schema)", class_name);
            }
            PropertyRange::Literal(_) => "z.any()",
            PropertyRange::Enum(values) => {
                let enum_values = values
                    .iter()
                    .map(|v| format!("'{}'", v))
                    .collect::<Vec<_>>()
                    .join(", ");
                return format!("z.enum([{}])", enum_values);
            }
        };

        match cardinality {
            Cardinality::One => base.to_string(),
            Cardinality::ZeroOrOne => format!("{}.optional()", base),
            Cardinality::Many => format!("z.array({})", base),
            Cardinality::OneOrMore => format!("z.array({}).nonempty()", base),
            Cardinality::Range { min, max } => {
                if let Some(max_val) = max {
                    format!("z.array({}).min({}).max({})", base, min, max_val)
                } else {
                    format!("z.array({}).min({})", base, min)
                }
            }
        }
    }

    /// Extract class name from URI
    /// Prefers fragment identifier (#) but falls back to path (/)
    fn extract_class_name(uri: &str) -> String {
        if let Some(fragment) = uri.split('#').next_back() {
            if fragment != uri {
                // # was found and it's not the whole string
                return fragment.to_string();
            }
        }
        // No # or # was the whole string, try /
        uri.split('/').next_back().unwrap_or("Unknown").to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ontology::{OntClass, OntProperty};

    fn create_test_schema() -> OntologySchema {
        let schema = OntologySchema {
            classes: vec![OntClass {
                uri: "http://example.org#User".to_string(),
                name: "User".to_string(),
                label: "User".to_string(),
                description: Some("A user in the system".to_string()),
                is_abstract: false,
                parent_classes: vec![],
                properties: vec![
                    "http://example.org#name".to_string(),
                    "http://example.org#email".to_string(),
                ],
                restrictions: vec![],
            }],
            properties: vec![
                OntProperty {
                    uri: "http://example.org#name".to_string(),
                    name: "name".to_string(),
                    label: "Name".to_string(),
                    description: Some("User's full name".to_string()),
                    range: PropertyRange::String,
                    domain: vec!["http://example.org#User".to_string()],
                    is_functional: true,
                    is_inverse_functional: false,
                    required: true,
                    cardinality: Cardinality::One,
                    inverse_of: None,
                },
                OntProperty {
                    uri: "http://example.org#email".to_string(),
                    name: "email".to_string(),
                    label: "Email".to_string(),
                    description: Some("User's email address".to_string()),
                    range: PropertyRange::String,
                    domain: vec!["http://example.org#User".to_string()],
                    is_functional: true,
                    is_inverse_functional: false,
                    required: true,
                    cardinality: Cardinality::One,
                    inverse_of: None,
                },
            ],
            relationships: vec![],
            namespace: "http://example.org#".to_string(),
            version: "1.0.0".to_string(),
            label: "Example Schema".to_string(),
            description: None,
            metadata: Default::default(),
        };
        schema
    }

    #[test]
    fn test_generate_interfaces() {
        let schema = create_test_schema();
        let result = TypeScriptGenerator::generate_interfaces(&schema);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("export interface User"));
        assert!(code.contains("name: string"));
        assert!(code.contains("email: string"));
    }

    #[test]
    fn test_generate_zod_schemas() {
        let schema = create_test_schema();
        let result = TypeScriptGenerator::generate_zod_schemas(&schema);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("export const UserSchema = z.object"));
        assert!(code.contains("name: z.string()"));
        assert!(code.contains("email: z.string()"));
    }

    #[test]
    fn test_generate_utility_types() {
        let schema = create_test_schema();
        let result = TypeScriptGenerator::generate_utility_types(&schema);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("export type UserCreate"));
        assert!(code.contains("export type UserUpdate"));
        assert!(code.contains("export type UserResponse"));
    }

    #[test]
    fn test_property_to_zod_primitives() {
        assert_eq!(
            TypeScriptGenerator::property_to_zod(&PropertyRange::String, &Cardinality::One),
            "z.string()"
        );
        assert_eq!(
            TypeScriptGenerator::property_to_zod(&PropertyRange::Integer, &Cardinality::One),
            "z.number().int()"
        );
        assert_eq!(
            TypeScriptGenerator::property_to_zod(&PropertyRange::Boolean, &Cardinality::One),
            "z.boolean()"
        );
    }

    #[test]
    fn test_property_to_zod_cardinality() {
        let range = PropertyRange::String;
        assert!(
            TypeScriptGenerator::property_to_zod(&range, &Cardinality::ZeroOrOne)
                .contains("optional()")
        );
        assert!(
            TypeScriptGenerator::property_to_zod(&range, &Cardinality::Many).contains("z.array")
        );
    }

    #[test]
    fn test_extract_class_name() {
        // Prefers # delimiter (fragment identifier) over / (path)
        assert_eq!(
            TypeScriptGenerator::extract_class_name("http://example.org#User"),
            "User"
        );
        // Falls back to / delimiter when # is not present
        assert_eq!(
            TypeScriptGenerator::extract_class_name("http://example.org/ontology/Product"),
            "Product"
        );
    }
}
