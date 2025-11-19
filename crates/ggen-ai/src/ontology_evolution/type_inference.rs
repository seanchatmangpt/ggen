//! Automatic Type Inference for Polyglot Code Generation
//!
//! This module implements advanced type inference across multiple programming
//! languages using neural symbolic reasoning and ontology-driven semantic analysis.

use anyhow::{Result, Context};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use super::neural_symbolic::NeuralSymbolicReasoner;

/// Programming language target
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum LanguageTarget {
    Rust,
    TypeScript,
    Python,
    Go,
    Java,
    CSharp,
    Kotlin,
    Swift,
}

impl LanguageTarget {
    /// Get language name as string
    pub fn as_str(&self) -> &str {
        match self {
            LanguageTarget::Rust => "rust",
            LanguageTarget::TypeScript => "typescript",
            LanguageTarget::Python => "python",
            LanguageTarget::Go => "go",
            LanguageTarget::Java => "java",
            LanguageTarget::CSharp => "csharp",
            LanguageTarget::Kotlin => "kotlin",
            LanguageTarget::Swift => "swift",
        }
    }

    /// Parse from string
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "rust" => Some(LanguageTarget::Rust),
            "typescript" | "ts" => Some(LanguageTarget::TypeScript),
            "python" | "py" => Some(LanguageTarget::Python),
            "go" | "golang" => Some(LanguageTarget::Go),
            "java" => Some(LanguageTarget::Java),
            "csharp" | "c#" => Some(LanguageTarget::CSharp),
            "kotlin" | "kt" => Some(LanguageTarget::Kotlin),
            "swift" => Some(LanguageTarget::Swift),
            _ => None,
        }
    }
}

/// Type signature in a specific language
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeSignature {
    /// Programming language
    pub language: LanguageTarget,

    /// Type name/expression
    pub type_name: String,

    /// Generic parameters
    pub generics: Vec<String>,

    /// Nullability/optionality
    pub nullable: bool,

    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

/// Semantic type information from ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SemanticType {
    /// RDF class URI
    pub class_uri: String,

    /// Properties and their ranges
    pub properties: HashMap<String, String>,

    /// Cardinality constraints
    pub cardinality: HashMap<String, (u32, Option<u32>)>,

    /// Validation rules
    pub validations: Vec<String>,
}

/// Type mapping between languages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeMapping {
    /// Source semantic type
    pub semantic_type: String,

    /// Target type signatures for each language
    pub signatures: HashMap<LanguageTarget, TypeSignature>,

    /// Confidence score
    pub confidence: f64,
}

/// Polyglot Type Inferencer
pub struct PolyglotTypeInferencer {
    /// Neural symbolic reasoner for type inference
    reasoner: NeuralSymbolicReasoner,

    /// Known type mappings
    type_mappings: Vec<TypeMapping>,

    /// Language-specific type systems
    type_systems: HashMap<LanguageTarget, TypeSystem>,

    /// Inference cache
    cache: HashMap<String, HashMap<LanguageTarget, TypeSignature>>,
}

/// Language-specific type system knowledge
#[derive(Debug, Clone)]
struct TypeSystem {
    /// Primitive types
    primitives: HashSet<String>,

    /// Collection types
    collections: HashMap<String, String>,

    /// Nullable representation
    nullable_pattern: String,

    /// Generic syntax
    generic_syntax: (String, String), // (open, close) e.g., ("<", ">")
}

impl PolyglotTypeInferencer {
    /// Create a new polyglot type inferencer
    pub fn new() -> Self {
        let mut inferencer = Self {
            reasoner: NeuralSymbolicReasoner::new(128),
            type_mappings: Vec::new(),
            type_systems: HashMap::new(),
            cache: HashMap::new(),
        };

        inferencer.initialize_type_systems();
        inferencer.initialize_base_mappings();

        inferencer
    }

    /// Initialize language-specific type systems
    fn initialize_type_systems(&mut self) {
        // Rust type system
        self.type_systems.insert(
            LanguageTarget::Rust,
            TypeSystem {
                primitives: ["i8", "i16", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128", "f32", "f64", "bool", "char", "String"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                collections: [
                    ("List", "Vec"),
                    ("Set", "HashSet"),
                    ("Map", "HashMap"),
                ]
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
                nullable_pattern: "Option<{}>".to_string(),
                generic_syntax: ("<".to_string(), ">".to_string()),
            },
        );

        // TypeScript type system
        self.type_systems.insert(
            LanguageTarget::TypeScript,
            TypeSystem {
                primitives: ["number", "string", "boolean", "null", "undefined", "bigint", "symbol"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                collections: [
                    ("List", "Array"),
                    ("Set", "Set"),
                    ("Map", "Map"),
                ]
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
                nullable_pattern: "{} | null".to_string(),
                generic_syntax: ("<".to_string(), ">".to_string()),
            },
        );

        // Python type system
        self.type_systems.insert(
            LanguageTarget::Python,
            TypeSystem {
                primitives: ["int", "float", "str", "bool", "bytes", "None"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                collections: [
                    ("List", "list"),
                    ("Set", "set"),
                    ("Map", "dict"),
                ]
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
                nullable_pattern: "Optional[{}]".to_string(),
                generic_syntax: ("[".to_string(), "]".to_string()),
            },
        );

        // Go type system
        self.type_systems.insert(
            LanguageTarget::Go,
            TypeSystem {
                primitives: ["int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", "float32", "float64", "bool", "string", "byte", "rune"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                collections: [
                    ("List", "[]"),
                    ("Set", "map[{}]bool"),
                    ("Map", "map"),
                ]
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
                nullable_pattern: "*{}".to_string(),
                generic_syntax: ("[".to_string(), "]".to_string()),
            },
        );

        // Java type system
        self.type_systems.insert(
            LanguageTarget::Java,
            TypeSystem {
                primitives: ["byte", "short", "int", "long", "float", "double", "boolean", "char", "String"]
                    .iter()
                    .map(|s| s.to_string())
                    .collect(),
                collections: [
                    ("List", "List"),
                    ("Set", "Set"),
                    ("Map", "Map"),
                ]
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
                nullable_pattern: "{}".to_string(), // Java uses null by default
                generic_syntax: ("<".to_string(), ">".to_string()),
            },
        );
    }

    /// Initialize base type mappings from ontology primitives
    fn initialize_base_mappings(&mut self) {
        // Integer mapping
        self.add_base_mapping(
            "xsd:integer",
            [
                (LanguageTarget::Rust, "i64", false),
                (LanguageTarget::TypeScript, "number", false),
                (LanguageTarget::Python, "int", false),
                (LanguageTarget::Go, "int64", false),
                (LanguageTarget::Java, "long", false),
            ],
        );

        // String mapping
        self.add_base_mapping(
            "xsd:string",
            [
                (LanguageTarget::Rust, "String", false),
                (LanguageTarget::TypeScript, "string", false),
                (LanguageTarget::Python, "str", false),
                (LanguageTarget::Go, "string", false),
                (LanguageTarget::Java, "String", false),
            ],
        );

        // Boolean mapping
        self.add_base_mapping(
            "xsd:boolean",
            [
                (LanguageTarget::Rust, "bool", false),
                (LanguageTarget::TypeScript, "boolean", false),
                (LanguageTarget::Python, "bool", false),
                (LanguageTarget::Go, "bool", false),
                (LanguageTarget::Java, "boolean", false),
            ],
        );

        // Float mapping
        self.add_base_mapping(
            "xsd:double",
            [
                (LanguageTarget::Rust, "f64", false),
                (LanguageTarget::TypeScript, "number", false),
                (LanguageTarget::Python, "float", false),
                (LanguageTarget::Go, "float64", false),
                (LanguageTarget::Java, "double", false),
            ],
        );

        // DateTime mapping
        self.add_base_mapping(
            "xsd:dateTime",
            [
                (LanguageTarget::Rust, "chrono::DateTime<chrono::Utc>", false),
                (LanguageTarget::TypeScript, "Date", false),
                (LanguageTarget::Python, "datetime.datetime", false),
                (LanguageTarget::Go, "time.Time", false),
                (LanguageTarget::Java, "java.time.Instant", false),
            ],
        );
    }

    /// Add a base type mapping
    fn add_base_mapping<const N: usize>(
        &mut self,
        semantic_type: &str,
        mappings: [(LanguageTarget, &str, bool); N],
    ) {
        let mut signatures = HashMap::new();

        for (lang, type_name, nullable) in mappings {
            signatures.insert(
                lang.clone(),
                TypeSignature {
                    language: lang,
                    type_name: type_name.to_string(),
                    generics: Vec::new(),
                    nullable,
                    metadata: HashMap::new(),
                },
            );
        }

        self.type_mappings.push(TypeMapping {
            semantic_type: semantic_type.to_string(),
            signatures,
            confidence: 1.0,
        });
    }

    /// Infer type signatures from RDF/OWL class
    pub async fn infer_from_rdf(&mut self, class_uri: &str) -> Result<HashMap<LanguageTarget, TypeSignature>> {
        // Check cache
        if let Some(cached) = self.cache.get(class_uri) {
            return Ok(cached.clone());
        }

        // Query ontology for type information
        let semantic_type = self.extract_semantic_type(class_uri).await?;

        // Map to each target language
        let mut signatures = HashMap::new();

        for (lang, type_system) in &self.type_systems {
            let signature = self.map_semantic_to_language(&semantic_type, lang, type_system)?;
            signatures.insert(lang.clone(), signature);
        }

        // Cache result
        self.cache.insert(class_uri.to_string(), signatures.clone());

        Ok(signatures)
    }

    /// Extract semantic type information from ontology
    async fn extract_semantic_type(&self, class_uri: &str) -> Result<SemanticType> {
        // Query the reasoner for class properties
        let knowledge = self.reasoner.get_symbolic_knowledge();

        let mut properties = HashMap::new();
        let mut cardinality = HashMap::new();
        let mut validations = Vec::new();

        // Find all properties with this class as domain
        for triple in &knowledge.triples {
            if triple.0 == class_uri && triple.1 == "hasProperty" {
                properties.insert(triple.2.clone(), "xsd:string".to_string());
            }
        }

        Ok(SemanticType {
            class_uri: class_uri.to_string(),
            properties,
            cardinality,
            validations,
        })
    }

    /// Map semantic type to language-specific type
    fn map_semantic_to_language(
        &self,
        semantic_type: &SemanticType,
        language: &LanguageTarget,
        type_system: &TypeSystem,
    ) -> Result<TypeSignature> {
        // Check if we have a direct mapping
        for mapping in &self.type_mappings {
            if mapping.semantic_type == semantic_type.class_uri {
                if let Some(sig) = mapping.signatures.get(language) {
                    return Ok(sig.clone());
                }
            }
        }

        // Generate type name from class URI
        let type_name = self.generate_type_name(&semantic_type.class_uri, language);

        Ok(TypeSignature {
            language: language.clone(),
            type_name,
            generics: Vec::new(),
            nullable: false,
            metadata: HashMap::new(),
        })
    }

    /// Generate type name from class URI
    fn generate_type_name(&self, class_uri: &str, language: &LanguageTarget) -> String {
        // Extract local name from URI
        let local_name = class_uri
            .split(['#', '/'])
            .last()
            .unwrap_or(class_uri);

        // Apply language-specific naming conventions
        match language {
            LanguageTarget::Rust => {
                // PascalCase for Rust
                Self::to_pascal_case(local_name)
            }
            LanguageTarget::TypeScript => {
                // PascalCase for TypeScript interfaces
                Self::to_pascal_case(local_name)
            }
            LanguageTarget::Python => {
                // snake_case for Python
                Self::to_snake_case(local_name)
            }
            LanguageTarget::Go => {
                // PascalCase for Go
                Self::to_pascal_case(local_name)
            }
            LanguageTarget::Java => {
                // PascalCase for Java
                Self::to_pascal_case(local_name)
            }
            _ => local_name.to_string(),
        }
    }

    /// Convert to PascalCase
    fn to_pascal_case(s: &str) -> String {
        s.split(['_', '-', ' '])
            .filter(|part| !part.is_empty())
            .map(|part| {
                let mut chars = part.chars();
                match chars.next() {
                    Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                    None => String::new(),
                }
            })
            .collect()
    }

    /// Convert to snake_case
    fn to_snake_case(s: &str) -> String {
        let mut result = String::new();
        let mut prev_is_upper = false;

        for (i, ch) in s.chars().enumerate() {
            if ch.is_uppercase() {
                if i > 0 && !prev_is_upper {
                    result.push('_');
                }
                result.push(ch.to_lowercase().next().unwrap());
                prev_is_upper = true;
            } else {
                result.push(ch);
                prev_is_upper = false;
            }
        }

        result
    }

    /// Infer collection type
    pub fn infer_collection(
        &self,
        element_type: &str,
        collection_kind: &str,
        language: &LanguageTarget,
    ) -> Result<TypeSignature> {
        let type_system = self
            .type_systems
            .get(language)
            .context("Language not supported")?;

        let collection_template = type_system
            .collections
            .get(collection_kind)
            .context("Collection kind not found")?;

        let type_name = match language {
            LanguageTarget::Rust => {
                format!("{}<{}>", collection_template, element_type)
            }
            LanguageTarget::TypeScript => {
                if collection_kind == "List" {
                    format!("{}[]", element_type)
                } else {
                    format!("{}<{}>", collection_template, element_type)
                }
            }
            LanguageTarget::Python => {
                format!("{}[{}]", collection_template, element_type)
            }
            LanguageTarget::Go => {
                if collection_kind == "List" {
                    format!("[]{}", element_type)
                } else {
                    collection_template.replace("{}", element_type)
                }
            }
            LanguageTarget::Java => {
                format!("{}<{}>", collection_template, element_type)
            }
            _ => format!("{}<{}>", collection_template, element_type),
        };

        Ok(TypeSignature {
            language: language.clone(),
            type_name,
            generics: vec![element_type.to_string()],
            nullable: false,
            metadata: HashMap::new(),
        })
    }

    /// Make type nullable/optional
    pub fn make_nullable(
        &self,
        signature: &TypeSignature,
    ) -> Result<TypeSignature> {
        if signature.nullable {
            return Ok(signature.clone());
        }

        let type_system = self
            .type_systems
            .get(&signature.language)
            .context("Language not supported")?;

        let type_name = type_system
            .nullable_pattern
            .replace("{}", &signature.type_name);

        Ok(TypeSignature {
            language: signature.language.clone(),
            type_name,
            generics: signature.generics.clone(),
            nullable: true,
            metadata: signature.metadata.clone(),
        })
    }

    /// Get neural symbolic reasoner
    pub fn get_reasoner_mut(&mut self) -> &mut NeuralSymbolicReasoner {
        &mut self.reasoner
    }
}

impl Default for PolyglotTypeInferencer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_language_target() {
        assert_eq!(LanguageTarget::from_str("rust"), Some(LanguageTarget::Rust));
        assert_eq!(LanguageTarget::from_str("typescript"), Some(LanguageTarget::TypeScript));
        assert_eq!(LanguageTarget::from_str("ts"), Some(LanguageTarget::TypeScript));
    }

    #[test]
    fn test_type_inferencer_creation() {
        let inferencer = PolyglotTypeInferencer::new();
        assert!(!inferencer.type_mappings.is_empty());
        assert!(inferencer.type_systems.contains_key(&LanguageTarget::Rust));
    }

    #[test]
    fn test_collection_inference() {
        let inferencer = PolyglotTypeInferencer::new();

        // Rust list
        let sig = inferencer
            .infer_collection("String", "List", &LanguageTarget::Rust)
            .unwrap();
        assert_eq!(sig.type_name, "Vec<String>");

        // TypeScript array
        let sig = inferencer
            .infer_collection("string", "List", &LanguageTarget::TypeScript)
            .unwrap();
        assert_eq!(sig.type_name, "string[]");

        // Python list
        let sig = inferencer
            .infer_collection("str", "List", &LanguageTarget::Python)
            .unwrap();
        assert_eq!(sig.type_name, "list[str]");
    }

    #[test]
    fn test_nullable() {
        let inferencer = PolyglotTypeInferencer::new();

        let sig = TypeSignature {
            language: LanguageTarget::Rust,
            type_name: "String".to_string(),
            generics: Vec::new(),
            nullable: false,
            metadata: HashMap::new(),
        };

        let nullable = inferencer.make_nullable(&sig).unwrap();
        assert_eq!(nullable.type_name, "Option<String>");
        assert!(nullable.nullable);
    }

    #[test]
    fn test_naming_conventions() {
        assert_eq!(
            PolyglotTypeInferencer::to_pascal_case("user_profile"),
            "UserProfile"
        );
        assert_eq!(
            PolyglotTypeInferencer::to_snake_case("UserProfile"),
            "user_profile"
        );
    }
}
