//! Ontology schema definitions for code entities

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A code entity in the ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeEntity {
    /// Unique identifier
    pub id: String,

    /// Entity name
    pub name: String,

    /// Entity kind
    pub kind: EntityKind,

    /// File path
    pub file_path: String,

    /// Line number
    pub line_number: Option<usize>,

    /// Documentation
    pub documentation: Option<String>,

    /// Code examples
    pub examples: Vec<String>,

    /// Relationships to other entities
    pub relationships: Vec<Relationship>,

    /// Attributes (key-value metadata)
    pub attributes: HashMap<String, String>,

    /// Visibility (public, private, crate)
    pub visibility: Visibility,
}

impl CodeEntity {
    /// Create a new code entity
    pub fn new(id: String, name: String, kind: EntityKind) -> Self {
        Self {
            id,
            name,
            kind,
            file_path: String::new(),
            line_number: None,
            documentation: None,
            examples: Vec::new(),
            relationships: Vec::new(),
            attributes: HashMap::new(),
            visibility: Visibility::Public,
        }
    }

    /// Add a relationship to another entity
    pub fn add_relationship(&mut self, kind: RelationshipKind, target: String) {
        self.relationships.push(Relationship {
            kind,
            source: self.id.clone(),
            target,
        });
    }

    /// Add an attribute
    pub fn add_attribute(&mut self, key: String, value: String) {
        self.attributes.insert(key, value);
    }
}

/// Kind of code entity
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EntityKind {
    /// Module
    Module,

    /// Function
    Function,

    /// Struct
    Struct,

    /// Enum
    Enum,

    /// Trait
    Trait,

    /// Implementation block
    Impl,

    /// Constant
    Const,

    /// Static variable
    Static,

    /// Type alias
    Type,

    /// Macro
    Macro,

    /// Field (struct/enum field)
    Field,

    /// Method
    Method,

    /// Associated type
    AssociatedType,

    /// Associated constant
    AssociatedConst,
}

impl std::fmt::Display for EntityKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EntityKind::Module => write!(f, "module"),
            EntityKind::Function => write!(f, "function"),
            EntityKind::Struct => write!(f, "struct"),
            EntityKind::Enum => write!(f, "enum"),
            EntityKind::Trait => write!(f, "trait"),
            EntityKind::Impl => write!(f, "impl"),
            EntityKind::Const => write!(f, "const"),
            EntityKind::Static => write!(f, "static"),
            EntityKind::Type => write!(f, "type"),
            EntityKind::Macro => write!(f, "macro"),
            EntityKind::Field => write!(f, "field"),
            EntityKind::Method => write!(f, "method"),
            EntityKind::AssociatedType => write!(f, "associated_type"),
            EntityKind::AssociatedConst => write!(f, "associated_const"),
        }
    }
}

/// Relationship between code entities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Relationship {
    /// Relationship kind
    pub kind: RelationshipKind,

    /// Source entity ID
    pub source: String,

    /// Target entity ID
    pub target: String,
}

/// Kind of relationship between entities
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RelationshipKind {
    /// Calls (function → function)
    Calls,

    /// Uses (module → module, trait)
    Uses,

    /// Implements (struct/enum → trait)
    Implements,

    /// Contains (module → function/struct/etc.)
    Contains,

    /// DependsOn (module → module)
    DependsOn,

    /// Extends (trait → trait)
    Extends,

    /// HasField (struct/enum → field)
    HasField,

    /// HasMethod (impl → method)
    HasMethod,

    /// Returns (function → type)
    Returns,

    /// TakesParameter (function → type)
    TakesParameter,

    /// DefinedIn (entity → module)
    DefinedIn,
}

impl std::fmt::Display for RelationshipKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RelationshipKind::Calls => write!(f, "calls"),
            RelationshipKind::Uses => write!(f, "uses"),
            RelationshipKind::Implements => write!(f, "implements"),
            RelationshipKind::Contains => write!(f, "contains"),
            RelationshipKind::DependsOn => write!(f, "dependsOn"),
            RelationshipKind::Extends => write!(f, "extends"),
            RelationshipKind::HasField => write!(f, "hasField"),
            RelationshipKind::HasMethod => write!(f, "hasMethod"),
            RelationshipKind::Returns => write!(f, "returns"),
            RelationshipKind::TakesParameter => write!(f, "takesParameter"),
            RelationshipKind::DefinedIn => write!(f, "definedIn"),
        }
    }
}

/// Visibility of code entity
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    /// Public
    Public,

    /// Private
    Private,

    /// Crate-visible
    Crate,

    /// Module-visible
    Module,
}
