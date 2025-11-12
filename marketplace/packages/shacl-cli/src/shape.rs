//! SHACL shape definitions and management

use crate::{Error, Result};

/// Represents a SHACL shape
#[derive(Debug, Clone)]
pub struct Shape {
    pub name: String,
    pub shape_type: ShapeType,
    pub target: Option<Target>,
}

#[derive(Debug, Clone)]
pub enum ShapeType {
    Node,
    Property,
}

#[derive(Debug, Clone)]
pub enum Target {
    Class(String),
    Node(String),
    SubjectsOf(String),
    ObjectsOf(String),
}

impl Shape {
    /// Create a new shape
    pub fn new(name: String, shape_type: ShapeType) -> Self {
        Self {
            name,
            shape_type,
            target: None,
        }
    }

    /// Set the target for this shape
    pub fn with_target(mut self, target: Target) -> Self {
        self.target = Some(target);
        self
    }
}
