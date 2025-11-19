//! Probabilistic ontology with uncertainty quantification
//!
//! This module provides structures for representing ontologies with
//! uncertain relationships, probabilistic properties, and belief degrees.

use crate::{
    confidence::ConfidenceScore,
    types::{ProbabilisticType, TypeDistribution},
    Probability,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Probabilistic ontology with uncertain relationships
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticOntology {
    /// Ontology name
    pub name: String,
    /// Entities with probabilistic types
    pub entities: HashMap<String, EntityBelief>,
    /// Relationships with confidence scores
    pub relations: Vec<UncertainRelation>,
    /// Class hierarchy with probabilities
    pub hierarchy: Vec<ClassRelation>,
    /// Constraints with confidence
    pub constraints: Vec<ProbabilisticConstraint>,
}

impl ProbabilisticOntology {
    /// Create a new probabilistic ontology
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            entities: HashMap::new(),
            relations: Vec::new(),
            hierarchy: Vec::new(),
            constraints: Vec::new(),
        }
    }

    /// Add an entity with type belief
    pub fn add_entity(&mut self, name: impl Into<String>, belief: EntityBelief) {
        self.entities.insert(name.into(), belief);
    }

    /// Add a type belief for an entity
    pub fn add_type_belief(
        &mut self,
        entity: impl Into<String>,
        type_name: impl Into<String>,
        probability: f64,
    ) {
        let entity = entity.into();
        let type_name = type_name.into();

        self.entities
            .entry(entity.clone())
            .or_insert_with(|| EntityBelief::new(&entity))
            .add_type(type_name, probability);
    }

    /// Add an uncertain relationship
    pub fn add_relation(&mut self, relation: UncertainRelation) {
        self.relations.push(relation);
    }

    /// Add a class hierarchy relationship
    pub fn add_class_relation(&mut self, subclass: impl Into<String>, superclass: impl Into<String>, confidence: f64) {
        self.hierarchy.push(ClassRelation {
            subclass: subclass.into(),
            superclass: superclass.into(),
            confidence: confidence.clamp(0.0, 1.0),
        });
    }

    /// Add a probabilistic constraint
    pub fn add_constraint(&mut self, constraint: ProbabilisticConstraint) {
        self.constraints.push(constraint);
    }

    /// Get entity belief
    pub fn get_entity(&self, name: &str) -> Option<&EntityBelief> {
        self.entities.get(name)
    }

    /// Get relations involving an entity
    pub fn get_relations(&self, entity: &str) -> Vec<&UncertainRelation> {
        self.relations
            .iter()
            .filter(|r| r.subject == entity || r.object == entity)
            .collect()
    }

    /// Query types for an entity
    pub fn query_types(&self, entity: &str) -> Option<&TypeDistribution> {
        self.get_entity(entity).map(|e| &e.types)
    }

    /// Validate constraints
    pub fn validate(&self) -> Vec<ConstraintViolation> {
        let mut violations = Vec::new();

        for constraint in &self.constraints {
            if let Some(entity) = self.get_entity(&constraint.entity) {
                if !constraint.is_satisfied(entity) {
                    violations.push(ConstraintViolation {
                        constraint: constraint.clone(),
                        entity: constraint.entity.clone(),
                        actual_confidence: entity.types.get(&constraint.type_constraint).unwrap_or(0.0),
                    });
                }
            }
        }

        violations
    }

    /// Compute ontology confidence score
    pub fn confidence_score(&self) -> ConfidenceScore {
        let mut total_confidence = 0.0;
        let mut count = 0;

        // Average entity confidences
        for entity in self.entities.values() {
            if let Some((_, prob)) = entity.types.most_likely() {
                total_confidence += prob;
                count += 1;
            }
        }

        // Average relation confidences
        for relation in &self.relations {
            total_confidence += relation.confidence;
            count += 1;
        }

        let overall = if count > 0 {
            total_confidence / count as f64
        } else {
            0.0
        };

        ConfidenceScore::new(overall)
    }
}

/// Entity with probabilistic type beliefs
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntityBelief {
    /// Entity identifier
    pub name: String,
    /// Type distribution
    pub types: TypeDistribution,
    /// Properties with confidence
    pub properties: HashMap<String, PropertyBelief>,
}

impl EntityBelief {
    /// Create a new entity belief
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            types: TypeDistribution::new(),
            properties: HashMap::new(),
        }
    }

    /// Add a type with probability
    pub fn add_type(&mut self, type_name: impl Into<String>, probability: f64) {
        self.types.add(type_name, probability);
    }

    /// Add a property with belief
    pub fn add_property(&mut self, name: impl Into<String>, belief: PropertyBelief) {
        self.properties.insert(name.into(), belief);
    }

    /// Get most likely type
    pub fn most_likely_type(&self) -> Option<(&str, f64)> {
        self.types.most_likely()
    }
}

/// Property with probabilistic value
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PropertyBelief {
    /// Property name
    pub name: String,
    /// Possible values with probabilities
    pub values: HashMap<String, f64>,
    /// Confidence in property existence
    pub existence_confidence: f64,
}

impl PropertyBelief {
    /// Create a new property belief
    pub fn new(name: impl Into<String>, existence_confidence: f64) -> Self {
        Self {
            name: name.into(),
            values: HashMap::new(),
            existence_confidence: existence_confidence.clamp(0.0, 1.0),
        }
    }

    /// Add a possible value
    pub fn add_value(&mut self, value: impl Into<String>, probability: f64) {
        self.values.insert(value.into(), probability.clamp(0.0, 1.0));
    }

    /// Get most likely value
    pub fn most_likely_value(&self) -> Option<(&str, f64)> {
        self.values
            .iter()
            .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
            .map(|(v, p)| (v.as_str(), *p))
    }
}

/// Uncertain relationship between entities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UncertainRelation {
    /// Subject entity
    pub subject: String,
    /// Predicate/relationship type
    pub predicate: String,
    /// Object entity
    pub object: String,
    /// Confidence in this relationship [0.0, 1.0]
    pub confidence: f64,
    /// Source of the relationship
    pub source: RelationSource,
}

impl UncertainRelation {
    /// Create a new uncertain relationship
    pub fn new(
        subject: impl Into<String>,
        predicate: impl Into<String>,
        object: impl Into<String>,
        confidence: f64,
    ) -> Self {
        Self {
            subject: subject.into(),
            predicate: predicate.into(),
            object: object.into(),
            confidence: confidence.clamp(0.0, 1.0),
            source: RelationSource::Unknown,
        }
    }

    /// Set the source
    pub fn with_source(mut self, source: RelationSource) -> Self {
        self.source = source;
        self
    }
}

/// Source of a relationship
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RelationSource {
    /// Inferred from data
    Inference { method: String },
    /// Extracted from text
    Extraction { confidence: f64 },
    /// User-provided
    Manual,
    /// From external ontology
    External { ontology: String },
    /// Unknown source
    Unknown,
}

/// Class hierarchy relationship with confidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClassRelation {
    /// Subclass name
    pub subclass: String,
    /// Superclass name
    pub superclass: String,
    /// Confidence in this relationship
    pub confidence: f64,
}

/// Probabilistic constraint on entities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProbabilisticConstraint {
    /// Entity this constraint applies to
    pub entity: String,
    /// Type constraint
    pub type_constraint: String,
    /// Minimum confidence required
    pub min_confidence: f64,
}

impl ProbabilisticConstraint {
    /// Check if constraint is satisfied
    pub fn is_satisfied(&self, entity: &EntityBelief) -> bool {
        entity.types.get(&self.type_constraint) >= self.min_confidence
    }
}

/// Constraint violation
#[derive(Debug, Clone)]
pub struct ConstraintViolation {
    /// The violated constraint
    pub constraint: ProbabilisticConstraint,
    /// Entity that violated it
    pub entity: String,
    /// Actual confidence
    pub actual_confidence: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_probabilistic_ontology() {
        let mut ontology = ProbabilisticOntology::new("test_ontology");

        ontology.add_type_belief("user_id", "String", 0.95);
        ontology.add_type_belief("user_id", "Integer", 0.05);

        let entity = ontology.get_entity("user_id").unwrap();
        assert_eq!(entity.most_likely_type().unwrap().0, "String");
    }

    #[test]
    fn test_uncertain_relation() {
        let mut ontology = ProbabilisticOntology::new("test");

        let relation = UncertainRelation::new("Person", "subClassOf", "Entity", 0.9)
            .with_source(RelationSource::Inference {
                method: "reasoning".to_string(),
            });

        ontology.add_relation(relation);

        let relations = ontology.get_relations("Person");
        assert_eq!(relations.len(), 1);
        assert_eq!(relations[0].confidence, 0.9);
    }

    #[test]
    fn test_entity_belief() {
        let mut entity = EntityBelief::new("user");
        entity.add_type("Person", 0.8);
        entity.add_type("Agent", 0.2);

        let mut prop = PropertyBelief::new("age", 0.95);
        prop.add_value("25", 0.7);
        prop.add_value("26", 0.3);

        entity.add_property("age", prop);

        assert_eq!(entity.most_likely_type().unwrap().0, "Person");
        assert!(entity.properties.contains_key("age"));
    }

    #[test]
    fn test_constraint_validation() {
        let mut ontology = ProbabilisticOntology::new("test");

        ontology.add_type_belief("user", "Person", 0.6);

        ontology.add_constraint(ProbabilisticConstraint {
            entity: "user".to_string(),
            type_constraint: "Person".to_string(),
            min_confidence: 0.8,
        });

        let violations = ontology.validate();
        assert_eq!(violations.len(), 1);
        assert_eq!(violations[0].actual_confidence, 0.6);
    }

    #[test]
    fn test_confidence_score() {
        let mut ontology = ProbabilisticOntology::new("test");

        ontology.add_type_belief("e1", "Type1", 0.9);
        ontology.add_type_belief("e2", "Type2", 0.8);

        ontology.add_relation(UncertainRelation::new("e1", "relatesTo", "e2", 0.85));

        let score = ontology.confidence_score();
        assert!(score.overall > 0.8);
    }
}
