//! Neural Symbolic Reasoning Engine
//!
//! This module implements a hybrid neural-symbolic reasoning system that
//! combines the pattern recognition capabilities of neural networks with
//! the logical reasoning of symbolic AI for ontology evolution and type inference.

use anyhow::{Result, Context};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Symbolic knowledge representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SymbolicKnowledge {
    /// RDF triples (subject, predicate, object)
    pub triples: Vec<(String, String, String)>,

    /// Inference rules in SWRL-like format
    pub rules: Vec<InferenceRule>,

    /// Type hierarchy
    pub type_hierarchy: HashMap<String, Vec<String>>,

    /// Property constraints
    pub constraints: HashMap<String, Vec<Constraint>>,
}

/// Inference rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InferenceRule {
    pub name: String,
    pub premises: Vec<Pattern>,
    pub conclusions: Vec<Pattern>,
    pub confidence: f64,
}

/// Pattern for rule matching
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Pattern {
    pub subject: PatternTerm,
    pub predicate: PatternTerm,
    pub object: PatternTerm,
}

/// Pattern term (variable or constant)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PatternTerm {
    Variable(String),
    Constant(String),
}

/// Constraint on properties
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constraint {
    TypeConstraint { required_type: String },
    CardinalityConstraint { min: u32, max: Option<u32> },
    RangeConstraint { min: f64, max: f64 },
    RegexConstraint { pattern: String },
    Custom { name: String, parameters: HashMap<String, String> },
}

/// Neural representation (embeddings)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NeuralRepresentation {
    /// Embedding vectors for entities
    pub entity_embeddings: HashMap<String, Vec<f64>>,

    /// Embedding vectors for predicates
    pub predicate_embeddings: HashMap<String, Vec<f64>>,

    /// Embedding dimension
    pub dimension: usize,
}

impl NeuralRepresentation {
    /// Create a new neural representation
    pub fn new(dimension: usize) -> Self {
        Self {
            entity_embeddings: HashMap::new(),
            predicate_embeddings: HashMap::new(),
            dimension,
        }
    }

    /// Initialize random embeddings for an entity
    pub fn initialize_entity(&mut self, entity: String) {
        if !self.entity_embeddings.contains_key(&entity) {
            let embedding: Vec<f64> = (0..self.dimension)
                .map(|_| rand::random::<f64>() * 2.0 - 1.0)
                .collect();
            self.entity_embeddings.insert(entity, embedding);
        }
    }

    /// Initialize random embeddings for a predicate
    pub fn initialize_predicate(&mut self, predicate: String) {
        if !self.predicate_embeddings.contains_key(&predicate) {
            let embedding: Vec<f64> = (0..self.dimension)
                .map(|_| rand::random::<f64>() * 2.0 - 1.0)
                .collect();
            self.predicate_embeddings.insert(predicate, embedding);
        }
    }

    /// Compute similarity between two entities
    pub fn entity_similarity(&self, e1: &str, e2: &str) -> f64 {
        if let (Some(v1), Some(v2)) = (
            self.entity_embeddings.get(e1),
            self.entity_embeddings.get(e2),
        ) {
            cosine_similarity(v1, v2)
        } else {
            0.0
        }
    }

    /// Compute similarity between two predicates
    pub fn predicate_similarity(&self, p1: &str, p2: &str) -> f64 {
        if let (Some(v1), Some(v2)) = (
            self.predicate_embeddings.get(p1),
            self.predicate_embeddings.get(p2),
        ) {
            cosine_similarity(v1, v2)
        } else {
            0.0
        }
    }
}

/// Neural Symbolic Reasoner
pub struct NeuralSymbolicReasoner {
    /// Symbolic knowledge base
    symbolic: SymbolicKnowledge,

    /// Neural representations
    neural: NeuralRepresentation,

    /// Learned rule confidences
    rule_confidences: HashMap<String, f64>,

    /// Reasoning cache
    cache: HashMap<String, ReasoningResult>,
}

/// Reasoning result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReasoningResult {
    pub inferred_triples: Vec<(String, String, String)>,
    pub confidence: f64,
    pub explanation: Vec<String>,
}

impl NeuralSymbolicReasoner {
    /// Create a new neural symbolic reasoner
    pub fn new(embedding_dimension: usize) -> Self {
        Self {
            symbolic: SymbolicKnowledge {
                triples: Vec::new(),
                rules: Vec::new(),
                type_hierarchy: HashMap::new(),
                constraints: HashMap::new(),
            },
            neural: NeuralRepresentation::new(embedding_dimension),
            rule_confidences: HashMap::new(),
            cache: HashMap::new(),
        }
    }

    /// Add a triple to the knowledge base
    pub fn add_triple(&mut self, subject: String, predicate: String, object: String) {
        // Add to symbolic KB
        self.symbolic.triples.push((
            subject.clone(),
            predicate.clone(),
            object.clone(),
        ));

        // Initialize neural representations
        self.neural.initialize_entity(subject);
        self.neural.initialize_entity(object);
        self.neural.initialize_predicate(predicate);
    }

    /// Add an inference rule
    pub fn add_rule(&mut self, rule: InferenceRule) {
        self.rule_confidences.insert(rule.name.clone(), rule.confidence);
        self.symbolic.rules.push(rule);
    }

    /// Forward chaining inference
    pub fn forward_chain(&self) -> Result<Vec<(String, String, String)>> {
        let mut inferred = Vec::new();
        let mut known_triples: HashSet<_> = self.symbolic.triples.iter().cloned().collect();

        let mut changed = true;
        while changed {
            changed = false;

            for rule in &self.symbolic.rules {
                // Try to match premises
                if let Some(bindings) = self.match_premises(&rule.premises, &known_triples) {
                    // Apply conclusions with bindings
                    for conclusion in &rule.conclusions {
                        if let Some(triple) = self.apply_pattern(conclusion, &bindings) {
                            if !known_triples.contains(&triple) {
                                known_triples.insert(triple.clone());
                                inferred.push(triple);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }

        Ok(inferred)
    }

    /// Match premises against known triples
    fn match_premises(
        &self,
        premises: &[Pattern],
        triples: &HashSet<(String, String, String)>,
    ) -> Option<HashMap<String, String>> {
        let mut bindings = HashMap::new();

        for premise in premises {
            let mut matched = false;

            for triple in triples {
                if let Some(new_bindings) = self.match_pattern(premise, triple, &bindings) {
                    bindings = new_bindings;
                    matched = true;
                    break;
                }
            }

            if !matched {
                return None;
            }
        }

        Some(bindings)
    }

    /// Match a pattern against a triple
    fn match_pattern(
        &self,
        pattern: &Pattern,
        triple: &(String, String, String),
        existing_bindings: &HashMap<String, String>,
    ) -> Option<HashMap<String, String>> {
        let mut bindings = existing_bindings.clone();

        // Match subject
        if !self.match_term(&pattern.subject, &triple.0, &mut bindings) {
            return None;
        }

        // Match predicate
        if !self.match_term(&pattern.predicate, &triple.1, &mut bindings) {
            return None;
        }

        // Match object
        if !self.match_term(&pattern.object, &triple.2, &mut bindings) {
            return None;
        }

        Some(bindings)
    }

    /// Match a pattern term
    fn match_term(
        &self,
        term: &PatternTerm,
        value: &str,
        bindings: &mut HashMap<String, String>,
    ) -> bool {
        match term {
            PatternTerm::Constant(c) => c == value,
            PatternTerm::Variable(v) => {
                if let Some(bound_value) = bindings.get(v) {
                    bound_value == value
                } else {
                    bindings.insert(v.clone(), value.to_string());
                    true
                }
            }
        }
    }

    /// Apply pattern with variable bindings
    fn apply_pattern(
        &self,
        pattern: &Pattern,
        bindings: &HashMap<String, String>,
    ) -> Option<(String, String, String)> {
        let subject = self.resolve_term(&pattern.subject, bindings)?;
        let predicate = self.resolve_term(&pattern.predicate, bindings)?;
        let object = self.resolve_term(&pattern.object, bindings)?;

        Some((subject, predicate, object))
    }

    /// Resolve a pattern term with bindings
    fn resolve_term(
        &self,
        term: &PatternTerm,
        bindings: &HashMap<String, String>,
    ) -> Option<String> {
        match term {
            PatternTerm::Constant(c) => Some(c.clone()),
            PatternTerm::Variable(v) => bindings.get(v).cloned(),
        }
    }

    /// Neural-guided inference (use embeddings to suggest likely inferences)
    pub fn neural_guided_inference(&self, subject: &str, predicate: &str) -> Result<Vec<String>> {
        let mut candidates = Vec::new();

        // Find similar predicates
        let similar_predicates: Vec<_> = self
            .neural
            .predicate_embeddings
            .keys()
            .filter(|p| self.neural.predicate_similarity(predicate, p) > 0.7)
            .cloned()
            .collect();

        // Find triples with similar predicates
        for triple in &self.symbolic.triples {
            if triple.0 == subject && similar_predicates.contains(&triple.1) {
                // Compute confidence based on similarity
                let similarity = self.neural.predicate_similarity(predicate, &triple.1);
                if similarity > 0.7 {
                    candidates.push(triple.2.clone());
                }
            }
        }

        Ok(candidates)
    }

    /// Learn embeddings from triples using simple matrix factorization
    pub fn train_embeddings(&mut self, epochs: usize, learning_rate: f64) -> Result<()> {
        for _ in 0..epochs {
            for triple in &self.symbolic.triples.clone() {
                self.update_embedding_for_triple(triple, learning_rate)?;
            }
        }
        Ok(())
    }

    /// Update embeddings for a single triple (simplified TransE)
    fn update_embedding_for_triple(
        &mut self,
        triple: &(String, String, String),
        learning_rate: f64,
    ) -> Result<()> {
        let (subj, pred, obj) = triple;

        // Ensure embeddings exist
        self.neural.initialize_entity(subj.clone());
        self.neural.initialize_entity(obj.clone());
        self.neural.initialize_predicate(pred.clone());

        // TransE: h + r ≈ t
        // Gradient descent to minimize ||h + r - t||

        let h = self.neural.entity_embeddings.get(subj).unwrap().clone();
        let r = self.neural.predicate_embeddings.get(pred).unwrap().clone();
        let t = self.neural.entity_embeddings.get(obj).unwrap().clone();

        let mut h_new = h.clone();
        let mut r_new = r.clone();
        let mut t_new = t.clone();

        for i in 0..self.neural.dimension {
            let diff = h[i] + r[i] - t[i];

            // Update rule: θ := θ - α * ∇L
            h_new[i] -= learning_rate * diff;
            r_new[i] -= learning_rate * diff;
            t_new[i] += learning_rate * diff;
        }

        // Update embeddings
        self.neural.entity_embeddings.insert(subj.clone(), h_new);
        self.neural.predicate_embeddings.insert(pred.clone(), r_new);
        self.neural.entity_embeddings.insert(obj.clone(), t_new);

        Ok(())
    }

    /// Hybrid reasoning: combine symbolic and neural
    pub fn hybrid_reason(&self, query: &str) -> Result<ReasoningResult> {
        // Parse query (simplified)
        let parts: Vec<&str> = query.split_whitespace().collect();
        if parts.len() < 3 {
            return Ok(ReasoningResult {
                inferred_triples: Vec::new(),
                confidence: 0.0,
                explanation: vec!["Invalid query format".to_string()],
            });
        }

        let subject = parts[0];
        let predicate = parts[1];

        // Symbolic inference
        let symbolic_results = self.forward_chain()?;

        // Neural inference
        let neural_candidates = self.neural_guided_inference(subject, predicate)?;

        // Combine results
        let mut inferred = symbolic_results.clone();
        for candidate in neural_candidates {
            let triple = (
                subject.to_string(),
                predicate.to_string(),
                candidate,
            );
            if !inferred.contains(&triple) {
                inferred.push(triple);
            }
        }

        Ok(ReasoningResult {
            inferred_triples: inferred,
            confidence: 0.8,
            explanation: vec![
                "Combined symbolic and neural reasoning".to_string(),
            ],
        })
    }

    /// Get symbolic knowledge
    pub fn get_symbolic_knowledge(&self) -> &SymbolicKnowledge {
        &self.symbolic
    }

    /// Get neural representation
    pub fn get_neural_representation(&self) -> &NeuralRepresentation {
        &self.neural
    }
}

/// Compute cosine similarity between two vectors
fn cosine_similarity(v1: &[f64], v2: &[f64]) -> f64 {
    if v1.len() != v2.len() {
        return 0.0;
    }

    let dot_product: f64 = v1.iter().zip(v2.iter()).map(|(a, b)| a * b).sum();
    let norm1: f64 = v1.iter().map(|x| x * x).sum::<f64>().sqrt();
    let norm2: f64 = v2.iter().map(|x| x * x).sum::<f64>().sqrt();

    if norm1 == 0.0 || norm2 == 0.0 {
        0.0
    } else {
        dot_product / (norm1 * norm2)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_neural_representation() {
        let mut neural = NeuralRepresentation::new(10);
        neural.initialize_entity("Entity1".to_string());
        neural.initialize_entity("Entity2".to_string());

        assert!(neural.entity_embeddings.contains_key("Entity1"));
        assert_eq!(neural.entity_embeddings.get("Entity1").unwrap().len(), 10);
    }

    #[test]
    fn test_symbolic_reasoning() {
        let mut reasoner = NeuralSymbolicReasoner::new(10);

        reasoner.add_triple(
            "Dog".to_string(),
            "subClassOf".to_string(),
            "Animal".to_string(),
        );

        reasoner.add_triple(
            "Fido".to_string(),
            "instanceOf".to_string(),
            "Dog".to_string(),
        );

        // Add rule: X instanceOf Y, Y subClassOf Z => X instanceOf Z
        let rule = InferenceRule {
            name: "transitive_instance".to_string(),
            premises: vec![
                Pattern {
                    subject: PatternTerm::Variable("x".to_string()),
                    predicate: PatternTerm::Constant("instanceOf".to_string()),
                    object: PatternTerm::Variable("y".to_string()),
                },
                Pattern {
                    subject: PatternTerm::Variable("y".to_string()),
                    predicate: PatternTerm::Constant("subClassOf".to_string()),
                    object: PatternTerm::Variable("z".to_string()),
                },
            ],
            conclusions: vec![Pattern {
                subject: PatternTerm::Variable("x".to_string()),
                predicate: PatternTerm::Constant("instanceOf".to_string()),
                object: PatternTerm::Variable("z".to_string()),
            }],
            confidence: 1.0,
        };

        reasoner.add_rule(rule);

        let inferred = reasoner.forward_chain().unwrap();
        assert_eq!(inferred.len(), 1);
        assert_eq!(inferred[0].0, "Fido");
        assert_eq!(inferred[0].1, "instanceOf");
        assert_eq!(inferred[0].2, "Animal");
    }

    #[test]
    fn test_cosine_similarity() {
        let v1 = vec![1.0, 0.0, 0.0];
        let v2 = vec![1.0, 0.0, 0.0];
        assert!((cosine_similarity(&v1, &v2) - 1.0).abs() < 1e-10);

        let v3 = vec![1.0, 0.0, 0.0];
        let v4 = vec![0.0, 1.0, 0.0];
        assert!((cosine_similarity(&v3, &v4) - 0.0).abs() < 1e-10);
    }
}
