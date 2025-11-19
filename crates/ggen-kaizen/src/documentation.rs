//! # Standard Work Documentation Generator
//!
//! Auto-generates documentation from identified best practices, creating
//! standardized work procedures for ontology development.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tera::{Context, Tera};
use tracing::info;

use crate::{KaizenError, Result};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BestPractice {
    pub id: String,
    pub title: String,
    pub description: String,
    pub category: PracticeCategory,
    pub confidence: f64,
    pub evidence_count: usize,
    pub success_rate: f64,
    pub steps: Vec<PracticeStep>,
    pub examples: Vec<Example>,
    pub identified_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PracticeCategory {
    /// Ontology design patterns
    OntologyDesign,
    /// SPARQL query patterns
    QueryPattern,
    /// Naming conventions
    NamingConvention,
    /// Validation rules
    Validation,
    /// Performance optimization
    Performance,
    /// Documentation standards
    Documentation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PracticeStep {
    pub step_number: usize,
    pub action: String,
    pub rationale: String,
    pub expected_outcome: String,
    pub common_pitfalls: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Example {
    pub title: String,
    pub description: String,
    pub before: Option<String>,
    pub after: String,
    pub context: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StandardWorkDocument {
    pub id: String,
    pub title: String,
    pub purpose: String,
    pub scope: String,
    pub best_practices: Vec<BestPractice>,
    pub procedures: Vec<Procedure>,
    pub decision_trees: Vec<DecisionTree>,
    pub glossary: HashMap<String, String>,
    pub generated_at: DateTime<Utc>,
    pub version: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Procedure {
    pub name: String,
    pub description: String,
    pub when_to_use: String,
    pub steps: Vec<String>,
    pub prerequisites: Vec<String>,
    pub quality_checks: Vec<QualityCheck>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityCheck {
    pub name: String,
    pub description: String,
    pub acceptance_criteria: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecisionTree {
    pub title: String,
    pub root_question: String,
    pub nodes: Vec<DecisionNode>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecisionNode {
    pub id: String,
    pub question: String,
    pub options: Vec<DecisionOption>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DecisionOption {
    pub label: String,
    pub next_node: Option<String>,
    pub recommendation: Option<String>,
}

pub struct StandardWorkGenerator {
    best_practices: HashMap<String, BestPractice>,
    templates: Tera,
}

impl StandardWorkGenerator {
    pub fn new() -> Result<Self> {
        let mut templates = Tera::default();

        // Add standard work templates
        templates
            .add_raw_template(
                "standard_work.md",
                include_str!("templates/standard_work.md.tera"),
            )
            .map_err(|e| KaizenError::DocumentationError(e.to_string()))?;

        templates
            .add_raw_template(
                "best_practice.md",
                include_str!("templates/best_practice.md.tera"),
            )
            .map_err(|e| KaizenError::DocumentationError(e.to_string()))?;

        Ok(Self {
            best_practices: HashMap::new(),
            templates,
        })
    }

    /// Extract best practices from usage patterns and feedback
    pub fn extract_best_practice(
        &mut self,
        title: String,
        category: PracticeCategory,
        evidence_count: usize,
        success_rate: f64,
    ) -> BestPractice {
        let confidence = self.calculate_confidence(evidence_count, success_rate);

        let steps = self.generate_steps_for_category(&category);
        let examples = self.generate_examples_for_category(&category);

        let practice = BestPractice {
            id: uuid::Uuid::new_v4().to_string(),
            title: title.clone(),
            description: format!("Best practice for {}", title),
            category,
            confidence,
            evidence_count,
            success_rate,
            steps,
            examples,
            identified_at: Utc::now(),
        };

        info!(
            practice_id = %practice.id,
            category = ?practice.category,
            confidence = practice.confidence,
            "Extracted best practice"
        );

        self.best_practices.insert(practice.id.clone(), practice.clone());
        practice
    }

    /// Generate standard work documentation from best practices
    pub fn generate_documentation(
        &self,
        title: String,
        purpose: String,
        scope: String,
    ) -> Result<StandardWorkDocument> {
        let best_practices: Vec<BestPractice> =
            self.best_practices.values().cloned().collect();

        let procedures = self.generate_procedures(&best_practices);
        let decision_trees = self.generate_decision_trees(&best_practices);
        let glossary = self.generate_glossary(&best_practices);

        Ok(StandardWorkDocument {
            id: uuid::Uuid::new_v4().to_string(),
            title,
            purpose,
            scope,
            best_practices,
            procedures,
            decision_trees,
            glossary,
            generated_at: Utc::now(),
            version: "1.0.0".to_string(),
        })
    }

    /// Render standard work documentation to markdown
    pub fn render_to_markdown(&self, document: &StandardWorkDocument) -> Result<String> {
        let mut context = Context::new();
        context.insert("document", document);

        self.templates
            .render("standard_work.md", &context)
            .map_err(|e| KaizenError::DocumentationError(e.to_string()))
    }

    /// Render a single best practice to markdown
    pub fn render_best_practice(&self, practice: &BestPractice) -> Result<String> {
        let mut context = Context::new();
        context.insert("practice", practice);

        self.templates
            .render("best_practice.md", &context)
            .map_err(|e| KaizenError::DocumentationError(e.to_string()))
    }

    fn calculate_confidence(&self, evidence_count: usize, success_rate: f64) -> f64 {
        // Confidence increases with evidence count and success rate
        let evidence_factor = (evidence_count as f64 / 100.0).min(1.0);
        let confidence = (success_rate * 0.7 + evidence_factor * 0.3).min(1.0);
        confidence
    }

    fn generate_steps_for_category(&self, category: &PracticeCategory) -> Vec<PracticeStep> {
        match category {
            PracticeCategory::OntologyDesign => vec![
                PracticeStep {
                    step_number: 1,
                    action: "Identify domain concepts".to_string(),
                    rationale: "Clear concept identification ensures semantic clarity".to_string(),
                    expected_outcome: "List of core domain concepts".to_string(),
                    common_pitfalls: vec!["Over-abstraction".to_string(), "Ambiguous naming".to_string()],
                },
                PracticeStep {
                    step_number: 2,
                    action: "Define class hierarchy".to_string(),
                    rationale: "Proper hierarchy enables inference and reasoning".to_string(),
                    expected_outcome: "Well-structured class taxonomy".to_string(),
                    common_pitfalls: vec!["Deep hierarchies".to_string(), "Multiple inheritance conflicts".to_string()],
                },
                PracticeStep {
                    step_number: 3,
                    action: "Specify properties and constraints".to_string(),
                    rationale: "Constraints ensure data quality and consistency".to_string(),
                    expected_outcome: "Fully specified properties with SHACL constraints".to_string(),
                    common_pitfalls: vec!["Over-constraining".to_string(), "Missing cardinality".to_string()],
                },
            ],

            PracticeCategory::QueryPattern => vec![
                PracticeStep {
                    step_number: 1,
                    action: "Identify query pattern".to_string(),
                    rationale: "Reusable patterns improve maintainability".to_string(),
                    expected_outcome: "Documented query pattern".to_string(),
                    common_pitfalls: vec!["Complex joins".to_string(), "Cartesian products".to_string()],
                },
                PracticeStep {
                    step_number: 2,
                    action: "Optimize query structure".to_string(),
                    rationale: "Efficient queries improve system performance".to_string(),
                    expected_outcome: "Optimized SPARQL query".to_string(),
                    common_pitfalls: vec!["Missing FILTER placement".to_string(), "Unnecessary OPTIONAL".to_string()],
                },
            ],

            PracticeCategory::NamingConvention => vec![
                PracticeStep {
                    step_number: 1,
                    action: "Apply consistent naming style".to_string(),
                    rationale: "Consistency improves readability and maintainability".to_string(),
                    expected_outcome: "Uniformly named elements".to_string(),
                    common_pitfalls: vec!["Mixed case styles".to_string(), "Unclear abbreviations".to_string()],
                },
            ],

            PracticeCategory::Validation => vec![
                PracticeStep {
                    step_number: 1,
                    action: "Define SHACL shapes".to_string(),
                    rationale: "Validation shapes ensure data quality".to_string(),
                    expected_outcome: "Comprehensive validation rules".to_string(),
                    common_pitfalls: vec!["Over-validation".to_string(), "Missing edge cases".to_string()],
                },
            ],

            PracticeCategory::Performance => vec![
                PracticeStep {
                    step_number: 1,
                    action: "Profile query performance".to_string(),
                    rationale: "Measurement enables optimization".to_string(),
                    expected_outcome: "Performance baseline".to_string(),
                    common_pitfalls: vec!["Premature optimization".to_string()],
                },
            ],

            PracticeCategory::Documentation => vec![
                PracticeStep {
                    step_number: 1,
                    action: "Document intent and usage".to_string(),
                    rationale: "Clear documentation enables correct usage".to_string(),
                    expected_outcome: "Comprehensive element documentation".to_string(),
                    common_pitfalls: vec!["Outdated documentation".to_string()],
                },
            ],
        }
    }

    fn generate_examples_for_category(&self, category: &PracticeCategory) -> Vec<Example> {
        match category {
            PracticeCategory::OntologyDesign => vec![Example {
                title: "Class Hierarchy Example".to_string(),
                description: "Proper class hierarchy design".to_string(),
                before: Some("Flat structure with no relationships".to_string()),
                after: "Hierarchical structure with clear parent-child relationships".to_string(),
                context: "E-commerce product catalog".to_string(),
            }],

            PracticeCategory::QueryPattern => vec![Example {
                title: "Optimized Query Pattern".to_string(),
                description: "Efficient SPARQL query structure".to_string(),
                before: Some("SELECT * WHERE { ?s ?p ?o . FILTER(?p = ex:name) }".to_string()),
                after: "SELECT ?s ?name WHERE { ?s ex:name ?name }".to_string(),
                context: "Retrieving entity names".to_string(),
            }],

            _ => vec![],
        }
    }

    fn generate_procedures(&self, _practices: &[BestPractice]) -> Vec<Procedure> {
        vec![
            Procedure {
                name: "Ontology Review".to_string(),
                description: "Standard procedure for reviewing ontology changes".to_string(),
                when_to_use: "Before committing ontology modifications".to_string(),
                steps: vec![
                    "Run SHACL validation".to_string(),
                    "Check naming conventions".to_string(),
                    "Verify no breaking changes".to_string(),
                    "Update documentation".to_string(),
                ],
                prerequisites: vec!["Completed ontology changes".to_string()],
                quality_checks: vec![
                    QualityCheck {
                        name: "Validation Passed".to_string(),
                        description: "All SHACL constraints satisfied".to_string(),
                        acceptance_criteria: "Zero validation errors".to_string(),
                    },
                ],
            },
        ]
    }

    fn generate_decision_trees(&self, _practices: &[BestPractice]) -> Vec<DecisionTree> {
        vec![DecisionTree {
            title: "When to Create a New Class".to_string(),
            root_question: "Is this concept distinct from existing classes?".to_string(),
            nodes: vec![
                DecisionNode {
                    id: "node1".to_string(),
                    question: "Is this concept distinct from existing classes?".to_string(),
                    options: vec![
                        DecisionOption {
                            label: "Yes".to_string(),
                            next_node: Some("node2".to_string()),
                            recommendation: None,
                        },
                        DecisionOption {
                            label: "No".to_string(),
                            next_node: None,
                            recommendation: Some("Use existing class or property".to_string()),
                        },
                    ],
                },
            ],
        }]
    }

    fn generate_glossary(&self, _practices: &[BestPractice]) -> HashMap<String, String> {
        let mut glossary = HashMap::new();
        glossary.insert(
            "SHACL".to_string(),
            "Shapes Constraint Language - RDF validation framework".to_string(),
        );
        glossary.insert(
            "SPARQL".to_string(),
            "RDF query language".to_string(),
        );
        glossary.insert(
            "Ontology".to_string(),
            "Formal representation of knowledge domain".to_string(),
        );
        glossary
    }
}

impl Default for StandardWorkGenerator {
    fn default() -> Self {
        Self::new().expect("Failed to initialize StandardWorkGenerator")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_confidence_calculation() {
        let generator = StandardWorkGenerator::new().unwrap();

        let confidence1 = generator.calculate_confidence(100, 0.9);
        assert!(confidence1 > 0.8);

        let confidence2 = generator.calculate_confidence(10, 0.5);
        assert!(confidence2 < confidence1);
    }

    #[test]
    fn test_best_practice_extraction() {
        let mut generator = StandardWorkGenerator::new().unwrap();

        let practice = generator.extract_best_practice(
            "Use PascalCase for classes".to_string(),
            PracticeCategory::NamingConvention,
            50,
            0.95,
        );

        assert_eq!(practice.category, PracticeCategory::NamingConvention);
        assert!(practice.confidence > 0.7);
    }

    #[test]
    fn test_documentation_generation() {
        let generator = StandardWorkGenerator::new().unwrap();

        let doc = generator
            .generate_documentation(
                "Ontology Development Guide".to_string(),
                "Standardize ontology development practices".to_string(),
                "All ontology development activities".to_string(),
            )
            .unwrap();

        assert_eq!(doc.title, "Ontology Development Guide");
        assert!(!doc.procedures.is_empty());
    }
}
