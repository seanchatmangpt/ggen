//! Template transformation risk analysis with automated RPN calculation
//!
//! Analyzes template changes and transformations for:
//! - Breaking template syntax changes
//! - Variable dependency risks
//! - File generation failures
//! - Automated RPN calculation for each risk

use super::scoring::{
    ChangeFrequency, DetectionMetrics, ImpactMetrics, OccurrenceMetrics, SodScorer, UserImpact,
};
use super::types::{
    FailureMode, FailureModeCategory, ImpactTarget, MitigationCost, MitigationStatus,
    MitigationStrategy,
};
use crate::template::Template;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Template risk analyzer with automated RPN calculation
pub struct TemplateRiskAnalyzer {
    scorer: SodScorer,
}

impl TemplateRiskAnalyzer {
    /// Create a new template risk analyzer
    pub fn new() -> Self {
        Self {
            scorer: SodScorer::new(),
        }
    }

    /// Analyze a template for risks
    pub fn analyze_template(&self, template: &Template) -> Result<TemplateRisk> {
        let changes = self.detect_template_changes(template)?;
        let dependencies = self.extract_variable_dependencies(template)?;
        let complexity = self.calculate_template_complexity(template);

        let mut risks = Vec::new();

        // Analyze each change type
        for change in &changes {
            if let Some(risk) = self.analyze_change(change, &dependencies, complexity) {
                risks.push(risk);
            }
        }

        // Calculate overall risk
        let overall_sod = self.calculate_overall_risk(&risks, complexity);

        Ok(TemplateRisk {
            template_name: "template".to_string(),
            changes,
            dependencies,
            complexity_score: complexity,
            risks,
            severity: overall_sod.severity,
            occurrence: overall_sod.occurrence,
            detection: overall_sod.detection,
            rpn: overall_sod.rpn,
        })
    }

    /// Detect changes in template
    fn detect_template_changes(&self, template: &Template) -> Result<Vec<TemplateChange>> {
        let mut changes = Vec::new();

        // Check for potentially risky patterns in template body
        if template.body.contains("{% include") {
            changes.push(TemplateChange {
                change_type: TemplateChangeType::IncludeAdded,
                description: "Template includes another template".to_string(),
                affected_elements: vec!["template_body".to_string()],
            });
        }

        if template.body.contains("{% for") {
            changes.push(TemplateChange {
                change_type: TemplateChangeType::LoopAdded,
                description: "Template contains loop construct".to_string(),
                affected_elements: vec!["template_body".to_string()],
            });
        }

        if template.body.contains("{% if") {
            changes.push(TemplateChange {
                change_type: TemplateChangeType::ConditionalAdded,
                description: "Template contains conditional logic".to_string(),
                affected_elements: vec!["template_body".to_string()],
            });
        }

        Ok(changes)
    }

    /// Extract variable dependencies from template
    fn extract_variable_dependencies(&self, template: &Template) -> Result<Vec<VariableDependency>> {
        let mut dependencies = Vec::new();
        let mut seen = HashSet::new();

        // Simple regex-like pattern matching for {{ variable }}
        let body = &template.body;
        let mut start = 0;

        while let Some(open_pos) = body[start..].find("{{") {
            let open_idx = start + open_pos;
            if let Some(close_pos) = body[open_idx..].find("}}") {
                let var_content = &body[open_idx + 2..open_idx + close_pos];
                let var_name = var_content.trim().split_whitespace().next().unwrap_or("");

                if !var_name.is_empty() && !seen.contains(var_name) {
                    seen.insert(var_name.to_string());
                    dependencies.push(VariableDependency {
                        variable_name: var_name.to_string(),
                        required: true, // Assume required unless in optional block
                        default_value: None,
                    });
                }

                start = open_idx + close_pos + 2;
            } else {
                break;
            }
        }

        Ok(dependencies)
    }

    /// Calculate template complexity (0.0-1.0)
    fn calculate_template_complexity(&self, template: &Template) -> f64 {
        let mut score = 0.0;

        // Base complexity from template size
        let lines = template.body.lines().count();
        score += (lines as f64 / 100.0).min(0.2);

        // Includes add complexity
        let includes = template.body.matches("{% include").count();
        score += (includes as f64) * 0.1;

        // Loops add significant complexity
        let loops = template.body.matches("{% for").count();
        score += (loops as f64) * 0.15;

        // Conditionals add complexity
        let conditionals = template.body.matches("{% if").count();
        score += (conditionals as f64) * 0.1;

        // Nested structures
        let nesting_level = self.estimate_nesting_level(&template.body);
        score += (nesting_level as f64) * 0.1;

        score.min(1.0)
    }

    /// Estimate nesting level in template
    fn estimate_nesting_level(&self, body: &str) -> usize {
        let mut max_depth = 0;
        let mut current_depth = 0;

        for line in body.lines() {
            if line.contains("{% for") || line.contains("{% if") {
                current_depth += 1;
                max_depth = max_depth.max(current_depth);
            } else if line.contains("{% endfor") || line.contains("{% endif") {
                current_depth = current_depth.saturating_sub(1);
            }
        }

        max_depth
    }

    /// Analyze a specific change
    fn analyze_change(
        &self,
        change: &TemplateChange,
        dependencies: &[VariableDependency],
        complexity: f64,
    ) -> Option<ChangeRisk> {
        let (impact, occurrence, detection) = match &change.change_type {
            TemplateChangeType::VariableAdded | TemplateChangeType::VariableRemoved => (
                ImpactMetrics {
                    breaking_change: true,
                    affected_systems: 1,
                    user_impact: UserImpact::Moderate,
                    ..Default::default()
                },
                OccurrenceMetrics {
                    change_frequency: ChangeFrequency::Frequent,
                    complexity_score: 0.4,
                    ..Default::default()
                },
                DetectionMetrics {
                    test_coverage: 0.6,
                    has_static_analysis: true,
                    ..Default::default()
                },
            ),
            TemplateChangeType::IncludeAdded => (
                ImpactMetrics {
                    breaking_change: false,
                    affected_systems: 1,
                    user_impact: UserImpact::Minimal,
                    ..Default::default()
                },
                OccurrenceMetrics {
                    change_frequency: ChangeFrequency::Occasional,
                    complexity_score: complexity,
                    ..Default::default()
                },
                DetectionMetrics {
                    test_coverage: 0.5,
                    has_static_analysis: true,
                    requires_manual_review: true,
                    ..Default::default()
                },
            ),
            TemplateChangeType::LoopAdded => (
                ImpactMetrics {
                    breaking_change: false,
                    affected_systems: 1,
                    user_impact: UserImpact::Moderate,
                    ..Default::default()
                },
                OccurrenceMetrics {
                    change_frequency: ChangeFrequency::Frequent,
                    complexity_score: complexity,
                    ..Default::default()
                },
                DetectionMetrics {
                    test_coverage: 0.4,
                    has_static_analysis: false,
                    requires_manual_review: true,
                    ..Default::default()
                },
            ),
            TemplateChangeType::ConditionalAdded => (
                ImpactMetrics {
                    breaking_change: false,
                    affected_systems: 1,
                    user_impact: UserImpact::Minimal,
                    ..Default::default()
                },
                OccurrenceMetrics {
                    change_frequency: ChangeFrequency::Frequent,
                    complexity_score: complexity,
                    ..Default::default()
                },
                DetectionMetrics {
                    test_coverage: 0.5,
                    has_static_analysis: true,
                    ..Default::default()
                },
            ),
            _ => return None,
        };

        let sod = self.scorer.calculate_sod(&impact, &occurrence, &detection);

        Some(ChangeRisk {
            change_type: change.change_type.clone(),
            description: change.description.clone(),
            severity: sod.severity,
            occurrence: sod.occurrence,
            detection: sod.detection,
            rpn: sod.rpn,
        })
    }

    /// Calculate overall risk for template
    fn calculate_overall_risk(
        &self,
        risks: &[ChangeRisk],
        complexity: f64,
    ) -> super::scoring::SodScore {
        if risks.is_empty() {
            // No specific risks, base on complexity
            let impact = ImpactMetrics {
                user_impact: if complexity > 0.7 {
                    UserImpact::Moderate
                } else {
                    UserImpact::Minimal
                },
                ..Default::default()
            };

            let occurrence = OccurrenceMetrics {
                complexity_score: complexity,
                change_frequency: ChangeFrequency::Occasional,
                ..Default::default()
            };

            let detection = DetectionMetrics {
                test_coverage: 0.5,
                has_static_analysis: true,
                ..Default::default()
            };

            return self.scorer.calculate_sod(&impact, &occurrence, &detection);
        }

        // Use highest RPN from individual risks
        let max_risk = risks.iter().max_by_key(|r| r.rpn).unwrap();

        super::scoring::SodScore {
            severity: max_risk.severity,
            occurrence: max_risk.occurrence,
            detection: max_risk.detection,
            rpn: max_risk.rpn,
        }
    }

    /// Convert template risk to failure mode
    pub fn template_risk_to_failure_mode(&self, risk: &TemplateRisk) -> FailureMode {
        let mut fm = FailureMode::new(
            format!("TEMPLATE-{}", risk.template_name),
            FailureModeCategory::TemplateTransformation,
            format!(
                "Template transformation risk (complexity: {:.2})",
                risk.complexity_score
            ),
            risk.severity,
            risk.occurrence,
            risk.detection,
        );

        // Add change risks as effects
        for change_risk in &risk.risks {
            fm = fm.add_effect(format!(
                "{} (RPN: {})",
                change_risk.description,
                change_risk.rpn.value()
            ));
        }

        // Add dependency risks
        for dep in &risk.dependencies {
            if dep.required && dep.default_value.is_none() {
                fm = fm.add_effect(format!(
                    "Missing required variable: {}",
                    dep.variable_name
                ));
            }
        }

        // Add mitigations
        if risk.complexity_score > 0.6 {
            fm = fm.add_mitigation(MitigationStrategy {
                description: "Refactor template to reduce complexity".to_string(),
                impact_on: ImpactTarget::Severity,
                reduction: 2,
                status: MitigationStatus::Planned,
                cost: MitigationCost::Medium,
            });
        }

        fm = fm.add_mitigation(MitigationStrategy {
            description: "Add comprehensive template tests".to_string(),
            impact_on: ImpactTarget::Detection,
            reduction: 4,
            status: MitigationStatus::Planned,
            cost: MitigationCost::Low,
        });

        fm = fm.add_mitigation(MitigationStrategy {
            description: "Implement template validation in CI pipeline".to_string(),
            impact_on: ImpactTarget::Detection,
            reduction: 3,
            status: MitigationStatus::Planned,
            cost: MitigationCost::Low,
        });

        fm
    }
}

impl Default for TemplateRiskAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

/// Template risk assessment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateRisk {
    /// Template name/identifier
    pub template_name: String,

    /// Detected changes
    pub changes: Vec<TemplateChange>,

    /// Variable dependencies
    pub dependencies: Vec<VariableDependency>,

    /// Complexity score (0.0-1.0)
    pub complexity_score: f64,

    /// Individual change risks
    pub risks: Vec<ChangeRisk>,

    /// Overall SOD scores
    pub severity: super::types::Severity,
    pub occurrence: super::types::Occurrence,
    pub detection: super::types::Detection,
    pub rpn: super::types::RiskPriorityNumber,
}

/// A detected template change
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateChange {
    /// Type of change
    pub change_type: TemplateChangeType,

    /// Description
    pub description: String,

    /// Affected template elements
    pub affected_elements: Vec<String>,
}

/// Types of template changes
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TemplateChangeType {
    /// Variable added
    VariableAdded,

    /// Variable removed
    VariableRemoved,

    /// Variable renamed
    VariableRenamed,

    /// Include directive added
    IncludeAdded,

    /// Loop construct added
    LoopAdded,

    /// Conditional added
    ConditionalAdded,

    /// File path changed
    FilePathChanged,

    /// Frontmatter modified
    FrontmatterModified,
}

/// Variable dependency in template
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VariableDependency {
    /// Variable name
    pub variable_name: String,

    /// Is this variable required
    pub required: bool,

    /// Default value if any
    pub default_value: Option<String>,
}

/// Risk for a specific change
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChangeRisk {
    /// Type of change
    pub change_type: TemplateChangeType,

    /// Description
    pub description: String,

    /// SOD scores with automated RPN
    pub severity: super::types::Severity,
    pub occurrence: super::types::Occurrence,
    pub detection: super::types::Detection,
    pub rpn: super::types::RiskPriorityNumber,
}

/// Template dependency graph analyzer
pub struct TemplateDependencyAnalyzer {
    /// Template -> Templates it includes
    includes: HashMap<String, HashSet<String>>,

    /// Variable -> Templates that use it
    variable_usage: HashMap<String, HashSet<String>>,
}

impl TemplateDependencyAnalyzer {
    /// Create a new dependency analyzer
    pub fn new() -> Self {
        Self {
            includes: HashMap::new(),
            variable_usage: HashMap::new(),
        }
    }

    /// Add template include relationship
    pub fn add_include(&mut self, template: String, included: String) {
        self.includes
            .entry(template)
            .or_default()
            .insert(included);
    }

    /// Add variable usage
    pub fn add_variable_usage(&mut self, variable: String, template: String) {
        self.variable_usage
            .entry(variable)
            .or_default()
            .insert(template);
    }

    /// Find circular dependencies
    pub fn find_circular_dependencies(&self) -> Vec<Vec<String>> {
        let mut cycles = Vec::new();
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for template in self.includes.keys() {
            if !visited.contains(template) {
                self.dfs_cycle_detection(template, &mut visited, &mut rec_stack, &mut Vec::new(), &mut cycles);
            }
        }

        cycles
    }

    fn dfs_cycle_detection(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
        path: &mut Vec<String>,
        cycles: &mut Vec<Vec<String>>,
    ) {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());
        path.push(node.to_string());

        if let Some(neighbors) = self.includes.get(node) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    self.dfs_cycle_detection(neighbor, visited, rec_stack, path, cycles);
                } else if rec_stack.contains(neighbor) {
                    // Found a cycle
                    if let Some(cycle_start) = path.iter().position(|t| t == neighbor) {
                        cycles.push(path[cycle_start..].to_vec());
                    }
                }
            }
        }

        path.pop();
        rec_stack.remove(node);
    }

    /// Find templates affected by variable change
    pub fn find_affected_templates(&self, variable: &str) -> Vec<String> {
        self.variable_usage
            .get(variable)
            .map(|set| set.iter().cloned().collect())
            .unwrap_or_default()
    }
}

impl Default for TemplateDependencyAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_template_risk_analyzer_creation() {
        let analyzer = TemplateRiskAnalyzer::new();
        assert!(true);
    }

    #[test]
    fn test_complexity_calculation() {
        let analyzer = TemplateRiskAnalyzer::new();

        let simple_template = Template {
            raw_frontmatter: String::new(),
            front: Default::default(),
            body: "Hello {{ name }}".to_string(),
        };

        let complexity = analyzer.calculate_template_complexity(&simple_template);
        assert!(complexity < 0.3);

        let complex_template = Template {
            raw_frontmatter: String::new(),
            front: Default::default(),
            body: r#"
                {% for item in items %}
                    {% if item.active %}
                        {% include "partial.html" %}
                        {{ item.name }}
                    {% endif %}
                {% endfor %}
            "#
            .to_string(),
        };

        let high_complexity = analyzer.calculate_template_complexity(&complex_template);
        assert!(high_complexity > 0.3);
    }

    #[test]
    fn test_variable_extraction() {
        let analyzer = TemplateRiskAnalyzer::new();

        let template = Template {
            raw_frontmatter: String::new(),
            front: Default::default(),
            body: "Hello {{ name }}, your score is {{ score }}".to_string(),
        };

        let deps = analyzer.extract_variable_dependencies(&template).unwrap();
        assert_eq!(deps.len(), 2);
    }

    #[test]
    fn test_circular_dependency_detection() {
        let mut analyzer = TemplateDependencyAnalyzer::new();

        analyzer.add_include("A".to_string(), "B".to_string());
        analyzer.add_include("B".to_string(), "C".to_string());
        analyzer.add_include("C".to_string(), "A".to_string());

        let cycles = analyzer.find_circular_dependencies();
        assert!(!cycles.is_empty());
    }
}
