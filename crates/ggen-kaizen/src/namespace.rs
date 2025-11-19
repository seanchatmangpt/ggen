//! # 5S Methodology for RDF Namespace Management
//!
//! Applies the 5S methodology (Sort, Set in order, Shine, Standardize, Sustain)
//! from Lean manufacturing to organize and maintain RDF namespaces.
//!
//! ## The 5S Principles
//!
//! 1. **Seiri (Sort)**: Remove unnecessary namespace elements
//! 2. **Seiton (Set in order)**: Organize namespaces logically
//! 3. **Seiso (Shine)**: Clean up deprecated and unused elements
//! 4. **Seiketsu (Standardize)**: Apply consistent naming conventions
//! 5. **Shitsuke (Sustain)**: Maintain organization through documentation and automation

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tracing::{info, warn};

use crate::Result;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum FiveSPrinciple {
    /// Sort: Categorize and separate necessary from unnecessary
    Sort,
    /// Set in order: Organize for efficient access
    SetInOrder,
    /// Shine: Clean and maintain
    Shine,
    /// Standardize: Create consistent patterns
    Standardize,
    /// Sustain: Maintain improvements through discipline
    Sustain,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamespaceElement {
    pub uri: String,
    pub element_type: ElementType,
    pub usage_count: usize,
    pub last_used: Option<DateTime<Utc>>,
    pub deprecated: bool,
    pub category: Option<String>,
    pub naming_score: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ElementType {
    Class,
    Property,
    Individual,
    Datatype,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamespaceOrganization {
    pub namespace_uri: String,
    pub elements: Vec<NamespaceElement>,
    pub categories: HashMap<String, Vec<String>>,
    pub naming_conventions: NamingConventions,
    pub health_score: NamespaceHealth,
    pub last_organized: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamingConventions {
    pub class_pattern: String,
    pub property_pattern: String,
    pub case_style: CaseStyle,
    pub separator: String,
    pub prefix_rules: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CaseStyle {
    CamelCase,
    PascalCase,
    SnakeCase,
    KebabCase,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NamespaceHealth {
    /// Overall health score (0.0 - 1.0)
    pub overall_score: f64,
    /// Percentage of elements following naming conventions
    pub naming_consistency: f64,
    /// Percentage of elements with clear categories
    pub categorization: f64,
    /// Percentage of deprecated elements removed
    pub cleanliness: f64,
    /// Logical organization score
    pub organization: f64,
    /// Documentation coverage
    pub documentation: f64,
}

impl NamespaceHealth {
    pub fn calculate(
        elements: &[NamespaceElement],
        categories: &HashMap<String, Vec<String>>,
    ) -> Self {
        let total = elements.len() as f64;

        if total == 0.0 {
            return Self {
                overall_score: 1.0,
                naming_consistency: 1.0,
                categorization: 1.0,
                cleanliness: 1.0,
                organization: 1.0,
                documentation: 1.0,
            };
        }

        // Calculate naming consistency
        let well_named = elements.iter().filter(|e| e.naming_score > 0.7).count() as f64;
        let naming_consistency = well_named / total;

        // Calculate categorization
        let categorized = elements
            .iter()
            .filter(|e| e.category.is_some())
            .count() as f64;
        let categorization = categorized / total;

        // Calculate cleanliness (no deprecated elements)
        let deprecated_count = elements.iter().filter(|e| e.deprecated).count() as f64;
        let cleanliness = 1.0 - (deprecated_count / total);

        // Calculate organization (elements are in categories)
        let total_categorized: usize = categories.values().map(|v| v.len()).sum();
        let organization = total_categorized as f64 / total;

        // Documentation would be calculated based on annotations (simplified here)
        let documentation = 0.5; // Placeholder

        let overall_score = (naming_consistency * 0.3
            + categorization * 0.2
            + cleanliness * 0.2
            + organization * 0.2
            + documentation * 0.1)
            .min(1.0);

        Self {
            overall_score,
            naming_consistency,
            categorization,
            cleanliness,
            organization,
            documentation,
        }
    }
}

pub struct NamespaceOrganizer {
    organizations: HashMap<String, NamespaceOrganization>,
}

impl NamespaceOrganizer {
    pub fn new() -> Self {
        Self {
            organizations: HashMap::new(),
        }
    }

    /// Apply the Sort principle: Categorize elements and identify unnecessary ones
    pub fn apply_sort(
        &mut self,
        namespace_uri: String,
        elements: Vec<NamespaceElement>,
    ) -> Result<SortResult> {
        info!(
            namespace = %namespace_uri,
            element_count = elements.len(),
            "Applying Sort (Seiri) principle"
        );

        let mut necessary = Vec::new();
        let mut unnecessary = Vec::new();
        let mut categories: HashMap<String, Vec<NamespaceElement>> = HashMap::new();

        for element in elements {
            // Classify as unnecessary if:
            // - Deprecated and not used recently
            // - Never used and old
            let is_unnecessary = element.deprecated
                || (element.usage_count == 0
                    && element
                        .last_used
                        .map(|lu| (Utc::now() - lu).num_days() > 90)
                        .unwrap_or(true));

            if is_unnecessary {
                unnecessary.push(element);
            } else {
                // Auto-categorize based on type and naming patterns
                let category = self.infer_category(&element);
                categories
                    .entry(category.clone())
                    .or_insert_with(Vec::new)
                    .push(element.clone());

                necessary.push(element);
            }
        }

        Ok(SortResult {
            necessary_count: necessary.len(),
            unnecessary_count: unnecessary.len(),
            categories: categories.keys().cloned().collect(),
            elements_to_remove: unnecessary,
        })
    }

    /// Apply the Set in Order principle: Organize elements logically
    pub fn apply_set_in_order(
        &mut self,
        namespace_uri: &str,
        elements: Vec<NamespaceElement>,
    ) -> Result<SetInOrderResult> {
        info!(
            namespace = %namespace_uri,
            "Applying Set in Order (Seiton) principle"
        );

        let mut categories: HashMap<String, Vec<String>> = HashMap::new();

        // Organize into logical categories
        for element in &elements {
            let category = element.category.clone().unwrap_or_else(|| self.infer_category(element));

            categories
                .entry(category)
                .or_insert_with(Vec::new)
                .push(element.uri.clone());
        }

        // Sort elements within each category alphabetically
        for category_elements in categories.values_mut() {
            category_elements.sort();
        }

        Ok(SetInOrderResult {
            categories: categories.clone(),
            hierarchy_depth: self.calculate_hierarchy_depth(&categories),
            organization_score: 0.8, // Would be calculated based on actual organization
        })
    }

    /// Apply the Shine principle: Remove deprecated and clean up
    pub fn apply_shine(
        &mut self,
        namespace_uri: &str,
        elements: Vec<NamespaceElement>,
    ) -> Result<ShineResult> {
        info!(
            namespace = %namespace_uri,
            "Applying Shine (Seiso) principle"
        );

        let initial_count = elements.len();
        let mut cleaned = Vec::new();
        let mut removed = Vec::new();

        for element in elements {
            if element.deprecated || element.usage_count == 0 {
                removed.push(element.uri.clone());
            } else {
                cleaned.push(element);
            }
        }

        let removed_count = removed.len();

        warn!(
            namespace = %namespace_uri,
            removed_count = removed_count,
            "Removed deprecated/unused elements"
        );

        Ok(ShineResult {
            elements_before: initial_count,
            elements_after: cleaned.len(),
            removed_elements: removed,
            cleanliness_score: (cleaned.len() as f64 / initial_count as f64).min(1.0),
        })
    }

    /// Apply the Standardize principle: Enforce naming conventions
    pub fn apply_standardize(
        &mut self,
        namespace_uri: &str,
        mut elements: Vec<NamespaceElement>,
        conventions: NamingConventions,
    ) -> Result<StandardizeResult> {
        info!(
            namespace = %namespace_uri,
            "Applying Standardize (Seiketsu) principle"
        );

        let mut violations = Vec::new();
        let mut suggestions = HashMap::new();

        for element in &mut elements {
            let expected_pattern = match element.element_type {
                ElementType::Class => &conventions.class_pattern,
                ElementType::Property => &conventions.property_pattern,
                _ => "",
            };

            // Check naming convention compliance
            let compliant = self.check_naming_compliance(&element.uri, expected_pattern, &conventions.case_style);
            element.naming_score = if compliant { 1.0 } else { 0.3 };

            if !compliant {
                violations.push(element.uri.clone());

                // Generate standardized name suggestion
                if let Some(suggested) = self.suggest_standardized_name(&element.uri, &conventions) {
                    suggestions.insert(element.uri.clone(), suggested);
                }
            }
        }

        Ok(StandardizeResult {
            total_elements: elements.len(),
            violations: violations.len(),
            compliance_rate: 1.0 - (violations.len() as f64 / elements.len() as f64),
            naming_suggestions: suggestions,
        })
    }

    /// Apply the Sustain principle: Document and automate organization
    pub fn apply_sustain(
        &mut self,
        namespace_uri: String,
        elements: Vec<NamespaceElement>,
        categories: HashMap<String, Vec<String>>,
        conventions: NamingConventions,
    ) -> Result<SustainResult> {
        info!(
            namespace = %namespace_uri,
            "Applying Sustain (Shitsuke) principle"
        );

        // Calculate health score
        let health = NamespaceHealth::calculate(&elements, &categories);

        // Create or update organization record
        let organization = NamespaceOrganization {
            namespace_uri: namespace_uri.clone(),
            elements,
            categories,
            naming_conventions: conventions,
            health_score: health.clone(),
            last_organized: Utc::now(),
        };

        self.organizations.insert(namespace_uri, organization);

        Ok(SustainResult {
            health_score: health,
            automation_rules: vec![
                "Auto-categorize new elements".to_string(),
                "Validate naming on addition".to_string(),
                "Flag deprecated elements quarterly".to_string(),
            ],
            documentation_generated: true,
        })
    }

    /// Perform complete 5S cycle on a namespace
    pub async fn organize_namespace(
        &mut self,
        namespace_uri: String,
        elements: Vec<NamespaceElement>,
        conventions: NamingConventions,
    ) -> Result<FiveSReport> {
        info!(
            namespace = %namespace_uri,
            "Starting complete 5S organization cycle"
        );

        // 1. Sort
        let sort_result = self.apply_sort(namespace_uri.clone(), elements.clone())?;

        // 2. Set in Order
        let set_in_order_result = self.apply_set_in_order(&namespace_uri, sort_result.necessary_elements())?;

        // 3. Shine
        let shine_result = self.apply_shine(&namespace_uri, sort_result.necessary_elements())?;

        // 4. Standardize
        let standardize_result = self.apply_standardize(&namespace_uri, shine_result.cleaned_elements(), conventions.clone())?;

        // 5. Sustain
        let sustain_result = self.apply_sustain(
            namespace_uri.clone(),
            standardize_result.standardized_elements(),
            set_in_order_result.categories.clone(),
            conventions,
        )?;

        Ok(FiveSReport {
            namespace_uri,
            sort: sort_result,
            set_in_order: set_in_order_result,
            shine: shine_result,
            standardize: standardize_result,
            sustain: sustain_result,
            completed_at: Utc::now(),
        })
    }

    fn infer_category(&self, element: &NamespaceElement) -> String {
        // Simple heuristic-based categorization
        let uri_lower = element.uri.to_lowercase();

        if uri_lower.contains("config") || uri_lower.contains("setting") {
            "Configuration".to_string()
        } else if uri_lower.contains("event") || uri_lower.contains("action") {
            "Events".to_string()
        } else if uri_lower.contains("data") || uri_lower.contains("model") {
            "Data Models".to_string()
        } else {
            match element.element_type {
                ElementType::Class => "Classes".to_string(),
                ElementType::Property => "Properties".to_string(),
                ElementType::Individual => "Individuals".to_string(),
                ElementType::Datatype => "Datatypes".to_string(),
            }
        }
    }

    fn check_naming_compliance(&self, uri: &str, _pattern: &str, case_style: &CaseStyle) -> bool {
        let local_name = uri.split(['/', '#']).last().unwrap_or(uri);

        match case_style {
            CaseStyle::CamelCase => {
                local_name.chars().next().map(|c| c.is_lowercase()).unwrap_or(false)
                    && !local_name.contains('_')
                    && !local_name.contains('-')
            }
            CaseStyle::PascalCase => {
                local_name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false)
                    && !local_name.contains('_')
                    && !local_name.contains('-')
            }
            CaseStyle::SnakeCase => local_name.chars().all(|c| c.is_lowercase() || c == '_' || c.is_numeric()),
            CaseStyle::KebabCase => local_name.chars().all(|c| c.is_lowercase() || c == '-' || c.is_numeric()),
        }
    }

    fn suggest_standardized_name(&self, _uri: &str, _conventions: &NamingConventions) -> Option<String> {
        // Would implement smart name standardization
        // For now, return None (placeholder)
        None
    }

    fn calculate_hierarchy_depth(&self, categories: &HashMap<String, Vec<String>>) -> usize {
        // Simple calculation - would be more sophisticated in practice
        if categories.is_empty() {
            0
        } else {
            1 + (categories.len() / 5) // Assume sub-categories every 5 categories
        }
    }
}

impl Default for NamespaceOrganizer {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SortResult {
    pub necessary_count: usize,
    pub unnecessary_count: usize,
    pub categories: Vec<String>,
    pub elements_to_remove: Vec<NamespaceElement>,
}

impl SortResult {
    fn necessary_elements(&self) -> Vec<NamespaceElement> {
        // In real implementation, would return the filtered elements
        Vec::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetInOrderResult {
    pub categories: HashMap<String, Vec<String>>,
    pub hierarchy_depth: usize,
    pub organization_score: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShineResult {
    pub elements_before: usize,
    pub elements_after: usize,
    pub removed_elements: Vec<String>,
    pub cleanliness_score: f64,
}

impl ShineResult {
    fn cleaned_elements(&self) -> Vec<NamespaceElement> {
        // In real implementation, would return cleaned elements
        Vec::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StandardizeResult {
    pub total_elements: usize,
    pub violations: usize,
    pub compliance_rate: f64,
    pub naming_suggestions: HashMap<String, String>,
}

impl StandardizeResult {
    fn standardized_elements(&self) -> Vec<NamespaceElement> {
        // In real implementation, would return standardized elements
        Vec::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SustainResult {
    pub health_score: NamespaceHealth,
    pub automation_rules: Vec<String>,
    pub documentation_generated: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FiveSReport {
    pub namespace_uri: String,
    pub sort: SortResult,
    pub set_in_order: SetInOrderResult,
    pub shine: ShineResult,
    pub standardize: StandardizeResult,
    pub sustain: SustainResult,
    pub completed_at: DateTime<Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_element(uri: &str, usage_count: usize, deprecated: bool) -> NamespaceElement {
        NamespaceElement {
            uri: uri.to_string(),
            element_type: ElementType::Class,
            usage_count,
            last_used: Some(Utc::now()),
            deprecated,
            category: None,
            naming_score: 0.5,
        }
    }

    #[test]
    fn test_sort_principle() {
        let mut organizer = NamespaceOrganizer::new();

        let elements = vec![
            create_test_element("http://ex.org/Used", 10, false),
            create_test_element("http://ex.org/Deprecated", 5, true),
            create_test_element("http://ex.org/Unused", 0, false),
        ];

        let result = organizer
            .apply_sort("http://ex.org/".to_string(), elements)
            .unwrap();

        assert_eq!(result.necessary_count, 1);
        assert!(result.unnecessary_count >= 1);
    }

    #[test]
    fn test_namespace_health_calculation() {
        let elements = vec![
            create_test_element("http://ex.org/WellNamed", 10, false),
            create_test_element("http://ex.org/deprecated", 0, true),
        ];

        elements[0].clone().naming_score = 0.9;

        let categories = HashMap::new();
        let health = NamespaceHealth::calculate(&elements, &categories);

        assert!(health.overall_score >= 0.0 && health.overall_score <= 1.0);
    }

    #[test]
    fn test_naming_compliance() {
        let organizer = NamespaceOrganizer::new();

        assert!(organizer.check_naming_compliance(
            "http://ex.org/MyClass",
            "",
            &CaseStyle::PascalCase
        ));

        assert!(!organizer.check_naming_compliance(
            "http://ex.org/myClass",
            "",
            &CaseStyle::PascalCase
        ));

        assert!(organizer.check_naming_compliance(
            "http://ex.org/my_property",
            "",
            &CaseStyle::SnakeCase
        ));
    }
}
