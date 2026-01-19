//! Deterministic SPARQL query generation
//!
//! Builds SPARQL queries from structured parameters, ensuring
//! the same input always produces the same query string.

use crate::errors::Result;

/// Generates deterministic SPARQL queries
///
/// All methods produce consistent, idempotent query strings.
/// Same input parameters always produce identical output.
///
/// # Examples
///
/// ```
/// use ggen_ontology_core::sparql_generator::SparqlGenerator;
///
/// let query = SparqlGenerator::find_policies_by_jurisdiction("US");
/// assert!(query.contains("SELECT"));
/// ```
pub struct SparqlGenerator;

impl SparqlGenerator {
    /// Generates a SPARQL query to find policies by jurisdiction
    ///
    /// # Arguments
    /// * `jurisdiction` - Jurisdiction identifier (e.g., "US", "EU", "APAC")
    ///
    /// # Returns
    /// A deterministic SPARQL query string
    ///
    /// # Determinism
    /// Same jurisdiction always produces identical query
    pub fn find_policies_by_jurisdiction(jurisdiction: &str) -> String {
        let escaped = escape_sparql_string(jurisdiction);
        format!(
            r#"SELECT ?policy ?label ?description
WHERE {{
  ?policy rdf:type :Policy .
  ?policy :hasJurisdiction ?jurisdiction .
  ?jurisdiction :code "{}" .
  OPTIONAL {{ ?policy rdfs:label ?label . }}
  OPTIONAL {{ ?policy rdfs:comment ?description . }}
}}
ORDER BY ?policy"#,
            escaped
        )
    }

    /// Generates a SPARQL query to find data classifications
    ///
    /// # Arguments
    /// * `classification` - Classification label (e.g., "Public", "Confidential", "Restricted")
    ///
    /// # Returns
    /// A deterministic SPARQL query string
    pub fn find_data_classifications(classification: &str) -> String {
        let escaped = escape_sparql_string(classification);
        format!(
            r#"SELECT ?class ?label ?level
WHERE {{
  ?class rdf:type :DataClassification .
  ?class :classificationLevel ?level .
  ?class rdfs:label "{}" .
  OPTIONAL {{ ?class rdfs:comment ?description . }}
}}
ORDER BY ?level"#,
            escaped
        )
    }

    /// Generates a SPARQL query to find services by SLA
    ///
    /// # Arguments
    /// * `min_availability` - Minimum availability percentage (0.0-1.0)
    ///
    /// # Returns
    /// A deterministic SPARQL query string
    pub fn find_services_by_sla(min_availability: f32) -> String {
        let availability = (min_availability * 100.0) as u32;
        format!(
            r#"SELECT ?service ?label ?availability
WHERE {{
  ?service rdf:type :ServiceLevelAgreement .
  ?service :availabilityPercentage ?availability .
  FILTER (?availability >= {})
  OPTIONAL {{ ?service rdfs:label ?label . }}
}}
ORDER BY DESC(?availability)"#,
            availability
        )
    }

    /// Generates a SPARQL query to find security controls
    ///
    /// # Arguments
    /// * `control_type` - Control type (e.g., "Authentication", "Encryption", "Access_Control")
    ///
    /// # Returns
    /// A deterministic SPARQL query string
    pub fn find_security_controls(control_type: &str) -> String {
        let escaped = escape_sparql_string(control_type);
        format!(
            r#"SELECT ?control ?label ?implementation
WHERE {{
  ?control rdf:type :SecurityControl .
  ?control :controlType "{}" .
  OPTIONAL {{ ?control rdfs:label ?label . }}
  OPTIONAL {{ ?control :implementationMethod ?implementation . }}
}}
ORDER BY ?label"#,
            escaped
        )
    }

    /// Generates a SPARQL query to find compute services by type
    ///
    /// # Arguments
    /// * `compute_type` - Compute type (e.g., "VM", "Container", "Serverless", "Kubernetes")
    ///
    /// # Returns
    /// A deterministic SPARQL query string
    pub fn find_compute_by_type(compute_type: &str) -> String {
        let escaped = escape_sparql_string(compute_type);
        format!(
            r#"SELECT ?compute ?label ?provider ?region
WHERE {{
  ?compute rdf:type :ComputeService .
  ?compute :computeType "{}" .
  ?compute :provider ?provider .
  OPTIONAL {{ ?compute rdfs:label ?label . }}
  OPTIONAL {{ ?compute :deploymentRegion ?region . }}
}}
ORDER BY ?provider ?compute"#,
            escaped
        )
    }

    /// Generates a generic SELECT query with constraints
    ///
    /// # Arguments
    /// * `variables` - Variables to select
    /// * `class_type` - RDF class to query
    /// * `filters` - Optional filter constraints
    ///
    /// # Returns
    /// A deterministic SPARQL query string
    pub fn select_with_filters(
        variables: &[&str],
        class_type: &str,
        filters: &[(String, String)],
    ) -> String {
        let var_list = variables.join(" ?");
        let escaped_class = escape_sparql_string(class_type);

        let mut where_clause = format!("  ?instance rdf:type {} .", escaped_class);

        // Sort filters for determinism
        let mut sorted_filters: Vec<_> = filters.to_vec();
        sorted_filters.sort_by(|a, b| a.0.cmp(&b.0));

        for (_var, condition) in sorted_filters {
            where_clause.push('\n');
            where_clause.push_str(&format!("  FILTER ({})", condition));
        }

        format!(
            r#"SELECT ?instance ?{}
WHERE {{
{}
}}
ORDER BY ?instance"#,
            var_list, where_clause
        )
    }
}

/// Escapes special characters in SPARQL string literals
///
/// Ensures safe embedding of user input in SPARQL queries
fn escape_sparql_string(input: &str) -> String {
    input
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_policies_by_jurisdiction_determinism() {
        let query1 = SparqlGenerator::find_policies_by_jurisdiction("US");
        let query2 = SparqlGenerator::find_policies_by_jurisdiction("US");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_policies_by_jurisdiction_contains_code() {
        let query = SparqlGenerator::find_policies_by_jurisdiction("EU");
        assert!(query.contains("EU"));
        assert!(query.contains("SELECT"));
        assert!(query.contains("Policy"));
    }

    #[test]
    fn test_find_data_classifications_determinism() {
        let query1 = SparqlGenerator::find_data_classifications("Confidential");
        let query2 = SparqlGenerator::find_data_classifications("Confidential");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_data_classifications_contains_label() {
        let query = SparqlGenerator::find_data_classifications("Public");
        assert!(query.contains("Public"));
        assert!(query.contains("DataClassification"));
    }

    #[test]
    fn test_find_services_by_sla_determinism() {
        let query1 = SparqlGenerator::find_services_by_sla(0.99);
        let query2 = SparqlGenerator::find_services_by_sla(0.99);
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_services_by_sla_contains_availability() {
        let query = SparqlGenerator::find_services_by_sla(0.95);
        assert!(query.contains("95"));
        assert!(query.contains("ServiceLevelAgreement"));
    }

    #[test]
    fn test_find_security_controls_determinism() {
        let query1 = SparqlGenerator::find_security_controls("Encryption");
        let query2 = SparqlGenerator::find_security_controls("Encryption");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_compute_by_type_determinism() {
        let query1 = SparqlGenerator::find_compute_by_type("Kubernetes");
        let query2 = SparqlGenerator::find_compute_by_type("Kubernetes");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_escape_sparql_string() {
        let input = r#"Test "quoted" and \escaped\ and newline
end"#;
        let escaped = escape_sparql_string(input);
        assert!(escaped.contains("\\\""));
        assert!(escaped.contains("\\\\"));
        assert!(escaped.contains("\\n"));
    }

    #[test]
    fn test_select_with_filters_determinism() {
        let filters = vec![
            ("availability".to_string(), "?avail > 99".to_string()),
            ("cost".to_string(), "?cost < 1000".to_string()),
        ];
        let query1 = SparqlGenerator::select_with_filters(&["?label", "?cost"], "Service", &filters);
        let query2 = SparqlGenerator::select_with_filters(&["?label", "?cost"], "Service", &filters);
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_select_with_filters_sorts_deterministically() {
        let filters1 = vec![
            ("z_filter".to_string(), "z".to_string()),
            ("a_filter".to_string(), "a".to_string()),
        ];
        let filters2 = vec![
            ("a_filter".to_string(), "a".to_string()),
            ("z_filter".to_string(), "z".to_string()),
        ];
        let query1 = SparqlGenerator::select_with_filters(&["?x"], "Class", &filters1);
        let query2 = SparqlGenerator::select_with_filters(&["?x"], "Class", &filters2);
        assert_eq!(query1, query2);
    }
}
