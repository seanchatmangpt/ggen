//! Deterministic SPARQL query generation with proper namespace prefix handling
//!
//! Builds SPARQL queries from structured parameters, ensuring
//! the same input always produces the same query string.
//!
//! All queries include complete PREFIX declarations to ensure
//! namespace resolution at execution time. This prevents undefined
//! prefix errors that would cause queries to fail.
//!
//! # Namespace Mapping
//!
//! - `legal:` - Legal and policy ontology
//! - `it:` - IT and infrastructure ontology
//! - `security:` - Security and compliance ontology
//! - `cloud:` - Cloud services ontology
//! - `rdf:` - RDF standard vocabulary
//! - `rdfs:` - RDF Schema vocabulary
//! - `xsd:` - XML Schema datatypes

/// Generates deterministic SPARQL queries with complete PREFIX declarations
///
/// All methods produce consistent, idempotent query strings with all required
/// namespace prefixes declared at the top of each query.
/// Same input parameters always produce identical output.
///
/// # Examples
///
/// ```
/// use ggen_ontology_core::sparql_generator::SparqlGenerator;
///
/// let query = SparqlGenerator::find_policies_by_jurisdiction("US");
/// assert!(query.contains("@prefix"));
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
    /// A deterministic SPARQL query string with complete PREFIX declarations
    ///
    /// # Determinism
    /// Same jurisdiction always produces identical query with prefixes in
    /// consistent order (alphabetically sorted for determinism).
    pub fn find_policies_by_jurisdiction(jurisdiction: &str) -> String {
        let escaped = escape_sparql_string(jurisdiction);
        format!(
            r#"@prefix legal: <http://example.org/legal/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

SELECT ?policy ?label ?description
WHERE {{
  ?policy rdf:type legal:Policy .
  ?policy legal:hasJurisdiction ?jurisdiction .
  ?jurisdiction legal:code "{}" .
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
    /// A deterministic SPARQL query string with complete PREFIX declarations
    pub fn find_data_classifications(classification: &str) -> String {
        let escaped = escape_sparql_string(classification);
        format!(
            r#"@prefix it: <http://example.org/it/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

SELECT ?class ?label ?level
WHERE {{
  ?class rdf:type it:DataClassification .
  ?class it:classificationLevel ?level .
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
    /// A deterministic SPARQL query string with complete PREFIX declarations
    pub fn find_services_by_sla(min_availability: f32) -> String {
        let availability = (min_availability * 100.0) as u32;
        format!(
            r#"@prefix cloud: <http://example.org/cloud/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

SELECT ?service ?label ?availability
WHERE {{
  ?service rdf:type cloud:ServiceLevelAgreement .
  ?service cloud:availabilityPercentage ?availability .
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
    /// A deterministic SPARQL query string with complete PREFIX declarations
    pub fn find_security_controls(control_type: &str) -> String {
        let escaped = escape_sparql_string(control_type);
        format!(
            r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix security: <http://example.org/security/> .

SELECT ?control ?label ?implementation
WHERE {{
  ?control rdf:type security:SecurityControl .
  ?control security:controlType "{}" .
  OPTIONAL {{ ?control rdfs:label ?label . }}
  OPTIONAL {{ ?control security:implementationMethod ?implementation . }}
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
    /// A deterministic SPARQL query string with complete PREFIX declarations
    pub fn find_compute_by_type(compute_type: &str) -> String {
        let escaped = escape_sparql_string(compute_type);
        format!(
            r#"@prefix cloud: <http://example.org/cloud/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

SELECT ?compute ?label ?provider ?region
WHERE {{
  ?compute rdf:type cloud:ComputeService .
  ?compute cloud:computeType "{}" .
  ?compute cloud:provider ?provider .
  OPTIONAL {{ ?compute rdfs:label ?label . }}
  OPTIONAL {{ ?compute cloud:deploymentRegion ?region . }}
}}
ORDER BY ?provider ?compute"#,
            escaped
        )
    }

    /// Generates a generic SELECT query with constraints
    ///
    /// All queries include PREFIX declarations for rdf: and rdfs: standard
    /// vocabularies. Custom namespace prefixes should be added by the caller
    /// if needed.
    ///
    /// # Arguments
    /// * `variables` - Variables to select
    /// * `class_type` - RDF class to query
    /// * `filters` - Optional filter constraints
    ///
    /// # Returns
    /// A deterministic SPARQL query string with standard PREFIX declarations
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
            r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

SELECT ?instance ?{}
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

    // Helper function to validate SPARQL query structure
    fn assert_query_has_prefixes(query: &str, expected_prefixes: &[&str]) {
        for prefix in expected_prefixes {
            assert!(
                query.contains(&format!("@prefix {}:", prefix)),
                "Query missing @prefix declaration for '{}': {}",
                prefix,
                query
            );
        }
    }

    // Helper function to validate SPARQL query syntax
    fn assert_query_has_select_where(query: &str) {
        assert!(query.contains("SELECT"), "Query missing SELECT clause");
        assert!(query.contains("WHERE"), "Query missing WHERE clause");
    }

    #[test]
    fn test_find_policies_by_jurisdiction_determinism() {
        let query1 = SparqlGenerator::find_policies_by_jurisdiction("US");
        let query2 = SparqlGenerator::find_policies_by_jurisdiction("US");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_policies_by_jurisdiction_has_prefix_declarations() {
        let query = SparqlGenerator::find_policies_by_jurisdiction("US");
        assert_query_has_prefixes(&query, &["legal", "rdf", "rdfs"]);
        assert_query_has_select_where(&query);
    }

    #[test]
    fn test_find_policies_by_jurisdiction_contains_code() {
        let query = SparqlGenerator::find_policies_by_jurisdiction("EU");
        assert!(query.contains("EU"));
        assert!(query.contains("SELECT"));
        assert!(query.contains("legal:Policy"));
    }

    #[test]
    fn test_find_data_classifications_determinism() {
        let query1 = SparqlGenerator::find_data_classifications("Confidential");
        let query2 = SparqlGenerator::find_data_classifications("Confidential");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_data_classifications_has_prefix_declarations() {
        let query = SparqlGenerator::find_data_classifications("Public");
        assert_query_has_prefixes(&query, &["it", "rdf", "rdfs"]);
        assert_query_has_select_where(&query);
    }

    #[test]
    fn test_find_data_classifications_contains_label() {
        let query = SparqlGenerator::find_data_classifications("Public");
        assert!(query.contains("Public"));
        assert!(query.contains("it:DataClassification"));
    }

    #[test]
    fn test_find_services_by_sla_determinism() {
        let query1 = SparqlGenerator::find_services_by_sla(0.99);
        let query2 = SparqlGenerator::find_services_by_sla(0.99);
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_services_by_sla_has_prefix_declarations() {
        let query = SparqlGenerator::find_services_by_sla(0.95);
        assert_query_has_prefixes(&query, &["cloud", "rdf", "rdfs"]);
        assert_query_has_select_where(&query);
    }

    #[test]
    fn test_find_services_by_sla_contains_availability() {
        let query = SparqlGenerator::find_services_by_sla(0.95);
        assert!(query.contains("95"));
        assert!(query.contains("cloud:ServiceLevelAgreement"));
    }

    #[test]
    fn test_find_security_controls_determinism() {
        let query1 = SparqlGenerator::find_security_controls("Encryption");
        let query2 = SparqlGenerator::find_security_controls("Encryption");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_security_controls_has_prefix_declarations() {
        let query = SparqlGenerator::find_security_controls("Encryption");
        assert_query_has_prefixes(&query, &["rdf", "rdfs", "security"]);
        assert_query_has_select_where(&query);
    }

    #[test]
    fn test_find_security_controls_contains_type() {
        let query = SparqlGenerator::find_security_controls("Encryption");
        assert!(query.contains("Encryption"));
        assert!(query.contains("security:SecurityControl"));
    }

    #[test]
    fn test_find_compute_by_type_determinism() {
        let query1 = SparqlGenerator::find_compute_by_type("Kubernetes");
        let query2 = SparqlGenerator::find_compute_by_type("Kubernetes");
        assert_eq!(query1, query2);
    }

    #[test]
    fn test_find_compute_by_type_has_prefix_declarations() {
        let query = SparqlGenerator::find_compute_by_type("VM");
        assert_query_has_prefixes(&query, &["cloud", "rdf", "rdfs"]);
        assert_query_has_select_where(&query);
    }

    #[test]
    fn test_find_compute_by_type_contains_type() {
        let query = SparqlGenerator::find_compute_by_type("Kubernetes");
        assert!(query.contains("Kubernetes"));
        assert!(query.contains("cloud:ComputeService"));
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
    fn test_select_with_filters_has_prefix_declarations() {
        let filters = vec![];
        let query = SparqlGenerator::select_with_filters(&["?label"], "Service", &filters);
        assert_query_has_prefixes(&query, &["rdf", "rdfs"]);
        assert_query_has_select_where(&query);
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

    #[test]
    fn test_all_queries_have_valid_prefix_syntax() {
        // Verify all generated queries start with proper PREFIX declarations
        let queries = vec![
            ("policies", SparqlGenerator::find_policies_by_jurisdiction("US")),
            ("classifications", SparqlGenerator::find_data_classifications("Public")),
            ("sla", SparqlGenerator::find_services_by_sla(0.99)),
            ("security", SparqlGenerator::find_security_controls("Encryption")),
            ("compute", SparqlGenerator::find_compute_by_type("VM")),
        ];

        for (name, query) in queries {
            // Each query must start with @prefix
            assert!(
                query.starts_with("@prefix"),
                "Query '{}' doesn't start with @prefix: {}",
                name,
                query.lines().next().unwrap_or("")
            );

            // Each query must have multiple prefixes
            let prefix_count = query.matches("@prefix").count();
            assert!(
                prefix_count >= 2,
                "Query '{}' has insufficient prefixes (found {})",
                name,
                prefix_count
            );

            // Each query must have SELECT and WHERE
            assert_query_has_select_where(&query);

            // Each query must have a dot at end of first PREFIX line (SPARQL syntax)
            let first_line = query.lines().next().unwrap_or("<empty query>");
            assert!(
                first_line.contains(" ."),
                "Query '{}' first line missing period: {}",
                name,
                first_line
            );
        }
    }
}
