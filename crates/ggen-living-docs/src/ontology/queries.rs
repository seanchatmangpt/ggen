//! SPARQL query templates for code ontology

use super::schema::EntityKind;

/// Pre-defined SPARQL queries for common operations
pub struct SparqlQueries {
    base_uri: String,
}

impl SparqlQueries {
    /// Create new SPARQL queries helper
    pub fn new(base_uri: &str) -> Self {
        Self {
            base_uri: base_uri.to_string(),
        }
    }

    /// Query for entities of a specific kind
    pub fn entities_by_kind(&self, kind: EntityKind) -> String {
        format!(
            r#"
            PREFIX code: <{}>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT ?entity ?name ?doc
            WHERE {{
                ?entity rdf:type code:{} .
                ?entity code:name ?name .
                OPTIONAL {{ ?entity code:documentation ?doc }}
            }}
            "#,
            self.base_uri, kind
        )
    }

    /// Query for relationships of an entity
    pub fn relationships_for_entity(&self, entity_id: &str) -> String {
        format!(
            r#"
            PREFIX code: <{}>

            SELECT ?predicate ?target
            WHERE {{
                code:{} ?predicate ?target .
                FILTER(isURI(?target))
            }}
            "#,
            self.base_uri, entity_id
        )
    }

    /// Query for dependency graph
    pub fn dependency_graph(&self) -> String {
        format!(
            r#"
            PREFIX code: <{}>

            SELECT ?source ?target
            WHERE {{
                ?source code:dependsOn ?target .
            }}
            "#,
            self.base_uri
        )
    }

    /// Query for undocumented entities
    pub fn undocumented_entities(&self) -> String {
        format!(
            r#"
            PREFIX code: <{}>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT ?entity ?name
            WHERE {{
                ?entity code:name ?name .
                FILTER NOT EXISTS {{ ?entity code:documentation ?doc }}
            }}
            "#,
            self.base_uri
        )
    }

    /// Query for call graph
    pub fn call_graph(&self) -> String {
        format!(
            r#"
            PREFIX code: <{}>

            SELECT ?caller ?callee
            WHERE {{
                ?caller code:calls ?callee .
            }}
            "#,
            self.base_uri
        )
    }

    /// Query for trait implementations
    pub fn trait_implementations(&self, trait_name: &str) -> String {
        format!(
            r#"
            PREFIX code: <{}>

            SELECT ?impl
            WHERE {{
                ?impl code:implements code:{} .
            }}
            "#,
            self.base_uri, trait_name
        )
    }

    /// Full-text search query
    pub fn search(&self, search_term: &str) -> String {
        format!(
            r#"
            PREFIX code: <{}>

            SELECT ?entity ?name ?doc
            WHERE {{
                ?entity code:name ?name .
                OPTIONAL {{ ?entity code:documentation ?doc }}
                FILTER(CONTAINS(LCASE(?name), LCASE("{}")) ||
                       CONTAINS(LCASE(?doc), LCASE("{}")))
            }}
            "#,
            self.base_uri, search_term, search_term
        )
    }
}
