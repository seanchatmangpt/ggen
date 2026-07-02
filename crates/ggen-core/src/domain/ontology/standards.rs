use crate::graph::Graph;
use crate::utils::error::Error;

pub const OWL_TTL: &str = include_str!("../../../ontologies/core/owl.ttl");
pub const RDF_TTL: &str = include_str!("../../../ontologies/core/rdf-syntax-ns.ttl");
pub const RDFS_TTL: &str = include_str!("../../../ontologies/core/rdf-schema.ttl");
pub const SCHEMA_TTL: &str =
    include_str!("../../../ontologies/schema-org/schemaorg-all-https.ttl");
pub const FOAF_TTL: &str = include_str!("../../../ontologies/foaf.ttl");
pub const DUBLIN_CORE_TTL: &str =
    include_str!("../../../ontologies/dublin-core-elements-1.1.ttl");

/// Helper to load standard public ontologies into a graph
pub fn load_standard_vocabularies(graph: &Graph) -> Result<(), Error> {
    graph
        .insert_turtle(OWL_TTL)
        .map_err(|e| Error::new(&format!("Failed to load embedded OWL ontology: {}", e)))?;

    graph
        .insert_turtle(RDF_TTL)
        .map_err(|e| Error::new(&format!("Failed to load embedded RDF ontology: {}", e)))?;

    graph
        .insert_turtle(RDFS_TTL)
        .map_err(|e| Error::new(&format!("Failed to load embedded RDFS ontology: {}", e)))?;

    graph.insert_turtle(SCHEMA_TTL).map_err(|e| {
        Error::new(&format!(
            "Failed to load embedded schema.org ontology: {}",
            e
        ))
    })?;

    graph
        .insert_turtle(FOAF_TTL)
        .map_err(|e| Error::new(&format!("Failed to load embedded FOAF ontology: {}", e)))?;

    graph.insert_turtle(DUBLIN_CORE_TTL).map_err(|e| {
        Error::new(&format!(
            "Failed to load embedded Dublin Core ontology: {}",
            e
        ))
    })?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_standard_vocabularies() {
        let graph = Graph::new().unwrap();
        load_standard_vocabularies(&graph).unwrap();

        // Verify FOAF Person exists
        let query_foaf = "ASK { <http://xmlns.com/foaf/0.1/Person> ?p ?o }";
        let results_foaf = graph.query(query_foaf).unwrap();
        match results_foaf {
            oxigraph::sparql::QueryResults::Boolean(b) => {
                assert!(b, "foaf:Person not found in graph")
            }
            _ => panic!("Expected boolean result for ASK query"),
        }

        // Verify Dublin Core title exists
        let query_dc = "ASK { <http://purl.org/dc/elements/1.1/title> ?p ?o }";
        let results_dc = graph.query(query_dc).unwrap();
        match results_dc {
            oxigraph::sparql::QueryResults::Boolean(b) => assert!(b, "dc:title not found in graph"),
            _ => panic!("Expected boolean result for ASK query"),
        }
    }
}
