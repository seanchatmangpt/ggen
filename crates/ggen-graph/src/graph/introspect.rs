//! Graph introspection for LSP completion and hover.
//!
//! Enumerates the classes and properties declared in a graph so a language
//! server can offer them as completion candidates and resolve hovers.

use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;

use crate::graph::DeterministicGraph;
use crate::GraphError;

/// Classes and properties discovered in a graph (IRIs, sorted, de-duplicated).
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct IriTerms {
    /// Class IRIs (rdfs:Class, owl:Class, sh:NodeShape, or any rdf:type object).
    pub classes: Vec<String>,
    /// Property IRIs (rdf:Property, owl:{Object,Datatype}Property, or any predicate used).
    pub properties: Vec<String>,
}

fn collect(graph: &DeterministicGraph, query: &str, var: &str) -> Result<Vec<String>, GraphError> {
    let mut out = Vec::new();
    if let QueryResults::Solutions(sols) = graph.query(query)? {
        for sol in sols {
            let sol = sol.map_err(|e| GraphError::Serialization(e.to_string()))?;
            if let Some(Term::NamedNode(n)) = sol.get(var) {
                out.push(n.as_str().to_string());
            }
        }
    }
    out.sort();
    out.dedup();
    Ok(out)
}

/// Enumerate the class and property IRIs declared or used in `graph`.
pub fn iri_terms(graph: &DeterministicGraph) -> Result<IriTerms, GraphError> {
    let classes = collect(
        graph,
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
         PREFIX owl: <http://www.w3.org/2002/07/owl#>
         PREFIX sh: <http://www.w3.org/ns/shacl#>
         SELECT DISTINCT ?c WHERE {
           { ?c a rdfs:Class } UNION { ?c a owl:Class }
           UNION { ?c a sh:NodeShape } UNION { ?x a ?c }
           FILTER(isIRI(?c))
         }",
        "c",
    )?;
    let properties = collect(
        graph,
        "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
         PREFIX owl: <http://www.w3.org/2002/07/owl#>
         SELECT DISTINCT ?p WHERE {
           { ?p a rdf:Property } UNION { ?p a owl:ObjectProperty }
           UNION { ?p a owl:DatatypeProperty } UNION { ?s ?p ?o }
           FILTER(isIRI(?p))
         }",
        "p",
    )?;
    Ok(IriTerms {
        classes,
        properties,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::parse::parse_turtle;

    #[test]
    fn enumerates_classes_and_properties() -> Result<(), GraphError> {
        let ttl = r#"
            @prefix ex: <http://example.org/> .
            @prefix owl: <http://www.w3.org/2002/07/owl#> .
            ex:Person a owl:Class .
            ex:name a owl:DatatypeProperty .
            ex:alice a ex:Person ; ex:name "Alice" .
        "#;
        let graph = DeterministicGraph::new()?;
        for quad in parse_turtle(ttl)? {
            graph.insert_quad(&quad)?;
        }
        let terms = iri_terms(&graph)?;
        assert!(terms.classes.contains(&"http://example.org/Person".to_string()));
        assert!(terms
            .properties
            .contains(&"http://example.org/name".to_string()));
        Ok(())
    }
}
