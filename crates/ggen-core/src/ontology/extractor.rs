//! Ontology Extraction from RDF/OWL
//!
//! Uses SPARQL queries against RDF graphs to extract semantic ontology definitions.
//! Supports OWL 2 and RDFS vocabulary for comprehensive ontology parsing.

use super::schema::*;
use crate::graph::Graph;
use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;
use std::collections::{BTreeMap, HashMap, HashSet};

/// Extractor for RDF/OWL ontologies using SPARQL queries
pub struct OntologyExtractor;

impl OntologyExtractor {
    /// Extract complete ontology schema from an RDF graph
    ///
    /// # Arguments
    /// - `graph`: RDF graph containing OWL/RDFS definitions
    /// - `namespace`: Ontology namespace filter (e.g., "http://example.org/")
    ///
    /// # Returns
    /// Complete OntologySchema with all classes, properties, and relationships
    pub fn extract(graph: &Graph, namespace: &str) -> Result<OntologySchema, String> {
        let mut schema = OntologySchema::new(namespace, "1.0.0");
        schema.label = Self::extract_ontology_label(graph, namespace)?;
        schema.description = Self::extract_ontology_description(graph, namespace);

        // Extract classes first (they may be referenced by properties)
        schema.classes = Self::extract_classes(graph, namespace)?;

        // Extract properties
        schema.properties = Self::extract_properties(graph, namespace, &schema.classes)?;

        // Build relationships from properties
        schema.relationships = Self::build_relationships(&schema);

        Ok(schema)
    }

    /// Extract all classes from the RDF graph
    fn extract_classes(graph: &Graph, namespace: &str) -> Result<Vec<OntClass>, String> {
        // SPARQL query to extract all classes with their metadata
        let query = r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT DISTINCT ?class ?label ?comment ?parent
            WHERE {
              ?class a owl:Class .
              OPTIONAL { ?class rdfs:label ?label }
              OPTIONAL { ?class rdfs:comment ?comment }
              OPTIONAL { ?class rdfs:subClassOf ?parent }
            }
        "#;

        let mut classes = Vec::new();
        let mut class_map: HashMap<String, OntClass> = HashMap::new();

        let results = graph.query(query).map_err(|e| e.to_string())?;

        if let QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                match solution {
                    Ok(bindings) => {
                        if let Some(class_term) = bindings.get("class") {
                            let class_uri = Self::term_to_string(class_term);

                            // Filter by namespace
                            if !class_uri.contains(namespace) {
                                continue;
                            }

                            let name = Self::extract_local_name(&class_uri);
                            let label = bindings
                                .get("label")
                                .map(Self::term_to_string)
                                .unwrap_or_else(|| name.clone());
                            let description = bindings.get("comment").map(Self::term_to_string);

                            let mut parent_classes = Vec::new();
                            if let Some(parent_term) = bindings.get("parent") {
                                let parent_uri = Self::term_to_string(parent_term);
                                if parent_uri != "http://www.w3.org/2002/07/owl#Thing" {
                                    parent_classes.push(parent_uri);
                                }
                            }

                            let entry =
                                class_map
                                    .entry(class_uri.clone())
                                    .or_insert_with(|| OntClass {
                                        uri: class_uri.clone(),
                                        name,
                                        label,
                                        description,
                                        parent_classes: Vec::new(),
                                        properties: Vec::new(),
                                        is_abstract: false,
                                        restrictions: Vec::new(),
                                    });

                            entry.parent_classes.extend(parent_classes);
                        }
                    }
                    Err(e) => return Err(format!("SPARQL solution error: {}", e)),
                }
            }
        }

        for class in class_map.values() {
            classes.push(class.clone());
        }
        classes.sort_by(|a, b| a.name.cmp(&b.name));

        Ok(classes)
    }

    /// Extract all properties from the RDF graph
    fn extract_properties(
        graph: &Graph, namespace: &str, classes: &[OntClass],
    ) -> Result<Vec<OntProperty>, String> {
        // SPARQL query to extract ObjectProperties and DatatypeProperties
        let query = r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

            SELECT DISTINCT ?property ?label ?comment ?domain ?range ?type
            WHERE {
              {
                ?property a owl:ObjectProperty .
                BIND("object" AS ?type)
              } UNION {
                ?property a owl:DatatypeProperty .
                BIND("datatype" AS ?type)
              }
              OPTIONAL { ?property rdfs:label ?label }
              OPTIONAL { ?property rdfs:comment ?comment }
              OPTIONAL { ?property rdfs:domain ?domain }
              OPTIONAL { ?property rdfs:range ?range }
            }
        "#;

        let mut properties = Vec::new();
        let class_uris: HashSet<_> = classes.iter().map(|c| c.uri.clone()).collect();

        let results = graph.query(query).map_err(|e| e.to_string())?;

        if let QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                match solution {
                    Ok(bindings) => {
                        if let Some(prop_term) = bindings.get("property") {
                            let prop_uri = Self::term_to_string(prop_term);

                            // Filter by namespace
                            if !prop_uri.contains(namespace) {
                                continue;
                            }

                            let name = Self::extract_local_name(&prop_uri);
                            let label = bindings
                                .get("label")
                                .map(Self::term_to_string)
                                .unwrap_or_else(|| name.clone());
                            let description = bindings.get("comment").map(Self::term_to_string);
                            let prop_type = bindings
                                .get("type")
                                .map(Self::term_to_string)
                                .unwrap_or_else(|| "datatype".to_string());

                            let mut domain = Vec::new();
                            if let Some(domain_term) = bindings.get("domain") {
                                let domain_uri = Self::term_to_string(domain_term);
                                if class_uris.contains(&domain_uri) {
                                    domain.push(domain_uri);
                                }
                            }

                            let range = if prop_type == "object" {
                                if let Some(range_term) = bindings.get("range") {
                                    let range_uri = Self::term_to_string(range_term);
                                    if class_uris.contains(&range_uri) {
                                        PropertyRange::Reference(range_uri)
                                    } else {
                                        PropertyRange::String
                                    }
                                } else {
                                    PropertyRange::String
                                }
                            } else {
                                // Infer range from range term or default to String
                                if let Some(range_term) = bindings.get("range") {
                                    let range_uri = Self::term_to_string(range_term);
                                    Self::infer_property_range(&range_uri)
                                } else {
                                    PropertyRange::String
                                }
                            };

                            let cardinality = Cardinality::ZeroOrOne; // Default, will be refined by constraints

                            let is_functional = Self::check_functional(graph, &prop_uri)?;

                            properties.push(OntProperty {
                                uri: prop_uri,
                                name,
                                label,
                                description,
                                domain,
                                range,
                                cardinality,
                                required: false,
                                is_functional,
                                is_inverse_functional: false,
                                inverse_of: None,
                            });
                        }
                    }
                    Err(e) => return Err(format!("SPARQL solution error: {}", e)),
                }
            }
        }

        properties.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(properties)
    }

    /// Extract cardinality constraints for properties
    pub fn extract_cardinality(
        graph: &Graph, class_uri: &str,
    ) -> Result<BTreeMap<String, Cardinality>, String> {
        let escaped_class = class_uri.replace(['<', '>'], "");
        let query = format!(
            r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

            SELECT ?property ?minCard ?maxCard ?exactCard
            WHERE {{
              <{}> rdfs:subClassOf ?restriction .
              ?restriction a owl:Restriction ;
                           owl:onProperty ?property .
              OPTIONAL {{ ?restriction owl:minCardinality ?minCard }}
              OPTIONAL {{ ?restriction owl:maxCardinality ?maxCard }}
              OPTIONAL {{ ?restriction owl:cardinality ?exactCard }}
            }}
            "#,
            escaped_class
        );

        let mut cardinalities = BTreeMap::new();
        let results = graph.query(&query).map_err(|e| e.to_string())?;

        if let QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                match solution {
                    Ok(bindings) => {
                        if let Some(prop_term) = bindings.get("property") {
                            let prop_uri = Self::term_to_string(prop_term);

                            let cardinality = if let Some(exact_term) = bindings.get("exactCard") {
                                if let Ok(val) = Self::term_to_string(exact_term).parse::<u32>() {
                                    if val == 1 {
                                        Cardinality::One
                                    } else {
                                        Cardinality::Range {
                                            min: val,
                                            max: Some(val),
                                        }
                                    }
                                } else {
                                    Cardinality::Many
                                }
                            } else {
                                let min = bindings
                                    .get("minCard")
                                    .and_then(|t| Self::term_to_string(t).parse::<u32>().ok())
                                    .unwrap_or(0);
                                let max = bindings
                                    .get("maxCard")
                                    .and_then(|t| Self::term_to_string(t).parse::<u32>().ok());

                                match (min, max) {
                                    (0, Some(1)) => Cardinality::ZeroOrOne,
                                    (1, Some(1)) => Cardinality::One,
                                    (1, None) => Cardinality::OneOrMore,
                                    _ => Cardinality::Range { min, max },
                                }
                            };

                            cardinalities.insert(prop_uri, cardinality);
                        }
                    }
                    Err(e) => return Err(format!("SPARQL solution error: {}", e)),
                }
            }
        }

        Ok(cardinalities)
    }

    // Helper methods

    /// Extract string value from an oxigraph Term
    fn term_to_string(term: &Term) -> String {
        match term {
            Term::NamedNode(n) => n.as_str().to_string(),
            Term::Literal(l) => l.value().to_string(),
            Term::BlankNode(b) => b.as_str().to_string(),
            _ => term.to_string(),
        }
    }

    /// Extract local name from full URI
    fn extract_local_name(uri: &str) -> String {
        // Remove URI brackets if present
        let clean_uri = uri.trim_start_matches('<').trim_end_matches('>');

        // Try splitting on '#' first
        if let Some(local) = clean_uri.split('#').nth(1) {
            return local.to_string();
        }

        // If no '#', try splitting on '/' and get the last segment
        clean_uri
            .split('/')
            .last()
            .unwrap_or("unknown")
            .to_string()
    }

    /// Infer property range from XSD type URI
    fn infer_property_range(range_uri: &str) -> PropertyRange {
        let uri_lower = range_uri.to_lowercase();

        // Check for full XSD URIs first (most specific)
        if uri_lower.ends_with("#integer") || uri_lower.ends_with("/integer") {
            return PropertyRange::Integer;
        }
        if uri_lower.ends_with("#string") || uri_lower.ends_with("/string") {
            return PropertyRange::String;
        }
        if uri_lower.ends_with("#boolean") || uri_lower.ends_with("/boolean") {
            return PropertyRange::Boolean;
        }
        if uri_lower.ends_with("#float") || uri_lower.ends_with("/float")
            || uri_lower.ends_with("#double") || uri_lower.ends_with("/double")
            || uri_lower.ends_with("#decimal") || uri_lower.ends_with("/decimal") {
            return PropertyRange::Float;
        }
        if uri_lower.ends_with("#datetime") || uri_lower.ends_with("/datetime") {
            return PropertyRange::DateTime;
        }
        if uri_lower.ends_with("#date") || uri_lower.ends_with("/date") {
            return PropertyRange::Date;
        }
        if uri_lower.ends_with("#time") || uri_lower.ends_with("/time") {
            return PropertyRange::Time;
        }

        // Fallback to checking for patterns in the URI
        if uri_lower.contains("integer") {
            return PropertyRange::Integer;
        }
        if uri_lower.contains("boolean") {
            return PropertyRange::Boolean;
        }
        if uri_lower.contains("float") || uri_lower.contains("double") || uri_lower.contains("decimal") {
            return PropertyRange::Float;
        }
        if uri_lower.contains("datetime") {
            return PropertyRange::DateTime;
        }
        if uri_lower.contains("date") {
            return PropertyRange::Date;
        }
        if uri_lower.contains("time") {
            return PropertyRange::Time;
        }

        // Default to String for unknown types
        PropertyRange::String
    }

    /// Check if property is functional (has at most one value)
    fn check_functional(graph: &Graph, prop_uri: &str) -> Result<bool, String> {
        let escaped_uri = prop_uri.replace(['<', '>'], "");
        let query = format!(
            r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            ASK {{
              <{}> a owl:FunctionalProperty .
            }}
            "#,
            escaped_uri
        );

        let results = graph.query(&query).map_err(|e| e.to_string())?;

        if let QueryResults::Boolean(result) = results {
            Ok(result)
        } else {
            Ok(false)
        }
    }

    /// Extract ontology label (rdfs:label of the ontology resource)
    fn extract_ontology_label(graph: &Graph, namespace: &str) -> Result<String, String> {
        let query = format!(
            r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?label
            WHERE {{
              ?ontology a owl:Ontology ;
                        rdfs:label ?label .
              FILTER(STRSTARTS(STR(?ontology), "{}"))
            }}
            LIMIT 1
            "#,
            namespace
        );

        let results = graph.query(&query).map_err(|e| e.to_string())?;

        if let QueryResults::Solutions(mut solutions) = results {
            if let Some(Ok(bindings)) = solutions.next() {
                if let Some(label_term) = bindings.get("label") {
                    return Ok(Self::term_to_string(label_term));
                }
            }
        }

        Ok("Unknown Ontology".to_string())
    }

    /// Extract ontology description
    fn extract_ontology_description(graph: &Graph, namespace: &str) -> Option<String> {
        let query = format!(
            r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?comment
            WHERE {{
              ?ontology a owl:Ontology ;
                        rdfs:comment ?comment .
              FILTER(STRSTARTS(STR(?ontology), "{}"))
            }}
            LIMIT 1
            "#,
            namespace
        );

        if let Ok(QueryResults::Solutions(mut solutions)) = graph.query(&query) {
            if let Some(Ok(bindings)) = solutions.next() {
                if let Some(comment_term) = bindings.get("comment") {
                    return Some(Self::term_to_string(comment_term));
                }
            }
        }

        None
    }

    /// Build relationship graph from properties
    fn build_relationships(schema: &OntologySchema) -> Vec<OntRelationship> {
        let mut relationships = Vec::new();
        let class_uri_map: HashMap<&str, &OntClass> =
            schema.classes.iter().map(|c| (c.uri.as_str(), c)).collect();

        for prop in &schema.properties {
            for from_class_uri in &prop.domain {
                if let PropertyRange::Reference(to_class_uri) = &prop.range {
                    if class_uri_map.contains_key(to_class_uri.as_str()) {
                        let relationship_type = match &prop.cardinality {
                            Cardinality::One | Cardinality::ZeroOrOne => {
                                RelationshipType::ManyToOne
                            }
                            Cardinality::Many | Cardinality::OneOrMore => {
                                RelationshipType::ManyToMany
                            }
                            Cardinality::Range { .. } => RelationshipType::ManyToMany,
                        };

                        relationships.push(OntRelationship {
                            from_class: from_class_uri.clone(),
                            to_class: to_class_uri.clone(),
                            property: prop.uri.clone(),
                            relationship_type,
                            bidirectional: prop.inverse_of.is_some(),
                            label: prop.label.clone(),
                        });
                    }
                }
            }
        }

        relationships
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_local_name() {
        assert_eq!(
            OntologyExtractor::extract_local_name("http://example.org/schema#Product"),
            "Product"
        );
        assert_eq!(
            OntologyExtractor::extract_local_name("http://example.org/schema/Product"),
            "Product"
        );
    }

    #[test]
    fn test_infer_property_range() {
        assert_eq!(
            OntologyExtractor::infer_property_range("http://www.w3.org/2001/XMLSchema#string"),
            PropertyRange::String
        );
        assert_eq!(
            OntologyExtractor::infer_property_range("http://www.w3.org/2001/XMLSchema#integer"),
            PropertyRange::Integer
        );
        assert_eq!(
            OntologyExtractor::infer_property_range("http://www.w3.org/2001/XMLSchema#boolean"),
            PropertyRange::Boolean
        );
    }
}
