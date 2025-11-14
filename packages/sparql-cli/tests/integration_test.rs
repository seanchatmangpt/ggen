use sparql_cli::{query, optimization, federation};

#[test]
fn test_query_parsing() {
    let query = "SELECT * WHERE { ?s ?p ?o } LIMIT 10";
    let result = query::parse(query);
    assert!(result.is_ok());
}

#[test]
fn test_query_execution() {
    let query = "SELECT * WHERE { ?s ?p ?o } LIMIT 10";
    let endpoint = "http://example.org/sparql";
    let result = query::execute(query, endpoint);
    assert!(result.is_ok());
}

#[test]
fn test_optimization() {
    let query = "SELECT * WHERE { ?s ?p ?o . FILTER(?o > 10) }";
    let result = optimization::optimize(query, 2);
    assert!(result.is_ok());
}

#[test]
fn test_federation() {
    let query = "SELECT * WHERE { ?s ?p ?o }";
    let endpoints = vec!["http://ep1.org/sparql", "http://ep2.org/sparql"];
    let result = federation::execute_federated(query, &endpoints);
    assert!(result.is_ok());
}

#[test]
fn test_optimization_levels() {
    let query = "SELECT ?s ?p ?o WHERE { ?s ?p ?o . ?s a ?type }";

    for level in 1..=3 {
        let result = optimization::optimize(query, level);
        assert!(result.is_ok(), "Optimization level {} failed", level);
    }
}

#[test]
fn test_complex_query_parsing() {
    let query = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX dbo: <http://dbpedia.org/ontology/>

        SELECT ?person ?name ?birthDate
        WHERE {
            ?person a dbo:Person ;
                    foaf:name ?name ;
                    dbo:birthDate ?birthDate .
            FILTER(?birthDate > "1900-01-01"^^xsd:date)
        }
        ORDER BY DESC(?birthDate)
        LIMIT 50
    "#;

    let result = query::parse(query);
    assert!(result.is_ok());
}

#[test]
fn test_ask_query() {
    let query = r#"
        ASK {
            ?person a foaf:Person .
            ?person foaf:name "Test Person"
        }
    "#;

    let result = query::parse(query);
    assert!(result.is_ok());
}

#[test]
fn test_construct_query() {
    let query = r#"
        CONSTRUCT {
            ?person foaf:name ?name .
        }
        WHERE {
            ?person a foaf:Person .
            ?person rdfs:label ?name
        }
    "#;

    let result = query::parse(query);
    assert!(result.is_ok());
}

#[test]
fn test_describe_query() {
    let query = "DESCRIBE <http://example.org/person/123>";
    let result = query::parse(query);
    assert!(result.is_ok());
}
