#![cfg(test)]

use super::*;
use crate::{Encoder, Parser, Syntax};
#[test]
fn test_remove() {
    let mut index = TripleIndex::new();
    let data = "<http://example2.com/a> a test:SubClass.\n\
                <http://example2.com/a> test:hasRef <http://example2.com/b>.\n\
                <http://example2.com/b> test:hasRef <http://example2.com/c>.\n\
                <http://example2.com/c> a test:SubClass.";
    let (content, _rules) = Parser::parse(data.to_string());
    let rc_triples: Vec<Rc<Triple>> = content.into_iter().map(Rc::new).collect();
    rc_triples.iter().for_each(|t| index.add_ref(t.clone()));
    assert_eq!(4, index.len());
    index.remove_ref(&rc_triples.first().unwrap().clone());
    assert_eq!(3, index.len());
}

#[test]
fn test_query_fact() {
    let mut index = TripleIndex::new();
    let data = ":a a :C.\n\
                :b a :D.\n\
                {:a a :C}=>{:a a :D}";
    let (content, rules) = Parser::parse(data.to_string());
    content.into_iter().for_each(|t| index.add(t));
    let query = &rules.first().unwrap().body.first().unwrap().pattern;
    let result = index.query(query, None);
    assert!(result.is_some());
}
#[test]
fn test_quad_filter() {
    let nquads = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
<http://example.com/foo> <http://schema.org/name> \"Foo\" <http://example.com/> .
<http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> .
<http://example.com/bar> <http://schema.org/name> \"Bar\" .";
    let triples = Parser::parse_triples(nquads, Syntax::NQuads).unwrap();
    let mut index = TripleIndex::new();
    triples.into_iter().for_each(|t| index.add(t));
    let query_triple = Triple::from_with_graph_name(
        "?s".to_string(),
        "?p".to_string(),
        "?o".to_string(),
        "http://example.com/".to_string(),
    );
    let bindings = index.query(&query_triple, None);
    assert_eq!(2, bindings.unwrap().len());
}

#[test]
fn test_quad_query() {
    let nquads = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
<http://example.com/bar> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/somethingelse> .";
    let triples = Parser::parse_triples(nquads, Syntax::NQuads).unwrap();
    let mut index = TripleIndex::new();
    triples.into_iter().for_each(|t| index.add(t));
    let query_triple = Triple::from_with_graph_name(
        "http://example.com/foo".to_string(),
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),
        "http://schema.org/Person".to_string(),
        "?g".to_string(),
    );
    let bindings = index.query(&query_triple, None).unwrap();
    assert_eq!(1, bindings.len());
    assert_eq!(
        &Encoder::add("<http://example.com/>".to_string()),
        bindings
            .get(&Encoder::add("g".to_string()))
            .unwrap()
            .first()
            .unwrap()
    );
}
#[test]
fn test_same_triple_in_multiple_graphs_query() {
    let nquads = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/somethingelse> .";
    let triples = Parser::parse_triples(nquads, Syntax::NQuads).unwrap();
    let mut index = TripleIndex::new();
    triples.into_iter().for_each(|t| index.add(t));
    let query_triple = Triple::from_with_graph_name(
        "http://example.com/foo".to_string(),
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),
        "http://schema.org/Person".to_string(),
        "?g".to_string(),
    );
    let bindings = index.query(&query_triple, None).unwrap();
    assert_eq!(2, bindings.len());
    assert_eq!(
        &Encoder::add("<http://example.com/>".to_string()),
        bindings
            .get(&Encoder::add("g".to_string()))
            .unwrap()
            .first()
            .unwrap()
    );
}

#[test]
fn test_iterator() {
    let nquads = "<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Person> <http://example.com/> .
<http://example.com/foo> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://schema.org/Student> <http://example.com/somethingelse> .";
    let triples = Parser::parse_triples(nquads, Syntax::NQuads).unwrap();
    let mut index = TripleIndex::new();
    triples.into_iter().for_each(|t| index.add(t));
    let query_triple = Triple::from(
        "?s".to_string(),
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),
        "?o".to_string(),
    );
    let it = index.query_help(&query_triple, None);
    for item in it {
        println!("item {:?}", item);
    }
}
