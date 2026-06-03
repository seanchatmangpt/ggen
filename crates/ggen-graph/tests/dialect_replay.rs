#![allow(clippy::unwrap_used)]
use ggen_graph::dialect::{check_n3, check_sparql};

#[test]
fn test_dialect_replay() {
    // Replay SPARQL
    let query = "ASK { ?s ?p ?o }";
    let res1 = check_sparql(query).unwrap();
    let res2 = check_sparql(query).unwrap();
    assert_eq!(res1.conforms, res2.conforms);
    assert_eq!(res1.message, res2.message);
    assert_eq!(res1.supported, res2.supported);

    // Replay N3
    let n3 = "{ ?x a <http://www.w3.org/ns/prov#Entity> } => { ?x a <http://www.w3.org/ns/prov#Item> } .";
    let res_n3_1 = check_n3(n3).unwrap();
    let res_n3_2 = check_n3(n3).unwrap();
    assert_eq!(res_n3_1.conforms, res_n3_2.conforms);
    assert_eq!(res_n3_1.message, res_n3_2.message);
    assert_eq!(res_n3_1.supported, res_n3_2.supported);
}
