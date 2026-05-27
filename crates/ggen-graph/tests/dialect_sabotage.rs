use ggen_graph::dialect::{check_datalog, check_n3, check_shacl, check_shex, check_sparql};

#[test]
fn test_dialect_sabotage_checks() {
    // 1. Sabotage SPARQL: invalid query string
    let res = check_sparql("SELECT ?s WHERE {");
    assert!(res.is_err());

    // 2. Sabotage SHACL: invalid turtle syntax
    let res = check_shacl("PREFIX sh: <http://www.w3.org/ns/shacl#> \n <Shape> a sh:NodeShape");
    assert!(res.is_err());

    // 3. Sabotage N3: unbalanced brackets
    let res = check_n3("{ ?x a <http://example.org/T> .");
    assert!(res.is_err());

    // 4. Sabotage Datalog: missing rule termination
    let res = check_datalog("ancestor(X, Y) :- parent(X, Y)");
    assert!(res.is_err());

    // 5. Sabotage ShEx: missing brace
    let res = check_shex("<Shape> {");
    assert!(res.is_err());
}
