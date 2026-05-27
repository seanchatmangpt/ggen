use ggen_graph::dialect::{check_datalog, check_n3, check_shacl, check_shex, check_sparql};
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::store::Store;
use std::fs;

fn get_fixture_path(subpath: &str) -> String {
    let current_dir = std::env::current_dir().expect("Failed to get current dir");
    let base_path = if current_dir.ends_with("crates/ggen-graph") {
        current_dir.join("tests/fixtures/dialects")
    } else {
        current_dir.join("crates/ggen-graph/tests/fixtures/dialects")
    };
    base_path.join(subpath).to_string_lossy().to_string()
}

#[test]
fn test_sparql_completeness() -> Result<(), Box<dyn std::error::Error>> {
    let store = Store::new()?;
    // Load some simple data so queries have something to match
    let ttl = "<http://example.org/s> <http://example.org/p> <http://example.org/o> .";
    store.load_from_reader(RdfParser::from_format(RdfFormat::Turtle), ttl.as_bytes())?;

    // 1. Positive ASK
    let ask_pass = fs::read_to_string(get_fixture_path("sparql/ask_pass.rq"))?;
    let res = check_sparql(&ask_pass)?;
    assert!(res.conforms);
    assert!(res.supported);

    // 2. Negative ASK
    let ask_fail = fs::read_to_string(get_fixture_path("sparql/ask_fail.rq"))?;
    let res = check_sparql(&ask_fail)?;
    assert!(!res.conforms);

    // 3. Positive SELECT
    let select_pass = fs::read_to_string(get_fixture_path("sparql/select_pass.rq"))?;
    let res = check_sparql(&select_pass)?;
    assert!(res.conforms);

    // 4. Positive CONSTRUCT
    let construct_pass = fs::read_to_string(get_fixture_path("sparql/construct_pass.rq"))?;
    let res = check_sparql(&construct_pass)?;
    assert!(res.conforms);

    // 5. Malformed
    let malformed = fs::read_to_string(get_fixture_path("sparql/malformed.rq"))?;
    let err_res = check_sparql(&malformed);
    assert!(err_res.is_err());

    Ok(())
}

#[test]
fn test_shacl_completeness() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Conforming
    {
        let _store = Store::new()?;
        let conforms_ttl = fs::read_to_string(get_fixture_path("shacl/conforms.ttl"))?;
        let res = check_shacl(&conforms_ttl)?;
        assert!(res.conforms);
        assert!(res.supported);
    }

    // 2. Violating
    {
        let _store = Store::new()?;
        let violates_ttl = fs::read_to_string(get_fixture_path("shacl/violates.ttl"))?;
        let res = check_shacl(&violates_ttl)?;
        assert!(!res.conforms);
        assert!(res.message.contains("violates shape"));
    }

    // 3. Malformed
    {
        let _store = Store::new()?;
        let malformed_ttl = fs::read_to_string(get_fixture_path("shacl/malformed.ttl"))?;
        let err_res = check_shacl(&malformed_ttl);
        assert!(err_res.is_err());
    }

    Ok(())
}

#[test]
fn test_n3_completeness() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Pass (N3 syntax is valid, but conforms: false because it is unsupported)
    let n3_pass = fs::read_to_string(get_fixture_path("n3/rule_pass.n3"))?;
    let res = check_n3(&n3_pass)?;
    assert!(!res.conforms);
    assert!(!res.supported);
    assert!(res.message.contains("unsupported capability"));

    // 2. Malformed
    let malformed = fs::read_to_string(get_fixture_path("n3/malformed.n3"))?;
    let err_res = check_n3(&malformed);
    assert!(err_res.is_err());

    Ok(())
}

#[test]
fn test_datalog_completeness() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Pass
    let dl_pass = fs::read_to_string(get_fixture_path("datalog/rule_pass.dl"))?;
    let res = check_datalog(&dl_pass)?;
    assert!(!res.conforms);
    assert!(!res.supported);

    // 2. Malformed
    let malformed = fs::read_to_string(get_fixture_path("datalog/malformed.dl"))?;
    let err_res = check_datalog(&malformed);
    assert!(err_res.is_err());

    Ok(())
}

#[test]
fn test_shex_completeness() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Conforms (syntax valid, returns conforms: false, supported: false)
    let shex_conforms = fs::read_to_string(get_fixture_path("shex/conforms.shex"))?;
    let res = check_shex(&shex_conforms)?;
    assert!(!res.conforms);
    assert!(!res.supported);

    // 2. Malformed
    let malformed = fs::read_to_string(get_fixture_path("shex/malformed.shex"))?;
    let err_res = check_shex(&malformed);
    assert!(err_res.is_err());

    Ok(())
}
