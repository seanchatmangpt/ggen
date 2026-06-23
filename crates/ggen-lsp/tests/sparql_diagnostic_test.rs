use ggen_lsp::check::check_content;
use lsp_max::lsp_types::DiagnosticSeverity;

#[test]
fn test_e0013_select_without_order_by() {
    let content = "SELECT ?s WHERE { ?s ?p ?o }";
    let report = check_content("test.rq", content).expect("should be a law surface");

    let e0013 = report.diagnostics.iter().find(|d| {
        matches!(&d.code, Some(lsp_max::lsp_types::NumberOrString::String(s)) if s == "E0013")
    }).expect("E0013 diagnostic missing");

    assert_eq!(e0013.severity, Some(DiagnosticSeverity::WARNING));
    assert!(e0013.message.contains("SELECT query lacks ORDER BY"));
}

#[test]
fn test_e0015_identity_construct() {
    let content = "CONSTRUCT WHERE { ?s ?p ?o }";
    let report = check_content("test.rq", content).expect("should be a law surface");

    let e0015 = report.diagnostics.iter().find(|d| {
        matches!(&d.code, Some(lsp_max::lsp_types::NumberOrString::String(s)) if s == "E0015")
    }).expect("E0015 diagnostic missing");

    assert_eq!(e0015.severity, Some(DiagnosticSeverity::WARNING));
    assert!(e0015.message.contains("Identity CONSTRUCT query detected"));
}

#[test]
fn test_clean_select_no_e0013() {
    let content = "SELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s";
    let report = check_content("test.rq", content).expect("should be a law surface");

    assert!(!report.diagnostics.iter().any(|d| {
        matches!(&d.code, Some(lsp_max::lsp_types::NumberOrString::String(s)) if s == "E0013")
    }));
}
