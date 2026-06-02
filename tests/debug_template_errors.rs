/// Debug test to reveal actual Tera parse errors for failing templates

fn get_template_body(content: &str) -> String {
    if content.starts_with("---") {
        if let Some(idx) = content[3..].find("\n---\n") {
            return content[3 + idx + 5..].to_string();
        }
    }
    content.to_string()
}

#[test]
fn debug_template_parse_errors() {
    let failing = [
        "templates/cli-command.tera",
        "templates/ontology-diff-report.tera",
        "templates/c4-component-diagrams.tera",
        "templates/receipt-report.tera",
        "templates/dod-compliance-report.tera",
        "templates/code-review-prompt.tera",
        "templates/dod-checklist.tera",
        "templates/erlang-adapter.tera",
        "templates/type-registry.tera",
        "templates/kubernetes-deployment.tera",
        "templates/runbook-template.tera",
        "templates/openapi-from-registry.tera",
        "templates/slo-dashboard.tera",
        "templates/rust-struct-from-ontology.tera",
        "templates/ontology-explorer-dashboard.tera",
    ];
    let mut any_fail = false;
    for f in &failing {
        let content = match std::fs::read_to_string(f) {
            Ok(c) => c,
            Err(e) => {
                eprintln!("READ_ERROR {}: {}", f, e);
                continue;
            }
        };
        let body = get_template_body(&content);
        let mut tera = tera::Tera::default();
        match tera.add_raw_template("_test", &body) {
            Ok(_) => eprintln!("OK: {}", f),
            Err(e) => {
                eprintln!("FAIL: {}", f);
                eprintln!("  ERROR: {:#?}", e);
                any_fail = true;
            }
        }
    }
    assert!(
        !any_fail,
        "Some templates still fail to parse - see stderr above"
    );
}
