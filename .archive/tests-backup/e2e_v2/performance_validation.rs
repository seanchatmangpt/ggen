// Scenario 9: Performance validation
// Chicago TDD: Measure REAL performance on actual operations

use assert_cmd::Command;
use std::fs;
use std::time::Instant;

use super::test_helpers::*;

#[test]
fn test_project_generation_performance() {
    let workspace = setup_workspace().unwrap();

    // Project generation should complete in < 5 seconds
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .arg("project")
        .arg("new")
        .arg("perf-test-app")
        .current_dir(workspace.path())
        .assert()
        .success();

    let duration = start.elapsed();

    println!("Project generation took: {:?}", duration);

    assert!(
        duration.as_secs() < 5,
        "Project generation should complete in < 5s, took {:?}",
        duration
    );

    println!("✅ Project generation performance: PASSED ({:?})", duration);
}

#[test]
fn test_template_rendering_performance() {
    let workspace = setup_workspace().unwrap();

    // Create a moderately complex template
    let template = r#"
// Generated REST API
{% for i in range(end=100) %}
pub async fn handler_{{ i }}() -> impl Responder {
    HttpResponse::Ok().json(json!({"id": {{ i }}}))
}
{% endfor %}

pub fn configure_routes(cfg: &mut web::ServiceConfig) {
    cfg
    {% for i in range(end=100) %}
        .route("/api/endpoint{{ i }}", web::get().to(handler_{{ i }}))
    {% endfor %}
    ;
}
"#;

    let template_file = workspace.path().join("api.tmpl");
    fs::write(&template_file, template).unwrap();

    // Rendering should complete in < 1 second
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .arg("template")
        .arg("render")
        .arg(&template_file)
        .current_dir(workspace.path())
        .assert()
        .success();

    let duration = start.elapsed();

    println!("Template rendering took: {:?}", duration);

    assert!(
        duration.as_millis() < 1000,
        "Template rendering should complete in < 1s, took {:?}",
        duration
    );

    println!("✅ Template rendering performance: PASSED ({:?})", duration);
}

#[test]
fn test_rdf_query_performance() {
    let workspace = setup_workspace().unwrap();

    // Create RDF file with moderate data
    let mut rdf_data = String::from(
        "@prefix : <http://example.org/> .\n\
         @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\n",
    );

    // Add 1000 triples
    for i in 0..1000 {
        rdf_data.push_str(&format!(":entity{} :hasValue {} .\n", i, i));
    }

    let rdf_file = workspace.path().join("large.ttl");
    fs::write(&rdf_file, rdf_data).unwrap();

    // Query should complete in < 1 second
    let start = Instant::now();

    Command::cargo_bin("ggen")
        .unwrap()
        .arg("rdf")
        .arg("query")
        .arg(&rdf_file)
        .arg("SELECT ?s ?o WHERE { ?s :hasValue ?o } LIMIT 100")
        .current_dir(workspace.path())
        .assert()
        .success();

    let duration = start.elapsed();

    println!("RDF query took: {:?}", duration);

    assert!(
        duration.as_millis() < 1000,
        "RDF query should complete in < 1s, took {:?}",
        duration
    );

    println!("✅ RDF query performance: PASSED ({:?})", duration);
}

#[test]
fn test_batch_generation_performance() {
    let workspace = setup_workspace().unwrap();

    // Create a simple template
    let template = "File {{ file_num }}: {{ content }}";
    let template_file = workspace.path().join("batch.tmpl");
    fs::write(&template_file, template).unwrap();

    // Generate 50 files
    let start = Instant::now();

    for i in 1..=50 {
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--var")
            .arg(format!("file_num={}", i))
            .arg("--var")
            .arg("content=Test")
            .arg("--output")
            .arg(workspace.path().join(format!("file-{}.txt", i)))
            .current_dir(workspace.path())
            .assert()
            .success();
    }

    let duration = start.elapsed();

    println!("Batch generation of 50 files took: {:?}", duration);

    // Should complete in reasonable time (< 10s for 50 files)
    assert!(
        duration.as_secs() < 10,
        "Batch generation should complete in < 10s, took {:?}",
        duration
    );

    println!("✅ Batch generation performance: PASSED ({:?})", duration);
}
