# Agent 10: End-to-End Validation - Complete System Verification

**Mission**: Execute comprehensive end-to-end validation proving ggen v2.0.0 works for real users.

**Approach**: Chicago TDD - Test complete workflows with REAL operations, not mocks.

**80/20 Focus**: Critical user journeys that deliver 80% of value.

---

## Validation Strategy

### Critical User Journeys (80/20)

1. **Install → Generate → Build**: Complete new user onboarding
2. **Marketplace → Template → Render**: Template consumption workflow
3. **RDF → Query → Generate**: Knowledge graph-driven generation
4. **Error Handling**: Real-world failure scenarios

**Skipped** (20% edge cases):
- Offline mode
- Network retry logic
- Corrupted cache recovery
- Unicode edge cases in templates
- Platform-specific path separators

---

## E2E Test Scenarios

### Scenario 1: Complete New User Journey
```rust
// tests/e2e_v2/complete_user_journey.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_new_user_complete_workflow() {
    // Setup: Fresh environment
    let workspace = TempDir::new().unwrap();

    // 1. User installs ggen v2.0.0 (simulated by cargo build)
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("--version")
        .assert()
        .success()
        .stdout(predicates::str::contains("ggen 2.0.0"));

    // 2. Search marketplace for rust templates
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("search").arg("rust")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains("cli-subcommand"));

    // 3. Install template from marketplace
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("install").arg("io.ggen.rust.cli-subcommand")
        .current_dir(&workspace)
        .assert()
        .success();

    // Verify lockfile created
    let lockfile = workspace.path().join("ggen.lock");
    assert!(lockfile.exists());
    let lockfile_content = fs::read_to_string(&lockfile).unwrap();
    assert!(lockfile_content.contains("io.ggen.rust.cli-subcommand"));

    // 4. Generate project using installed template
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template").arg("generate")
        .arg("io.ggen.rust.cli-subcommand:rust.tmpl")
        .arg("my-cli-app")
        .current_dir(&workspace)
        .assert()
        .success();

    // Verify generated files exist
    let project_dir = workspace.path().join("my-cli-app");
    assert!(project_dir.exists());
    assert!(project_dir.join("Cargo.toml").exists());
    assert!(project_dir.join("src/main.rs").exists());

    // 5. Build generated project
    let mut build_cmd = Command::new("cargo");
    build_cmd.arg("build")
        .current_dir(&project_dir)
        .assert()
        .success();

    // 6. Run generated binary
    let binary_path = project_dir.join("target/debug/my-cli-app");
    assert!(binary_path.exists());

    let mut run_cmd = Command::new(&binary_path);
    run_cmd.arg("--help")
        .assert()
        .success()
        .stdout(predicates::str::contains("Usage:"));
}
```

**Expected Result**: ✅ User goes from zero to working CLI app in 6 commands

---

### Scenario 2: Template Customization with RDF
```rust
// tests/e2e_v2/rdf_template_workflow.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_rdf_driven_code_generation() {
    let workspace = TempDir::new().unwrap();

    // 1. Create RDF data file
    let rdf_data = r#"
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:MyAPI rdf:type :RestAPI ;
    :hasEndpoint :UsersEndpoint ;
    :hasEndpoint :PostsEndpoint .

:UsersEndpoint :path "/users" ;
    :method "GET" ;
    :returns :UserList .

:PostsEndpoint :path "/posts" ;
    :method "GET" ;
    :returns :PostList .
"#;
    let rdf_file = workspace.path().join("api.ttl");
    fs::write(&rdf_file, rdf_data).unwrap();

    // 2. Create template that queries RDF
    let template = r#"
// Generated REST API
{% for endpoint in endpoints %}
pub async fn {{ endpoint.name }}() -> impl Responder {
    // {{ endpoint.method }} {{ endpoint.path }}
    HttpResponse::Ok().json(vec![])
}
{% endfor %}
"#;
    let template_file = workspace.path().join("api.tmpl");
    fs::write(&template_file, template).unwrap();

    // 3. Run template with RDF data
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template").arg("render")
        .arg(&template_file)
        .arg("--rdf").arg(&rdf_file)
        .arg("--query").arg("SELECT ?endpoint WHERE { :MyAPI :hasEndpoint ?endpoint }")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains("pub async fn"))
        .stdout(predicates::str::contains("GET /users"))
        .stdout(predicates::str::contains("GET /posts"));
}
```

**Expected Result**: ✅ RDF data drives code generation, proving graph-aware capabilities

---

### Scenario 3: Marketplace Search and Discovery
```rust
// tests/e2e_v2/marketplace_discovery.rs

use assert_cmd::Command;
use tempfile::TempDir;

#[test]
fn test_marketplace_full_workflow() {
    let workspace = TempDir::new().unwrap();

    // 1. Search with no results
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("search").arg("nonexistent-template-xyz")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains("No templates found"));

    // 2. Search with filters
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("search")
        .arg("--language").arg("rust")
        .arg("--category").arg("cli")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains("cli-subcommand"));

    // 3. Get template details
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("info").arg("io.ggen.rust.cli-subcommand")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains("Description:"))
        .stdout(predicates::str::contains("Latest Version:"))
        .stdout(predicates::str::contains("Author:"));

    // 4. List installed templates (should be empty)
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("list")
        .current_dir(&workspace)
        .assert()
        .success();

    // 5. Install template
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("install").arg("io.ggen.rust.cli-subcommand")
        .current_dir(&workspace)
        .assert()
        .success();

    // 6. List installed templates (should show installed)
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("list")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains("io.ggen.rust.cli-subcommand"));

    // 7. Update template (idempotent)
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("update").arg("io.ggen.rust.cli-subcommand")
        .current_dir(&workspace)
        .assert()
        .success();
}
```

**Expected Result**: ✅ Complete marketplace workflow from discovery to installation

---

### Scenario 4: Error Handling and Recovery
```rust
// tests/e2e_v2/error_handling.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_graceful_error_handling() {
    let workspace = TempDir::new().unwrap();

    // 1. Template not found
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template").arg("generate")
        .arg("nonexistent-template")
        .current_dir(&workspace)
        .assert()
        .failure()
        .stderr(predicates::str::contains("Template not found"));

    // 2. Invalid RDF syntax
    let bad_rdf = "@prefix : <>. INVALID SYNTAX HERE";
    let rdf_file = workspace.path().join("bad.ttl");
    fs::write(&rdf_file, bad_rdf).unwrap();

    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("rdf").arg("validate")
        .arg(&rdf_file)
        .current_dir(&workspace)
        .assert()
        .failure()
        .stderr(predicates::str::contains("Parse error"));

    // 3. Missing required variables
    let template = "Hello {{ required_var }}!";
    let template_file = workspace.path().join("incomplete.tmpl");
    fs::write(&template_file, template).unwrap();

    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template").arg("render")
        .arg(&template_file)
        .current_dir(&workspace)
        .assert()
        .failure()
        .stderr(predicates::str::contains("required_var"));

    // 4. Invalid marketplace package ID
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("install").arg("invalid..package.id")
        .current_dir(&workspace)
        .assert()
        .failure()
        .stderr(predicates::str::contains("Invalid package"));
}
```

**Expected Result**: ✅ Clear, actionable error messages for common failures

---

### Scenario 5: Multi-Language Template Generation
```rust
// tests/e2e_v2/multilang_generation.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_multiple_language_templates() {
    let workspace = TempDir::new().unwrap();

    // Generate Rust project
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("project").arg("new")
        .arg("--template").arg("rust-cli")
        .arg("rust-app")
        .current_dir(&workspace)
        .assert()
        .success();

    assert!(workspace.path().join("rust-app/Cargo.toml").exists());

    // Generate Python project
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("project").arg("new")
        .arg("--template").arg("python-web")
        .arg("python-app")
        .current_dir(&workspace)
        .assert()
        .success();

    assert!(workspace.path().join("python-app/pyproject.toml").exists());

    // Generate TypeScript project
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("project").arg("new")
        .arg("--template").arg("typescript-api")
        .arg("ts-app")
        .current_dir(&workspace)
        .assert()
        .success();

    assert!(workspace.path().join("ts-app/package.json").exists());
}
```

**Expected Result**: ✅ Multi-language support verified across ecosystems

---

### Scenario 6: RDF Graph Query Integration
```rust
// tests/e2e_v2/rdf_query_workflow.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_rdf_query_to_code_generation() {
    let workspace = TempDir::new().unwrap();

    // 1. Load RDF graph
    let schema = r#"
@prefix schema: <http://schema.org/> .
@prefix : <http://example.org/> .

:Person a schema:Class ;
    schema:property :name, :email, :age .

:name a schema:Property ;
    schema:rangeIncludes schema:Text .

:email a schema:Property ;
    schema:rangeIncludes schema:Text .

:age a schema:Property ;
    schema:rangeIncludes schema:Integer .
"#;
    let schema_file = workspace.path().join("schema.ttl");
    fs::write(&schema_file, schema).unwrap();

    // 2. Query for all properties
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("rdf").arg("query")
        .arg(&schema_file)
        .arg("SELECT ?property WHERE { :Person schema:property ?property }")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains(":name"))
        .stdout(predicates::str::contains(":email"))
        .stdout(predicates::str::contains(":age"));

    // 3. Generate code from query results
    let template = r#"
pub struct Person {
{% for prop in properties %}
    pub {{ prop.name }}: {{ prop.type }},
{% endfor %}
}
"#;
    let template_file = workspace.path().join("struct.tmpl");
    fs::write(&template_file, template).unwrap();

    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template").arg("render")
        .arg(&template_file)
        .arg("--rdf").arg(&schema_file)
        .arg("--query").arg("SELECT ?property WHERE { :Person schema:property ?property }")
        .current_dir(&workspace)
        .assert()
        .success()
        .stdout(predicates::str::contains("pub struct Person"))
        .stdout(predicates::str::contains("pub name:"))
        .stdout(predicates::str::contains("pub email:"));
}
```

**Expected Result**: ✅ RDF queries seamlessly integrate with template rendering

---

### Scenario 7: Template Version Management
```rust
// tests/e2e_v2/template_versioning.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_template_version_pinning() {
    let workspace = TempDir::new().unwrap();

    // 1. Install specific version
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("install")
        .arg("io.ggen.rust.cli-subcommand@1.1.0")
        .current_dir(&workspace)
        .assert()
        .success();

    // Verify lockfile has correct version
    let lockfile = workspace.path().join("ggen.lock");
    let content = fs::read_to_string(&lockfile).unwrap();
    assert!(content.contains("version = \"1.1.0\""));

    // 2. Upgrade to latest
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("upgrade")
        .arg("io.ggen.rust.cli-subcommand")
        .current_dir(&workspace)
        .assert()
        .success();

    // Verify version updated
    let content = fs::read_to_string(&lockfile).unwrap();
    assert!(content.contains("version = \"1.2.0\""));

    // 3. Downgrade to specific version
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("install")
        .arg("io.ggen.rust.cli-subcommand@1.0.0")
        .arg("--force")
        .current_dir(&workspace)
        .assert()
        .success();

    let content = fs::read_to_string(&lockfile).unwrap();
    assert!(content.contains("version = \"1.0.0\""));
}
```

**Expected Result**: ✅ Version management works consistently

---

### Scenario 8: Deterministic Output Verification
```rust
// tests/e2e_v2/deterministic_output.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_deterministic_code_generation() {
    let workspace = TempDir::new().unwrap();

    // Generate same template 3 times
    for i in 1..=3 {
        let output_dir = workspace.path().join(format!("output-{}", i));
        fs::create_dir_all(&output_dir).unwrap();

        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.arg("template").arg("generate")
            .arg("rust-cli")
            .arg("test-app")
            .current_dir(&output_dir)
            .assert()
            .success();
    }

    // Compare all 3 outputs
    let file1 = fs::read_to_string(
        workspace.path().join("output-1/test-app/src/main.rs")
    ).unwrap();
    let file2 = fs::read_to_string(
        workspace.path().join("output-2/test-app/src/main.rs")
    ).unwrap();
    let file3 = fs::read_to_string(
        workspace.path().join("output-3/test-app/src/main.rs")
    ).unwrap();

    // All outputs must be byte-for-byte identical
    assert_eq!(file1, file2);
    assert_eq!(file2, file3);
}
```

**Expected Result**: ✅ Deterministic generation verified

---

### Scenario 9: Performance Benchmarking
```rust
// tests/e2e_v2/performance_validation.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::time::Instant;

#[test]
fn test_performance_requirements() {
    let workspace = TempDir::new().unwrap();

    // 1. Template generation should complete < 5s
    let start = Instant::now();
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("project").arg("new")
        .arg("--template").arg("rust-cli")
        .arg("perf-test")
        .current_dir(&workspace)
        .assert()
        .success();
    let duration = start.elapsed();

    assert!(duration.as_secs() < 5,
        "Template generation took {:?}, should be < 5s", duration);

    // 2. Marketplace search should complete < 2s
    let start = Instant::now();
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("marketplace").arg("search").arg("rust")
        .current_dir(&workspace)
        .assert()
        .success();
    let duration = start.elapsed();

    assert!(duration.as_secs() < 2,
        "Marketplace search took {:?}, should be < 2s", duration);

    // 3. RDF query should complete < 1s
    let rdf_file = workspace.path().join("data.ttl");
    std::fs::write(&rdf_file, "@prefix : <http://example.org/> . :a :b :c .").unwrap();

    let start = Instant::now();
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("rdf").arg("query")
        .arg(&rdf_file)
        .arg("SELECT * WHERE { ?s ?p ?o }")
        .current_dir(&workspace)
        .assert()
        .success();
    let duration = start.elapsed();

    assert!(duration.as_secs() < 1,
        "RDF query took {:?}, should be < 1s", duration);
}
```

**Expected Result**: ✅ Performance meets user expectations

---

### Scenario 10: GitHub Integration Workflow
```rust
// tests/e2e_v2/github_integration.rs

use assert_cmd::Command;
use tempfile::TempDir;
use std::fs;

#[test]
#[ignore] // Requires network and GitHub token
fn test_github_pages_deployment() {
    let workspace = TempDir::new().unwrap();

    // 1. Generate project with GitHub Pages config
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("project").arg("new")
        .arg("--template").arg("docs-site")
        .arg("--github-pages")
        .arg("my-docs")
        .current_dir(&workspace)
        .assert()
        .success();

    // Verify GitHub Actions workflow created
    let workflow = workspace.path()
        .join("my-docs/.github/workflows/deploy.yml");
    assert!(workflow.exists());

    let content = fs::read_to_string(&workflow).unwrap();
    assert!(content.contains("github-pages"));

    // 2. Deploy to GitHub Pages
    cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("deploy")
        .arg("--github-pages")
        .arg("--repo").arg("username/my-docs")
        .current_dir(workspace.path().join("my-docs"))
        .assert()
        .success();
}
```

**Expected Result**: ✅ GitHub integration simplifies deployment

---

## Validation Metrics

### Success Criteria

| Metric | Target | Actual |
|--------|--------|--------|
| Scenario Pass Rate | 100% | TBD |
| Performance < 5s | Yes | TBD |
| Error Messages Clear | Yes | TBD |
| Multi-language Support | 3+ | TBD |
| Deterministic Output | 100% | TBD |

### Test Execution Plan

```bash
# Run all E2E tests
cargo test --test e2e_v2 -- --test-threads=1

# Run with detailed output
cargo test --test e2e_v2 -- --test-threads=1 --nocapture

# Run specific scenario
cargo test test_new_user_complete_workflow -- --nocapture

# Performance benchmarks
cargo test test_performance_requirements -- --nocapture --ignored
```

---

## Implementation Status

- [ ] Scenario 1: Complete User Journey
- [ ] Scenario 2: RDF Template Workflow
- [ ] Scenario 3: Marketplace Discovery
- [ ] Scenario 4: Error Handling
- [ ] Scenario 5: Multi-Language Generation
- [ ] Scenario 6: RDF Query Integration
- [ ] Scenario 7: Template Versioning
- [ ] Scenario 8: Deterministic Output
- [ ] Scenario 9: Performance Validation
- [ ] Scenario 10: GitHub Integration

---

## Notes

**Chicago TDD Principles Applied**:
- ✅ Real user workflows, not unit tests
- ✅ Actual CLI commands executed
- ✅ Generated code built and run
- ✅ Real marketplace interactions
- ✅ Actual RDF parsing and queries

**80/20 Focus Maintained**:
- ✅ Core workflows that 80% of users need
- ❌ Skipped rare edge cases and platform-specific quirks
- ✅ Performance validated for common operations
- ✅ Error handling covers typical mistakes

**Next Steps**:
1. Implement test infrastructure in `tests/e2e_v2/`
2. Create helper utilities for test setup
3. Run scenarios and document results
4. Fix any discovered integration issues
5. Validate against v2.0.0 requirements
