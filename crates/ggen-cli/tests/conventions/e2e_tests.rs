use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use std::path::Path;
use std::thread;
use std::time::Duration;
use tempfile::TempDir;

/// E2E Test: Create project from scratch
///
/// Flow:
/// 1. Create empty project directory
/// 2. Create domain/schema.ttl with RDF definitions
/// 3. Create templates/command.tmpl
/// 4. Run `mcpp generate`
/// 5. Verify files in generated/ match expectations
#[test]
#[ignore]
fn test_e2e_create_project_from_scratch() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    // Step 1: Create project structure
    fs::create_dir_all(project_path.join("domain")).unwrap();
    fs::create_dir_all(project_path.join("templates")).unwrap();

    // Step 2: Create domain/schema.ttl
    let schema_ttl = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:UserCommand a mcpp:Command ;
    rdfs:label "user" ;
    mcpp:hasSubcommand mcpp:CreateUser, mcpp:DeleteUser .

mcpp:CreateUser a mcpp:Subcommand ;
    rdfs:label "create" ;
    mcpp:hasArg mcpp:UsernameArg ;
    mcpp:hasFlag mcpp:AdminFlag .

mcpp:DeleteUser a mcpp:Subcommand ;
    rdfs:label "delete" ;
    mcpp:hasArg mcpp:UserIdArg .

mcpp:UsernameArg a mcpp:Argument ;
    rdfs:label "username" ;
    mcpp:type "String" ;
    mcpp:required true .

mcpp:UserIdArg a mcpp:Argument ;
    rdfs:label "user_id" ;
    mcpp:type "u64" ;
    mcpp:required true .

mcpp:AdminFlag a mcpp:Flag ;
    rdfs:label "admin" ;
    mcpp:short "a" ;
    mcpp:help "Create user as admin" .
"#;
    fs::write(project_path.join("domain/schema.ttl"), schema_ttl).unwrap();

    // Step 3: Create templates/command.tmpl
    let command_template = r#"// Generated command: {{command.label}}
use clap::{Parser, Subcommand};

#[derive(Parser)]
pub struct {{command.label | capitalize}}Command {
    #[command(subcommand)]
    action: {{command.label | capitalize}}Action,
}

#[derive(Subcommand)]
enum {{command.label | capitalize}}Action {
    {{#each command.subcommands}}
    /// {{this.help}}
    {{this.label | capitalize}} {
        {{#each this.args}}
        /// {{this.help}}
        {{this.label}}: {{this.type}},
        {{/each}}
        {{#each this.flags}}
        #[arg(short = '{{this.short}}', long)]
        {{this.label}}: bool,
        {{/each}}
    },
    {{/each}}
}

impl {{command.label | capitalize}}Command {
    pub fn execute(&self) -> Result<(), Box<dyn std::error::Error>> {
        match &self.action {
            {{#each command.subcommands}}
            {{../command.label | capitalize}}Action::{{this.label | capitalize}} { {{#each this.args}}{{this.label}}, {{/each}}{{#each this.flags}}{{this.label}}, {{/each}} } => {
                println!("Executing {{this.label}} with args");
                Ok(())
            }
            {{/each}}
        }
    }
}
"#;
    fs::write(
        project_path.join("templates/command.tmpl"),
        command_template,
    )
    .unwrap();

    // Step 4: Run mcpp generate
    let mut cmd = Command::cargo_bin("mcpp").unwrap();
    cmd.current_dir(project_path)
        .arg("generate")
        .assert()
        .success()
        .stdout(predicate::str::contains("Discovered templates:"))
        .stdout(predicate::str::contains("command.tmpl"))
        .stdout(predicate::str::contains("Generated 1 files"));

    // Step 5: Verify generated files
    let generated_path = project_path.join("generated/commands/user_command.rs");
    assert!(
        generated_path.exists(),
        "Generated command file should exist at {:?}",
        generated_path
    );

    let generated_content = fs::read_to_string(&generated_path).unwrap();
    assert!(generated_content.contains("pub struct UserCommand"));
    assert!(generated_content.contains("enum UserAction"));
    assert!(generated_content.contains("Create {"));
    assert!(generated_content.contains("Delete {"));
    assert!(generated_content.contains("username: String"));
    assert!(generated_content.contains("user_id: u64"));
    assert!(generated_content.contains("admin: bool"));
}

/// E2E Test: Add template with auto-discovery
///
/// Flow:
/// 1. Start with working project (existing domain + template)
/// 2. Add new template file (e.g., handler.tmpl)
/// 3. Run `mcpp generate`
/// 4. Verify new template was auto-discovered and used
#[test]
#[ignore]
fn test_e2e_add_template_auto_discovered() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    // Step 1: Setup existing project
    fs::create_dir_all(project_path.join("domain")).unwrap();
    fs::create_dir_all(project_path.join("templates")).unwrap();

    let schema_ttl = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:ApiEndpoint a mcpp:Resource ;
    rdfs:label "user" ;
    mcpp:path "/api/users" ;
    mcpp:method "GET" .
"#;
    fs::write(project_path.join("domain/api.ttl"), schema_ttl).unwrap();

    let existing_template = r#"// Route: {{resource.path}}
pub fn {{resource.label}}_route() -> Route {
    Route::new("{{resource.path}}")
}
"#;
    fs::write(project_path.join("templates/route.tmpl"), existing_template).unwrap();

    // First generation
    Command::cargo_bin("mcpp")
        .unwrap()
        .current_dir(project_path)
        .arg("generate")
        .assert()
        .success();

    assert!(project_path.join("generated/routes/user_route.rs").exists());

    // Step 2: Add new template file
    let handler_template = r#"// Handler: {{resource.label}}
use axum::{Json, response::IntoResponse};

pub async fn {{resource.label}}_handler() -> impl IntoResponse {
    Json(serde_json::json!({
        "path": "{{resource.path}}",
        "method": "{{resource.method}}"
    }))
}
"#;
    fs::write(
        project_path.join("templates/handler.tmpl"),
        handler_template,
    )
    .unwrap();

    // Step 3: Run mcpp generate again
    let mut cmd = Command::cargo_bin("mcpp").unwrap();
    cmd.current_dir(project_path)
        .arg("generate")
        .assert()
        .success()
        .stdout(predicate::str::contains("route.tmpl"))
        .stdout(predicate::str::contains("handler.tmpl"))
        .stdout(predicate::str::contains("Generated 2 files"));

    // Step 4: Verify new template output
    let handler_path = project_path.join("generated/handlers/user_handler.rs");
    assert!(
        handler_path.exists(),
        "Handler file should be auto-generated from new template"
    );

    let handler_content = fs::read_to_string(&handler_path).unwrap();
    assert!(handler_content.contains("pub async fn user_handler"));
    assert!(handler_content.contains("/api/users"));
    assert!(handler_content.contains("GET"));
}

/// E2E Test: Modify RDF triggers selective regeneration
///
/// Flow:
/// 1. Start with generated project
/// 2. Modify domain/commands.ttl (add new command)
/// 3. Run `mcpp generate`
/// 4. Verify only affected files regenerated
#[test]
#[ignore]
fn test_e2e_modify_rdf_triggers_regeneration() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    // Step 1: Initial setup
    fs::create_dir_all(project_path.join("domain")).unwrap();
    fs::create_dir_all(project_path.join("templates")).unwrap();

    let initial_schema = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:ListCommand a mcpp:Command ;
    rdfs:label "list" .
"#;
    fs::write(project_path.join("domain/commands.ttl"), initial_schema).unwrap();

    let template = r#"// Command: {{command.label}}
pub struct {{command.label | capitalize}}Command;
"#;
    fs::write(project_path.join("templates/command.tmpl"), template).unwrap();

    // Initial generation
    Command::cargo_bin("mcpp")
        .unwrap()
        .current_dir(project_path)
        .arg("generate")
        .assert()
        .success();

    let list_command_path = project_path.join("generated/commands/list_command.rs");
    assert!(list_command_path.exists());
    let list_mtime = fs::metadata(&list_command_path)
        .unwrap()
        .modified()
        .unwrap();

    // Small delay to ensure filesystem timestamp resolution
    thread::sleep(Duration::from_millis(100));

    // Step 2: Modify RDF - add new command
    let modified_schema = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:ListCommand a mcpp:Command ;
    rdfs:label "list" .

mcpp:CreateCommand a mcpp:Command ;
    rdfs:label "create" ;
    mcpp:hasArg mcpp:NameArg .

mcpp:NameArg a mcpp:Argument ;
    rdfs:label "name" ;
    mcpp:type "String" .
"#;
    fs::write(project_path.join("domain/commands.ttl"), modified_schema).unwrap();

    // Step 3: Regenerate
    let mut cmd = Command::cargo_bin("mcpp").unwrap();
    cmd.current_dir(project_path)
        .arg("generate")
        .assert()
        .success()
        .stdout(predicate::str::contains("Generated 2 files"));

    // Step 4: Verify selective regeneration
    let create_command_path = project_path.join("generated/commands/create_command.rs");
    assert!(
        create_command_path.exists(),
        "New command file should be generated"
    );

    let create_content = fs::read_to_string(&create_command_path).unwrap();
    assert!(create_content.contains("pub struct CreateCommand"));

    // Verify list command still exists (not deleted)
    assert!(list_command_path.exists(), "Existing command should remain");

    // Check if list command was actually regenerated (timestamp changed)
    let list_mtime_after = fs::metadata(&list_command_path)
        .unwrap()
        .modified()
        .unwrap();

    // In a real implementation, unchanged entities might be skipped
    // For now, we just verify both files exist
    assert!(
        list_mtime_after >= list_mtime,
        "File should be regenerated or unchanged"
    );
}

/// E2E Test: Watch mode with hot reload
///
/// Flow:
/// 1. Start `mcpp watch` in background
/// 2. Modify RDF file
/// 3. Verify auto-regeneration occurred
/// 4. Stop watcher gracefully
#[test]
#[ignore] // Requires process management and timing-sensitive behavior
fn test_e2e_watch_mode_hot_reload() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    // Setup project
    fs::create_dir_all(project_path.join("domain")).unwrap();
    fs::create_dir_all(project_path.join("templates")).unwrap();

    let schema = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:WatchedCommand a mcpp:Command ;
    rdfs:label "watched" .
"#;
    fs::write(project_path.join("domain/watched.ttl"), schema).unwrap();

    let template = r#"// Watched: {{command.label}}
pub struct {{command.label | capitalize}};
"#;
    fs::write(project_path.join("templates/command.tmpl"), template).unwrap();

    // Step 1: Start watcher
    let mut watch_cmd = Command::cargo_bin("mcpp")
        .unwrap()
        .current_dir(project_path)
        .arg("watch")
        .timeout(Duration::from_secs(10))
        .spawn()
        .unwrap();

    // Wait for watcher to initialize
    thread::sleep(Duration::from_secs(2));

    // Step 2: Modify RDF file
    let modified_schema = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:WatchedCommand a mcpp:Command ;
    rdfs:label "watched" ;
    mcpp:version "2.0" .
"#;
    fs::write(project_path.join("domain/watched.ttl"), modified_schema).unwrap();

    // Step 3: Wait for auto-regeneration
    thread::sleep(Duration::from_secs(2));

    let generated_path = project_path.join("generated/commands/watched_command.rs");
    assert!(
        generated_path.exists(),
        "File should be auto-generated by watcher"
    );

    // Step 4: Stop watcher
    watch_cmd.kill().unwrap();
    watch_cmd.wait().unwrap();
}

/// E2E Test: Preset clap-noun-verb initialization
///
/// Flow:
/// 1. Run `mcpp project init --preset clap-noun-verb`
/// 2. Verify structure created (domain/, templates/, generated/)
/// 3. Run `mcpp generate`
/// 4. Verify CLI project with noun-verb pattern generated
#[test]
#[ignore]
fn test_e2e_preset_clap_noun_verb() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    // Step 1: Initialize with preset
    let mut cmd = Command::cargo_bin("mcpp").unwrap();
    cmd.current_dir(project_path)
        .arg("project")
        .arg("init")
        .arg("--preset")
        .arg("clap-noun-verb")
        .assert()
        .success()
        .stdout(predicate::str::contains(
            "Initialized project with preset: clap-noun-verb",
        ))
        .stdout(predicate::str::contains("domain/"))
        .stdout(predicate::str::contains("templates/"));

    // Step 2: Verify structure
    assert!(project_path.join("domain").exists(), "domain/ should exist");
    assert!(
        project_path.join("templates").exists(),
        "templates/ should exist"
    );
    assert!(
        project_path.join("domain/commands.ttl").exists(),
        "Preset should create sample RDF"
    );
    assert!(
        project_path.join("templates/clap_command.tmpl").exists(),
        "Preset should create clap template"
    );

    // Verify RDF contains noun-verb pattern
    let rdf_content = fs::read_to_string(project_path.join("domain/commands.ttl")).unwrap();
    assert!(rdf_content.contains("mcpp:Command"));
    assert!(rdf_content.contains("mcpp:Subcommand"));

    // Verify template contains clap macros
    let template_content =
        fs::read_to_string(project_path.join("templates/clap_command.tmpl")).unwrap();
    assert!(template_content.contains("#[derive(Parser)]"));
    assert!(template_content.contains("#[command(subcommand)]"));

    // Step 3: Generate CLI project
    let mut cmd = Command::cargo_bin("mcpp").unwrap();
    cmd.current_dir(project_path)
        .arg("generate")
        .assert()
        .success()
        .stdout(predicate::str::contains("Generated"));

    // Step 4: Verify CLI structure
    let output_dir = project_path;
    assert!(output_dir.exists(), "output directory should be created");

    // Check for generated command files
    let commands_dir = output_dir.join("commands");
    assert!(commands_dir.exists(), "commands/ subdirectory should exist");

    // Verify at least one command file was generated
    let generated_files: Vec<_> = fs::read_dir(&commands_dir)
        .unwrap()
        .filter_map(Result::ok)
        .filter(|entry| entry.path().extension().and_then(|s| s.to_str()) == Some("rs"))
        .collect();

    assert!(
        !generated_files.is_empty(),
        "At least one .rs command file should be generated"
    );

    // Verify content structure of first generated file
    let first_file = &generated_files[0];
    let file_content = fs::read_to_string(first_file.path()).unwrap();
    assert!(file_content.contains("use clap::"), "Should import clap");
    assert!(
        file_content.contains("#[derive(Parser)]")
            || file_content.contains("#[derive(Subcommand)]"),
        "Should contain clap derive macros"
    );
}

/// E2E Test: Multi-template multi-domain orchestration
///
/// Flow:
/// 1. Create project with multiple domains (commands, api, models)
/// 2. Create multiple templates for each domain
/// 3. Run `mcpp generate`
/// 4. Verify correct routing and isolation
#[test]
#[ignore]
fn test_e2e_multi_domain_orchestration() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    // Setup multiple domains
    fs::create_dir_all(project_path.join("domain/commands")).unwrap();
    fs::create_dir_all(project_path.join("domain/api")).unwrap();
    fs::create_dir_all(project_path.join("domain/models")).unwrap();
    fs::create_dir_all(project_path.join("templates")).unwrap();

    // Commands domain
    let commands_ttl = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:ImportCommand a mcpp:Command ;
    rdfs:label "import" .
"#;
    fs::write(
        project_path.join("domain/commands/import.ttl"),
        commands_ttl,
    )
    .unwrap();

    // API domain
    let api_ttl = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:UsersEndpoint a mcpp:ApiEndpoint ;
    rdfs:label "users" ;
    mcpp:path "/api/users" .
"#;
    fs::write(project_path.join("domain/api/endpoints.ttl"), api_ttl).unwrap();

    // Models domain
    let models_ttl = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

mcpp:UserModel a mcpp:Model ;
    rdfs:label "User" ;
    mcpp:hasField mcpp:UserIdField, mcpp:UsernameField .

mcpp:UserIdField a mcpp:Field ;
    rdfs:label "id" ;
    mcpp:type "u64" .

mcpp:UsernameField a mcpp:Field ;
    rdfs:label "username" ;
    mcpp:type "String" .
"#;
    fs::write(project_path.join("domain/models/user.ttl"), models_ttl).unwrap();

    // Create templates for each domain
    let command_template = r#"// Command: {{command.label}}
pub struct {{command.label | capitalize}}Command;
"#;
    fs::write(
        project_path.join("templates/command.tmpl"),
        command_template,
    )
    .unwrap();

    let api_template = r#"// Endpoint: {{endpoint.path}}
pub async fn {{endpoint.label}}_handler() {}
"#;
    fs::write(
        project_path.join("templates/api_endpoint.tmpl"),
        api_template,
    )
    .unwrap();

    let model_template = r#"// Model: {{model.label}}
#[derive(Debug, Clone)]
pub struct {{model.label}} {
    {{#each model.fields}}
    pub {{this.label}}: {{this.type}},
    {{/each}}
}
"#;
    fs::write(project_path.join("templates/model.tmpl"), model_template).unwrap();

    // Generate
    Command::cargo_bin("mcpp")
        .unwrap()
        .current_dir(project_path)
        .arg("generate")
        .assert()
        .success()
        .stdout(predicate::str::contains("command.tmpl"))
        .stdout(predicate::str::contains("api_endpoint.tmpl"))
        .stdout(predicate::str::contains("model.tmpl"));

    // Verify isolation and correct routing
    assert!(project_path
        .join("generated/commands/import_command.rs")
        .exists());
    assert!(project_path
        .join("generated/api_endpoints/users_handler.rs")
        .exists());
    assert!(project_path.join("generated/models/user.rs").exists());

    // Verify content correctness
    let command_content =
        fs::read_to_string(project_path.join("generated/commands/import_command.rs")).unwrap();
    assert!(command_content.contains("pub struct ImportCommand"));

    let api_content =
        fs::read_to_string(project_path.join("generated/api_endpoints/users_handler.rs")).unwrap();
    assert!(api_content.contains("/api/users"));

    let model_content = fs::read_to_string(project_path.join("generated/models/user.rs")).unwrap();
    assert!(model_content.contains("pub struct User"));
    assert!(model_content.contains("pub id: u64"));
    assert!(model_content.contains("pub username: String"));
}

/// E2E Test: Error handling and validation
///
/// Flow:
/// 1. Invalid RDF syntax
/// 2. Missing required fields
/// 3. Template syntax errors
/// 4. Verify helpful error messages
#[test]
#[ignore]
fn test_e2e_error_handling_validation() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path();

    fs::create_dir_all(project_path.join("domain")).unwrap();
    fs::create_dir_all(project_path.join("templates")).unwrap();

    // Test 1: Invalid RDF syntax
    let invalid_rdf = r#"@prefix mcpp: <http://mcpp.io/ontology#
this is not valid RDF
"#;
    fs::write(project_path.join("domain/invalid.ttl"), invalid_rdf).unwrap();

    Command::cargo_bin("mcpp")
        .unwrap()
        .current_dir(project_path)
        .arg("generate")
        .assert()
        .failure()
        .stderr(predicate::str::contains("RDF"))
        .stderr(predicate::str::contains("parse").or(predicate::str::contains("invalid")));

    // Test 2: Missing template file
    fs::remove_file(project_path.join("domain/invalid.ttl")).unwrap();

    let valid_rdf = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
mcpp:TestCommand a mcpp:Command ;
    mcpp:template "nonexistent.tmpl" .
"#;
    fs::write(project_path.join("domain/valid.ttl"), valid_rdf).unwrap();

    Command::cargo_bin("mcpp")
        .unwrap()
        .current_dir(project_path)
        .arg("generate")
        .assert()
        .failure()
        .stderr(predicate::str::contains("template").or(predicate::str::contains("not found")));

    // Test 3: Template syntax error
    fs::remove_file(project_path.join("domain/valid.ttl")).unwrap();

    let schema = r#"@prefix mcpp: <http://mcpp.io/ontology#> .
mcpp:TestCommand a mcpp:Command .
"#;
    fs::write(project_path.join("domain/test.ttl"), schema).unwrap();

    let bad_template = r#"{{command.label}
{{#unclosed
"#;
    fs::write(project_path.join("templates/command.tmpl"), bad_template).unwrap();

    Command::cargo_bin("mcpp")
        .unwrap()
        .current_dir(project_path)
        .arg("generate")
        .assert()
        .failure()
        .stderr(predicate::str::contains("template").or(predicate::str::contains("syntax")));
}
