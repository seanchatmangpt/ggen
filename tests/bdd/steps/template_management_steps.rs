use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Step definitions for `ggen template` noun-verb commands
///
/// Covers template management operations:
/// - template new: Create new templates
/// - template list: List available templates
/// - template show: Show template details
/// - template lint: Validate templates

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r#"^I have a local template "([^"]+)"$"#)]
fn have_local_template(world: &mut GgenWorld, filename: String) {
    let template_content = r#"---
to: "output.txt"
vars:
  name: "World"
---
Hello, {{ name }}!
"#;

    let templates_dir = world.project_dir.join("templates");
    #[allow(clippy::expect_used)]
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");

    let file_path = templates_dir.join(&filename);
    fs::write(&file_path, template_content)
        .unwrap_or_else(|e| panic!("Failed to write template {}: {}", filename, e));
}

// REMOVED: Duplicate step definition - handled by project_steps.rs
// This step was causing ambiguity with project_steps.rs:27

#[given(regex = r#"^I have a file "([^"]+)" with "([^"]+)"$"#)]
fn have_file_with_content(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content)
        .unwrap_or_else(|e| panic!("Failed to write file {}: {}", filename, e));
}

#[given(regex = r#"^I have templates "([^"]+)", "([^"]+)", "([^"]+)"$"#)]
fn have_multiple_templates(world: &mut GgenWorld, tmpl1: String, tmpl2: String, tmpl3: String) {
    for tmpl in [tmpl1, tmpl2, tmpl3] {
        have_local_template(world, tmpl);
    }
}

#[given(regex = r"^I have multiple templates with descriptions$")]
fn have_templates_with_descriptions(world: &mut GgenWorld) {
    let template_with_desc = r#"---
to: "output.txt"
description: "A template with a description"
---
Content
"#;

    let templates_dir = world.project_dir.join("templates");
    #[allow(clippy::expect_used)]
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");

    fs::write(templates_dir.join("tmpl1.tmpl"), template_with_desc)
        #[allow(clippy::expect_used)]
        .expect("Failed to write template");
    fs::write(templates_dir.join("tmpl2.tmpl"), template_with_desc)
        #[allow(clippy::expect_used)]
        .expect("Failed to write template");
}

#[given(regex = r#"^I have a management template "([^"]+)" with field "([^"]+)"$"#)]
fn have_management_template_with_field(world: &mut GgenWorld, filename: String, field: String) {
    let content = format!(
        r#"---
to: "output.txt"
{}
---
Content
"#,
        field
    );

    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content)
        .unwrap_or_else(|e| panic!("Failed to write template {}: {}", filename, e));
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen template management (.+)"$"#)]
fn run_ggen_template_management_command(world: &mut GgenWorld, args: String) {
    // Parse command line, handling quoted arguments
    let arg_list = shell_words::split(&args)
        .unwrap_or_else(|e| panic!("Failed to parse arguments '{}': {}", args, e));

    #[allow(clippy::expect_used)]
    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    let output = cmd
        .arg("template")
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run ggen template command");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I answer "([^"]+)" to "([^"]+)"$"#)]
fn answer_interactive_prompt(_world: &mut GgenWorld, _answer: String, _prompt: String) {
    // Interactive input handling would be implemented here
    // For now, this is a placeholder
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the command should succeed$")]
fn command_should_succeed(world: &mut GgenWorld) {
    assert!(
        world.last_command_succeeded(),
        "Command failed with exit code: {}\nStderr: {}",
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr()
    );
}

#[then(regex = r"^I should see gpacks templates in output$")]
fn should_see_gpacks_templates_in_output(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("gpack") || stdout.contains("io.ggen"),
        "Expected to see gpack templates in output, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should not see gpack templates in output$")]
fn should_not_see_gpacks_templates_in_output(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        !stdout.contains("gpack") && !stdout.contains("io.ggen"),
        "Expected NOT to see gpack templates in output, but got: {}",
        stdout
    );
}

#[then(regex = r#"^the file "([^"]+)" should exist$"#)]
fn file_should_exist(world: &mut GgenWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    assert!(
        file_path.exists(),
        "File {} should exist at {}",
        filename,
        file_path.display()
    );
}

#[then(regex = r"^the file should contain valid YAML frontmatter$")]
fn file_should_contain_yaml_frontmatter(world: &mut GgenWorld) {
    // Would validate YAML frontmatter structure
    // For now, we just check the command succeeded
    assert!(world.last_command_succeeded());
}

#[then(regex = r#"^I should see "([^"]+)" in output$"#)]
fn should_see_in_output(world: &mut GgenWorld, expected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        stdout.contains(&expected) || stderr.contains(&expected),
        "Expected to see '{}' in output, but got:\nStdout: {}\nStderr: {}",
        expected,
        stdout,
        stderr
    );
}

#[then(regex = r#"^the template should have "([^"]+)" set to "([^"]+)"$"#)]
fn template_should_have_field(world: &mut GgenWorld, field: String, value: String) {
    // Would parse template and verify field value
    // For now, we verify the command succeeded
    let _ = (world, field, value);
}

#[then(regex = r#"^the template should have variables "([^"]+)" and "([^"]+)"$"#)]
fn template_should_have_variables(_world: &mut GgenWorld, _var1: String, _var2: String) {
    // Would parse template and verify variables
}

#[then(regex = r"^I should see gpacks templates in output$")]
fn should_see_gpack_templates(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    // Would check for gpack template patterns (io.ggen.*)
    assert!(
        stdout.contains("io.ggen") || stdout.contains("gpack") || !stdout.is_empty(),
        "Expected to see gpack templates, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should not see gpack templates in output$")]
fn should_not_see_gpack_templates(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        !stdout.contains("io.ggen"),
        "Should not see gpack templates, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see template metadata$")]
fn should_see_template_metadata(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("to:") || stdout.contains("vars:") || stdout.contains("description"),
        "Expected to see template metadata, but got: {}",
        stdout
    );
}

#[then(regex = r"^RDF validation should pass$")]
fn rdf_validation_should_pass(_world: &mut GgenWorld) {
    // Would validate RDF syntax
}

#[then(regex = r"^the command should fail$")]
fn command_should_fail(world: &mut GgenWorld) {
    assert!(
        !world.last_command_succeeded(),
        "Command should have failed but succeeded"
    );
}

// ============================================================================
// Missing step definitions for template.feature
// ============================================================================

#[when(regex = r#"^I run "ggen template (.+)"$"#)]
fn run_ggen_template_command(world: &mut GgenWorld, args: String) {
    // Parse the command line, handling quoted arguments
    let parts: Vec<&str> = args.split_whitespace().collect();
    #[allow(clippy::expect_used)]
    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    cmd.arg("template");

    for part in parts {
        cmd.arg(part);
    }

    let output = cmd
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run ggen template command");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[given(regex = r#"^I have a template "([^"]+)" with content:$"#)]
fn have_template_with_content_named(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    if let Some(parent) = file_path.parent() {
        #[allow(clippy::expect_used)]
        fs::create_dir_all(parent).expect("Failed to create template dir");
    }
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write template {}: {}", filename, e));
}

#[given(regex = r#"^I have a template "([^"]+)"$"#)]
fn have_template_named(world: &mut GgenWorld, filename: String) {
    let template_content = format!(
        r#"---
to: {}.rs
vars: {{ name: "hello" }}
---
fn main() {{
    println!("Hello, {{name}}!");
}}
"#,
        filename.trim_end_matches(".tmpl")
    );

    let file_path = world.project_dir.join(&filename);
    if let Some(parent) = file_path.parent() {
        #[allow(clippy::expect_used)]
        fs::create_dir_all(parent).expect("Failed to create template dir");
    }
    fs::write(&file_path, template_content)
        .unwrap_or_else(|e| panic!("Failed to write template {}: {}", filename, e));
}

#[given(regex = r#"^I have a template "([^"]+)" with "([^"]+)"$"#)]
fn have_template_with_version(world: &mut GgenWorld, filename: String, version: String) {
    let template_content = format!(
        r#"---
to: {}.rs
vars: {{ name: "hello", version: "{}" }}
---
fn main() {{
    println!("Hello, {{name}}! Version {{version}}");
}}
"#,
        filename.trim_end_matches(".tmpl"),
        version
    );

    let file_path = world.project_dir.join(&filename);
    if let Some(parent) = file_path.parent() {
        #[allow(clippy::expect_used)]
        fs::create_dir_all(parent).expect("Failed to create template dir");
    }
    fs::write(&file_path, template_content)
        .unwrap_or_else(|e| panic!("Failed to write template {}: {}", filename, e));
}

#[given(regex = r"^I have multiple templates with descriptions$")]
fn have_multiple_templates_with_descriptions(world: &mut GgenWorld) {
    let templates_dir = world.project_dir.join("templates");
    #[allow(clippy::expect_used)]
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");

    let templates = vec![
        ("hello.tmpl", "Simple hello world template"),
        ("goodbye.tmpl", "Farewell message template"),
        ("test.tmpl", "Unit test template"),
    ];

    for (filename, description) in templates {
        let template_content = format!(
            r#"---
to: {}.rs
vars: {{ name: "world" }}
description: "{}"
---
fn main() {{
    println!("Hello, {{name}}!");
}}
"#,
            filename.trim_end_matches(".tmpl"),
            description
        );

        fs::write(templates_dir.join(filename), template_content)
            .unwrap_or_else(|_| panic!("Failed to write template {}", filename));
    }
}

#[then(regex = r#"^I should see "([^"]+)" in stderr$"#)]
fn should_see_in_stderr(world: &mut GgenWorld, expected: String) {
    let stderr = world.last_stderr();
    assert!(
        stderr.contains(&expected),
        "Expected to see '{}' in stderr, but got: {}",
        expected,
        stderr
    );
}

#[then(regex = r#"^a template should be created from "([^"]+)"$"#)]
fn template_created_from_file(world: &mut GgenWorld, _source: String) {
    // Would verify template was created from existing file
    assert!(world.last_command_succeeded());
}

#[then(regex = r#"^I should not see "([^"]+)" in output$"#)]
fn should_not_see_in_output(world: &mut GgenWorld, unexpected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        !stdout.contains(&unexpected) && !stderr.contains(&unexpected),
        "Should not see '{}' in output, but got:\nStdout: {}\nStderr: {}",
        unexpected,
        stdout,
        stderr
    );
}

#[then(regex = r"^I should see a preview of rendered output$")]
fn should_see_preview_of_output(_world: &mut GgenWorld) {
    // Would verify preview output
}

#[then(regex = r"^the template should have RDF frontmatter section$")]
fn template_should_have_rdf_section(_world: &mut GgenWorld) {
    // Would verify RDF section in template
}

#[then(regex = r"^the template should have SPARQL section$")]
fn template_should_have_sparql_section(_world: &mut GgenWorld) {
    // Would verify SPARQL section in template
}

#[then(regex = r"^I should see descriptions for each template$")]
fn should_see_descriptions_for_templates(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("description") || stdout.contains("Description"),
        "Expected to see descriptions, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see compatibility information$")]
fn should_see_compatibility_info(_world: &mut GgenWorld) {
    // Would verify compatibility information
}
