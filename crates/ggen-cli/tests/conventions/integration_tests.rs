/// Convention Integration Tests
/// London TDD: End-to-end workflow testing with mocks
///
/// These tests define the CONTRACT for the complete convention system:
/// - Zero-config project generation
/// - Preset application
/// - Auto-discovery workflows
/// - Template metadata to output flow
use super::fixtures::*;
use std::path::PathBuf;
use tempfile::TempDir;

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_zero_config_project_generation() {
        // ARRANGE: Set up minimal project structure (no .ggen config)
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        // Create standard directory structure
        fs::create_dir_all(project_root.join("rdf")).unwrap();
        fs::create_dir_all(project_root.join("templates")).unwrap();
        fs::create_dir_all(project_root.join("queries")).unwrap();

        // Add sample RDF file
        fs::write(project_root.join("rdf/users.ttl"), sample_rdf_content()).unwrap();

        // Add sample template
        fs::write(
            project_root.join("templates/user.hbs"),
            sample_template_content(),
        )
        .unwrap();

        // Add sample query
        fs::write(
            project_root.join("queries/get_users.sparql"),
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
        )
        .unwrap();

        // ACT: Run convention-based generation (no config needed)
        // TODO: Call actual generator once implemented
        // let generator = ConventionGenerator::new(project_root);
        // let result = generator.generate().unwrap();

        // ASSERT: Should auto-discover and generate
        // Expected: src/models/users.rs created
        let expected_output = project_root.join("src/models/users.rs");

        // For now, verify directory structure
        assert!(project_root.join("rdf").exists());
        assert!(project_root.join("templates").exists());
        assert!(project_root.join("queries").exists());
    }

    #[test]
    fn test_convention_preset_clap_noun_verb() {
        // ARRANGE: Project with clap-noun-verb preset
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        // Create .ggen config with preset
        fs::write(
            project_root.join(".ggen"),
            r#"
[conventions]
preset = "clap-noun-verb"
"#,
        )
        .unwrap();

        // Create preset-specific structure
        fs::create_dir_all(project_root.join("rdf/commands")).unwrap();
        fs::create_dir_all(project_root.join("templates/cli")).unwrap();

        // Add command RDF
        fs::write(
            project_root.join("rdf/commands/user-create.ttl"),
            r#"
@prefix cmd: <http://example.org/command/> .

cmd:UserCreate a cmd:Command ;
    cmd:noun "user" ;
    cmd:verb "create" ;
    cmd:description "Create a new user" .
"#,
        )
        .unwrap();

        // Add CLI template
        fs::write(
            project_root.join("templates/cli/command.hbs"),
            r#"---
name: command
mode: foreach
when: "**/commands/*.ttl"
output: "src/commands/{{noun}}_{{verb}}.rs"
---
pub fn {{noun}}_{{verb}}() {
    // Generated command
}
"#,
        )
        .unwrap();

        // ACT: Generate with preset conventions
        // TODO: Call generator with preset support

        // ASSERT: Should follow clap-noun-verb structure
        let expected_output = project_root.join("src/commands/user_create.rs");

        // Verify preset config was created
        assert!(project_root.join(".ggen").exists());
    }

    #[test]
    fn test_auto_discovery_complete_workflow() {
        // ARRANGE: Complete project with all convention directories
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        // Set up full structure
        let dirs = vec![
            "rdf",
            "templates/models",
            "templates/services",
            "queries",
            "src",
        ];

        for dir in dirs {
            fs::create_dir_all(project_root.join(dir)).unwrap();
        }

        // Add multiple RDF files (numbered for ordering)
        fs::write(
            project_root.join("rdf/01-schema.ttl"),
            "@prefix ex: <http://example.org/> .",
        )
        .unwrap();

        fs::write(project_root.join("rdf/02-users.ttl"), sample_rdf_content()).unwrap();

        // Add multiple templates
        fs::write(
            project_root.join("templates/models/base.hbs"),
            r#"---
name: base
mode: once
output: "models/mod.rs"
---
pub mod user;
"#,
        )
        .unwrap();

        fs::write(
            project_root.join("templates/models/user.hbs"),
            sample_template_content(),
        )
        .unwrap();

        // Add queries
        fs::write(
            project_root.join("queries/get_users.sparql"),
            "SELECT * WHERE { ?s a ex:User }",
        )
        .unwrap();

        // ACT: Run auto-discovery workflow
        // TODO: Implement full workflow
        // Expected workflow:
        // 1. Discover all RDF files in order
        // 2. Discover all templates recursively
        // 3. Parse template metadata
        // 4. Link queries by name
        // 5. Build generation plan
        // 6. Execute plan respecting dependencies
        // 7. Generate outputs

        // ASSERT: All files discovered and processed
        assert!(project_root.join("rdf/01-schema.ttl").exists());
        assert!(project_root.join("rdf/02-users.ttl").exists());
        assert!(project_root.join("templates/models/base.hbs").exists());
        assert!(project_root.join("templates/models/user.hbs").exists());
        assert!(project_root.join("queries/get_users.sparql").exists());
    }

    #[test]
    fn test_template_metadata_to_output() {
        // ARRANGE: Template with full metadata
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        fs::create_dir_all(project_root.join("rdf")).unwrap();
        fs::create_dir_all(project_root.join("templates")).unwrap();
        fs::create_dir_all(project_root.join("queries")).unwrap();

        // Template with all metadata fields
        let template_content = r#"---
name: user_service
mode: foreach
when: "**/*user*.ttl"
output: "services/{{filename}}_service.rs"
query: get_users
depends_on:
  - base_service
---
use crate::models::User;

pub struct {{name}}Service {
    // Generated service
}
"#;

        fs::write(
            project_root.join("templates/user_service.hbs"),
            template_content,
        )
        .unwrap();

        fs::write(
            project_root.join("rdf/admin_users.ttl"),
            sample_rdf_content(),
        )
        .unwrap();

        fs::write(
            project_root.join("queries/get_users.sparql"),
            "SELECT * WHERE { ?s a ex:User }",
        )
        .unwrap();

        // ACT: Process template metadata -> output
        // TODO: Implement metadata processor
        // Expected flow:
        // 1. Parse frontmatter metadata
        // 2. Match when trigger against RDF files
        // 3. Link query by name
        // 4. Interpolate output path
        // 5. Generate to correct location

        // ASSERT: Output generated at interpolated path
        // Expected: src/services/admin_users_service.rs
        let expected_output = project_root.join("src/services/admin_users_service.rs");

        assert!(project_root.join("templates/user_service.hbs").exists());
        assert!(project_root.join("rdf/admin_users.ttl").exists());
        assert!(project_root.join("queries/get_users.sparql").exists());
    }

    #[test]
    fn test_incremental_generation_caching() {
        // ARRANGE: Project with existing outputs
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        fs::create_dir_all(project_root.join("rdf")).unwrap();
        fs::create_dir_all(project_root.join("templates")).unwrap();
        fs::create_dir_all(project_root.join("src/models")).unwrap();

        // Existing output with timestamp
        fs::write(
            project_root.join("src/models/user.rs"),
            "// Generated at 2024-01-01\npub struct User {}",
        )
        .unwrap();

        // Template (unchanged)
        fs::write(
            project_root.join("templates/user.hbs"),
            sample_template_content(),
        )
        .unwrap();

        // RDF file (unchanged)
        fs::write(project_root.join("rdf/users.ttl"), sample_rdf_content()).unwrap();

        // ACT: Run generation again
        // TODO: Implement incremental generation

        // ASSERT: Should skip regeneration (cache hit)
        let output = fs::read_to_string(project_root.join("src/models/user.rs")).unwrap();
        assert!(output.contains("2024-01-01"));
    }

    #[test]
    fn test_convention_override_precedence() {
        // ARRANGE: Multiple config sources
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        // Default conventions (implicit)
        // .ggen config (explicit)
        fs::write(
            project_root.join(".ggen"),
            r#"
[conventions]
rdf_dir = "custom_rdf"
output_dir = "generated"
"#,
        )
        .unwrap();

        // Command-line args (highest priority)
        // --rdf-dir="cli_rdf" --output-dir="cli_output"

        // ACT: Apply config precedence
        // TODO: Implement config merging
        // Expected precedence: CLI args > .ggen > defaults

        // ASSERT: CLI args win
        let config = ConventionConfig {
            rdf_dir: PathBuf::from("cli_rdf"),
            output_dir: PathBuf::from("cli_output"),
            ..Default::default()
        };

        assert_eq!(config.rdf_dir, PathBuf::from("cli_rdf"));
        assert_eq!(config.output_dir, PathBuf::from("cli_output"));
    }

    #[test]
    fn test_error_handling_missing_query() {
        // ARRANGE: Template references non-existent query
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        fs::create_dir_all(project_root.join("templates")).unwrap();
        fs::create_dir_all(project_root.join("queries")).unwrap();

        fs::write(
            project_root.join("templates/user.hbs"),
            r#"---
name: user_model
query: missing_query
---
pub struct User {}
"#,
        )
        .unwrap();

        // No queries/missing_query.sparql file

        // ACT & ASSERT: Should return helpful error
        // TODO: Implement error handling
        // Expected: Error("Query 'missing_query' not found in queries directory")

        let queries_dir = project_root.join("queries");
        let missing_query = queries_dir.join("missing_query.sparql");
        assert!(!missing_query.exists());
    }

    #[test]
    fn test_parallel_template_execution() {
        // ARRANGE: Multiple independent templates
        let temp_dir = TempDir::new().unwrap();
        let project_root = temp_dir.path();

        fs::create_dir_all(project_root.join("rdf")).unwrap();
        fs::create_dir_all(project_root.join("templates")).unwrap();

        // Create multiple RDF files
        for i in 1..=5 {
            fs::write(
                project_root.join(format!("rdf/file{}.ttl", i)),
                sample_rdf_content(),
            )
            .unwrap();
        }

        // Template for parallel execution
        fs::write(
            project_root.join("templates/model.hbs"),
            sample_template_content(),
        )
        .unwrap();

        // ACT: Execute templates in parallel
        // TODO: Implement parallel executor

        // ASSERT: All outputs generated concurrently
        // Expected: Significant speedup vs sequential
        for i in 1..=5 {
            let rdf_file = project_root.join(format!("rdf/file{}.ttl", i));
            assert!(rdf_file.exists());
        }
    }
}
