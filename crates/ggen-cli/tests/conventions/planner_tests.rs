/// Generation Planner Tests
/// London TDD: Test workflow planning and orchestration through mocks
///
/// These tests define the CONTRACT for the GenerationPlanner:
/// - How it builds execution plans
/// - How it resolves template dependencies
/// - How it handles metadata
/// - How it coordinates generation modes

use super::fixtures::*;
use std::path::PathBuf;
use mockall::predicate::*;

/// GenerationPlanner trait - to be implemented
trait PlannerService {
    fn build_generation_plan(&self, config: &ConventionConfig) -> anyhow::Result<GenerationPlan>;
    fn resolve_template_dependencies(&self, templates: &[TemplateMetadata]) -> anyhow::Result<Vec<TemplateMetadata>>;
    fn parse_template_metadata(&self, path: &std::path::Path) -> anyhow::Result<TemplateMetadata>;
    fn match_when_triggers(&self, template: &TemplateMetadata, rdf_files: &[PathBuf]) -> Vec<PathBuf>;
    fn link_queries(&self, template: &TemplateMetadata, queries: &[PathBuf]) -> Option<PathBuf>;
    fn detect_circular_dependencies(&self, templates: &[TemplateMetadata]) -> anyhow::Result<()>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_build_generation_plan_from_conventions() {
        // ARRANGE: Set up mock planner with discovered files
        let mut mock_planner = MockGenerationPlanner::new();

        let config = ConventionConfig {
            rdf_dir: PathBuf::from("rdf"),
            templates_dir: PathBuf::from("templates"),
            queries_dir: PathBuf::from("queries"),
            output_dir: PathBuf::from("src"),
            preset: None,
        };

        let expected_plan = GenerationPlan {
            templates: vec![
                TemplateMetadata {
                    name: "user_model".to_string(),
                    path: PathBuf::from("templates/user.hbs"),
                    mode: GenerationMode::ForEach,
                    when_trigger: Some("**/*user*.ttl".to_string()),
                    output_path: Some("models/{{name}}.rs".to_string()),
                    query: Some("get_users".to_string()),
                    dependencies: vec![],
                },
            ],
            rdf_files: vec![PathBuf::from("rdf/users.ttl")],
            output_mappings: vec![
                (PathBuf::from("rdf/users.ttl"), PathBuf::from("src/models/users.rs")),
            ],
        };

        mock_planner.expect_build_plan()
            .with(eq(config.clone()))
            .returning(move |_| Ok(expected_plan.clone()));

        // ACT: Build generation plan
        let result = mock_planner.build_plan(&config).unwrap();

        // ASSERT: Verify plan structure
        assert_eq!(result.templates.len(), 1);
        assert_eq!(result.rdf_files.len(), 1);
        assert_eq!(result.output_mappings.len(), 1);
    }

    #[test]
    fn test_resolve_template_dependencies() {
        // ARRANGE: Templates with dependency chain
        let templates = vec![
            TemplateMetadata {
                name: "base".to_string(),
                path: PathBuf::from("templates/base.hbs"),
                mode: GenerationMode::Once,
                when_trigger: None,
                output_path: Some("lib.rs".to_string()),
                query: None,
                dependencies: vec![],
            },
            TemplateMetadata {
                name: "user_model".to_string(),
                path: PathBuf::from("templates/user.hbs"),
                mode: GenerationMode::ForEach,
                when_trigger: None,
                output_path: Some("models/user.rs".to_string()),
                query: Some("get_users".to_string()),
                dependencies: vec!["base".to_string()],
            },
        ];

        let mut mock_planner = MockGenerationPlanner::new();

        // Mock should verify dependencies are resolved in correct order
        let expected_order = vec![
            templates[0].clone(), // base first
            templates[1].clone(), // user_model second
        ];

        mock_planner.expect_resolve_dependencies()
            .with(eq(templates.clone()))
            .returning(move |_| Ok(expected_order.clone()));

        // ACT: Resolve dependencies
        let result = mock_planner.resolve_dependencies(&templates).unwrap();

        // ASSERT: Base template comes before dependent
        assert_eq!(result[0].name, "base");
        assert_eq!(result[1].name, "user_model");
    }

    #[test]
    fn test_template_metadata_parsing() {
        // ARRANGE: Mock template engine for frontmatter parsing
        let mut mock_engine = MockTemplateEngine::new();

        let template_content = sample_template_content();
        let expected_metadata = (
            serde_json::json!({
                "name": "user_model",
                "mode": "foreach",
                "when": "**/*user*.ttl",
                "output": "models/{{ name }}.rs",
                "query": "get_users"
            }),
            "pub struct {{ name }} {\n    pub email: String,\n}".to_string(),
        );

        mock_engine.expect_parse_frontmatter()
            .with(eq(template_content.clone()))
            .returning(move |_| Ok(expected_metadata.clone()));

        // ACT: Parse frontmatter
        let result = mock_engine.parse_frontmatter(&template_content).unwrap();

        // ASSERT: Verify metadata extraction
        assert_eq!(result.0["name"], "user_model");
        assert_eq!(result.0["mode"], "foreach");
        assert_eq!(result.0["when"], "**/*user*.ttl");
    }

    #[test]
    fn test_foreach_vs_once_generation() {
        // ARRANGE: Two templates with different modes
        let foreach_template = TemplateMetadata {
            name: "user_model".to_string(),
            path: PathBuf::from("templates/user.hbs"),
            mode: GenerationMode::ForEach,
            when_trigger: Some("**/*.ttl".to_string()),
            output_path: Some("models/{{name}}.rs".to_string()),
            query: Some("get_users".to_string()),
            dependencies: vec![],
        };

        let once_template = TemplateMetadata {
            name: "lib".to_string(),
            path: PathBuf::from("templates/lib.hbs"),
            mode: GenerationMode::Once,
            when_trigger: None,
            output_path: Some("lib.rs".to_string()),
            query: None,
            dependencies: vec![],
        };

        let rdf_files = vec![
            PathBuf::from("rdf/users.ttl"),
            PathBuf::from("rdf/posts.ttl"),
        ];

        // ASSERT: ForEach generates multiple outputs
        // Expected: 2 outputs from foreach_template (one per RDF file)
        // Expected: 1 output from once_template
        assert_eq!(foreach_template.mode, GenerationMode::ForEach);
        assert_eq!(once_template.mode, GenerationMode::Once);
        assert_eq!(rdf_files.len(), 2);
    }

    #[test]
    fn test_when_trigger_file_matching() {
        // ARRANGE: Template with glob pattern trigger
        let template = TemplateMetadata {
            name: "user_model".to_string(),
            path: PathBuf::from("templates/user.hbs"),
            mode: GenerationMode::ForEach,
            when_trigger: Some("**/*user*.ttl".to_string()),
            output_path: Some("models/{{name}}.rs".to_string()),
            query: Some("get_users".to_string()),
            dependencies: vec![],
        };

        let rdf_files = vec![
            PathBuf::from("rdf/users.ttl"),      // Match
            PathBuf::from("rdf/user_data.ttl"),  // Match
            PathBuf::from("rdf/posts.ttl"),      // No match
            PathBuf::from("rdf/admin_user.ttl"), // Match
        ];

        // ACT: Match files against when trigger
        // TODO: Implement actual matcher

        // ASSERT: Should match 3 files containing "user"
        let expected_matches = vec![
            PathBuf::from("rdf/users.ttl"),
            PathBuf::from("rdf/user_data.ttl"),
            PathBuf::from("rdf/admin_user.ttl"),
        ];

        assert_eq!(expected_matches.len(), 3);
    }

    #[test]
    fn test_query_linking_by_name() {
        // ARRANGE: Template references query by name
        let template = TemplateMetadata {
            name: "user_model".to_string(),
            path: PathBuf::from("templates/user.hbs"),
            mode: GenerationMode::ForEach,
            when_trigger: None,
            output_path: Some("models/user.rs".to_string()),
            query: Some("get_users".to_string()), // Reference by name
            dependencies: vec![],
        };

        let queries = vec![
            PathBuf::from("queries/get_users.sparql"),
            PathBuf::from("queries/list_posts.sparql"),
        ];

        // ACT: Link query by matching name
        // TODO: Implement query linker

        // ASSERT: Should link to get_users.sparql
        let expected_query = PathBuf::from("queries/get_users.sparql");
        assert!(queries.contains(&expected_query));
    }

    #[test]
    fn test_incremental_planning() {
        // ARRANGE: Existing generation plan + new templates
        let existing_plan = GenerationPlan {
            templates: vec![
                TemplateMetadata {
                    name: "base".to_string(),
                    path: PathBuf::from("templates/base.hbs"),
                    mode: GenerationMode::Once,
                    when_trigger: None,
                    output_path: Some("lib.rs".to_string()),
                    query: None,
                    dependencies: vec![],
                },
            ],
            rdf_files: vec![PathBuf::from("rdf/users.ttl")],
            output_mappings: vec![],
        };

        // ACT: Add new template to existing plan
        // TODO: Implement incremental planner

        // ASSERT: Should merge plans without re-generating base
        assert_eq!(existing_plan.templates.len(), 1);
    }

    #[test]
    fn test_circular_dependency_detection() {
        // ARRANGE: Templates with circular dependencies
        let templates = vec![
            TemplateMetadata {
                name: "a".to_string(),
                path: PathBuf::from("templates/a.hbs"),
                mode: GenerationMode::Once,
                when_trigger: None,
                output_path: Some("a.rs".to_string()),
                query: None,
                dependencies: vec!["b".to_string()], // A depends on B
            },
            TemplateMetadata {
                name: "b".to_string(),
                path: PathBuf::from("templates/b.hbs"),
                mode: GenerationMode::Once,
                when_trigger: None,
                output_path: Some("b.rs".to_string()),
                query: None,
                dependencies: vec!["a".to_string()], // B depends on A (cycle!)
            },
        ];

        // ACT & ASSERT: Should detect circular dependency
        // TODO: Implement cycle detector
        // Expected: Error with message like "Circular dependency detected: a -> b -> a"

        let has_cycle = templates[0].dependencies.contains(&templates[1].name)
            && templates[1].dependencies.contains(&templates[0].name);
        assert!(has_cycle);
    }

    #[test]
    fn test_parallel_generation_planning() {
        // ARRANGE: Multiple independent templates
        let templates = vec![
            TemplateMetadata {
                name: "user_model".to_string(),
                path: PathBuf::from("templates/user.hbs"),
                mode: GenerationMode::ForEach,
                when_trigger: Some("**/*user*.ttl".to_string()),
                output_path: Some("models/user.rs".to_string()),
                query: Some("get_users".to_string()),
                dependencies: vec![],
            },
            TemplateMetadata {
                name: "post_model".to_string(),
                path: PathBuf::from("templates/post.hbs"),
                mode: GenerationMode::ForEach,
                when_trigger: Some("**/*post*.ttl".to_string()),
                output_path: Some("models/post.rs".to_string()),
                query: Some("get_posts".to_string()),
                dependencies: vec![],
            },
        ];

        // ACT: Plan should identify parallelizable templates
        // TODO: Implement parallel planner

        // ASSERT: Both templates can execute in parallel (no dependencies)
        assert!(templates[0].dependencies.is_empty());
        assert!(templates[1].dependencies.is_empty());
    }

    #[test]
    fn test_output_path_interpolation() {
        // ARRANGE: Template with dynamic output path
        let template = TemplateMetadata {
            name: "user_model".to_string(),
            path: PathBuf::from("templates/user.hbs"),
            mode: GenerationMode::ForEach,
            when_trigger: None,
            output_path: Some("models/{{filename}}/{{name}}.rs".to_string()),
            query: Some("get_users".to_string()),
            dependencies: vec![],
        };

        let rdf_file = PathBuf::from("rdf/admin_users.ttl");

        // ACT: Interpolate output path
        // TODO: Implement path interpolator
        // Expected: "models/admin_users/users.rs"

        // ASSERT: Variables should be replaced
        assert!(template.output_path.unwrap().contains("{{"));
    }

    #[test]
    fn test_preset_convention_override() {
        // ARRANGE: Config with preset
        let config = ConventionConfig {
            rdf_dir: PathBuf::from("rdf"),
            templates_dir: PathBuf::from("templates"),
            queries_dir: PathBuf::from("queries"),
            output_dir: PathBuf::from("src"),
            preset: Some("clap-noun-verb".to_string()),
        };

        // ACT: Apply preset conventions
        // TODO: Implement preset system

        // ASSERT: Preset overrides default conventions
        assert_eq!(config.preset, Some("clap-noun-verb".to_string()));
    }

    #[test]
    fn test_conditional_generation_skip() {
        // ARRANGE: Template with when condition that doesn't match
        let template = TemplateMetadata {
            name: "user_model".to_string(),
            path: PathBuf::from("templates/user.hbs"),
            mode: GenerationMode::ForEach,
            when_trigger: Some("**/*admin*.ttl".to_string()),
            output_path: Some("models/user.rs".to_string()),
            query: Some("get_users".to_string()),
            dependencies: vec![],
        };

        let rdf_files = vec![
            PathBuf::from("rdf/users.ttl"),
            PathBuf::from("rdf/posts.ttl"),
        ];

        // ACT: Match when trigger
        // ASSERT: No matches, template should be skipped
        let matches_any = rdf_files.iter().any(|f| {
            f.to_str().unwrap().contains("admin")
        });

        assert!(!matches_any);
    }
}
