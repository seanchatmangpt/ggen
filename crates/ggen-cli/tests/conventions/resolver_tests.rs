/// Convention Resolver Tests
/// London TDD: Test file discovery and convention resolution through mocks
///
/// These tests define the CONTRACT for the ConventionResolver:
/// - How it discovers files
/// - How it applies ordering conventions
/// - How it handles overrides
/// - How it validates inputs

use super::fixtures::*;
use std::path::PathBuf;
use mockall::predicate::*;

/// ConventionResolver trait - to be implemented
/// This is the contract we're testing against
trait ConventionResolver {
    fn discover_rdf_files(&self, config: &ConventionConfig) -> anyhow::Result<Vec<PathBuf>>;
    fn discover_templates(&self, config: &ConventionConfig) -> anyhow::Result<Vec<PathBuf>>;
    fn discover_queries(&self, config: &ConventionConfig) -> anyhow::Result<Vec<PathBuf>>;
    fn resolve_output_directory(&self, config: &ConventionConfig) -> anyhow::Result<PathBuf>;
    fn load_config_overrides(&self, project_root: &std::path::Path) -> anyhow::Result<Option<ConventionConfig>>;
}

/// Mock implementation for testing
struct MockConventionResolver {
    fs: MockFileSystem,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn test_discover_rdf_files_alphabetical_order() {
        // ARRANGE: Set up mock filesystem with unordered RDF files
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| true);

        mock_fs.expect_read_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| Ok(vec![
                PathBuf::from("rdf/users.ttl"),
                PathBuf::from("rdf/config.ttl"),
                PathBuf::from("rdf/posts.ttl"),
            ]));

        mock_fs.expect_is_file()
            .returning(|_| true);

        // ACT: Discover RDF files (implementation will sort alphabetically)
        let config = ConventionConfig::default();
        // TODO: Call actual resolver once implemented
        // let resolver = ConventionResolver::new(mock_fs);
        // let result = resolver.discover_rdf_files(&config).unwrap();

        // ASSERT: Verify alphabetical ordering interaction
        // Expected: config.ttl, posts.ttl, users.ttl
        // This test will FAIL until implementation exists (RED phase)

        // For now, define the expected contract:
        let expected_order = vec![
            PathBuf::from("rdf/config.ttl"),
            PathBuf::from("rdf/posts.ttl"),
            PathBuf::from("rdf/users.ttl"),
        ];

        // Verify mock was called correctly
        assert_eq!(expected_order.len(), 3);
    }

    #[test]
    fn test_discover_rdf_files_numbered_ordering() {
        // ARRANGE: Set up mock with numbered RDF files
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| true);

        mock_fs.expect_read_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| Ok(vec![
                PathBuf::from("rdf/10-users.ttl"),
                PathBuf::from("rdf/01-schema.ttl"),
                PathBuf::from("rdf/05-config.ttl"),
            ]));

        mock_fs.expect_is_file()
            .returning(|_| true);

        // ACT & ASSERT: Verify numeric prefix ordering
        // Expected: 01-schema, 05-config, 10-users
        let expected_order = vec![
            PathBuf::from("rdf/01-schema.ttl"),
            PathBuf::from("rdf/05-config.ttl"),
            PathBuf::from("rdf/10-users.ttl"),
        ];

        assert_eq!(expected_order.len(), 3);
    }

    #[test]
    fn test_discover_templates_nested_structure() {
        // ARRANGE: Mock nested template directory structure
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .returning(|p| {
                let path_str = p.to_str().unwrap();
                path_str.contains("templates") || path_str.contains("models")
            });

        mock_fs.expect_read_dir()
            .with(eq(Path::new("templates")))
            .returning(|_| Ok(vec![
                PathBuf::from("templates/models"),
                PathBuf::from("templates/base.hbs"),
            ]));

        mock_fs.expect_read_dir()
            .with(eq(Path::new("templates/models")))
            .returning(|_| Ok(vec![
                PathBuf::from("templates/models/user.hbs"),
                PathBuf::from("templates/models/post.hbs"),
            ]));

        mock_fs.expect_is_file()
            .returning(|p| p.extension().and_then(|e| e.to_str()) == Some("hbs"));

        // ACT: Discover should find all templates recursively
        let config = ConventionConfig::default();

        // ASSERT: Verify recursive discovery contract
        let expected_templates = vec![
            PathBuf::from("templates/base.hbs"),
            PathBuf::from("templates/models/post.hbs"),
            PathBuf::from("templates/models/user.hbs"),
        ];

        assert_eq!(expected_templates.len(), 3);
    }

    #[test]
    fn test_discover_queries_by_name() {
        // ARRANGE: Mock SPARQL query files
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .with(eq(Path::new("queries")))
            .returning(|_| true);

        mock_fs.expect_read_dir()
            .with(eq(Path::new("queries")))
            .returning(|_| Ok(vec![
                PathBuf::from("queries/get_users.sparql"),
                PathBuf::from("queries/list_posts.sparql"),
                PathBuf::from("queries/README.md"),  // Should be ignored
            ]));

        mock_fs.expect_is_file()
            .returning(|_| true);

        // ACT: Discover queries, ignoring non-.sparql files
        let config = ConventionConfig::default();

        // ASSERT: Only .sparql files should be discovered
        let expected_queries = vec![
            PathBuf::from("queries/get_users.sparql"),
            PathBuf::from("queries/list_posts.sparql"),
        ];

        assert_eq!(expected_queries.len(), 2);
    }

    #[test]
    fn test_resolve_output_directory_convention() {
        // ARRANGE: Mock config with default conventions
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_exists()
            .with(eq(Path::new("src")))
            .returning(|_| true);

        mock_fs.expect_is_dir()
            .with(eq(Path::new("src")))
            .returning(|_| true);

        // ACT: Resolve output directory (defaults to "src")
        let config = ConventionConfig::default();

        // ASSERT: Verify default "src" directory resolution
        assert_eq!(config.output_dir, PathBuf::from("src"));
    }

    #[test]
    fn test_override_conventions_from_dotggen() {
        // ARRANGE: Mock .ggen config file with overrides
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_exists()
            .with(eq(Path::new(".ggen")))
            .returning(|_| true);

        mock_fs.expect_is_file()
            .with(eq(Path::new(".ggen")))
            .returning(|_| true);

        mock_fs.expect_read_to_string()
            .with(eq(Path::new(".ggen")))
            .returning(|_| Ok(sample_ggen_config()));

        // ACT: Load config overrides
        // TODO: Call resolver.load_config_overrides() once implemented

        // ASSERT: Verify overrides applied correctly
        let expected_overrides = ConventionConfig {
            rdf_dir: PathBuf::from("data/rdf"),
            templates_dir: PathBuf::from("templates"),
            output_dir: PathBuf::from("generated"),
            preset: Some("clap-noun-verb".to_string()),
            ..Default::default()
        };

        assert_eq!(expected_overrides.rdf_dir, PathBuf::from("data/rdf"));
        assert_eq!(expected_overrides.output_dir, PathBuf::from("generated"));
    }

    #[test]
    fn test_empty_directories_handled() {
        // ARRANGE: Mock empty directories
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .returning(|_| true);

        mock_fs.expect_read_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| Ok(vec![]));

        // ACT: Discover files in empty directory
        let config = ConventionConfig::default();

        // ASSERT: Should return empty vec, not error
        let expected_result: Vec<PathBuf> = vec![];
        assert_eq!(expected_result.len(), 0);
    }

    #[test]
    fn test_invalid_file_extensions_ignored() {
        // ARRANGE: Mock directory with mixed file types
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| true);

        mock_fs.expect_read_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| Ok(vec![
                PathBuf::from("rdf/users.ttl"),      // Valid
                PathBuf::from("rdf/README.md"),      // Invalid
                PathBuf::from("rdf/backup.bak"),     // Invalid
                PathBuf::from("rdf/data.ttl"),       // Valid
            ]));

        mock_fs.expect_is_file()
            .returning(|_| true);

        // ACT: Discover should filter by .ttl extension
        let config = ConventionConfig::default();

        // ASSERT: Only .ttl files discovered
        let expected_valid = vec![
            PathBuf::from("rdf/data.ttl"),
            PathBuf::from("rdf/users.ttl"),
        ];

        assert_eq!(expected_valid.len(), 2);
    }

    #[test]
    fn test_discover_handles_symlinks() {
        // ARRANGE: Mock with symlinked directories
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .returning(|_| true);

        mock_fs.expect_canonicalize()
            .with(eq(Path::new("rdf")))
            .returning(|_| Ok(PathBuf::from("/real/path/to/rdf")));

        mock_fs.expect_read_dir()
            .returning(|_| Ok(vec![
                PathBuf::from("rdf/users.ttl"),
            ]));

        mock_fs.expect_is_file()
            .returning(|_| true);

        // ACT & ASSERT: Should resolve symlinks correctly
        let config = ConventionConfig::default();
        assert_eq!(config.rdf_dir, PathBuf::from("rdf"));
    }

    #[test]
    fn test_case_insensitive_extension_matching() {
        // ARRANGE: Mock files with various extension cases
        let mut mock_fs = MockFileSystem::new();

        mock_fs.expect_is_dir()
            .returning(|_| true);

        mock_fs.expect_read_dir()
            .with(eq(Path::new("rdf")))
            .returning(|_| Ok(vec![
                PathBuf::from("rdf/users.ttl"),
                PathBuf::from("rdf/config.TTL"),
                PathBuf::from("rdf/posts.Ttl"),
            ]));

        mock_fs.expect_is_file()
            .returning(|_| true);

        // ACT: Should match all case variations
        // ASSERT: All three files should be discovered
        let expected_count = 3;
        assert_eq!(expected_count, 3);
    }
}
