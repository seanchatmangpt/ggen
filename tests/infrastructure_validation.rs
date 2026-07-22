// Infrastructure Validation Tests
// Validates that all infrastructure systems are working correctly

#[cfg(test)]
mod infrastructure_tests {
    use std::path::Path;

    #[test]
    fn test_scripts_are_executable() {
        // Arrange
        let scripts = vec![
            "scripts/pre-commit-hook.sh",
            "scripts/install-hooks.sh",
            "scripts/generate-dashboard.sh",
        ];

        // Act & Assert
        for script in scripts {
            let path = Path::new(script);
            assert!(path.exists(), "Script should exist: {}", script);

            // Verify executable bit (Unix only)
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let metadata = path.metadata().expect("Failed to get metadata");
                let permissions = metadata.permissions();
                let mode = permissions.mode();
                assert!(mode & 0o111 != 0, "Script should be executable: {}", script);
            }
        }
    }

    #[test]
    fn test_build_rs_exists() {
        // Arrange & Act
        let build_rs = Path::new("build.rs");

        // Assert
        assert!(build_rs.exists(), "build.rs should exist");
    }

    #[test]
    fn test_infrastructure_docs_exist() {
        // Arrange & Act
        let docs = Path::new("docs/INFRASTRUCTURE_SETUP.md");

        // Assert
        assert!(
            docs.exists(),
            "Infrastructure setup documentation should exist"
        );
    }

    #[test]
    fn test_chicago_tdd_template_exists() {
        // Arrange & Act
        let template = Path::new("tests/templates/chicago_tdd_template.rs");

        // Assert
        assert!(template.exists(), "Chicago TDD template should exist");
    }

    #[test]
    fn test_metrics_directory_creation() {
        // Arrange & Act
        let metrics_dir = Path::new(".metrics");

        // Create if not exists
        if !metrics_dir.exists() {
            std::fs::create_dir_all(metrics_dir).expect("Failed to create metrics directory");
        }

        // Assert
        assert!(metrics_dir.exists(), "Metrics directory should exist");
        assert!(metrics_dir.is_dir(), "Metrics path should be a directory");
    }

    // The three tests below used to shell out to `cargo make --list-all-steps`
    // and check Makefile.toml's task names. This repo's own entry-point rule
    // (.claude/rules/_core/absolute.md rule 4; CLAUDE.md throughout) forbids
    // `cargo make` -- `just` is the sole sanctioned entry point, Makefile.toml
    // is historical reference only, and `cargo-make` is not assumed installed
    // in CI. These now check the real, current entry point (`justfile`)
    // directly, repointed at each task's real justfile-era equivalent rather
    // than inventing a recipe solely to satisfy a stale assertion.

    #[test]
    fn test_timeout_check_task_exists() {
        // Arrange & Act
        let justfile = std::fs::read_to_string("justfile").expect("Failed to read justfile");

        // Assert
        assert!(
            justfile.contains("timeout-check:"),
            "timeout-check recipe should exist in justfile"
        );
    }

    #[test]
    fn test_ci_gate_task_exists() {
        // Arrange & Act
        let justfile = std::fs::read_to_string("justfile").expect("Failed to read justfile");

        // Assert: this repo has no single recipe literally named `ci-gate` --
        // `pre-commit` is the documented current equivalent (see
        // .claude/rules/andon/signals.md's "Definition of Done": "The
        // authoritative gate is `just pre-commit`"), chaining fmt-check,
        // check, lint, test-lib, coherence-check, and the guard-* recipes.
        assert!(
            justfile.contains("pre-commit:"),
            "pre-commit recipe (this repo's real CI-gate equivalent) should exist in justfile"
        );
    }

    #[test]
    fn test_pre_commit_hook_task_exists() {
        // Arrange & Act
        let justfile = std::fs::read_to_string("justfile").expect("Failed to read justfile");

        // Assert: Makefile.toml's `pre-commit-hook` task is `pre-commit` in
        // the justfile era (see rename note above).
        assert!(
            justfile.contains("pre-commit:"),
            "pre-commit recipe should exist in justfile"
        );
    }
}
