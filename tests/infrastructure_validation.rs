// Infrastructure Validation Tests
// Validates that all infrastructure systems are working correctly

#[cfg(test)]
mod infrastructure_tests {
    use std::path::Path;
    use std::process::Command;

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
                #[allow(clippy::expect_used)]
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
            #[allow(clippy::expect_used)]
            std::fs::create_dir_all(metrics_dir).expect("Failed to create metrics directory");
        }

        // Assert
        assert!(metrics_dir.exists(), "Metrics directory should exist");
        assert!(metrics_dir.is_dir(), "Metrics path should be a directory");
    }

    #[test]
    fn test_timeout_check_task_exists() {
        // Arrange & Act
        let output = Command::new("cargo")
            .args(&["make", "--list-all-steps"])
            .output()
            #[allow(clippy::expect_used)]
            .expect("Failed to run cargo make");

        let stdout = String::from_utf8_lossy(&output.stdout);

        // Assert
        assert!(
            stdout.contains("timeout-check"),
            "timeout-check task should exist in Makefile.toml"
        );
    }

    #[test]
    fn test_ci_gate_task_exists() {
        // Arrange & Act
        let output = Command::new("cargo")
            .args(&["make", "--list-all-steps"])
            .output()
            #[allow(clippy::expect_used)]
            .expect("Failed to run cargo make");

        let stdout = String::from_utf8_lossy(&output.stdout);

        // Assert
        assert!(
            stdout.contains("ci-gate"),
            "ci-gate task should exist in Makefile.toml"
        );
    }

    #[test]
    fn test_pre_commit_hook_task_exists() {
        // Arrange & Act
        let output = Command::new("cargo")
            .args(&["make", "--list-all-steps"])
            .output()
            #[allow(clippy::expect_used)]
            .expect("Failed to run cargo make");

        let stdout = String::from_utf8_lossy(&output.stdout);

        // Assert
        assert!(
            stdout.contains("pre-commit-hook"),
            "pre-commit-hook task should exist in Makefile.toml"
        );
    }
}
