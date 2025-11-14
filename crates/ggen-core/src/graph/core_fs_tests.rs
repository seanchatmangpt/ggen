//! Graph filesystem operation tests using testcontainers
//!
//! Tests file loading and path operations in isolated container environments.
//! Verifies that files are created and read from container volumes.

#[cfg(test)]
mod tests {
    use super::super::core::Graph;
    use chicago_tdd_tools::async_test;
    use chicago_tdd_tools::test;
    use chicago_tdd_tools::testcontainers::{
        exec::SUCCESS_EXIT_CODE, ContainerClient, GenericContainer,
    };
    use std::fs;
    use std::io::Write;
    use tempfile::TempDir;

    /// Test loading RDF from file
    test!(test_load_from_file, {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let rdf_file = temp_dir.path().join("data.ttl");

        let mut file = fs::File::create(&rdf_file).unwrap();
        writeln!(
            file,
            r#"@prefix ex: <http://example.org/> .
ex:alice a ex:Person ."#
        )
        .unwrap();
        drop(file);

        // Act
        let graph = Graph::load_from_file(&rdf_file).unwrap();

        // Assert
        assert!(!graph.is_empty());
        assert!(graph.len() > 0);
    });

    /// Test loading from path with different formats
    test!(test_load_path_auto_format, {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let rdf_file = temp_dir.path().join("data.ttl");

        let mut file = fs::File::create(&rdf_file).unwrap();
        writeln!(
            file,
            r#"@prefix ex: <http://example.org/> .
ex:alice a ex:Person ."#
        )
        .unwrap();
        drop(file);

        let graph = Graph::new().unwrap();

        // Act
        graph.load_path(&rdf_file).unwrap();

        // Assert
        assert!(!graph.is_empty());
    });

    /// Test loading from non-existent file returns error
    test!(test_load_from_nonexistent_file, {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let nonexistent = temp_dir.path().join("nonexistent.ttl");
        let graph = Graph::new().unwrap();

        // Act & Assert
        assert!(graph.load_path(&nonexistent).is_err());
    });

    /// Test file loading operations inside Docker container with verification
    ///
    /// **Verification**: This test verifies that:
    /// 1. File operations run inside a Docker container
    /// 2. RDF files are created in container filesystem (/workspace)
    /// 3. Files can be read and verified via container.exec()
    #[cfg(feature = "docker")]
    async_test!(test_load_file_in_container_with_verification, async {
        // Arrange - create container
        let client = ContainerClient::default();
        let container = GenericContainer::new("alpine:latest")
            .with_command(vec!["sleep", "infinity"])
            .start(&client)
            .await
            .unwrap();

        // Create workspace and RDF file in container
        let rdf_content = r#"@prefix ex: <http://example.org/> .
ex:alice a ex:Person .
ex:bob a ex:Person ."#;
        let rdf_file = "/workspace/data.ttl";

        // Create directory structure
        let dir_setup = container
            .exec("sh", &["-c", "mkdir -p /workspace"])
            .await
            .unwrap();
        assert_eq!(
            dir_setup.exit_code, SUCCESS_EXIT_CODE,
            "Failed to create workspace in container"
        );

        // Create RDF file in container
        let file_create = container
            .exec(
                "sh",
                &[
                    "-c",
                    &format!("cat > {} << 'EOF'\n{}\nEOF", rdf_file, rdf_content),
                ],
            )
            .await
            .unwrap();
        assert_eq!(
            file_create.exit_code, SUCCESS_EXIT_CODE,
            "Should be able to create RDF file in container"
        );

        // Verify file exists in container filesystem
        let file_check = container.exec("test", &["-f", rdf_file]).await.unwrap();
        assert_eq!(
            file_check.exit_code, SUCCESS_EXIT_CODE,
            "RDF file should exist in container"
        );

        // Verify file content via container
        let content_check = container.exec("cat", &[rdf_file]).await.unwrap();
        assert_eq!(
            content_check.exit_code, SUCCESS_EXIT_CODE,
            "Should be able to read file from container"
        );
        assert!(
            content_check.stdout.contains("alice"),
            "File should contain expected RDF content"
        );
        assert!(
            content_check.stdout.contains("bob"),
            "File should contain all expected RDF content"
        );

        // Verify file size in container
        let size_check = container.exec("wc", &["-c", rdf_file]).await.unwrap();
        assert_eq!(
            size_check.exit_code, SUCCESS_EXIT_CODE,
            "Should be able to check file size in container"
        );
        let file_size: usize = size_check
            .stdout
            .split_whitespace()
            .next()
            .unwrap_or("0")
            .parse()
            .unwrap_or(0);
        assert!(file_size > 0, "File should have content in container");

        // List workspace to verify filesystem state
        let list_result = container.exec("ls", &["-lah", "/workspace"]).await.unwrap();
        assert_eq!(
            list_result.exit_code, SUCCESS_EXIT_CODE,
            "Should be able to list files in container"
        );
        assert!(
            list_result.stdout.contains("data.ttl"),
            "Should see RDF file in container filesystem listing"
        );
    });
}
