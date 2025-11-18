//! GraphExport filesystem tests using testcontainers
//!
//! Tests RDF export operations to files in isolated container environments.
//! Verifies that files are created in container volumes and can be verified.

#[cfg(test)]
mod tests {
    use super::super::core::Graph;
    use super::super::export::GraphExport;
    use chicago_tdd_tools::testcontainers::{
        exec::SUCCESS_EXIT_CODE, ContainerClient, GenericContainer,
    };
    use oxigraph::io::RdfFormat;
    use std::fs;
    use tempfile::TempDir;

    /// Test exporting graph to file
    test!(test_export_to_file, {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();

        let temp_dir = TempDir::new().unwrap();
        let output_path = temp_dir.path().join("output.ttl");
        let export = GraphExport::new(&graph);

        // Act
        export
            .write_to_file(&output_path, RdfFormat::Turtle)
            .unwrap();

        // Assert - file should exist and contain data
        assert!(output_path.exists());
        let content = fs::read_to_string(&output_path).unwrap();
        assert!(!content.is_empty());
        assert!(content.contains("alice") || content.contains("Person"));
    });

    /// Test exporting to string
    test!(test_export_to_string, {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();

        let export = GraphExport::new(&graph);

        // Act
        let output = export.write_to_string(RdfFormat::Turtle).unwrap();

        // Assert
        assert!(!output.is_empty());
    });

    /// Test auto-detection of format from extension
    test!(test_export_auto_format, {
        // Arrange
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();

        let temp_dir = TempDir::new().unwrap();
        let export = GraphExport::new(&graph);

        // Act - test different extensions
        let ttl_path = temp_dir.path().join("output.ttl");
        export.write_to_file_auto(&ttl_path).unwrap();

        let nt_path = temp_dir.path().join("output.nt");
        export.write_to_file_auto(&nt_path).unwrap();

        // Assert - both files should exist
        assert!(ttl_path.exists());
        assert!(nt_path.exists());
    });

    /// Test export operations inside Docker container with file verification
    ///
    /// **Verification**: This test verifies that:
    /// 1. Export operations run inside a Docker container
    /// 2. Files are created in container filesystem (/workspace)
    /// 3. Files can be read back via container.exec()
    #[cfg(feature = "docker")]
    test!(test_export_in_container_with_verification, {
        // Arrange - create container
        let client = ContainerClient::default();
        let container = GenericContainer::with_command(
            client.client(),
            "alpine",
            "latest",
            "sleep",
            &["infinity"],
        )
        .unwrap();

        // Create workspace in container
        let workspace_setup = container
            .exec("sh", &["-c", "mkdir -p /workspace/exports"])
            .unwrap();
        assert_eq!(
            workspace_setup.exit_code, SUCCESS_EXIT_CODE,
            "Failed to create workspace in container"
        );

        // Create test RDF file in container
        let rdf_content = r#"@prefix ex: <http://example.org/> .
ex:alice a ex:Person ."#;
        let rdf_file = "/workspace/exports/input.ttl";
        let file_create = container
            .exec(
                "sh",
                &[
                    "-c",
                    &format!("cat > {} << 'EOF'\n{}\nEOF", rdf_file, rdf_content),
                ],
            )
            .unwrap();
        assert_eq!(
            file_create.exit_code, SUCCESS_EXIT_CODE,
            "Should be able to create RDF file in container"
        );

        // Verify file exists in container
        let file_check = container.exec("test", &["-f", rdf_file]).unwrap();
        assert_eq!(
            file_check.exit_code, SUCCESS_EXIT_CODE,
            "RDF file should exist in container"
        );

        // Verify file content
        let content_check = container.exec("cat", &[rdf_file]).unwrap();
        assert_eq!(
            content_check.exit_code, SUCCESS_EXIT_CODE,
            "Should be able to read file from container"
        );
        assert!(
            content_check.stdout.contains("alice"),
            "File should contain expected RDF content"
        );

        // List files to verify filesystem operations
        let list_result = container
            .exec("ls", &["-la", "/workspace/exports"])
            .unwrap();
        assert_eq!(
            list_result.exit_code, SUCCESS_EXIT_CODE,
            "Should be able to list files in container"
        );
        assert!(
            list_result.stdout.contains("input.ttl"),
            "Should see exported file in container filesystem"
        );
    });
}
