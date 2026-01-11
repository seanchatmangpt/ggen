//! Integration tests for advanced-cli-tool

use anyhow::Result;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// Mock imports for testing (in real code, these would be from the main crate)
#[path = "../src/config.rs"]
mod config;
#[path = "../src/processor.rs"]
mod processor;

use config::Config;
use processor::{FileProcessor, ProcessingOptions};

/// Test configuration loading and validation
#[test]
fn test_config_lifecycle() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let config_path = temp_dir.path().join("test_config.toml");

    // Create and save default config
    let config = Config::default();
    config.save(&config_path)?;

    // Load and validate
    let loaded = Config::load(&config_path)?;
    loaded.validate()?;

    assert_eq!(config.buffer_size, loaded.buffer_size);
    assert_eq!(config.max_workers, loaded.max_workers);

    Ok(())
}

/// Test file processing with mock data
#[tokio::test]
async fn test_file_processing() -> Result<()> {
    let input_dir = TempDir::new()?;
    let output_dir = TempDir::new()?;

    // Create test files
    let test_file = input_dir.path().join("test.txt");
    fs::write(&test_file, b"Hello, World!")?;

    let nested_dir = input_dir.path().join("nested");
    fs::create_dir(&nested_dir)?;
    let nested_file = nested_dir.join("nested.txt");
    fs::write(&nested_file, b"Nested content")?;

    // Process files
    let options = ProcessingOptions {
        workers: 2,
        compress: false,
        buffer_size: 1024,
    };

    let processor = FileProcessor::new(options);
    let stats = processor
        .process_directory(
            input_dir.path().to_str().unwrap(),
            output_dir.path().to_str().unwrap(),
        )
        .await?;

    assert_eq!(stats.files_processed, 2);
    assert!(stats.bytes_processed > 0);
    assert_eq!(stats.errors, 0);

    // Verify output files exist
    let output_file = output_dir.path().join("test.txt");
    assert!(output_file.exists());

    let output_nested = output_dir.path().join("nested/nested.txt");
    assert!(output_nested.exists());

    Ok(())
}

/// Test path analysis
#[tokio::test]
async fn test_analyze_path() -> Result<()> {
    let temp_dir = TempDir::new()?;

    // Create test files with different extensions
    fs::write(temp_dir.path().join("file1.txt"), b"content1")?;
    fs::write(temp_dir.path().join("file2.txt"), b"content2")?;
    fs::write(temp_dir.path().join("file3.md"), b"markdown")?;

    let processor = FileProcessor::default();
    let analysis = processor
        .analyze_path(temp_dir.path().to_str().unwrap())
        .await?;

    assert_eq!(analysis.total_files, 3);
    assert_eq!(analysis.file_types.get("txt"), Some(&2));
    assert_eq!(analysis.file_types.get("md"), Some(&1));
    assert!(analysis.total_size > 0);

    Ok(())
}

/// Test file conversion
#[tokio::test]
async fn test_file_conversion() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let input_file = temp_dir.path().join("input.txt");
    let output_file = temp_dir.path().join("output.hash");

    fs::write(&input_file, b"Test content for hashing")?;

    let processor = FileProcessor::default();
    processor
        .convert_file(
            input_file.to_str().unwrap(),
            output_file.to_str().unwrap(),
            "hash",
        )
        .await?;

    assert!(output_file.exists());
    let hash_content = fs::read_to_string(output_file)?;
    assert_eq!(hash_content.len(), 64); // SHA-256 hex length

    Ok(())
}

/// Test benchmark functionality
#[tokio::test]
async fn test_benchmark() -> Result<()> {
    let processor = FileProcessor::default();
    let result = processor.benchmark(5, 1).await?;

    assert_eq!(result.iterations, 5);
    assert!(result.total_secs > 0.0);
    assert!(result.avg_secs > 0.0);
    assert!(result.throughput_mbps > 0.0);

    Ok(())
}

/// Test error handling for invalid paths
#[tokio::test]
async fn test_invalid_path_handling() {
    let processor = FileProcessor::default();
    let result = processor
        .process_directory("/nonexistent/path", "/tmp/output")
        .await;

    // Should handle gracefully
    assert!(result.is_ok());
    let stats = result.unwrap();
    assert_eq!(stats.files_processed, 0);
}

/// Test concurrent processing
#[tokio::test]
async fn test_concurrent_processing() -> Result<()> {
    let input_dir = TempDir::new()?;
    let output_dir = TempDir::new()?;

    // Create multiple test files
    for i in 0..10 {
        let file_path = input_dir.path().join(format!("file_{}.txt", i));
        fs::write(&file_path, format!("Content {}", i))?;
    }

    let options = ProcessingOptions {
        workers: 4,
        compress: false,
        buffer_size: 1024,
    };

    let processor = FileProcessor::new(options);
    let stats = processor
        .process_directory(
            input_dir.path().to_str().unwrap(),
            output_dir.path().to_str().unwrap(),
        )
        .await?;

    assert_eq!(stats.files_processed, 10);
    assert!(stats.throughput() > 0.0);

    Ok(())
}
