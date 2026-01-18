//! Basic usage example for the cleanroom testing framework
//!
//! This example demonstrates how to use cleanroom for isolated testing
//! with temporary filesystem isolation, performance metrics, and validation.

use cleanroom::{
    CleanroomConfig, CleanroomConfigBuilder, CleanroomEnv, TestStatus, ValidationResult,
    ValidationSuite, Validator,
};
use std::time::Duration;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧪 Cleanroom Testing Framework - Basic Usage Example\n");

    // Example 1: Basic cleanroom test
    basic_cleanroom_test()?;

    // Example 2: Custom configuration
    custom_configuration_test()?;

    // Example 3: Performance benchmarking
    performance_benchmarking_test()?;

    // Example 4: Custom validation
    custom_validation_test()?;

    println!("\n✅ All examples completed successfully!");
    Ok(())
}

/// Example 1: Basic cleanroom test with default configuration
fn basic_cleanroom_test() -> Result<(), Box<dyn std::error::Error>> {
    println!("📝 Example 1: Basic Cleanroom Test");

    let config = CleanroomConfig::default();
    let mut env = CleanroomEnv::new(config)?;

    let result = env.run_test(|env| {
        // Write test data
        env.write_file("data.txt", "Hello, Cleanroom!")?;

        // Create subdirectory
        env.create_dir("subdir")?;
        env.write_file("subdir/nested.txt", "Nested file")?;

        // Read and validate
        let content = env.read_file("data.txt")?;
        assert_eq!(content, "Hello, Cleanroom!");

        let nested = env.read_file("subdir/nested.txt")?;
        assert_eq!(nested, "Nested file");

        Ok(())
    });

    println!(
        "   ✓ Test completed in {}ms - Status: {:?}",
        result.duration_ms, result.status
    );
    assert_eq!(result.status, TestStatus::Passed);
    println!();
    Ok(())
}

/// Example 2: Custom configuration with builder pattern
fn custom_configuration_test() -> Result<(), Box<dyn std::error::Error>> {
    println!("📝 Example 2: Custom Configuration");

    let config = CleanroomConfigBuilder::new()
        .timeout(Duration::from_secs(5))
        .benchmarking(true)
        .logging(true)
        .concurrency(4)
        .scalability(true, 50)  // Enable scalability with 50 iterations
        .error_validation(true)
        .auto_cleanup(true)
        .build();

    let mut env = CleanroomEnv::new(config)?;

    let result = env.run_test(|env| {
        // Perform test operations
        for i in 0..10 {
            env.write_file(&format!("file_{}.txt", i), &format!("Content {}", i))?;
        }

        // Verify all files created
        for i in 0..10 {
            let content = env.read_file(&format!("file_{}.txt", i))?;
            assert_eq!(content, format!("Content {}", i));
        }

        Ok(())
    });

    println!(
        "   ✓ Test completed in {}ms",
        result.duration_ms
    );
    if let Some(metrics) = result.metrics {
        println!("   ✓ Metrics collected: {} entries", metrics.all().len());
    }
    println!();
    Ok(())
}

/// Example 3: Performance benchmarking
fn performance_benchmarking_test() -> Result<(), Box<dyn std::error::Error>> {
    println!("📝 Example 3: Performance Benchmarking");

    let config = CleanroomConfigBuilder::new()
        .benchmarking(true)
        .build();

    let mut env = CleanroomEnv::new(config)?;

    let result = env.run_test(|env| {
        let start = std::time::Instant::now();

        // Simulate some work
        for i in 0..100 {
            env.write_file(&format!("bench_{}.txt", i), "benchmark data")?;
        }

        let duration = start.elapsed();
        println!("   ✓ Wrote 100 files in {:.2}ms", duration.as_secs_f64() * 1000.0);

        Ok(())
    });

    println!(
        "   ✓ Benchmark completed in {}ms",
        result.duration_ms
    );
    println!();
    Ok(())
}

/// Example 4: Custom validation with ValidationSuite
fn custom_validation_test() -> Result<(), Box<dyn std::error::Error>> {
    println!("📝 Example 4: Custom Validation");

    // Create custom validator
    struct FileCountValidator {
        expected_count: usize,
    }

    impl Validator for FileCountValidator {
        fn validate(&self, env: &CleanroomEnv) -> cleanroom::Result<ValidationResult> {
            let mut count = 0;
            let path = env.path();

            if let Ok(entries) = std::fs::read_dir(path) {
                count = entries.filter(|e| e.is_ok()).count();
            }

            let passed = count == self.expected_count;
            let mut result = ValidationResult::new(passed);
            result.add_message(format!("Found {} files, expected {}", count, self.expected_count));
            result.set_score(if passed { 1.0 } else { count as f64 / self.expected_count as f64 });

            Ok(result)
        }

        fn name(&self) -> &str {
            "FileCountValidator"
        }
    }

    let config = CleanroomConfig::default();
    let env = CleanroomEnv::new(config)?;

    // Create files
    for i in 0..5 {
        env.write_file(&format!("test_{}.txt", i), "data")?;
    }

    // Create validation suite
    let mut suite = ValidationSuite::new("File Count Validation");
    suite.add_validator(Box::new(FileCountValidator { expected_count: 5 }));

    // Run validation
    let results = suite.run(&env)?;
    println!(
        "   ✓ Validation suite completed - {}/{} passed",
        results.iter().filter(|r| r.passed).count(),
        results.len()
    );

    for result in &results {
        let status = if result.passed { "PASS" } else { "FAIL" };
        for message in &result.messages {
            println!("   ✓ [{}] {}", status, message);
        }
        println!("   ✓ Score: {:.2}", result.score);
    }
    println!();
    Ok(())
}
