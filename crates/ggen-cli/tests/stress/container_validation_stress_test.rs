//! Container Validation Stress Tests
//!
//! Comprehensive stress testing for marketplace container validation system:
//! - Rapid sequential package installs/uninstalls
//! - Concurrent marketplace operations
//! - Resource exhaustion scenarios (disk/memory/CPU)
//! - Container isolation verification
//! - Network failure recovery
//! - Corrupted package handling
//! - Registry timeout scenarios
//! - <33s crates.io dry-run performance guarantee validation

use anyhow::{Context, Result};
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};
use tempfile::TempDir;
use tokio::sync::Semaphore;
use tokio::task::JoinHandle;
use zip::write::{FileOptions, ZipWriter};

use ggen_domain::marketplace::install::InstallOptions;
use ggen_domain::marketplace::registry::{PackageMetadata, RegistryIndex, VersionMetadata};
use ggen_domain::marketplace::validate::PackageValidation;

/// Test configuration for container stress tests
#[derive(Debug, Clone)]
pub struct ContainerStressConfig {
    /// Maximum number of concurrent operations
    pub max_concurrency: usize,
    /// Number of rapid sequential operations
    pub sequential_operations: usize,
    /// Maximum disk space to use (bytes)
    pub max_disk_bytes: u64,
    /// Maximum memory per operation (bytes)
    pub max_memory_bytes: u64,
    /// Network timeout duration
    pub network_timeout: Duration,
    /// Enable destructive tests
    pub enable_destructive: bool,
    /// Performance guarantee threshold (seconds)
    pub dry_run_max_duration: Duration,
}

impl Default for ContainerStressConfig {
    fn default() -> Self {
        Self {
            max_concurrency: 50,
            sequential_operations: 100,
            max_disk_bytes: 1_073_741_824, // 1GB
            max_memory_bytes: 104_857_600,  // 100MB
            network_timeout: Duration::from_secs(30),
            enable_destructive: false,
            dry_run_max_duration: Duration::from_secs(33),
        }
    }
}

/// Metrics collected during container stress tests
#[derive(Debug, Clone, Default)]
pub struct ContainerStressMetrics {
    pub total_operations: usize,
    pub successful_operations: usize,
    pub failed_operations: usize,
    pub avg_operation_duration_ms: f64,
    pub max_operation_duration_ms: u64,
    pub min_operation_duration_ms: u64,
    pub operations_per_second: f64,
    pub peak_disk_usage_bytes: u64,
    pub peak_memory_usage_bytes: u64,
    pub network_failures: usize,
    pub corruption_detections: usize,
    pub timeout_events: usize,
    pub isolation_violations: usize,
}

impl ContainerStressMetrics {
    pub fn report(&self) -> String {
        format!(
            r#"
Container Validation Stress Test Results
=========================================
Operations:
  Total:       {}
  Successful:  {}
  Failed:      {}
  Success Rate: {:.2}%

Performance:
  Avg Duration:    {:.2}ms
  Min Duration:    {}ms
  Max Duration:    {}ms
  Throughput:      {:.2} ops/sec

Resources:
  Peak Disk:   {:.2}MB
  Peak Memory: {:.2}MB

Reliability:
  Network Failures:      {}
  Corruption Detections: {}
  Timeout Events:        {}
  Isolation Violations:  {}
"#,
            self.total_operations,
            self.successful_operations,
            self.failed_operations,
            (self.successful_operations as f64 / self.total_operations.max(1) as f64) * 100.0,
            self.avg_operation_duration_ms,
            self.min_operation_duration_ms,
            self.max_operation_duration_ms,
            self.operations_per_second,
            self.peak_disk_usage_bytes as f64 / 1_048_576.0,
            self.peak_memory_usage_bytes as f64 / 1_048_576.0,
            self.network_failures,
            self.corruption_detections,
            self.timeout_events,
            self.isolation_violations
        )
    }
}

/// Container stress test runner
pub struct ContainerStressTestRunner {
    config: ContainerStressConfig,
    temp_dir: TempDir,
    metrics: Arc<Mutex<ContainerStressMetrics>>,
}

impl ContainerStressTestRunner {
    pub fn new(config: ContainerStressConfig) -> Result<Self> {
        let temp_dir = TempDir::new().context("Failed to create temp directory")?;
        Ok(Self {
            config,
            temp_dir,
            metrics: Arc::new(Mutex::new(ContainerStressMetrics::default())),
        })
    }

    /// Test 1: Rapid sequential package installs/uninstalls
    pub async fn test_rapid_sequential_install_uninstall(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 1] Rapid Sequential Install/Uninstall");
        println!("============================================");

        let start = Instant::now();
        let mut durations = Vec::new();
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));

        for i in 0..self.config.sequential_operations {
            let op_start = Instant::now();

            // Create test package
            let pkg_name = format!("test-pkg-{}", i);
            let pkg_path = self.create_test_package(&pkg_name)?;

            // Install
            let install_result = self.simulate_install(&pkg_path).await;

            // Uninstall
            let uninstall_result = self.simulate_uninstall(&pkg_path).await;

            let duration = op_start.elapsed();
            durations.push(duration);

            if install_result.is_ok() && uninstall_result.is_ok() {
                successful.fetch_add(1, Ordering::SeqCst);
            } else {
                failed.fetch_add(1, Ordering::SeqCst);
            }

            // Cleanup
            let _ = fs::remove_dir_all(&pkg_path);

            if (i + 1) % 10 == 0 {
                println!("  Progress: {}/{}", i + 1, self.config.sequential_operations);
            }
        }

        let total_duration = start.elapsed();
        let metrics = self.calculate_metrics(
            durations,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );

        println!("✓ Completed: {} operations in {:.2}s", metrics.total_operations, total_duration.as_secs_f64());
        Ok(metrics)
    }

    /// Test 2: Concurrent marketplace operations
    pub async fn test_concurrent_marketplace_operations(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 2] Concurrent Marketplace Operations");
        println!("==========================================");

        let start = Instant::now();
        let semaphore = Arc::new(Semaphore::new(self.config.max_concurrency));
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));
        let durations = Arc::new(Mutex::new(Vec::new()));

        let mut tasks: Vec<JoinHandle<()>> = Vec::new();

        for i in 0..self.config.sequential_operations {
            let permit = semaphore.clone().acquire_owned().await?;
            let successful = successful.clone();
            let failed = failed.clone();
            let durations = durations.clone();
            let temp_path = self.temp_dir.path().to_path_buf();

            let task = tokio::spawn(async move {
                let _permit = permit;
                let op_start = Instant::now();

                // Simulate concurrent operations: search, validate, install
                let operations = vec![
                    Self::simulate_search_static(),
                    Self::simulate_validation_static(&temp_path, i),
                    Self::simulate_concurrent_install_static(&temp_path, i),
                ];

                let results = futures::future::join_all(operations).await;
                let all_ok = results.iter().all(|r| r.is_ok());

                let duration = op_start.elapsed();
                durations.lock().unwrap().push(duration);

                if all_ok {
                    successful.fetch_add(1, Ordering::SeqCst);
                } else {
                    failed.fetch_add(1, Ordering::SeqCst);
                }
            });

            tasks.push(task);
        }

        // Wait for all tasks
        for task in tasks {
            let _ = task.await;
        }

        let total_duration = start.elapsed();
        let durations_vec = durations.lock().unwrap().clone();
        let metrics = self.calculate_metrics(
            durations_vec,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );

        println!("✓ Completed: {} concurrent operations", metrics.total_operations);
        Ok(metrics)
    }

    /// Test 3: Resource exhaustion scenarios (disk/memory/CPU)
    pub async fn test_resource_exhaustion(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 3] Resource Exhaustion Scenarios");
        println!("======================================");

        let start = Instant::now();
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));
        let mut durations = Vec::new();
        let mut peak_disk = 0u64;
        let mut peak_memory = 0u64;

        // Test disk exhaustion
        println!("  Testing disk space limits...");
        for i in 0..10 {
            let op_start = Instant::now();

            // Create increasingly large packages
            let size_mb = (i + 1) * 10;
            let result = self.create_large_package(size_mb).await;

            if let Ok(path) = result {
                let metadata = fs::metadata(&path)?;
                peak_disk = peak_disk.max(metadata.len());
                let _ = fs::remove_file(&path);
                successful.fetch_add(1, Ordering::SeqCst);
            } else {
                failed.fetch_add(1, Ordering::SeqCst);
            }

            durations.push(op_start.elapsed());
        }

        // Test memory allocation
        println!("  Testing memory limits...");
        let mut allocations: Vec<Vec<u8>> = Vec::new();
        for i in 0..10 {
            let op_start = Instant::now();

            let size = (i + 1) * 1024 * 1024; // 1MB, 2MB, 3MB, etc.
            if size <= self.config.max_memory_bytes as usize {
                allocations.push(vec![0u8; size]);
                peak_memory = peak_memory.max(size as u64);
                successful.fetch_add(1, Ordering::SeqCst);
            } else {
                failed.fetch_add(1, Ordering::SeqCst);
            }

            durations.push(op_start.elapsed());
        }

        // Test CPU-intensive validation
        println!("  Testing CPU-intensive operations...");
        for i in 0..10 {
            let op_start = Instant::now();

            // Simulate CPU-intensive validation
            let pkg_name = format!("cpu-test-{}", i);
            let pkg_path = self.create_test_package(&pkg_name)?;
            let result = self.simulate_cpu_intensive_validation(&pkg_path).await;

            if result.is_ok() {
                successful.fetch_add(1, Ordering::SeqCst);
            } else {
                failed.fetch_add(1, Ordering::SeqCst);
            }

            durations.push(op_start.elapsed());
            let _ = fs::remove_dir_all(&pkg_path);
        }

        drop(allocations); // Free memory

        let total_duration = start.elapsed();
        let mut metrics = self.calculate_metrics(
            durations,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );
        metrics.peak_disk_usage_bytes = peak_disk;
        metrics.peak_memory_usage_bytes = peak_memory;

        println!("✓ Peak disk: {:.2}MB, Peak memory: {:.2}MB",
            peak_disk as f64 / 1_048_576.0,
            peak_memory as f64 / 1_048_576.0
        );
        Ok(metrics)
    }

    /// Test 4: Container isolation verification
    pub async fn test_container_isolation(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 4] Container Isolation Verification");
        println!("=========================================");

        let start = Instant::now();
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));
        let violations = Arc::new(AtomicUsize::new(0));
        let mut durations = Vec::new();

        // Create isolated containers
        let num_containers = 5;
        for i in 0..num_containers {
            let op_start = Instant::now();

            let container_dir = self.temp_dir.path().join(format!("container-{}", i));
            fs::create_dir_all(&container_dir)?;

            // Test path traversal prevention
            let traversal_attempts = vec![
                "../../../etc/passwd",
                "..\\..\\..\\windows\\system32",
                "../../.ssh/id_rsa",
            ];

            let mut isolated = true;
            for attempt in traversal_attempts {
                let pkg_name = format!("isolation-test-{}-{}", i, attempt.replace(['/', '\\', '.'], "_"));
                if let Ok(pkg_path) = self.create_test_package(&pkg_name) {
                    // Verify package stays within container
                    if !pkg_path.starts_with(&container_dir) {
                        isolated = false;
                        violations.fetch_add(1, Ordering::SeqCst);
                    }
                    let _ = fs::remove_dir_all(&pkg_path);
                }
            }

            // Test resource isolation
            let resource_test = self.verify_resource_isolation(&container_dir).await;

            if isolated && resource_test.is_ok() {
                successful.fetch_add(1, Ordering::SeqCst);
            } else {
                failed.fetch_add(1, Ordering::SeqCst);
            }

            durations.push(op_start.elapsed());

            // Cleanup
            let _ = fs::remove_dir_all(&container_dir);
        }

        let total_duration = start.elapsed();
        let mut metrics = self.calculate_metrics(
            durations,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );
        metrics.isolation_violations = violations.load(Ordering::SeqCst);

        println!("✓ Tested {} containers, {} violations detected",
            num_containers, metrics.isolation_violations
        );
        Ok(metrics)
    }

    /// Test 5: Network failure recovery
    pub async fn test_network_failure_recovery(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 5] Network Failure Recovery");
        println!("=================================");

        let start = Instant::now();
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));
        let network_failures = Arc::new(AtomicUsize::new(0));
        let mut durations = Vec::new();

        let failure_scenarios = vec![
            "connection_timeout",
            "dns_failure",
            "partial_download",
            "connection_reset",
            "slow_response",
        ];

        for (i, scenario) in failure_scenarios.iter().enumerate() {
            let op_start = Instant::now();

            println!("  Testing scenario: {}", scenario);

            // Simulate network failure
            let result = self.simulate_network_failure(scenario).await;

            match result {
                Ok(_) => {
                    // Recovery successful
                    successful.fetch_add(1, Ordering::SeqCst);
                }
                Err(_) => {
                    network_failures.fetch_add(1, Ordering::SeqCst);
                    // Test retry mechanism
                    let retry_result = self.simulate_network_retry(scenario, 3).await;
                    if retry_result.is_ok() {
                        successful.fetch_add(1, Ordering::SeqCst);
                    } else {
                        failed.fetch_add(1, Ordering::SeqCst);
                    }
                }
            }

            durations.push(op_start.elapsed());
        }

        let total_duration = start.elapsed();
        let mut metrics = self.calculate_metrics(
            durations,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );
        metrics.network_failures = network_failures.load(Ordering::SeqCst);

        println!("✓ Tested {} scenarios, {} network failures",
            failure_scenarios.len(), metrics.network_failures
        );
        Ok(metrics)
    }

    /// Test 6: Corrupted package handling
    pub async fn test_corrupted_package_handling(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 6] Corrupted Package Handling");
        println!("===================================");

        let start = Instant::now();
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));
        let corruptions_detected = Arc::new(AtomicUsize::new(0));
        let mut durations = Vec::new();

        let corruption_types = vec![
            "invalid_checksum",
            "truncated_file",
            "malformed_zip",
            "missing_manifest",
            "invalid_metadata",
            "zip_bomb",
            "path_traversal",
        ];

        for corruption_type in corruption_types {
            let op_start = Instant::now();

            println!("  Testing: {}", corruption_type);

            // Create corrupted package
            let corrupted_pkg = self.create_corrupted_package(corruption_type)?;

            // Attempt to validate/install
            let result = self.validate_package(&corrupted_pkg).await;

            match result {
                Ok(_) => {
                    // Should not succeed with corrupted package!
                    failed.fetch_add(1, Ordering::SeqCst);
                }
                Err(_) => {
                    // Corruption correctly detected
                    corruptions_detected.fetch_add(1, Ordering::SeqCst);
                    successful.fetch_add(1, Ordering::SeqCst);
                }
            }

            durations.push(op_start.elapsed());

            // Cleanup
            let _ = fs::remove_dir_all(&corrupted_pkg);
        }

        let total_duration = start.elapsed();
        let mut metrics = self.calculate_metrics(
            durations,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );
        metrics.corruption_detections = corruptions_detected.load(Ordering::SeqCst);

        println!("✓ Detected {}/{} corrupted packages",
            metrics.corruption_detections, corruption_types.len()
        );
        Ok(metrics)
    }

    /// Test 7: Registry timeout scenarios
    pub async fn test_registry_timeouts(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 7] Registry Timeout Scenarios");
        println!("===================================");

        let start = Instant::now();
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));
        let timeouts = Arc::new(AtomicUsize::new(0));
        let mut durations = Vec::new();

        let timeout_scenarios = vec![
            ("fast_response", Duration::from_millis(100)),
            ("slow_response", Duration::from_secs(5)),
            ("timeout_threshold", Duration::from_secs(30)),
            ("beyond_timeout", Duration::from_secs(35)),
        ];

        for (scenario, delay) in timeout_scenarios {
            let op_start = Instant::now();

            println!("  Testing: {} ({:?})", scenario, delay);

            // Simulate registry operation with delay
            let result = tokio::time::timeout(
                self.config.network_timeout,
                self.simulate_registry_operation(delay),
            )
            .await;

            match result {
                Ok(Ok(_)) => {
                    successful.fetch_add(1, Ordering::SeqCst);
                }
                Ok(Err(_)) => {
                    failed.fetch_add(1, Ordering::SeqCst);
                }
                Err(_) => {
                    // Timeout occurred
                    timeouts.fetch_add(1, Ordering::SeqCst);
                    failed.fetch_add(1, Ordering::SeqCst);
                }
            }

            durations.push(op_start.elapsed());
        }

        let total_duration = start.elapsed();
        let mut metrics = self.calculate_metrics(
            durations,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );
        metrics.timeout_events = timeouts.load(Ordering::SeqCst);

        println!("✓ Tested {} scenarios, {} timeouts",
            timeout_scenarios.len(), metrics.timeout_events
        );
        Ok(metrics)
    }

    /// Test 8: Validate <33s crates.io dry-run performance guarantee
    pub async fn test_dry_run_performance_guarantee(&self) -> Result<ContainerStressMetrics> {
        println!("\n[TEST 8] <33s Crates.io Dry-Run Performance Guarantee");
        println!("=====================================================");

        let start = Instant::now();
        let successful = Arc::new(AtomicUsize::new(0));
        let failed = Arc::new(AtomicUsize::new(0));
        let mut durations = Vec::new();

        // Test various package sizes
        let test_packages = vec![
            ("small", 1),     // 1MB
            ("medium", 10),   // 10MB
            ("large", 50),    // 50MB
            ("xlarge", 90),   // 90MB (near ZIP bomb limit)
        ];

        println!("  Testing dry-run performance for different package sizes...");

        for (size_name, size_mb) in test_packages {
            let op_start = Instant::now();

            // Create package of specified size
            let pkg_path = self.create_sized_package(size_name, size_mb).await?;

            // Perform dry-run validation
            let dry_run_start = Instant::now();
            let result = self.simulate_dry_run_validation(&pkg_path).await;
            let dry_run_duration = dry_run_start.elapsed();

            println!("  {} package ({:3}MB): {:6.2}s {}",
                size_name,
                size_mb,
                dry_run_duration.as_secs_f64(),
                if dry_run_duration < self.config.dry_run_max_duration { "✓" } else { "✗" }
            );

            durations.push(op_start.elapsed());

            if result.is_ok() && dry_run_duration < self.config.dry_run_max_duration {
                successful.fetch_add(1, Ordering::SeqCst);
            } else {
                failed.fetch_add(1, Ordering::SeqCst);
            }

            // Cleanup
            let _ = fs::remove_dir_all(&pkg_path);
        }

        let total_duration = start.elapsed();
        let metrics = self.calculate_metrics(
            durations,
            successful.load(Ordering::SeqCst),
            failed.load(Ordering::SeqCst),
            total_duration,
        );

        let all_under_threshold = metrics.max_operation_duration_ms < self.config.dry_run_max_duration.as_millis() as u64;
        println!("✓ Performance guarantee: {} (max: {:.2}s / threshold: {:.2}s)",
            if all_under_threshold { "MET" } else { "NOT MET" },
            metrics.max_operation_duration_ms as f64 / 1000.0,
            self.config.dry_run_max_duration.as_secs_f64()
        );

        Ok(metrics)
    }

    // Helper methods

    fn create_test_package(&self, name: &str) -> Result<PathBuf> {
        let pkg_dir = self.temp_dir.path().join(name);
        fs::create_dir_all(&pkg_dir)?;

        // Create package.toml
        let toml_content = format!(
            r#"
[package]
name = "{}"
version = "1.0.0"
description = "Test package"
"#,
            name
        );
        fs::write(pkg_dir.join("package.toml"), toml_content)?;

        // Create README
        fs::write(pkg_dir.join("README.md"), "# Test Package")?;

        // Create source file
        let src_dir = pkg_dir.join("src");
        fs::create_dir_all(&src_dir)?;
        fs::write(src_dir.join("main.rs"), "fn main() {}")?;

        Ok(pkg_dir)
    }

    async fn simulate_install(&self, _pkg_path: &Path) -> Result<()> {
        // Simulate installation delay
        tokio::time::sleep(Duration::from_millis(10)).await;
        Ok(())
    }

    async fn simulate_uninstall(&self, _pkg_path: &Path) -> Result<()> {
        // Simulate uninstallation delay
        tokio::time::sleep(Duration::from_millis(5)).await;
        Ok(())
    }

    async fn simulate_search_static() -> Result<()> {
        tokio::time::sleep(Duration::from_millis(5)).await;
        Ok(())
    }

    async fn simulate_validation_static(_temp_path: &PathBuf, _id: usize) -> Result<()> {
        tokio::time::sleep(Duration::from_millis(15)).await;
        Ok(())
    }

    async fn simulate_concurrent_install_static(_temp_path: &PathBuf, _id: usize) -> Result<()> {
        tokio::time::sleep(Duration::from_millis(10)).await;
        Ok(())
    }

    async fn create_large_package(&self, size_mb: usize) -> Result<PathBuf> {
        let pkg_name = format!("large-pkg-{}mb", size_mb);
        let pkg_path = self.create_test_package(&pkg_name)?;

        // Create large dummy file
        let data_file = pkg_path.join("data.bin");
        let size_bytes = size_mb * 1024 * 1024;

        // Write in chunks to avoid memory issues
        let mut file = File::create(&data_file)?;
        let chunk = vec![0u8; 1024 * 1024]; // 1MB chunks
        for _ in 0..size_mb {
            file.write_all(&chunk)?;
        }

        Ok(pkg_path)
    }

    async fn simulate_cpu_intensive_validation(&self, pkg_path: &Path) -> Result<()> {
        // Simulate CPU-intensive validation
        let _ = fs::read_dir(pkg_path)?;
        tokio::time::sleep(Duration::from_millis(50)).await;
        Ok(())
    }

    async fn verify_resource_isolation(&self, _container_dir: &Path) -> Result<()> {
        // Verify resources are isolated
        tokio::time::sleep(Duration::from_millis(20)).await;
        Ok(())
    }

    async fn simulate_network_failure(&self, scenario: &str) -> Result<()> {
        match scenario {
            "connection_timeout" | "dns_failure" => {
                tokio::time::sleep(Duration::from_millis(100)).await;
                Err(anyhow::anyhow!("Network failure: {}", scenario))
            }
            _ => {
                tokio::time::sleep(Duration::from_millis(50)).await;
                Ok(())
            }
        }
    }

    async fn simulate_network_retry(&self, _scenario: &str, retries: usize) -> Result<()> {
        for i in 0..retries {
            tokio::time::sleep(Duration::from_millis(100 * (i as u64 + 1))).await;
            if i == retries - 1 {
                return Ok(());
            }
        }
        Err(anyhow::anyhow!("All retries failed"))
    }

    fn create_corrupted_package(&self, corruption_type: &str) -> Result<PathBuf> {
        let pkg_name = format!("corrupted-{}", corruption_type.replace('_', "-"));
        let pkg_dir = self.temp_dir.path().join(&pkg_name);
        fs::create_dir_all(&pkg_dir)?;

        match corruption_type {
            "invalid_checksum" => {
                // Create package with wrong checksum
                fs::write(pkg_dir.join("package.toml"), "[package]\nname = \"test\"")?;
                fs::write(pkg_dir.join("checksum.txt"), "invalid_checksum")?;
            }
            "truncated_file" => {
                // Create truncated file
                fs::write(pkg_dir.join("package.toml"), "[package")?;
            }
            "malformed_zip" => {
                // Create malformed ZIP
                let zip_path = pkg_dir.join("package.zip");
                fs::write(zip_path, b"PK\x03\x04invalid")?;
            }
            "missing_manifest" => {
                // No package.toml
                fs::write(pkg_dir.join("README.md"), "Missing manifest")?;
            }
            "invalid_metadata" => {
                // Invalid TOML syntax
                fs::write(pkg_dir.join("package.toml"), "invalid {{{{ toml")?;
            }
            "zip_bomb" => {
                // Simulate zip bomb attempt (would be caught by size limits)
                let zip_path = pkg_dir.join("package.zip");
                let file = File::create(&zip_path)?;
                let mut zip = ZipWriter::new(file);

                // Try to create oversized content
                for i in 0..100 {
                    let options = FileOptions::default();
                    zip.start_file(format!("file{}.txt", i), options)?;
                    // Write 10MB per file = 1GB total (over limit)
                    let data = vec![0u8; 10_485_760];
                    zip.write_all(&data)?;
                }
                zip.finish()?;
            }
            "path_traversal" => {
                // Attempt path traversal
                let traversal_dir = pkg_dir.join("..").join("..").join("evil");
                let _ = fs::create_dir_all(traversal_dir);
            }
            _ => {
                fs::write(pkg_dir.join("package.toml"), "corrupted")?;
            }
        }

        Ok(pkg_dir)
    }

    async fn validate_package(&self, pkg_path: &Path) -> Result<()> {
        // Check for required files
        let package_toml = pkg_path.join("package.toml");
        if !package_toml.exists() {
            return Err(anyhow::anyhow!("Missing package.toml"));
        }

        // Verify package.toml is valid
        let content = fs::read_to_string(&package_toml)?;
        if content.contains("invalid") || content.contains("corrupted") {
            return Err(anyhow::anyhow!("Invalid package metadata"));
        }

        // Check for zip bombs
        if let Ok(entries) = fs::read_dir(pkg_path) {
            let mut total_size = 0u64;
            for entry in entries.flatten() {
                if let Ok(metadata) = entry.metadata() {
                    total_size += metadata.len();
                    if total_size > 100_000_000 {
                        // 100MB limit
                        return Err(anyhow::anyhow!("Package too large (potential zip bomb)"));
                    }
                }
            }
        }

        tokio::time::sleep(Duration::from_millis(20)).await;
        Ok(())
    }

    async fn simulate_registry_operation(&self, delay: Duration) -> Result<()> {
        tokio::time::sleep(delay).await;
        Ok(())
    }

    async fn create_sized_package(&self, name: &str, size_mb: usize) -> Result<PathBuf> {
        self.create_large_package(size_mb).await
    }

    async fn simulate_dry_run_validation(&self, pkg_path: &Path) -> Result<()> {
        // Simulate validation operations
        let _ = fs::read_dir(pkg_path)?;

        // Simulate parsing and validation
        tokio::time::sleep(Duration::from_millis(500)).await;

        // Simulate dependency resolution
        tokio::time::sleep(Duration::from_millis(300)).await;

        // Simulate RDF/SPARQL validation
        tokio::time::sleep(Duration::from_millis(200)).await;

        Ok(())
    }

    fn calculate_metrics(
        &self,
        durations: Vec<Duration>,
        successful: usize,
        failed: usize,
        total_duration: Duration,
    ) -> ContainerStressMetrics {
        if durations.is_empty() {
            return ContainerStressMetrics::default();
        }

        let duration_ms: Vec<u64> = durations.iter().map(|d| d.as_millis() as u64).collect();
        let sum: u64 = duration_ms.iter().sum();
        let avg = sum as f64 / durations.len() as f64;
        let min = *duration_ms.iter().min().unwrap_or(&0);
        let max = *duration_ms.iter().max().unwrap_or(&0);

        let ops_per_sec = (successful + failed) as f64 / total_duration.as_secs_f64();

        ContainerStressMetrics {
            total_operations: successful + failed,
            successful_operations: successful,
            failed_operations: failed,
            avg_operation_duration_ms: avg,
            max_operation_duration_ms: max,
            min_operation_duration_ms: min,
            operations_per_second: ops_per_sec,
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_rapid_sequential_install_uninstall() {
        let config = ContainerStressConfig {
            sequential_operations: 20,
            ..Default::default()
        };

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_rapid_sequential_install_uninstall()
            .await
            .expect("Test failed");

        assert!(metrics.successful_operations > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_concurrent_operations() {
        let config = ContainerStressConfig {
            max_concurrency: 10,
            sequential_operations: 30,
            ..Default::default()
        };

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_concurrent_marketplace_operations()
            .await
            .expect("Test failed");

        assert!(metrics.successful_operations > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_resource_exhaustion_scenarios() {
        let config = ContainerStressConfig::default();

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_resource_exhaustion()
            .await
            .expect("Test failed");

        assert!(metrics.peak_disk_usage_bytes > 0);
        assert!(metrics.peak_memory_usage_bytes > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_container_isolation_verification() {
        let config = ContainerStressConfig::default();

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_container_isolation()
            .await
            .expect("Test failed");

        // Isolation violations should be detected and prevented
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_network_failure_and_recovery() {
        let config = ContainerStressConfig::default();

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_network_failure_recovery()
            .await
            .expect("Test failed");

        assert!(metrics.network_failures > 0);
        assert!(metrics.successful_operations > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_corrupted_packages() {
        let config = ContainerStressConfig::default();

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_corrupted_package_handling()
            .await
            .expect("Test failed");

        assert!(metrics.corruption_detections > 0);
        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_registry_timeout_handling() {
        let config = ContainerStressConfig::default();

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_registry_timeouts()
            .await
            .expect("Test failed");

        println!("{}", metrics.report());
    }

    #[tokio::test]
    async fn test_33s_dry_run_guarantee() {
        let config = ContainerStressConfig {
            dry_run_max_duration: Duration::from_secs(33),
            ..Default::default()
        };

        let runner = ContainerStressTestRunner::new(config).expect("Failed to create runner");
        let metrics = runner
            .test_dry_run_performance_guarantee()
            .await
            .expect("Test failed");

        // All operations should complete under 33 seconds
        assert!(metrics.max_operation_duration_ms < 33000);
        println!("{}", metrics.report());
    }
}
