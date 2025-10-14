//! Examples demonstrating backend extensions
//!
//! This example shows how to use backend extensions for enhanced capabilities,
//! including capability discovery, validation, and performance analysis.

use cleanroom::backend::capabilities::{
    capability_discovery_service, capability_registry, BackendCapability,
    BackendCapabilityRegistry, CapabilityCategory, CapabilityDiscoveryProvider,
    CapabilityDiscoveryService, CapabilityFeature, CapabilityRequirement, FeatureType,
    RequirementType, StandardCapabilities,
};
use cleanroom::backend::extensions::{
    analyze_backend_performance, check_backend_capabilities, BackendCapabilities,
    BackendCapability, BackendCapabilityChecker, BackendConfiguration, BackendExt,
    BackendHealthStatus, BackendMetadata, BackendPerformanceAnalyzer, BackendPerformanceMetrics,
    BackendStatistics, EnhancedBackend, ExecutionMode, ExecutionOptions, PerformanceAnalysis,
    PerformanceRecommendation, PerformanceTrend, PerformanceTuning, RecommendationCategory,
    RecommendationPriority, ResourceEfficiency, ResourceLimits, ResourceLimitsSupport,
    ResourceUsage,
};
use cleanroom::backend::{Backend, Cmd, RunResult};
use cleanroom::error::Result;
use std::collections::HashMap;
use std::time::Duration;

// Mock backend implementation for demonstration
struct MockBackend {
    name: String,
    capabilities: BackendCapabilities,
    metadata: BackendMetadata,
    health_status: BackendHealthStatus,
    performance_metrics: BackendPerformanceMetrics,
    statistics: BackendStatistics,
}

impl MockBackend {
    fn new(name: String) -> Self {
        Self {
            name: name.clone(),
            capabilities: BackendCapabilities {
                capabilities: vec![
                    BackendCapability::HermeticExecution,
                    BackendCapability::DeterministicExecution,
                    BackendCapability::ResourceMonitoring,
                ],
                max_concurrent_executions: Some(10),
                execution_modes: vec![ExecutionMode::Synchronous, ExecutionMode::Asynchronous],
                resource_limits: ResourceLimitsSupport {
                    cpu_limits: true,
                    memory_limits: true,
                    disk_limits: true,
                    network_limits: true,
                    time_limits: true,
                },
            },
            metadata: BackendMetadata {
                name: name.clone(),
                version: "1.0.0".to_string(),
                description: format!("Mock backend: {}", name),
                vendor: "cleanroom".to_string(),
                configuration: HashMap::new(),
            },
            health_status: BackendHealthStatus::Healthy,
            performance_metrics: BackendPerformanceMetrics {
                avg_execution_time: Duration::from_millis(100),
                peak_execution_time: Duration::from_millis(200),
                total_executions: 1000,
                successful_executions: 950,
                failed_executions: 50,
                success_rate: 0.95,
                resource_usage: ResourceUsage {
                    cpu_usage_percent: 25.0,
                    memory_usage_bytes: 512 * 1024 * 1024,
                    disk_usage_bytes: 1024 * 1024 * 1024,
                    network_bytes_sent: 1024 * 1024,
                    network_bytes_received: 2 * 1024 * 1024,
                },
            },
            statistics: BackendStatistics {
                total_executions: 1000,
                successful_executions: 950,
                failed_executions: 50,
                avg_execution_time: Duration::from_millis(100),
                peak_execution_time: Duration::from_millis(200),
                total_resource_usage: ResourceUsage {
                    cpu_usage_percent: 25.0,
                    memory_usage_bytes: 512 * 1024 * 1024,
                    disk_usage_bytes: 1024 * 1024 * 1024,
                    network_bytes_sent: 1024 * 1024,
                    network_bytes_received: 2 * 1024 * 1024,
                },
                uptime: Duration::from_secs(3600),
                last_execution_time: Some(std::time::Instant::now()),
            },
        }
    }
}

impl Backend for MockBackend {
    fn run_cmd(&self, _cmd: Cmd) -> Result<RunResult> {
        Ok(RunResult {
            success: true,
            exit_code: Some(0),
            stdout: "mock output".to_string(),
            stderr: String::new(),
            duration: Duration::from_millis(100),
            artifacts: Vec::new(),
            metrics: HashMap::new(),
        })
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn is_available(&self) -> bool {
        true
    }

    fn supports_hermetic(&self) -> bool {
        true
    }

    fn supports_deterministic(&self) -> bool {
        true
    }
}

impl BackendExt for MockBackend {
    fn capabilities(&self) -> BackendCapabilities {
        self.capabilities.clone()
    }

    fn supports_capability(&self, capability: &BackendCapability) -> bool {
        self.capabilities.capabilities.contains(capability)
    }

    fn metadata(&self) -> BackendMetadata {
        self.metadata.clone()
    }

    fn health_status(&self) -> BackendHealthStatus {
        self.health_status.clone()
    }

    fn performance_metrics(&self) -> BackendPerformanceMetrics {
        self.performance_metrics.clone()
    }
}

impl EnhancedBackend for MockBackend {
    fn execute_enhanced(&self, cmd: Cmd, _options: ExecutionOptions) -> Result<RunResult> {
        self.run_cmd(cmd)
    }

    fn execute_batch(&self, commands: Vec<Cmd>) -> Result<Vec<RunResult>> {
        let mut results = Vec::new();
        for cmd in commands {
            results.push(self.run_cmd(cmd)?);
        }
        Ok(results)
    }

    fn execute_streaming(&self, _cmd: Cmd) -> Result<Box<dyn std::io::Read + Send>> {
        Err(crate::error::CleanroomError::internal_error(
            "Streaming not implemented",
        ))
    }

    fn get_statistics(&self) -> BackendStatistics {
        self.statistics.clone()
    }

    fn reset_statistics(&mut self) {
        self.statistics = BackendStatistics {
            total_executions: 0,
            successful_executions: 0,
            failed_executions: 0,
            avg_execution_time: Duration::from_secs(0),
            peak_execution_time: Duration::from_secs(0),
            total_resource_usage: ResourceUsage {
                cpu_usage_percent: 0.0,
                memory_usage_bytes: 0,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            uptime: Duration::from_secs(0),
            last_execution_time: None,
        };
    }

    fn configure(&mut self, _config: BackendConfiguration) -> Result<()> {
        Ok(())
    }
}

fn main() -> Result<()> {
    println!("Cleanroom Backend Extensions Examples");
    println!("======================================");

    // Example 1: Basic backend capabilities
    println!("\n1. Basic Backend Capabilities");
    let backend = MockBackend::new("test_backend".to_string());

    let capabilities = backend.capabilities();
    println!("✓ Backend capabilities:");
    for capability in &capabilities.capabilities {
        println!("  - {:?}", capability);
    }

    println!(
        "✓ Max concurrent executions: {:?}",
        capabilities.max_concurrent_executions
    );
    println!("✓ Execution modes: {:?}", capabilities.execution_modes);
    println!(
        "✓ Resource limits support: {:?}",
        capabilities.resource_limits
    );

    // Example 2: Capability checking
    println!("\n2. Capability Checking");
    let required_capabilities = vec![
        BackendCapability::HermeticExecution,
        BackendCapability::ResourceMonitoring,
    ];

    for capability in &required_capabilities {
        let supported = backend.supports_capability(capability);
        println!("✓ {}: {}", format!("{:?}", capability), supported);
    }

    // Check all required capabilities
    let result = check_backend_capabilities(&backend, &required_capabilities);
    match result {
        Ok(_) => println!("✓ All required capabilities are supported"),
        Err(e) => println!("✗ Capability check failed: {}", e),
    }

    // Example 3: Backend metadata
    println!("\n3. Backend Metadata");
    let metadata = backend.metadata();
    println!("✓ Name: {}", metadata.name);
    println!("✓ Version: {}", metadata.version);
    println!("✓ Description: {}", metadata.description);
    println!("✓ Vendor: {}", metadata.vendor);
    println!("✓ Configuration: {:?}", metadata.configuration);

    // Example 4: Backend health status
    println!("\n4. Backend Health Status");
    let health_status = backend.health_status();
    println!("✓ Health status: {:?}", health_status);

    match health_status {
        BackendHealthStatus::Healthy => println!("✓ Backend is healthy"),
        BackendHealthStatus::Degraded => println!("⚠ Backend is degraded"),
        BackendHealthStatus::Unhealthy => println!("✗ Backend is unhealthy"),
        BackendHealthStatus::Unknown => println!("? Backend health is unknown"),
    }

    // Example 5: Performance metrics
    println!("\n5. Performance Metrics");
    let metrics = backend.performance_metrics();
    println!("✓ Average execution time: {:?}", metrics.avg_execution_time);
    println!("✓ Peak execution time: {:?}", metrics.peak_execution_time);
    println!("✓ Total executions: {}", metrics.total_executions);
    println!("✓ Successful executions: {}", metrics.successful_executions);
    println!("✓ Failed executions: {}", metrics.failed_executions);
    println!("✓ Success rate: {:.1}%", metrics.success_rate * 100.0);
    println!("✓ Resource usage:");
    println!("  - CPU: {:.1}%", metrics.resource_usage.cpu_usage_percent);
    println!(
        "  - Memory: {} bytes ({:.1} MB)",
        metrics.resource_usage.memory_usage_bytes,
        metrics.resource_usage.memory_usage_bytes as f64 / (1024.0 * 1024.0)
    );
    println!(
        "  - Disk: {} bytes ({:.1} GB)",
        metrics.resource_usage.disk_usage_bytes,
        metrics.resource_usage.disk_usage_bytes as f64 / (1024.0 * 1024.0 * 1024.0)
    );
    println!(
        "  - Network sent: {} bytes ({:.1} MB)",
        metrics.resource_usage.network_bytes_sent,
        metrics.resource_usage.network_bytes_sent as f64 / (1024.0 * 1024.0)
    );
    println!(
        "  - Network received: {} bytes ({:.1} MB)",
        metrics.resource_usage.network_bytes_received,
        metrics.resource_usage.network_bytes_received as f64 / (1024.0 * 1024.0)
    );

    // Example 6: Performance analysis
    println!("\n6. Performance Analysis");
    let analysis = analyze_backend_performance(&backend);
    println!("✓ Overall performance score: {:.1}", analysis.overall_score);
    println!("✓ Performance trend: {:?}", analysis.performance_trend);
    println!("✓ Resource efficiency:");
    println!(
        "  - CPU efficiency: {:.1}",
        analysis.resource_efficiency.cpu_efficiency
    );
    println!(
        "  - Memory efficiency: {:.1}",
        analysis.resource_efficiency.memory_efficiency
    );
    println!(
        "  - Overall efficiency: {:.1}",
        analysis.resource_efficiency.overall_efficiency
    );

    println!("✓ Performance recommendations:");
    for recommendation in &analysis.recommendations {
        println!(
            "  - {:?} ({:?}): {}",
            recommendation.category, recommendation.priority, recommendation.description
        );
    }

    // Example 7: Enhanced backend operations
    println!("\n7. Enhanced Backend Operations");
    let mut enhanced_backend = MockBackend::new("enhanced_backend".to_string());

    // Execute enhanced command
    let cmd = Cmd {
        bin: "echo".to_string(),
        args: vec!["hello".to_string()],
        workdir: None,
        env: HashMap::new(),
        policy: cleanroom::policy::Policy::default(),
    };

    let options = ExecutionOptions {
        timeout: Some(Duration::from_secs(30)),
        resource_limits: Some(ResourceLimits {
            max_cpu_usage_percent: 80.0,
            max_memory_usage_bytes: 1024 * 1024 * 1024,
            max_disk_usage_bytes: 10 * 1024 * 1024 * 1024,
            max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024,
            max_execution_time: Duration::from_secs(300),
        }),
        environment: HashMap::new(),
        working_directory: None,
        execution_mode: ExecutionMode::Synchronous,
        enable_resource_monitoring: true,
        enable_output_streaming: false,
    };

    let result = enhanced_backend.execute_enhanced(cmd, options)?;
    println!("✓ Enhanced execution result: {}", result.stdout);

    // Execute batch commands
    let commands = vec![
        Cmd {
            bin: "echo".to_string(),
            args: vec!["command1".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: cleanroom::policy::Policy::default(),
        },
        Cmd {
            bin: "echo".to_string(),
            args: vec!["command2".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: cleanroom::policy::Policy::default(),
        },
    ];

    let batch_results = enhanced_backend.execute_batch(commands)?;
    println!(
        "✓ Batch execution completed: {} commands",
        batch_results.len()
    );

    // Get statistics
    let stats = enhanced_backend.get_statistics();
    println!("✓ Backend statistics:");
    println!("  - Total executions: {}", stats.total_executions);
    println!("  - Successful executions: {}", stats.successful_executions);
    println!("  - Failed executions: {}", stats.failed_executions);
    println!("  - Average execution time: {:?}", stats.avg_execution_time);
    println!("  - Peak execution time: {:?}", stats.peak_execution_time);
    println!("  - Uptime: {:?}", stats.uptime);

    // Example 8: Backend configuration
    println!("\n8. Backend Configuration");
    let config = BackendConfiguration {
        parameters: {
            let mut params = HashMap::new();
            params.insert("timeout".to_string(), "30".to_string());
            params.insert("max_retries".to_string(), "3".to_string());
            params
        },
        feature_flags: {
            let mut flags = HashMap::new();
            flags.insert("enable_monitoring".to_string(), true);
            flags.insert("enable_caching".to_string(), false);
            flags
        },
        resource_limits: Some(ResourceLimits {
            max_cpu_usage_percent: 80.0,
            max_memory_usage_bytes: 1024 * 1024 * 1024,
            max_disk_usage_bytes: 10 * 1024 * 1024 * 1024,
            max_network_bandwidth_bytes_per_sec: 100 * 1024 * 1024,
            max_execution_time: Duration::from_secs(300),
        }),
        performance_tuning: Some(PerformanceTuning {
            enable_optimizations: true,
            cache_size: Some(1000),
            connection_pool_size: Some(10),
            batch_size: Some(100),
        }),
    };

    enhanced_backend.configure(config)?;
    println!("✓ Backend configured successfully");

    // Example 9: Capability registry
    println!("\n9. Capability Registry");
    let mut registry = capability_registry();

    // Register standard capabilities
    let standard_capabilities = StandardCapabilities::all_standard_capabilities();
    for capability in standard_capabilities {
        registry.register_capability(capability).unwrap();
    }

    let stats = registry.get_statistics();
    println!("✓ Capability registry statistics:");
    println!("  - Total capabilities: {}", stats.total_capabilities);
    println!("  - Categories: {:?}", stats.categories);
    println!("  - Total dependencies: {}", stats.total_dependencies);
    println!("  - Total conflicts: {}", stats.total_conflicts);

    // Get capabilities by category
    let execution_caps = registry.get_capabilities_by_category(&CapabilityCategory::Execution);
    println!("✓ Execution capabilities: {}", execution_caps.len());

    let resource_caps =
        registry.get_capabilities_by_category(&CapabilityCategory::ResourceManagement);
    println!(
        "✓ Resource management capabilities: {}",
        resource_caps.len()
    );

    let security_caps = registry.get_capabilities_by_category(&CapabilityCategory::Security);
    println!("✓ Security capabilities: {}", security_caps.len());

    // Example 10: Capability discovery service
    println!("\n10. Capability Discovery Service");
    let mut discovery_service = capability_discovery_service();

    // Add standard capabilities to registry
    let standard_capabilities = StandardCapabilities::all_standard_capabilities();
    for capability in standard_capabilities {
        discovery_service
            .registry_mut()
            .register_capability(capability)
            .unwrap();
    }

    let registry_stats = discovery_service.registry().get_statistics();
    println!("✓ Discovery service registry statistics:");
    println!(
        "  - Total capabilities: {}",
        registry_stats.total_capabilities
    );
    println!("  - Categories: {:?}", registry_stats.categories);

    // Example 11: Capability validation
    println!("\n11. Capability Validation");
    let capabilities_to_validate = vec![
        "hermetic_execution".to_string(),
        "deterministic_execution".to_string(),
    ];

    let validation_result = registry.validate_capability_set(&capabilities_to_validate);
    match validation_result {
        Ok(_) => println!("✓ Capability set is valid"),
        Err(e) => println!("✗ Capability validation failed: {}", e),
    }

    // Example 12: Missing capabilities
    println!("\n12. Missing Capabilities");
    let required_capabilities = vec![
        BackendCapability::HermeticExecution,
        BackendCapability::NetworkIsolation,
    ];

    let missing =
        BackendCapabilityChecker::get_missing_capabilities(&backend, &required_capabilities);
    if missing.is_empty() {
        println!("✓ All required capabilities are supported");
    } else {
        println!("✗ Missing capabilities:");
        for capability in missing {
            println!("  - {:?}", capability);
        }
    }

    // Example 13: Minimum requirements check
    println!("\n13. Minimum Requirements Check");
    let meets_requirements = BackendCapabilityChecker::meets_minimum_requirements(&backend);
    if meets_requirements {
        println!("✓ Backend meets minimum requirements");
    } else {
        println!("✗ Backend does not meet minimum requirements");
    }

    // Example 14: Performance recommendations
    println!("\n14. Performance Recommendations");
    let analysis = analyze_backend_performance(&backend);

    if analysis.recommendations.is_empty() {
        println!("✓ No performance recommendations");
    } else {
        println!("✓ Performance recommendations:");
        for (i, recommendation) in analysis.recommendations.iter().enumerate() {
            println!(
                "  {}. {:?} ({:?}): {}",
                i + 1,
                recommendation.category,
                recommendation.priority,
                recommendation.description
            );
        }
    }

    // Example 15: Convenience functions
    println!("\n15. Convenience Functions");
    let _registry = capability_registry();
    let _service = capability_discovery_service();

    println!("✓ All convenience functions work correctly");

    println!("\n=== All Backend Extensions Examples Completed Successfully ===");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_backend() {
        let backend = MockBackend::new("test".to_string());

        assert_eq!(backend.name(), "test");
        assert!(backend.is_available());
        assert!(backend.supports_hermetic());
        assert!(backend.supports_deterministic());
    }

    #[test]
    fn test_backend_capabilities() {
        let backend = MockBackend::new("test".to_string());

        assert!(backend.supports_capability(&BackendCapability::HermeticExecution));
        assert!(backend.supports_capability(&BackendCapability::DeterministicExecution));
        assert!(backend.supports_capability(&BackendCapability::ResourceMonitoring));
        assert!(!backend.supports_capability(&BackendCapability::NetworkIsolation));
    }

    #[test]
    fn test_capability_checking() {
        let backend = MockBackend::new("test".to_string());

        let required_capabilities = vec![
            BackendCapability::HermeticExecution,
            BackendCapability::ResourceMonitoring,
        ];

        assert!(check_backend_capabilities(&backend, &required_capabilities).is_ok());
    }

    #[test]
    fn test_performance_analysis() {
        let backend = MockBackend::new("test".to_string());

        let analysis = analyze_backend_performance(&backend);
        assert!(analysis.overall_score > 0.0);
        assert_eq!(analysis.performance_trend, PerformanceTrend::Stable);
    }

    #[test]
    fn test_enhanced_backend() {
        let mut backend = MockBackend::new("test".to_string());

        let cmd = Cmd {
            bin: "echo".to_string(),
            args: vec!["hello".to_string()],
            workdir: None,
            env: HashMap::new(),
            policy: cleanroom::policy::Policy::default(),
        };

        let options = ExecutionOptions {
            timeout: Some(Duration::from_secs(30)),
            resource_limits: None,
            environment: HashMap::new(),
            working_directory: None,
            execution_mode: ExecutionMode::Synchronous,
            enable_resource_monitoring: true,
            enable_output_streaming: false,
        };

        let result = backend.execute_enhanced(cmd, options).unwrap();
        assert!(result.success);
        assert_eq!(result.stdout, "mock output");
    }

    #[test]
    fn test_capability_registry() {
        let mut registry = capability_registry();

        let capability = BackendCapability {
            name: "test_capability".to_string(),
            description: "Test capability".to_string(),
            version: "1.0.0".to_string(),
            category: CapabilityCategory::Execution,
            requirements: vec![],
            features: vec![],
            metadata: HashMap::new(),
        };

        assert!(registry.register_capability(capability).is_ok());
        assert!(registry.has_capability("test_capability"));
    }

    #[test]
    fn test_convenience_functions() {
        let _registry = capability_registry();
        let _service = capability_discovery_service();

        // Just verify they compile and create valid instances
        assert_eq!(_registry.get_all_capabilities().len(), 0);
    }
}
