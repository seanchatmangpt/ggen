//! Metrics collection and aggregation
//!
//! This module provides comprehensive metrics collection, aggregation,
//! and analysis capabilities for the observability layer.

use super::{Metrics, ResourceUsageMetrics, PerformanceMetrics, ContainerMetrics, TestMetrics};
use crate::error::Result;
use std::collections::HashMap;
use std::time::{Instant, Duration};
use serde::{Deserialize, Serialize};

/// Metrics aggregator for collecting and analyzing metrics over time
pub struct MetricsAggregator {
    /// Collected metrics
    metrics: Vec<Metrics>,
    /// Aggregation window
    window_size: Duration,
    /// Maximum number of metrics to keep
    max_metrics: usize,
}

/// Aggregated metrics over a time window
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedMetrics {
    /// Time window start
    pub window_start: Instant,
    /// Time window end
    pub window_end: Instant,
    /// Number of metrics in window
    pub count: usize,
    /// Aggregated resource usage
    pub resource_usage: AggregatedResourceUsage,
    /// Aggregated performance metrics
    pub performance: AggregatedPerformance,
    /// Aggregated container metrics
    pub containers: AggregatedContainerMetrics,
    /// Aggregated test metrics
    pub tests: AggregatedTestMetrics,
}

/// Aggregated resource usage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedResourceUsage {
    /// Average CPU usage
    pub avg_cpu_usage_percent: f64,
    /// Peak CPU usage
    pub peak_cpu_usage_percent: f64,
    /// Average memory usage
    pub avg_memory_usage_bytes: u64,
    /// Peak memory usage
    pub peak_memory_usage_bytes: u64,
    /// Average disk usage
    pub avg_disk_usage_bytes: u64,
    /// Peak disk usage
    pub peak_disk_usage_bytes: u64,
    /// Total network bytes sent
    pub total_network_bytes_sent: u64,
    /// Total network bytes received
    pub total_network_bytes_received: u64,
}

/// Aggregated performance metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedPerformance {
    /// Average test execution time
    pub avg_test_execution_time: Duration,
    /// Min test execution time
    pub min_test_execution_time: Duration,
    /// Max test execution time
    pub max_test_execution_time: Duration,
    /// Average container startup time
    pub avg_container_startup_time: Duration,
    /// Average throughput
    pub avg_throughput: f64,
    /// Peak throughput
    pub peak_throughput: f64,
}

/// Aggregated container metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedContainerMetrics {
    /// Total containers created
    pub total_created: u32,
    /// Total containers destroyed
    pub total_destroyed: u32,
    /// Average containers running
    pub avg_running: f64,
    /// Peak containers running
    pub peak_running: u32,
    /// Average container lifetime
    pub avg_lifetime: Duration,
}

/// Aggregated test metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AggregatedTestMetrics {
    /// Total tests executed
    pub total_executed: u32,
    /// Total tests passed
    pub total_passed: u32,
    /// Total tests failed
    pub total_failed: u32,
    /// Average success rate
    pub avg_success_rate: f64,
    /// Min success rate
    pub min_success_rate: f64,
    /// Max success rate
    pub max_success_rate: f64,
    /// Average test duration
    pub avg_duration: Duration,
    /// Min test duration
    pub min_duration: Duration,
    /// Max test duration
    pub max_duration: Duration,
}

impl MetricsAggregator {
    /// Create a new metrics aggregator
    pub fn new(window_size: Duration, max_metrics: usize) -> Self {
        Self {
            metrics: Vec::new(),
            window_size,
            max_metrics,
        }
    }

    /// Create an aggregator with default settings
    pub fn default() -> Self {
        Self::new(Duration::from_secs(60), 1000)
    }

    /// Add metrics to the aggregator
    pub fn add_metrics(&mut self, metrics: Metrics) {
        self.metrics.push(metrics);
        
        // Trim old metrics if needed
        if self.metrics.len() > self.max_metrics {
            self.metrics.drain(0..self.metrics.len() - self.max_metrics);
        }
    }

    /// Get aggregated metrics for a time window
    pub fn get_aggregated_metrics(&self, window_start: Instant, window_end: Instant) -> Option<AggregatedMetrics> {
        let window_metrics: Vec<&Metrics> = self.metrics
            .iter()
            .filter(|m| m.timestamp >= window_start && m.timestamp <= window_end)
            .collect();

        if window_metrics.is_empty() {
            return None;
        }

        Some(AggregatedMetrics {
            window_start,
            window_end,
            count: window_metrics.len(),
            resource_usage: Self::aggregate_resource_usage(&window_metrics),
            performance: Self::aggregate_performance(&window_metrics),
            containers: Self::aggregate_containers(&window_metrics),
            tests: Self::aggregate_tests(&window_metrics),
        })
    }

    /// Get aggregated metrics for the last N seconds
    pub fn get_recent_aggregated_metrics(&self, seconds: u64) -> Option<AggregatedMetrics> {
        let now = Instant::now();
        let window_start = now - Duration::from_secs(seconds);
        self.get_aggregated_metrics(window_start, now)
    }

    /// Get aggregated metrics for the default window
    pub fn get_window_aggregated_metrics(&self) -> Option<AggregatedMetrics> {
        if self.metrics.is_empty() {
            return None;
        }

        let now = Instant::now();
        let window_start = now - self.window_size;
        self.get_aggregated_metrics(window_start, now)
    }

    /// Get all metrics
    pub fn get_all_metrics(&self) -> &[Metrics] {
        &self.metrics
    }

    /// Get metrics count
    pub fn metrics_count(&self) -> usize {
        self.metrics.len()
    }

    /// Clear all metrics
    pub fn clear(&mut self) {
        self.metrics.clear();
    }

    /// Get metrics statistics
    pub fn get_statistics(&self) -> AggregatorStatistics {
        AggregatorStatistics {
            total_metrics: self.metrics.len(),
            window_size: self.window_size,
            max_metrics: self.max_metrics,
            oldest_metric: self.metrics.first().map(|m| m.timestamp),
            newest_metric: self.metrics.last().map(|m| m.timestamp),
        }
    }

    /// Aggregate resource usage metrics
    fn aggregate_resource_usage(metrics: &[&Metrics]) -> AggregatedResourceUsage {
        if metrics.is_empty() {
            return AggregatedResourceUsage {
                avg_cpu_usage_percent: 0.0,
                peak_cpu_usage_percent: 0.0,
                avg_memory_usage_bytes: 0,
                peak_memory_usage_bytes: 0,
                avg_disk_usage_bytes: 0,
                peak_disk_usage_bytes: 0,
                total_network_bytes_sent: 0,
                total_network_bytes_received: 0,
            };
        }

        let mut total_cpu = 0.0;
        let mut peak_cpu = 0.0;
        let mut total_memory = 0u64;
        let mut peak_memory = 0u64;
        let mut total_disk = 0u64;
        let mut peak_disk = 0u64;
        let mut total_network_sent = 0u64;
        let mut total_network_received = 0u64;

        for metric in metrics {
            let usage = &metric.resource_usage;
            total_cpu += usage.cpu_usage_percent;
            peak_cpu = peak_cpu.max(usage.cpu_usage_percent);
            total_memory += usage.memory_usage_bytes;
            peak_memory = peak_memory.max(usage.memory_usage_bytes);
            total_disk += usage.disk_usage_bytes;
            peak_disk = peak_disk.max(usage.disk_usage_bytes);
            total_network_sent += usage.network_bytes_sent;
            total_network_received += usage.network_bytes_received;
        }

        let count = metrics.len() as f64;
        AggregatedResourceUsage {
            avg_cpu_usage_percent: total_cpu / count,
            peak_cpu_usage_percent: peak_cpu,
            avg_memory_usage_bytes: (total_memory as f64 / count) as u64,
            peak_memory_usage_bytes: peak_memory,
            avg_disk_usage_bytes: (total_disk as f64 / count) as u64,
            peak_disk_usage_bytes: peak_disk,
            total_network_bytes_sent: total_network_sent,
            total_network_bytes_received: total_network_received,
        }
    }

    /// Aggregate performance metrics
    fn aggregate_performance(metrics: &[&Metrics]) -> AggregatedPerformance {
        if metrics.is_empty() {
            return AggregatedPerformance {
                avg_test_execution_time: Duration::from_secs(0),
                min_test_execution_time: Duration::from_secs(0),
                max_test_execution_time: Duration::from_secs(0),
                avg_container_startup_time: Duration::from_secs(0),
                avg_throughput: 0.0,
                peak_throughput: 0.0,
            };
        }

        let mut total_test_time = Duration::from_secs(0);
        let mut min_test_time = Duration::from_secs(u64::MAX);
        let mut max_test_time = Duration::from_secs(0);
        let mut total_startup_time = Duration::from_secs(0);
        let mut total_throughput = 0.0;
        let mut peak_throughput = 0.0;

        for metric in metrics {
            let perf = &metric.performance;
            total_test_time += perf.avg_test_execution_time;
            min_test_time = min_test_time.min(perf.avg_test_execution_time);
            max_test_time = max_test_time.max(perf.avg_test_execution_time);
            total_startup_time += perf.container_startup_time;
            total_throughput += perf.throughput;
            peak_throughput = peak_throughput.max(perf.throughput);
        }

        let count = metrics.len() as f64;
        AggregatedPerformance {
            avg_test_execution_time: Duration::from_nanos(
                (total_test_time.as_nanos() as f64 / count) as u128
            ),
            min_test_execution_time: min_test_time,
            max_test_execution_time: max_test_time,
            avg_container_startup_time: Duration::from_nanos(
                (total_startup_time.as_nanos() as f64 / count) as u128
            ),
            avg_throughput: total_throughput / count,
            peak_throughput,
        }
    }

    /// Aggregate container metrics
    fn aggregate_containers(metrics: &[&Metrics]) -> AggregatedContainerMetrics {
        if metrics.is_empty() {
            return AggregatedContainerMetrics {
                total_created: 0,
                total_destroyed: 0,
                avg_running: 0.0,
                peak_running: 0,
                avg_lifetime: Duration::from_secs(0),
            };
        }

        let mut total_created = 0u32;
        let mut total_destroyed = 0u32;
        let mut total_running = 0u32;
        let mut peak_running = 0u32;
        let mut total_lifetime = Duration::from_secs(0);

        for metric in metrics {
            let containers = &metric.containers;
            total_created += containers.total_created;
            total_destroyed += containers.total_destroyed;
            total_running += containers.currently_running;
            peak_running = peak_running.max(containers.currently_running);
            total_lifetime += containers.avg_lifetime;
        }

        let count = metrics.len() as f64;
        AggregatedContainerMetrics {
            total_created,
            total_destroyed,
            avg_running: total_running as f64 / count,
            peak_running,
            avg_lifetime: Duration::from_nanos(
                (total_lifetime.as_nanos() as f64 / count) as u128
            ),
        }
    }

    /// Aggregate test metrics
    fn aggregate_tests(metrics: &[&Metrics]) -> AggregatedTestMetrics {
        if metrics.is_empty() {
            return AggregatedTestMetrics {
                total_executed: 0,
                total_passed: 0,
                total_failed: 0,
                avg_success_rate: 0.0,
                min_success_rate: 0.0,
                max_success_rate: 0.0,
                avg_duration: Duration::from_secs(0),
                min_duration: Duration::from_secs(0),
                max_duration: Duration::from_secs(0),
            };
        }

        let mut total_executed = 0u32;
        let mut total_passed = 0u32;
        let mut total_failed = 0u32;
        let mut total_success_rate = 0.0;
        let mut min_success_rate = 1.0;
        let mut max_success_rate = 0.0;
        let mut total_duration = Duration::from_secs(0);
        let mut min_duration = Duration::from_secs(u64::MAX);
        let mut max_duration = Duration::from_secs(0);

        for metric in metrics {
            let tests = &metric.tests;
            total_executed += tests.total_executed;
            total_passed += tests.passed;
            total_failed += tests.failed;
            total_success_rate += tests.success_rate;
            min_success_rate = min_success_rate.min(tests.success_rate);
            max_success_rate = max_success_rate.max(tests.success_rate);
            total_duration += tests.avg_duration;
            min_duration = min_duration.min(tests.avg_duration);
            max_duration = max_duration.max(tests.avg_duration);
        }

        let count = metrics.len() as f64;
        AggregatedTestMetrics {
            total_executed,
            total_passed,
            total_failed,
            avg_success_rate: total_success_rate / count,
            min_success_rate,
            max_success_rate,
            avg_duration: Duration::from_nanos(
                (total_duration.as_nanos() as f64 / count) as u128
            ),
            min_duration,
            max_duration,
        }
    }
}

/// Aggregator statistics
#[derive(Debug, Clone)]
pub struct AggregatorStatistics {
    /// Total number of metrics
    pub total_metrics: usize,
    /// Window size
    pub window_size: Duration,
    /// Maximum number of metrics
    pub max_metrics: usize,
    /// Oldest metric timestamp
    pub oldest_metric: Option<Instant>,
    /// Newest metric timestamp
    pub newest_metric: Option<Instant>,
}

/// Metrics analyzer for trend analysis and anomaly detection
pub struct MetricsAnalyzer {
    /// Metrics aggregator
    aggregator: MetricsAggregator,
    /// Trend analysis window
    trend_window: Duration,
}

/// Trend analysis result
#[derive(Debug, Clone)]
pub struct TrendAnalysis {
    /// CPU usage trend
    pub cpu_trend: Trend,
    /// Memory usage trend
    pub memory_trend: Trend,
    /// Throughput trend
    pub throughput_trend: Trend,
    /// Success rate trend
    pub success_rate_trend: Trend,
}

/// Trend direction
#[derive(Debug, Clone, PartialEq)]
pub enum Trend {
    /// Increasing trend
    Increasing,
    /// Decreasing trend
    Decreasing,
    /// Stable trend
    Stable,
    /// Unknown trend
    Unknown,
}

/// Anomaly detection result
#[derive(Debug, Clone)]
pub struct AnomalyDetection {
    /// Detected anomalies
    pub anomalies: Vec<Anomaly>,
    /// Analysis timestamp
    pub timestamp: Instant,
}

/// Anomaly type
#[derive(Debug, Clone)]
pub struct Anomaly {
    /// Anomaly type
    pub anomaly_type: AnomalyType,
    /// Severity
    pub severity: AnomalySeverity,
    /// Description
    pub description: String,
    /// Timestamp
    pub timestamp: Instant,
    /// Value
    pub value: f64,
    /// Expected range
    pub expected_range: (f64, f64),
}

/// Anomaly types
#[derive(Debug, Clone, PartialEq)]
pub enum AnomalyType {
    /// High CPU usage
    HighCpuUsage,
    /// High memory usage
    HighMemoryUsage,
    /// Low throughput
    LowThroughput,
    /// Low success rate
    LowSuccessRate,
    /// High test duration
    HighTestDuration,
}

/// Anomaly severity
#[derive(Debug, Clone, PartialEq)]
pub enum AnomalySeverity {
    /// Low severity
    Low,
    /// Medium severity
    Medium,
    /// High severity
    High,
    /// Critical severity
    Critical,
}

impl MetricsAnalyzer {
    /// Create a new metrics analyzer
    pub fn new(window_size: Duration, max_metrics: usize, trend_window: Duration) -> Self {
        Self {
            aggregator: MetricsAggregator::new(window_size, max_metrics),
            trend_window,
        }
    }

    /// Create an analyzer with default settings
    pub fn default() -> Self {
        Self::new(
            Duration::from_secs(300), // 5 minutes
            1000,
            Duration::from_secs(60), // 1 minute
        )
    }

    /// Add metrics to the analyzer
    pub fn add_metrics(&mut self, metrics: Metrics) {
        self.aggregator.add_metrics(metrics);
    }

    /// Analyze trends
    pub fn analyze_trends(&self) -> Option<TrendAnalysis> {
        let now = Instant::now();
        let window_start = now - self.trend_window;
        
        let aggregated = self.aggregator.get_aggregated_metrics(window_start, now)?;
        
        Some(TrendAnalysis {
            cpu_trend: self.analyze_cpu_trend(&aggregated),
            memory_trend: self.analyze_memory_trend(&aggregated),
            throughput_trend: self.analyze_throughput_trend(&aggregated),
            success_rate_trend: self.analyze_success_rate_trend(&aggregated),
        })
    }

    /// Detect anomalies
    pub fn detect_anomalies(&self) -> AnomalyDetection {
        let mut anomalies = Vec::new();
        let now = Instant::now();
        
        if let Some(aggregated) = self.aggregator.get_window_aggregated_metrics() {
            // Check for high CPU usage
            if aggregated.resource_usage.avg_cpu_usage_percent > 80.0 {
                anomalies.push(Anomaly {
                    anomaly_type: AnomalyType::HighCpuUsage,
                    severity: AnomalySeverity::High,
                    description: "High CPU usage detected".to_string(),
                    timestamp: now,
                    value: aggregated.resource_usage.avg_cpu_usage_percent,
                    expected_range: (0.0, 80.0),
                });
            }
            
            // Check for high memory usage
            if aggregated.resource_usage.avg_memory_usage_bytes > 1024 * 1024 * 1024 {
                anomalies.push(Anomaly {
                    anomaly_type: AnomalyType::HighMemoryUsage,
                    severity: AnomalySeverity::High,
                    description: "High memory usage detected".to_string(),
                    timestamp: now,
                    value: aggregated.resource_usage.avg_memory_usage_bytes as f64,
                    expected_range: (0.0, 1024.0 * 1024.0 * 1024.0),
                });
            }
            
            // Check for low throughput
            if aggregated.performance.avg_throughput < 1.0 {
                anomalies.push(Anomaly {
                    anomaly_type: AnomalyType::LowThroughput,
                    severity: AnomalySeverity::Medium,
                    description: "Low throughput detected".to_string(),
                    timestamp: now,
                    value: aggregated.performance.avg_throughput,
                    expected_range: (1.0, f64::INFINITY),
                });
            }
            
            // Check for low success rate
            if aggregated.tests.avg_success_rate < 0.95 {
                anomalies.push(Anomaly {
                    anomaly_type: AnomalyType::LowSuccessRate,
                    severity: AnomalySeverity::High,
                    description: "Low success rate detected".to_string(),
                    timestamp: now,
                    value: aggregated.tests.avg_success_rate,
                    expected_range: (0.95, 1.0),
                });
            }
        }
        
        AnomalyDetection {
            anomalies,
            timestamp: now,
        }
    }

    /// Analyze CPU trend
    fn analyze_cpu_trend(&self, _aggregated: &AggregatedMetrics) -> Trend {
        // Simplified trend analysis - in real implementation, this would
        // analyze historical data to determine trend direction
        Trend::Stable
    }

    /// Analyze memory trend
    fn analyze_memory_trend(&self, _aggregated: &AggregatedMetrics) -> Trend {
        Trend::Stable
    }

    /// Analyze throughput trend
    fn analyze_throughput_trend(&self, _aggregated: &AggregatedMetrics) -> Trend {
        Trend::Stable
    }

    /// Analyze success rate trend
    fn analyze_success_rate_trend(&self, _aggregated: &AggregatedMetrics) -> Trend {
        Trend::Stable
    }

    /// Get aggregator statistics
    pub fn get_statistics(&self) -> AggregatorStatistics {
        self.aggregator.get_statistics()
    }
}

/// Convenience function to create a metrics aggregator
pub fn metrics_aggregator(window_size: Duration, max_metrics: usize) -> MetricsAggregator {
    MetricsAggregator::new(window_size, max_metrics)
}

/// Convenience function to create a metrics analyzer
pub fn metrics_analyzer(window_size: Duration, max_metrics: usize, trend_window: Duration) -> MetricsAnalyzer {
    MetricsAnalyzer::new(window_size, max_metrics, trend_window)
}

#[cfg(test)]
mod tests {
    use super::*;
    use uuid::Uuid;

    #[test]
    fn test_metrics_aggregator() {
        let mut aggregator = MetricsAggregator::new(Duration::from_secs(60), 100);
        
        let metrics = Metrics {
            timestamp: Instant::now(),
            session_id: Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 25.0,
                memory_usage_bytes: 1024 * 1024,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: Duration::from_millis(100),
                container_startup_time: Duration::from_millis(50),
                total_execution_time: Duration::from_secs(10),
                throughput: 10.0,
            },
            containers: ContainerMetrics {
                total_created: 5,
                total_destroyed: 3,
                currently_running: 2,
                avg_lifetime: Duration::from_secs(60),
            },
            tests: TestMetrics {
                total_executed: 100,
                passed: 95,
                failed: 5,
                success_rate: 0.95,
                avg_duration: Duration::from_millis(100),
            },
        };
        
        aggregator.add_metrics(metrics);
        assert_eq!(aggregator.metrics_count(), 1);
        
        let stats = aggregator.get_statistics();
        assert_eq!(stats.total_metrics, 1);
    }

    #[test]
    fn test_aggregated_metrics() {
        let mut aggregator = MetricsAggregator::new(Duration::from_secs(60), 100);
        
        let now = Instant::now();
        let metrics1 = Metrics {
            timestamp: now,
            session_id: Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 25.0,
                memory_usage_bytes: 1024 * 1024,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: Duration::from_millis(100),
                container_startup_time: Duration::from_millis(50),
                total_execution_time: Duration::from_secs(10),
                throughput: 10.0,
            },
            containers: ContainerMetrics {
                total_created: 5,
                total_destroyed: 3,
                currently_running: 2,
                avg_lifetime: Duration::from_secs(60),
            },
            tests: TestMetrics {
                total_executed: 100,
                passed: 95,
                failed: 5,
                success_rate: 0.95,
                avg_duration: Duration::from_millis(100),
            },
        };
        
        let metrics2 = Metrics {
            timestamp: now + Duration::from_secs(30),
            session_id: Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 35.0,
                memory_usage_bytes: 2 * 1024 * 1024,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: Duration::from_millis(120),
                container_startup_time: Duration::from_millis(60),
                total_execution_time: Duration::from_secs(12),
                throughput: 12.0,
            },
            containers: ContainerMetrics {
                total_created: 7,
                total_destroyed: 4,
                currently_running: 3,
                avg_lifetime: Duration::from_secs(70),
            },
            tests: TestMetrics {
                total_executed: 120,
                passed: 110,
                failed: 10,
                success_rate: 0.92,
                avg_duration: Duration::from_millis(120),
            },
        };
        
        aggregator.add_metrics(metrics1);
        aggregator.add_metrics(metrics2);
        
        let aggregated = aggregator.get_aggregated_metrics(now, now + Duration::from_secs(60)).unwrap();
        assert_eq!(aggregated.count, 2);
        assert_eq!(aggregated.resource_usage.avg_cpu_usage_percent, 30.0);
        assert_eq!(aggregated.resource_usage.peak_cpu_usage_percent, 35.0);
    }

    #[test]
    fn test_metrics_analyzer() {
        let mut analyzer = MetricsAnalyzer::default();
        
        let metrics = Metrics {
            timestamp: Instant::now(),
            session_id: Uuid::new_v4(),
            resource_usage: ResourceUsageMetrics {
                cpu_usage_percent: 90.0, // High CPU usage
                memory_usage_bytes: 1024 * 1024,
                disk_usage_bytes: 0,
                network_bytes_sent: 0,
                network_bytes_received: 0,
            },
            performance: PerformanceMetrics {
                avg_test_execution_time: Duration::from_millis(100),
                container_startup_time: Duration::from_millis(50),
                total_execution_time: Duration::from_secs(10),
                throughput: 0.5, // Low throughput
            },
            containers: ContainerMetrics {
                total_created: 5,
                total_destroyed: 3,
                currently_running: 2,
                avg_lifetime: Duration::from_secs(60),
            },
            tests: TestMetrics {
                total_executed: 100,
                passed: 80, // Low success rate
                failed: 20,
                success_rate: 0.80,
                avg_duration: Duration::from_millis(100),
            },
        };
        
        analyzer.add_metrics(metrics);
        
        let anomalies = analyzer.detect_anomalies();
        assert!(!anomalies.anomalies.is_empty());
        
        // Check for high CPU usage anomaly
        let cpu_anomaly = anomalies.anomalies.iter()
            .find(|a| a.anomaly_type == AnomalyType::HighCpuUsage);
        assert!(cpu_anomaly.is_some());
        
        // Check for low throughput anomaly
        let throughput_anomaly = anomalies.anomalies.iter()
            .find(|a| a.anomaly_type == AnomalyType::LowThroughput);
        assert!(throughput_anomaly.is_some());
        
        // Check for low success rate anomaly
        let success_rate_anomaly = anomalies.anomalies.iter()
            .find(|a| a.anomaly_type == AnomalyType::LowSuccessRate);
        assert!(success_rate_anomaly.is_some());
    }

    #[test]
    fn test_convenience_functions() {
        let _aggregator = metrics_aggregator(Duration::from_secs(60), 100);
        let _analyzer = metrics_analyzer(Duration::from_secs(60), 100, Duration::from_secs(30));
        
        // Just verify they compile and create valid instances
        assert_eq!(_aggregator.metrics_count(), 0);
        assert_eq!(_analyzer.get_statistics().total_metrics, 0);
    }
}
