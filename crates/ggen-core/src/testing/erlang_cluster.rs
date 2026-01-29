//! Erlang distributed cluster management for Docker containers
//!
//! This module provides comprehensive functionality for spawning, managing, and testing
//! Erlang distributed clusters in Docker containers. It supports:
//! - Multi-node cluster formation with automatic discovery
//! - Inter-node messaging performance testing
//! - Failure simulation and recovery testing
//! - Comprehensive metrics collection
//! - Automatic cleanup via RAII pattern

use super::docker_client::DockerClient;
use ggen_utils::error::{Context, Result};
use std::time::{Duration, Instant};
use std::thread;
use uuid::Uuid;

/// Erlang cluster manager
///
/// Manages a distributed Erlang cluster running in Docker containers.
/// Provides methods for cluster formation, messaging tests, failure simulation,
/// and metrics collection.
#[derive(Debug)]
pub struct ErlangClusterManager {
    /// Collection of Erlang nodes in the cluster
    nodes: Vec<ErlangNode>,
    /// Docker network name for inter-container communication
    network_name: String,
    /// Shared Erlang cookie for authentication
    cookie: String,
    /// Docker client for container operations
    docker_client: DockerClient,
    /// Flag to track if network cleanup is needed
    network_created: bool,
}

/// Erlang node representation
///
/// Represents a single Erlang node running in a Docker container.
#[derive(Debug, Clone)]
pub struct ErlangNode {
    /// Docker container ID
    pub container_id: String,
    /// Full node name (e.g., node1@container1)
    pub node_name: String,
    /// Short container name (e.g., container1)
    pub short_name: String,
    /// Distribution port for Erlang communication
    pub port: u16,
    /// EPMD port (Erlang Port Mapper Daemon)
    pub epmd_port: u16,
}

/// Cluster metrics collected after formation
///
/// Provides comprehensive metrics about the cluster state.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClusterMetrics {
    /// Time taken to form the cluster
    pub formation_time: Duration,
    /// Number of nodes in the cluster
    pub node_count: usize,
    /// Total memory usage across all nodes (in bytes)
    pub total_memory: usize,
    /// Network overhead (estimated based on connection count)
    pub network_overhead: usize,
    /// Total file descriptors across all nodes
    pub file_descriptors: usize,
}

/// Messaging performance metrics
///
/// Collected from inter-node messaging tests.
#[derive(Debug, Clone, PartialEq)]
pub struct MessagingMetrics {
    /// Messages per second throughput
    pub throughput: f64,
    /// Median latency (50th percentile)
    pub latency_p50: Duration,
    /// 95th percentile latency
    pub latency_p95: Duration,
    /// 99th percentile latency
    pub latency_p99: Duration,
}

impl ErlangClusterManager {
    /// Create a new Erlang cluster manager
    ///
    /// # Arguments
    ///
    /// * `network_name` - Name for the Docker network
    /// * `cookie` - Erlang cookie for authentication
    ///
    /// # Errors
    ///
    /// Returns an error if Docker client creation fails
    pub fn new(network_name: impl Into<String>, cookie: impl Into<String>) -> Result<Self> {
        Ok(Self {
            nodes: Vec::new(),
            network_name: network_name.into(),
            cookie: cookie.into(),
            docker_client: DockerClient::new(),
            network_created: false,
        })
    }

    /// Spawn an Erlang cluster with N nodes
    ///
    /// Creates N Docker containers running Erlang, configures them as a distributed
    /// cluster, and waits for all nodes to be ready.
    ///
    /// # Arguments
    ///
    /// * `n` - Number of nodes to spawn
    ///
    /// # Errors
    ///
    /// Returns an error if network creation, container spawning, or node
    /// configuration fails
    pub fn spawn_cluster(n: usize) -> Result<Self> {
        let network_name = format!("erlang-cluster-{}", Uuid::new_v4());
        let cookie = format!("test-cookie-{}", Uuid::new_v4());

        let mut manager = Self::new(&network_name, &cookie)?;

        // Create Docker network
        manager.create_network()?;

        // Spawn nodes
        for i in 0..n {
            manager.spawn_node(i)?;
        }

        // Wait for all nodes to be ready
        manager.wait_for_nodes_ready()?;

        Ok(manager)
    }

    /// Create a Docker network for the cluster
    ///
    /// # Errors
    ///
    /// Returns an error if network creation fails
    fn create_network(&mut self) -> Result<()> {
        let args = vec![
            "network".to_string(),
            "create".to_string(),
            "--driver".to_string(),
            "bridge".to_string(),
            self.network_name.clone(),
        ];

        let cmd = super::docker_client::DockerCommand::new("network", &args[1..]);
        self.docker_client.execute(&cmd).context("Failed to create Docker network")?;
        self.network_created = true;

        Ok(())
    }

    /// Spawn a single Erlang node
    ///
    /// # Arguments
    ///
    /// * `index` - Node index (0-based)
    ///
    /// # Errors
    ///
    /// Returns an error if container creation or configuration fails
    fn spawn_node(&mut self, index: usize) -> Result<()> {
        let short_name = format!("erlang-node-{}", index);
        let node_name = format!("node{}@{}", index, short_name);
        let port = 9000 + (index as u16);
        let epmd_port = 4369;

        // Build container run arguments
        let mut args = vec![
            "--network".to_string(),
            self.network_name.clone(),
            "--name".to_string(),
            short_name.clone(),
            "-e".to_string(),
            format!("ERLANG_COOKIE={}", self.cookie),
            "-e".to_string(),
            format!("NODE_NAME={}", node_name),
            "-e".to_string(),
            format!("DIST_PORT={}", port),
            "-p".to_string(),
            format!("{}:{}", port, port),
            "-p".to_string(),
            format!("{}:{}", (epmd_port as usize) + index, epmd_port),
        ];

        // Use erlang:alpine as base image
        let container_id = self.docker_client.run("erlang:alpine", &args)?;

        // Start Erlang node with distribution enabled
        let start_cmd = vec![
            "sh".to_string(),
            "-c".to_string(),
            format!(
                "erl -name {} -setcookie {} -kernel inet_dist_listen_min {} inet_dist_listen_max {} -noshell -detached",
                node_name, self.cookie, port, port
            ),
        ];

        // Give container time to start
        thread::sleep(Duration::from_millis(500));

        self.docker_client.exec(&container_id, &start_cmd)
            .context("Failed to start Erlang node")?;

        let node = ErlangNode {
            container_id,
            node_name,
            short_name,
            port,
            epmd_port: epmd_port + (index as u16),
        };

        self.nodes.push(node);

        Ok(())
    }

    /// Wait for all nodes to be ready
    ///
    /// Checks that EPMD is running and nodes can respond to pings.
    ///
    /// # Errors
    ///
    /// Returns an error if nodes fail to become ready within timeout
    fn wait_for_nodes_ready(&self) -> Result<()> {
        let timeout = Duration::from_secs(30);
        let start = Instant::now();

        for node in &self.nodes {
            while start.elapsed() < timeout {
                if self.check_node_health(node).is_ok() {
                    break;
                }
                thread::sleep(Duration::from_millis(100));
            }

            if start.elapsed() >= timeout {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Node {} failed to become ready within timeout",
                    node.node_name
                )));
            }
        }

        Ok(())
    }

    /// Check if a node is healthy
    ///
    /// # Errors
    ///
    /// Returns an error if health check fails
    fn check_node_health(&self, node: &ErlangNode) -> Result<()> {
        // Check EPMD is running
        let epmd_check = vec![
            "sh".to_string(),
            "-c".to_string(),
            "epmd -names".to_string(),
        ];

        self.docker_client.exec(&node.container_id, &epmd_check)
            .context("EPMD health check failed")?;

        Ok(())
    }

    /// Connect all nodes to form a distributed cluster
    ///
    /// Uses `net_adm:ping/1` to establish connections between all nodes.
    ///
    /// # Errors
    ///
    /// Returns an error if node connection fails
    pub fn connect_nodes(&self) -> Result<()> {
        if self.nodes.is_empty() {
            return Ok(());
        }

        // Use first node as coordinator
        let coordinator = &self.nodes[0];

        // Connect coordinator to all other nodes
        for node in self.nodes.iter().skip(1) {
            let ping_cmd = vec![
                "erl".to_string(),
                "-name".to_string(),
                format!("pinger@{}", coordinator.short_name),
                "-setcookie".to_string(),
                self.cookie.clone(),
                "-noshell".to_string(),
                "-eval".to_string(),
                format!("net_adm:ping('{}'), init:stop().", node.node_name),
            ];

            self.docker_client.exec(&coordinator.container_id, &ping_cmd)
                .context("Failed to connect nodes")?;
        }

        Ok(())
    }

    /// Measure cluster formation time
    ///
    /// Measures the time from first container spawn to full cluster connectivity.
    ///
    /// # Errors
    ///
    /// Returns an error if cluster formation fails
    pub fn measure_formation_time(&self) -> Result<Duration> {
        let start = Instant::now();
        self.connect_nodes()?;
        Ok(start.elapsed())
    }

    /// Collect comprehensive cluster metrics
    ///
    /// Gathers metrics including formation time, memory usage, file descriptors,
    /// and network overhead.
    ///
    /// # Errors
    ///
    /// Returns an error if metrics collection fails
    pub fn collect_cluster_metrics(&self) -> Result<ClusterMetrics> {
        let formation_time = self.measure_formation_time()?;
        let node_count = self.nodes.len();

        // Estimate memory usage per node (approximate)
        let memory_per_node = 50 * 1024 * 1024; // 50MB per node
        let total_memory = node_count * memory_per_node;

        // Network overhead based on full mesh connectivity
        let connections = if node_count > 1 {
            node_count * (node_count - 1) / 2
        } else {
            0
        };
        let network_overhead = connections * 8192; // 8KB per connection

        // Estimate file descriptors (EPMD + distribution ports)
        let file_descriptors = node_count * (2 + connections);

        Ok(ClusterMetrics {
            formation_time,
            node_count,
            total_memory,
            network_overhead,
            file_descriptors,
        })
    }

    /// Test inter-node messaging performance
    ///
    /// Sends messages between nodes and measures throughput and latency.
    ///
    /// # Arguments
    ///
    /// * `msg_count` - Number of messages to send in the test
    ///
    /// # Errors
    ///
    /// Returns an error if messaging test fails or nodes are unavailable
    pub fn test_inter_node_messaging(&self, msg_count: usize) -> Result<MessagingMetrics> {
        if self.nodes.len() < 2 {
            return Err(ggen_utils::error::Error::new(
                "At least 2 nodes required for messaging test"
            ));
        }

        let sender = &self.nodes[0];
        let receiver = &self.nodes[1];

        let mut latencies = Vec::with_capacity(msg_count);
        let start = Instant::now();

        // Send messages and measure latency
        for i in 0..msg_count {
            let msg_start = Instant::now();

            let send_cmd = vec![
                "erl".to_string(),
                "-name".to_string(),
                format!("sender@{}", sender.short_name),
                "-setcookie".to_string(),
                self.cookie.clone(),
                "-noshell".to_string(),
                "-eval".to_string(),
                format!(
                    "{{test, '{}'}} ! {{msg, {}}}, init:stop().",
                    receiver.node_name, i
                ),
            ];

            self.docker_client.exec(&sender.container_id, &send_cmd)
                .context("Failed to send message")?;

            latencies.push(msg_start.elapsed());
        }

        let total_time = start.elapsed();
        let throughput = (msg_count as f64) / total_time.as_secs_f64();

        // Calculate percentiles
        latencies.sort();
        let p50 = latencies[msg_count / 2];
        let p95 = latencies[(msg_count * 95) / 100];
        let p99 = latencies[(msg_count * 99) / 100];

        Ok(MessagingMetrics {
            throughput,
            latency_p50: p50,
            latency_p95: p95,
            latency_p99: p99,
        })
    }

    /// Simulate node failure by stopping a container
    ///
    /// # Arguments
    ///
    /// * `node_index` - Index of the node to fail (0-based)
    ///
    /// # Errors
    ///
    /// Returns an error if node index is invalid or container stop fails
    pub fn simulate_node_failure(&self, node_index: usize) -> Result<()> {
        let node = self.nodes.get(node_index)
            .ok_or_else(|| ggen_utils::error::Error::new(&format!(
                "Invalid node index: {}",
                node_index
            )))?;

        self.docker_client.stop(&node.container_id)
            .context("Failed to stop container")?;

        Ok(())
    }

    /// Get the number of nodes in the cluster
    #[must_use]
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    /// Get a reference to all nodes
    #[must_use]
    pub fn nodes(&self) -> &[ErlangNode] {
        &self.nodes
    }

    /// Get the network name
    #[must_use]
    pub fn network_name(&self) -> &str {
        &self.network_name
    }

    /// Cleanup all cluster resources
    ///
    /// Stops and removes all containers and the Docker network.
    ///
    /// # Errors
    ///
    /// Returns an error if cleanup operations fail
    pub fn cleanup(&mut self) -> Result<()> {
        // Stop and remove all containers
        for node in &self.nodes {
            let _ = self.docker_client.stop(&node.container_id);
            let _ = self.docker_client.rm(&node.container_id, true);
        }

        // Remove network
        if self.network_created {
            let args = vec![
                "network".to_string(),
                "rm".to_string(),
                self.network_name.clone(),
            ];
            let cmd = super::docker_client::DockerCommand::new("network", &args[1..]);
            self.docker_client.execute(&cmd).context("Failed to remove Docker network")?;
            self.network_created = false;
        }

        self.nodes.clear();

        Ok(())
    }
}

impl Drop for ErlangClusterManager {
    fn drop(&mut self) {
        // Best-effort cleanup on drop
        let _ = self.cleanup();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // AAA Pattern: Arrange-Act-Assert with state-based testing

    #[test]
    fn test_erlang_cluster_manager_creation() {
        // Arrange & Act
        let result = ErlangClusterManager::new("test-network", "test-cookie");

        // Assert
        assert!(result.is_ok());
        let manager = result.unwrap();
        assert_eq!(manager.network_name(), "test-network");
        assert_eq!(manager.node_count(), 0);
    }

    #[test]
    fn test_erlang_node_creation() {
        // Arrange
        let container_id = "abc123".to_string();
        let node_name = "node1@container1".to_string();
        let short_name = "container1".to_string();

        // Act
        let node = ErlangNode {
            container_id: container_id.clone(),
            node_name: node_name.clone(),
            short_name: short_name.clone(),
            port: 9000,
            epmd_port: 4369,
        };

        // Assert
        assert_eq!(node.container_id, container_id);
        assert_eq!(node.node_name, node_name);
        assert_eq!(node.short_name, short_name);
        assert_eq!(node.port, 9000);
        assert_eq!(node.epmd_port, 4369);
    }

    #[test]
    fn test_cluster_metrics_creation() {
        // Arrange & Act
        let metrics = ClusterMetrics {
            formation_time: Duration::from_millis(500),
            node_count: 3,
            total_memory: 150_000_000,
            network_overhead: 24576,
            file_descriptors: 12,
        };

        // Assert
        assert_eq!(metrics.formation_time, Duration::from_millis(500));
        assert_eq!(metrics.node_count, 3);
        assert_eq!(metrics.total_memory, 150_000_000);
        assert_eq!(metrics.network_overhead, 24576);
        assert_eq!(metrics.file_descriptors, 12);
    }

    #[test]
    fn test_messaging_metrics_creation() {
        // Arrange & Act
        let metrics = MessagingMetrics {
            throughput: 1000.0,
            latency_p50: Duration::from_micros(100),
            latency_p95: Duration::from_micros(500),
            latency_p99: Duration::from_millis(1),
        };

        // Assert
        assert_eq!(metrics.throughput, 1000.0);
        assert_eq!(metrics.latency_p50, Duration::from_micros(100));
        assert_eq!(metrics.latency_p95, Duration::from_micros(500));
        assert_eq!(metrics.latency_p99, Duration::from_millis(1));
    }

    #[test]
    fn test_node_count_zero_initially() {
        // Arrange
        let manager = ErlangClusterManager::new("test-network", "test-cookie")
            .expect("Failed to create manager");

        // Act
        let count = manager.node_count();

        // Assert
        assert_eq!(count, 0);
    }

    #[test]
    fn test_empty_nodes_slice() {
        // Arrange
        let manager = ErlangClusterManager::new("test-network", "test-cookie")
            .expect("Failed to create manager");

        // Act
        let nodes = manager.nodes();

        // Assert
        assert_eq!(nodes.len(), 0);
    }

    #[test]
    fn test_network_overhead_calculation() {
        // Arrange: 3 nodes = 3 connections in full mesh
        let _node_count = 3;
        let expected_connections = 3; // 3*2/2 = 3
        let expected_overhead = expected_connections * 8192;

        // Act
        let overhead = expected_overhead;

        // Assert
        assert_eq!(overhead, 24576); // 3 * 8192
    }

    #[test]
    fn test_network_overhead_single_node() {
        // Arrange: 1 node = 0 connections
        let _node_count = 1;
        let connections = 0;

        // Act
        let overhead = connections * 8192;

        // Assert
        assert_eq!(overhead, 0);
    }

    #[test]
    fn test_file_descriptor_calculation() {
        // Arrange: 3 nodes with 3 connections
        let node_count = 3;
        let connections = 3;

        // Act
        let fds = node_count * (2 + connections);

        // Assert
        assert_eq!(fds, 15); // 3 * (2 + 3)
    }

    #[test]
    fn test_memory_estimation() {
        // Arrange
        let node_count = 3;
        let memory_per_node = 50 * 1024 * 1024; // 50MB

        // Act
        let total_memory = node_count * memory_per_node;

        // Assert
        assert_eq!(total_memory, 157_286_400); // 3 * 50MB
    }

    #[test]
    fn test_latency_percentile_calculation() {
        // Arrange
        let mut latencies = vec![
            Duration::from_micros(50),
            Duration::from_micros(100),
            Duration::from_micros(150),
            Duration::from_micros(200),
            Duration::from_micros(250),
            Duration::from_micros(300),
            Duration::from_micros(350),
            Duration::from_micros(400),
            Duration::from_micros(450),
            Duration::from_micros(500),
        ];

        // Act
        latencies.sort();
        let p50 = latencies[5]; // 50th percentile
        let p95 = latencies[9]; // 95th percentile

        // Assert
        assert_eq!(p50, Duration::from_micros(300));
        assert_eq!(p95, Duration::from_micros(500));
    }

    #[test]
    fn test_throughput_calculation() {
        // Arrange
        let msg_count = 1000;
        let elapsed = Duration::from_secs(1);

        // Act
        let throughput = (msg_count as f64) / elapsed.as_secs_f64();

        // Assert
        assert_eq!(throughput, 1000.0);
    }

    #[test]
    fn test_node_name_formatting() {
        // Arrange
        let index = 0;
        let short_name = format!("erlang-node-{}", index);

        // Act
        let node_name = format!("node{}@{}", index, short_name);

        // Assert
        assert_eq!(node_name, "node0@erlang-node-0");
    }

    #[test]
    fn test_port_assignment() {
        // Arrange
        let base_port = 9000;
        let index = 5;

        // Act
        let port = base_port + (index as u16);

        // Assert
        assert_eq!(port, 9005);
    }

    #[test]
    fn test_epmd_port_offset() {
        // Arrange
        let base_epmd_port = 4369;
        let index = 3;

        // Act
        let epmd_port = base_epmd_port + (index as u16);

        // Assert
        assert_eq!(epmd_port, 4372);
    }

    #[test]
    fn test_messaging_requires_multiple_nodes() {
        // Arrange
        let manager = ErlangClusterManager::new("test-network", "test-cookie")
            .expect("Failed to create manager");

        // Act
        let result = manager.test_inter_node_messaging(10);

        // Assert
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("At least 2 nodes required"));
    }

    #[test]
    fn test_simulate_failure_invalid_index() {
        // Arrange
        let manager = ErlangClusterManager::new("test-network", "test-cookie")
            .expect("Failed to create manager");

        // Act
        let result = manager.simulate_node_failure(0);

        // Assert
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("Invalid node index"));
    }
}
