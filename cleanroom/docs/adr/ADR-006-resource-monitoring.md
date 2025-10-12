# ADR-006: Resource Monitoring Architecture

## Status
Accepted

## Context

Resource monitoring in testing frameworks is critical for:
- **Performance Optimization**: Identify performance bottlenecks
- **Resource Management**: Prevent resource exhaustion
- **Cost Control**: Optimize resource usage and costs
- **SLA Compliance**: Meet performance service level objectives
- **Debugging**: Understand resource usage patterns

Resource monitoring challenges:
- **Real-time Monitoring**: Track resources in real-time
- **Multi-dimensional Metrics**: CPU, memory, disk, network
- **Container-level Monitoring**: Monitor individual containers
- **Aggregate Metrics**: System-wide resource usage
- **Alerting**: Proactive alerts for resource issues

We need a monitoring architecture that:
1. Tracks all resource types comprehensively
2. Provides real-time monitoring capabilities
3. Supports alerting and notifications
4. Offers detailed analytics and reporting
5. Integrates with external monitoring systems

## Decision

Implement a **comprehensive resource monitoring architecture** with the following design:

### Core Monitoring Framework

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceMonitor {
    pub cpu_monitor: CpuMonitor,
    pub memory_monitor: MemoryMonitor,
    pub disk_monitor: DiskMonitor,
    pub network_monitor: NetworkMonitor,
    pub container_monitor: ContainerMonitor,
    pub system_monitor: SystemMonitor,
}

impl ResourceMonitor {
    pub fn new(config: MonitoringConfig) -> Self {
        Self {
            cpu_monitor: CpuMonitor::new(config.cpu_config),
            memory_monitor: MemoryMonitor::new(config.memory_config),
            disk_monitor: DiskMonitor::new(config.disk_config),
            network_monitor: NetworkMonitor::new(config.network_config),
            container_monitor: ContainerMonitor::new(config.container_config),
            system_monitor: SystemMonitor::new(config.system_config),
        }
    }
    
    pub async fn start_monitoring(&self) -> Result<()> {
        // Start all monitors
        self.cpu_monitor.start().await?;
        self.memory_monitor.start().await?;
        self.disk_monitor.start().await?;
        self.network_monitor.start().await?;
        self.container_monitor.start().await?;
        self.system_monitor.start().await?;
        
        Ok(())
    }
    
    pub async fn get_metrics(&self) -> Result<ResourceMetrics> {
        Ok(ResourceMetrics {
            cpu: self.cpu_monitor.get_current_metrics().await?,
            memory: self.memory_monitor.get_current_metrics().await?,
            disk: self.disk_monitor.get_current_metrics().await?,
            network: self.network_monitor.get_current_metrics().await?,
            containers: self.container_monitor.get_current_metrics().await?,
            system: self.system_monitor.get_current_metrics().await?,
            timestamp: SystemTime::now(),
        })
    }
}
```

### Resource Metrics Structure

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResourceMetrics {
    pub cpu: CpuMetrics,
    pub memory: MemoryMetrics,
    pub disk: DiskMetrics,
    pub network: NetworkMetrics,
    pub containers: ContainerMetrics,
    pub system: SystemMetrics,
    pub timestamp: SystemTime,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CpuMetrics {
    pub usage_percent: f64,
    pub load_average: [f64; 3],
    pub cores: u32,
    pub frequency_mhz: f64,
    pub temperature_celsius: Option<f64>,
    pub processes: Vec<ProcessCpuMetrics>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryMetrics {
    pub total_bytes: u64,
    pub used_bytes: u64,
    pub free_bytes: u64,
    pub cached_bytes: u64,
    pub buffers_bytes: u64,
    pub swap_total_bytes: u64,
    pub swap_used_bytes: u64,
    pub swap_free_bytes: u64,
    pub processes: Vec<ProcessMemoryMetrics>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiskMetrics {
    pub total_bytes: u64,
    pub used_bytes: u64,
    pub free_bytes: u64,
    pub read_bytes_per_sec: u64,
    pub write_bytes_per_sec: u64,
    pub read_ops_per_sec: u64,
    pub write_ops_per_sec: u64,
    pub partitions: Vec<PartitionMetrics>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkMetrics {
    pub bytes_sent: u64,
    pub bytes_received: u64,
    pub packets_sent: u64,
    pub packets_received: u64,
    pub errors_sent: u64,
    pub errors_received: u64,
    pub bandwidth_bytes_per_sec: u64,
    pub interfaces: Vec<InterfaceMetrics>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerMetrics {
    pub total_containers: u32,
    pub running_containers: u32,
    pub stopped_containers: u32,
    pub container_details: Vec<ContainerDetailMetrics>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemMetrics {
    pub uptime_seconds: u64,
    pub boot_time: SystemTime,
    pub users: u32,
    pub processes: u32,
    pub threads: u32,
    pub open_files: u32,
    pub context_switches: u64,
}
```

### Monitoring Configuration

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MonitoringConfig {
    pub enable_monitoring: bool,
    pub collection_interval: Duration,
    pub retention_period: Duration,
    pub alerting: AlertingConfig,
    pub thresholds: ThresholdConfig,
    pub exporters: Vec<ExporterConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AlertingConfig {
    pub enable_alerting: bool,
    pub alert_channels: Vec<AlertChannel>,
    pub alert_rules: Vec<AlertRule>,
    pub cooldown_period: Duration,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ThresholdConfig {
    pub cpu_usage_percent: f64,
    pub memory_usage_percent: f64,
    pub disk_usage_percent: f64,
    pub network_bandwidth_percent: f64,
    pub container_count: u32,
    pub process_count: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExporterConfig {
    pub name: String,
    pub exporter_type: ExporterType,
    pub endpoint: String,
    pub credentials: Option<Credentials>,
    pub batch_size: u32,
    pub flush_interval: Duration,
}
```

### CPU Monitoring

```rust
pub struct CpuMonitor {
    config: CpuConfig,
    metrics_collector: MetricsCollector,
    alert_manager: AlertManager,
}

impl CpuMonitor {
    pub fn new(config: CpuConfig) -> Self {
        Self {
            config,
            metrics_collector: MetricsCollector::new(),
            alert_manager: AlertManager::new(),
        }
    }
    
    pub async fn start(&self) -> Result<()> {
        let interval = self.config.collection_interval;
        let mut timer = tokio::time::interval(interval);
        
        loop {
            timer.tick().await;
            
            // Collect CPU metrics
            let metrics = self.collect_cpu_metrics().await?;
            
            // Store metrics
            self.metrics_collector.store_metrics(metrics.clone()).await?;
            
            // Check alerts
            self.check_cpu_alerts(&metrics).await?;
        }
    }
    
    async fn collect_cpu_metrics(&self) -> Result<CpuMetrics> {
        // Collect system CPU metrics
        let usage_percent = self.get_cpu_usage_percent().await?;
        let load_average = self.get_load_average().await?;
        let cores = self.get_cpu_cores().await?;
        let frequency = self.get_cpu_frequency().await?;
        let temperature = self.get_cpu_temperature().await?;
        
        // Collect process CPU metrics
        let processes = self.get_process_cpu_metrics().await?;
        
        Ok(CpuMetrics {
            usage_percent,
            load_average,
            cores,
            frequency_mhz: frequency,
            temperature_celsius: temperature,
            processes,
        })
    }
    
    async fn check_cpu_alerts(&self, metrics: &CpuMetrics) -> Result<()> {
        if metrics.usage_percent > self.config.threshold_cpu_usage_percent {
            self.alert_manager.send_alert(Alert {
                severity: AlertSeverity::Warning,
                message: format!("High CPU usage: {:.1}%", metrics.usage_percent),
                resource: "cpu".to_string(),
                value: metrics.usage_percent,
                threshold: self.config.threshold_cpu_usage_percent,
                timestamp: SystemTime::now(),
            }).await?;
        }
        
        Ok(())
    }
}
```

### Memory Monitoring

```rust
pub struct MemoryMonitor {
    config: MemoryConfig,
    metrics_collector: MetricsCollector,
    alert_manager: AlertManager,
}

impl MemoryMonitor {
    pub fn new(config: MemoryConfig) -> Self {
        Self {
            config,
            metrics_collector: MetricsCollector::new(),
            alert_manager: AlertManager::new(),
        }
    }
    
    pub async fn start(&self) -> Result<()> {
        let interval = self.config.collection_interval;
        let mut timer = tokio::time::interval(interval);
        
        loop {
            timer.tick().await;
            
            // Collect memory metrics
            let metrics = self.collect_memory_metrics().await?;
            
            // Store metrics
            self.metrics_collector.store_metrics(metrics.clone()).await?;
            
            // Check alerts
            self.check_memory_alerts(&metrics).await?;
        }
    }
    
    async fn collect_memory_metrics(&self) -> Result<MemoryMetrics> {
        // Collect system memory metrics
        let total_bytes = self.get_total_memory().await?;
        let used_bytes = self.get_used_memory().await?;
        let free_bytes = self.get_free_memory().await?;
        let cached_bytes = self.get_cached_memory().await?;
        let buffers_bytes = self.get_buffers_memory().await?;
        
        // Collect swap metrics
        let swap_total_bytes = self.get_swap_total().await?;
        let swap_used_bytes = self.get_swap_used().await?;
        let swap_free_bytes = self.get_swap_free().await?;
        
        // Collect process memory metrics
        let processes = self.get_process_memory_metrics().await?;
        
        Ok(MemoryMetrics {
            total_bytes,
            used_bytes,
            free_bytes,
            cached_bytes,
            buffers_bytes,
            swap_total_bytes,
            swap_used_bytes,
            swap_free_bytes,
            processes,
        })
    }
    
    async fn check_memory_alerts(&self, metrics: &MemoryMetrics) -> Result<()> {
        let usage_percent = (metrics.used_bytes as f64 / metrics.total_bytes as f64) * 100.0;
        
        if usage_percent > self.config.threshold_memory_usage_percent {
            self.alert_manager.send_alert(Alert {
                severity: AlertSeverity::Warning,
                message: format!("High memory usage: {:.1}%", usage_percent),
                resource: "memory".to_string(),
                value: usage_percent,
                threshold: self.config.threshold_memory_usage_percent,
                timestamp: SystemTime::now(),
            }).await?;
        }
        
        Ok(())
    }
}
```

### Container Monitoring

```rust
pub struct ContainerMonitor {
    config: ContainerConfig,
    docker_client: DockerClient,
    metrics_collector: MetricsCollector,
    alert_manager: AlertManager,
}

impl ContainerMonitor {
    pub fn new(config: ContainerConfig) -> Self {
        Self {
            config,
            docker_client: DockerClient::new(),
            metrics_collector: MetricsCollector::new(),
            alert_manager: AlertManager::new(),
        }
    }
    
    pub async fn start(&self) -> Result<()> {
        let interval = self.config.collection_interval;
        let mut timer = tokio::time::interval(interval);
        
        loop {
            timer.tick().await;
            
            // Collect container metrics
            let metrics = self.collect_container_metrics().await?;
            
            // Store metrics
            self.metrics_collector.store_metrics(metrics.clone()).await?;
            
            // Check alerts
            self.check_container_alerts(&metrics).await?;
        }
    }
    
    async fn collect_container_metrics(&self) -> Result<ContainerMetrics> {
        // Get container list
        let containers = self.docker_client.list_containers().await?;
        
        let total_containers = containers.len() as u32;
        let running_containers = containers.iter().filter(|c| c.state == "running").count() as u32;
        let stopped_containers = containers.iter().filter(|c| c.state == "stopped").count() as u32;
        
        // Collect detailed metrics for each container
        let mut container_details = Vec::new();
        for container in &containers {
            let detail = self.collect_container_detail_metrics(container).await?;
            container_details.push(detail);
        }
        
        Ok(ContainerMetrics {
            total_containers,
            running_containers,
            stopped_containers,
            container_details,
        })
    }
    
    async fn collect_container_detail_metrics(&self, container: &Container) -> Result<ContainerDetailMetrics> {
        // Get container stats
        let stats = self.docker_client.get_container_stats(container.id).await?;
        
        // Calculate resource usage
        let cpu_usage_percent = self.calculate_cpu_usage_percent(&stats).await?;
        let memory_usage_bytes = self.calculate_memory_usage_bytes(&stats).await?;
        let disk_usage_bytes = self.calculate_disk_usage_bytes(&stats).await?;
        let network_bytes_sent = self.calculate_network_bytes_sent(&stats).await?;
        let network_bytes_received = self.calculate_network_bytes_received(&stats).await?;
        
        Ok(ContainerDetailMetrics {
            container_id: container.id.clone(),
            container_name: container.name.clone(),
            state: container.state.clone(),
            cpu_usage_percent,
            memory_usage_bytes,
            disk_usage_bytes,
            network_bytes_sent,
            network_bytes_received,
            created_at: container.created_at,
            started_at: container.started_at,
        })
    }
}
```

### Alerting System

```rust
pub struct AlertManager {
    config: AlertingConfig,
    alert_channels: Vec<Box<dyn AlertChannel>>,
    alert_rules: Vec<AlertRule>,
}

impl AlertManager {
    pub fn new(config: AlertingConfig) -> Self {
        Self {
            config,
            alert_channels: Vec::new(),
            alert_rules: config.alert_rules.clone(),
        }
    }
    
    pub async fn send_alert(&self, alert: Alert) -> Result<()> {
        // Check if alert should be sent based on rules
        if !self.should_send_alert(&alert) {
            return Ok(());
        }
        
        // Send alert to all channels
        for channel in &self.alert_channels {
            channel.send_alert(&alert).await?;
        }
        
        Ok(())
    }
    
    fn should_send_alert(&self, alert: &Alert) -> bool {
        // Check cooldown period
        if let Some(last_alert) = self.get_last_alert(&alert.resource) {
            if alert.timestamp.duration_since(last_alert.timestamp).unwrap_or_default() < self.config.cooldown_period {
                return false;
            }
        }
        
        // Check alert rules
        for rule in &self.alert_rules {
            if rule.matches(alert) {
                return true;
            }
        }
        
        false
    }
}

pub trait AlertChannel: Send + Sync {
    async fn send_alert(&self, alert: &Alert) -> Result<()>;
}

pub struct EmailAlertChannel {
    smtp_client: SmtpClient,
    recipients: Vec<String>,
}

impl AlertChannel for EmailAlertChannel {
    async fn send_alert(&self, alert: &Alert) -> Result<()> {
        let message = format!(
            "Resource Alert: {}\nSeverity: {:?}\nResource: {}\nValue: {}\nThreshold: {}\nTime: {:?}",
            alert.message,
            alert.severity,
            alert.resource,
            alert.value,
            alert.threshold,
            alert.timestamp
        );
        
        self.smtp_client.send_email(&self.recipients, "Resource Alert", &message).await?;
        
        Ok(())
    }
}

pub struct SlackAlertChannel {
    webhook_url: String,
    channel: String,
}

impl AlertChannel for SlackAlertChannel {
    async fn send_alert(&self, alert: &Alert) -> Result<()> {
        let payload = serde_json::json!({
            "channel": self.channel,
            "text": format!("🚨 Resource Alert: {}", alert.message),
            "attachments": [{
                "color": match alert.severity {
                    AlertSeverity::Critical => "danger",
                    AlertSeverity::Warning => "warning",
                    AlertSeverity::Info => "good",
                },
                "fields": [
                    {"title": "Resource", "value": alert.resource, "short": true},
                    {"title": "Value", "value": alert.value.to_string(), "short": true},
                    {"title": "Threshold", "value": alert.threshold.to_string(), "short": true},
                    {"title": "Time", "value": alert.timestamp.to_string(), "short": true}
                ]
            }]
        });
        
        let client = reqwest::Client::new();
        client.post(&self.webhook_url)
            .json(&payload)
            .send()
            .await?;
        
        Ok(())
    }
}
```

### Metrics Export

```rust
pub struct MetricsExporter {
    config: ExporterConfig,
    client: reqwest::Client,
}

impl MetricsExporter {
    pub fn new(config: ExporterConfig) -> Self {
        Self {
            config,
            client: reqwest::Client::new(),
        }
    }
    
    pub async fn export_metrics(&self, metrics: &ResourceMetrics) -> Result<()> {
        match self.config.exporter_type {
            ExporterType::Prometheus => self.export_to_prometheus(metrics).await,
            ExporterType::InfluxDB => self.export_to_influxdb(metrics).await,
            ExporterType::Elasticsearch => self.export_to_elasticsearch(metrics).await,
            ExporterType::Custom => self.export_to_custom(metrics).await,
        }
    }
    
    async fn export_to_prometheus(&self, metrics: &ResourceMetrics) -> Result<()> {
        let prometheus_metrics = self.convert_to_prometheus_format(metrics);
        
        self.client.post(&self.config.endpoint)
            .header("Content-Type", "text/plain")
            .body(prometheus_metrics)
            .send()
            .await?;
        
        Ok(())
    }
    
    async fn export_to_influxdb(&self, metrics: &ResourceMetrics) -> Result<()> {
        let influxdb_data = self.convert_to_influxdb_format(metrics);
        
        self.client.post(&self.config.endpoint)
            .header("Content-Type", "application/json")
            .json(&influxdb_data)
            .send()
            .await?;
        
        Ok(())
    }
    
    fn convert_to_prometheus_format(&self, metrics: &ResourceMetrics) -> String {
        format!(
            "# HELP cpu_usage_percent CPU usage percentage\n\
             # TYPE cpu_usage_percent gauge\n\
             cpu_usage_percent {}\n\
             \n\
             # HELP memory_usage_bytes Memory usage in bytes\n\
             # TYPE memory_usage_bytes gauge\n\
             memory_usage_bytes {}\n\
             \n\
             # HELP disk_usage_bytes Disk usage in bytes\n\
             # TYPE disk_usage_bytes gauge\n\
             disk_usage_bytes {}\n",
            metrics.cpu.usage_percent,
            metrics.memory.used_bytes,
            metrics.disk.used_bytes
        )
    }
}
```

## Consequences

### Positive

- **Comprehensive Monitoring**: Tracks all resource types
- **Real-time Alerts**: Proactive alerting for resource issues
- **Performance Insights**: Detailed performance analytics
- **Cost Optimization**: Resource usage optimization
- **SLA Compliance**: Meet performance objectives

### Negative

- **Performance Overhead**: Monitoring adds overhead
- **Storage Requirements**: Metrics storage needs
- **Complexity**: More complex monitoring setup
- **Resource Usage**: Additional resources for monitoring

### Neutral

- **Configuration**: Requires careful monitoring configuration
- **Maintenance**: Ongoing monitoring system maintenance
- **Integration**: Integration with external monitoring systems

## Alternatives Considered

### 1. No Resource Monitoring

Run without resource monitoring.

**Rejected because:**
- No performance insights
- No resource optimization
- No proactive alerting
- Poor user experience

### 2. Basic System Monitoring

Use basic system monitoring tools.

**Rejected because:**
- Limited container visibility
- No custom metrics
- Poor integration
- Limited alerting

### 3. External Monitoring Only

Rely only on external monitoring systems.

**Rejected because:**
- External dependencies
- Limited customization
- Integration complexity
- Performance overhead

### 4. Simple Metrics Collection

Implement basic metrics collection only.

**Rejected because:**
- No alerting
- No analytics
- Limited insights
- Poor user experience

## Implementation Details

### Integration with CleanroomEnvironment

```rust
impl CleanroomEnvironment {
    pub async fn start_monitoring(&self) -> Result<()> {
        if !self.config.monitoring.enable_monitoring {
            return Ok(());
        }
        
        // Start resource monitoring
        self.resource_monitor.start_monitoring().await?;
        
        // Start alerting
        self.alert_manager.start().await?;
        
        // Start metrics export
        self.metrics_exporter.start().await?;
        
        Ok(())
    }
    
    pub async fn get_current_metrics(&self) -> Result<ResourceMetrics> {
        self.resource_monitor.get_metrics().await
    }
    
    pub async fn check_resource_limits(&self) -> Result<()> {
        let metrics = self.get_current_metrics().await?;
        let limits = &self.config.resource_limits;
        
        // Check CPU limits
        if metrics.cpu.usage_percent > limits.max_cpu_usage_percent {
            return Err(CleanroomError::resource_limit_exceeded(
                "CPU usage",
                format!("{:.1}%", limits.max_cpu_usage_percent),
                format!("{:.1}%", metrics.cpu.usage_percent),
            ));
        }
        
        // Check memory limits
        let memory_usage_percent = (metrics.memory.used_bytes as f64 / metrics.memory.total_bytes as f64) * 100.0;
        if memory_usage_percent > limits.max_memory_usage_percent {
            return Err(CleanroomError::resource_limit_exceeded(
                "Memory usage",
                format!("{:.1}%", limits.max_memory_usage_percent),
                format!("{:.1}%", memory_usage_percent),
            ));
        }
        
        // Check disk limits
        let disk_usage_percent = (metrics.disk.used_bytes as f64 / metrics.disk.total_bytes as f64) * 100.0;
        if disk_usage_percent > limits.max_disk_usage_percent {
            return Err(CleanroomError::resource_limit_exceeded(
                "Disk usage",
                format!("{:.1}%", limits.max_disk_usage_percent),
                format!("{:.1}%", disk_usage_percent),
            ));
        }
        
        Ok(())
    }
}
```

## Performance Impact

### Monitoring Overhead
- **CPU**: ~1-2% overhead
- **Memory**: ~50-100MB additional usage
- **Disk**: ~10-50MB per day for metrics storage
- **Network**: ~1-5MB per hour for metrics export

### Optimization Strategies
- **Sampling**: Reduce collection frequency for less critical metrics
- **Aggregation**: Aggregate metrics to reduce storage requirements
- **Compression**: Compress metrics data for storage efficiency
- **Filtering**: Filter out irrelevant metrics

## Configuration Examples

### Development Configuration
```toml
[cleanroom.monitoring]
enable_monitoring = true
collection_interval = 30s
retention_period = 7d

[cleanroom.monitoring.thresholds]
cpu_usage_percent = 80.0
memory_usage_percent = 85.0
disk_usage_percent = 90.0
network_bandwidth_percent = 80.0
container_count = 10
process_count = 1000

[cleanroom.monitoring.alerting]
enable_alerting = true
cooldown_period = 5m
```

### Production Configuration
```toml
[cleanroom.monitoring]
enable_monitoring = true
collection_interval = 10s
retention_period = 30d

[cleanroom.monitoring.thresholds]
cpu_usage_percent = 70.0
memory_usage_percent = 80.0
disk_usage_percent = 85.0
network_bandwidth_percent = 70.0
container_count = 50
process_count = 5000

[cleanroom.monitoring.alerting]
enable_alerting = true
cooldown_period = 2m

[cleanroom.monitoring.exporters]
[[cleanroom.monitoring.exporters]]
name = "prometheus"
exporter_type = "Prometheus"
endpoint = "http://prometheus:9090/api/v1/write"
batch_size = 100
flush_interval = 30s
```

## References

- [Prometheus Monitoring](https://prometheus.io/docs/)
- [Grafana Dashboards](https://grafana.com/docs/)
- [Docker Stats API](https://docs.docker.com/engine/api/v1.41/#operation/ContainerStats)
- [System Resource Monitoring](https://docs.rs/sysinfo/)

## Future Considerations

- **Machine Learning**: ML-based anomaly detection
- **Predictive Analytics**: Predictive resource usage analysis
- **Auto-scaling**: Automatic resource scaling based on metrics
- **Cost Optimization**: AI-driven cost optimization recommendations
- **Advanced Alerting**: Intelligent alerting with context awareness
