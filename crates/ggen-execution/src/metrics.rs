// Metrics collection and monitoring for the execution framework
use crate::error::*;
use crate::types::*;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

// ============================================================================
// PERFORMANCE METRICS
// ============================================================================

/// Performance metrics collection
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    pub timestamp: DateTime<Utc>,
    pub execution_duration_ms: u64,
    pub throughput_per_second: f64,
    pub success_rate: f64,
    pub error_rate: f64,
    pub resource_usage: ResourceUsage,
    pub memory_usage_mb: u64,
    pub cpu_usage_percent: f64,
    pub disk_usage_percent: f64,
    pub network_io_mb: u64,
}

impl Default for PerformanceMetrics {
    fn default() -> Self {
        Self {
            timestamp: Utc::now(),
            execution_duration_ms: 0,
            throughput_per_second: 0.0,
            success_rate: 0.0,
            error_rate: 0.0,
            resource_usage: ResourceUsage {
                cpu_percent: 0.0,
                memory_mb: 0,
                network_in_mb: 0,
                network_out_mb: 0,
            },
            memory_usage_mb: 0,
            cpu_usage_percent: 0.0,
            disk_usage_percent: 0.0,
            network_io_mb: 0,
        }
    }
}

/// Metrics collector for tracking performance
#[derive(Debug)]
pub struct MetricsCollector {
    metrics_history: VecDeque<PerformanceMetrics>,
    max_history_size: usize,
    aggregations: HashMap<String, MetricAggregation>,
}

/// Metric aggregation for calculating rolling averages
#[derive(Debug, Clone)]
pub struct MetricAggregation {
    pub name: String,
    pub values: VecDeque<f64>,
    pub max_values: usize,
    pub current_average: f64,
}

// Serialize implementation for MetricAggregation
impl Serialize for MetricAggregation {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("MetricAggregation", 4)?;
        state.serialize_field("name", &self.name)?;
        state.serialize_field("values", &self.values.iter().collect::<Vec<_>>())?;
        state.serialize_field("max_values", &self.max_values)?;
        state.serialize_field("current_average", &self.current_average)?;
        state.end()
    }
}

// Deserialize implementation for MetricAggregation
impl<'de> Deserialize<'de> for MetricAggregation {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, Visitor};
        use std::fmt;

        struct MetricAggregationVisitor;

        impl<'de> Visitor<'de> for MetricAggregationVisitor {
            type Value = MetricAggregation;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct MetricAggregation")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut name = None;
                let mut values = None;
                let mut max_values = None;
                let mut current_average = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        "name" => {
                            if name.is_some() {
                                return Err(de::Error::duplicate_field("name"));
                            }
                            name = Some(map.next_value()?);
                        }
                        "values" => {
                            if values.is_some() {
                                return Err(de::Error::duplicate_field("values"));
                            }
                            let vals: Vec<f64> = map.next_value()?;
                            values = Some(vals.into_iter().collect());
                        }
                        "max_values" => {
                            if max_values.is_some() {
                                return Err(de::Error::duplicate_field("max_values"));
                            }
                            max_values = Some(map.next_value()?);
                        }
                        "current_average" => {
                            if current_average.is_some() {
                                return Err(de::Error::duplicate_field("current_average"));
                            }
                            current_average = Some(map.next_value()?);
                        }
                        _ => {
                            map.next_value::<de::IgnoredAny>()?;
                        }
                    }
                }

                let name = name.ok_or_else(|| de::Error::missing_field("name"))?;
                let values = values.ok_or_else(|| de::Error::missing_field("values"))?;
                let max_values =
                    max_values.ok_or_else(|| de::Error::missing_field("max_values"))?;
                let current_average =
                    current_average.ok_or_else(|| de::Error::missing_field("current_average"))?;

                Ok(MetricAggregation {
                    name,
                    values,
                    max_values,
                    current_average,
                })
            }
        }

        deserializer.deserialize_struct(
            "MetricAggregation",
            &["name", "values", "max_values", "current_average"],
            MetricAggregationVisitor,
        )
    }
}

impl MetricAggregation {
    pub fn new(name: &str, max_values: usize) -> Self {
        Self {
            name: name.to_string(),
            values: VecDeque::with_capacity(max_values),
            max_values,
            current_average: 0.0,
        }
    }

    pub fn add_value(&mut self, value: f64) {
        self.values.push_back(value);
        if self.values.len() > self.max_values {
            self.values.pop_front();
        }
        self.update_average();
    }

    fn update_average(&mut self) {
        if self.values.is_empty() {
            self.current_average = 0.0;
        } else {
            let sum: f64 = self.values.iter().sum();
            self.current_average = sum / self.values.len() as f64;
        }
    }
}

impl MetricsCollector {
    pub fn new(max_history_size: usize) -> Self {
        Self {
            metrics_history: VecDeque::with_capacity(max_history_size),
            max_history_size,
            aggregations: HashMap::new(),
        }
    }

    /// Record a new metrics entry
    pub fn record_metrics(&mut self, metrics: PerformanceMetrics) {
        // Update aggregations first
        self.update_aggregations(&metrics);

        // Then add to history
        self.metrics_history.push_back(metrics);

        // Keep history size within limits
        if self.metrics_history.len() > self.max_history_size {
            self.metrics_history.pop_front();
        }
    }

    /// Update aggregations with new metrics
    fn update_aggregations(&mut self, metrics: &PerformanceMetrics) {
        // Throughput aggregation
        self.get_or_create_aggregation("throughput", 100)
            .add_value(metrics.throughput_per_second);

        // Success rate aggregation
        self.get_or_create_aggregation("success_rate", 100)
            .add_value(metrics.success_rate);

        // Error rate aggregation
        self.get_or_create_aggregation("error_rate", 100)
            .add_value(metrics.error_rate);

        // CPU usage aggregation
        self.get_or_create_aggregation("cpu_usage", 100)
            .add_value(metrics.cpu_usage_percent);

        // Memory usage aggregation
        self.get_or_create_aggregation("memory_usage", 100)
            .add_value(metrics.memory_usage_mb as f64);

        // Execution duration aggregation
        self.get_or_create_aggregation("execution_duration", 100)
            .add_value(metrics.execution_duration_ms as f64);
    }

    /// Get or create aggregation
    fn get_or_create_aggregation(
        &mut self, name: &str, max_values: usize,
    ) -> &mut MetricAggregation {
        self.aggregations
            .entry(name.to_string())
            .or_insert_with(|| MetricAggregation::new(name, max_values))
    }

    /// Get metrics history
    pub fn get_history(&self, count: Option<usize>) -> Vec<&PerformanceMetrics> {
        let history = self.metrics_history.iter().rev();
        if let Some(count) = count {
            history.take(count).collect()
        } else {
            history.collect()
        }
    }

    /// Get aggregations
    pub fn get_aggregations(&self) -> &HashMap<String, MetricAggregation> {
        &self.aggregations
    }

    /// Get average metrics over time period
    pub fn get_average_metrics(&self, time_period_ms: u64) -> Option<PerformanceMetrics> {
        let cutoff_time = Utc::now() - chrono::Duration::milliseconds(time_period_ms as i64);
        let relevant_metrics: Vec<_> = self
            .metrics_history
            .iter()
            .filter(|m| m.timestamp >= cutoff_time)
            .collect();

        if relevant_metrics.is_empty() {
            return None;
        }

        let avg_duration = relevant_metrics
            .iter()
            .map(|m| m.execution_duration_ms)
            .sum::<u64>()
            / relevant_metrics.len() as u64;

        let avg_throughput = relevant_metrics
            .iter()
            .map(|m| m.throughput_per_second)
            .sum::<f64>()
            / relevant_metrics.len() as f64;

        let avg_success_rate = relevant_metrics.iter().map(|m| m.success_rate).sum::<f64>()
            / relevant_metrics.len() as f64;

        let avg_error_rate = relevant_metrics.iter().map(|m| m.error_rate).sum::<f64>()
            / relevant_metrics.len() as f64;

        let avg_cpu = relevant_metrics
            .iter()
            .map(|m| m.cpu_usage_percent)
            .sum::<f64>()
            / relevant_metrics.len() as f64;

        let avg_memory = relevant_metrics
            .iter()
            .map(|m| m.memory_usage_mb)
            .sum::<u64>()
            / relevant_metrics.len() as u64;

        let avg_network = relevant_metrics
            .iter()
            .map(|m| m.network_io_mb)
            .sum::<u64>()
            / relevant_metrics.len() as u64;

        Some(PerformanceMetrics {
            timestamp: Utc::now(),
            execution_duration_ms: avg_duration,
            throughput_per_second: avg_throughput,
            success_rate: avg_success_rate,
            error_rate: avg_error_rate,
            resource_usage: ResourceUsage {
                cpu_percent: avg_cpu,
                memory_mb: avg_memory,
                network_in_mb: avg_network,
                network_out_mb: avg_network,
            },
            memory_usage_mb: avg_memory,
            cpu_usage_percent: avg_cpu,
            disk_usage_percent: 0.0,
            network_io_mb: avg_network,
        })
    }

    /// Get peak usage metrics
    pub fn get_peak_metrics(&self) -> Option<PerformanceMetrics> {
        if self.metrics_history.is_empty() {
            return None;
        }

        let peak_metrics = self
            .metrics_history
            .iter()
            .max_by_key(|m| m.memory_usage_mb)
            .unwrap();

        Some(peak_metrics.clone())
    }

    /// Clear old metrics
    pub fn cleanup_old_metrics(&mut self, max_age_ms: u64) {
        let cutoff_time = Utc::now() - chrono::Duration::milliseconds(max_age_ms as i64);
        self.metrics_history.retain(|m| m.timestamp >= cutoff_time);
    }
}

// ============================================================================
// AGENT METRICS
// ============================================================================

/// Agent-specific metrics collection
pub struct AgentMetricsCollector {
    agent_metrics: HashMap<AgentId, AgentMetricsData>,
    global_metrics: GlobalAgentMetrics,
}

/// Agent-specific metrics data
#[derive(Debug, Clone)]
pub struct AgentMetricsData {
    pub agent_id: AgentId,
    pub name: String,
    pub tasks_completed: u64,
    pub tasks_failed: u64,
    pub total_execution_time_ms: u64,
    pub average_execution_time_ms: u64,
    pub current_concurrent_tasks: usize,
    pub max_concurrent_tasks: usize,
    pub last_activity: DateTime<Utc>,
    pub cpu_usage_percent: f64,
    pub memory_usage_mb: u64,
    pub error_rate: f64,
    pub throughput_tasks_per_second: f64,
    pub uptime_seconds: u64,
}

/// Global agent metrics
#[derive(Debug, Clone)]
pub struct GlobalAgentMetrics {
    pub total_agents: usize,
    pub active_agents: usize,
    pub total_tasks: u64,
    pub total_failures: u64,
    pub average_cpu_usage: f64,
    pub average_memory_usage: f64,
    pub system_throughput: f64,
    pub last_updated: DateTime<Utc>,
}

impl AgentMetricsCollector {
    pub fn new() -> Self {
        Self {
            agent_metrics: HashMap::new(),
            global_metrics: GlobalAgentMetrics {
                total_agents: 0,
                active_agents: 0,
                total_tasks: 0,
                total_failures: 0,
                average_cpu_usage: 0.0,
                average_memory_usage: 0.0,
                system_throughput: 0.0,
                last_updated: Utc::now(),
            },
        }
    }

    /// Register or update agent metrics
    pub fn update_agent_metrics(&mut self, agent_id: &str, name: &str, metrics: AgentMetrics) {
        let agent_data = AgentMetricsData {
            agent_id: agent_id.to_string(),
            name: name.to_string(),
            tasks_completed: metrics.tasks_completed,
            tasks_failed: metrics.tasks_failed,
            total_execution_time_ms: metrics.average_task_duration_ms * metrics.tasks_completed,
            average_execution_time_ms: metrics.average_task_duration_ms,
            current_concurrent_tasks: 0, // Would be updated by actual concurrent task tracking
            max_concurrent_tasks: 10,    // Default max
            last_activity: Utc::now(),
            cpu_usage_percent: 0.0,
            memory_usage_mb: 0,
            error_rate: metrics.error_rate,
            throughput_tasks_per_second: metrics.throughput_tasks_per_second,
            uptime_seconds: 0,
        };

        self.agent_metrics.insert(agent_id.to_string(), agent_data);
        self.update_global_metrics();
    }

    /// Update global metrics based on all agent metrics
    fn update_global_metrics(&mut self) {
        let total_agents = self.agent_metrics.len();
        let active_agents = self
            .agent_metrics
            .values()
            .filter(|m| {
                Utc::now()
                    .signed_duration_since(m.last_activity)
                    .num_seconds()
                    < 300
            })
            .count();

        let total_tasks: u64 = self
            .agent_metrics
            .values()
            .map(|m| m.tasks_completed + m.tasks_failed)
            .sum();
        let total_failures: u64 = self.agent_metrics.values().map(|m| m.tasks_failed).sum();

        let total_cpu: f64 = self
            .agent_metrics
            .values()
            .map(|m| m.cpu_usage_percent)
            .sum();
        let total_memory: u64 = self.agent_metrics.values().map(|m| m.memory_usage_mb).sum();

        let avg_cpu = if total_agents > 0 {
            total_cpu / total_agents as f64
        } else {
            0.0
        };
        let avg_memory = if total_agents > 0 {
            total_memory as f64 / total_agents as f64
        } else {
            0.0
        };

        let total_throughput: f64 = self
            .agent_metrics
            .values()
            .map(|m| m.throughput_tasks_per_second)
            .sum();

        self.global_metrics = GlobalAgentMetrics {
            total_agents,
            active_agents,
            total_tasks,
            total_failures,
            average_cpu_usage: avg_cpu,
            average_memory_usage: avg_memory,
            system_throughput: total_throughput,
            last_updated: Utc::now(),
        };
    }

    /// Get agent metrics
    pub fn get_agent_metrics(&self, agent_id: &str) -> Option<&AgentMetricsData> {
        self.agent_metrics.get(agent_id)
    }

    /// Get global metrics
    pub fn get_global_metrics(&self) -> &GlobalAgentMetrics {
        &self.global_metrics
    }

    /// Get all agent metrics
    pub fn get_all_agent_metrics(&self) -> &HashMap<AgentId, AgentMetricsData> {
        &self.agent_metrics
    }

    /// Get top performing agents
    pub fn get_top_agents(&self, limit: usize) -> Vec<&AgentMetricsData> {
        let mut agents: Vec<_> = self.agent_metrics.values().collect();
        agents.sort_by(|a, b| {
            // Sort by success rate first, then by throughput
            let a_success_rate =
                a.tasks_completed as f64 / (a.tasks_completed + a.tasks_failed).max(1) as f64;
            let b_success_rate =
                b.tasks_completed as f64 / (b.tasks_completed + b.tasks_failed).max(1) as f64;
            b_success_rate
                .partial_cmp(&a_success_rate)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        agents.into_iter().take(limit).collect()
    }

    /// Get agents with high error rates
    pub fn get_high_error_agents(&self, threshold: f64) -> Vec<&AgentMetricsData> {
        self.agent_metrics
            .values()
            .filter(|m| m.error_rate > threshold)
            .collect()
    }
}

// ============================================================================
// EXECUTION METRICS
// ============================================================================

/// Execution metrics for tracking workflow and pipeline performance
pub struct ExecutionMetricsCollector {
    workflow_metrics: HashMap<WorkflowId, WorkflowExecutionMetrics>,
    pipeline_metrics: HashMap<PipelineId, PipelineExecutionMetrics>,
    execution_history: VecDeque<ExecutionRecord>,
}

/// Workflow execution metrics
#[derive(Debug, Clone)]
pub struct WorkflowExecutionMetrics {
    pub workflow_id: WorkflowId,
    pub workflow_name: String,
    pub total_executions: u64,
    pub successful_executions: u64,
    pub failed_executions: u64,
    pub average_duration_ms: u64,
    total_duration_ms: u64,
    pub total_tasks: u64,
    pub average_tasks_per_execution: f64,
    pub success_rate: f64,
    pub last_execution: DateTime<Utc>,
}

/// Pipeline execution metrics
#[derive(Debug, Clone)]
pub struct PipelineExecutionMetrics {
    pub pipeline_id: PipelineId,
    pub pipeline_name: String,
    pub total_executions: u64,
    pub successful_executions: u64,
    pub failed_executions: u64,
    pub average_duration_ms: u64,
    total_duration_ms: u64,
    pub total_stages: u64,
    pub average_stages_per_execution: f64,
    pub success_rate: f64,
    pub last_execution: DateTime<Utc>,
}

/// Execution record
#[derive(Debug, Clone)]
pub struct ExecutionRecord {
    pub record_id: String,
    pub record_type: ExecutionRecordType,
    pub entity_id: String,
    pub entity_name: String,
    pub status: ExecutionStatus,
    pub start_time: DateTime<Utc>,
    pub end_time: Option<DateTime<Utc>>,
    pub duration_ms: Option<u64>,
    pub tasks_count: Option<u64>,
    pub error_message: Option<String>,
}

/// Execution record types
#[derive(Debug, Clone, PartialEq)]
pub enum ExecutionRecordType {
    Workflow,
    Pipeline,
    Task,
    Stage,
}

impl ExecutionMetricsCollector {
    pub fn new() -> Self {
        Self {
            workflow_metrics: HashMap::new(),
            pipeline_metrics: HashMap::new(),
            execution_history: VecDeque::new(),
        }
    }

    /// Record workflow execution
    pub fn record_workflow_execution(
        &mut self, workflow_id: &str, workflow_name: &str, duration_ms: u64, success: bool,
        tasks_count: u64,
    ) {
        let metrics = self
            .workflow_metrics
            .entry(workflow_id.to_string())
            .or_insert_with(|| WorkflowExecutionMetrics {
                workflow_id: workflow_id.to_string(),
                workflow_name: workflow_name.to_string(),
                total_executions: 0,
                successful_executions: 0,
                failed_executions: 0,
                average_duration_ms: 0,
                total_duration_ms: 0,
                total_tasks: 0,
                average_tasks_per_execution: 0.0,
                success_rate: 0.0,
                last_execution: Utc::now(),
            });

        metrics.total_executions += 1;
        metrics.total_duration_ms += duration_ms;
        metrics.average_duration_ms = metrics.total_duration_ms / metrics.total_executions;
        metrics.total_tasks += tasks_count;
        metrics.average_tasks_per_execution =
            metrics.total_tasks as f64 / metrics.total_executions as f64;
        metrics.last_execution = Utc::now();

        if success {
            metrics.successful_executions += 1;
        } else {
            metrics.failed_executions += 1;
        }

        metrics.success_rate =
            metrics.successful_executions as f64 / metrics.total_executions as f64;

        // Add to execution history
        self.execution_history.push_back(ExecutionRecord {
            record_id: uuid::Uuid::new_v4().to_string(),
            record_type: ExecutionRecordType::Workflow,
            entity_id: workflow_id.to_string(),
            entity_name: workflow_name.to_string(),
            status: if success {
                ExecutionStatus::Completed
            } else {
                ExecutionStatus::Failed("Workflow execution failed".to_string())
            },
            start_time: Utc::now() - chrono::Duration::milliseconds(duration_ms as i64),
            end_time: Some(Utc::now()),
            duration_ms: Some(duration_ms),
            tasks_count: Some(tasks_count),
            error_message: if !success {
                Some("Workflow execution failed".to_string())
            } else {
                None
            },
        });
    }

    /// Record pipeline execution
    pub fn record_pipeline_execution(
        &mut self, pipeline_id: &str, pipeline_name: &str, duration_ms: u64, success: bool,
        stages_count: u64,
    ) {
        let metrics = self
            .pipeline_metrics
            .entry(pipeline_id.to_string())
            .or_insert_with(|| PipelineExecutionMetrics {
                pipeline_id: pipeline_id.to_string(),
                pipeline_name: pipeline_name.to_string(),
                total_executions: 0,
                successful_executions: 0,
                failed_executions: 0,
                average_duration_ms: 0,
                total_duration_ms: 0,
                total_stages: 0,
                average_stages_per_execution: 0.0,
                success_rate: 0.0,
                last_execution: Utc::now(),
            });

        metrics.total_executions += 1;
        metrics.total_duration_ms += duration_ms;
        metrics.average_duration_ms = metrics.total_duration_ms / metrics.total_executions;
        metrics.total_stages += stages_count;
        metrics.average_stages_per_execution =
            metrics.total_stages as f64 / metrics.total_executions as f64;
        metrics.last_execution = Utc::now();

        if success {
            metrics.successful_executions += 1;
        } else {
            metrics.failed_executions += 1;
        }

        metrics.success_rate =
            metrics.successful_executions as f64 / metrics.total_executions as f64;

        // Add to execution history
        self.execution_history.push_back(ExecutionRecord {
            record_id: uuid::Uuid::new_v4().to_string(),
            record_type: ExecutionRecordType::Pipeline,
            entity_id: pipeline_id.to_string(),
            entity_name: pipeline_name.to_string(),
            status: if success {
                ExecutionStatus::Completed
            } else {
                ExecutionStatus::Failed("Pipeline execution failed".to_string())
            },
            start_time: Utc::now() - chrono::Duration::milliseconds(duration_ms as i64),
            end_time: Some(Utc::now()),
            duration_ms: Some(duration_ms),
            tasks_count: None,
            error_message: if !success {
                Some("Pipeline execution failed".to_string())
            } else {
                None
            },
        });
    }

    /// Get workflow metrics
    pub fn get_workflow_metrics(&self, workflow_id: &str) -> Option<&WorkflowExecutionMetrics> {
        self.workflow_metrics.get(workflow_id)
    }

    /// Get pipeline metrics
    pub fn get_pipeline_metrics(&self, pipeline_id: &str) -> Option<&PipelineExecutionMetrics> {
        self.pipeline_metrics.get(pipeline_id)
    }

    /// Get all workflow metrics
    pub fn get_all_workflow_metrics(&self) -> &HashMap<WorkflowId, WorkflowExecutionMetrics> {
        &self.workflow_metrics
    }

    /// Get all pipeline metrics
    pub fn get_all_pipeline_metrics(&self) -> &HashMap<PipelineId, PipelineExecutionMetrics> {
        &self.pipeline_metrics
    }

    /// Get execution history
    pub fn get_execution_history(&self, limit: Option<usize>) -> Vec<&ExecutionRecord> {
        let history = self.execution_history.iter().rev();
        if let Some(limit) = limit {
            history.take(limit).collect()
        } else {
            history.collect()
        }
    }

    /// Get top performing workflows
    pub fn get_top_workflows(&self, limit: usize) -> Vec<&WorkflowExecutionMetrics> {
        let mut workflows: Vec<_> = self.workflow_metrics.values().collect();
        workflows.sort_by(|a, b| {
            b.success_rate
                .partial_cmp(&a.success_rate)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        workflows.into_iter().take(limit).collect()
    }

    /// Get most executed workflows
    pub fn get_most_executed_workflows(&self, limit: usize) -> Vec<&WorkflowExecutionMetrics> {
        let mut workflows: Vec<_> = self.workflow_metrics.values().collect();
        workflows.sort_by(|a, b| b.total_executions.cmp(&a.total_executions));
        workflows.into_iter().take(limit).collect()
    }
}

// ============================================================================
// METRICS EXPORTER
// ============================================================================

/// Metrics exporter for different formats
pub struct MetricsExporter {
    format: ExportFormat,
    include_metadata: bool,
}

/// Export formats
#[derive(Debug, Clone, PartialEq)]
pub enum ExportFormat {
    Json,
    Csv,
    Prometheus,
    InfluxDb,
}

impl MetricsExporter {
    pub fn new(format: ExportFormat, include_metadata: bool) -> Self {
        Self {
            format,
            include_metadata,
        }
    }

    /// Export performance metrics
    pub async fn export_performance_metrics(
        &self, collector: &MetricsCollector, output_path: &str,
    ) -> Result<(), ExecutionError> {
        match self.format {
            ExportFormat::Json => self.export_performance_json(collector, output_path).await,
            ExportFormat::Csv => self.export_performance_csv(collector, output_path).await,
            ExportFormat::Prometheus => {
                self.export_performance_prometheus(collector, output_path)
                    .await
            }
            ExportFormat::InfluxDb => {
                self.export_performance_influxdb(collector, output_path)
                    .await
            }
        }
    }

    /// Export to JSON format
    async fn export_performance_json(
        &self, collector: &MetricsCollector, output_path: &str,
    ) -> Result<(), ExecutionError> {
        let metrics = collector.get_average_metrics(3600000); // Last hour
        let export_data = if let Some(metrics) = metrics {
            serde_json::json!({
                "format": "json",
                "timestamp": metrics.timestamp.to_rfc3339(),
                "metrics": metrics,
                "metadata": if self.include_metadata {
                    serde_json::json!({
                        "history_size": collector.metrics_history.len(),
                        "aggregations": collector.get_aggregations()
                    })
                } else {
                    serde_json::json!(null)
                }
            })
        } else {
            serde_json::json!({
                "error": "No metrics data available"
            })
        };

        let json_str = serde_json::to_string_pretty(&export_data)
            .map_err(|e| ExecutionError::Serialization(e))?;

        tokio::fs::write(output_path, json_str)
            .await
            .map_err(|e| ExecutionError::Io(e))?;

        Ok(())
    }

    /// Export to CSV format
    async fn export_performance_csv(
        &self, collector: &MetricsCollector, output_path: &str,
    ) -> Result<(), ExecutionError> {
        let metrics = collector.get_history(Some(100)); // Last 100 metrics

        let mut csv_content = String::new();
        csv_content.push_str("timestamp,execution_duration_ms,throughput_per_second,success_rate,error_rate,cpu_usage_percent,memory_usage_mb\n");

        for metric in metrics {
            csv_content.push_str(&format!(
                "{},{},{},{},{},{},{}\n",
                metric.timestamp.to_rfc3339(),
                metric.execution_duration_ms,
                metric.throughput_per_second,
                metric.success_rate,
                metric.error_rate,
                metric.cpu_usage_percent,
                metric.memory_usage_mb
            ));
        }

        tokio::fs::write(output_path, csv_content)
            .await
            .map_err(|e| ExecutionError::Io(e))?;

        Ok(())
    }

    /// Export to Prometheus format
    async fn export_performance_prometheus(
        &self, collector: &MetricsCollector, output_path: &str,
    ) -> Result<(), ExecutionError> {
        let metrics = collector.get_average_metrics(60000); // Last minute

        let mut prometheus_content = String::new();

        if let Some(metrics) = metrics {
            prometheus_content.push_str(&format!(
                "# HELP ggen_execution_duration_ms Execution duration in milliseconds\n"
            ));
            prometheus_content.push_str(&format!("# TYPE ggen_execution_duration_ms gauge\n"));
            prometheus_content.push_str(&format!(
                "ggen_execution_duration_ms {}\n",
                metrics.execution_duration_ms
            ));

            prometheus_content.push_str(&format!(
                "# HELP ggen_throughput_per_second Tasks per second\n"
            ));
            prometheus_content.push_str(&format!("# TYPE ggen_throughput_per_second gauge\n"));
            prometheus_content.push_str(&format!(
                "ggen_throughput_per_second {}\n",
                metrics.throughput_per_second
            ));

            prometheus_content.push_str(&format!("# HELP ggen_success_rate Success rate\n"));
            prometheus_content.push_str(&format!("# TYPE ggen_success_rate gauge\n"));
            prometheus_content.push_str(&format!("ggen_success_rate {}\n", metrics.success_rate));

            prometheus_content.push_str(&format!(
                "# HELP ggen_cpu_usage_percent CPU usage percentage\n"
            ));
            prometheus_content.push_str(&format!("# TYPE ggen_cpu_usage_percent gauge\n"));
            prometheus_content.push_str(&format!(
                "ggen_cpu_usage_percent {}\n",
                metrics.cpu_usage_percent
            ));
        }

        tokio::fs::write(output_path, prometheus_content)
            .await
            .map_err(|e| ExecutionError::Io(e))?;

        Ok(())
    }

    /// Export to InfluxDB format
    async fn export_performance_influxdb(
        &self, collector: &MetricsCollector, output_path: &str,
    ) -> Result<(), ExecutionError> {
        let metrics = collector.get_average_metrics(60000); // Last minute

        let mut influx_content = String::new();

        if let Some(metrics) = metrics {
            // InfluxDB line protocol format: measurement,tag_set field_set timestamp
            let fields = format!(
                "execution_duration_ms={},throughput_per_second={},success_rate={},error_rate={},cpu_usage_percent={},memory_usage_mb={}",
                metrics.execution_duration_ms,
                metrics.throughput_per_second,
                metrics.success_rate,
                metrics.error_rate,
                metrics.cpu_usage_percent,
                metrics.memory_usage_mb
            );
            let timestamp = metrics.timestamp.timestamp_nanos_opt().unwrap_or(0);
            influx_content.push_str(&format!(
                "ggen_metrics,timestamp={} {} {}\n",
                metrics.timestamp.timestamp(),
                fields,
                timestamp
            ));
        }

        tokio::fs::write(output_path, influx_content)
            .await
            .map_err(|e| ExecutionError::Io(e))?;

        Ok(())
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metrics_collector_creation() {
        let collector = MetricsCollector::new(1000);
        assert_eq!(collector.metrics_history.len(), 0);
        assert_eq!(collector.aggregations.len(), 0);
    }

    #[test]
    fn test_metrics_recording() {
        let mut collector = MetricsCollector::new(100);

        let metrics = PerformanceMetrics {
            timestamp: Utc::now(),
            execution_duration_ms: 1000,
            throughput_per_second: 10.0,
            success_rate: 0.95,
            error_rate: 0.05,
            resource_usage: ResourceUsage::default(),
            memory_usage_mb: 512,
            cpu_usage_percent: 50.0,
            disk_usage_percent: 30.0,
            network_io_mb: 100,
        };

        collector.record_metrics(metrics);

        assert_eq!(collector.metrics_history.len(), 1);
        assert!(!collector.aggregations.is_empty());
    }

    #[test]
    fn test_aggregations() {
        let mut collector = MetricsCollector::new(100);

        // Record multiple metrics
        for i in 0..10 {
            let metrics = PerformanceMetrics {
                timestamp: Utc::now(),
                execution_duration_ms: i * 100,
                throughput_per_second: i as f64 * 2.0,
                success_rate: 0.9 + (i as f64 * 0.01),
                error_rate: 0.1 - (i as f64 * 0.01),
                resource_usage: ResourceUsage::default(),
                memory_usage_mb: 512,
                cpu_usage_percent: 50.0,
                disk_usage_percent: 30.0,
                network_io_mb: 100,
            };

            collector.record_metrics(metrics);
        }

        // Check aggregations
        let throughput_agg = collector.aggregations.get("throughput").unwrap();
        assert_eq!(throughput_agg.current_average, 9.0); // Average of 0, 2, 4, 6, 8, 10, 12, 14, 16, 18
    }

    #[test]
    fn test_agent_metrics_collector() {
        let mut collector = AgentMetricsCollector::new();

        let metrics = AgentMetrics {
            tasks_completed: 100,
            tasks_failed: 5,
            average_task_duration_ms: 500,
            throughput_tasks_per_second: 2.0,
            error_rate: 0.05,
        };

        collector.update_agent_metrics("agent-1", "Test Agent", metrics);

        let agent_data = collector.get_agent_metrics("agent-1").unwrap();
        assert_eq!(agent_data.tasks_completed, 100);
        assert_eq!(agent_data.tasks_failed, 5);
        assert_eq!(agent_data.error_rate, 0.05);
    }

    #[test]
    fn test_exporter_creation() {
        let exporter = MetricsExporter::new(ExportFormat::Json, true);
        assert_eq!(exporter.format, ExportFormat::Json);
        assert_eq!(exporter.include_metadata, true);
    }
}
