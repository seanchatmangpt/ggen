//! Toyota Production System (TPS) Patterns Integration
//!
//! Implements the core TPS patterns: Jidoka, JIT, Kaizen, Genchi Genbutsu

use serde_json::Value;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tokio::time::sleep;
use tracing::{debug, info, warn};

use crate::{OSIRISSignal, SignalLevel};

/// TPS execution modes
#[derive(Debug, Clone, PartialEq)]
pub enum TPSMode {
    /// Standard mode with balanced optimization
    Standard,
    /// Just-in-Time execution mode
    JIT,
    /// Full TPS integration mode
    Full,
}

/// TPS Metrics collection and management
#[derive(Debug, Clone)]
pub struct TPSMetrics {
    pub tps_score: f64,
    pub quality_threshold: f64,
    pub improvement_rate: f64,
    pub execution_times: Vec<Duration>,
    pub error_counts: HashMap<String, u64>,
    pub last_update: Instant,
}

impl Default for TPSMetrics {
    fn default() -> Self {
        Self {
            tps_score: 0.0,
            quality_threshold: 0.95,
            improvement_rate: 0.0,
            execution_times: Vec::new(),
            error_counts: HashMap::new(),
            last_update: Instant::now(),
        }
    }
}

impl TPSMetrics {
    /// Update execution time metrics
    pub fn update_execution_time(&mut self, time_ms: u64) {
        let duration = Duration::from_millis(time_ms);
        self.execution_times.push(duration);

        // Keep only last 100 executions
        if self.execution_times.len() > 100 {
            self.execution_times.remove(0);
        }

        // Calculate TPS score
        if let Some(avg_time) = self.average_execution_time() {
            self.tps_score = if avg_time.as_millis() > 0 {
                1000.0 / avg_time.as_millis() as f64
            } else {
                0.0
            };
        }

        self.last_update = Instant::now();
    }

    /// Update error count
    pub fn record_error(&mut self, error_type: &str) {
        *self.error_counts.entry(error_type.to_string()).or_insert(0) += 1;
    }

    /// Get average execution time
    pub fn average_execution_time(&self) -> Option<Duration> {
        if self.execution_times.is_empty() {
            return None;
        }

        let total: Duration = self.execution_times.iter().sum();
        Some(total / self.execution_times.len() as u32)
    }

    /// Calculate improvement rate
    pub fn calculate_improvement_rate(&mut self) {
        if self.execution_times.len() < 10 {
            self.improvement_rate = 0.0;
            return;
        }

        // Compare first half with second half
        let mid = self.execution_times.len() / 2;
        let first_half: Vec<_> = self.execution_times.iter().take(mid).collect();
        let second_half: Vec<_> = self.execution_times.iter().skip(mid).collect();

        let first_avg: Duration = first_half.iter().sum();
        let second_avg: Duration = second_half.iter().sum();

        let first_avg = first_avg.as_millis() as f64 / mid as f64;
        let second_avg = second_avg.as_millis() as f64 / mid as f64;

        if first_avg > 0.0 {
            let improvement = (first_avg - second_avg) / first_avg;
            self.improvement_rate = improvement.max(0.0);
        }
    }

    /// Check if metrics meet SLOs
    pub fn check_slos(&self) -> bool {
        if self.tps_score < 10.0 {
            return false;
        }

        if self.quality_threshold < 0.95 {
            return false;
        }

        if self.improvement_rate < -0.1 {
            return false;
        }

        true
    }
}

/// Jidoka (Automation with Human Intelligence) implementation
pub struct Jidoka {
    quality_threshold: f64,
    defect_detection_enabled: bool,
}

impl Jidoka {
    pub fn new(quality_threshold: f64) -> Self {
        Self {
            quality_threshold,
            defect_detection_enabled: true,
        }
    }

    /// Validate quality before execution
    pub async fn validate_quality(&self, input: &Value) -> Result<(), String> {
        if !self.defect_detection_enabled {
            return Ok(());
        }

        // Quality validation logic
        if input.is_null() {
            return Err("Null input detected".to_string());
        }

        // Check required fields
        if let Some(obj) = input.as_object() {
            if !obj.contains_key("id") {
                return Err("Missing required field: id".to_string());
            }
            if !obj.contains_key("data") {
                return Err("Missing required field: data".to_string());
            }
        }

        Ok(())
    }

    /// Handle defect detection
    pub async fn handle_defect(&mut self, workflow: &mut Workflow, error: &str) -> Result<(), String> {
        warn!("Jidoka detected defect: {}", error);

        // Stop workflow execution
        workflow.status = WorkflowStatus::Error(error.to_string());

        // Trigger quality alert
        let signal = OSIRISSignal::new(
            "defect_detected",
            format!("Jidoka defect: {}", error),
            SignalLevel::Warning,
        );

        // In full implementation, emit signal
        info!("Jidoka defect signal: {}", signal.message);

        Ok(())
    }
}

/// Just-in-Time execution
pub struct JIT {
    execution_timeout_ms: u64,
    flow_optimization_enabled: bool,
}

impl JIT {
    pub fn new(timeout_ms: u64) -> Self {
        Self {
            execution_timeout_ms: timeout_ms,
            flow_optimization_enabled: true,
        }
    }

    /// Execute with timeout optimization
    pub async fn execute_optimized<F, T>(&self, task: F) -> Result<T, String>
    where
        F: std::future::Future<Output = T>,
    {
        if !self.flow_optimization_enabled {
            // Execute without timeout
            return Ok(task.await);
        }

        // Execute with timeout
        match tokio::time::timeout(
            Duration::from_millis(self.execution_timeout_ms),
            task,
        )
        .await
        {
            Ok(result) => Ok(result),
            Err(_) => Err("JIT execution timeout".to_string()),
        }
    }

    /// Optimize flow timing
    pub async fn optimize_flow(&self, workflows: &mut Vec<Workflow>) {
        debug!("Optimizing JIT flow for {} workflows", workflows.len());

        // Sort workflows by priority/urgency
        workflows.sort_by(|a, b| {
            a.priority.cmp(&b.priority)
        });

        // Parallelize independent workflows
        let mut futures = Vec::new();
        for workflow in workflows {
            if workflow.status == WorkflowStatus::Pending {
                futures.push(self.execute_workflow(workflow));
            }
        }

        // Execute in parallel
        let results = futures::future::join_all(futures).await;

        // Handle results
        for result in results {
            match result {
                Ok(_) => debug!("Workflow optimized successfully"),
                Err(e) => warn!("Workflow optimization failed: {}", e),
            }
        }
    }

    async fn execute_workflow(&self, workflow: &mut Workflow) -> Result<(), String> {
        workflow.status = WorkflowStatus::InProgress;

        // Simulate execution
        sleep(Duration::from_millis(50)).await;

        workflow.status = WorkflowStatus::Completed;
        Ok(())
    }
}

/// Kaizen (Continuous Improvement) implementation
pub struct Kaizen {
    improvement_opportunities: Vec<KaizenOpportunity>,
    learning_rate: f64,
}

#[derive(Debug, Clone)]
pub struct KaizenOpportunity {
    pub id: String,
    pub description: String,
    pub impact: f64,
    pub effort: f64,
    pub category: String,
}

impl Kaizen {
    pub fn new() -> Self {
        Self {
            improvement_opportunities: Vec::new(),
            learning_rate: 0.1,
        }
    }

    /// Analyze workflows for improvement opportunities
    pub async fn analyze_improvements(&self, workflows: &HashMap<String, Workflow>) -> Vec<KaizenOpportunity> {
        let mut opportunities = Vec::new();

        for (id, workflow) in workflows {
            // Check for long execution times
            if let Some(avg_time) = workflow.get_average_execution_time() {
                if avg_time > Duration::from_millis(200) {
                    opportunities.push(KaizenOpportunity {
                        id: format!("speed_{}", id),
                        description: format!("Workflow {} execution is slow (avg: {:.2}ms)", id, avg_time.as_millis()),
                        impact: 0.8,
                        effort: 0.3,
                        category: "performance".to_string(),
                    });
                }
            }

            // Check for high error rates
            if workflow.error_rate > 0.05 {
                opportunities.push(KaizenOpportunity {
                    id: format!("quality_{}", id),
                    description: format!("Workflow {} has high error rate ({}%)", id, (workflow.error_rate * 100.0)),
                    impact: 0.9,
                    effort: 0.5,
                    category: "quality".to_string(),
                });
            }
        }

        opportunities
    }

    /// Apply improvements to workflows
    pub async fn apply_improvements(&mut self, workflows: &mut HashMap<String, Workflow>, opportunities: &[KaizenOpportunity]) {
        for opportunity in opportunities {
            if opportunity.impact > opportunity.effort {
                // Apply improvement
                if let Some(workflow) = workflows.get_mut(&opportunity.id) {
                    workflow.apply_improvement(opportunity).await;
                    info!("Applied improvement: {}", opportunity.description);
                }
            }
        }
    }
}

/// Genchi Genbutsu (Go and See for Yourself) implementation
pub struct GenchiGenbutsu {
    verification_enabled: bool,
    direct_observation_required: bool,
}

impl GenchiGenbutsu {
    pub fn new() -> Self {
        Self {
            verification_enabled: true,
            direct_observation_required: true,
        }
    }

    /// Verify workflow state at source
    pub async fn verify_at_source(&self, workflow: &Workflow) -> Result<VerificationResult, String> {
        if !self.verification_enabled {
            return Ok(VerificationResult {
                is_verified: true,
                source: "verification_disabled".to_string(),
                confidence: 1.0,
            });
        }

        // Simulate direct observation
        if self.direct_observation_required {
            // In real implementation, this would connect to actual sources
            let verified = workflow.validate_directly().await?;

            Ok(VerificationResult {
                is_verified: verified,
                source: "direct_observation".to_string(),
                confidence: 1.0,
            })
        } else {
            // Use indirect verification
            let confidence = workflow.calculate_confidence();

            Ok(VerificationResult {
                is_verified: confidence > 0.9,
                source: "indirect_verification".to_string(),
                confidence,
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct VerificationResult {
    pub is_verified: bool,
    pub source: String,
    pub confidence: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_tps_metrics() {
        let mut metrics = TPSMetrics::default();

        metrics.update_execution_time(100);
        metrics.update_execution_time(150);
        metrics.update_execution_time(120);

        assert!(metrics.tps_score > 0.0);
        assert_eq!(metrics.execution_times.len(), 3);

        let avg_time = metrics.average_execution_time().unwrap();
        assert!(avg_time > Duration::from_millis(100) && avg_time < Duration::from_millis(200));
    }

    #[test]
    fn test_jidoka_validation() {
        let jidoka = Jidoka::new(0.95);

        let valid_input = serde_json::json!({
            "id": "test-123",
            "data": "test data"
        });

        let invalid_input = serde_json::json!(null);

        assert!(jidoka.validate_quality(&valid_input).await.is_ok());
        assert!(jidoka.validate_quality(&invalid_input).await.is_err());
    }

    #[tokio::test]
    async fn test_jit_execution() {
        let jit = JIT::new(1000);

        async fn simple_task() -> String {
            "completed".to_string()
        }

        let result = jit.execute_optimized(simple_task()).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "completed");
    }

    #[tokio::test]
    async fn test_kaizen_analysis() {
        let mut workflows = std::collections::HashMap::new();

        let mut slow_workflow = Workflow::new("slow-workflow");
        slow_workflow.execution_times.push(Duration::from_millis(300));
        slow_workflow.error_rate = 0.06;

        workflows.insert("slow-workflow".to_string(), slow_workflow);

        let kaizen = Kaizen::new();
        let opportunities = kaizen.analyze_improvements(&workflows).await;

        assert!(!opportunities.is_empty());
    }

    #[tokio::test]
    async fn test_genchi_genbutsu_verification() {
        let genchi = GenchiGenbutsu::new();

        let mut workflow = Workflow::new("test-workflow");
        workflow.verification_passed = true;

        let result = genchi.verify_at_source(&workflow).await;
        assert!(result.is_ok());
    }
}