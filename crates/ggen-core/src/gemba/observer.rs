//! Gemba Observer - Real-time Observation at Generation Site
//!
//! Coordinates all Gemba Walk components to provide real-time
//! observability during code generation.

use super::*;
use super::value_stream::ValueStreamMapper;
use super::waste::WasteDetector;
use super::root_cause::RootCauseAnalyzer;
use super::kaizen::KaizenTracker;
use super::feedback::FeedbackLoop;
use std::sync::{Arc, Mutex};
use std::time::Instant;

/// Gemba Observer - observes code generation in real-time
pub struct GembaObserver {
    /// Session ID
    session_id: String,

    /// Value stream mapper
    value_stream: Arc<Mutex<ValueStreamMapper>>,

    /// Waste detector
    waste_detector: Arc<Mutex<WasteDetector>>,

    /// Kaizen tracker
    kaizen: Arc<Mutex<KaizenTracker>>,

    /// Feedback loop
    feedback: Arc<Mutex<FeedbackLoop>>,

    /// Current observations
    observations: Vec<GembaObservation>,

    /// Session start time
    session_start: Instant,

    /// Session metrics
    session_metrics: SessionMetrics,

    /// Configuration
    config: ObserverConfig,
}

/// Observer configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ObserverConfig {
    /// Enable value stream mapping
    pub enable_value_stream: bool,

    /// Enable waste detection
    pub enable_waste_detection: bool,

    /// Enable root cause analysis
    pub enable_root_cause: bool,

    /// Enable continuous improvement tracking
    pub enable_kaizen: bool,

    /// Enable feedback collection
    pub enable_feedback: bool,

    /// Minimum duration to track (ms)
    pub min_duration_ms: u64,

    /// Auto-identify bottlenecks
    pub auto_identify_bottlenecks: bool,

    /// Threshold for bottleneck identification (percentile)
    pub bottleneck_threshold: f64,
}

impl Default for ObserverConfig {
    fn default() -> Self {
        Self {
            enable_value_stream: true,
            enable_waste_detection: true,
            enable_root_cause: true,
            enable_kaizen: true,
            enable_feedback: true,
            min_duration_ms: 10,
            auto_identify_bottlenecks: true,
            bottleneck_threshold: 0.8,
        }
    }
}

impl GembaObserver {
    /// Create new Gemba observer
    pub fn new(session_id: String, config: ObserverConfig) -> Self {
        Self {
            session_id,
            value_stream: Arc::new(Mutex::new(ValueStreamMapper::new())),
            waste_detector: Arc::new(Mutex::new(WasteDetector::new())),
            kaizen: Arc::new(Mutex::new(KaizenTracker::new())),
            feedback: Arc::new(Mutex::new(FeedbackLoop::new())),
            observations: Vec::new(),
            session_start: Instant::now(),
            session_metrics: SessionMetrics::default(),
            config,
        }
    }

    /// Observe a generation activity
    pub fn observe(
        &mut self,
        location: GenerationSite,
        activity: Activity,
        value_type: ValueType,
        duration: Duration,
        developer_context: DeveloperContext,
    ) -> GembaObservation {
        let observation = GembaObservation {
            id: format!("obs-{}", uuid::Uuid::new_v4()),
            timestamp: chrono::Utc::now().to_rfc3339(),
            location: location.clone(),
            activity: activity.clone(),
            value_type: value_type.clone(),
            waste_type: None,
            duration,
            developer_context: developer_context.clone(),
            metrics: GembaMetrics::default(),
        };

        // Update value stream
        if self.config.enable_value_stream {
            if let Ok(mut mapper) = self.value_stream.lock() {
                mapper.start_stage(
                    activity.name.clone(),
                    location.phase.clone(),
                    value_type.clone(),
                );
                mapper.add_activity(activity.description.clone());
            }
        }

        // Detect waste
        if self.config.enable_waste_detection && value_type == ValueType::Waste {
            self.detect_and_classify_waste(&observation);
        }

        // Collect developer feedback if there are pain points
        if self.config.enable_feedback && !developer_context.pain_points.is_empty() {
            if let Ok(mut feedback_loop) = self.feedback.lock() {
                for pain_point in &developer_context.pain_points {
                    feedback_loop.add_pain_point(pain_point.clone());
                }
            }
        }

        self.observations.push(observation.clone());
        observation
    }

    /// Start observing a pipeline stage
    pub fn start_stage(&mut self, name: String, phase: PipelinePhase, value_type: ValueType) {
        if self.config.enable_value_stream {
            if let Ok(mut mapper) = self.value_stream.lock() {
                mapper.start_stage(name, phase, value_type);
            }
        }
    }

    /// Add activity to current stage
    pub fn add_activity(&mut self, activity: String) {
        if self.config.enable_value_stream {
            if let Ok(mut mapper) = self.value_stream.lock() {
                mapper.add_activity(activity);
            }
        }
    }

    /// Record a defect
    pub fn record_defect(&mut self, description: String, location: String) {
        if self.config.enable_waste_detection {
            if let Ok(mut detector) = self.waste_detector.lock() {
                detector.detect_defect(description, location, 0);
            }
        }

        if self.config.enable_value_stream {
            if let Ok(mut mapper) = self.value_stream.lock() {
                mapper.record_defect();
            }
        }
    }

    /// Record waiting time
    pub fn record_waiting(&mut self, description: String, location: String, duration_ms: u64) {
        if self.config.enable_waste_detection && duration_ms >= self.config.min_duration_ms {
            if let Ok(mut detector) = self.waste_detector.lock() {
                detector.detect_waiting(description, location, duration_ms);
            }
        }
    }

    /// Detect and classify waste
    fn detect_and_classify_waste(&self, observation: &GembaObservation) {
        if let Ok(mut detector) = self.waste_detector.lock() {
            let time_ms = observation.duration.as_millis() as u64;
            let location = format!("{:?}", observation.location.phase);

            // Classify based on activity description
            let desc_lower = observation.activity.description.to_lowercase();

            if desc_lower.contains("wait") || desc_lower.contains("block") {
                detector.detect_waiting(
                    observation.activity.description.clone(),
                    location,
                    time_ms,
                );
            } else if desc_lower.contains("error") || desc_lower.contains("fail") {
                detector.detect_defect(
                    observation.activity.description.clone(),
                    location,
                    time_ms,
                );
            } else if desc_lower.contains("unnecessary") || desc_lower.contains("redundant") {
                detector.detect_motion(
                    observation.activity.description.clone(),
                    location,
                    time_ms,
                );
            }
        }
    }

    /// Complete the observation session
    pub fn complete_session(mut self) -> GembaSession {
        let ended_at = chrono::Utc::now().to_rfc3339();

        // Build value stream map
        let value_stream_map = if self.config.enable_value_stream {
            if let Ok(mapper) = Arc::try_unwrap(self.value_stream) {
                let mut mapper = mapper.into_inner().unwrap();
                if self.config.auto_identify_bottlenecks {
                    mapper.identify_bottlenecks();
                }
                Some(mapper.build(
                    self.session_id.clone(),
                    "Generation Session".to_string(),
                ))
            } else {
                None
            }
        } else {
            None
        };

        // Calculate session metrics
        let total_time_ms = self.session_start.elapsed().as_millis() as u64;

        if let Some(ref vsm) = value_stream_map {
            self.session_metrics.total_time_ms = total_time_ms;
            self.session_metrics.value_added_ratio = vsm.value_added_ratio;
            self.session_metrics.flow_efficiency = vsm.flow_efficiency;
        }

        // Get waste report
        if self.config.enable_waste_detection {
            if let Ok(detector) = Arc::try_unwrap(self.waste_detector) {
                let detector = detector.into_inner().unwrap();
                let waste_report = detector.generate_report(total_time_ms);

                self.session_metrics.waste_breakdown = waste_report.metrics_by_type
                    .into_iter()
                    .map(|(k, v)| (k, v.total_time_ms))
                    .collect();

                // Calculate quality score based on defects
                let defect_time = self.session_metrics.waste_breakdown
                    .get(&WasteType::Defects)
                    .copied()
                    .unwrap_or(0);

                self.session_metrics.quality_score = if total_time_ms > 0 {
                    1.0 - (defect_time as f64 / total_time_ms as f64)
                } else {
                    1.0
                };
            }
        }

        // Identify improvement opportunities
        let improvements = self.identify_improvements(&value_stream_map);

        GembaSession {
            session_id: self.session_id,
            started_at: chrono::Utc::now().to_rfc3339(), // Would be better to store this
            ended_at: Some(ended_at),
            observations: self.observations,
            session_metrics: self.session_metrics,
            improvements,
        }
    }

    /// Identify improvement opportunities from observations
    fn identify_improvements(
        &self,
        value_stream_map: &Option<value_stream::ValueStreamMap>,
    ) -> Vec<ImprovementOpportunity> {
        let mut improvements = Vec::new();

        // Check for bottlenecks
        if let Some(vsm) = value_stream_map {
            for stage in &vsm.stages {
                if stage.is_bottleneck {
                    improvements.push(ImprovementOpportunity {
                        id: format!("imp-{}", uuid::Uuid::new_v4()),
                        title: format!("Optimize bottleneck: {}", stage.name),
                        description: format!(
                            "Stage '{}' is a bottleneck, taking {} ms",
                            stage.name, stage.processing_time_ms
                        ),
                        improvement_type: ImprovementType::Simplify,
                        addresses_waste: Some(WasteType::Waiting),
                        expected_impact: 8,
                        effort_required: 5,
                        priority_score: 8.0 / 5.0,
                        status: ImprovementStatus::Identified,
                    });
                }
            }

            // Check for low flow efficiency
            if vsm.flow_efficiency < 0.3 {
                improvements.push(ImprovementOpportunity {
                    id: format!("imp-{}", uuid::Uuid::new_v4()),
                    title: "Improve flow efficiency".to_string(),
                    description: format!(
                        "Flow efficiency is {:.1}%, target is >30%",
                        vsm.flow_efficiency * 100.0
                    ),
                    improvement_type: ImprovementType::Eliminate,
                    addresses_waste: Some(WasteType::Waiting),
                    expected_impact: 9,
                    effort_required: 6,
                    priority_score: 9.0 / 6.0,
                    status: ImprovementStatus::Identified,
                });
            }
        }

        improvements
    }

    /// Get current session metrics
    pub fn get_current_metrics(&self) -> &SessionMetrics {
        &self.session_metrics
    }

    /// Get feedback loop
    pub fn get_feedback_loop(&self) -> Arc<Mutex<FeedbackLoop>> {
        Arc::clone(&self.feedback)
    }

    /// Get kaizen tracker
    pub fn get_kaizen_tracker(&self) -> Arc<Mutex<KaizenTracker>> {
        Arc::clone(&self.kaizen)
    }
}

/// Builder for GembaObserver
pub struct GembaObserverBuilder {
    session_id: Option<String>,
    config: ObserverConfig,
}

impl GembaObserverBuilder {
    /// Create new builder
    pub fn new() -> Self {
        Self {
            session_id: None,
            config: ObserverConfig::default(),
        }
    }

    /// Set session ID
    pub fn session_id(mut self, id: String) -> Self {
        self.session_id = Some(id);
        self
    }

    /// Set configuration
    pub fn config(mut self, config: ObserverConfig) -> Self {
        self.config = config;
        self
    }

    /// Enable/disable value stream mapping
    pub fn enable_value_stream(mut self, enable: bool) -> Self {
        self.config.enable_value_stream = enable;
        self
    }

    /// Enable/disable waste detection
    pub fn enable_waste_detection(mut self, enable: bool) -> Self {
        self.config.enable_waste_detection = enable;
        self
    }

    /// Build the observer
    pub fn build(self) -> GembaObserver {
        let session_id = self.session_id.unwrap_or_else(|| {
            format!("gemba-{}", uuid::Uuid::new_v4())
        });

        GembaObserver::new(session_id, self.config)
    }
}

impl Default for GembaObserverBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gemba_observer() {
        let mut observer = GembaObserverBuilder::new()
            .session_id("test-session".to_string())
            .build();

        observer.start_stage(
            "Template Loading".to_string(),
            PipelinePhase::TemplateLoading,
            ValueType::NecessaryNonValueAdding,
        );

        observer.add_activity("Parse YAML".to_string());

        std::thread::sleep(Duration::from_millis(10));

        observer.start_stage(
            "SPARQL Execution".to_string(),
            PipelinePhase::SparqlExecution,
            ValueType::ValueAdding,
        );

        let session = observer.complete_session();

        assert_eq!(session.session_id, "test-session");
        assert!(session.session_metrics.total_time_ms > 0);
    }

    #[test]
    fn test_observer_builder() {
        let observer = GembaObserverBuilder::new()
            .enable_value_stream(true)
            .enable_waste_detection(false)
            .build();

        assert!(observer.config.enable_value_stream);
        assert!(!observer.config.enable_waste_detection);
    }
}
