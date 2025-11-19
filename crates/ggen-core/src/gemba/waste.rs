//! Waste Identification and Classification
//!
//! Implements TIMWOOD waste detection for code generation:
//! - Transport: Moving data/information unnecessarily
//! - Inventory: Excess WIP, buffered data
//! - Motion: Unnecessary process steps
//! - Waiting: Idle time, blocked on dependencies
//! - Overproduction: Generating more than needed
//! - Overprocessing: Unnecessary complexity
//! - Defects: Errors requiring rework

use super::*;
use std::collections::BTreeMap;

/// Waste detector for identifying TIMWOOD wastes
pub struct WasteDetector {
    /// Detected wastes
    wastes: Vec<DetectedWaste>,

    /// Waste metrics by type
    metrics: BTreeMap<WasteType, WasteMetrics>,
}

/// A detected waste instance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectedWaste {
    /// Waste type
    pub waste_type: WasteType,

    /// Description of the waste
    pub description: String,

    /// Where the waste was detected
    pub location: String,

    /// Time wasted (ms)
    pub time_wasted_ms: u64,

    /// Severity (1-10)
    pub severity: u8,

    /// Potential root causes
    pub root_causes: Vec<String>,

    /// Suggested countermeasures
    pub countermeasures: Vec<String>,

    /// Timestamp of detection
    pub detected_at: String,
}

/// Metrics for a specific waste type
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct WasteMetrics {
    /// Total occurrences
    pub count: usize,

    /// Total time wasted (ms)
    pub total_time_ms: u64,

    /// Average time per occurrence (ms)
    pub avg_time_ms: u64,

    /// Percentage of total time
    pub percentage: f64,
}

impl WasteDetector {
    /// Create a new waste detector
    pub fn new() -> Self {
        Self {
            wastes: Vec::new(),
            metrics: BTreeMap::new(),
        }
    }

    /// Detect transport waste - unnecessary data movement
    pub fn detect_transport(&mut self, description: String, location: String, time_ms: u64) {
        self.record_waste(
            WasteType::Transport,
            description.clone(),
            location,
            time_ms,
            5,
            vec![
                "Inefficient data pipeline".to_string(),
                "Redundant serialization/deserialization".to_string(),
                "Excessive network calls".to_string(),
            ],
            vec![
                "Cache intermediate results".to_string(),
                "Use in-memory data structures".to_string(),
                "Batch operations".to_string(),
            ],
        );
    }

    /// Detect inventory waste - excess WIP or buffered data
    pub fn detect_inventory(&mut self, description: String, location: String, time_ms: u64) {
        self.record_waste(
            WasteType::Inventory,
            description,
            location,
            time_ms,
            4,
            vec![
                "Batching too many items".to_string(),
                "Lack of streaming".to_string(),
                "Buffering unnecessary data".to_string(),
            ],
            vec![
                "Implement streaming processing".to_string(),
                "Reduce batch sizes".to_string(),
                "Use lazy evaluation".to_string(),
            ],
        );
    }

    /// Detect motion waste - unnecessary process steps
    pub fn detect_motion(&mut self, description: String, location: String, time_ms: u64) {
        self.record_waste(
            WasteType::Motion,
            description,
            location,
            time_ms,
            6,
            vec![
                "Redundant validation".to_string(),
                "Unnecessary transformations".to_string(),
                "Complex control flow".to_string(),
            ],
            vec![
                "Eliminate redundant steps".to_string(),
                "Simplify workflow".to_string(),
                "Combine related operations".to_string(),
            ],
        );
    }

    /// Detect waiting waste - idle time
    pub fn detect_waiting(&mut self, description: String, location: String, time_ms: u64) {
        self.record_waste(
            WasteType::Waiting,
            description,
            location,
            time_ms,
            8,
            vec![
                "Synchronous I/O blocking".to_string(),
                "Sequential processing of parallelizable tasks".to_string(),
                "Network latency".to_string(),
                "Disk I/O bottleneck".to_string(),
            ],
            vec![
                "Use async I/O".to_string(),
                "Parallelize independent operations".to_string(),
                "Implement caching".to_string(),
                "Optimize database queries".to_string(),
            ],
        );
    }

    /// Detect overproduction waste - generating more than needed
    pub fn detect_overproduction(
        &mut self,
        description: String,
        location: String,
        time_ms: u64,
    ) {
        self.record_waste(
            WasteType::Overproduction,
            description,
            location,
            time_ms,
            7,
            vec![
                "Generating unused code".to_string(),
                "Creating unnecessary artifacts".to_string(),
                "Premature optimization".to_string(),
            ],
            vec![
                "Generate on-demand only".to_string(),
                "Use lazy generation".to_string(),
                "Follow YAGNI principle".to_string(),
            ],
        );
    }

    /// Detect overprocessing waste - unnecessary complexity
    pub fn detect_overprocessing(
        &mut self,
        description: String,
        location: String,
        time_ms: u64,
    ) {
        self.record_waste(
            WasteType::Overprocessing,
            description,
            location,
            time_ms,
            6,
            vec![
                "Over-engineered solution".to_string(),
                "Unnecessary abstraction layers".to_string(),
                "Excessive error handling".to_string(),
            ],
            vec![
                "Simplify design".to_string(),
                "Remove unnecessary abstractions".to_string(),
                "Apply KISS principle".to_string(),
            ],
        );
    }

    /// Detect defect waste - errors requiring rework
    pub fn detect_defect(&mut self, description: String, location: String, time_ms: u64) {
        self.record_waste(
            WasteType::Defects,
            description,
            location,
            time_ms,
            9,
            vec![
                "Insufficient validation".to_string(),
                "Type mismatches".to_string(),
                "Logic errors".to_string(),
                "Missing error handling".to_string(),
            ],
            vec![
                "Implement poka-yoke (error prevention)".to_string(),
                "Add automated testing".to_string(),
                "Use type system to prevent errors".to_string(),
                "Validate inputs early".to_string(),
            ],
        );
    }

    /// Record a waste instance
    fn record_waste(
        &mut self,
        waste_type: WasteType,
        description: String,
        location: String,
        time_ms: u64,
        severity: u8,
        root_causes: Vec<String>,
        countermeasures: Vec<String>,
    ) {
        let waste = DetectedWaste {
            waste_type: waste_type.clone(),
            description,
            location,
            time_wasted_ms: time_ms,
            severity,
            root_causes,
            countermeasures,
            detected_at: chrono::Utc::now().to_rfc3339(),
        };

        // Update metrics
        let metrics = self.metrics.entry(waste_type).or_insert_with(Default::default);
        metrics.count += 1;
        metrics.total_time_ms += time_ms;
        metrics.avg_time_ms = metrics.total_time_ms / metrics.count as u64;

        self.wastes.push(waste);
    }

    /// Get all detected wastes
    pub fn get_wastes(&self) -> &[DetectedWaste] {
        &self.wastes
    }

    /// Get waste metrics by type
    pub fn get_metrics(&self) -> &BTreeMap<WasteType, WasteMetrics> {
        &self.metrics
    }

    /// Calculate total waste time
    pub fn total_waste_time_ms(&self) -> u64 {
        self.wastes.iter().map(|w| w.time_wasted_ms).sum()
    }

    /// Get top wastes by time
    pub fn top_wastes_by_time(&self, limit: usize) -> Vec<&DetectedWaste> {
        let mut wastes: Vec<_> = self.wastes.iter().collect();
        wastes.sort_by(|a, b| b.time_wasted_ms.cmp(&a.time_wasted_ms));
        wastes.into_iter().take(limit).collect()
    }

    /// Get top wastes by severity
    pub fn top_wastes_by_severity(&self, limit: usize) -> Vec<&DetectedWaste> {
        let mut wastes: Vec<_> = self.wastes.iter().collect();
        wastes.sort_by(|a, b| b.severity.cmp(&a.severity));
        wastes.into_iter().take(limit).collect()
    }

    /// Generate waste report
    pub fn generate_report(&self, total_time_ms: u64) -> WasteReport {
        // Calculate percentages
        let mut metrics_with_percentage = self.metrics.clone();
        for (_, metrics) in &mut metrics_with_percentage {
            metrics.percentage = if total_time_ms > 0 {
                (metrics.total_time_ms as f64 / total_time_ms as f64) * 100.0
            } else {
                0.0
            };
        }

        // Get top wastes
        let top_by_time = self
            .top_wastes_by_time(10)
            .into_iter()
            .cloned()
            .collect();

        let top_by_severity = self
            .top_wastes_by_severity(10)
            .into_iter()
            .cloned()
            .collect();

        // Calculate total waste percentage
        let total_waste_percentage = if total_time_ms > 0 {
            (self.total_waste_time_ms() as f64 / total_time_ms as f64) * 100.0
        } else {
            0.0
        };

        WasteReport {
            total_wastes: self.wastes.len(),
            total_waste_time_ms: self.total_waste_time_ms(),
            total_waste_percentage,
            metrics_by_type: metrics_with_percentage,
            top_wastes_by_time,
            top_wastes_by_severity,
            recommendations: self.generate_recommendations(),
        }
    }

    /// Generate recommendations based on detected wastes
    fn generate_recommendations(&self) -> Vec<String> {
        let mut recs = Vec::new();

        // Check for high waiting time
        if let Some(waiting_metrics) = self.metrics.get(&WasteType::Waiting) {
            if waiting_metrics.percentage > 20.0 {
                recs.push(
                    "Critical: Over 20% time spent waiting. Implement async I/O and parallelization.".to_string(),
                );
            }
        }

        // Check for defects
        if let Some(defect_metrics) = self.metrics.get(&WasteType::Defects) {
            if defect_metrics.count > 5 {
                recs.push(
                    "High defect count detected. Implement poka-yoke and validation.".to_string(),
                );
            }
        }

        // Check for overproduction
        if let Some(overprod_metrics) = self.metrics.get(&WasteType::Overproduction) {
            if overprod_metrics.count > 0 {
                recs.push(
                    "Overproduction detected. Generate only what's needed, when it's needed."
                        .to_string(),
                );
            }
        }

        // General recommendations
        if recs.is_empty() {
            recs.push("Continue monitoring for waste patterns.".to_string());
        }

        recs
    }
}

impl Default for WasteDetector {
    fn default() -> Self {
        Self::new()
    }
}

/// Comprehensive waste report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WasteReport {
    /// Total number of wastes detected
    pub total_wastes: usize,

    /// Total time wasted (ms)
    pub total_waste_time_ms: u64,

    /// Percentage of total time that is waste
    pub total_waste_percentage: f64,

    /// Metrics by waste type
    pub metrics_by_type: BTreeMap<WasteType, WasteMetrics>,

    /// Top wastes by time wasted
    pub top_wastes_by_time: Vec<DetectedWaste>,

    /// Top wastes by severity
    pub top_wastes_by_severity: Vec<DetectedWaste>,

    /// Recommendations for improvement
    pub recommendations: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_waste_detector() {
        let mut detector = WasteDetector::new();

        detector.detect_waiting(
            "Waiting for database query".to_string(),
            "SPARQL Execution".to_string(),
            100,
        );

        detector.detect_defect(
            "Type mismatch in template".to_string(),
            "Template Rendering".to_string(),
            50,
        );

        assert_eq!(detector.get_wastes().len(), 2);
        assert_eq!(detector.total_waste_time_ms(), 150);
    }

    #[test]
    fn test_waste_report() {
        let mut detector = WasteDetector::new();

        detector.detect_waiting("Test".to_string(), "Test".to_string(), 100);

        let report = detector.generate_report(1000);

        assert_eq!(report.total_wastes, 1);
        assert_eq!(report.total_waste_time_ms, 100);
        assert!((report.total_waste_percentage - 10.0).abs() < 0.1);
    }

    #[test]
    fn test_top_wastes() {
        let mut detector = WasteDetector::new();

        detector.detect_waiting("Low".to_string(), "Test".to_string(), 10);
        detector.detect_waiting("High".to_string(), "Test".to_string(), 100);
        detector.detect_waiting("Medium".to_string(), "Test".to_string(), 50);

        let top = detector.top_wastes_by_time(2);

        assert_eq!(top.len(), 2);
        assert_eq!(top[0].time_wasted_ms, 100);
        assert_eq!(top[1].time_wasted_ms, 50);
    }
}
