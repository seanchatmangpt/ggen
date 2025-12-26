//! Statistical Process Control (SPC) charts for ontology generation consistency
//!
//! SPC charts monitor process variation over time to detect special causes
//! and maintain process stability. This module implements various control
//! charts for RDF generation quality monitoring.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};

/// Statistical Process Control chart
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpcChart {
    pub id: String,
    pub chart_type: ChartType,
    pub metric_name: String,
    pub data_points: Vec<DataPoint>,
    pub control_limits: ControlLimits,
    pub specification_limits: Option<SpecificationLimits>,
    pub violations: Vec<ControlViolation>,
    pub created_at: chrono::DateTime<chrono::Utc>,
}

/// Type of control chart
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ChartType {
    /// Individual-Moving Range chart for individual measurements
    IMR,
    /// X-bar and R chart for subgroup averages and ranges
    XBarR,
    /// X-bar and S chart for subgroup averages and standard deviations
    XBarS,
    /// p-chart for proportion defective
    P,
    /// np-chart for number defective
    NP,
    /// c-chart for count of defects
    C,
    /// u-chart for defects per unit
    U,
    /// EWMA - Exponentially Weighted Moving Average
    EWMA,
    /// CUSUM - Cumulative Sum
    CUSUM,
}

/// Data point in control chart
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DataPoint {
    pub sequence: u64,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub value: f64,
    pub subgroup_id: Option<String>,
    pub sample_size: Option<u64>,
    pub is_out_of_control: bool,
    pub violations: Vec<String>,
}

/// Control limits for process monitoring
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlLimits {
    /// Center line (process mean)
    pub center_line: f64,
    /// Upper Control Limit (UCL) - typically mean + 3σ
    pub ucl: f64,
    /// Lower Control Limit (LCL) - typically mean - 3σ
    pub lcl: f64,
    /// Upper Warning Limit (UWL) - typically mean + 2σ
    pub uwl: Option<f64>,
    /// Lower Warning Limit (LWL) - typically mean - 2σ
    pub lwl: Option<f64>,
    /// Process standard deviation
    pub sigma: f64,
    /// Number of sigma for control limits (usually 3)
    pub sigma_level: f64,
}

/// Specification limits from customer or design requirements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpecificationLimits {
    pub upper: Option<f64>,
    pub lower: Option<f64>,
    pub target: f64,
}

/// Control chart violation (out-of-control condition)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlViolation {
    pub violation_type: ViolationType,
    pub sequence: u64,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub description: String,
    pub severity: ViolationSeverity,
}

/// Types of control chart violations (Western Electric Rules)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum ViolationType {
    /// Rule 1: One point beyond 3σ from centerline
    BeyondControlLimits,
    /// Rule 2: Nine points in a row on same side of centerline
    NinePointsOneSide,
    /// Rule 3: Six points in a row steadily increasing or decreasing
    SixPointsTrend,
    /// Rule 4: Fourteen points in a row alternating up and down
    FourteenPointsAlternating,
    /// Rule 5: Two out of three points beyond 2σ on same side
    TwoOfThreeBeyondTwoSigma,
    /// Rule 6: Four out of five points beyond 1σ on same side
    FourOfFiveBeyondOneSigma,
    /// Rule 7: Fifteen points in a row within 1σ of centerline (both sides)
    FifteenPointsWithinOneSigma,
    /// Rule 8: Eight points in a row beyond 1σ from centerline (either side)
    EightPointsBeyondOneSigma,
}

/// Severity of violation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ViolationSeverity {
    Critical = 3,  // Beyond control limits
    Warning = 2,   // Pattern violations
    Info = 1,      // Trending toward limits
}

/// Process metric being monitored
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessMetric {
    pub name: String,
    pub description: String,
    pub unit: String,
    pub metric_type: MetricType,
}

/// Type of process metric
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum MetricType {
    /// Continuous variable (measurements)
    Variable,
    /// Discrete attribute (counts, proportions)
    Attribute,
}

/// Control chart implementation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ControlChart {
    charts: Vec<SpcChart>,
}

impl ControlChart {
    /// Create new control chart manager
    pub fn new() -> Self {
        Self { charts: Vec::new() }
    }

    /// Create I-MR chart for individual measurements
    pub fn create_imr_chart(
        &mut self,
        id: String,
        metric_name: String,
        baseline_data: &[f64],
    ) -> Result<&SpcChart> {
        if baseline_data.len() < 20 {
            return Err(Error::new(
                "Need at least 20 baseline points for I-MR chart",
            ));
        }

        // Calculate process mean
        let mean = baseline_data.iter().sum::<f64>() / baseline_data.len() as f64;

        // Calculate moving ranges
        let moving_ranges: Vec<f64> = baseline_data
            .windows(2)
            .map(|w| (w[1] - w[0]).abs())
            .collect();

        // Average moving range
        let mr_bar = moving_ranges.iter().sum::<f64>() / moving_ranges.len() as f64;

        // Process sigma (using d2 constant for n=2)
        let d2 = 1.128;
        let sigma = mr_bar / d2;

        // Control limits (3σ)
        let ucl = mean + 3.0 * sigma;
        let lcl = mean - 3.0 * sigma;
        let uwl = mean + 2.0 * sigma;
        let lwl = mean - 2.0 * sigma;

        let control_limits = ControlLimits {
            center_line: mean,
            ucl,
            lcl,
            uwl: Some(uwl),
            lwl: Some(lwl),
            sigma,
            sigma_level: 3.0,
        };

        // Create data points
        let now = chrono::Utc::now();
        let data_points: Vec<DataPoint> = baseline_data
            .iter()
            .enumerate()
            .map(|(i, &value)| DataPoint {
                sequence: i as u64,
                timestamp: now,
                value,
                subgroup_id: None,
                sample_size: Some(1),
                is_out_of_control: value > ucl || value < lcl,
                violations: Vec::new(),
            })
            .collect();

        let chart = SpcChart {
            id,
            chart_type: ChartType::IMR,
            metric_name,
            data_points,
            control_limits,
            specification_limits: None,
            violations: Vec::new(),
            created_at: now,
        };

        self.charts.push(chart);
        Ok(self.charts.last().unwrap())
    }

    /// Create p-chart for proportion defective
    pub fn create_p_chart(
        &mut self,
        id: String,
        metric_name: String,
        defect_data: &[(u64, u64)], // (defects, sample_size) pairs
    ) -> Result<&SpcChart> {
        if defect_data.len() < 20 {
            return Err(Error::new("Need at least 20 baseline points for p-chart"));
        }

        // Calculate overall proportion defective
        let total_defects: u64 = defect_data.iter().map(|(d, _)| d).sum();
        let total_samples: u64 = defect_data.iter().map(|(_, n)| n).sum();
        let p_bar = total_defects as f64 / total_samples as f64;

        // Average sample size
        let n_bar = total_samples as f64 / defect_data.len() as f64;

        // Standard deviation of proportion
        let sigma_p = ((p_bar * (1.0 - p_bar)) / n_bar).sqrt();

        // Control limits
        let ucl = (p_bar + 3.0 * sigma_p).min(1.0);
        let lcl = (p_bar - 3.0 * sigma_p).max(0.0);
        let uwl = (p_bar + 2.0 * sigma_p).min(1.0);
        let lwl = (p_bar - 2.0 * sigma_p).max(0.0);

        let control_limits = ControlLimits {
            center_line: p_bar,
            ucl,
            lcl,
            uwl: Some(uwl),
            lwl: Some(lwl),
            sigma: sigma_p,
            sigma_level: 3.0,
        };

        // Create data points
        let now = chrono::Utc::now();
        let data_points: Vec<DataPoint> = defect_data
            .iter()
            .enumerate()
            .map(|(i, &(defects, sample_size))| {
                let p = defects as f64 / sample_size as f64;
                DataPoint {
                    sequence: i as u64,
                    timestamp: now,
                    value: p,
                    subgroup_id: None,
                    sample_size: Some(sample_size),
                    is_out_of_control: p > ucl || p < lcl,
                    violations: Vec::new(),
                }
            })
            .collect();

        let chart = SpcChart {
            id,
            chart_type: ChartType::P,
            metric_name,
            data_points,
            control_limits,
            specification_limits: None,
            violations: Vec::new(),
            created_at: now,
        };

        self.charts.push(chart);
        Ok(self.charts.last().unwrap())
    }

    /// Create u-chart for defects per unit
    pub fn create_u_chart(
        &mut self,
        id: String,
        metric_name: String,
        defect_data: &[(u64, u64)], // (defects, units) pairs
    ) -> Result<&SpcChart> {
        if defect_data.len() < 20 {
            return Err(Error::new("Need at least 20 baseline points for u-chart"));
        }

        // Calculate average defects per unit
        let total_defects: u64 = defect_data.iter().map(|(d, _)| d).sum();
        let total_units: u64 = defect_data.iter().map(|(_, n)| n).sum();
        let u_bar = total_defects as f64 / total_units as f64;

        // Average units per sample
        let n_bar = total_units as f64 / defect_data.len() as f64;

        // Standard deviation
        let sigma_u = (u_bar / n_bar).sqrt();

        // Control limits
        let ucl = u_bar + 3.0 * sigma_u;
        let lcl = (u_bar - 3.0 * sigma_u).max(0.0);
        let uwl = u_bar + 2.0 * sigma_u;
        let lwl = (u_bar - 2.0 * sigma_u).max(0.0);

        let control_limits = ControlLimits {
            center_line: u_bar,
            ucl,
            lcl,
            uwl: Some(uwl),
            lwl: Some(lwl),
            sigma: sigma_u,
            sigma_level: 3.0,
        };

        // Create data points
        let now = chrono::Utc::now();
        let data_points: Vec<DataPoint> = defect_data
            .iter()
            .enumerate()
            .map(|(i, &(defects, units))| {
                let u = defects as f64 / units as f64;
                DataPoint {
                    sequence: i as u64,
                    timestamp: now,
                    value: u,
                    subgroup_id: None,
                    sample_size: Some(units),
                    is_out_of_control: u > ucl || u < lcl,
                    violations: Vec::new(),
                }
            })
            .collect();

        let chart = SpcChart {
            id,
            chart_type: ChartType::U,
            metric_name,
            data_points,
            control_limits,
            specification_limits: None,
            violations: Vec::new(),
            created_at: now,
        };

        self.charts.push(chart);
        Ok(self.charts.last().unwrap())
    }

    /// Add new data point and check for violations
    pub fn add_data_point(
        &mut self,
        chart_id: &str,
        value: f64,
        sample_size: Option<u64>,
    ) -> Result<Vec<ViolationType>> {
        let chart = self
            .charts
            .iter_mut()
            .find(|c| c.id == chart_id)
            .ok_or_else(|| Error::new(&format!("Chart not found: {}", chart_id)))?;

        let sequence = chart.data_points.len() as u64;
        let timestamp = chrono::Utc::now();

        let is_out_of_control = value > chart.control_limits.ucl || value < chart.control_limits.lcl;

        let data_point = DataPoint {
            sequence,
            timestamp,
            value,
            subgroup_id: None,
            sample_size,
            is_out_of_control,
            violations: Vec::new(),
        };

        chart.data_points.push(data_point);

        // Check for violations using Western Electric Rules
        let violations = self.check_western_electric_rules(chart_id)?;

        Ok(violations)
    }

    /// Check Western Electric Rules for special causes
    pub fn check_western_electric_rules(&mut self, chart_id: &str) -> Result<Vec<ViolationType>> {
        let chart = self
            .charts
            .iter_mut()
            .find(|c| c.id == chart_id)
            .ok_or_else(|| Error::new(&format!("Chart not found: {}", chart_id)))?;

        let mut violations = Vec::new();
        let n = chart.data_points.len();

        if n == 0 {
            return Ok(violations);
        }

        let cl = chart.control_limits.center_line;
        let ucl = chart.control_limits.ucl;
        let lcl = chart.control_limits.lcl;
        let sigma = chart.control_limits.sigma;

        // Rule 1: One point beyond 3σ
        if let Some(last) = chart.data_points.last() {
            if last.value > ucl || last.value < lcl {
                violations.push(ViolationType::BeyondControlLimits);
                chart.violations.push(ControlViolation {
                    violation_type: ViolationType::BeyondControlLimits,
                    sequence: last.sequence,
                    timestamp: last.timestamp,
                    description: format!("Point beyond control limits: {}", last.value),
                    severity: ViolationSeverity::Critical,
                });
            }
        }

        // Rule 2: Nine points in a row on same side of centerline
        if n >= 9 {
            let last_nine = &chart.data_points[n - 9..];
            let all_above = last_nine.iter().all(|p| p.value > cl);
            let all_below = last_nine.iter().all(|p| p.value < cl);

            if all_above || all_below {
                violations.push(ViolationType::NinePointsOneSide);
                chart.violations.push(ControlViolation {
                    violation_type: ViolationType::NinePointsOneSide,
                    sequence: last_nine.last().unwrap().sequence,
                    timestamp: last_nine.last().unwrap().timestamp,
                    description: "Nine consecutive points on same side of centerline".to_string(),
                    severity: ViolationSeverity::Warning,
                });
            }
        }

        // Rule 3: Six points in a row steadily increasing or decreasing
        if n >= 6 {
            let last_six = &chart.data_points[n - 6..];
            let mut increasing = true;
            let mut decreasing = true;

            for i in 1..last_six.len() {
                if last_six[i].value <= last_six[i - 1].value {
                    increasing = false;
                }
                if last_six[i].value >= last_six[i - 1].value {
                    decreasing = false;
                }
            }

            if increasing || decreasing {
                violations.push(ViolationType::SixPointsTrend);
                chart.violations.push(ControlViolation {
                    violation_type: ViolationType::SixPointsTrend,
                    sequence: last_six.last().unwrap().sequence,
                    timestamp: last_six.last().unwrap().timestamp,
                    description: "Six consecutive points showing trend".to_string(),
                    severity: ViolationSeverity::Warning,
                });
            }
        }

        // Rule 5: Two out of three points beyond 2σ on same side
        if n >= 3 {
            let last_three = &chart.data_points[n - 3..];
            let two_sigma_upper = cl + 2.0 * sigma;
            let two_sigma_lower = cl - 2.0 * sigma;

            let above_two_sigma = last_three.iter().filter(|p| p.value > two_sigma_upper).count();
            let below_two_sigma = last_three.iter().filter(|p| p.value < two_sigma_lower).count();

            if above_two_sigma >= 2 || below_two_sigma >= 2 {
                violations.push(ViolationType::TwoOfThreeBeyondTwoSigma);
                chart.violations.push(ControlViolation {
                    violation_type: ViolationType::TwoOfThreeBeyondTwoSigma,
                    sequence: last_three.last().unwrap().sequence,
                    timestamp: last_three.last().unwrap().timestamp,
                    description: "Two out of three points beyond 2σ".to_string(),
                    severity: ViolationSeverity::Warning,
                });
            }
        }

        Ok(violations)
    }

    /// Calculate process capability indices
    pub fn calculate_process_capability(
        &self,
        chart_id: &str,
        spec_lower: Option<f64>,
        spec_upper: Option<f64>,
    ) -> Result<(f64, f64)> {
        let chart = self
            .charts
            .iter()
            .find(|c| c.id == chart_id)
            .ok_or_else(|| Error::new(&format!("Chart not found: {}", chart_id)))?;

        let mean = chart.control_limits.center_line;
        let sigma = chart.control_limits.sigma;

        // Calculate Cp (process capability)
        let cp = if let (Some(lsl), Some(usl)) = (spec_lower, spec_upper) {
            (usl - lsl) / (6.0 * sigma)
        } else {
            return Err(Error::new("Both spec limits required for Cp"));
        };

        // Calculate Cpk (process capability index)
        let cpk = if let (Some(lsl), Some(usl)) = (spec_lower, spec_upper) {
            let cpu = (usl - mean) / (3.0 * sigma);
            let cpl = (mean - lsl) / (3.0 * sigma);
            cpu.min(cpl)
        } else if let Some(usl) = spec_upper {
            (usl - mean) / (3.0 * sigma)
        } else if let Some(lsl) = spec_lower {
            (mean - lsl) / (3.0 * sigma)
        } else {
            return Err(Error::new("At least one spec limit required for Cpk"));
        };

        Ok((cp, cpk))
    }

    /// Get chart by ID
    pub fn get_chart(&self, chart_id: &str) -> Option<&SpcChart> {
        self.charts.iter().find(|c| c.id == chart_id)
    }

    /// Get all charts
    pub fn get_all_charts(&self) -> &[SpcChart] {
        &self.charts
    }
}

impl Default for ControlChart {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_imr_chart_creation() {
        let mut cc = ControlChart::new();
        let baseline: Vec<f64> = (0..30).map(|i| 100.0 + (i as f64 * 0.5)).collect();

        let result = cc.create_imr_chart(
            "test-imr-1".to_string(),
            "RDF Parsing Time".to_string(),
            &baseline,
        );

        assert!(result.is_ok());
        let chart = cc.get_chart("test-imr-1").unwrap();
        assert_eq!(chart.chart_type, ChartType::IMR);
        assert_eq!(chart.data_points.len(), 30);
    }

    #[test]
    fn test_p_chart_creation() {
        let mut cc = ControlChart::new();
        // (defects, sample_size) pairs
        let defect_data: Vec<(u64, u64)> = (0..25).map(|_| (5, 100)).collect();

        let result = cc.create_p_chart(
            "test-p-1".to_string(),
            "Defect Rate".to_string(),
            &defect_data,
        );

        assert!(result.is_ok());
        let chart = cc.get_chart("test-p-1").unwrap();
        assert_eq!(chart.chart_type, ChartType::P);
        assert_eq!(chart.data_points.len(), 25);
        assert!(chart.control_limits.center_line > 0.0);
    }

    #[test]
    fn test_western_electric_rule_1() {
        let mut cc = ControlChart::new();
        let baseline: Vec<f64> = vec![100.0; 30];

        cc.create_imr_chart(
            "test-rule-1".to_string(),
            "Test Metric".to_string(),
            &baseline,
        )
        .unwrap();

        // Add point way out of control
        let violations = cc.add_data_point("test-rule-1", 200.0, Some(1)).unwrap();

        assert!(violations.contains(&ViolationType::BeyondControlLimits));
    }
}
