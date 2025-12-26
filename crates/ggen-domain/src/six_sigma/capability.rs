//! Process capability indices (Cp, Cpk, Pp, Ppk) for template precision
//!
//! Process capability indices measure how well a process meets specification
//! limits. They are critical for Six Sigma quality assessment.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};

/// Process capability analysis results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessCapability {
    pub process_name: String,
    pub sample_size: u64,
    pub mean: f64,
    pub std_dev: f64,
    pub specification_limits: SpecificationLimits,
    pub indices: CapabilityIndex,
    pub sigma_level: f64,
    pub expected_dpmo: f64,
    pub performance_rating: PerformanceRating,
}

/// Specification limits for the process
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpecificationLimits {
    pub upper: Option<f64>,
    pub lower: Option<f64>,
    pub target: f64,
}

/// Process capability indices
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CapabilityIndex {
    /// Cp: Potential capability (assumes centered process)
    pub cp: Option<f64>,
    /// Cpk: Actual capability (accounts for centering)
    pub cpk: Option<f64>,
    /// Pp: Potential performance (uses overall variation)
    pub pp: Option<f64>,
    /// Ppk: Actual performance (uses overall variation, accounts for centering)
    pub ppk: Option<f64>,
    /// Cpm: Capability about target
    pub cpm: Option<f64>,
}

/// Performance rating based on capability indices
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum PerformanceRating {
    /// Cpk >= 2.0: World-class (Six Sigma)
    WorldClass,
    /// Cpk >= 1.67: Excellent (5 Sigma)
    Excellent,
    /// Cpk >= 1.33: Good (4 Sigma)
    Good,
    /// Cpk >= 1.0: Adequate (3 Sigma)
    Adequate,
    /// Cpk < 1.0: Poor
    Poor,
}

impl ProcessCapability {
    /// Calculate process capability from sample data
    pub fn from_samples(
        process_name: String,
        samples: &[f64],
        spec_limits: SpecificationLimits,
    ) -> Result<Self> {
        if samples.is_empty() {
            return Err(Error::new("Cannot calculate capability from empty samples"));
        }

        if samples.len() < 30 {
            return Err(Error::new(
                "Need at least 30 samples for reliable capability analysis",
            ));
        }

        // Calculate mean
        let mean = samples.iter().sum::<f64>() / samples.len() as f64;

        // Calculate standard deviation (sample std dev)
        let variance = samples
            .iter()
            .map(|x| (x - mean).powi(2))
            .sum::<f64>()
            / (samples.len() - 1) as f64;
        let std_dev = variance.sqrt();

        // Calculate capability indices
        let indices = Self::calculate_indices(mean, std_dev, &spec_limits)?;

        // Determine sigma level from Cpk
        let sigma_level = if let Some(cpk) = indices.cpk {
            cpk * 3.0 + 1.5 // Accounting for 1.5σ shift
        } else {
            0.0
        };

        // Calculate expected DPMO from sigma level
        let expected_dpmo = Self::sigma_to_dpmo(sigma_level);

        // Determine performance rating
        let performance_rating = Self::rate_performance(indices.cpk);

        Ok(Self {
            process_name,
            sample_size: samples.len() as u64,
            mean,
            std_dev,
            specification_limits: spec_limits,
            indices,
            sigma_level,
            expected_dpmo,
            performance_rating,
        })
    }

    /// Calculate all capability indices
    fn calculate_indices(
        mean: f64,
        std_dev: f64,
        spec_limits: &SpecificationLimits,
    ) -> Result<CapabilityIndex> {
        if std_dev <= 0.0 {
            return Err(Error::new("Standard deviation must be positive"));
        }

        let mut indices = CapabilityIndex {
            cp: None,
            cpk: None,
            pp: None,
            ppk: None,
            cpm: None,
        };

        // Calculate Cp (requires both limits)
        if let (Some(usl), Some(lsl)) = (spec_limits.upper, spec_limits.lower) {
            if usl <= lsl {
                return Err(Error::new("Upper spec limit must be greater than lower"));
            }
            indices.cp = Some((usl - lsl) / (6.0 * std_dev));
            indices.pp = Some((usl - lsl) / (6.0 * std_dev)); // For now, Pp = Cp
        }

        // Calculate Cpk
        if let (Some(usl), Some(lsl)) = (spec_limits.upper, spec_limits.lower) {
            let cpu = (usl - mean) / (3.0 * std_dev);
            let cpl = (mean - lsl) / (3.0 * std_dev);
            indices.cpk = Some(cpu.min(cpl));
            indices.ppk = Some(cpu.min(cpl)); // For now, Ppk = Cpk
        } else if let Some(usl) = spec_limits.upper {
            // One-sided upper
            indices.cpk = Some((usl - mean) / (3.0 * std_dev));
            indices.ppk = Some((usl - mean) / (3.0 * std_dev));
        } else if let Some(lsl) = spec_limits.lower {
            // One-sided lower
            indices.cpk = Some((mean - lsl) / (3.0 * std_dev));
            indices.ppk = Some((mean - lsl) / (3.0 * std_dev));
        }

        // Calculate Cpm (capability about target)
        if let (Some(usl), Some(lsl)) = (spec_limits.upper, spec_limits.lower) {
            let target = spec_limits.target;
            let deviation_from_target = (mean - target).powi(2);
            let tau = (std_dev.powi(2) + deviation_from_target).sqrt();
            indices.cpm = Some((usl - lsl) / (6.0 * tau));
        }

        Ok(indices)
    }

    /// Convert sigma level to DPMO (Defects Per Million Opportunities)
    fn sigma_to_dpmo(sigma: f64) -> f64 {
        // Approximate conversion using normal distribution
        // This uses the standard Six Sigma table values
        match sigma {
            s if s >= 6.0 => 3.4,
            s if s >= 5.0 => 233.0,
            s if s >= 4.0 => 6_210.0,
            s if s >= 3.0 => 66_807.0,
            s if s >= 2.0 => 308_538.0,
            s if s >= 1.0 => 691_462.0,
            _ => 1_000_000.0,
        }
    }

    /// Rate process performance based on Cpk
    fn rate_performance(cpk: Option<f64>) -> PerformanceRating {
        match cpk {
            Some(c) if c >= 2.0 => PerformanceRating::WorldClass,
            Some(c) if c >= 1.67 => PerformanceRating::Excellent,
            Some(c) if c >= 1.33 => PerformanceRating::Good,
            Some(c) if c >= 1.0 => PerformanceRating::Adequate,
            _ => PerformanceRating::Poor,
        }
    }

    /// Check if process is capable (Cpk >= 1.33)
    pub fn is_capable(&self) -> bool {
        self.indices
            .cpk
            .map(|cpk| cpk >= 1.33)
            .unwrap_or(false)
    }

    /// Check if process is centered (|Cp - Cpk| < 0.1)
    pub fn is_centered(&self) -> bool {
        if let (Some(cp), Some(cpk)) = (self.indices.cp, self.indices.cpk) {
            (cp - cpk).abs() < 0.1
        } else {
            false
        }
    }

    /// Calculate percentage of output within spec limits
    pub fn calculate_yield(&self) -> f64 {
        if let Some(dpmo) = Some(self.expected_dpmo) {
            (1.0 - (dpmo / 1_000_000.0)) * 100.0
        } else {
            0.0
        }
    }
}

/// Capability analysis for RDF ontology metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OntologyCapability {
    /// Template parsing time capability
    pub parsing_time: Option<ProcessCapability>,
    /// Validation success rate capability
    pub validation_rate: Option<ProcessCapability>,
    /// SHACL compliance capability
    pub shacl_compliance: Option<ProcessCapability>,
    /// Semantic consistency capability
    pub semantic_consistency: Option<ProcessCapability>,
    /// Overall process capability score
    pub overall_score: f64,
}

impl OntologyCapability {
    /// Create new ontology capability analysis
    pub fn new() -> Self {
        Self {
            parsing_time: None,
            validation_rate: None,
            shacl_compliance: None,
            semantic_consistency: None,
            overall_score: 0.0,
        }
    }

    /// Calculate overall capability score (average of all Cpk values)
    pub fn calculate_overall_score(&mut self) {
        let mut cpk_values = Vec::new();

        if let Some(ref cap) = self.parsing_time {
            if let Some(cpk) = cap.indices.cpk {
                cpk_values.push(cpk);
            }
        }

        if let Some(ref cap) = self.validation_rate {
            if let Some(cpk) = cap.indices.cpk {
                cpk_values.push(cpk);
            }
        }

        if let Some(ref cap) = self.shacl_compliance {
            if let Some(cpk) = cap.indices.cpk {
                cpk_values.push(cpk);
            }
        }

        if let Some(ref cap) = self.semantic_consistency {
            if let Some(cpk) = cap.indices.cpk {
                cpk_values.push(cpk);
            }
        }

        if !cpk_values.is_empty() {
            self.overall_score = cpk_values.iter().sum::<f64>() / cpk_values.len() as f64;
        }
    }

    /// Get overall performance rating
    pub fn get_overall_rating(&self) -> PerformanceRating {
        ProcessCapability::rate_performance(Some(self.overall_score))
    }
}

impl Default for OntologyCapability {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_capability_calculation() {
        // Generate sample data centered at 100 with std dev ~5
        let samples: Vec<f64> = (0..100).map(|i| 100.0 + ((i % 10) as f64 - 5.0)).collect();

        let spec_limits = SpecificationLimits {
            upper: Some(115.0),
            lower: Some(85.0),
            target: 100.0,
        };

        let result = ProcessCapability::from_samples("Test Process".to_string(), &samples, spec_limits);

        assert!(result.is_ok());
        let capability = result.unwrap();
        assert!(capability.indices.cp.is_some());
        assert!(capability.indices.cpk.is_some());
        assert!(capability.mean > 95.0 && capability.mean < 105.0);
    }

    #[test]
    fn test_performance_rating() {
        assert_eq!(
            ProcessCapability::rate_performance(Some(2.0)),
            PerformanceRating::WorldClass
        );
        assert_eq!(
            ProcessCapability::rate_performance(Some(1.67)),
            PerformanceRating::Excellent
        );
        assert_eq!(
            ProcessCapability::rate_performance(Some(1.33)),
            PerformanceRating::Good
        );
        assert_eq!(
            ProcessCapability::rate_performance(Some(1.0)),
            PerformanceRating::Adequate
        );
        assert_eq!(
            ProcessCapability::rate_performance(Some(0.8)),
            PerformanceRating::Poor
        );
    }

    #[test]
    fn test_sigma_to_dpmo_conversion() {
        assert_eq!(ProcessCapability::sigma_to_dpmo(6.0), 3.4);
        assert_eq!(ProcessCapability::sigma_to_dpmo(5.0), 233.0);
        assert_eq!(ProcessCapability::sigma_to_dpmo(4.0), 6_210.0);
        assert_eq!(ProcessCapability::sigma_to_dpmo(3.0), 66_807.0);
    }

    #[test]
    fn test_insufficient_samples() {
        let samples: Vec<f64> = vec![1.0, 2.0, 3.0]; // Only 3 samples

        let spec_limits = SpecificationLimits {
            upper: Some(10.0),
            lower: Some(0.0),
            target: 5.0,
        };

        let result = ProcessCapability::from_samples("Test".to_string(), &samples, spec_limits);

        assert!(result.is_err());
    }
}
