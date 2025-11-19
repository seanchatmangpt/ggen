//! Artifact analysis tools.

use super::Artifact;
use crate::ontology::CognitiveArchitecture;
use crate::paradigms::ComputationalParadigm;

/// Statistical analyzer for artifacts
pub struct StatisticalAnalyzer;

impl StatisticalAnalyzer {
    /// Create a new statistical analyzer
    #[must_use]
    pub fn new() -> Self {
        Self
    }

    /// Calculate entropy of the data
    #[must_use]
    pub fn entropy(&self, data: &[u8]) -> f64 {
        if data.is_empty() {
            return 0.0;
        }

        let mut counts = [0u64; 256];
        for &byte in data {
            counts[byte as usize] += 1;
        }

        let len = data.len() as f64;
        let mut entropy = 0.0;

        for &count in &counts {
            if count > 0 {
                let p = count as f64 / len;
                entropy -= p * p.log2();
            }
        }

        entropy
    }

    /// Guess the computational paradigm based on data characteristics
    #[must_use]
    pub fn guess_paradigm(&self, artifact: &Artifact) -> Option<ComputationalParadigm> {
        let entropy = self.entropy(&artifact.raw_data);

        // High entropy might indicate quantum or encrypted data
        if entropy > 7.5 {
            Some(ComputationalParadigm::Quantum)
        } else if entropy < 3.0 {
            // Low entropy might indicate crystalline patterns
            Some(ComputationalParadigm::Analog)
        } else {
            Some(ComputationalParadigm::Digital)
        }
    }

    /// Guess the cognitive architecture
    #[must_use]
    pub fn guess_architecture(&self, artifact: &Artifact) -> Option<CognitiveArchitecture> {
        let entropy = self.entropy(&artifact.raw_data);
        let size = artifact.raw_data.len();

        // Large, high-entropy data might be collective
        if size > 1_000_000 && entropy > 7.0 {
            Some(CognitiveArchitecture::Collective)
        } else if entropy > 7.5 {
            Some(CognitiveArchitecture::Quantum)
        } else if self.has_harmonic_patterns(&artifact.raw_data) {
            Some(CognitiveArchitecture::Crystalline)
        } else {
            Some(CognitiveArchitecture::Unknown)
        }
    }

    fn has_harmonic_patterns(&self, data: &[u8]) -> bool {
        // Simplified harmonic detection
        if data.len() < 16 {
            return false;
        }

        // Check for regular spacing in byte values
        let sample_size = data.len().min(1000);
        let mut diffs: Vec<i16> = Vec::new();

        for i in 1..sample_size {
            diffs.push(data[i] as i16 - data[i - 1] as i16);
        }

        // Check if differences show periodicity
        let mean_diff: i16 = diffs.iter().sum::<i16>() / diffs.len() as i16;
        let variance: f64 = diffs
            .iter()
            .map(|&d| {
                let diff = d - mean_diff;
                (diff * diff) as f64
            })
            .sum::<f64>()
            / diffs.len() as f64;

        // Low variance suggests regular patterns
        variance < 100.0
    }
}

impl Default for StatisticalAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}
