//! Quantum-Inspired Template Selection Algorithms
//!
//! This module implements quantum-inspired algorithms for template selection,
//! using concepts from quantum annealing, superposition, and amplitude
//! amplification to find optimal templates for code generation.

use anyhow::{Result, Context};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::f64::consts::PI;

/// Quantum state representing a template with amplitude
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuantumState {
    /// Template identifier
    pub template_id: String,

    /// Probability amplitude (complex number represented as magnitude and phase)
    pub amplitude: f64,

    /// Phase angle in radians
    pub phase: f64,

    /// Template metadata
    pub metadata: HashMap<String, String>,
}

impl QuantumState {
    /// Create a new quantum state
    pub fn new(template_id: String) -> Self {
        Self {
            template_id,
            amplitude: 1.0,
            phase: 0.0,
            metadata: HashMap::new(),
        }
    }

    /// Calculate probability (|amplitude|^2)
    pub fn probability(&self) -> f64 {
        self.amplitude * self.amplitude
    }

    /// Get complex amplitude
    pub fn complex_amplitude(&self) -> (f64, f64) {
        let real = self.amplitude * self.phase.cos();
        let imag = self.amplitude * self.phase.sin();
        (real, imag)
    }
}

/// Amplitude distribution over templates
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AmplitudeDistribution {
    /// States in superposition
    pub states: Vec<QuantumState>,

    /// Total normalization factor
    pub normalization: f64,
}

impl AmplitudeDistribution {
    /// Create uniform superposition
    pub fn uniform(template_ids: Vec<String>) -> Self {
        let n = template_ids.len();
        let amplitude = 1.0 / (n as f64).sqrt();

        let states = template_ids
            .into_iter()
            .map(|id| QuantumState {
                template_id: id,
                amplitude,
                phase: 0.0,
                metadata: HashMap::new(),
            })
            .collect();

        Self {
            states,
            normalization: 1.0,
        }
    }

    /// Normalize the distribution
    pub fn normalize(&mut self) {
        let total_prob: f64 = self.states.iter().map(|s| s.probability()).sum();
        if total_prob > 0.0 {
            let norm_factor = total_prob.sqrt();
            for state in &mut self.states {
                state.amplitude /= norm_factor;
            }
            self.normalization = norm_factor;
        }
    }

    /// Measure (collapse to single state)
    pub fn measure(&self) -> Option<String> {
        let total_prob: f64 = self.states.iter().map(|s| s.probability()).sum();
        if total_prob == 0.0 {
            return None;
        }

        let mut rng = rand::random::<f64>() * total_prob;
        for state in &self.states {
            let prob = state.probability();
            if rng <= prob {
                return Some(state.template_id.clone());
            }
            rng -= prob;
        }

        // Fallback to last state
        self.states.last().map(|s| s.template_id.clone())
    }
}

/// Quantum Template Selector
pub struct QuantumTemplateSelector {
    /// Current quantum state distribution
    distribution: AmplitudeDistribution,

    /// Temperature for simulated annealing
    temperature: f64,

    /// Cooling rate
    cooling_rate: f64,

    /// Oracle function for amplitude amplification
    oracle: Box<dyn Fn(&str) -> f64 + Send + Sync>,

    /// Selection history
    history: Vec<SelectionEvent>,
}

/// Selection event for tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SelectionEvent {
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub selected_template: String,
    pub probability: f64,
    pub temperature: f64,
    pub iteration: usize,
}

impl QuantumTemplateSelector {
    /// Create a new quantum template selector
    pub fn new<F>(template_ids: Vec<String>, temperature: f64, oracle: F) -> Self
    where
        F: Fn(&str) -> f64 + Send + Sync + 'static,
    {
        Self {
            distribution: AmplitudeDistribution::uniform(template_ids),
            temperature,
            cooling_rate: 0.95,
            oracle: Box::new(oracle),
            history: Vec::new(),
        }
    }

    /// Perform Grover-like amplitude amplification
    pub fn amplify_good_templates(&mut self, iterations: usize) -> Result<()> {
        for _ in 0..iterations {
            // Oracle: mark good templates by flipping their phase
            for state in &mut self.distribution.states {
                let quality = (self.oracle)(&state.template_id);
                if quality > 0.5 {
                    // Flip phase for good templates
                    state.phase += PI;
                }
            }

            // Diffusion operator: inversion about average
            let avg_amplitude = self
                .distribution
                .states
                .iter()
                .map(|s| {
                    let (real, _) = s.complex_amplitude();
                    real
                })
                .sum::<f64>()
                / self.distribution.states.len() as f64;

            for state in &mut self.distribution.states {
                let (real, imag) = state.complex_amplitude();
                let new_real = 2.0 * avg_amplitude - real;
                let new_imag = -imag;

                state.amplitude = (new_real * new_real + new_imag * new_imag).sqrt();
                state.phase = new_imag.atan2(new_real);
            }

            // Normalize
            self.distribution.normalize();
        }

        Ok(())
    }

    /// Perform quantum annealing for template optimization
    pub fn quantum_anneal(&mut self, max_iterations: usize) -> Result<String> {
        let mut best_template = String::new();
        let mut best_energy = f64::INFINITY;

        for iteration in 0..max_iterations {
            // Evaluate energy for each template
            for state in &mut self.distribution.states {
                let quality = (self.oracle)(&state.template_id);
                let energy = -quality; // Higher quality = lower energy

                // Metropolis criterion with quantum fluctuations
                if energy < best_energy {
                    best_energy = energy;
                    best_template = state.template_id.clone();
                } else {
                    // Accept worse solution with probability exp(-ΔE/T)
                    let delta_e = energy - best_energy;
                    let accept_prob = (-delta_e / self.temperature).exp();

                    if rand::random::<f64>() < accept_prob {
                        best_energy = energy;
                        best_template = state.template_id.clone();
                    }
                }

                // Quantum tunneling: occasionally jump to random state
                if rand::random::<f64>() < 0.01 {
                    state.amplitude *= 1.5;
                }
            }

            // Normalize after quantum fluctuations
            self.distribution.normalize();

            // Cool down
            self.temperature *= self.cooling_rate;

            // Record selection event
            if let Some(state) = self
                .distribution
                .states
                .iter()
                .find(|s| s.template_id == best_template)
            {
                self.history.push(SelectionEvent {
                    timestamp: chrono::Utc::now(),
                    selected_template: best_template.clone(),
                    probability: state.probability(),
                    temperature: self.temperature,
                    iteration,
                });
            }

            // Check convergence
            if self.temperature < 0.001 {
                break;
            }
        }

        Ok(best_template)
    }

    /// Select template using quantum interference
    pub fn select_with_interference(&mut self) -> Result<String> {
        // Apply amplitude amplification
        self.amplify_good_templates(3)?;

        // Measure the quantum state
        self.distribution
            .measure()
            .context("Failed to measure quantum state")
    }

    /// Select template using simulated quantum annealing
    pub fn select_with_annealing(&mut self, iterations: usize) -> Result<String> {
        self.quantum_anneal(iterations)
    }

    /// Get selection history
    pub fn get_history(&self) -> &[SelectionEvent] {
        &self.history
    }

    /// Get current distribution
    pub fn get_distribution(&self) -> &AmplitudeDistribution {
        &self.distribution
    }

    /// Update oracle function
    pub fn update_oracle<F>(&mut self, oracle: F)
    where
        F: Fn(&str) -> f64 + Send + Sync + 'static,
    {
        self.oracle = Box::new(oracle);
    }
}

/// Quantum walk-based template exploration
pub struct QuantumWalkSelector {
    /// Adjacency matrix for template graph
    adjacency: HashMap<String, Vec<String>>,

    /// Current position distribution
    position: HashMap<String, f64>,

    /// Walk steps
    steps: usize,
}

impl QuantumWalkSelector {
    /// Create a new quantum walk selector
    pub fn new(adjacency: HashMap<String, Vec<String>>, initial: String) -> Self {
        let mut position = HashMap::new();
        position.insert(initial, 1.0);

        Self {
            adjacency,
            position,
            steps: 0,
        }
    }

    /// Perform one step of quantum walk
    pub fn step(&mut self) -> Result<()> {
        let mut new_position = HashMap::new();

        for (node, prob) in &self.position {
            if let Some(neighbors) = self.adjacency.get(node) {
                let neighbor_count = neighbors.len() as f64;
                let transfer_prob = prob / neighbor_count;

                for neighbor in neighbors {
                    *new_position.entry(neighbor.clone()).or_insert(0.0) += transfer_prob;
                }
            }
        }

        // Apply quantum interference
        for (node, prob) in &mut new_position {
            // Add phase interference based on node properties
            let phase = (self.steps as f64 * PI / 4.0).sin();
            *prob *= 1.0 + 0.1 * phase;
        }

        // Normalize
        let total: f64 = new_position.values().sum();
        if total > 0.0 {
            for prob in new_position.values_mut() {
                *prob /= total;
            }
        }

        self.position = new_position;
        self.steps += 1;

        Ok(())
    }

    /// Walk for multiple steps
    pub fn walk(&mut self, steps: usize) -> Result<()> {
        for _ in 0..steps {
            self.step()?;
        }
        Ok(())
    }

    /// Measure final position
    pub fn measure(&self) -> Option<String> {
        let total: f64 = self.position.values().sum();
        if total == 0.0 {
            return None;
        }

        let mut rng = rand::random::<f64>() * total;
        for (node, prob) in &self.position {
            if rng <= *prob {
                return Some(node.clone());
            }
            rng -= prob;
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_quantum_state() {
        let state = QuantumState::new("test".to_string());
        assert_eq!(state.probability(), 1.0);
        assert_eq!(state.phase, 0.0);
    }

    #[test]
    fn test_amplitude_distribution() {
        let mut dist = AmplitudeDistribution::uniform(vec![
            "a".to_string(),
            "b".to_string(),
            "c".to_string(),
        ]);

        assert_eq!(dist.states.len(), 3);

        let total_prob: f64 = dist.states.iter().map(|s| s.probability()).sum();
        assert!((total_prob - 1.0).abs() < 1e-10);

        // Test measurement
        let result = dist.measure();
        assert!(result.is_some());
    }

    #[test]
    fn test_quantum_template_selector() {
        let templates = vec!["template1".to_string(), "template2".to_string()];

        // Oracle that prefers template1
        let oracle = |id: &str| if id == "template1" { 0.9 } else { 0.1 };

        let mut selector = QuantumTemplateSelector::new(templates, 1.0, oracle);

        // Test amplitude amplification
        selector.amplify_good_templates(2).unwrap();

        // The amplitude for template1 should be higher
        let template1_state = selector
            .distribution
            .states
            .iter()
            .find(|s| s.template_id == "template1")
            .unwrap();

        let template2_state = selector
            .distribution
            .states
            .iter()
            .find(|s| s.template_id == "template2")
            .unwrap();

        assert!(template1_state.probability() > template2_state.probability());
    }

    #[test]
    fn test_quantum_walk() {
        let mut adjacency = HashMap::new();
        adjacency.insert("a".to_string(), vec!["b".to_string(), "c".to_string()]);
        adjacency.insert("b".to_string(), vec!["a".to_string(), "c".to_string()]);
        adjacency.insert("c".to_string(), vec!["a".to_string(), "b".to_string()]);

        let mut walker = QuantumWalkSelector::new(adjacency, "a".to_string());
        walker.walk(10).unwrap();

        let result = walker.measure();
        assert!(result.is_some());
    }
}
