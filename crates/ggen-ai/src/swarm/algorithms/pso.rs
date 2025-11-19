//! Particle Swarm Optimization for Template Parameter Tuning
//!
//! This module implements PSO to optimize template parameters by:
//! - Treating each parameter set as a particle in solution space
//! - Using velocity and position updates to explore parameter space
//! - Tracking personal and global best solutions
//! - Adapting to quality feedback from generated code

use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info};

/// Particle Swarm Optimization configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PsoConfig {
    /// Number of particles in the swarm
    pub num_particles: usize,
    /// Maximum iterations
    pub max_iterations: usize,
    /// Inertia weight (how much to preserve velocity)
    pub inertia_weight: f64,
    /// Cognitive weight (attraction to personal best)
    pub cognitive_weight: f64,
    /// Social weight (attraction to global best)
    pub social_weight: f64,
    /// Maximum velocity per dimension
    pub max_velocity: f64,
    /// Convergence threshold
    pub convergence_threshold: f64,
}

impl Default for PsoConfig {
    fn default() -> Self {
        Self {
            num_particles: 30,
            max_iterations: 100,
            inertia_weight: 0.7,
            cognitive_weight: 1.5,
            social_weight: 1.5,
            max_velocity: 0.5,
            convergence_threshold: 0.001,
        }
    }
}

/// Template parameter definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateParameter {
    /// Parameter name
    pub name: String,
    /// Minimum value
    pub min: f64,
    /// Maximum value
    pub max: f64,
    /// Is this parameter discrete (integer)?
    pub discrete: bool,
    /// Parameter type
    pub param_type: ParameterType,
}

/// Parameter types for different template aspects
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ParameterType {
    /// Complexity level (e.g., number of layers, depth)
    Complexity,
    /// Verbosity level (e.g., comment density, documentation)
    Verbosity,
    /// Performance tuning (e.g., buffer sizes, optimization levels)
    Performance,
    /// Code style (e.g., line length, naming conventions)
    Style,
    /// Feature flags (binary 0/1)
    Feature,
}

/// A particle in the parameter space
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Particle {
    /// Current position in parameter space
    pub position: HashMap<String, f64>,
    /// Current velocity
    pub velocity: HashMap<String, f64>,
    /// Personal best position
    pub personal_best_position: HashMap<String, f64>,
    /// Personal best fitness
    pub personal_best_fitness: f64,
    /// Current fitness
    pub current_fitness: f64,
}

/// PSO solution for template parameters
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PsoSolution {
    /// Optimized parameters
    pub parameters: HashMap<String, f64>,
    /// Quality score
    pub quality: f64,
    /// Number of iterations to converge
    pub iterations: usize,
}

/// Fitness function for evaluating template parameter quality
#[async_trait::async_trait]
pub trait FitnessFunction: Send + Sync {
    /// Evaluate quality of parameter set
    async fn evaluate(&self, parameters: &HashMap<String, f64>) -> Result<f64>;
}

/// Template quality metrics for fitness evaluation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateQuality {
    /// Code correctness score (0.0 to 1.0)
    pub correctness: f64,
    /// Readability score (0.0 to 1.0)
    pub readability: f64,
    /// Performance score (0.0 to 1.0)
    pub performance: f64,
    /// Maintainability score (0.0 to 1.0)
    pub maintainability: f64,
    /// Test coverage score (0.0 to 1.0)
    pub test_coverage: f64,
}

impl TemplateQuality {
    /// Calculate composite quality score
    pub fn composite_score(&self) -> f64 {
        (self.correctness * 0.3
            + self.readability * 0.2
            + self.performance * 0.2
            + self.maintainability * 0.2
            + self.test_coverage * 0.1)
    }
}

/// Particle Swarm Optimizer for template parameters
#[derive(Debug)]
pub struct TemplateParameterOptimizer {
    /// Configuration
    config: PsoConfig,
    /// Parameter definitions
    parameters: Vec<TemplateParameter>,
    /// Swarm particles
    particles: Arc<RwLock<Vec<Particle>>>,
    /// Global best position
    global_best_position: Arc<RwLock<HashMap<String, f64>>>,
    /// Global best fitness
    global_best_fitness: Arc<RwLock<f64>>,
    /// Fitness history for convergence detection
    fitness_history: Arc<RwLock<Vec<f64>>>,
}

impl TemplateParameterOptimizer {
    /// Create a new template parameter optimizer
    pub fn new(config: PsoConfig, parameters: Vec<TemplateParameter>) -> Self {
        let particles = Vec::new();
        let global_best_position = HashMap::new();

        Self {
            config,
            parameters,
            particles: Arc::new(RwLock::new(particles)),
            global_best_position: Arc::new(RwLock::new(global_best_position)),
            global_best_fitness: Arc::new(RwLock::new(f64::NEG_INFINITY)),
            fitness_history: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Initialize swarm with random particles
    pub async fn initialize_swarm(&self) -> Result<()> {
        let mut particles = self.particles.write().await;
        particles.clear();

        for _ in 0..self.config.num_particles {
            let mut position = HashMap::new();
            let mut velocity = HashMap::new();

            for param in &self.parameters {
                // Random initial position within bounds
                let value = param.min + fastrand::f64() * (param.max - param.min);
                position.insert(param.name.clone(), value);

                // Random initial velocity
                let v = (fastrand::f64() * 2.0 - 1.0) * self.config.max_velocity;
                velocity.insert(param.name.clone(), v);
            }

            particles.push(Particle {
                position: position.clone(),
                velocity,
                personal_best_position: position,
                personal_best_fitness: f64::NEG_INFINITY,
                current_fitness: f64::NEG_INFINITY,
            });
        }

        debug!("Initialized PSO swarm with {} particles", self.config.num_particles);
        Ok(())
    }

    /// Optimize template parameters using PSO
    pub async fn optimize<F>(&self, fitness_fn: &F) -> Result<PsoSolution>
    where
        F: FitnessFunction,
    {
        self.initialize_swarm().await?;

        for iteration in 0..self.config.max_iterations {
            // Evaluate all particles
            self.evaluate_particles(fitness_fn).await?;

            // Update velocities and positions
            self.update_particles().await?;

            // Check convergence
            if self.has_converged().await? {
                info!("PSO converged at iteration {}", iteration);
                break;
            }

            if iteration % 10 == 0 {
                let best_fitness = *self.global_best_fitness.read().await;
                info!("PSO iteration {}: best fitness = {:.4}", iteration, best_fitness);
            }
        }

        let global_best = self.global_best_position.read().await.clone();
        let global_fitness = *self.global_best_fitness.read().await;
        let iterations = self.fitness_history.read().await.len();

        Ok(PsoSolution {
            parameters: global_best,
            quality: global_fitness,
            iterations,
        })
    }

    /// Evaluate fitness of all particles
    async fn evaluate_particles<F>(&self, fitness_fn: &F) -> Result<()>
    where
        F: FitnessFunction,
    {
        let mut particles = self.particles.write().await;
        let mut global_best_position = self.global_best_position.write().await;
        let mut global_best_fitness = self.global_best_fitness.write().await;

        for particle in particles.iter_mut() {
            // Evaluate current position
            let fitness = fitness_fn.evaluate(&particle.position).await?;
            particle.current_fitness = fitness;

            // Update personal best
            if fitness > particle.personal_best_fitness {
                particle.personal_best_fitness = fitness;
                particle.personal_best_position = particle.position.clone();
            }

            // Update global best
            if fitness > *global_best_fitness {
                *global_best_fitness = fitness;
                *global_best_position = particle.position.clone();
                debug!("New global best fitness: {:.4}", fitness);
            }
        }

        // Record global best for convergence detection
        self.fitness_history.write().await.push(*global_best_fitness);

        Ok(())
    }

    /// Update particle velocities and positions
    async fn update_particles(&self) -> Result<()> {
        let mut particles = self.particles.write().await;
        let global_best = self.global_best_position.read().await;

        for particle in particles.iter_mut() {
            for param in &self.parameters {
                let param_name = &param.name;

                // Get current values
                let position = *particle.position.get(param_name).unwrap_or(&0.0);
                let velocity = *particle.velocity.get(param_name).unwrap_or(&0.0);
                let personal_best = *particle.personal_best_position.get(param_name).unwrap_or(&0.0);
                let global_best_val = *global_best.get(param_name).unwrap_or(&0.0);

                // Update velocity: v = w*v + c1*r1*(pbest - x) + c2*r2*(gbest - x)
                let r1 = fastrand::f64();
                let r2 = fastrand::f64();

                let cognitive_component = self.config.cognitive_weight * r1 * (personal_best - position);
                let social_component = self.config.social_weight * r2 * (global_best_val - position);

                let mut new_velocity = self.config.inertia_weight * velocity
                    + cognitive_component
                    + social_component;

                // Clamp velocity
                new_velocity = new_velocity.clamp(-self.config.max_velocity, self.config.max_velocity);

                // Update position
                let mut new_position = position + new_velocity;

                // Clamp position to bounds
                new_position = new_position.clamp(param.min, param.max);

                // Handle discrete parameters
                if param.discrete {
                    new_position = new_position.round();
                }

                particle.velocity.insert(param_name.clone(), new_velocity);
                particle.position.insert(param_name.clone(), new_position);
            }
        }

        Ok(())
    }

    /// Check if swarm has converged
    async fn has_converged(&self) -> Result<bool> {
        let history = self.fitness_history.read().await;

        if history.len() < 10 {
            return Ok(false);
        }

        // Check if fitness has not improved significantly in last 10 iterations
        let recent: Vec<f64> = history.iter().rev().take(10).copied().collect();
        let max_recent = recent.iter().copied().fold(f64::NEG_INFINITY, f64::max);
        let min_recent = recent.iter().copied().fold(f64::INFINITY, f64::min);

        let improvement = max_recent - min_recent;

        Ok(improvement < self.config.convergence_threshold)
    }

    /// Get current global best solution
    pub async fn get_best_solution(&self) -> PsoSolution {
        let parameters = self.global_best_position.read().await.clone();
        let quality = *self.global_best_fitness.read().await;
        let iterations = self.fitness_history.read().await.len();

        PsoSolution {
            parameters,
            quality,
            iterations,
        }
    }
}

/// Default fitness function based on template quality metrics
pub struct DefaultTemplateFitness {
    /// Quality evaluator
    quality_evaluator: Arc<dyn Fn(&HashMap<String, f64>) -> TemplateQuality + Send + Sync>,
}

impl DefaultTemplateFitness {
    /// Create a new default fitness function
    pub fn new<F>(evaluator: F) -> Self
    where
        F: Fn(&HashMap<String, f64>) -> TemplateQuality + Send + Sync + 'static,
    {
        Self {
            quality_evaluator: Arc::new(evaluator),
        }
    }
}

#[async_trait::async_trait]
impl FitnessFunction for DefaultTemplateFitness {
    async fn evaluate(&self, parameters: &HashMap<String, f64>) -> Result<f64> {
        let quality = (self.quality_evaluator)(parameters);
        Ok(quality.composite_score())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockFitness;

    #[async_trait::async_trait]
    impl FitnessFunction for MockFitness {
        async fn evaluate(&self, parameters: &HashMap<String, f64>) -> Result<f64> {
            // Simple fitness: prefer parameters close to 0.5
            let fitness = parameters.values()
                .map(|v| 1.0 - (v - 0.5).abs())
                .sum::<f64>()
                / parameters.len() as f64;

            Ok(fitness)
        }
    }

    #[tokio::test]
    async fn test_pso_optimizer_creation() {
        let parameters = vec![
            TemplateParameter {
                name: "complexity".to_string(),
                min: 0.0,
                max: 1.0,
                discrete: false,
                param_type: ParameterType::Complexity,
            },
            TemplateParameter {
                name: "verbosity".to_string(),
                min: 0.0,
                max: 1.0,
                discrete: false,
                param_type: ParameterType::Verbosity,
            },
        ];

        let config = PsoConfig::default();
        let optimizer = TemplateParameterOptimizer::new(config, parameters);

        optimizer.initialize_swarm().await.unwrap();
        let particles = optimizer.particles.read().await;
        assert_eq!(particles.len(), 30);
    }

    #[tokio::test]
    async fn test_pso_optimization() {
        let parameters = vec![
            TemplateParameter {
                name: "complexity".to_string(),
                min: 0.0,
                max: 1.0,
                discrete: false,
                param_type: ParameterType::Complexity,
            },
            TemplateParameter {
                name: "verbosity".to_string(),
                min: 0.0,
                max: 1.0,
                discrete: false,
                param_type: ParameterType::Verbosity,
            },
        ];

        let config = PsoConfig {
            num_particles: 10,
            max_iterations: 20,
            ..Default::default()
        };

        let optimizer = TemplateParameterOptimizer::new(config, parameters);
        let fitness_fn = MockFitness;

        let solution = optimizer.optimize(&fitness_fn).await.unwrap();

        assert!(solution.quality > 0.0);
        assert_eq!(solution.parameters.len(), 2);

        // Check that parameters are close to optimal (0.5)
        for (_name, value) in solution.parameters {
            assert!((value - 0.5).abs() < 0.2, "Parameter should be close to 0.5");
        }
    }

    #[test]
    fn test_template_quality() {
        let quality = TemplateQuality {
            correctness: 0.9,
            readability: 0.8,
            performance: 0.85,
            maintainability: 0.75,
            test_coverage: 0.7,
        };

        let score = quality.composite_score();
        assert!(score > 0.7 && score < 0.9);
    }
}
