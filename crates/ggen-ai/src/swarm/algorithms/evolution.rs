//! Multi-Agent Collaborative Template Evolution
//!
//! This module implements collaborative evolution of code templates using:
//! - Multi-agent genetic algorithms
//! - Crossover between high-performing templates
//! - Mutation for exploration
//! - Fitness-based selection
//! - Emergent collaborative behaviors

use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info};

/// Evolution configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvolutionConfig {
    /// Population size
    pub population_size: usize,
    /// Number of generations
    pub num_generations: usize,
    /// Crossover rate (0.0 to 1.0)
    pub crossover_rate: f64,
    /// Mutation rate (0.0 to 1.0)
    pub mutation_rate: f64,
    /// Elite preservation count
    pub elite_count: usize,
    /// Tournament selection size
    pub tournament_size: usize,
    /// Multi-objective optimization
    pub multi_objective: bool,
}

impl Default for EvolutionConfig {
    fn default() -> Self {
        Self {
            population_size: 50,
            num_generations: 100,
            crossover_rate: 0.8,
            mutation_rate: 0.1,
            elite_count: 5,
            tournament_size: 3,
            multi_objective: true,
        }
    }
}

/// Template genome representing evolvable template characteristics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateGenome {
    /// Template structure genes
    pub structure: Vec<StructureGene>,
    /// Code pattern genes
    pub patterns: Vec<PatternGene>,
    /// Quality attribute genes
    pub attributes: HashMap<String, f64>,
    /// Fitness score
    pub fitness: f64,
    /// Multi-objective scores
    pub objectives: HashMap<String, f64>,
}

/// Structure gene defining template structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructureGene {
    /// Component type (e.g., class, function, module)
    pub component_type: String,
    /// Nesting level
    pub nesting_level: u32,
    /// Component count
    pub count: usize,
    /// Composition strategy
    pub composition: CompositionStrategy,
}

/// Pattern gene defining code patterns
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PatternGene {
    /// Pattern name (e.g., singleton, factory, observer)
    pub pattern_name: String,
    /// Pattern strength (0.0 to 1.0)
    pub strength: f64,
    /// Pattern applicability
    pub applicable_to: Vec<String>,
}

/// Composition strategies for template structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CompositionStrategy {
    /// Linear composition
    Linear,
    /// Hierarchical composition
    Hierarchical,
    /// Modular composition
    Modular,
    /// Layered composition
    Layered,
}

/// Fitness evaluator for template genomes
#[async_trait::async_trait]
pub trait GenomeFitnessEvaluator: Send + Sync {
    /// Evaluate genome fitness
    async fn evaluate(&self, genome: &TemplateGenome) -> Result<f64>;

    /// Evaluate multiple objectives
    async fn evaluate_objectives(&self, genome: &TemplateGenome) -> Result<HashMap<String, f64>>;
}

/// Collaborative template evolution engine
#[derive(Debug)]
pub struct TemplateEvolutionEngine {
    /// Configuration
    config: EvolutionConfig,
    /// Current population
    population: Arc<RwLock<Vec<TemplateGenome>>>,
    /// Best genome across all generations
    best_genome: Arc<RwLock<Option<TemplateGenome>>>,
    /// Generation history
    generation_history: Arc<RwLock<Vec<GenerationStats>>>,
    /// Collaboration matrix (agent interactions)
    collaboration_matrix: Arc<RwLock<HashMap<(usize, usize), f64>>>,
}

/// Statistics for each generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationStats {
    /// Generation number
    pub generation: usize,
    /// Best fitness in generation
    pub best_fitness: f64,
    /// Average fitness
    pub average_fitness: f64,
    /// Diversity score
    pub diversity: f64,
    /// Multi-objective pareto front size
    pub pareto_front_size: usize,
}

impl TemplateEvolutionEngine {
    /// Create a new template evolution engine
    pub fn new(config: EvolutionConfig) -> Self {
        Self {
            config,
            population: Arc::new(RwLock::new(Vec::new())),
            best_genome: Arc::new(RwLock::new(None)),
            generation_history: Arc::new(RwLock::new(Vec::new())),
            collaboration_matrix: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Initialize population with random genomes
    pub async fn initialize_population(&self, seed_genomes: Option<Vec<TemplateGenome>>) -> Result<()> {
        let mut population = self.population.write().await;
        population.clear();

        if let Some(seeds) = seed_genomes {
            // Start with seed genomes and fill the rest randomly
            population.extend(seeds);

            while population.len() < self.config.population_size {
                let genome = self.create_random_genome();
                population.push(genome);
            }
        } else {
            // Create entirely random population
            for _ in 0..self.config.population_size {
                let genome = self.create_random_genome();
                population.push(genome);
            }
        }

        debug!("Initialized population with {} genomes", population.len());
        Ok(())
    }

    /// Evolve templates over multiple generations
    pub async fn evolve<F>(&self, fitness_evaluator: &F) -> Result<TemplateGenome>
    where
        F: GenomeFitnessEvaluator,
    {
        for generation in 0..self.config.num_generations {
            // Evaluate fitness
            self.evaluate_population(fitness_evaluator).await?;

            // Record statistics
            self.record_generation_stats(generation).await?;

            // Selection
            let parents = self.select_parents().await?;

            // Crossover and mutation
            let offspring = self.create_offspring(&parents).await?;

            // Replace population (elitism preserved)
            self.replace_population(offspring).await?;

            if generation % 10 == 0 {
                let best = self.best_genome.read().await;
                if let Some(genome) = best.as_ref() {
                    info!("Generation {}: best fitness = {:.4}", generation, genome.fitness);
                }
            }
        }

        let best = self.best_genome.read().await;
        best.clone().ok_or_else(|| GgenAiError::internal("No best genome found"))
    }

    /// Create a random genome
    fn create_random_genome(&self) -> TemplateGenome {
        let structure = vec![
            StructureGene {
                component_type: "class".to_string(),
                nesting_level: fastrand::u32(0..5),
                count: fastrand::usize(1..10),
                composition: match fastrand::usize(0..4) {
                    0 => CompositionStrategy::Linear,
                    1 => CompositionStrategy::Hierarchical,
                    2 => CompositionStrategy::Modular,
                    _ => CompositionStrategy::Layered,
                },
            },
            StructureGene {
                component_type: "function".to_string(),
                nesting_level: fastrand::u32(0..3),
                count: fastrand::usize(1..20),
                composition: CompositionStrategy::Linear,
            },
        ];

        let patterns = vec![
            PatternGene {
                pattern_name: "factory".to_string(),
                strength: fastrand::f64(),
                applicable_to: vec!["class".to_string()],
            },
            PatternGene {
                pattern_name: "builder".to_string(),
                strength: fastrand::f64(),
                applicable_to: vec!["class".to_string()],
            },
        ];

        let mut attributes = HashMap::new();
        attributes.insert("complexity".to_string(), fastrand::f64());
        attributes.insert("modularity".to_string(), fastrand::f64());
        attributes.insert("testability".to_string(), fastrand::f64());

        TemplateGenome {
            structure,
            patterns,
            attributes,
            fitness: 0.0,
            objectives: HashMap::new(),
        }
    }

    /// Evaluate all genomes in population
    async fn evaluate_population<F>(&self, fitness_evaluator: &F) -> Result<()>
    where
        F: GenomeFitnessEvaluator,
    {
        let mut population = self.population.write().await;
        let mut best_genome = self.best_genome.write().await;

        for genome in population.iter_mut() {
            // Evaluate fitness
            genome.fitness = fitness_evaluator.evaluate(genome).await?;

            // Evaluate objectives if multi-objective
            if self.config.multi_objective {
                genome.objectives = fitness_evaluator.evaluate_objectives(genome).await?;
            }

            // Update best genome
            if best_genome.is_none() || genome.fitness > best_genome.as_ref().unwrap().fitness {
                *best_genome = Some(genome.clone());
            }
        }

        Ok(())
    }

    /// Select parents using tournament selection
    async fn select_parents(&self) -> Result<Vec<TemplateGenome>> {
        let population = self.population.read().await;
        let mut parents = Vec::new();

        for _ in 0..self.config.population_size {
            // Tournament selection
            let mut tournament = Vec::new();
            for _ in 0..self.config.tournament_size {
                let idx = fastrand::usize(0..population.len());
                tournament.push(population[idx].clone());
            }

            // Select best from tournament
            let winner = tournament
                .iter()
                .max_by(|a, b| a.fitness.partial_cmp(&b.fitness).unwrap())
                .unwrap()
                .clone();

            parents.push(winner);
        }

        Ok(parents)
    }

    /// Create offspring through crossover and mutation
    async fn create_offspring(&self, parents: &[TemplateGenome]) -> Result<Vec<TemplateGenome>> {
        let mut offspring = Vec::new();

        for i in (0..parents.len()).step_by(2) {
            let parent1 = &parents[i];
            let parent2 = if i + 1 < parents.len() {
                &parents[i + 1]
            } else {
                &parents[0]
            };

            // Crossover
            let (mut child1, mut child2) = if fastrand::f64() < self.config.crossover_rate {
                self.crossover(parent1, parent2)
            } else {
                (parent1.clone(), parent2.clone())
            };

            // Mutation
            if fastrand::f64() < self.config.mutation_rate {
                self.mutate(&mut child1);
            }
            if fastrand::f64() < self.config.mutation_rate {
                self.mutate(&mut child2);
            }

            offspring.push(child1);
            if offspring.len() < self.config.population_size {
                offspring.push(child2);
            }

            // Record collaboration
            self.record_collaboration(i, i + 1).await;
        }

        Ok(offspring)
    }

    /// Perform crossover between two genomes
    fn crossover(&self, parent1: &TemplateGenome, parent2: &TemplateGenome) -> (TemplateGenome, TemplateGenome) {
        let mut child1 = parent1.clone();
        let mut child2 = parent2.clone();

        // Structure crossover (swap some structure genes)
        let crossover_point = fastrand::usize(0..parent1.structure.len().min(parent2.structure.len()));
        if crossover_point < child1.structure.len() && crossover_point < child2.structure.len() {
            let temp = child1.structure[crossover_point].clone();
            child1.structure[crossover_point] = child2.structure[crossover_point].clone();
            child2.structure[crossover_point] = temp;
        }

        // Pattern crossover (blend patterns)
        let pattern_crossover = fastrand::usize(0..parent1.patterns.len().min(parent2.patterns.len()));
        if pattern_crossover < child1.patterns.len() && pattern_crossover < child2.patterns.len() {
            let temp = child1.patterns[pattern_crossover].clone();
            child1.patterns[pattern_crossover] = child2.patterns[pattern_crossover].clone();
            child2.patterns[pattern_crossover] = temp;
        }

        // Attribute crossover (blend attributes)
        for (key, value1) in &parent1.attributes {
            if let Some(value2) = parent2.attributes.get(key) {
                let blend_factor = fastrand::f64();
                child1.attributes.insert(key.clone(), value1 * blend_factor + value2 * (1.0 - blend_factor));
                child2.attributes.insert(key.clone(), value2 * blend_factor + value1 * (1.0 - blend_factor));
            }
        }

        (child1, child2)
    }

    /// Mutate a genome
    fn mutate(&self, genome: &mut TemplateGenome) {
        // Mutate structure genes
        if !genome.structure.is_empty() {
            let idx = fastrand::usize(0..genome.structure.len());
            genome.structure[idx].count = genome.structure[idx].count.saturating_add(fastrand::usize(0..3)).saturating_sub(1);
        }

        // Mutate pattern genes
        if !genome.patterns.is_empty() {
            let idx = fastrand::usize(0..genome.patterns.len());
            genome.patterns[idx].strength = (genome.patterns[idx].strength + (fastrand::f64() * 0.2 - 0.1)).clamp(0.0, 1.0);
        }

        // Mutate attributes
        for (_key, value) in genome.attributes.iter_mut() {
            *value = (*value + (fastrand::f64() * 0.2 - 0.1)).clamp(0.0, 1.0);
        }
    }

    /// Replace population with offspring (preserve elites)
    async fn replace_population(&self, mut offspring: Vec<TemplateGenome>) -> Result<()> {
        let mut population = self.population.write().await;

        // Sort current population by fitness
        population.sort_by(|a, b| b.fitness.partial_cmp(&a.fitness).unwrap());

        // Preserve elite genomes
        let elites: Vec<TemplateGenome> = population.iter().take(self.config.elite_count).cloned().collect();

        // Replace population
        *population = elites;
        population.append(&mut offspring);
        population.truncate(self.config.population_size);

        Ok(())
    }

    /// Record generation statistics
    async fn record_generation_stats(&self, generation: usize) -> Result<()> {
        let population = self.population.read().await;

        let best_fitness = population
            .iter()
            .map(|g| g.fitness)
            .fold(f64::NEG_INFINITY, f64::max);

        let average_fitness = population.iter().map(|g| g.fitness).sum::<f64>() / population.len() as f64;

        let diversity = self.calculate_diversity(&population);

        let pareto_front_size = if self.config.multi_objective {
            self.calculate_pareto_front(&population).len()
        } else {
            0
        };

        let stats = GenerationStats {
            generation,
            best_fitness,
            average_fitness,
            diversity,
            pareto_front_size,
        };

        self.generation_history.write().await.push(stats);

        Ok(())
    }

    /// Calculate population diversity
    fn calculate_diversity(&self, population: &[TemplateGenome]) -> f64 {
        if population.is_empty() {
            return 0.0;
        }

        let mut total_distance = 0.0;
        let mut comparisons = 0;

        for i in 0..population.len() {
            for j in (i + 1)..population.len() {
                // Simple diversity measure: attribute differences
                let distance: f64 = population[i]
                    .attributes
                    .iter()
                    .map(|(k, v1)| {
                        let v2 = population[j].attributes.get(k).unwrap_or(&0.0);
                        (v1 - v2).abs()
                    })
                    .sum();

                total_distance += distance;
                comparisons += 1;
            }
        }

        if comparisons > 0 {
            total_distance / comparisons as f64
        } else {
            0.0
        }
    }

    /// Calculate Pareto front for multi-objective optimization
    fn calculate_pareto_front(&self, population: &[TemplateGenome]) -> Vec<TemplateGenome> {
        let mut pareto_front = Vec::new();

        for candidate in population {
            let mut dominated = false;

            for other in population {
                if self.dominates(other, candidate) {
                    dominated = true;
                    break;
                }
            }

            if !dominated {
                pareto_front.push(candidate.clone());
            }
        }

        pareto_front
    }

    /// Check if genome1 dominates genome2 (Pareto dominance)
    fn dominates(&self, genome1: &TemplateGenome, genome2: &TemplateGenome) -> bool {
        let mut better_in_one = false;

        for (key, value1) in &genome1.objectives {
            if let Some(value2) = genome2.objectives.get(key) {
                if value1 < value2 {
                    return false; // genome1 is worse in this objective
                }
                if value1 > value2 {
                    better_in_one = true;
                }
            }
        }

        better_in_one
    }

    /// Record collaboration between two parents
    async fn record_collaboration(&self, parent1_idx: usize, parent2_idx: usize) {
        let mut matrix = self.collaboration_matrix.write().await;
        let key = (parent1_idx.min(parent2_idx), parent1_idx.max(parent2_idx));
        *matrix.entry(key).or_insert(0.0) += 1.0;
    }

    /// Get evolution statistics
    pub async fn get_statistics(&self) -> Vec<GenerationStats> {
        self.generation_history.read().await.clone()
    }

    /// Get collaboration matrix (emergent behavior)
    pub async fn get_collaboration_matrix(&self) -> HashMap<(usize, usize), f64> {
        self.collaboration_matrix.read().await.clone()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockGenomeFitness;

    #[async_trait::async_trait]
    impl GenomeFitnessEvaluator for MockGenomeFitness {
        async fn evaluate(&self, genome: &TemplateGenome) -> Result<f64> {
            // Simple fitness: sum of attributes
            Ok(genome.attributes.values().sum::<f64>() / genome.attributes.len() as f64)
        }

        async fn evaluate_objectives(&self, genome: &TemplateGenome) -> Result<HashMap<String, f64>> {
            Ok(genome.attributes.clone())
        }
    }

    #[tokio::test]
    async fn test_evolution_engine_creation() {
        let config = EvolutionConfig::default();
        let engine = TemplateEvolutionEngine::new(config);

        engine.initialize_population(None).await.unwrap();
        let population = engine.population.read().await;
        assert_eq!(population.len(), 50);
    }

    #[tokio::test]
    async fn test_template_evolution() {
        let config = EvolutionConfig {
            population_size: 20,
            num_generations: 10,
            ..Default::default()
        };

        let engine = TemplateEvolutionEngine::new(config);
        engine.initialize_population(None).await.unwrap();

        let fitness_evaluator = MockGenomeFitness;
        let best_genome = engine.evolve(&fitness_evaluator).await.unwrap();

        assert!(best_genome.fitness > 0.0);
    }

    #[test]
    fn test_genome_crossover() {
        let config = EvolutionConfig::default();
        let engine = TemplateEvolutionEngine::new(config);

        let parent1 = engine.create_random_genome();
        let parent2 = engine.create_random_genome();

        let (child1, child2) = engine.crossover(&parent1, &parent2);

        assert_eq!(child1.structure.len(), parent1.structure.len());
        assert_eq!(child2.structure.len(), parent2.structure.len());
    }
}
