//! AI Ontology Evolution Command
//!
//! This module provides CLI commands for running the AI-native ontology evolution engine.

use anyhow::Result;
use clap::Parser;
use ggen_ai::{EvolutionConfig, OntologyEvolutionCoordinator};
use serde_json;
use std::path::PathBuf;

/// Run AI-native ontology evolution
#[derive(Parser, Debug)]
#[command(about = "Evolve RDF ontologies using AI with RL feedback loops and quantum-inspired algorithms")]
pub struct EvolveCommand {
    /// Number of evolution cycles to run
    #[arg(short = 'n', long, default_value = "10")]
    pub cycles: usize,

    /// Learning rate for reinforcement learning
    #[arg(long, default_value = "0.01")]
    pub learning_rate: f64,

    /// Discount factor for future rewards
    #[arg(long, default_value = "0.95")]
    pub discount_factor: f64,

    /// Exploration rate for RL
    #[arg(long, default_value = "0.1")]
    pub exploration_rate: f64,

    /// Quantum annealing temperature
    #[arg(long, default_value = "1.0")]
    pub quantum_temperature: f64,

    /// Number of quantum superposition states
    #[arg(long, default_value = "128")]
    pub quantum_states: usize,

    /// Target languages for polyglot generation (comma-separated)
    #[arg(long, default_value = "rust,typescript,python,go")]
    pub target_languages: String,

    /// Output file for evolved ontology (RDF/Turtle format)
    #[arg(short = 'o', long)]
    pub output: Option<PathBuf>,

    /// Export metrics to JSON file
    #[arg(long)]
    pub metrics_output: Option<PathBuf>,

    /// Export evolution history to JSON file
    #[arg(long)]
    pub history_output: Option<PathBuf>,

    /// Enable verbose output
    #[arg(short = 'v', long)]
    pub verbose: bool,
}

impl EvolveCommand {
    pub async fn execute(&self) -> Result<()> {
        println!("🧬 AI-Native Ontology Evolution Engine");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        // Parse target languages
        let target_languages: Vec<String> = self
            .target_languages
            .split(',')
            .map(|s| s.trim().to_string())
            .collect();

        // Create evolution configuration
        let config = EvolutionConfig {
            learning_rate: self.learning_rate,
            discount_factor: self.discount_factor,
            exploration_rate: self.exploration_rate,
            quantum_temperature: self.quantum_temperature,
            quantum_states: self.quantum_states,
            max_iterations: self.cycles,
            target_languages,
            ..Default::default()
        };

        if self.verbose {
            println!("Configuration:");
            println!("  Cycles: {}", self.cycles);
            println!("  Learning Rate: {}", self.learning_rate);
            println!("  Discount Factor: {}", self.discount_factor);
            println!("  Exploration Rate: {}", self.exploration_rate);
            println!("  Quantum Temperature: {}", self.quantum_temperature);
            println!("  Quantum States: {}", self.quantum_states);
            println!("  Target Languages: {}", self.target_languages);
            println!();
        }

        // Initialize coordinator
        println!("Initializing evolution coordinator...");
        let coordinator = OntologyEvolutionCoordinator::new(config);

        // Run evolution cycles
        println!("\nRunning {} evolution cycles...\n", self.cycles);

        let cycles = coordinator.evolve_n_cycles(self.cycles).await?;

        // Display results
        println!("\n✅ Evolution Complete!");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

        for cycle in &cycles {
            println!("Cycle {}: {}", cycle.cycle, cycle.template);
            println!("  Evolution: {:?}", cycle.evolution);
            println!("  Reward: {:.4}", cycle.reward);
            println!("  Inferences: {} new triples", cycle.inferences.len());
            println!("  Code Generated:");
            for (lang, _) in &cycle.code_samples {
                println!("    ✓ {}", lang);
            }
            println!();
        }

        // Get final metrics
        let metrics = coordinator.get_metrics().await;

        println!("\n📊 Evolution Metrics:");
        println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        println!("  Total Cycles: {}", metrics.iterations_completed);
        println!("  Schema Modifications: {}", metrics.schema_modifications);
        println!("  Cumulative Reward: {:.4}", metrics.cumulative_reward);
        println!("  Converged: {}", if metrics.converged { "Yes" } else { "No" });
        println!("\n  Language Quality Scores:");
        for (lang, score) in &metrics.language_quality_scores {
            println!("    {}: {:.4}", lang, score);
        }

        // Export evolved ontology
        if let Some(output_path) = &self.output {
            println!("\n💾 Exporting evolved ontology to: {:?}", output_path);
            let ontology_rdf = coordinator.export_ontology().await?;
            std::fs::write(output_path, ontology_rdf)?;
            println!("   ✓ Ontology exported successfully");
        }

        // Export metrics
        if let Some(metrics_path) = &self.metrics_output {
            println!("\n📈 Exporting metrics to: {:?}", metrics_path);
            let metrics_json = serde_json::to_string_pretty(&metrics)?;
            std::fs::write(metrics_path, metrics_json)?;
            println!("   ✓ Metrics exported successfully");
        }

        // Export history
        if let Some(history_path) = &self.history_output {
            println!("\n📜 Exporting evolution history to: {:?}", history_path);
            let history = coordinator.get_history().await;
            let history_json = serde_json::to_string_pretty(&history)?;
            std::fs::write(history_path, history_json)?;
            println!("   ✓ History exported successfully");
        }

        println!("\n✨ Done!");

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_evolve_command() {
        let cmd = EvolveCommand {
            cycles: 3,
            learning_rate: 0.01,
            discount_factor: 0.95,
            exploration_rate: 0.1,
            quantum_temperature: 1.0,
            quantum_states: 64,
            target_languages: "rust,typescript".to_string(),
            output: None,
            metrics_output: None,
            history_output: None,
            verbose: false,
        };

        // This should execute without errors
        let result = cmd.execute().await;
        assert!(result.is_ok());
    }
}
