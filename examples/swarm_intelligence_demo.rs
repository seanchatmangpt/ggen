//! Swarm Intelligence Code Generator Demo
//!
//! This example demonstrates the swarm intelligence code generator with:
//! - Ant Colony Optimization for SPARQL query optimization
//! - Particle Swarm Optimization for template parameter tuning
//! - Collaborative template evolution
//! - Emergent polyglot code synthesis

// Note: This is a demonstration file showing how to use the swarm intelligence features
// It won't compile until the workspace dependency issues are resolved

#[allow(dead_code)]
mod demo {
    use ggen_ai::swarm::algorithms::aco::{AcoConfig, SparqlAcoOptimizer, parse_sparql_to_nodes};
    use ggen_ai::swarm::algorithms::pso::{
        PsoConfig, TemplateParameter, TemplateParameterOptimizer,
        ParameterType, DefaultTemplateFitness, TemplateQuality,
    };
    use ggen_ai::swarm::algorithms::evolution::{
        EvolutionConfig, TemplateEvolutionEngine, TemplateGenome,
        GenomeFitnessEvaluator,
    };
    use ggen_ai::swarm::emergent::{
        PolyglotSynthesisCoordinator, LanguageAgent, Language,
        PolyglotRequirements, QualityRequirements,
    };
    use std::collections::HashMap;

    /// Demonstrate ACO for SPARQL optimization
    pub async fn demo_aco_sparql() -> anyhow::Result<()> {
        println!("=== ACO SPARQL Optimization Demo ===\n");

        let sparql = r#"
            SELECT ?person ?name ?age WHERE {
                ?person rdf:type foaf:Person .
                ?person foaf:name ?name .
                ?person foaf:age ?age .
                ?person foaf:email ?email .
            }
        "#;

        println!("Original SPARQL query:\n{}\n", sparql);

        // Parse query to nodes
        let nodes = parse_sparql_to_nodes(sparql)?;
        println!("Parsed {} query nodes", nodes.len());

        // Configure ACO
        let config = AcoConfig {
            num_ants: 20,
            evaporation_rate: 0.1,
            max_iterations: 50,
            elite_ant_count: 5,
            ..Default::default()
        };

        // Create optimizer
        let optimizer = SparqlAcoOptimizer::new(config, nodes);

        // Optimize
        println!("\nRunning ACO optimization...");
        let best_path = optimizer.optimize().await?;

        println!("\n✓ Optimization complete!");
        println!("  Optimal path cost: {:.4}", best_path.cost);
        println!("  Path quality: {:.4}", best_path.quality);
        println!("  Path length: {} nodes", best_path.nodes.len());

        Ok(())
    }

    /// Demonstrate PSO for template parameter tuning
    pub async fn demo_pso_template() -> anyhow::Result<()> {
        println!("\n=== PSO Template Parameter Optimization Demo ===\n");

        // Define template parameters
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
            TemplateParameter {
                name: "optimization_level".to_string(),
                min: 0.0,
                max: 3.0,
                discrete: true,
                param_type: ParameterType::Performance,
            },
        ];

        println!("Template parameters:");
        for param in &parameters {
            println!("  - {} ({:?}): [{}, {}]",
                param.name, param.param_type, param.min, param.max);
        }

        // Create fitness function
        let fitness_fn = DefaultTemplateFitness::new(|params| {
            let complexity = params.get("complexity").copied().unwrap_or(0.5);
            let verbosity = params.get("verbosity").copied().unwrap_or(0.5);
            let opt_level = params.get("optimization_level").copied().unwrap_or(1.0);

            TemplateQuality {
                correctness: 0.95,
                readability: verbosity * 0.6 + (1.0 - complexity) * 0.4,
                performance: opt_level / 3.0,
                maintainability: (1.0 - complexity) * 0.5 + verbosity * 0.5,
                test_coverage: 0.85,
            }
        });

        // Configure PSO
        let config = PsoConfig {
            num_particles: 30,
            max_iterations: 50,
            ..Default::default()
        };

        // Create optimizer
        let optimizer = TemplateParameterOptimizer::new(config, parameters);

        // Optimize
        println!("\nRunning PSO optimization...");
        let solution = optimizer.optimize(&fitness_fn).await?;

        println!("\n✓ Optimization complete!");
        println!("  Quality score: {:.4}", solution.quality);
        println!("  Iterations: {}", solution.iterations);
        println!("\n  Optimized parameters:");
        for (name, value) in &solution.parameters {
            println!("    {}: {:.4}", name, value);
        }

        Ok(())
    }

    /// Demonstrate collaborative template evolution
    pub async fn demo_template_evolution() -> anyhow::Result<()> {
        println!("\n=== Collaborative Template Evolution Demo ===\n");

        // Configure evolution
        let config = EvolutionConfig {
            population_size: 30,
            num_generations: 20,
            crossover_rate: 0.8,
            mutation_rate: 0.1,
            elite_count: 3,
            multi_objective: true,
            ..Default::default()
        };

        println!("Evolution configuration:");
        println!("  Population size: {}", config.population_size);
        println!("  Generations: {}", config.num_generations);
        println!("  Crossover rate: {:.2}", config.crossover_rate);
        println!("  Mutation rate: {:.2}", config.mutation_rate);

        // Create evolution engine
        let engine = TemplateEvolutionEngine::new(config);

        // Initialize population
        engine.initialize_population(None).await?;

        // Define fitness evaluator
        struct MyFitnessEvaluator;

        #[async_trait::async_trait]
        impl GenomeFitnessEvaluator for MyFitnessEvaluator {
            async fn evaluate(&self, genome: &TemplateGenome) -> ggen_ai::Result<f64> {
                Ok(genome.attributes.values().sum::<f64>() / genome.attributes.len() as f64)
            }

            async fn evaluate_objectives(&self, genome: &TemplateGenome) -> ggen_ai::Result<HashMap<String, f64>> {
                Ok(genome.attributes.clone())
            }
        }

        let fitness_evaluator = MyFitnessEvaluator;

        // Evolve
        println!("\nRunning collaborative evolution...");
        let best_genome = engine.evolve(&fitness_evaluator).await?;

        println!("\n✓ Evolution complete!");
        println!("  Best fitness: {:.4}", best_genome.fitness);
        println!("  Structure genes: {}", best_genome.structure.len());
        println!("  Pattern genes: {}", best_genome.patterns.len());
        println!("  Attributes:");
        for (name, value) in &best_genome.attributes {
            println!("    {}: {:.4}", name, value);
        }

        // Get statistics
        let stats = engine.get_statistics().await;
        println!("\n  Evolution statistics:");
        println!("    Total generations: {}", stats.len());
        if let Some(first) = stats.first() {
            if let Some(last) = stats.last() {
                println!("    Initial best fitness: {:.4}", first.best_fitness);
                println!("    Final best fitness: {:.4}", last.best_fitness);
                println!("    Improvement: {:.2}%",
                    ((last.best_fitness - first.best_fitness) / first.best_fitness) * 100.0);
            }
        }

        Ok(())
    }

    /// Demonstrate emergent polyglot synthesis
    pub async fn demo_polyglot_synthesis() -> anyhow::Result<()> {
        println!("\n=== Emergent Polyglot Synthesis Demo ===\n");

        // Create coordinator
        let coordinator = PolyglotSynthesisCoordinator::new();

        // Register language agents
        println!("Registering language agents...");

        let rust_agent = LanguageAgent::new(
            "rust-agent".to_string(),
            Language::Rust,
            vec![Language::Go, Language::Python],
        );

        let python_agent = LanguageAgent::new(
            "python-agent".to_string(),
            Language::Python,
            vec![Language::JavaScript, Language::TypeScript],
        );

        let go_agent = LanguageAgent::new(
            "go-agent".to_string(),
            Language::Go,
            vec![Language::Rust, Language::Java],
        );

        coordinator.register_agent(rust_agent).await?;
        coordinator.register_agent(python_agent).await?;
        coordinator.register_agent(go_agent).await?;

        println!("  ✓ Registered 3 language agents");

        // Facilitate pattern exchange
        println!("\nFacilitating pattern exchange...");
        coordinator.facilitate_pattern_exchange().await?;
        println!("  ✓ Patterns shared among agents");

        // Detect emergent behaviors
        println!("\nDetecting emergent behaviors...");
        coordinator.detect_emergent_behaviors().await?;

        let behaviors = coordinator.get_emergent_behaviors().await;
        println!("  ✓ Detected {} emergent behaviors", behaviors.len());

        for behavior in &behaviors {
            println!("\n  Behavior: {}", behavior.name);
            println!("    Strength: {:.2}", behavior.strength);
            println!("    Languages: {:?}", behavior.languages);
            println!("    Agents: {}", behavior.participating_agents.len());
        }

        // Synthesize polyglot code
        println!("\nSynthesizing polyglot code...");

        let requirements = PolyglotRequirements {
            description: "Create a REST API server".to_string(),
            target_languages: vec![Language::Rust, Language::Python, Language::Go],
            quality_requirements: QualityRequirements {
                min_correctness: 0.9,
                min_readability: 0.8,
                min_performance: 0.75,
            },
            cross_language_constraints: vec![],
        };

        let solution = coordinator.synthesize_polyglot_code(&requirements).await?;

        println!("  ✓ Generated code for {} languages", solution.outputs.len());
        println!("  Quality score: {:.4}", solution.quality_score);

        println!("\nGenerated code:");
        for (language, _code) in solution.outputs {
            println!("  - {:?}: Generated ✓", language);
        }

        Ok(())
    }
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("\n╔════════════════════════════════════════════════════════╗");
    println!("║    Swarm Intelligence Code Generator Demo              ║");
    println!("╚════════════════════════════════════════════════════════╝\n");

    // Run all demos
    demo::demo_aco_sparql().await?;
    demo::demo_pso_template().await?;
    demo::demo_template_evolution().await?;
    demo::demo_polyglot_synthesis().await?;

    println!("\n╔════════════════════════════════════════════════════════╗");
    println!("║    All demos completed successfully!                   ║");
    println!("╚════════════════════════════════════════════════════════╝\n");

    Ok(())
}
