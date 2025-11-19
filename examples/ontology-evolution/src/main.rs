//! AI-Native Ontology Evolution Example
//!
//! This example demonstrates the complete ontology evolution system with
//! RL feedback loops, quantum template selection, and neural symbolic reasoning.

use ggen_ai::{
    EvolutionConfig, FeedbackSignal, InferenceRule, LanguageTarget, NeuralSymbolicReasoner,
    OntologyEvolutionCoordinator, Pattern, PatternTerm, PolyglotTypeInferencer,
    QuantumTemplateSelector, RLFeedbackLoop, SchemaEvolution,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧬 AI-Native Ontology Evolution Engine");
    println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");

    // Example 1: RL Feedback Loop
    example_rl_feedback().await?;

    // Example 2: Quantum Template Selection
    example_quantum_selection().await?;

    // Example 3: Neural Symbolic Reasoning
    example_neural_symbolic().await?;

    // Example 4: Polyglot Type Inference
    example_type_inference().await?;

    // Example 5: Complete Evolution Cycle
    example_complete_evolution().await?;

    println!("\n✨ All examples completed successfully!");

    Ok(())
}

async fn example_rl_feedback() -> Result<(), Box<dyn std::error::Error>> {
    println!("📊 Example 1: RL Feedback Loop");
    println!("─────────────────────────────────────────\n");

    let rl = RLFeedbackLoop::new(0.1, 0.95, 0.1);

    let actions = vec![
        SchemaEvolution::AddClass {
            name: "User".to_string(),
            parent: Some("Thing".to_string()),
        },
        SchemaEvolution::AddProperty {
            name: "email".to_string(),
            domain: "User".to_string(),
            range: "String".to_string(),
        },
        SchemaEvolution::AddConstraint {
            property: "email".to_string(),
            rule: "format:email".to_string(),
        },
    ];

    println!("Available schema evolutions:");
    for (i, action) in actions.iter().enumerate() {
        println!("  {}. {:?}", i + 1, action);
    }
    println!();

    // Simulate 5 learning iterations
    for iteration in 1..=5 {
        println!("Iteration {}", iteration);

        // Select action
        let action = rl.select_action("state", &actions).await?;
        println!("  Selected: {:?}", action);

        // Simulate feedback
        let feedback = if rand::random::<f64>() > 0.3 {
            FeedbackSignal::CompilationSuccess {
                language: "rust".to_string(),
                time_ms: 300 + rand::random::<u64>() % 200,
            }
        } else {
            FeedbackSignal::CompilationFailure {
                language: "rust".to_string(),
                error: "Type mismatch".to_string(),
            }
        };

        let reward = feedback.to_reward();
        println!("  Feedback: {:?}", feedback);
        println!("  Reward: {:.4}", reward);

        // Update Q-values
        rl.process_feedback("state", &action, feedback, "next_state", &actions)
            .await?;
        println!();
    }

    // Get statistics
    let stats = rl.get_statistics().await;
    println!("Learning Statistics:");
    println!("  Total states: {}", stats.total_states);
    println!("  Total actions: {}", stats.total_actions);
    println!("  Average Q-value: {:.4}", stats.avg_q_value);
    println!("  Average reward: {:.4}", stats.avg_reward);
    println!();

    Ok(())
}

async fn example_quantum_selection() -> Result<(), Box<dyn std::error::Error>> {
    println!("⚛️  Example 2: Quantum Template Selection");
    println!("─────────────────────────────────────────\n");

    let templates: Vec<String> = vec![
        "minimal".to_string(),
        "standard".to_string(),
        "advanced".to_string(),
        "enterprise".to_string(),
    ];

    println!("Available templates:");
    for template in &templates {
        println!("  - {}", template);
    }
    println!();

    // Oracle function that scores templates
    let oracle = |template: &str| -> f64 {
        match template {
            "minimal" => 0.6,
            "standard" => 0.8,
            "advanced" => 0.9,
            "enterprise" => 0.95,
            _ => 0.5,
        }
    };

    let mut selector = QuantumTemplateSelector::new(templates.clone(), 1.0, oracle);

    println!("Initial quantum distribution:");
    let dist = selector.get_distribution();
    for state in &dist.states {
        println!(
            "  {}: amplitude={:.4}, probability={:.4}",
            state.template_id,
            state.amplitude,
            state.probability()
        );
    }
    println!();

    // Apply amplitude amplification
    println!("Applying Grover-like amplitude amplification...");
    selector.amplify_good_templates(3)?;

    println!("\nAmplified quantum distribution:");
    let dist = selector.get_distribution();
    for state in &dist.states {
        println!(
            "  {}: amplitude={:.4}, probability={:.4}",
            state.template_id,
            state.amplitude,
            state.probability()
        );
    }
    println!();

    // Perform quantum annealing
    println!("Running quantum annealing optimization...");
    let selected = selector.select_with_annealing(50)?;
    println!("Selected template: {}\n", selected);

    Ok(())
}

async fn example_neural_symbolic() -> Result<(), Box<dyn std::error::Error>> {
    println!("🧠 Example 3: Neural Symbolic Reasoning");
    println!("─────────────────────────────────────────\n");

    let mut reasoner = NeuralSymbolicReasoner::new(64);

    // Add domain knowledge
    println!("Building knowledge base:");
    reasoner.add_triple(
        "Dog".to_string(),
        "subClassOf".to_string(),
        "Animal".to_string(),
    );
    println!("  Dog subClassOf Animal");

    reasoner.add_triple(
        "Cat".to_string(),
        "subClassOf".to_string(),
        "Animal".to_string(),
    );
    println!("  Cat subClassOf Animal");

    reasoner.add_triple(
        "Fido".to_string(),
        "instanceOf".to_string(),
        "Dog".to_string(),
    );
    println!("  Fido instanceOf Dog");

    reasoner.add_triple(
        "Whiskers".to_string(),
        "instanceOf".to_string(),
        "Cat".to_string(),
    );
    println!("  Whiskers instanceOf Cat");
    println!();

    // Add inference rule: X instanceOf Y, Y subClassOf Z => X instanceOf Z
    println!("Adding transitive inference rule:");
    println!("  IF X instanceOf Y AND Y subClassOf Z");
    println!("  THEN X instanceOf Z\n");

    let rule = InferenceRule {
        name: "transitive_instance".to_string(),
        premises: vec![
            Pattern {
                subject: PatternTerm::Variable("x".to_string()),
                predicate: PatternTerm::Constant("instanceOf".to_string()),
                object: PatternTerm::Variable("y".to_string()),
            },
            Pattern {
                subject: PatternTerm::Variable("y".to_string()),
                predicate: PatternTerm::Constant("subClassOf".to_string()),
                object: PatternTerm::Variable("z".to_string()),
            },
        ],
        conclusions: vec![Pattern {
            subject: PatternTerm::Variable("x".to_string()),
            predicate: PatternTerm::Constant("instanceOf".to_string()),
            object: PatternTerm::Variable("z".to_string()),
        }],
        confidence: 1.0,
    };

    reasoner.add_rule(rule);

    // Perform forward chaining inference
    println!("Performing forward chaining inference...");
    let inferred = reasoner.forward_chain()?;

    println!("\nInferred triples:");
    for (s, p, o) in &inferred {
        println!("  {} {} {}", s, p, o);
    }
    println!();

    // Train neural embeddings
    println!("Training neural embeddings...");
    reasoner.train_embeddings(50, 0.01).await?;
    println!("  ✓ Embeddings trained\n");

    // Get entity similarity
    let neural = reasoner.get_neural_representation();
    let similarity = neural.entity_similarity("Dog", "Cat");
    println!("Semantic similarity:");
    println!("  Dog <-> Cat: {:.4}\n", similarity);

    Ok(())
}

async fn example_type_inference() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔤 Example 4: Polyglot Type Inference");
    println!("─────────────────────────────────────────\n");

    let inferencer = PolyglotTypeInferencer::new();

    println!("Inferring collection types across languages:\n");

    // List types
    println!("List<String>:");
    for lang in &[
        LanguageTarget::Rust,
        LanguageTarget::TypeScript,
        LanguageTarget::Python,
        LanguageTarget::Go,
        LanguageTarget::Java,
    ] {
        let sig = inferencer.infer_collection("String", "List", lang)?;
        println!("  {:?}: {}", lang, sig.type_name);
    }
    println!();

    // Map types
    println!("Map<String, Int>:");
    for lang in &[
        LanguageTarget::Rust,
        LanguageTarget::TypeScript,
        LanguageTarget::Python,
    ] {
        let sig = inferencer.infer_collection("String, Int", "Map", lang)?;
        println!("  {:?}: {}", lang, sig.type_name);
    }
    println!();

    // Nullable types
    println!("Nullable String:");
    for lang in &[
        LanguageTarget::Rust,
        LanguageTarget::TypeScript,
        LanguageTarget::Python,
        LanguageTarget::Go,
    ] {
        let base_sig = inferencer.infer_collection("String", "List", lang)?;
        let nullable = inferencer.make_nullable(&base_sig)?;
        println!("  {:?}: {}", lang, nullable.type_name);
    }
    println!();

    Ok(())
}

async fn example_complete_evolution() -> Result<(), Box<dyn std::error::Error>> {
    println!("🌟 Example 5: Complete Evolution Cycle");
    println!("─────────────────────────────────────────\n");

    let config = EvolutionConfig {
        learning_rate: 0.05,
        discount_factor: 0.95,
        exploration_rate: 0.15,
        quantum_temperature: 1.0,
        quantum_states: 64,
        max_iterations: 5,
        target_languages: vec![
            "rust".to_string(),
            "typescript".to_string(),
            "python".to_string(),
        ],
        ..Default::default()
    };

    println!("Configuration:");
    println!("  Learning rate: {}", config.learning_rate);
    println!("  Discount factor: {}", config.discount_factor);
    println!("  Exploration rate: {}", config.exploration_rate);
    println!("  Quantum temperature: {}", config.quantum_temperature);
    println!("  Target languages: {:?}", config.target_languages);
    println!();

    println!("Initializing evolution coordinator...");
    let coordinator = OntologyEvolutionCoordinator::new(config);
    println!("  ✓ Coordinator initialized\n");

    println!("Running evolution cycles:\n");
    let cycles = coordinator.evolve_n_cycles(5).await?;

    for cycle in &cycles {
        println!("Cycle {}:", cycle.cycle);
        println!("  Template: {}", cycle.template);
        println!("  Evolution: {:?}", cycle.evolution);
        println!("  Reward: {:.4}", cycle.reward);
        println!("  Inferences: {} new triples", cycle.inferences.len());
        println!("  Generated code for:");
        for (lang, _) in &cycle.code_samples {
            println!("    ✓ {}", lang);
        }
        println!();
    }

    let metrics = coordinator.get_metrics().await;
    println!("Final Metrics:");
    println!("  Cycles completed: {}", metrics.iterations_completed);
    println!("  Schema modifications: {}", metrics.schema_modifications);
    println!("  Cumulative reward: {:.4}", metrics.cumulative_reward);
    println!("  Converged: {}", metrics.converged);
    println!("\n  Language quality scores:");
    for (lang, score) in &metrics.language_quality_scores {
        println!("    {}: {:.4}", lang, score);
    }
    println!();

    // Export evolved ontology
    let ontology = coordinator.export_ontology().await?;
    println!("Evolved Ontology (first 500 chars):");
    println!("{}", &ontology[..ontology.len().min(500)]);
    if ontology.len() > 500 {
        println!("...\n");
    } else {
        println!();
    }

    Ok(())
}
