//! Basic example of probabilistic type inference
//!
//! This example demonstrates how to use the probabilistic framework
//! to infer types from uncertain data with confidence intervals.

use ggen_prob::{
    bayesian::{BayesianTypeInference, TypeEvidence},
    confidence::ConfidenceScoreBuilder,
    synthesis::{DataSample, FieldValue, StatisticalSynthesis, SynthesisConfig},
    types::TypeDistribution,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Probabilistic Type Inference Example ===\n");

    // Example 1: Bayesian Type Inference
    println!("1. Bayesian Type Inference");
    println!("-" .repeat(40));

    let mut priors = TypeDistribution::new();
    priors.add("String", 0.4);
    priors.add("Integer", 0.3);
    priors.add("Float", 0.3);

    let mut inference = BayesianTypeInference::new(priors);

    // Add evidence from data observations
    inference.add_evidence(TypeEvidence::new("String", 0.9, "has_quotes"));
    inference.add_evidence(TypeEvidence::new("String", 0.8, "no_numbers"));

    let posterior = inference.compute_posterior();
    println!("Prior distribution:");
    println!("  String: 0.4, Integer: 0.3, Float: 0.3");
    println!("\nPosterior distribution after evidence:");
    for (type_name, prob) in &posterior.types {
        println!("  {}: {:.3}", type_name, prob);
    }

    // Example 2: Statistical Type Synthesis from Noisy Data
    println!("\n\n2. Statistical Type Synthesis");
    println!("-".repeat(40));

    let config = SynthesisConfig::default();
    let mut synthesis = StatisticalSynthesis::new(config);

    // Add noisy data samples
    for i in 0..20 {
        let sample = DataSample::new(format!("sample_{}", i))
            .with_field("user_id", FieldValue::String(format!("{}", 1000 + i)))
            .with_field("age", FieldValue::Integer(20 + i))
            .with_field("score", FieldValue::Float(85.5 + i as f64));

        synthesis.add_sample(sample);
    }

    // Add some noisy samples
    for i in 20..25 {
        let sample = DataSample::new(format!("sample_{}", i))
            .with_field("user_id", FieldValue::String("invalid".to_string()))
            .with_field("age", FieldValue::String("unknown".to_string()));

        synthesis.add_sample(sample);
    }

    // Synthesize types
    let types = synthesis.synthesize_all();
    println!("\nInferred types from 25 samples (20 clean, 5 noisy):");
    for (field, ptype) in &types {
        if let Some((type_name, prob)) = ptype.most_likely_type() {
            let confidence = synthesis.confidence(field);
            println!(
                "  {}: {} (prob: {:.2}, confidence: {:.2})",
                field, type_name, prob, confidence.overall
            );
        }
    }

    // Example 3: Confidence Score Building
    println!("\n\n3. Confidence Score Computation");
    println!("-".repeat(40));

    let score = ConfidenceScoreBuilder::new()
        .component("data_quality", 0.85)
        .component("sample_size", 0.90)
        .component("type_consistency", 0.80)
        .component("schema_completeness", 0.75)
        .build(&[0.3, 0.2, 0.3, 0.2]); // Weighted average

    println!("\nConfidence score components:");
    for comp in &score.components {
        println!("  {}: {:.2}", comp.name, comp.score);
    }
    println!("\nOverall confidence: {:.2}", score.overall);
    println!("Uncertainty: {:.2}", score.uncertainty());

    println!("\n=== Example Complete ===");

    Ok(())
}
