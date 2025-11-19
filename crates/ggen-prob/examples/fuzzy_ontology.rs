//! Fuzzy logic for ambiguous domain modeling
//!
//! This example demonstrates fuzzy logic integration for handling
//! ambiguous domain concepts and uncertain type classification.

use ggen_prob::{
    fuzzy::{FuzzyLogic, FuzzyRule, FuzzySet, MembershipFunction},
    ontology::ProbabilisticOntology,
};
use std::collections::HashMap;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Fuzzy Logic Domain Modeling Example ===\n");

    // Example 1: Define Fuzzy Sets for Type Classification
    println!("1. Fuzzy Sets for Type Features");
    println!("-".repeat(40));

    let mut fuzzy = FuzzyLogic::new();

    // Define fuzzy sets for "has_quotes" feature
    fuzzy.add_set(FuzzySet::new(
        "low_quotes",
        MembershipFunction::Trapezoidal {
            a: 0.0,
            b: 0.0,
            c: 0.3,
            d: 0.5,
        },
        (0.0, 1.0),
    ));

    fuzzy.add_set(FuzzySet::new(
        "high_quotes",
        MembershipFunction::Trapezoidal {
            a: 0.5,
            b: 0.7,
            c: 1.0,
            d: 1.0,
        },
        (0.0, 1.0),
    ));

    // Define fuzzy sets for "has_numbers" feature
    fuzzy.add_set(FuzzySet::new(
        "low_numbers",
        MembershipFunction::Trapezoidal {
            a: 0.0,
            b: 0.0,
            c: 0.3,
            d: 0.5,
        },
        (0.0, 1.0),
    ));

    fuzzy.add_set(FuzzySet::new(
        "high_numbers",
        MembershipFunction::Trapezoidal {
            a: 0.5,
            b: 0.7,
            c: 1.0,
            d: 1.0,
        },
        (0.0, 1.0),
    ));

    // Define output fuzzy sets
    fuzzy.add_set(FuzzySet::new(
        "is_string",
        MembershipFunction::Triangular {
            a: 0.0,
            b: 0.5,
            c: 1.0,
        },
        (0.0, 1.0),
    ));

    fuzzy.add_set(FuzzySet::new(
        "is_numeric",
        MembershipFunction::Triangular {
            a: 0.0,
            b: 0.5,
            c: 1.0,
        },
        (0.0, 1.0),
    ));

    // Example 2: Define Fuzzy Rules
    println!("\nFuzzy Rules:");
    println!("  IF high_quotes THEN is_string");
    println!("  IF high_numbers THEN is_numeric");

    fuzzy.add_rule(
        FuzzyRule::new()
            .if_condition("quotes", "high_quotes")
            .then_action("type", "is_string")
            .with_weight(0.9),
    );

    fuzzy.add_rule(
        FuzzyRule::new()
            .if_condition("numbers", "high_numbers")
            .then_action("type", "is_numeric")
            .with_weight(0.8),
    );

    // Example 3: Test Different Input Values
    println!("\n2. Fuzzy Inference for Different Inputs");
    println!("-".repeat(40));

    let test_cases = vec![
        ("High quotes, low numbers", 0.9, 0.2),
        ("Low quotes, high numbers", 0.2, 0.9),
        ("Ambiguous (medium both)", 0.5, 0.5),
        ("High both (conflicting)", 0.8, 0.8),
    ];

    for (description, quotes, numbers) in test_cases {
        let mut inputs = HashMap::new();
        inputs.insert("quotes".to_string(), quotes);
        inputs.insert("numbers".to_string(), numbers);

        let outputs = fuzzy.infer(&inputs);

        println!("\n{}:", description);
        println!("  Input: quotes={:.1}, numbers={:.1}", quotes, numbers);
        println!("  Output:");
        for (var, membership) in &outputs {
            println!("    {}: {:.3}", var, membership);
        }
    }

    // Example 4: Probabilistic Ontology with Fuzzy Constraints
    println!("\n\n3. Probabilistic Ontology with Fuzzy Types");
    println!("-".repeat(40));

    let mut ontology = ProbabilisticOntology::new("fuzzy_example");

    // Add entities with fuzzy type beliefs
    ontology.add_type_belief("user_id", "String", 0.7);
    ontology.add_type_belief("user_id", "Integer", 0.3);

    ontology.add_type_belief("age", "Integer", 0.95);
    ontology.add_type_belief("age", "String", 0.05);

    ontology.add_type_belief("description", "String", 0.99);
    ontology.add_type_belief("description", "Text", 0.01);

    println!("\nOntology entities:");
    for (entity, belief) in &ontology.entities {
        if let Some((type_name, prob)) = belief.most_likely_type() {
            println!("  {}: {} (confidence: {:.2})", entity, type_name, prob);
        }
    }

    let confidence = ontology.confidence_score();
    println!("\nOntology confidence: {:.2}", confidence.overall);

    // Example 5: Defuzzification
    println!("\n\n4. Defuzzification Example");
    println!("-".repeat(40));

    let result = fuzzy.defuzzify("is_string", 0.8, 100);
    if let Some(crisp_value) = result {
        println!("Defuzzified value for 'is_string' at 0.8 membership: {:.3}", crisp_value);
    }

    println!("\n=== Example Complete ===");

    Ok(())
}
