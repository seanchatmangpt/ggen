//! Test example for AdaptiveLearningModel
//!
//! Run with: cargo run -p ggen-ai --example test_learning_model --features swarm

use ggen_ai::swarm::agents::{AdaptiveLearningModel, LearningData, LearningModel, PredictionInput, ModelUpdates};
use std::collections::HashMap;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Testing AdaptiveLearningModel...");

    // Create model
    let mut model = AdaptiveLearningModel::new();
    println!("✓ Model created");

    // Create training data
    let mut features = HashMap::new();
    features.insert("feature1".to_string(), 0.8);
    features.insert("feature2".to_string(), 0.6);

    let mut targets = HashMap::new();
    targets.insert("feature1".to_string(), 0.85);
    targets.insert("feature2".to_string(), 0.65);

    let learning_data = LearningData {
        features,
        targets,
        metadata: HashMap::new(),
    };

    // Train model
    let result = model.train(&learning_data).await?;
    println!("✓ Training complete: accuracy={:.2}, loss={:.4}", result.accuracy, result.loss);

    // Make prediction
    let mut pred_features = HashMap::new();
    pred_features.insert("feature1".to_string(), 0.8);
    pred_features.insert("feature2".to_string(), 0.6);

    let input = PredictionInput {
        features: pred_features,
        context: HashMap::new(),
    };

    let output = model.predict(&input).await?;
    println!("✓ Prediction complete: confidence={:.2}, predictions={}",
             output.confidence, output.predictions.len());

    // Update parameters
    let mut updates = HashMap::new();
    updates.insert("param1".to_string(), 0.1);

    let model_updates = ModelUpdates {
        parameter_updates: updates,
        learning_rate: 0.05,
        metadata: HashMap::new(),
    };

    model.update_parameters(&model_updates).await?;
    println!("✓ Parameters updated");

    println!("\n✅ All tests passed!");
    Ok(())
}
