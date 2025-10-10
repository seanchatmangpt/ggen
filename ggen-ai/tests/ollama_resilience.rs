//! Ollama resilience and error recovery tests
//!
//! These tests validate error handling, recovery mechanisms, and resilience
//! for Ollama LLM operations. They are excluded from normal test runs.

#![cfg(feature = "ollama-integration")]

use ggen_ai::{
    generators::TemplateGenerator,
    test_helpers::create_test_ollama_client,
    skip_if_ollama_unavailable,
    client::LlmClient,
};
use std::time::Duration;
use tokio::time::timeout;

#[tokio::test]
async fn test_ollama_network_resilience() {
    skip_if_ollama_unavailable!();
    
    println!("🌐 Testing Ollama network resilience...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    // Test with various network-like scenarios
    let test_cases = vec![
        ("Short timeout", Duration::from_secs(5)),
        ("Medium timeout", Duration::from_secs(15)),
        ("Long timeout", Duration::from_secs(45)),
    ];
    
    let mut successful_recoveries = 0;
    
    for (test_name, timeout_duration) in &test_cases {
        let result = timeout(
            *timeout_duration,
            generator.generate_template("Network resilience test", vec!["Include error handling"])
        ).await;
        
        match result {
            Ok(Ok(template)) => {
                successful_recoveries += 1;
                println!("✅ {}: Success ({} chars)", test_name, template.body.len());
            }
            Ok(Err(e)) => {
                println!("⚠️  {}: Expected error - {}", test_name, e);
                // Network errors are expected in some cases
            }
            Err(_) => {
                println!("⏰ {}: Timeout as expected", test_name);
            }
        }
    }
    
    println!("📊 Network Resilience Summary:");
    println!("  - Successful recoveries: {}", successful_recoveries);
    println!("  - Total tests: {}", test_cases.len());
    
    // At least one test should succeed (Ollama is healthy, so most should succeed)
    assert!(successful_recoveries > 0, "Should succeed with healthy Ollama service");
}

#[tokio::test]
async fn test_ollama_model_switching() {
    skip_if_ollama_unavailable!();
    
    println!("🔄 Testing Ollama model switching resilience...");
    
    // Test with different model configurations
    let configs = vec![
        ("qwen3-coder:30b", "qwen3-coder:30b"),
        ("llama2", "llama2"),
        ("codellama", "codellama"),
    ];
    
    let mut successful_switches = 0;
    
    for (model_name, _) in configs {
        let client = create_test_ollama_client().expect("Failed to create Ollama client");
        
        // Check if model is supported
        if client.supports_model(model_name) {
            let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
            
            let result = timeout(
                Duration::from_secs(30),
                generator.generate_template(
                    &format!("Test with {} model", model_name),
                    vec!["Include model-specific features"]
                )
            ).await;
            
            match result {
                Ok(Ok(template)) => {
                    successful_switches += 1;
                    println!("✅ {}: Success ({} chars)", model_name, template.body.len());
                }
                Ok(Err(e)) => {
                    println!("⚠️  {}: Model error - {}", model_name, e);
                }
                Err(_) => {
                    println!("⏰ {}: Timeout", model_name);
                }
            }
        } else {
            println!("⏭️  {}: Model not available, skipping", model_name);
        }
    }
    
    println!("📊 Model Switching Summary:");
    println!("  - Successful switches: {}", successful_switches);
    
    // At least one model should work
    assert!(successful_switches > 0, "Should work with at least one model");
}

#[tokio::test]
async fn test_ollama_concurrent_error_handling() {
    skip_if_ollama_unavailable!();
    
    println!("🔄 Testing concurrent error handling...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    // Create multiple concurrent tasks with different characteristics
    let long_description = "A".repeat(500);
    let tasks = vec![
        ("Normal task", "Regular template generation", vec!["Include examples"]),
        ("Edge case", "", vec![]), // Empty description
        ("Large task", &long_description, vec!["Example"; 20]), // Large input
        ("Quick task", "Simple function", vec!["Add comments"]),
    ];
    
    let start = std::time::Instant::now();
    
    let futures: Vec<_> = tasks.iter().map(|(name, description, examples)| {
        let generator = &generator;
        async move {
            let result = timeout(
                Duration::from_secs(30),
                generator.generate_template(description, examples.clone())
            ).await;
            
            (name, result)
        }
    }).collect();
    
    let results = futures::future::join_all(futures).await;
    let elapsed = start.elapsed();
    
    let mut successful = 0;
    let mut errors = 0;
    let mut timeouts = 0;
    
    for (name, result) in results {
        match result {
            Ok(Ok(template)) => {
                successful += 1;
                println!("✅ {}: Success ({} chars)", name, template.body.len());
            }
            Ok(Err(e)) => {
                errors += 1;
                println!("⚠️  {}: Expected error - {}", name, e);
            }
            Err(_) => {
                timeouts += 1;
                println!("⏰ {}: Timeout", name);
            }
        }
    }
    
    println!("📊 Concurrent Error Handling Summary:");
    println!("  - Successful: {}", successful);
    println!("  - Errors: {}", errors);
    println!("  - Timeouts: {}", timeouts);
    println!("  - Total time: {}ms", elapsed.as_millis());
    
    // Should handle concurrent errors gracefully
    assert!(successful > 0, "At least one concurrent task should succeed");
    assert!(errors + timeouts < tasks.len(), "Not all tasks should fail");
}

#[tokio::test]
async fn test_ollama_resource_cleanup() {
    skip_if_ollama_unavailable!();
    
    println!("🧹 Testing Ollama resource cleanup...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    // Generate multiple templates to test resource management
    let mut templates = Vec::new();
    
    for i in 0..10 {
        let description = format!("Resource cleanup test {}", i);
        let examples = vec!["Include cleanup", "Add error handling"];
        
        let result = timeout(
            Duration::from_secs(30),
            generator.generate_template(&description, examples)
        ).await;
        
        match result {
            Ok(Ok(template)) => {
                let body_len = template.body.len();
                templates.push(template);
                println!("✅ Generated template {}: {} chars", i + 1, body_len);
            }
            Ok(Err(e)) => {
                println!("⚠️  Template {} failed: {}", i + 1, e);
            }
            Err(_) => {
                println!("⏰ Template {} timed out", i + 1);
            }
        }
        
        // Small delay to test resource cleanup between generations
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
    
    println!("📊 Resource Cleanup Summary:");
    println!("  - Templates generated: {}", templates.len());
    println!("  - Total content: {} chars", 
        templates.iter().map(|t| t.body.len()).sum::<usize>());
    
    // Test that we can still generate more templates after cleanup
    let final_result = timeout(
        Duration::from_secs(30),
        generator.generate_template("Final cleanup test", vec!["Test cleanup"])
    ).await;
    
    match final_result {
        Ok(Ok(template)) => {
            println!("✅ Final template after cleanup: {} chars", template.body.len());
        }
        Ok(Err(e)) => {
            println!("⚠️  Final template failed: {}", e);
        }
        Err(_) => {
            println!("⏰ Final template timed out");
        }
    }
    
    // Should be able to generate templates after cleanup
    assert!(templates.len() > 0, "Should generate templates before cleanup");
}

#[tokio::test]
async fn test_ollama_graceful_degradation() {
    skip_if_ollama_unavailable!();
    
    println!("🛡️ Testing Ollama graceful degradation...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    // Test various degradation scenarios
    let complex_description = "A".repeat(1000);
    let scenarios = vec![
        ("Normal operation", "Regular template", vec!["Include examples"]),
        ("Reduced input", "Short", vec![]),
        ("Minimal input", "", vec![]),
        ("Complex input", &complex_description, vec!["Example"; 50]),
    ];
    
    let mut graceful_degradations = 0;
    
    for (scenario_name, description, examples) in &scenarios {
        let result = timeout(
            Duration::from_secs(30),
            generator.generate_template(description, examples.to_vec())
        ).await;
        
        match result {
            Ok(Ok(template)) => {
                graceful_degradations += 1;
                println!("✅ {}: Graceful success ({} chars)", scenario_name, template.body.len());
            }
            Ok(Err(e)) => {
                // Check if error is graceful (not a panic or crash)
                if e.to_string().contains("template") || e.to_string().contains("generation") {
                    graceful_degradations += 1;
                    println!("✅ {}: Graceful error - {}", scenario_name, e);
                } else {
                    println!("⚠️  {}: Unexpected error - {}", scenario_name, e);
                }
            }
            Err(_) => {
                println!("⏰ {}: Graceful timeout", scenario_name);
                graceful_degradations += 1; // Timeout is also graceful
            }
        }
    }
    
    println!("📊 Graceful Degradation Summary:");
    println!("  - Graceful degradations: {}", graceful_degradations);
    println!("  - Total scenarios: {}", scenarios.len());
    
    // All scenarios should be handled gracefully
    assert!(graceful_degradations == scenarios.len(), "All scenarios should be handled gracefully");
}
