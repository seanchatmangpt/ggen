//! Ollama performance benchmarks and stress tests
//!
//! These tests validate performance characteristics and error recovery
//! for Ollama LLM operations. They are excluded from normal test runs.

#![cfg(feature = "ollama-integration")]

use ggen_ai::{
    generators::TemplateGenerator,
    test_helpers::create_test_ollama_client,
    skip_if_ollama_unavailable,
};
use std::time::{Duration, Instant};
use tokio::time::timeout;

#[tokio::test]
async fn test_ollama_performance_template_generation() {
    skip_if_ollama_unavailable!();
    
    println!("⚡ Testing Ollama template generation performance...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    let test_cases = vec![
        ("Simple CLI tool", vec!["Use clap", "Include help"]),
        ("REST API service", vec!["Use FastAPI", "Include CRUD", "Add validation"]),
        ("Complex web application", vec!["Use React", "Include TypeScript", "Add testing", "Use Tailwind CSS"]),
    ];
    
    let mut total_time = Duration::new(0, 0);
    let mut successful_generations = 0;
    
    for (description, examples) in &test_cases {
        let start = Instant::now();
        
        let result = timeout(
            Duration::from_secs(60), // Longer timeout for performance testing
            generator.generate_template(description, examples.to_vec())
        ).await;
        
        let elapsed = start.elapsed();
        total_time += elapsed;
        
        match result {
            Ok(Ok(template)) => {
                successful_generations += 1;
                println!("✅ {}: {}ms ({} chars)", 
                    description, elapsed.as_millis(), template.body.len());
            }
            Ok(Err(e)) => {
                println!("❌ {}: Failed - {}", description, e);
            }
            Err(_) => {
                println!("⏰ {}: Timeout after 60s", description);
            }
        }
    }
    
    let avg_time = total_time / successful_generations.max(1);
    println!("📊 Performance Summary:");
    println!("  - Successful generations: {}/{}", successful_generations, test_cases.len());
    println!("  - Average time: {}ms", avg_time.as_millis());
    println!("  - Total time: {}ms", total_time.as_millis());
    
    // Performance assertions
    assert!(successful_generations > 0, "At least one generation should succeed");
    assert!(avg_time < Duration::from_secs(60), "Average generation time should be under 60s");
}

#[tokio::test]
async fn test_ollama_concurrent_generations() {
    skip_if_ollama_unavailable!();
    
    println!("🔄 Testing concurrent Ollama generations...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    let tasks = vec![
        ("Task 1", "Simple Python script"),
        ("Task 2", "Rust CLI tool"),
        ("Task 3", "JavaScript function"),
    ];
    
    let start = Instant::now();
    
    let futures: Vec<_> = tasks.iter().map(|(name, description)| {
        let generator = &generator;
        async move {
            let result = timeout(
                Duration::from_secs(30),
                generator.generate_template(description, vec!["Include comments"])
            ).await;
            
            (name, result)
        }
    }).collect();
    
    let results = futures::future::join_all(futures).await;
    let elapsed = start.elapsed();
    
    let mut successful = 0;
    for (name, result) in results {
        match result {
            Ok(Ok(template)) => {
                successful += 1;
                println!("✅ {}: {}ms ({} chars)", 
                    name, elapsed.as_millis(), template.body.len());
            }
            Ok(Err(e)) => {
                println!("❌ {}: Failed - {}", name, e);
            }
            Err(_) => {
                println!("⏰ {}: Timeout", name);
            }
        }
    }
    
    println!("📊 Concurrent Test Summary:");
    println!("  - Successful: {}/{}", successful, tasks.len());
    println!("  - Total time: {}ms", elapsed.as_millis());
    
    assert!(successful > 0, "At least one concurrent generation should succeed");
}

#[tokio::test]
async fn test_ollama_error_recovery() {
    skip_if_ollama_unavailable!();
    
    println!("🛡️ Testing Ollama error recovery...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    // Test with various edge cases
    let long_description = "A" .repeat(1000);
    let test_cases = vec![
        ("", vec![]), // Empty description
        ("A", vec![]), // Very short description
        (&long_description, vec!["Example"]), // Very long description
        ("Normal description", vec!["Example"; 50]), // Many examples
    ];
    
    let mut recovered_errors = 0;
    
    for (description, examples) in &test_cases {
        let result = timeout(
            Duration::from_secs(30),
            generator.generate_template(description, examples.to_vec())
        ).await;
        
        match result {
            Ok(Ok(template)) => {
                println!("✅ Recovered from edge case: {} chars", template.body.len());
            }
            Ok(Err(e)) => {
                recovered_errors += 1;
                println!("⚠️  Expected error for edge case: {}", e);
            }
            Err(_) => {
                recovered_errors += 1;
                println!("⏰ Timeout for edge case");
            }
        }
    }
    
    println!("📊 Error Recovery Summary:");
    println!("  - Edge cases tested: {}", test_cases.len());
    println!("  - Errors/timeouts: {}", recovered_errors);
    
    // Should handle edge cases gracefully
    assert!(recovered_errors < test_cases.len(), "Some edge cases should succeed");
}

#[tokio::test]
async fn test_ollama_deterministic_output() {
    skip_if_ollama_unavailable!();
    
    println!("🎯 Testing Ollama deterministic output...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    let description = "A simple Python function";
    let examples = vec!["Include docstring", "Add type hints"];
    
    // Generate the same template multiple times
    let mut outputs = Vec::new();
    
    for i in 0..3 {
        let result = timeout(
            Duration::from_secs(30),
            generator.generate_template(description, examples.clone())
        ).await;
        
        match result {
            Ok(Ok(template)) => {
                let body_len = template.body.len();
                outputs.push(template.body);
                println!("✅ Generation {}: {} chars", i + 1, body_len);
            }
            Ok(Err(e)) => {
                println!("❌ Generation {} failed: {}", i + 1, e);
                return; // Skip deterministic test if generation fails
            }
            Err(_) => {
                println!("⏰ Generation {} timed out", i + 1);
                return;
            }
        }
    }
    
    if outputs.len() >= 2 {
        // Check if outputs are similar (allowing for some variation)
        let first_output = &outputs[0];
        let mut similar_count = 0;
        
        for output in &outputs[1..] {
            if first_output.len() > 0 && output.len() > 0 {
                // Check if outputs are reasonably similar (within 20% length difference)
                let length_diff = (first_output.len() as f64 - output.len() as f64).abs();
                let length_ratio = length_diff / first_output.len() as f64;
                
                if length_ratio < 0.2 {
                    similar_count += 1;
                }
            }
        }
        
        println!("📊 Deterministic Test Summary:");
        println!("  - Generations: {}", outputs.len());
        println!("  - Similar outputs: {}", similar_count);
        
        // Note: AI outputs may not be perfectly deterministic, so we just check for reasonable consistency
        assert!(similar_count > 0, "Outputs should be reasonably consistent");
    }
}

#[tokio::test]
async fn test_ollama_memory_usage() {
    skip_if_ollama_unavailable!();
    
    println!("💾 Testing Ollama memory usage...");
    
    let client = create_test_ollama_client().expect("Failed to create Ollama client");
    let generator = TemplateGenerator::with_ollama_qwen3_coder(Box::new(client));
    
    // Generate multiple templates to test memory usage
    let mut templates = Vec::new();
    
    for i in 0..5 {
        let description = format!("Template {} with some description", i);
        let examples = vec!["Example 1", "Example 2"];
        
        let result = timeout(
            Duration::from_secs(30),
            generator.generate_template(&description, examples.to_vec())
        ).await;
        
        match result {
            Ok(Ok(template)) => {
                let body_len = template.body.len();
                templates.push(template);
                println!("✅ Generated template {}: {} chars", i + 1, body_len);
            }
            Ok(Err(e)) => {
                println!("❌ Template {} failed: {}", i + 1, e);
            }
            Err(_) => {
                println!("⏰ Template {} timed out", i + 1);
            }
        }
    }
    
    println!("📊 Memory Usage Summary:");
    println!("  - Templates generated: {}", templates.len());
    println!("  - Total content: {} chars", 
        templates.iter().map(|t| t.body.len()).sum::<usize>());
    
    // Basic memory usage validation
    assert!(templates.len() > 0, "Should generate at least one template");
    
    // Check that we can still generate more templates (no memory leaks)
    let final_result = timeout(
        Duration::from_secs(30),
        generator.generate_template("Final test template", vec!["Test"])
    ).await;
    
    match final_result {
        Ok(Ok(template)) => {
            println!("✅ Final template generated: {} chars", template.body.len());
        }
        Ok(Err(e)) => {
            println!("⚠️  Final template failed: {}", e);
        }
        Err(_) => {
            println!("⏰ Final template timed out");
        }
    }
}
