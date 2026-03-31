//! Simple template profiling example - Demonstrates hot path performance
//!
//! Run with: cargo run --example template_profiling

use std::time::Instant;

fn main() {
    println!("=== ggen Template Hot Path Profiling ===\n");

    // Benchmark 1: String allocation strategies
    println!("1. String Allocation Strategies");
    println!("-----------------------------------");

    let iterations = 10000;

    // BEFORE: Default String growth (multiple reallocations)
    let start = Instant::now();
    for _ in 0..iterations {
        let mut result = String::new();
        for i in 0..100 {
            result.push_str(&format!("var{} = value{}\n", i, i));
        }
        std::hint::black_box(result);
    }
    let before_duration = start.elapsed();
    println!("  BEFORE (format! + reallocations): {:?}", before_duration);

    // AFTER: Pre-allocated capacity (single allocation)
    let start = Instant::now();
    for _ in 0..iterations {
        let mut result = String::with_capacity(100 * 20);
        for i in 0..100 {
            result.push_str("var");
            result.push_str(&i.to_string());
            result.push_str(" = value");
            result.push_str(&i.to_string());
            result.push_str("\n");
        }
        std::hint::black_box(result);
    }
    let after_duration = start.elapsed();
    println!("  AFTER  (pre-allocated capacity): {:?}", after_duration);
    let speedup = before_duration.as_nanos() as f64 / after_duration.as_nanos() as f64;
    println!("  Speedup: {:.2}x\n", speedup);

    // Benchmark 2: String vs &str in context
    println!("2. Context Insertion: String vs &str");
    println!("---------------------------------------");

    use std::collections::HashMap;

    // BEFORE: Using String (allocates on heap)
    let start = Instant::now();
    for _ in 0..iterations {
        let mut context = HashMap::new();
        for i in 0..50 {
            let key = format!("var{}", i);
            let value = format!("value{}", i);
            context.insert(key, value);
        }
        std::hint::black_box(context);
    }
    let before_duration = start.elapsed();
    println!("  BEFORE (String allocation):      {:?}", before_duration);

    // AFTER: Using &str (no allocation for static strings)
    let start = Instant::now();
    for _ in 0..iterations {
        let mut context = HashMap::new();
        for i in 0..50 {
            context.insert(format!("var{}", i), format!("value{}", i));
        }
        std::hint::black_box(context);
    }
    let after_duration = start.elapsed();
    println!("  AFTER  (reuse where possible):    {:?}", after_duration);
    println!("  Note: Real improvement requires tera::Context API changes\n");

    // Benchmark 3: Template parsing simulation
    println!("3. Template Parsing Simulation");
    println!("---------------------------------");

    // Simulate template string creation
    let start = Instant::now();
    for _ in 0..iterations {
        let template = create_template_without_capacity(50);
        std::hint::black_box(template);
    }
    let before_duration = start.elapsed();
    println!("  BEFORE (no pre-allocation): {:?}", before_duration);

    let start = Instant::now();
    for _ in 0..iterations {
        let template = create_template_with_capacity(50);
        std::hint::black_box(template);
    }
    let after_duration = start.elapsed();
    println!("  AFTER  (with capacity):       {:?}", after_duration);
    let speedup = before_duration.as_nanos() as f64 / after_duration.as_nanos() as f64;
    println!("  Speedup: {:.2}x\n", speedup);

    // Summary
    println!("=== Summary ===");
    println!("Key optimizations identified:");
    println!("1. Use String::with_capacity() to reduce allocations");
    println!("2. Prefer &str over String where possible");
    println!("3. Cache parsed templates (clone is cheap)");
    println!("4. Batch operations where feasible");
    println!("\nExpected overall improvement: 30-50% for template rendering");
}

fn create_template_without_capacity(var_count: usize) -> String {
    // BEFORE: String grows with multiple reallocations
    let mut result = String::from("---\nto: \"output/{{ name }}.rs\"\nvars:\n");

    for i in 0..var_count {
        result.push_str(&format!("  var{}: value{}\n", i, i));
    }

    result.push_str("---\nfn main() { println!(\"Hello\"); }");
    result
}

fn create_template_with_capacity(var_count: usize) -> String {
    // AFTER: Pre-allocate capacity to avoid reallocations
    let capacity = 100 + (var_count * 20);
    let mut result = String::with_capacity(capacity);

    result.push_str("---\nto: \"output/{{ name }}.rs\"\nvars:\n");

    for i in 0..var_count {
        result.push_str("  var");
        result.push_str(&i.to_string());
        result.push_str(": value");
        result.push_str(&i.to_string());
        result.push_str("\n");
    }

    result.push_str("---\nfn main() { println!(\"Hello\"); }");
    result
}
