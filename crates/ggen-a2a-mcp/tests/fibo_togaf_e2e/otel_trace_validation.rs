//! OTEL Trace Validation for 70-Turn FIBO + TOGAF E2E
//!
//! Validates that every turn produces required OTEL spans.
//! Tests verify:
//! - All 70 turns have proper span attributes
//! - ggen sync operations (initial + final) are traced
//! - FIBO concepts are traced in data architecture turns
//! - TOGAF phases are properly annotated in spans

use ggen_a2a_mcp::{
    test_utils::{
        fibo_togaf_e2e::{run_70_turn_scenario, TurnExecution},
        init_tracing,
    },
};
use tracing::{span, Level};
use std::collections::HashMap;

/// Helper macro to assert span exists with required attributes
macro_rules! assert_has_span {
    ($execution:expr, span_name: $name:expr, required_attrs: $attrs:expr) => {
        {
            let span = $execution.spans.get($name)
                .ok_or_else(|| anyhow::anyhow!("Missing span: {}", $name))?;
            
            for attr in $attrs {
                assert!(span.attributes.contains_key(attr),
                    "Span {} missing required attribute: {}", $name, attr);
            }
        }
    };
}

/// Test that all 70 turns produce OTEL spans with required attributes
#[tokio::test]
async fn test_all_70_turns_have_otel_spans() {
    init_tracing();
    
    let _guard = span!(Level::INFO, "test_all_70_turns_have_otel_spans").entered();
    
    // Run full 70-turn scenario
    let execution = run_70_turn_scenario().await;
    
    // Validate each turn has required spans
    let mut missing_spans = vec![];
    let mut incomplete_spans = vec![];
    
    for turn in 1..=70 {
        let span_name = format!("turn_{}", turn);
        
        match execution.spans.get(&span_name) {
            None => {
                missing_spans.push(turn);
            }
            Some(span) => {
                // Check required attributes
                let required = [
                    "turn.number",
                    "agent.id",
                    "agent.phase",
                    "ggen.fibo.concepts_used",
                ];
                
                let missing: Vec<_> = required.iter()
                    .filter(|attr| !span.attributes.contains_key(*attr))
                    .collect();
                
                if !missing.is_empty() {
                    incomplete_spans.push((turn, missing));
                }
            }
        }
    }
    
    // Report results
    println!("\n=== OTEL Span Validation for 70 Turns ===");
    println!("Total turns: 70");
    println!("Turns with spans: {}", 70 - missing_spans.len());
    println!("Turns missing spans: {}", missing_spans.len());
    println!("Turns with incomplete spans: {}", incomplete_spans.len());
    
    if !missing_spans.is_empty() {
        println!("\n❌ Missing spans for turns: {:?}", missing_spans);
    }
    
    if !incomplete_spans.is_empty() {
        println!("\n⚠️  Incomplete spans:");
        for (turn, missing) in &incomplete_spans {
            println!("  Turn {}: missing {:?}", turn, missing);
        }
    }
    
    assert!(missing_spans.is_empty(), 
        "Missing OTEL spans for turns: {:?}", missing_spans);
    assert!(incomplete_spans.is_empty(),
        "Incomplete OTEL spans for turns: {:?}", incomplete_spans);
    
    println!("\n✅ All 70 turns have complete OTEL spans");
}

/// Test ggen sync operations produce proper spans
#[tokio::test]
async fn test_ggen_sync_otel_spans() {
    init_tracing();
    
    let _guard = span!(Level::INFO, "test_ggen_sync_otel_spans").entered();
    
    // Run scenario which includes ggen sync at start and end
    let execution = run_70_turn_scenario().await;
    
    // Validate initial ggen sync span
    let initial_sync = execution.spans.get("ggen.sync.initial")
        .expect("Missing initial ggen.sync span");
    
    let required_initial = [
        "ggen.phase",
        "ggen.agents_generated",
        "ggen.fibo_ontologies_loaded",
        "ggen.sync.duration_ms",
    ];
    
    for attr in &required_initial {
        assert!(initial_sync.attributes.contains_key(attr),
            "Initial ggen.sync missing attribute: {}", attr);
    }
    
    assert_eq!(initial_sync.attributes.get("ggen.phase"), Some(&"initial".to_string()),
        "Initial sync should have phase=initial");
    
    // Validate final ggen sync span
    let final_sync = execution.spans.get("ggen.sync.final")
        .expect("Missing final ggen.sync span");
    
    let required_final = [
        "ggen.phase",
        "ggen.code_generated",
        "ggen.files_written",
        "ggen.sync.duration_ms",
    ];
    
    for attr in &required_final {
        assert!(final_sync.attributes.contains_key(attr),
            "Final ggen.sync missing attribute: {}", attr);
    }
    
    assert_eq!(final_sync.attributes.get("ggen.phase"), Some(&"final".to_string()),
        "Final sync should have phase=final");
    
    println!("\n=== ggen sync OTEL Span Validation ===");
    println!("✅ Initial sync span present");
    println!("  Phase: {}", initial_sync.attributes.get("ggen.phase").unwrap());
    println!("  Duration: {}ms", initial_sync.attributes.get("ggen.sync.duration_ms").unwrap());
    
    println!("\n✅ Final sync span present");
    println!("  Phase: {}", final_sync.attributes.get("ggen.phase").unwrap());
    println!("  Duration: {}ms", final_sync.attributes.get("ggen.sync.duration_ms").unwrap());
    println!("  Files written: {}", final_sync.attributes.get("ggen.files_written").unwrap());
}

/// Test that FIBO concepts are traced in data architecture turns (Phase C: turns 17-28)
#[tokio::test]
async fn test_fibo_concepts_traced_in_spans() {
    init_tracing();
    
    let _guard = span!(Level::INFO, "test_fibo_concepts_traced_in_spans").entered();
    
    let execution = run_70_turn_scenario().await;
    
    println!("\n=== FIBO Concept Tracing in Phase C (Turns 17-28) ===");
    
    let mut turns_without_fibo = vec![];
    let mut fibo_concept_counts = HashMap::new();
    
    // Phase C turns (17-28) should have FIBO concepts in spans
    for turn in 17..=28 {
        let span_name = format!("turn_{}", turn);
        let turn_span = execution.spans.get(&span_name)
            .unwrap_or_else(|| panic!("Missing span for turn {}", turn));
        
        // Check for FIBO entity tracing
        if !turn_span.attributes.contains_key("fibo.entities") {
            turns_without_fibo.push(turn);
        } else {
            // Extract FIBO concept count
            if let Some(count_str) = turn_span.attributes.get("fibo.entities.count") {
                let count: usize = count_str.parse()
                    .unwrap_or_else(|_| panic!("Invalid fibo.entities.count for turn {}", turn));
                fibo_concept_counts.insert(turn, count);
            }
        }
    }
    
    // Report results
    println!("Phase C turns with FIBO concepts: {}", fibo_concept_counts.len());
    println!("Phase C turns without FIBO concepts: {}", turns_without_fibo.len());
    
    if !fibo_concept_counts.is_empty() {
        println!("\nFIBO concept counts by turn:");
        for (turn, count) in &fibo_concept_counts {
            println!("  Turn {}: {} FIBO entities", turn, count);
        }
    }
    
    if !turns_without_fibo.is_empty() {
        println!("\n⚠️  Turns missing FIBO entity tracing: {:?}", turns_without_fibo);
    }
    
    // At least 80% of Phase C turns should have FIBO concepts
    let coverage = (12 - turns_without_fibo.len()) as f64 / 12.0 * 100.0;
    println!("\nFIBO concept coverage: {:.1}%", coverage);
    
    assert!(turns_without_fibo.len() <= 2,
        "Too many Phase C turns missing FIBO concepts: {:?}", turns_without_fibo);
    
    println!("\n✅ FIBO concepts properly traced in data architecture phase");
}

/// Test that TOGAF phases are annotated in spans
#[tokio::test]
async fn test_togaf_phases_traced_in_spans() {
    init_tracing();
    
    let _guard = span!(Level::INFO, "test_togaf_phases_traced_in_spans").entered();
    
    let execution = run_70_turn_scenario().await;
    
    println!("\n=== TOGAF Phase Annotation Validation ===");
    
    // TOGAF ADM phases and their turn ranges
    let togaf_phases = vec![
        ("Preliminary", 1..=4),
        ("Phase A", 5..=8),
        ("Phase B", 9..=16),
        ("Phase C", 17..=28),
        ("Phase D", 29..=40),
        ("Phase E", 41..=52),
        ("Phase F", 53..=60),
        ("Phase G", 61..=66),
        ("Phase H", 67..=70),
    ];
    
    let mut phase_coverage = HashMap::new();
    
    for (phase_name, turn_range) in &togaf_phases {
        let mut turns_in_phase = 0;
        let mut turns_with_phase_attr = 0;
        
        for turn in turn_range.clone() {
            turns_in_phase += 1;
            
            let span_name = format!("turn_{}", turn);
            if let Some(span) = execution.spans.get(&span_name) {
                if let Some(phase) = span.attributes.get("togaf.phase") {
                    if phase == phase_name {
                        turns_with_phase_attr += 1;
                    }
                }
            }
        }
        
        let coverage = turns_with_phase_attr as f64 / turns_in_phase as f64 * 100.0;
        phase_coverage.insert(phase_name.to_string(), coverage);
        
        println!("{}: {}/{} turns ({:.1}%)", 
            phase_name, turns_with_phase_attr, turns_in_phase, coverage);
    }
    
    // All phases should have at least 80% coverage
    for (phase, coverage) in &phase_coverage {
        assert!(*coverage >= 80.0,
            "TOGAF phase {} has insufficient coverage: {:.1}%", phase, coverage);
    }
    
    println!("\n✅ All TOGAF phases properly annotated in spans");
}

/// Test that agent collaboration spans are present
#[tokio::test]
async fn test_agent_collaboration_spans() {
    init_tracing();
    
    let _guard = span!(Level::INFO, "test_agent_collaboration_spans").entered();
    
    let execution = run_70_turn_scenario().await;
    
    println!("\n=== Agent Collaboration Span Validation ===");
    
    // Count agent-specific spans
    let mut agent_spans = HashMap::new();
    
    for (span_name, span) in &execution.spans {
        if let Some(agent_id) = span.attributes.get("agent.id") {
            *agent_spans.entry(agent_id.clone()).or_insert(0) += 1;
        }
    }
    
    println!("Agent collaboration spans:");
    for (agent_id, count) in &agent_spans {
        println!("  Agent {}: {} spans", agent_id, count);
    }
    
    // Should have at least 2 agents (Enterprise Architect + Data Architect)
    assert!(agent_spans.len() >= 2,
        "Expected at least 2 agents, found: {}", agent_spans.len());
    
    // Total collaboration spans should be substantial
    let total_collab_spans: usize = agent_spans.values().sum();
    assert!(total_collab_spans >= 70,
        "Expected at least 70 collaboration spans, found: {}", total_collab_spans);
    
    println!("\n✅ Agent collaboration spans present");
}

/// Test span timing metrics are reasonable
#[tokio::test]
async fn test_span_timing_metrics() {
    init_tracing();
    
    let _guard = span!(Level::INFO, "test_span_timing_metrics").entered();
    
    let execution = run_70_turn_scenario().await;
    
    println!("\n=== Span Timing Metrics Validation ===");
    
    // Check that spans have duration information
    let mut spans_with_duration = 0;
    let mut total_duration_ms = 0;
    
    for (span_name, span) in &execution.spans {
        if let Some(duration_str) = span.attributes.get("span.duration_ms") {
            if let Ok(duration) = duration_str.parse::<u64>() {
                spans_with_duration += 1;
                total_duration_ms += duration;
                
                // Sanity check: no single turn should take > 5 minutes
                assert!(duration <= 300_000,
                    "Turn {} has excessive duration: {}ms", span_name, duration);
            }
        }
    }
    
    let avg_duration = if spans_with_duration > 0 {
        total_duration_ms / spans_with_duration as u64
    } else {
        0
    };
    
    println!("Spans with timing: {}", spans_with_duration);
    println!("Total duration: {}ms ({}s)", total_duration_ms, total_duration_ms / 1000);
    println!("Average span duration: {}ms", avg_duration);
    
    assert!(spans_with_duration > 0, "No spans have duration information");
    assert!(avg_duration < 60_000, "Average span duration too high: {}ms", avg_duration);
    
    println!("\n✅ Span timing metrics are reasonable");
}
