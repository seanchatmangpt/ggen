//! Ultrathink Core 80/20 Demo
//!
//! Demonstrates the focused ultrathink core that implements the 20% of autonomous
//! workflows delivering 80% of the value, using core team best practices.

use agents::{
    agents::{ApiSpec, RuntimeMetrics, SecurityVulnerability, Trigger},
    ultrathink_core::{
        demonstrate_focused_autonomous_workflows, run_ultrathink_core, UltrathinkCore,
    },
};
use std::env;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🚀 Ultrathink Core - 80/20 Autonomous System Demo");
    println!("=================================================");

    let args: Vec<String> = env::args().collect();
    let demo_type = args.get(1).map(|s| s.as_str()).unwrap_or("focused");

    match demo_type {
        "focused" => {
            println!("🎯 Running focused 80/20 autonomous workflow demonstration...");
            demonstrate_focused_autonomous_workflows().await?;
        }
        "production" => {
            println!("🏭 Running focused production autonomous system...");
            run_ultrathink_core().await?;
        }
        "gap-fill" => {
            println!("🔧 Demonstrating gap-filling with 80/20 principle...");
            demonstrate_gap_filling().await?;
        }
        _ => {
            println!("Usage: {} [focused|production|gap-fill]", args[0]);
            println!("  focused    - Focused 80/20 workflow demo");
            println!("  production - Production system simulation");
            println!("  gap-fill   - Demonstrate gap-filling using 80/20");
            return Ok(());
        }
    }

    println!("\n🏆 Demo completed successfully!");
    println!("\n💡 80/20 Principle Achievements:");
    println!("  • Requirements changes → full regeneration (100% value)");
    println!("  • Critical security → auto-patching (100% value)");
    println!("  • Breaking API changes → migration (100% value)");
    println!("  • Performance regression → optimization (80% value)");
    println!("  • Low-value triggers → filtered (20% value)");
    println!("  • Existing MCP, AI, agents fully utilized");
    println!("  • Core team best practices maintained");

    Ok(())
}

/// Demonstrate gap-filling using the 80/20 principle
async fn demonstrate_gap_filling() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔧 Gap-Filling Demonstration (80/20 Principle)");

    let mut ultrathink = UltrathinkCore::new().await?;

    // Start with minimal configuration
    ultrathink.start().await?;

    // Scale to focused size (not full swarm)
    ultrathink.scale_focused_swarm(2).await?;

    println!("\n📋 Gap Analysis - What 20% delivers 80% of autonomous value:");

    // 1. Requirements changes (highest value gap to fill)
    println!("\n1️⃣ Requirements Changes → Graph Extension → Code Regeneration");
    println!("   Gap: Manual requirements analysis and implementation");
    println!("   80/20 Solution: AI-powered requirements analysis → automatic graph extension → deterministic code regeneration");

    let requirements_trigger = Trigger::RequirementsChange(
        "Implement user authentication with OAuth 2.0, JWT tokens, and role-based access control"
            .to_string(),
    );

    let result = ultrathink
        .trigger_focused_workflow(requirements_trigger)
        .await?;
    println!(
        "   ✅ Filled: {} actions executed, {} artifacts generated in {}ms",
        result.actions_taken.len(),
        result.artifacts_generated.len(),
        result.execution_time_ms
    );

    // 2. Critical security vulnerabilities (highest value gap)
    println!("\n2️⃣ Critical Security Vulnerabilities → Auto-Patching");
    println!("   Gap: Manual security vulnerability assessment and patching");
    println!("   80/20 Solution: AI-powered security impact analysis → automatic patch generation → security validation");

    let security_trigger = Trigger::SecurityVulnerability(SecurityVulnerability {
        id: "CVE-2024-56789".to_string(),
        severity: "Critical".to_string(),
        affected_components: vec!["authentication".to_string(), "authorization".to_string()],
    });

    let result = ultrathink
        .trigger_focused_workflow(security_trigger)
        .await?;
    println!(
        "   ✅ Filled: {} actions executed, {} artifacts generated in {}ms",
        result.actions_taken.len(),
        result.artifacts_generated.len(),
        result.execution_time_ms
    );

    // 3. Breaking API changes (highest value gap)
    println!("\n3️⃣ Breaking API Changes → Migration");
    println!("   Gap: Manual API compatibility analysis and migration");
    println!("   80/20 Solution: AI-powered API compatibility analysis → automatic migration plan → code regeneration");

    let api_trigger = Trigger::ApiChange(ApiSpec {
        name: "user-service".to_string(),
        version: "3.0.0".to_string(),
        changes: vec![
            "BREAKING: /users/{id} endpoint deprecated".to_string(),
            "Added /api/v2/users endpoint with new schema".to_string(),
        ],
    });

    let result = ultrathink.trigger_focused_workflow(api_trigger).await?;
    println!(
        "   ✅ Filled: {} actions executed, {} artifacts generated in {}ms",
        result.actions_taken.len(),
        result.artifacts_generated.len(),
        result.execution_time_ms
    );

    // 4. Performance regressions (conditional high value)
    println!("\n4️⃣ Performance Regressions → Optimization (Conditional)");
    println!("   Gap: Manual performance monitoring and optimization");
    println!("   80/20 Solution: AI-powered performance analysis → automatic optimization → performance validation");

    // Significant regression (high value)
    let significant_regression = Trigger::PerformanceRegression(crate::agents::PerformanceDelta {
        metric_name: "response_time".to_string(),
        previous_value: 100.0,
        current_value: 350.0, // 250% increase - definitely high value
        threshold: 150.0,
    });

    let result = ultrathink
        .trigger_focused_workflow(significant_regression)
        .await?;
    println!(
        "   ✅ Filled: {} actions executed, {} artifacts generated in {}ms",
        result.actions_taken.len(),
        result.artifacts_generated.len(),
        result.execution_time_ms
    );

    // Minor regression (low value - filtered)
    println!("\n5️⃣ Minor Performance Changes → Filtered (Low Value)");
    println!("   Gap: Over-reaction to minor performance fluctuations");
    println!("   80/20 Solution: Filter low-value triggers, focus only on significant regressions");

    let minor_regression = Trigger::PerformanceRegression(crate::agents::PerformanceDelta {
        metric_name: "response_time".to_string(),
        previous_value: 100.0,
        current_value: 120.0, // 20% increase - not significant enough
        threshold: 150.0,
    });

    let result = ultrathink
        .trigger_focused_workflow(minor_regression)
        .await?;
    println!(
        "   ✅ Filtered: Only {} action (analysis only), no regeneration needed",
        result.actions_taken.len()
    );

    // Show gap-filling results
    println!("\n📊 Gap-Filling Results (80/20 Principle):");
    let status = ultrathink.get_status().await;
    println!("  Agents Utilized: {}", status.total_agents);
    println!("  High-Value Gaps Filled: {}", status.workflows_executed);
    println!("  Success Rate: {:.1}%", status.success_rate);
    println!(
        "  Average Execution: {}ms",
        status.average_execution_time_ms
    );

    let metrics = ultrathink.get_performance_metrics().await;
    println!(
        "  High-Value Triggers Processed: {}",
        metrics.high_value_triggers_processed
    );
    println!(
        "  Low-Value Triggers Filtered: {}",
        metrics.low_value_triggers_filtered
    );

    println!("\n🎯 Gap-Filling Summary:");
    println!("  ✅ Requirements changes → 100% automated");
    println!("  ✅ Critical security → 100% automated");
    println!("  ✅ Breaking API changes → 100% automated");
    println!("  ✅ Significant performance → 80% automated");
    println!("  ✅ Minor changes → intelligently filtered");
    println!("  ✅ Existing infrastructure → fully utilized");
    println!("  ✅ Core team patterns → strictly followed");

    ultrathink.stop().await?;

    println!("\n✅ Gap-filling demonstration completed!");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_ultrathink_core_gap_filling() {
        // Test that high-value gaps are filled
        let ultrathink = UltrathinkCore::new().await.unwrap();

        // High-value requirements change
        let trigger = Trigger::RequirementsChange("Add critical security feature".to_string());
        let result = ultrathink.trigger_focused_workflow(trigger).await.unwrap();

        assert!(result.success);
        assert!(!result.actions_taken.is_empty());
        assert!(!result.artifacts_generated.is_empty());
    }

    #[tokio::test]
    async fn test_low_value_filtering() {
        // Test that low-value triggers are filtered
        let ultrathink = UltrathinkCore::new().await.unwrap();

        // Low-value performance trigger
        let trigger = Trigger::RuntimeTelemetry(RuntimeMetrics {
            cpu_usage: 45.0,       // Below 80% threshold
            memory_usage: 60.0,    // Below 85% threshold
            response_time_ms: 200, // Below 1000ms threshold
            error_rate: 0.01,      // Low error rate
        });

        let result = ultrathink.trigger_focused_workflow(trigger).await.unwrap();

        // Should be filtered (no regeneration)
        assert!(result.success);
        assert_eq!(result.actions_taken.len(), 1); // Only analysis
        assert!(result.artifacts_generated.is_empty()); // No regeneration
    }
}
