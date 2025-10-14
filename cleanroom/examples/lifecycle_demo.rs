//! Lifecycle Management System Demo
//!
//! Demonstrates the complete ggen-style lifecycle management:
//! - Project initialization
//! - Test execution in cleanroom
//! - Production readiness tracking
//! - Environment validation
//! - Deployment with checks

use clnrm::lifecycle::{
    init_lifecycle, LifecycleConfig, LifecycleManager, Status,
};
use clnrm::cleanroom::CleanroomEnvironment;
use clnrm::config::CleanroomConfig;
use std::sync::Arc;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("🚀 Lifecycle Management System Demo\n");

    // 1. Initialize lifecycle configuration
    println!("📋 Step 1: Initialize Lifecycle Configuration");
    let config = init_lifecycle("demo-project").await?;
    println!("   ✓ Created lifecycle config for: {}", config.project_name);
    println!("   ✓ Phases: {}", config.phases.len());
    println!("   ✓ Environments: {}", config.environments.len());
    println!("   ✓ Requirements: {}\n", config.readiness_requirements.len());

    // 2. Create lifecycle manager (without cleanroom for this demo)
    println!("🏗️  Step 2: Create Lifecycle Manager");
    let manager = LifecycleManager::new(config.clone(), None)?;
    println!("   ✓ Lifecycle manager created\n");

    // Optional: Create with cleanroom for hermetic testing
    println!("🧪 Step 2b: Create Lifecycle Manager with Cleanroom");
    let cleanroom_config = CleanroomConfig::default();
    let cleanroom = Arc::new(CleanroomEnvironment::new(cleanroom_config).await?);
    let manager_with_cleanroom = LifecycleManager::new(config.clone(), Some(cleanroom))?;
    println!("   ✓ Lifecycle manager with cleanroom created\n");

    // 3. Check initial readiness
    println!("📊 Step 3: Check Production Readiness");
    let initial_readiness = manager.readiness().await?;
    println!("   📈 Initial Score: {}/100", initial_readiness.score);
    println!("   📋 Requirements:");
    for req in &initial_readiness.requirements {
        println!("      - {} [{}] ({})", req.name, req.status, req.priority);
    }
    if !initial_readiness.blockers.is_empty() {
        println!("   ⚠️  Blockers:");
        for blocker in &initial_readiness.blockers {
            println!("      - {}", blocker);
        }
    }
    if !initial_readiness.recommendations.is_empty() {
        println!("   💡 Recommendations:");
        for rec in &initial_readiness.recommendations {
            println!("      - {}", rec);
        }
    }
    println!();

    // 4. Update some requirements
    println!("✅ Step 4: Complete Some Requirements");
    if !config.readiness_requirements.is_empty() {
        let first_req = &config.readiness_requirements[0];
        manager.update_requirement(&first_req.id, Status::Complete).await?;
        println!("   ✓ Completed: {}", first_req.name);

        if config.readiness_requirements.len() > 1 {
            let second_req = &config.readiness_requirements[1];
            manager.update_requirement(&second_req.id, Status::InProgress).await?;
            println!("   ⏳ In Progress: {}", second_req.name);
        }
    }
    println!();

    // 5. Check readiness again
    println!("📊 Step 5: Re-check Production Readiness");
    let updated_readiness = manager.readiness().await?;
    println!("   📈 Updated Score: {}/100", updated_readiness.score);

    if updated_readiness.score > initial_readiness.score {
        println!("   ✨ Readiness improved by {} points!",
            updated_readiness.score - initial_readiness.score);
    }

    println!("   📊 Category Breakdown:");
    for (category, score) in &updated_readiness.category_scores {
        println!("      - {}: {}/100", category, score);
    }
    println!();

    // 6. Validate environments
    println!("🔍 Step 6: Validate Environments");
    for env_name in ["dev", "staging"] {
        if let Some(_env) = config.get_environment(env_name) {
            match manager.validate(env_name).await {
                Ok(report) => {
                    println!("   {} Environment: {}",
                        if report.passed { "✓" } else { "✗" },
                        env_name
                    );
                    if !report.warnings.is_empty() {
                        println!("      Warnings: {}", report.warnings.len());
                    }
                    if !report.errors.is_empty() {
                        println!("      Errors: {}", report.errors.len());
                    }
                }
                Err(e) => {
                    println!("   ✗ {} validation failed: {}", env_name, e);
                }
            }
        }
    }
    println!();

    // 7. Display deployment readiness
    println!("🚢 Step 7: Deployment Readiness Assessment");
    let can_deploy_staging = updated_readiness.score >= 60;
    let can_deploy_production = updated_readiness.score >= 80 && updated_readiness.blockers.is_empty();

    println!("   Staging:    {} (requires 60% readiness)",
        if can_deploy_staging { "✓ READY" } else { "✗ NOT READY" });
    println!("   Production: {} (requires 80% readiness + no blockers)",
        if can_deploy_production { "✓ READY" } else { "✗ NOT READY" });
    println!();

    // 8. Summary
    println!("📝 Summary");
    println!("   Project: {}", config.project_name);
    println!("   Readiness Score: {}/100", updated_readiness.score);
    println!("   Blockers: {}", updated_readiness.blockers.len());
    println!("   Warnings: {}", updated_readiness.warnings.len());
    println!("   Status: {}",
        if can_deploy_production {
            "🎉 READY FOR PRODUCTION"
        } else {
            "🔧 DEVELOPMENT IN PROGRESS"
        }
    );
    println!();

    println!("✨ Demo completed successfully!");

    Ok(())
}
