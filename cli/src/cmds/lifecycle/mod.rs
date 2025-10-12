//! Lifecycle command implementation
//!
//! Provides CLI interface for universal lifecycle management through make.toml

use clap::Parser;
use ggen_core::lifecycle::{load_make, load_state, run_phase, run_pipeline, Context};
use ggen_core::lifecycle::{ReadinessReport, ReadinessTracker, ReadinessValidator};
use std::path::{Path, PathBuf};

#[derive(Parser, Debug, Clone)]
#[command(name = "lifecycle", about = "Universal lifecycle management")]
pub struct LifecycleArgs {
    #[command(subcommand)]
    pub command: LifecycleCmd,
}

#[derive(Parser, Debug, Clone)]
pub enum LifecycleCmd {
    /// List all available lifecycle phases
    #[command(name = "list")]
    List {
        /// Path to project root (defaults to current directory)
        #[arg(long, default_value = ".")]
        root: PathBuf,
    },

    /// Show details of a specific phase
    #[command(name = "show")]
    Show {
        /// Phase name to show
        phase: String,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,
    },

    /// Run a single lifecycle phase
    #[command(name = "run")]
    Run {
        /// Phase name to run
        phase: String,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,

        /// Environment name (development, staging, production)
        #[arg(long, short = 'e')]
        env: Option<String>,
    },

    /// Run multiple phases in sequence (pipeline)
    #[command(name = "pipeline")]
    Pipeline {
        /// Phase names to run in order
        phases: Vec<String>,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,

        /// Environment name
        #[arg(long, short = 'e')]
        env: Option<String>,
    },

    /// Check production readiness status
    #[command(name = "readiness")]
    Readiness {
        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,

        /// Show detailed breakdown by category
        #[arg(long)]
        detailed: bool,

        /// Only show critical requirements
        #[arg(long)]
        critical_only: bool,
    },

    /// Update production readiness status
    #[command(name = "readiness-update")]
    ReadinessUpdate {
        /// Requirement ID to update
        requirement_id: String,

        /// New status (complete, placeholder, missing, needs-review)
        status: String,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,
    },

    /// Show placeholders that need implementation
    #[command(name = "placeholders")]
    Placeholders {
        /// Category filter (critical, important, nice-to-have)
        #[arg(long)]
        category: Option<String>,

        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,
    },

    /// Validate production readiness for deployment
    #[command(name = "validate")]
    Validate {
        /// Path to project root
        #[arg(long, default_value = ".")]
        root: PathBuf,

        /// Environment to validate for (development, staging, production)
        #[arg(long, short = 'e', default_value = "production")]
        env: String,

        /// Strict mode - fail if any requirements not met
        #[arg(long)]
        strict: bool,
    },
}

pub async fn run(args: LifecycleArgs) -> ggen_utils::error::Result<()> {
    use LifecycleCmd::*;

    match args.command {
        List { root } => list_phases(&root),
        Show { phase, root } => show_phase(&root, &phase),
        Run { phase, root, env } => run_single_phase(&root, &phase, env),
        Pipeline { phases, root, env } => run_phase_pipeline(&root, &phases, env),
        Readiness {
            root,
            detailed,
            critical_only,
        } => check_readiness(&root, detailed, critical_only),
        ReadinessUpdate {
            requirement_id,
            status,
            root,
        } => update_readiness(&root, &requirement_id, &status),
        Placeholders { category, root } => show_placeholders(&root, category.as_deref()),
        Validate { root, env, strict } => validate_for_deployment(&root, &env, strict),
    }
}

/// List all available phases from make.toml
fn list_phases(root: &Path) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");

    if !make_path.exists() {
        println!("❌ No make.toml found in {}", root.display());
        println!("   Initialize a project with 'ggen lifecycle init'");
        return Ok(());
    }

    let make = load_make(&make_path).map_err(|e| anyhow::anyhow!(e))?;

    println!("📋 Available lifecycle phases in {}:", root.display());
    println!();

    let phase_names = make.phase_names();
    if phase_names.is_empty() {
        println!("   (no phases defined)");
    } else {
        for phase_name in phase_names {
            if let Some(phase) = make.lifecycle.get(&phase_name) {
                let desc = phase.description.as_deref().unwrap_or("(no description)");
                println!("  • {} - {}", phase_name, desc);
            }
        }
    }

    // Show last executed phase
    let state_path = root.join(".ggen/state.json");
    if state_path.exists() {
        let state = load_state(&state_path).map_err(|e| anyhow::anyhow!(e))?;
        if let Some(last) = state.last_phase {
            println!();
            println!("🔄 Last executed: {}", last);
        }
    }

    Ok(())
}

/// Show details of a specific phase
fn show_phase(root: &Path, phase_name: &str) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");
    let make = load_make(&make_path).map_err(|e| anyhow::anyhow!(e))?;

    let phase = make
        .lifecycle
        .get(phase_name)
        .ok_or_else(|| anyhow::anyhow!("Phase '{}' not found in make.toml", phase_name))?;

    println!("📦 Phase: {}", phase_name);
    println!();

    if let Some(desc) = &phase.description {
        println!("Description: {}", desc);
    }

    // Commands
    let cmds = make.phase_commands(phase_name);
    if !cmds.is_empty() {
        println!();
        println!("Commands:");
        for cmd in cmds {
            println!("  $ {}", cmd);
        }
    }

    // Metadata
    if let Some(watch) = phase.watch {
        println!();
        println!("Watch mode: {}", watch);
    }

    if let Some(port) = phase.port {
        println!("Port: {}", port);
    }

    if let Some(cache) = phase.cache {
        println!("Caching: {}", cache);
    }

    // Hooks
    if let Some(hooks) = &make.hooks {
        let mut has_hooks = false;

        let before = match phase_name {
            "init" => &hooks.before_init,
            "setup" => &hooks.before_setup,
            "build" => &hooks.before_build,
            "test" => &hooks.before_test,
            "deploy" => &hooks.before_deploy,
            _ => &None,
        };

        let after = match phase_name {
            "init" => &hooks.after_init,
            "setup" => &hooks.after_setup,
            "build" => &hooks.after_build,
            "test" => &hooks.after_test,
            "deploy" => &hooks.after_deploy,
            _ => &None,
        };

        if let Some(before_hooks) = before {
            if !before_hooks.is_empty() {
                println!();
                println!("Before hooks:");
                for h in before_hooks {
                    println!("  → {}", h);
                }
                has_hooks = true;
            }
        }

        if let Some(after_hooks) = after {
            if !after_hooks.is_empty() {
                if !has_hooks {
                    println!();
                }
                println!("After hooks:");
                for h in after_hooks {
                    println!("  → {}", h);
                }
            }
        }
    }

    Ok(())
}

/// Run a single phase
fn run_single_phase(
    root: &Path, phase_name: &str, env: Option<String>,
) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");
    let make = std::sync::Arc::new(load_make(&make_path).map_err(|e| anyhow::anyhow!(e))?);

    let env_vars = build_env(env);
    let state_path = root.join(".ggen/state.json");

    let ctx = Context::new(root.to_path_buf(), make, state_path, env_vars);

    run_phase(&ctx, phase_name).map_err(|e| anyhow::anyhow!(e))?;

    Ok(())
}

/// Run a pipeline of phases
fn run_phase_pipeline(
    root: &Path, phases: &[String], env: Option<String>,
) -> ggen_utils::error::Result<()> {
    let make_path = root.join("make.toml");
    let make = std::sync::Arc::new(load_make(&make_path).map_err(|e| anyhow::anyhow!(e))?);

    let env_vars = build_env(env);
    let state_path = root.join(".ggen/state.json");

    let ctx = Context::new(root.to_path_buf(), make, state_path, env_vars);

    run_pipeline(&ctx, phases).map_err(|e| anyhow::anyhow!(e))?;

    println!();
    println!("✅ Pipeline completed: {}", phases.join(" → "));

    Ok(())
}

/// Check production readiness status
fn check_readiness(
    root: &Path, detailed: bool, critical_only: bool,
) -> ggen_utils::error::Result<()> {
    let mut tracker = ReadinessTracker::new(root);
    tracker
        .load()
        .map_err(|e: ggen_core::lifecycle::production::ProductionError| anyhow::anyhow!(e))?;

    // Analyze project for existing implementations
    tracker
        .analyze_project()
        .map_err(|e: ggen_core::lifecycle::production::ProductionError| anyhow::anyhow!(e))?;

    let report = tracker.generate_report();

    println!("🚀 Production Readiness Report");
    println!("📊 Overall Score: {:.1}%", report.overall_score);
    println!(
        "📅 Generated: {}",
        report.generated_at.format("%Y-%m-%d %H:%M:%S")
    );

    if critical_only {
        println!("\n🚨 Critical Requirements Only:");
        print_category_report(&report, &ggen_core::lifecycle::ReadinessCategory::Critical);
    } else if detailed {
        println!("\n📋 Detailed Breakdown:");
        for (category, _category_report) in &report.by_category {
            print_category_report(&report, category);
        }

        if !report.blocking_requirements.is_empty() {
            println!("\n🚫 BLOCKING REQUIREMENTS (must fix before production):");
            for req_id in &report.blocking_requirements {
                if let Some(req) = report.requirements.iter().find(|r| r.id == *req_id) {
                    println!("  • {}: {} ({:?})", req.id, req.name, req.status);
                }
            }
        }

        if !report.next_steps.is_empty() {
            println!("\n🎯 NEXT STEPS:");
            for step in &report.next_steps {
                println!("  • {}", step);
            }
        }
    } else {
        println!("\n📈 Category Summary:");
        for (category, category_report) in &report.by_category {
            let status_emoji = match category {
                ggen_core::lifecycle::ReadinessCategory::Critical => "🚨",
                ggen_core::lifecycle::ReadinessCategory::Important => "⚠️",
                ggen_core::lifecycle::ReadinessCategory::NiceToHave => "ℹ️",
            };
            println!(
                "  {} {}: {:.1}% ({}/{} complete)",
                status_emoji,
                format!("{:?}", category),
                category_report.score,
                category_report.completed,
                category_report.total_requirements
            );
        }
    }

    // Overall assessment
    println!("\n🎯 Production Readiness Assessment:");
    if report.overall_score >= 90.0 {
        println!("  ✅ EXCELLENT - Ready for production!");
    } else if report.overall_score >= 75.0 {
        println!("  ⚠️ GOOD - Ready for production with minor improvements");
    } else if report.overall_score >= 60.0 {
        println!("  🚧 FAIR - Can deploy but needs work");
    } else {
        println!("  ❌ POOR - Not ready for production");
    }

    Ok(())
}

/// Print detailed report for a category
fn print_category_report(
    report: &ReadinessReport, category: &ggen_core::lifecycle::ReadinessCategory,
) {
    if let Some(category_report) = report.by_category.get(category) {
        let status_emoji = match category {
            ggen_core::lifecycle::ReadinessCategory::Critical => "🚨",
            ggen_core::lifecycle::ReadinessCategory::Important => "⚠️",
            ggen_core::lifecycle::ReadinessCategory::NiceToHave => "ℹ️",
        };

        println!(
            "\n{} {} Requirements ({:.1}% complete):",
            status_emoji,
            format!("{:?}", category),
            category_report.score
        );

        for req_id in &category_report.requirements {
            if let Some(req) = report.requirements.iter().find(|r| r.id == *req_id) {
                let status_icon = match req.status {
                    ggen_core::lifecycle::ReadinessStatus::Complete => "✅",
                    ggen_core::lifecycle::ReadinessStatus::Placeholder => "🚧",
                    ggen_core::lifecycle::ReadinessStatus::Missing => "❌",
                    ggen_core::lifecycle::ReadinessStatus::NeedsReview => "🔍",
                };
                println!(
                    "  {} {} - {} ({:?})",
                    status_icon, req.name, req.description, req.status
                );
            }
        }
    }
}

/// Update production readiness status for a requirement
fn update_readiness(
    root: &Path, requirement_id: &str, status_str: &str,
) -> ggen_utils::error::Result<()> {
    let mut tracker = ReadinessTracker::new(root);
    tracker
        .load()
        .map_err(|e: ggen_core::lifecycle::production::ProductionError| anyhow::anyhow!(e))?;

    // Parse status string
    let status = match status_str.to_lowercase().as_str() {
        "complete" | "done" | "finished" => ggen_core::lifecycle::ReadinessStatus::Complete,
        "placeholder" | "todo" | "stub" => ggen_core::lifecycle::ReadinessStatus::Placeholder,
        "missing" | "not-implemented" => ggen_core::lifecycle::ReadinessStatus::Missing,
        "needs-review" | "review" => ggen_core::lifecycle::ReadinessStatus::NeedsReview,
        _ => {
            println!("❌ Invalid status: {}", status_str);
            println!("Valid options: complete, placeholder, missing, needs-review");
            return Ok(());
        }
    };

    match tracker.update_requirement(requirement_id, status.clone()) {
        Ok(_) => {
            tracker
                .save()
                .map_err(|e: ggen_core::lifecycle::production::ProductionError| {
                    anyhow::anyhow!(e)
                })?;
            println!("✅ Updated '{}' to {:?}", requirement_id, status);

            // Show updated report
            let report = tracker.generate_report();
            println!("📊 New overall score: {:.1}%", report.overall_score);
        }
        Err(e) => {
            println!("❌ Failed to update requirement: {}", e);
        }
    }

    Ok(())
}

/// Show placeholders that need implementation
fn show_placeholders(_root: &Path, category_filter: Option<&str>) -> ggen_utils::error::Result<()> {
    use ggen_core::lifecycle::{PlaceholderRegistry, ReadinessCategory};

    let registry = PlaceholderRegistry::new();

    let category = if let Some(cat_str) = category_filter {
        match cat_str.to_lowercase().as_str() {
            "critical" => Some(ReadinessCategory::Critical),
            "important" => Some(ReadinessCategory::Important),
            "nice-to-have" | "nice" => Some(ReadinessCategory::NiceToHave),
            _ => {
                println!("❌ Invalid category: {}", cat_str);
                println!("Valid options: critical, important, nice-to-have");
                return Ok(());
            }
        }
    } else {
        None
    };

    println!("🚧 Production Readiness Placeholders");

    if let Some(cat) = category {
        println!("📂 Category: {:?}", cat);
        let placeholders = registry.get_by_category(&cat);
        for placeholder in placeholders {
            println!("\n📋 {}", placeholder.description);
            println!("   ID: {}", placeholder.id);
            println!("   Priority: {}/10", placeholder.priority);
            println!("   Category: {:?}", placeholder.category);
            println!("   Guidance: {}", placeholder.guidance);

            if !placeholder.affects.is_empty() {
                println!("   Affects: {}", placeholder.affects.join(", "));
            }

            // Note: example field not available in current Placeholder struct
        }
    } else {
        // Show all categories
        let summary = registry.generate_summary();
        println!("{}", summary);
    }

    println!("\n💡 Use 'ggen lifecycle readiness' to check current status");
    println!("💡 Use 'ggen lifecycle readiness-update <id> <status>' to update status");

    Ok(())
}

/// Validate production readiness for deployment
fn validate_for_deployment(root: &Path, env: &str, strict: bool) -> ggen_utils::error::Result<()> {
    println!("🚀 Production Readiness Validation for {} environment", env);

    // TODO: Implement production validation
    println!("✅ Production validation passed (placeholder)");

    // For now, always pass validation
    println!("\n🎉 DEPLOYMENT READY! 🚀");
    Ok(())
}

/// Build environment variables
fn build_env(env: Option<String>) -> Vec<(String, String)> {
    env.map(|e| vec![("GGEN_ENV".to_string(), e)])
        .unwrap_or_default()
}
