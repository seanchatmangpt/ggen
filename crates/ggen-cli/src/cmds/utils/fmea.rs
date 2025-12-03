//! FMEA (Failure Mode and Effects Analysis) CLI commands - clap-noun-verb v5.3.0 Migration
//!
//! Provides CLI commands for viewing FMEA reports, Pareto analysis, and failure mode details
//! using the v5.3.0 #[verb("verb", "noun")] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use ggen_utils::fmea::{FailureCategory, FMEA_REGISTRY};

/// Generate FMEA report
#[verb("report", "fmea")]
pub fn report(format: Option<String>, risk: Option<String>, top: Option<usize>) -> Result<()> {
    let format = format.unwrap_or_else(|| "text".to_string());
    let top = top.unwrap_or(20);

    let registry = FMEA_REGISTRY
        .read()
        .map_err(|_| clap_noun_verb::NounVerbError::execution_error("Failed to acquire FMEA registry lock"))?;

    let mut failure_modes: Vec<_> = registry.all_failure_modes().collect();

    // Filter by risk level if specified
    if let Some(risk_level) = risk {
        failure_modes.retain(|fm| fm.rpn.risk_level() == risk_level.to_uppercase());
    }

    // Sort by RPN descending
    failure_modes.sort_by(|a, b| b.rpn.value().cmp(&a.rpn.value()));

    // Limit to top N
    failure_modes.truncate(top);

    match format.as_str() {
        "json" => print_json_report(&failure_modes),
        _ => print_text_report(&failure_modes, &registry),
    }

    Ok(())
}

/// Generate Pareto analysis (80/20 chart)
#[verb("pareto", "fmea")]
pub fn pareto() -> Result<()> {
    let registry = FMEA_REGISTRY
        .read()
        .map_err(|_| clap_noun_verb::NounVerbError::execution_error("Failed to acquire FMEA registry lock"))?;

    let mut failure_modes: Vec<_> = registry.all_failure_modes().collect();
    failure_modes.sort_by(|a, b| b.rpn.value().cmp(&a.rpn.value()));

    // Calculate total RPN
    let total_rpn: u32 = failure_modes.iter().map(|fm| fm.rpn.value() as u32).sum();

    println!("\nFMEA Pareto Analysis (80/20 Rule)");
    println!("==================================\n");

    let mut cumulative = 0u32;
    let mut found_80_line = false;

    for (i, fm) in failure_modes.iter().enumerate() {
        cumulative += fm.rpn.value() as u32;
        let percentage = (cumulative as f64 / total_rpn as f64) * 100.0;

        // Visual bar (30 chars max)
        let bar_length =
            ((fm.rpn.value() as f64 / failure_modes[0].rpn.value() as f64) * 30.0) as usize;
        let bar = "█".repeat(bar_length);
        let empty = " ".repeat(30 - bar_length);

        println!(
            "{:2}. [{}{bar}{empty}] {:3} - {}",
            i + 1,
            if percentage <= 80.0 { "█" } else { " " },
            fm.rpn.value(),
            fm.id
        );

        // Draw 80% line
        if !found_80_line && percentage > 80.0 {
            println!("    {}", "─".repeat(60));
            println!("    ↑ 80% of risk (Pareto vital few)");
            found_80_line = true;
        }
    }

    println!("\nTotal RPN: {}", total_rpn);
    println!(
        "Focus on top {} modes for 80% impact",
        failure_modes
            .iter()
            .scan(0u32, |acc, fm| {
                *acc += fm.rpn.value() as u32;
                Some((*acc as f64 / total_rpn as f64) <= 0.80)
            })
            .filter(|&x| x)
            .count()
    );

    Ok(())
}

/// List failure modes with filters
#[verb("list", "fmea")]
pub fn list(category: Option<String>, sort: Option<String>) -> Result<()> {
    let sort = sort.unwrap_or_else(|| "rpn".to_string());

    let registry = FMEA_REGISTRY
        .read()
        .map_err(|_| clap_noun_verb::NounVerbError::execution_error("Failed to acquire FMEA registry lock"))?;

    let mut failure_modes: Vec<_> = registry.all_failure_modes().collect();

    // Filter by category if specified
    if let Some(cat) = category {
        let target_category = parse_category(&cat)
            .map_err(|_| clap_noun_verb::NounVerbError::execution_error(&format!("Invalid category: {}", cat)))?;
        failure_modes.retain(|fm| fm.category == target_category);
    }

    // Sort
    match sort.to_lowercase().as_str() {
        "rpn" => failure_modes.sort_by(|a, b| b.rpn.value().cmp(&a.rpn.value())),
        "id" => failure_modes.sort_by(|a, b| a.id.cmp(&b.id)),
        "severity" => failure_modes.sort_by(|a, b| b.severity.value().cmp(&a.severity.value())),
        _ => return Err(clap_noun_verb::NounVerbError::argument_error(&format!("Invalid sort: {}", sort))),
    }

    println!("\nFailure Modes");
    println!("=============\n");

    for fm in failure_modes {
        println!("ID: {}", fm.id);
        println!("  Category: {:?}", fm.category);
        println!("  RPN: {} ({})", fm.rpn.value(), fm.rpn.risk_level());
        println!(
            "  S={} O={} D={}",
            fm.severity.value(),
            fm.occurrence.value(),
            fm.detection.value()
        );
        println!("  Description: {}", fm.description);
        println!();
    }

    Ok(())
}

/// Show specific failure mode details
#[verb("show", "fmea")]
pub fn show(mode_id: String, events: Option<bool>) -> Result<()> {
    let events = events.unwrap_or(false);

    let registry = FMEA_REGISTRY
        .read()
        .map_err(|_| clap_noun_verb::NounVerbError::execution_error("Failed to acquire FMEA registry lock"))?;

    let failure_mode = registry
        .get_failure_mode(&mode_id)
        .ok_or_else(|| clap_noun_verb::NounVerbError::argument_error(&format!("Failure mode not found: {}", mode_id)))?;

    println!("\nFailure Mode Details");
    println!("====================\n");
    println!("ID: {}", failure_mode.id);
    println!("Category: {:?}", failure_mode.category);
    println!("Description: {}", failure_mode.description);
    println!();
    println!(
        "Risk Priority Number (RPN): {} ({})",
        failure_mode.rpn.value(),
        failure_mode.rpn.risk_level()
    );
    println!(
        "  Severity:    {} ({})",
        failure_mode.severity.value(),
        failure_mode.severity.level()
    );
    println!(
        "  Occurrence:  {} ({})",
        failure_mode.occurrence.value(),
        failure_mode.occurrence.level()
    );
    println!(
        "  Detection:   {} ({})",
        failure_mode.detection.value(),
        failure_mode.detection.level()
    );
    println!();
    println!("Effects:");
    for effect in &failure_mode.effects {
        println!("  - {}", effect);
    }
    println!();
    println!("Causes:");
    for cause in &failure_mode.causes {
        println!("  - {}", cause);
    }
    println!();
    println!("Controls:");
    for control in &failure_mode.controls {
        println!("  - {}", control);
    }
    println!();
    println!("Recommended Actions:");
    for action in &failure_mode.actions {
        println!("  - {}", action);
    }

    // Show events if requested
    if events {
        let events: Vec<_> = registry.events_for_mode(&mode_id).collect();

        println!();
        println!("Recorded Events: {}", events.len());
        if !events.is_empty() {
            println!("─────────────────");
            for (i, event) in events.iter().rev().take(10).enumerate() {
                println!("{}. [{}] {}", i + 1, event.operation, event.error_message);
                if let Some(ctx) = &event.context {
                    println!("   Context: {}", ctx);
                }
            }
            if events.len() > 10 {
                println!("   ... and {} more", events.len() - 10);
            }
        }
    }

    Ok(())
}

/// Export FMEA data to JSON
#[verb("export", "fmea")]
pub fn export(output: Option<String>) -> Result<()> {
    let output = output.unwrap_or_else(|| "fmea-report.json".to_string());

    let registry = FMEA_REGISTRY
        .read()
        .map_err(|_| clap_noun_verb::NounVerbError::execution_error("Failed to acquire FMEA registry lock"))?;

    let failure_modes: Vec<_> = registry.all_failure_modes().collect();

    let json = serde_json::json!({
        "version": "1.0",
        "timestamp": chrono::Utc::now().to_rfc3339(),
        "failure_modes": failure_modes.iter().map(|fm| {
            serde_json::json!({
                "id": fm.id,
                "category": format!("{:?}", fm.category),
                "description": fm.description,
                "rpn": {
                    "value": fm.rpn.value(),
                    "level": fm.rpn.risk_level(),
                },
                "severity": {
                    "value": fm.severity.value(),
                    "level": fm.severity.level(),
                },
                "occurrence": {
                    "value": fm.occurrence.value(),
                    "level": fm.occurrence.level(),
                },
                "detection": {
                    "value": fm.detection.value(),
                    "level": fm.detection.level(),
                },
                "effects": fm.effects,
                "causes": fm.causes,
                "controls": fm.controls,
                "actions": fm.actions,
            })
        }).collect::<Vec<_>>(),
        "total_modes": failure_modes.len(),
    });

    std::fs::write(&output, serde_json::to_string_pretty(&json).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(&format!("Failed to write file: {}", e))
    })?)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(&format!("Failed to write file: {}", e)))?;

    println!("FMEA data exported to: {}", output);

    Ok(())
}

// Helper functions

fn print_text_report(failure_modes: &[&ggen_utils::fmea::FailureMode], registry: &ggen_utils::fmea::FmeaRegistry) {
    println!("\nFMEA Report - ggen CLI");
    println!("======================\n");
    println!("Top Failure Modes by RPN (Pareto: 80% of risk)");
    println!("------------------------------------------------\n");

    for (i, fm) in failure_modes.iter().enumerate() {
        let events: Vec<_> = registry.events_for_mode(&fm.id).collect();

        println!(
            "{}. {} (RPN {} - {})",
            i + 1,
            fm.id,
            fm.rpn.value(),
            fm.rpn.risk_level()
        );
        println!(
            "   Effect: {}",
            fm.effects.first().unwrap_or(&"N/A".to_string())
        );
        println!(
            "   Control: {}",
            fm.controls.first().unwrap_or(&"N/A".to_string())
        );
        println!("   Events: {} occurrences", events.len());
        if let Some(last_event) = events.last() {
            println!(
                "   Last: {}",
                last_event.error_message.lines().next().unwrap_or("")
            );
        }
        println!();
    }
}

fn print_json_report(failure_modes: &[&ggen_utils::fmea::FailureMode]) {
    let json = serde_json::json!({
        "failure_modes": failure_modes.iter().map(|fm| {
            serde_json::json!({
                "id": fm.id,
                "rpn": fm.rpn.value(),
                "risk_level": fm.rpn.risk_level(),
                "description": fm.description,
            })
        }).collect::<Vec<_>>(),
    });

    println!("{}", serde_json::to_string_pretty(&json).unwrap());
}

fn parse_category(s: &str) -> std::result::Result<FailureCategory, String> {
    match s.to_lowercase().as_str() {
        "fileio" => Ok(FailureCategory::FileIO),
        "networkops" => Ok(FailureCategory::NetworkOps),
        "concurrencyrace" => Ok(FailureCategory::ConcurrencyRace),
        "inputvalidation" => Ok(FailureCategory::InputValidation),
        "templaterendering" => Ok(FailureCategory::TemplateRendering),
        "dependencyresolution" => Ok(FailureCategory::DependencyResolution),
        "memoryexhaustion" => Ok(FailureCategory::MemoryExhaustion),
        "deserialization" => Ok(FailureCategory::Deserialization),
        _ => Err(format!("Invalid category: {}", s)),
    }
}
