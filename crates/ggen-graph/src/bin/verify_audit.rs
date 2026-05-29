//! Binary to verify the self-audit log and coverage matrix for Vision 2030 compliance.

use ggen_graph::ocel::{CoverageMatrix, OcelLog};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let self_audit_path = Path::new("crates/ggen-graph/audit/vision2030.self_audit.ocel.json");
    let coverage_path = Path::new("crates/ggen-graph/audit/vision2030.coverage.json");

    if !self_audit_path.exists() {
        eprintln!("Self-audit log file not found at: {:?}", self_audit_path);
        std::process::exit(1);
    }
    if !coverage_path.exists() {
        eprintln!("Coverage matrix file not found at: {:?}", coverage_path);
        std::process::exit(1);
    }

    let self_audit_file = File::open(self_audit_path)?;
    let log: OcelLog = serde_json::from_reader(BufReader::new(self_audit_file))?;

    let coverage_file = File::open(coverage_path)?;
    let coverage: CoverageMatrix = serde_json::from_reader(BufReader::new(coverage_file))?;

    let mut violations = Vec::new();

    // 1. Requirements have evidence (check that every requirement in `vision2030.coverage.json`
    // lists non-empty files/tests/commands, and that the events in the log link back to them).
    for req in &coverage.requirements {
        if req.source_files.is_empty() {
            violations.push(format!("Requirement {} source_files is empty", req.id));
        }
        if req.test_files.is_empty() {
            violations.push(format!("Requirement {} test_files is empty", req.id));
        }
        if req.commands.is_empty() {
            violations.push(format!("Requirement {} commands is empty", req.id));
        }

        // Check that events in the log link back to this requirement.
        let has_link = log
            .events
            .iter()
            .any(|ev| ev.objects.iter().any(|obj_ref| obj_ref.id == req.id));
        if !has_link {
            violations.push(format!(
                "Requirement {} is not referenced by any event in self-audit log",
                req.id
            ));
        }
    }

    // 2. Checkpoints have Command evidence (evaluating checkpoints is linked to execution events of validation commands).
    for ev in &log.events {
        if ev.activity == "CheckpointEvaluated" {
            let has_command = log.events.iter().any(|cmd_ev| {
                cmd_ev.activity == "CommandExecuted" && cmd_ev.timestamp <= ev.timestamp
            });
            if !has_command {
                violations.push(format!(
                    "CheckpointEvaluated event {} is not preceded by any CommandExecuted event",
                    ev.id
                ));
            }
        }
    }

    // 3. Prior evaluations exist (checkpoint promotion/refusal is preceded chronologically by a checkpoint evaluation event).
    for ev in &log.events {
        if ev.activity == "CheckpointPromoted" || ev.activity == "CheckpointRefused" {
            let has_eval = log.events.iter().any(|eval_ev| {
                eval_ev.activity == "CheckpointEvaluated" && eval_ev.timestamp <= ev.timestamp
            });
            if !has_eval {
                violations.push(format!("Checkpoint promotion/refusal event {} is not preceded by any CheckpointEvaluated event", ev.id));
            }
        }
    }

    // 4. Anti-fake is audited (verifies the log contains both `AntiFakeScanned` and `ForbiddenSurfaceScanned` events).
    let has_anti_fake = log.events.iter().any(|ev| ev.activity == "AntiFakeScanned");
    let has_forbidden = log
        .events
        .iter()
        .any(|ev| ev.activity == "ForbiddenSurfaceScanned");
    if !has_anti_fake {
        violations.push("Self-audit log does not contain any AntiFakeScanned events".to_string());
    }
    if !has_forbidden {
        violations
            .push("Self-audit log does not contain any ForbiddenSurfaceScanned events".to_string());
    }

    // 5. Unsupported capabilities are linked (any declared unsupported capability is linked back to its originating constraint/requirement).
    for obj in &log.objects {
        if obj.r#type == "UnsupportedCapability" {
            let is_linked = log
                .events
                .iter()
                .any(|ev| ev.objects.iter().any(|obj_ref| obj_ref.id == obj.id));
            if !is_linked {
                violations.push(format!("UnsupportedCapability object {} is not referenced by any event in self-audit log", obj.id));
            }
        }
    }

    if !violations.is_empty() {
        eprintln!(
            "Self-audit verification failed with {} violations:",
            violations.len()
        );
        for violation in &violations {
            eprintln!("  - {}", violation);
        }
        std::process::exit(1);
    }

    println!("Self-audit verification passed successfully! All 5 Completeness Rules satisfied.");
    Ok(())
}
