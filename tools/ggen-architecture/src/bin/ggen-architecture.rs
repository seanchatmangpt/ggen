//! Command-line surface for enterprise architecture machinery.

use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, Subcommand};
use ggen_architecture::{
    AutonomicController, CapacityEnvelope, DoctorReport, DoctorStatus, Severity, Stimulus,
};
use serde::de::DeserializeOwned;

#[derive(Debug, Parser)]
#[command(
    name = "ggen-architecture",
    version,
    about = "Executable enterprise architecture machinery, automation, and autonomic intents"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Diagnose lifecycle, dependency, capacity, standing, and autonomic policy.
    Doctor {
        /// Architecture state JSON.
        #[arg(long)]
        state: PathBuf,
        /// Emit JSON instead of human-readable text.
        #[arg(long)]
        json: bool,
    },
    /// Calculate transitive impact and revalidation order for one changed asset.
    Impact {
        /// Architecture state JSON.
        #[arg(long)]
        state: PathBuf,
        /// Stable architecture asset identifier.
        #[arg(long)]
        asset: String,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Run one MAPE-K cycle and emit bounded intents without actuation.
    Cycle {
        /// Architecture state JSON.
        #[arg(long)]
        state: PathBuf,
        /// JSON array of admitted stimuli.
        #[arg(long)]
        stimuli: PathBuf,
        /// Timestamp or deterministic sequence label carried into the receipt.
        #[arg(long)]
        observed_at: String,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Validate registry closure and dependency law.
    Validate {
        /// Architecture state JSON.
        #[arg(long)]
        state: PathBuf,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Analyze observed capacity samples and first breaking points.
    Capacity {
        /// Architecture state JSON.
        #[arg(long)]
        state: PathBuf,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
}

fn main() -> ExitCode {
    match run() {
        Ok(code) => ExitCode::from(code),
        Err(error) => {
            eprintln!("ggen-architecture refused: {error}");
            ExitCode::from(2)
        }
    }
}

fn run() -> Result<u8, Box<dyn Error>> {
    let cli = Cli::parse();
    match cli.command {
        Command::Doctor { state, json } => {
            let state = read_json(&state)?;
            let report = DoctorReport::analyze(&state)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&report)?);
            } else {
                print!("{}", report.render_text());
            }
            Ok(if report.status == DoctorStatus::Refused {
                2
            } else {
                0
            })
        }
        Command::Impact {
            state,
            asset,
            json,
        } => {
            let state = read_json(&state)?;
            let report = state.registry.impact_report(&asset)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&report)?);
            } else {
                println!("root: {}", report.root);
                println!("affected: {}", report.affected.join(", "));
                println!(
                    "revalidation order: {}",
                    report.ordered_revalidation.join(" -> ")
                );
            }
            Ok(0)
        }
        Command::Cycle {
            state,
            stimuli,
            observed_at,
            json,
        } => {
            let state = read_json(&state)?;
            let stimuli: Vec<Stimulus> = read_json(&stimuli)?;
            let cycle = AutonomicController::new(&state).run_cycle(observed_at, stimuli)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&cycle)?);
            } else {
                println!("cycle: {}", cycle.cycle_id);
                println!("diagnoses: {}", cycle.diagnoses.len());
                println!("intents: {}", cycle.intents.len());
                println!("actuation_performed: {}", cycle.actuation_performed);
            }
            Ok(0)
        }
        Command::Validate { state, json } => {
            let state = read_json(&state)?;
            let violations = state.registry.validate();
            if json {
                println!("{}", serde_json::to_string_pretty(&violations)?);
            } else if violations.is_empty() {
                println!("architecture registry: ALIVE");
            } else {
                for violation in &violations {
                    println!(
                        "[{:?}] {} {}: {}",
                        violation.severity, violation.code, violation.asset_id, violation.message
                    );
                }
            }
            Ok(if violations
                .iter()
                .any(|violation| violation.severity >= Severity::Error)
            {
                2
            } else {
                0
            })
        }
        Command::Capacity { state, json } => {
            let state = read_json(&state)?;
            let envelope =
                CapacityEnvelope::analyze(&state.capacity_samples, &state.capacity_policy);
            if json {
                println!("{}", serde_json::to_string_pretty(&envelope)?);
            } else {
                println!("samples: {}", envelope.samples.len());
                println!("latest level: {:?}", envelope.latest_level);
                println!(
                    "first warning: {}",
                    envelope.first_warning.as_deref().unwrap_or("not observed")
                );
                println!(
                    "first refusal: {}",
                    envelope.first_refusal.as_deref().unwrap_or("not observed")
                );
                println!(
                    "first knee: {}",
                    envelope.first_knee.as_deref().unwrap_or("not observed")
                );
            }
            Ok(0)
        }
    }
}

fn read_json<T: DeserializeOwned>(path: &Path) -> Result<T, Box<dyn Error>> {
    let bytes = fs::read(path)?;
    Ok(serde_json::from_slice(&bytes)?)
}
