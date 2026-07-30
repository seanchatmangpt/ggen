//! Command-line surface for enterprise architecture machinery.

use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, Subcommand};
use ggen_architecture::{
    ArchitectureState, AutonomicController, CapacityEnvelope, DoctorReport, DoctorStatus,
    Fortune5Assessment, Fortune5AutonomicPlan, Fortune5Catalog, Fortune5Program,
    LevelFiveCrownAssessment, LevelFiveCrownProgram, Severity, Stimulus,
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
    /// Inspect, assess, plan, or crown against the Fortune 5 Level-5 contract.
    Fortune5 {
        /// Fortune 5 operation.
        #[command(subcommand)]
        command: Fortune5Command,
    },
}

#[derive(Debug, Subcommand)]
enum Fortune5Command {
    /// Emit the canonical twenty-one-dimension, ninety-nine-control, sixty-three-obligation catalog.
    Catalog {
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Assess an evidence program against the conjunctive Level-5 profile.
    Assess {
        /// Fortune 5 evidence program JSON.
        #[arg(long)]
        program: PathBuf,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Emit bounded autonomic intents from a Fortune 5 assessment.
    Plan {
        /// Fortune 5 evidence program JSON.
        #[arg(long)]
        program: PathBuf,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Assess the exact crown: 21/99/63 plus release truths, SLA governors, guards, replay, and zero actuation.
    Crown {
        /// Fortune 5 evidence program JSON.
        #[arg(long)]
        program: PathBuf,
        /// Crown evidence and operational policy JSON.
        #[arg(long)]
        crown: PathBuf,
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
            let state: ArchitectureState = read_json(&state)?;
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
        Command::Impact { state, asset, json } => {
            let state: ArchitectureState = read_json(&state)?;
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
            let state: ArchitectureState = read_json(&state)?;
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
            let state: ArchitectureState = read_json(&state)?;
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
            Ok(
                if violations
                    .iter()
                    .any(|violation| violation.severity >= Severity::Error)
                {
                    2
                } else {
                    0
                },
            )
        }
        Command::Capacity { state, json } => {
            let state: ArchitectureState = read_json(&state)?;
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
        Command::Fortune5 { command } => match command {
            Fortune5Command::Catalog { json } => {
                let catalog = Fortune5Catalog::canonical();
                if json {
                    println!("{}", serde_json::to_string_pretty(&catalog)?);
                } else {
                    let control_count: usize = catalog
                        .dimensions
                        .iter()
                        .map(|dimension| dimension.required_controls.len())
                        .sum();
                    println!("profile: {}", catalog.profile);
                    println!("dimensions: {}", catalog.dimensions.len());
                    println!("controls: {control_count}");
                    println!("proof obligations: {}", catalog.obligations().count());
                }
                Ok(if catalog.validate().is_empty() { 0 } else { 2 })
            }
            Fortune5Command::Assess { program, json } => {
                let program: Fortune5Program = read_json(&program)?;
                let assessment = Fortune5Assessment::assess(&program)?;
                if json {
                    println!("{}", serde_json::to_string_pretty(&assessment)?);
                } else {
                    print!("{}", assessment.render_text());
                }
                Ok(if assessment.level_five_ready { 0 } else { 2 })
            }
            Fortune5Command::Plan { program, json } => {
                let program: Fortune5Program = read_json(&program)?;
                let assessment = Fortune5Assessment::assess(&program)?;
                let plan = Fortune5AutonomicPlan::plan(&assessment)?;
                if json {
                    println!("{}", serde_json::to_string_pretty(&plan)?);
                } else {
                    println!("assessment: {}", plan.assessment_receipt);
                    println!("intents: {}", plan.intents.len());
                    println!("actuation_performed: {}", plan.actuation_performed);
                    println!("receipt: {}", plan.receipt_hash);
                }
                Ok(0)
            }
            Fortune5Command::Crown {
                program,
                crown,
                json,
            } => {
                let program: Fortune5Program = read_json(&program)?;
                let crown: LevelFiveCrownProgram = read_json(&crown)?;
                let assessment = Fortune5Assessment::assess(&program)?;
                let crown_assessment = LevelFiveCrownAssessment::assess(&assessment, &crown)?;
                if json {
                    println!("{}", serde_json::to_string_pretty(&crown_assessment)?);
                } else {
                    println!("program: {}", crown_assessment.program);
                    println!(
                        "taxonomy: {}/{}/{}",
                        crown_assessment.taxonomy.dimensions,
                        crown_assessment.taxonomy.controls,
                        crown_assessment.taxonomy.obligations
                    );
                    println!(
                        "release truths: {}/6",
                        crown_assessment.release_truths_alive
                    );
                    println!(
                        "SLA governors: {}/5",
                        crown_assessment.sla_governors_alive
                    );
                    println!(
                        "operational controls: {}/6",
                        crown_assessment.operational_controls_alive
                    );
                    println!("structurally_ready: {}", crown_assessment.structurally_ready);
                    println!("promotion_ready: {}", crown_assessment.promotion_ready);
                    println!("synthetic: {}", crown_assessment.synthetic);
                    println!("receipt: {}", crown_assessment.receipt_hash);
                }
                Ok(if crown_assessment.structurally_ready {
                    0
                } else {
                    2
                })
            }
        },
    }
}

fn read_json<T: DeserializeOwned>(path: &Path) -> Result<T, Box<dyn Error>> {
    let bytes = fs::read(path)?;
    Ok(serde_json::from_slice(&bytes)?)
}
