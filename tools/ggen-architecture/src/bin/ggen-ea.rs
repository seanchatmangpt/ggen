//! CLI for the canonical enterprise-architecture graph.
//!
//! This binary performs deterministic local analysis only.  It has no network,
//! cloud, deployment, shell, or infrastructure actuation path.

use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, Subcommand, ValueEnum};
use ggen_architecture::{
    ArchitectureViewpoint, EnterpriseArchitectureModel, EnterpriseArchitectureReceipt,
};
use serde::de::DeserializeOwned;

#[derive(Debug, Parser)]
#[command(
    name = "ggen-ea",
    version,
    about = "Deterministic enterprise architecture graph analysis and receipts"
)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Validate enterprise/boundary identity, accountability, relationships, and transitions.
    Validate {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Assess static enterprise governance closure without manufacturing ALIVE standing.
    Governance {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Calculate transitive architecture impact across admitted relationships.
    Impact {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Stable architecture element identity.
        #[arg(long)]
        subject: String,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Traverse cross-layer requirement-to-evidence traceability.
    Trace {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Stable architecture element identity.
        #[arg(long)]
        subject: String,
        /// Maximum traversal depth.
        #[arg(long, default_value_t = 16)]
        max_depth: usize,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Emit the capability-to-realization portfolio matrix.
    Portfolio {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Project a disposable architecture viewpoint.
    View {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Viewpoint to project.
        #[arg(long, value_enum)]
        viewpoint: ViewpointArg,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Calculate the deterministic dependency-closed work-package order.
    TransitionOrder {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Stable transition architecture identity.
        #[arg(long)]
        transition: String,
        /// Emit JSON.
        #[arg(long)]
        json: bool,
    },
    /// Manufacture and optionally verify a deterministic static model receipt.
    Receipt {
        /// Canonical EnterpriseArchitectureModel JSON.
        #[arg(long)]
        model: PathBuf,
        /// Operation identity bound into the receipt.
        #[arg(long, default_value = "admit-enterprise-model")]
        operation: String,
        /// Optional predecessor receipt digest.
        #[arg(long)]
        predecessor: Option<String>,
        /// Existing receipt JSON to verify instead of manufacturing a new receipt.
        #[arg(long)]
        verify: Option<PathBuf>,
        /// Emit JSON for a newly manufactured receipt.
        #[arg(long)]
        json: bool,
    },
}

#[derive(Debug, Clone, Copy, ValueEnum)]
enum ViewpointArg {
    Motivation,
    Strategy,
    Business,
    Information,
    Application,
    Technology,
    ImplementationMigration,
    Governance,
    Evidence,
    Full,
}

impl From<ViewpointArg> for ArchitectureViewpoint {
    fn from(value: ViewpointArg) -> Self {
        match value {
            ViewpointArg::Motivation => Self::Motivation,
            ViewpointArg::Strategy => Self::Strategy,
            ViewpointArg::Business => Self::Business,
            ViewpointArg::Information => Self::Information,
            ViewpointArg::Application => Self::Application,
            ViewpointArg::Technology => Self::Technology,
            ViewpointArg::ImplementationMigration => Self::ImplementationMigration,
            ViewpointArg::Governance => Self::Governance,
            ViewpointArg::Evidence => Self::Evidence,
            ViewpointArg::Full => Self::Full,
        }
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(code) => ExitCode::from(code),
        Err(error) => {
            eprintln!("ggen-ea refused: {error}");
            ExitCode::from(2)
        }
    }
}

fn run() -> Result<u8, Box<dyn Error>> {
    let cli = Cli::parse();
    match cli.command {
        Command::Validate { model, json } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            let violations = model.validate();
            if json {
                println!("{}", serde_json::to_string_pretty(&violations)?);
            } else if violations.is_empty() {
                println!("enterprise architecture: structurally admitted");
                println!("standing: static admission only; runtime standing unchanged");
            } else {
                for violation in &violations {
                    println!(
                        "{} {}: {}",
                        violation.code, violation.subject, violation.message
                    );
                }
            }
            Ok(if violations.is_empty() { 0 } else { 2 })
        }
        Command::Governance { model, json } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            let assessment = model.assess_governance();
            if json {
                println!("{}", serde_json::to_string_pretty(&assessment)?);
            } else {
                println!("structurally_valid: {}", assessment.structurally_valid);
                println!("statically_complete: {}", assessment.statically_complete);
                println!("findings: {}", assessment.findings.len());
                println!("runtime_standing_promoted: false");
                for finding in &assessment.findings {
                    println!(
                        "[{:?}] {} {}: {}",
                        finding.severity, finding.code, finding.subject, finding.message
                    );
                }
            }
            Ok(
                if assessment.structurally_valid && assessment.statically_complete {
                    0
                } else {
                    2
                },
            )
        }
        Command::Impact {
            model,
            subject,
            json,
        } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            let report = model.impact(&subject)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&report)?);
            } else {
                println!("subject: {}", report.subject);
                println!(
                    "impacted: {}",
                    report
                        .impacted
                        .iter()
                        .map(String::as_str)
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                println!(
                    "relations: {}",
                    report
                        .traversed_relations
                        .iter()
                        .map(String::as_str)
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }
            Ok(0)
        }
        Command::Trace {
            model,
            subject,
            max_depth,
            json,
        } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            let hops = model.traceability(&subject, max_depth)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&hops)?);
            } else {
                for hop in &hops {
                    println!(
                        "{}: {} -{:?}-> {} [{}]",
                        hop.depth, hop.from, hop.kind, hop.to, hop.relation_id
                    );
                }
            }
            Ok(0)
        }
        Command::Portfolio { model, json } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            let portfolio = model.capability_portfolio();
            if json {
                println!("{}", serde_json::to_string_pretty(&portfolio)?);
            } else {
                for (capability, realizations) in portfolio {
                    println!(
                        "{}: {}",
                        capability,
                        realizations
                            .iter()
                            .map(String::as_str)
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
            }
            Ok(0)
        }
        Command::View {
            model,
            viewpoint,
            json,
        } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            let view = model.project(viewpoint.into());
            if json {
                println!("{}", serde_json::to_string_pretty(&view)?);
            } else {
                println!("viewpoint: {:?}", view.viewpoint);
                println!("elements: {}", view.elements.len());
                println!("relations: {}", view.relations.len());
                for element in view.elements.values() {
                    println!("{} [{:?}] {}", element.id, element.kind, element.name);
                }
            }
            Ok(0)
        }
        Command::TransitionOrder {
            model,
            transition,
            json,
        } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            let order = model.transition_order(&transition)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&order)?);
            } else {
                println!("{}", order.join(" -> "));
            }
            Ok(0)
        }
        Command::Receipt {
            model,
            operation,
            predecessor,
            verify,
            json,
        } => {
            let model: EnterpriseArchitectureModel = read_json(&model)?;
            if let Some(receipt_path) = verify {
                let receipt: EnterpriseArchitectureReceipt = read_json(&receipt_path)?;
                let verified = model.verify_receipt(&receipt);
                println!("verified: {verified}");
                return Ok(if verified { 0 } else { 2 });
            }

            let receipt = model.receipt(operation, predecessor)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&receipt)?);
            } else {
                println!("schema: {}", receipt.schema);
                println!("subject_digest: {}", receipt.subject_digest);
                if let Some(predecessor) = &receipt.predecessor_digest {
                    println!("predecessor_digest: {predecessor}");
                }
                println!("operation: {}", receipt.operation);
                println!("consequence_digest: {}", receipt.consequence_digest);
                println!("runtime_execution_claimed: false");
            }
            Ok(0)
        }
    }
}

fn read_json<T: DeserializeOwned>(path: &Path) -> Result<T, Box<dyn Error>> {
    let bytes = fs::read(path)?;
    Ok(serde_json::from_slice(&bytes)?)
}
