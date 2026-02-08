//! # ggen-process-mining: Process Mining Integration for ggen
//!
//! This crate integrates process mining capabilities with ggen's workflow generation
//! system, enabling:
//!
//! - **Process Discovery**: Extract process models from event logs using Alpha++ algorithm
//! - **Conformance Checking**: Validate event logs against YAWL workflow specifications
//! - **YAWL↔PetriNet Bridge**: Convert between YAWL workflows and Petri nets
//! - **XES/OCEL Support**: Parse and process standard event log formats
//!
//! ## Architecture
//!
//! ```text
//! Event Logs (XES/OCEL)          YAWL Workflows
//!         ↓                              ↓
//! ┌─────────────────────────────────────────────┐
//! │         ggen-process-mining                 │
//! │  ┌──────────────┐      ┌──────────────┐    │
//! │  │  Alpha++     │      │   YAWL →     │    │
//! │  │  Discovery   │      │  PetriNet    │    │
//! │  └──────────────┘      └──────────────┘    │
//! │         ↓                      ↓             │
//! │  ┌──────────────────────────────────┐      │
//! │  │      Conformance Checking        │      │
//! │  └──────────────────────────────────┘      │
//! └─────────────────────────────────────────────┘
//!         ↓                              ↓
//!    Petri Net Models                Analysis Results
//! ```
//!
//! ## Usage
//!
//! ### Process Discovery from Event Logs
//!
//! ```rust,no_run
//! use ggen_process_mining::{ProcessMiner, EventLog};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Load XES event log
//! let log = EventLog::from_xes_file("logs.xes")?;
//!
//! // Discover process model using Alpha++
//! let miner = ProcessMiner::new();
//! let petri_net = miner.discover_alpha_plusplus(&log)?;
//!
//! // Convert to YAWL for execution
//! let yawl_workflow = miner.to_yawl(petri_net)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Conformance Checking
//!
//! ```rust,no_run
//! use ggen_process_mining::{ProcessMiner, EventLog, YawlModel};
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Load YAWL workflow specification
//! let model = YawlModel::from_file("workflow.yawl")?;
//!
//! // Load event log for validation
//! let log = EventLog::from_xes_file("execution.xes")?;
//!
//! // Check conformance
//! let miner = ProcessMiner::new();
//! let report = miner.check_conformance(&model, &log)?;
//!
//! println!("Fitness: {:.2}%", report.fitness() * 100.0);
//! # Ok(())
//! # }
//! ```

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time
#![warn(missing_docs)]
#![warn(clippy::all, clippy::pedantic)]

pub mod conformance;
pub mod discovery;
pub mod error;
pub mod event_log;
pub mod ocel;
pub mod petri_net;
pub mod xes;
pub mod yawl_bridge;

pub use conformance::{Alignment, ConformanceChecker, ConformanceReport};
pub use discovery::{AlphaPlusPlus, ProcessMiner};
pub use error::{Error, Result};
pub use event_log::{Event, EventLog, Trace};
pub use ocel::{OcelEvent, OcelObject, OcelParser};
pub use petri_net::{Marking, PetriNet, Place, Transition};
pub use xes::{XesParser, XesWriter};
pub use yawl_bridge::{PetriNetToYawl, YawlBridge, YawlToPetriNet};

/// Current version of ggen-process-mining.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Create a new process miner with default configuration.
///
/// This is a convenience function for [`ProcessMiner::new`].
///
/// # Example
///
/// ```rust
/// use ggen_process_mining::process_miner;
///
/// let miner = process_miner();
/// ```
#[must_use]
pub fn process_miner() -> ProcessMiner {
    ProcessMiner::new()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_is_set() {
        assert!(!VERSION.is_empty());
        assert!(VERSION.starts_with('0'));
    }

    #[test]
    fn test_process_miner_creation() {
        let miner = ProcessMiner::new();
        assert!(miner.config().validate_output);
    }

    #[test]
    fn test_process_miner_convenience_function() {
        let miner = process_miner();
        assert!(miner.config().validate_output);
    }
}
