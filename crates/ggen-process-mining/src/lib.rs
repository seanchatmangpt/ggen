//! # ggen-process-mining: Process Mining Integration for ggen
//!
//! This crate integrates high-performance process mining capabilities from pictl
//! into the ggen manufacturing pipeline.
//!
//! - **Process Discovery**: Alpha miner implementation for Petri net discovery.
//! - **Conformance Checking**: Alignment and token replay algorithms.
//! - **YAWL Bridge**: Bidirectional conversion between YAWL and Petri nets.
//! - **Native Integration**: Direct use of pictl-types and pictl-algos.

#![deny(warnings)]
#![warn(missing_docs)]
#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::module_name_repetitions)]

pub mod conformance;
pub mod discovery;
pub mod error;
pub mod event_log;
pub mod ocel;
pub mod petri_net;
pub mod xes;
pub mod yawl_bridge;

pub use conformance::{ConformanceChecker, ConformanceResult};
pub use discovery::{PetriNet, ProcessMiner};
pub use error::{Error, Result};
pub use event_log::{AttributeValue, Event, EventLog, Trace, EventLogExt};
pub use ocel::{OcelEvent, OcelLog, OcelObject, OcelParser};
pub use petri_net::{Arc, Marking, Place, Transition, PetriNetExt};
pub use xes::XesParser;
pub use yawl_bridge::YawlBridge;

/// Current version of ggen-process-mining.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");
