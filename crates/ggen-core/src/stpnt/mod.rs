//! Stewards of the Pentecost (stpnt) module
//!
//! This module implements the stewardship obligations and their integration
//! with external surfaces like GitHub.

pub mod github;
pub mod obligation;

pub use github::GitHubStewardshipMembrane;
pub use obligation::{
    CanonSet, ObligationStatus, StewardshipEvent, StewardshipObligation, TerminalState,
};
