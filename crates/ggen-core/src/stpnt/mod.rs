//! Stewards of the Pentecost (stpnt) module
//! 
//! This module implements the stewardship obligations and their integration
//! with external surfaces like GitHub.

pub mod obligation;
pub mod github;

pub use obligation::{StewardshipObligation, StewardshipEvent, CanonSet, TerminalState, ObligationStatus};
pub use github::GitHubStewardshipMembrane;
