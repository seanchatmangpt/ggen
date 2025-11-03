//! Core Agent Implementations
//!
//! Critical agents that power autonomous workflows (80/20 principle):
//! - GraphEvolutionAgent: Manages knowledge graph evolution
//! - RegenerationAgent: Handles code regeneration workflows
//! - FeedbackAgent: Processes feedback loops

pub mod feedback;
pub mod graph_evolution;
pub mod regeneration;

pub use feedback::FeedbackAgent;
pub use graph_evolution::GraphEvolutionAgent;
pub use regeneration::RegenerationAgent;
