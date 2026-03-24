//! Agent implementations
//!
//! Different agent types demonstrating tool discovery and use:
//! - ResearchAgent: discovers and uses tools to gather information
//! - CodeAgent: uses code generation, validation, formatting tools
//! - DataAgent: uses data analysis and transformation tools

pub mod base;
pub mod research;
pub mod code;
pub mod data;

pub use base::{Agent, AgentBase, AgentConfig};
pub use research::ResearchAgent;
pub use code::CodeAgent;
pub use data::DataAgent;
