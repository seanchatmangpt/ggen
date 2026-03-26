//! Domain Agents - Autonomous agents managing life domains

pub mod base;
pub mod health;
pub mod career;
pub mod relationships;
pub mod finance;
pub mod learning;
pub mod spirituality;

pub use base::{AgentBase, AgentStatus};
pub use health::HealthAgent;
pub use career::CareerAgent;
pub use relationships::RelationshipAgent;
pub use finance::FinanceAgent;
pub use learning::LearningAgent;
pub use spirituality::SpiritualityAgent;
