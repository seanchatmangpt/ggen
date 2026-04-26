//! MCPP domain — runtime/state models promoted from
//! `chatmangpt-mcpp-v2-cell` per
//! `portfolio-obl-0002-promote-v2-constraints-into-canonical-ggen-mcpp`.

pub mod andon_state;
pub mod control_pack;
pub mod line_state;
pub mod mcp_config;

pub use andon_state::{AndonEvent, AndonState};
pub use control_pack::{
    AgentRole, Agents, CompletionContract, ControlPack, ExtensionPolicy, Gate, Naming,
    TruthHierarchy, CONTROL_PACK_SCHEMA,
};
pub use line_state::{AgentRoles, LineState, Wip};
