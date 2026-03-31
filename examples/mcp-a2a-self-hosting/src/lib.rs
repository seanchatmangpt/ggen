// Library module for MCP/A2A Self-Hosting Example
// Auto-generated from ontology/agent.ttl
// DO NOT EDIT - This file is generated from RDF ontology

pub mod generated;
pub use generated::{agent, skills};

use a2a_rs::prelude::*;
use anyhow::Result;

/// Main agent handler implementing all A2A protocol traits
pub struct GgenAgentHandler {
    // Add state here if needed
}

impl GgenAgentHandler {
    pub fn new() -> Self {
        Self {}
    }
}

// Implement A2A traits using generated skills
// (Implementation would be in generated/skills.rs)

/// MCP handler for Model Context Protocol integration
pub struct McpHandler {
    ontology_path: String,
}

impl McpHandler {
    pub fn new(ontology_path: String) -> Result<Self> {
        Ok(Self { ontology_path })
    }
}

// Implement MCP traits
// (Implementation would be in generated/mcp.rs)
