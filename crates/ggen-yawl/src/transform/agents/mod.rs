//! SPARQL CONSTRUCT queries for A2A agent discovery

pub mod queries;

/// A2A Agent discovery patterns
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum A2aPattern {
    /// Discover agents by capability and state
    AgentDiscovery,
    /// Capability discovery and profiling
    CapabilityMatch,
    /// Skill matching with proficiency scoring
    SkillMatch,
    /// Message construction for A2A communication
    MessageConstruction,
    /// Protocol filtering for compatibility
    ProtocolFilter,
    /// Workflow construction from agent capabilities
    WorkflowConstruction,
}

impl A2aPattern {
    /// Get the query file name for this pattern
    pub fn query_file(&self) -> &'static str {
        match self {
            Self::AgentDiscovery => "01-discover-agents.rq",
            Self::CapabilityMatch => "02-discover-capabilities.rq",
            Self::SkillMatch => "03-match-skills.rq",
            Self::MessageConstruction => "04-construct-messages.rq",
            Self::ProtocolFilter => "05-filter-protocols.rq",
            Self::WorkflowConstruction => "06-build-workflows.rq",
        }
    }

    /// Get the SPARQL query content for this pattern
    pub fn query_content(&self) -> &'static str {
        match self {
            Self::AgentDiscovery => include_str!("../../../queries/01-discover-agents.rq"),
            Self::CapabilityMatch => include_str!("../../../queries/02-discover-capabilities.rq"),
            Self::SkillMatch => include_str!("../../../queries/03-match-skills.rq"),
            Self::MessageConstruction => include_str!("../../../queries/04-construct-messages.rq"),
            Self::ProtocolFilter => include_str!("../../../queries/05-filter-protocols.rq"),
            Self::WorkflowConstruction => include_str!("../../../queries/06-build-workflows.rq"),
        }
    }

    /// Get the query name for registration with ConstructExecutor
    pub fn query_name(&self) -> &'static str {
        match self {
            Self::AgentDiscovery => "a2a_agent_discovery",
            Self::CapabilityMatch => "a2a_capability_match",
            Self::SkillMatch => "a2a_skill_match",
            Self::MessageConstruction => "a2a_message_construction",
            Self::ProtocolFilter => "a2a_protocol_filter",
            Self::WorkflowConstruction => "a2a_workflow_construction",
        }
    }
}

/// Iterator over all A2A patterns
impl IntoIterator for A2aPattern {
    type Item = A2aPattern;
    type IntoIter = std::array::IntoIter<A2aPattern, 6>;

    fn into_iter(self) -> Self::IntoIter {
        [
            Self::AgentDiscovery,
            Self::CapabilityMatch,
            Self::SkillMatch,
            Self::MessageConstruction,
            Self::ProtocolFilter,
            Self::WorkflowConstruction,
        ]
        .into_iter()
    }
}

/// Get all A2A patterns as a slice
pub fn all_patterns() -> &'static [A2aPattern] {
    &[
        A2aPattern::AgentDiscovery,
        A2aPattern::CapabilityMatch,
        A2aPattern::SkillMatch,
        A2aPattern::MessageConstruction,
        A2aPattern::ProtocolFilter,
        A2aPattern::WorkflowConstruction,
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_query_files() {
        assert_eq!(A2aPattern::AgentDiscovery.query_file(), "01-discover-agents.rq");
        assert_eq!(A2aPattern::CapabilityMatch.query_file(), "02-discover-capabilities.rq");
        assert_eq!(A2aPattern::SkillMatch.query_file(), "03-match-skills.rq");
        assert_eq!(
            A2aPattern::MessageConstruction.query_file(),
            "04-construct-messages.rq"
        );
        assert_eq!(A2aPattern::ProtocolFilter.query_file(), "05-filter-protocols.rq");
        assert_eq!(
            A2aPattern::WorkflowConstruction.query_file(),
            "06-build-workflows.rq"
        );
    }

    #[test]
    fn test_pattern_query_names() {
        assert_eq!(A2aPattern::AgentDiscovery.query_name(), "a2a_agent_discovery");
        assert_eq!(A2aPattern::CapabilityMatch.query_name(), "a2a_capability_match");
        assert_eq!(A2aPattern::SkillMatch.query_name(), "a2a_skill_match");
        assert_eq!(
            A2aPattern::MessageConstruction.query_name(),
            "a2a_message_construction"
        );
        assert_eq!(A2aPattern::ProtocolFilter.query_name(), "a2a_protocol_filter");
        assert_eq!(
            A2aPattern::WorkflowConstruction.query_name(),
            "a2a_workflow_construction"
        );
    }

    #[test]
    fn test_query_content_is_valid_sparql() {
        // Verify all queries contain required SPARQL elements
        for pattern in all_patterns() {
            let content = pattern.query_content();
            
            // Must contain CONSTRUCT keyword
            assert!(content.contains("CONSTRUCT"), 
                "Query for {:?} must contain CONSTRUCT keyword", pattern);
            
            // Must contain WHERE clause
            assert!(content.contains("WHERE"), 
                "Query for {:?} must contain WHERE clause", pattern);
            
            // Must contain PREFIX declarations
            assert!(content.contains("PREFIX"), 
                "Query for {:?} must contain PREFIX declarations", pattern);
        }
    }

    #[test]
    fn test_all_patterns_count() {
        assert_eq!(all_patterns().len(), 6);
    }
}
