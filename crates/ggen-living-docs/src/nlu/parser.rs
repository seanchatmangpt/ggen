//! Natural language parser

use super::Intent;
use crate::{Error, Result, ontology::SemanticUpdate};
use regex::Regex;
use tracing::debug;

/// Natural language parser
pub struct NlParser {
    update_pattern: Regex,
    query_pattern: Regex,
}

impl NlParser {
    /// Create a new NL parser
    pub fn new() -> Self {
        Self {
            update_pattern: Regex::new(r"(?i)update\s+(\w+)\s+documentation:\s+(.+)").unwrap(),
            query_pattern: Regex::new(r"(?i)(show|get|find|list|what|where)\s+(.+)").unwrap(),
        }
    }

    /// Parse natural language input into semantic updates
    pub fn parse_updates(&self, input: &str) -> Result<Vec<SemanticUpdate>> {
        debug!("Parsing updates from: {}", input);

        let mut updates = Vec::new();

        // Match update pattern
        if let Some(captures) = self.update_pattern.captures(input) {
            let entity = captures.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
            let documentation = captures.get(2).map(|m| m.as_str().to_string()).unwrap_or_default();

            updates.push(SemanticUpdate::UpdateDocumentation {
                entity_id: entity,
                documentation,
            });
        }

        // Parse triple additions like "X calls Y"
        if input.contains(" calls ") {
            let parts: Vec<&str> = input.split(" calls ").collect();
            if parts.len() == 2 {
                updates.push(SemanticUpdate::AddTriple {
                    subject: parts[0].trim().to_string(),
                    predicate: "calls".to_string(),
                    object: parts[1].trim().to_string(),
                });
            }
        }

        // Parse triple additions like "X implements Y"
        if input.contains(" implements ") {
            let parts: Vec<&str> = input.split(" implements ").collect();
            if parts.len() == 2 {
                updates.push(SemanticUpdate::AddTriple {
                    subject: parts[0].trim().to_string(),
                    predicate: "implements".to_string(),
                    object: parts[1].trim().to_string(),
                });
            }
        }

        Ok(updates)
    }

    /// Parse intent from natural language input
    pub fn parse_intent(&self, input: &str) -> Result<Intent> {
        let input_lower = input.to_lowercase();

        // Check for query intent
        if self.query_pattern.is_match(&input_lower) {
            return Ok(Intent::Query {
                topic: input.to_string(),
            });
        }

        // Check for update intent
        if input_lower.contains("update") && input_lower.contains("documentation") {
            if let Some(captures) = self.update_pattern.captures(input) {
                let entity = captures.get(1).map(|m| m.as_str().to_string()).unwrap_or_default();
                let content = captures.get(2).map(|m| m.as_str().to_string()).unwrap_or_default();

                return Ok(Intent::UpdateDoc { entity, content });
            }
        }

        // Check for relationship intent
        if input_lower.contains(" calls ") || input_lower.contains(" implements ") || input_lower.contains(" uses ") {
            return Ok(Intent::AddRelationship {
                source: String::new(),
                target: String::new(),
                relationship_type: String::new(),
            });
        }

        // Check for search intent
        if input_lower.starts_with("search") || input_lower.starts_with("find") {
            return Ok(Intent::Search {
                query: input.to_string(),
            });
        }

        Ok(Intent::Unknown)
    }
}

impl Default for NlParser {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_update_documentation() {
        let parser = NlParser::new();
        let input = "update execute_sparql documentation: Executes SPARQL queries with caching";
        let updates = parser.parse_updates(input).unwrap();

        assert_eq!(updates.len(), 1);
        if let SemanticUpdate::UpdateDocumentation { entity_id, documentation } = &updates[0] {
            assert_eq!(entity_id, "execute_sparql");
            assert_eq!(documentation, "Executes SPARQL queries with caching");
        } else {
            panic!("Wrong update type");
        }
    }

    #[test]
    fn test_parse_calls_relationship() {
        let parser = NlParser::new();
        let input = "function_a calls function_b";
        let updates = parser.parse_updates(input).unwrap();

        assert_eq!(updates.len(), 1);
        if let SemanticUpdate::AddTriple { subject, predicate, object } = &updates[0] {
            assert_eq!(subject, "function_a");
            assert_eq!(predicate, "calls");
            assert_eq!(object, "function_b");
        } else {
            panic!("Wrong update type");
        }
    }
}
