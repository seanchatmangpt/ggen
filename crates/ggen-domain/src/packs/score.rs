//! Pack maturity scoring (similar to marketplace scoring)

use crate::packs::types::Pack;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Pack maturity score
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackScore {
    pub pack_id: String,
    pub total_score: u32,
    pub maturity_level: String,
    pub scores: DimensionScores,
    pub feedback: Vec<String>,
}

/// Dimension scores
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DimensionScores {
    pub documentation: u32,
    pub completeness: u32,
    pub quality: u32,
    pub usability: u32,
}

/// Score a pack for maturity
pub fn score_pack(pack: &Pack) -> Result<PackScore> {
    let mut scores = DimensionScores {
        documentation: 0,
        completeness: 0,
        quality: 0,
        usability: 0,
    };

    let mut feedback = Vec::new();

    // Documentation scoring (0-25 points)
    if !pack.description.is_empty() {
        scores.documentation += 10;
    }
    if pack.description.len() > 100 {
        scores.documentation += 10;
        feedback.push("Good description length".to_string());
    }
    if pack.metadata.documentation_files.unwrap_or(0) > 0 {
        scores.documentation += 5;
    }

    // Completeness scoring (0-25 points)
    if !pack.packages.is_empty() {
        scores.completeness += 10;
    }
    if pack.packages.len() >= 3 {
        scores.completeness += 5;
        feedback.push("Multiple packages included".to_string());
    }
    if !pack.templates.is_empty() {
        scores.completeness += 5;
    }
    if !pack.sparql_queries.is_empty() {
        scores.completeness += 5;
    }

    // Quality scoring (0-25 points)
    if pack.production_ready {
        scores.quality += 15;
        feedback.push("Marked as production ready".to_string());
    }
    if pack.metadata.test_coverage.is_some() {
        scores.quality += 5;
    }
    if pack.metadata.code_examples.unwrap_or(0) > 0 {
        scores.quality += 5;
    }

    // Usability scoring (0-25 points)
    if !pack.tags.is_empty() {
        scores.usability += 5;
    }
    if !pack.keywords.is_empty() {
        scores.usability += 5;
    }
    if pack.author.is_some() {
        scores.usability += 5;
    }
    if pack.repository.is_some() {
        scores.usability += 5;
        feedback.push("Repository link available".to_string());
    }
    if pack.license.is_some() {
        scores.usability += 5;
    }

    let total_score =
        scores.documentation + scores.completeness + scores.quality + scores.usability;

    let maturity_level = match total_score {
        0..=40 => "experimental",
        41..=60 => "beta",
        61..=80 => "production",
        _ => "enterprise",
    };

    Ok(PackScore {
        pack_id: pack.id.clone(),
        total_score,
        maturity_level: maturity_level.to_string(),
        scores,
        feedback,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::types::{PackMetadata, PackTemplate};
    use std::collections::HashMap;

    #[test]
    fn test_score_pack_empty() {
        let pack = Pack {
            id: "test".to_string(),
            name: "Test".to_string(),
            version: "1.0.0".to_string(),
            description: String::new(),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: vec![],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: false,
            metadata: PackMetadata::default(),
        };

        let score = score_pack(&pack).unwrap();
        assert_eq!(score.maturity_level, "experimental");
    }

    #[test]
    fn test_score_pack_production_ready() {
        let pack = Pack {
            id: "test".to_string(),
            name: "Test".to_string(),
            version: "1.0.0".to_string(),
            description: "A comprehensive test pack with detailed description for production use"
                .to_string(),
            category: "test".to_string(),
            author: Some("Test Author".to_string()),
            repository: Some("https://github.com/test/pack".to_string()),
            license: Some("MIT".to_string()),
            packages: vec!["pkg1".to_string(), "pkg2".to_string(), "pkg3".to_string()],
            templates: vec![PackTemplate {
                name: "template1".to_string(),
                path: "path/to/template".to_string(),
                description: "Test template".to_string(),
                variables: vec![],
            }],
            sparql_queries: {
                let mut queries = HashMap::new();
                queries.insert("query1".to_string(), "SELECT * WHERE {}".to_string());
                queries
            },
            dependencies: vec![],
            tags: vec!["tag1".to_string()],
            keywords: vec!["keyword1".to_string()],
            production_ready: true,
            metadata: PackMetadata {
                test_coverage: Some("95%".to_string()),
                code_examples: Some(5),
                documentation_files: Some(3),
                ..Default::default()
            },
        };

        let score = score_pack(&pack).unwrap();
        assert!(
            score.total_score >= 61,
            "Expected production or enterprise level"
        );
    }
}
