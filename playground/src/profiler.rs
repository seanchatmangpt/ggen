//! Π-Profiler: Map research shards to HTF categories
//!
//! Shows how your specific work fits into the HTF framework

use crate::models::*;
use crate::ontology;
use crate::Result;
use std::collections::HashMap;

/// Generate Π-profile showing coverage across families
pub fn profile_thesis(shards: Vec<DeltaShard>) -> Result<PiProfile> {
    let all_families = ontology::all_families();
    let mut coverage: HashMap<ShardFamily, f32> = HashMap::new();
    let mut total_words = 0;

    // Initialize all families with 0% coverage
    for family in &all_families {
        coverage.insert(*family, 0.0);
    }

    // Count words per family
    let mut family_words: HashMap<ShardFamily, usize> = HashMap::new();
    let shards_map: HashMap<String, DeltaShard> =
        shards.iter().cloned().map(|s| (s.id.clone(), s)).collect();

    for shard in &shards {
        let word_count = shard.content.split_whitespace().count();
        total_words += word_count;
        family_words
            .entry(shard.family)
            .and_modify(|w| *w += word_count)
            .or_insert(word_count);
    }

    // Calculate coverage percentages
    if total_words > 0 {
        for family in all_families {
            let words = family_words.get(&family).copied().unwrap_or(0);
            coverage.insert(family, (words as f32 / total_words as f32) * 100.0);
        }
    }

    Ok(PiProfile {
        thesis_id: uuid::Uuid::new_v4().to_string(),
        shards: shards_map,
        coverage,
        total_words,
    })
}

/// Generate a detailed coverage report
pub fn generate_coverage_report(profile: &PiProfile) -> String {
    let mut report = String::new();
    report.push_str("=== HTF Coverage Report ===\n\n");
    report.push_str(&format!("Total Words: {}\n\n", profile.total_words));

    let mut families: Vec<_> = profile.coverage.iter().collect();
    families.sort_by(|a, b| b.1.partial_cmp(a.1).unwrap_or(std::cmp::Ordering::Equal));

    report.push_str("Coverage by Family:\n");
    for (family, coverage) in families {
        let bar_length = (*coverage as usize) / 5;
        let bar = "█".repeat(bar_length.min(20));
        report.push_str(&format!(
            "  {:15} | {:20} | {:6.1}%\n",
            format!("{:?}", family),
            bar,
            coverage
        ));
    }

    report.push_str("\n\nUncovered Families:\n");
    for (family, coverage) in &profile.coverage {
        if *coverage == 0.0 {
            report.push_str(&format!("  - {:?}\n", family));
        }
    }

    report
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profile_thesis() {
        let shards = vec![
            DeltaShard {
                id: "intro-1".to_string(),
                name: "Intro".to_string(),
                family: ShardFamily::Intro,
                content: "Introduction text. ".repeat(50),
                status: ShardStatus::Draft,
                dependencies: vec![],
            },
            DeltaShard {
                id: "problem-1".to_string(),
                name: "Problem".to_string(),
                family: ShardFamily::Problem,
                content: "Problem statement. ".repeat(50),
                status: ShardStatus::Draft,
                dependencies: vec![],
            },
        ];

        let profile = profile_thesis(shards).expect("Should profile thesis");
        assert_eq!(profile.total_words > 0, true);
        assert!(profile.coverage.get(&ShardFamily::Intro).unwrap() > &0.0);
    }
}
