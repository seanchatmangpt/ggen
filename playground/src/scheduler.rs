//! Λ-Scheduler: Chapter planning from canonical ordering
//!
//! Maps shards into chapters respecting Λ-total order

use crate::models::*;
use crate::ontology;
use crate::{Error, Result};
use std::collections::HashMap;

/// Plan chapters from shards using Λ-ordering
pub fn schedule_chapters(
    shards: Vec<DeltaShard>, target_chapter_size: usize,
) -> Result<ChapterPlan> {
    // Sort shards according to Λ-ordering
    let lambda_order = ontology::lambda_order();
    let family_index: HashMap<ShardFamily, usize> = lambda_order
        .iter()
        .enumerate()
        .map(|(i, f)| (*f, i))
        .collect();

    let mut sorted_shards = shards.clone();
    sorted_shards.sort_by_key(|s| family_index.get(&s.family).copied().unwrap_or(1000));

    // Group shards into chapters
    let mut chapters = Vec::new();
    let mut current_chapter_shards = Vec::new();
    let mut current_words = 0;

    for shard in &sorted_shards {
        let shard_words = shard.content.split_whitespace().count();
        current_words += shard_words;

        current_chapter_shards.push(shard.id.clone());

        if current_words >= target_chapter_size {
            let chapter =
                create_chapter(chapters.len() + 1, &current_chapter_shards, &sorted_shards)?;
            chapters.push(chapter);
            current_chapter_shards.clear();
            current_words = 0;
        }
    }

    // Add final chapter if not empty
    if !current_chapter_shards.is_empty() {
        let chapter = create_chapter(chapters.len() + 1, &current_chapter_shards, &sorted_shards)?;
        chapters.push(chapter);
    }

    Ok(ChapterPlan {
        thesis_id: uuid::Uuid::new_v4().to_string(),
        chapters,
    })
}

fn create_chapter(
    number: usize, shard_ids: &[String], all_shards: &[DeltaShard],
) -> Result<Chapter> {
    let mut families = Vec::new();
    let mut estimated_words = 0;

    for shard_id in shard_ids {
        if let Some(shard) = all_shards.iter().find(|s| &s.id == shard_id) {
            families.push(shard.family);
            estimated_words += shard.content.split_whitespace().count();
        }
    }

    families.sort_by_key(|f| (*f as u8));
    families.dedup();

    Ok(Chapter {
        number,
        title: format!(
            "Chapter {}: {:?}",
            number,
            families.first().unwrap_or(&ShardFamily::Intro)
        ),
        shards: shard_ids.to_vec(),
        estimated_words,
        families,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_schedule_chapters() {
        let shards = vec![
            DeltaShard {
                id: "intro-1".to_string(),
                name: "Introduction".to_string(),
                family: ShardFamily::Intro,
                content: "This is the introduction section. ".repeat(100),
                status: ShardStatus::Draft,
                dependencies: vec![],
            },
            DeltaShard {
                id: "method-1".to_string(),
                name: "Methods".to_string(),
                family: ShardFamily::Method,
                content: "Methods used in this study. ".repeat(100),
                status: ShardStatus::Draft,
                dependencies: vec!["intro-1".to_string()],
            },
        ];

        let plan = schedule_chapters(shards, 500).expect("Should schedule chapters");
        assert!(!plan.chapters.is_empty());
    }
}
