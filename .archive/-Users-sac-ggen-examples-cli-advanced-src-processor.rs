//! Core processing logic

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::time::Instant;

#[derive(Debug, Serialize, Deserialize)]
pub struct Statistics {
    pub length: usize,
    pub word_count: usize,
    pub unique_chars: usize,
}

pub async fn process_file(input: &str, output: Option<&str>) -> Result<()> {
    tracing::info!(input = %input, output = ?output, "Processing file");

    let content = tokio::fs::read_to_string(input).await?;
    let processed = content.to_uppercase();

    if let Some(out) = output {
        tokio::fs::write(out, processed).await?;
        tracing::info!(output = %out, "Written to file");
    } else {
        println!("{}", processed);
    }

    Ok(())
}

pub fn analyze(input: &str) -> Result<Statistics> {
    use std::collections::HashSet;

    let words: Vec<&str> = input.split_whitespace().collect();
    let unique_chars: HashSet<char> = input.chars().collect();

    Ok(Statistics {
        length: input.len(),
        word_count: words.len(),
        unique_chars: unique_chars.len(),
    })
}

pub async fn benchmark(iterations: usize) -> Result<()> {
    tracing::info!(iterations, "Running benchmark");

    let start = Instant::now();

    for i in 0..iterations {
        let input = format!("test string {}", i);
        let _ = analyze(&input)?;
    }

    let duration = start.elapsed();
    let per_iter = duration.as_micros() / iterations as u128;

    println!("Benchmark Results:");
    println!("  Total time: {:?}", duration);
    println!("  Iterations: {}", iterations);
    println!("  Per iteration: {}µs", per_iter);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_analyze() {
        let stats = analyze("hello world hello").unwrap();
        assert_eq!(stats.length, 17);
        assert_eq!(stats.word_count, 3);
    }

    #[tokio::test]
    async fn test_process_file() {
        let temp = tempfile::NamedTempFile::new().unwrap();
        let input_path = temp.path().to_str().unwrap();

        tokio::fs::write(input_path, "hello").await.unwrap();

        let output = tempfile::NamedTempFile::new().unwrap();
        let output_path = output.path().to_str().unwrap();

        process_file(input_path, Some(output_path)).await.unwrap();

        let result = tokio::fs::read_to_string(output_path).await.unwrap();
        assert_eq!(result, "HELLO");
    }
}
