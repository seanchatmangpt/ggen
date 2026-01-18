//! File processing engine with async I/O and progress tracking

use anyhow::{Context, Result};
use dashmap::DashMap;
use futures::stream::{self, StreamExt};
use indicatif::{ProgressBar, ProgressStyle};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::fs;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tracing::{debug, info, warn};
use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub struct ProcessingOptions {
    pub workers: usize,
    pub compress: bool,
    pub buffer_size: usize,
}

impl Default for ProcessingOptions {
    fn default() -> Self {
        Self {
            workers: num_cpus::get(),
            compress: false,
            buffer_size: 8192,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessingStats {
    pub files_processed: usize,
    pub bytes_processed: u64,
    pub elapsed_secs: f64,
    pub errors: usize,
}

impl ProcessingStats {
    pub fn throughput(&self) -> f64 {
        if self.elapsed_secs > 0.0 {
            self.files_processed as f64 / self.elapsed_secs
        } else {
            0.0
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisResult {
    pub total_files: usize,
    pub total_size: u64,
    pub file_types: HashMap<String, usize>,
    pub largest_file: Option<PathBuf>,
    pub largest_size: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub iterations: usize,
    pub total_secs: f64,
    pub avg_secs: f64,
    pub throughput_mbps: f64,
}

pub struct FileProcessor {
    options: ProcessingOptions,
}

impl FileProcessor {
    pub fn new(options: ProcessingOptions) -> Self {
        Self { options }
    }

    /// Process all files in a directory
    pub async fn process_directory(
        &self,
        input: &str,
        output: &str,
    ) -> Result<ProcessingStats> {
        let start = Instant::now();
        let input_path = Path::new(input);
        let output_path = Path::new(output);

        // Create output directory
        fs::create_dir_all(output_path)
            .await
            .context("Failed to create output directory")?;

        // Collect all files
        let files: Vec<PathBuf> = WalkDir::new(input_path)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
            .map(|e| e.path().to_path_buf())
            .collect();

        let total_files = files.len();
        info!("Found {} files to process", total_files);

        // Setup progress bar
        let pb = ProgressBar::new(total_files as u64);
        pb.set_style(
            ProgressStyle::default_bar()
                .template(
                    "{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} ({eta})",
                )?
                .progress_chars("#>-"),
        );

        // Process files concurrently
        let stats = Arc::new(DashMap::new());
        let errors = Arc::new(DashMap::new());

        stream::iter(files)
            .map(|file_path| {
                let output_path = output_path.to_path_buf();
                let input_path = input_path.to_path_buf();
                let stats = Arc::clone(&stats);
                let errors = Arc::clone(&errors);
                let pb = pb.clone();
                let options = self.options.clone();

                async move {
                    match process_file(&file_path, &input_path, &output_path, &options).await {
                        Ok(bytes) => {
                            stats.insert(file_path.clone(), bytes);
                        }
                        Err(e) => {
                            warn!("Error processing {:?}: {}", file_path, e);
                            errors.insert(file_path.clone(), e.to_string());
                        }
                    }
                    pb.inc(1);
                }
            })
            .buffer_unordered(self.options.workers)
            .collect::<Vec<_>>()
            .await;

        pb.finish_with_message("Complete");

        let elapsed = start.elapsed();
        let bytes_processed: u64 = stats.iter().map(|entry| *entry.value()).sum();

        Ok(ProcessingStats {
            files_processed: stats.len(),
            bytes_processed,
            elapsed_secs: elapsed.as_secs_f64(),
            errors: errors.len(),
        })
    }

    /// Analyze a path and generate statistics
    pub async fn analyze_path(&self, path: &str) -> Result<AnalysisResult> {
        let path = Path::new(path);
        let mut total_files = 0;
        let mut total_size = 0u64;
        let mut file_types: HashMap<String, usize> = HashMap::new();
        let mut largest_file: Option<PathBuf> = None;
        let mut largest_size = 0u64;

        for entry in WalkDir::new(path).into_iter().filter_map(|e| e.ok()) {
            if entry.file_type().is_file() {
                total_files += 1;

                let metadata = entry.metadata()?;
                let size = metadata.len();
                total_size += size;

                // Track file types
                if let Some(ext) = entry.path().extension() {
                    let ext_str = ext.to_string_lossy().to_string();
                    *file_types.entry(ext_str).or_insert(0) += 1;
                }

                // Track largest file
                if size > largest_size {
                    largest_size = size;
                    largest_file = Some(entry.path().to_path_buf());
                }
            }
        }

        Ok(AnalysisResult {
            total_files,
            total_size,
            file_types,
            largest_file,
            largest_size,
        })
    }

    /// Convert a file to a different format
    pub async fn convert_file(
        &self,
        input: &str,
        output: &str,
        format: &str,
    ) -> Result<()> {
        let mut input_file = fs::File::open(input)
            .await
            .context("Failed to open input file")?;

        let mut output_file = fs::File::create(output)
            .await
            .context("Failed to create output file")?;

        let mut buffer = vec![0u8; self.options.buffer_size];

        match format {
            "hex" => {
                // Convert to hexadecimal representation
                loop {
                    let n = input_file.read(&mut buffer).await?;
                    if n == 0 {
                        break;
                    }
                    let hex = hex::encode(&buffer[..n]);
                    output_file.write_all(hex.as_bytes()).await?;
                }
            }
            "hash" => {
                // Generate SHA-256 hash
                let mut hasher = Sha256::new();
                loop {
                    let n = input_file.read(&mut buffer).await?;
                    if n == 0 {
                        break;
                    }
                    hasher.update(&buffer[..n]);
                }
                let hash = format!("{:x}", hasher.finalize());
                output_file.write_all(hash.as_bytes()).await?;
            }
            _ => {
                anyhow::bail!("Unsupported format: {}", format);
            }
        }

        output_file.flush().await?;
        Ok(())
    }

    /// Run performance benchmark
    pub async fn benchmark(&self, iterations: usize, size_mb: usize) -> Result<BenchmarkResult> {
        let start = Instant::now();
        let data = vec![0u8; size_mb * 1024 * 1024];

        for _ in 0..iterations {
            let mut hasher = Sha256::new();
            hasher.update(&data);
            let _hash = hasher.finalize();
        }

        let elapsed = start.elapsed();
        let total_secs = elapsed.as_secs_f64();
        let avg_secs = total_secs / iterations as f64;
        let total_mb = (size_mb * iterations) as f64;
        let throughput_mbps = total_mb / total_secs;

        Ok(BenchmarkResult {
            iterations,
            total_secs,
            avg_secs,
            throughput_mbps,
        })
    }
}

impl Default for FileProcessor {
    fn default() -> Self {
        Self::new(ProcessingOptions::default())
    }
}

/// Process a single file
async fn process_file(
    file_path: &Path,
    input_base: &Path,
    output_base: &Path,
    options: &ProcessingOptions,
) -> Result<u64> {
    debug!("Processing: {:?}", file_path);

    // Calculate relative path and output path
    let rel_path = file_path
        .strip_prefix(input_base)
        .context("Failed to strip prefix")?;
    let output_path = output_base.join(rel_path);

    // Create parent directory
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent).await?;
    }

    // Copy file with buffering
    let mut input = fs::File::open(file_path).await?;
    let mut output = fs::File::create(&output_path).await?;

    let mut buffer = vec![0u8; options.buffer_size];
    let mut total_bytes = 0u64;

    loop {
        let n = input.read(&mut buffer).await?;
        if n == 0 {
            break;
        }
        output.write_all(&buffer[..n]).await?;
        total_bytes += n as u64;
    }

    output.flush().await?;
    Ok(total_bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_analyze_empty_dir() {
        let temp_dir = TempDir::new().unwrap();
        let processor = FileProcessor::default();

        let result = processor
            .analyze_path(temp_dir.path().to_str().unwrap())
            .await
            .unwrap();

        assert_eq!(result.total_files, 0);
        assert_eq!(result.total_size, 0);
    }

    #[tokio::test]
    async fn test_benchmark() {
        let processor = FileProcessor::default();
        let result = processor.benchmark(10, 1).await.unwrap();

        assert_eq!(result.iterations, 10);
        assert!(result.total_secs > 0.0);
        assert!(result.throughput_mbps > 0.0);
    }
}
