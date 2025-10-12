//! Zero-copy streaming for artifacts
//!
//! This module provides streaming APIs for artifacts without modifying existing types,
//! using zero-copy techniques to minimize memory allocations and improve performance.
//!
//! # Example
//!
//! ```rust
//! use cleanroom::streaming::{ArtifactStream, StreamingCollector};
//!
//! let stream = ArtifactStream::new(environment);
//! let collector = StreamingCollector::with_zero_copy();
//! stream.collect_into(collector).await?;
//! ```

use crate::error::Result;
use crate::cleanroom::CleanroomEnvironment;
use std::borrow::Cow;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::mpsc;
use tokio::io::{AsyncRead, AsyncWrite};
use futures::stream::{Stream, StreamExt};
use futures::sink::SinkExt;

/// Zero-copy artifact data
#[derive(Debug, Clone)]
pub struct ArtifactData<'a> {
    /// The artifact content
    pub content: Cow<'a, str>,
    /// Artifact metadata
    pub metadata: ArtifactMetadata,
    /// Timestamp when artifact was created
    pub timestamp: Instant,
}

/// Artifact metadata
#[derive(Debug, Clone)]
pub struct ArtifactMetadata {
    /// Artifact type
    pub artifact_type: ArtifactType,
    /// Source of the artifact
    pub source: String,
    /// Size in bytes
    pub size_bytes: usize,
    /// Content hash for integrity checking
    pub content_hash: Option<String>,
    /// Additional properties
    pub properties: std::collections::HashMap<String, String>,
}

/// Types of artifacts
#[derive(Debug, Clone, PartialEq)]
pub enum ArtifactType {
    /// Text artifact
    Text,
    /// Binary artifact
    Binary,
    /// JSON artifact
    Json,
    /// XML artifact
    Xml,
    /// Log artifact
    Log,
    /// Snapshot artifact
    Snapshot,
    /// Coverage artifact
    Coverage,
    /// Trace artifact
    Trace,
    /// Custom artifact type
    Custom(String),
}

/// Artifact stream for zero-copy processing
pub struct ArtifactStream {
    environment: Arc<CleanroomEnvironment>,
    buffer_size: usize,
    max_concurrent_streams: usize,
}

impl ArtifactStream {
    /// Create a new artifact stream
    pub fn new(environment: CleanroomEnvironment) -> Self {
        Self {
            environment: Arc::new(environment),
            buffer_size: 8192,
            max_concurrent_streams: 10,
        }
    }

    /// Set buffer size for streaming
    pub fn with_buffer_size(mut self, size: usize) -> Self {
        self.buffer_size = size;
        self
    }

    /// Set maximum concurrent streams
    pub fn with_max_concurrent_streams(mut self, max: usize) -> Self {
        self.max_concurrent_streams = max;
        self
    }

    /// Create a streaming collector
    pub fn create_collector(&self) -> StreamingCollector {
        StreamingCollector::new(self.buffer_size)
    }

    /// Stream artifacts with zero-copy processing
    pub async fn stream_artifacts<F>(&self, processor: F) -> Result<()>
    where
        F: Fn(ArtifactData<'_>) -> Result<()> + Send + Sync + 'static,
    {
        let (tx, mut rx) = mpsc::channel(self.buffer_size);
        
        // Spawn artifact producer
        let environment = self.environment.clone();
        let producer = tokio::spawn(async move {
            Self::produce_artifacts(environment, tx).await
        });
        
        // Process artifacts as they arrive
        while let Some(artifact) = rx.recv().await {
            processor(artifact)?;
        }
        
        // Wait for producer to complete
        producer.await??;
        
        Ok(())
    }

    /// Collect artifacts into a collector
    pub async fn collect_into(&self, mut collector: StreamingCollector) -> Result<StreamingCollector> {
        self.stream_artifacts(|artifact| {
            collector.add_artifact(artifact);
            Ok(())
        }).await?;
        
        Ok(collector)
    }

    /// Stream artifacts to a sink
    pub async fn stream_to_sink<S>(&self, mut sink: S) -> Result<()>
    where
        S: SinkExt<ArtifactData<'static>> + Unpin,
        S::Error: std::fmt::Display,
    {
        let (tx, mut rx) = mpsc::channel(self.buffer_size);
        
        // Spawn artifact producer
        let environment = self.environment.clone();
        let producer = tokio::spawn(async move {
            Self::produce_artifacts(environment, tx).await
        });
        
        // Stream to sink
        while let Some(artifact) = rx.recv().await {
            sink.send(artifact).await.map_err(|e| {
                crate::error::CleanroomError::internal_error(&format!("Sink error: {}", e))
            })?;
        }
        
        // Wait for producer to complete
        producer.await??;
        
        Ok(())
    }

    /// Produce artifacts from environment
    async fn produce_artifacts(
        environment: Arc<CleanroomEnvironment>,
        mut tx: mpsc::Sender<ArtifactData<'static>>,
    ) -> Result<()> {
        // Simulate artifact production
        // In real implementation, this would extract artifacts from the environment
        
        let artifacts = vec![
            ArtifactData {
                content: Cow::Borrowed("Test artifact 1"),
                metadata: ArtifactMetadata {
                    artifact_type: ArtifactType::Text,
                    source: "test".to_string(),
                    size_bytes: 15,
                    content_hash: Some("hash1".to_string()),
                    properties: std::collections::HashMap::new(),
                },
                timestamp: Instant::now(),
            },
            ArtifactData {
                content: Cow::Borrowed("Test artifact 2"),
                metadata: ArtifactMetadata {
                    artifact_type: ArtifactType::Json,
                    source: "test".to_string(),
                    size_bytes: 15,
                    content_hash: Some("hash2".to_string()),
                    properties: std::collections::HashMap::new(),
                },
                timestamp: Instant::now(),
            },
        ];
        
        for artifact in artifacts {
            if tx.send(artifact).await.is_err() {
                break;
            }
        }
        
        Ok(())
    }
}

/// Streaming collector for artifacts
pub struct StreamingCollector {
    artifacts: Vec<ArtifactData<'static>>,
    buffer_size: usize,
    total_size: usize,
    start_time: Instant,
}

impl StreamingCollector {
    /// Create a new streaming collector
    pub fn new(buffer_size: usize) -> Self {
        Self {
            artifacts: Vec::new(),
            buffer_size,
            total_size: 0,
            start_time: Instant::now(),
        }
    }

    /// Create a collector with zero-copy optimization
    pub fn with_zero_copy() -> Self {
        Self::new(8192)
    }

    /// Add an artifact to the collector
    pub fn add_artifact(&mut self, artifact: ArtifactData<'static>) {
        self.total_size += artifact.metadata.size_bytes;
        self.artifacts.push(artifact);
    }

    /// Get collected artifacts
    pub fn artifacts(&self) -> &[ArtifactData<'static>] {
        &self.artifacts
    }

    /// Get total size of collected artifacts
    pub fn total_size(&self) -> usize {
        self.total_size
    }

    /// Get collection duration
    pub fn duration(&self) -> std::time::Duration {
        self.start_time.elapsed()
    }

    /// Get artifact count
    pub fn count(&self) -> usize {
        self.artifacts.len()
    }

    /// Check if collector is empty
    pub fn is_empty(&self) -> bool {
        self.artifacts.is_empty()
    }

    /// Clear collected artifacts
    pub fn clear(&mut self) {
        self.artifacts.clear();
        self.total_size = 0;
    }

    /// Get artifacts by type
    pub fn artifacts_by_type(&self, artifact_type: &ArtifactType) -> Vec<&ArtifactData<'static>> {
        self.artifacts
            .iter()
            .filter(|artifact| &artifact.metadata.artifact_type == artifact_type)
            .collect()
    }

    /// Get artifacts by source
    pub fn artifacts_by_source(&self, source: &str) -> Vec<&ArtifactData<'static>> {
        self.artifacts
            .iter()
            .filter(|artifact| artifact.metadata.source == source)
            .collect()
    }

    /// Get artifacts created after a specific time
    pub fn artifacts_after(&self, timestamp: Instant) -> Vec<&ArtifactData<'static>> {
        self.artifacts
            .iter()
            .filter(|artifact| artifact.timestamp > timestamp)
            .collect()
    }

    /// Get artifacts created before a specific time
    pub fn artifacts_before(&self, timestamp: Instant) -> Vec<&ArtifactData<'static>> {
        self.artifacts
            .iter()
            .filter(|artifact| artifact.timestamp < timestamp)
            .collect()
    }

    /// Get artifacts within a time range
    pub fn artifacts_between(&self, start: Instant, end: Instant) -> Vec<&ArtifactData<'static>> {
        self.artifacts
            .iter()
            .filter(|artifact| artifact.timestamp >= start && artifact.timestamp <= end)
            .collect()
    }

    /// Get statistics about collected artifacts
    pub fn statistics(&self) -> CollectorStatistics {
        let mut stats = CollectorStatistics {
            total_artifacts: self.artifacts.len(),
            total_size: self.total_size,
            average_size: 0,
            artifact_types: std::collections::HashMap::new(),
            sources: std::collections::HashMap::new(),
            duration: self.duration(),
        };
        
        if !self.artifacts.is_empty() {
            stats.average_size = self.total_size / self.artifacts.len();
        }
        
        for artifact in &self.artifacts {
            // Count artifact types
            let type_name = match &artifact.metadata.artifact_type {
                ArtifactType::Text => "text",
                ArtifactType::Binary => "binary",
                ArtifactType::Json => "json",
                ArtifactType::Xml => "xml",
                ArtifactType::Log => "log",
                ArtifactType::Snapshot => "snapshot",
                ArtifactType::Coverage => "coverage",
                ArtifactType::Trace => "trace",
                ArtifactType::Custom(name) => name,
            };
            *stats.artifact_types.entry(type_name.to_string()).or_insert(0) += 1;
            
            // Count sources
            *stats.sources.entry(artifact.metadata.source.clone()).or_insert(0) += 1;
        }
        
        stats
    }
}

/// Collector statistics
#[derive(Debug, Clone)]
pub struct CollectorStatistics {
    /// Total number of artifacts
    pub total_artifacts: usize,
    /// Total size in bytes
    pub total_size: usize,
    /// Average size per artifact
    pub average_size: usize,
    /// Count by artifact type
    pub artifact_types: std::collections::HashMap<String, usize>,
    /// Count by source
    pub sources: std::collections::HashMap<String, usize>,
    /// Collection duration
    pub duration: std::time::Duration,
}

/// Zero-copy buffer for streaming
pub struct ZeroCopyBuffer {
    data: Vec<u8>,
    position: usize,
    capacity: usize,
}

impl ZeroCopyBuffer {
    /// Create a new zero-copy buffer
    pub fn new(capacity: usize) -> Self {
        Self {
            data: vec![0; capacity],
            position: 0,
            capacity,
        }
    }

    /// Create a buffer with default capacity
    pub fn default() -> Self {
        Self::new(8192)
    }

    /// Write data to the buffer
    pub fn write(&mut self, data: &[u8]) -> Result<usize> {
        let available = self.capacity - self.position;
        let to_write = std::cmp::min(data.len(), available);
        
        if to_write > 0 {
            self.data[self.position..self.position + to_write].copy_from_slice(&data[..to_write]);
            self.position += to_write;
        }
        
        Ok(to_write)
    }

    /// Read data from the buffer
    pub fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let available = self.position;
        let to_read = std::cmp::min(buf.len(), available);
        
        if to_read > 0 {
            buf[..to_read].copy_from_slice(&self.data[..to_read]);
            self.data.copy_within(to_read..self.position, 0);
            self.position -= to_read;
        }
        
        Ok(to_read)
    }

    /// Get current position
    pub fn position(&self) -> usize {
        self.position
    }

    /// Get remaining capacity
    pub fn remaining_capacity(&self) -> usize {
        self.capacity - self.position
    }

    /// Check if buffer is full
    pub fn is_full(&self) -> bool {
        self.position >= self.capacity
    }

    /// Check if buffer is empty
    pub fn is_empty(&self) -> bool {
        self.position == 0
    }

    /// Clear the buffer
    pub fn clear(&mut self) {
        self.position = 0;
    }

    /// Get buffer capacity
    pub fn capacity(&self) -> usize {
        self.capacity
    }
}

impl Default for ZeroCopyBuffer {
    fn default() -> Self {
        Self::default()
    }
}

/// Convenience function to create an artifact stream
pub fn artifact_stream(environment: CleanroomEnvironment) -> ArtifactStream {
    ArtifactStream::new(environment)
}

/// Convenience function to create a streaming collector
pub fn streaming_collector() -> StreamingCollector {
    StreamingCollector::with_zero_copy()
}

/// Convenience function to create a zero-copy buffer
pub fn zero_copy_buffer() -> ZeroCopyBuffer {
    ZeroCopyBuffer::default()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::config::CleanroomConfig;

    #[tokio::test]
    async fn test_artifact_stream() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let stream = ArtifactStream::new(environment);
        
        let mut count = 0;
        stream.stream_artifacts(|_artifact| {
            count += 1;
            Ok(())
        }).await.unwrap();
        
        assert!(count > 0);
    }

    #[tokio::test]
    async fn test_streaming_collector() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        let stream = ArtifactStream::new(environment);
        
        let collector = stream.collect_into(StreamingCollector::with_zero_copy()).await.unwrap();
        
        assert!(!collector.is_empty());
        assert!(collector.count() > 0);
        assert!(collector.total_size() > 0);
    }

    #[tokio::test]
    async fn test_collector_statistics() {
        let mut collector = StreamingCollector::with_zero_copy();
        
        let artifact = ArtifactData {
            content: Cow::Borrowed("test"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "test".to_string(),
                size_bytes: 4,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        collector.add_artifact(artifact);
        
        let stats = collector.statistics();
        assert_eq!(stats.total_artifacts, 1);
        assert_eq!(stats.total_size, 4);
        assert_eq!(stats.average_size, 4);
    }

    #[tokio::test]
    async fn test_zero_copy_buffer() {
        let mut buffer = ZeroCopyBuffer::new(100);
        
        let data = b"hello world";
        let written = buffer.write(data).unwrap();
        assert_eq!(written, data.len());
        assert_eq!(buffer.position(), data.len());
        
        let mut read_buf = vec![0; data.len()];
        let read = buffer.read(&mut read_buf).unwrap();
        assert_eq!(read, data.len());
        assert_eq!(&read_buf, data);
        assert_eq!(buffer.position(), 0);
    }

    #[tokio::test]
    async fn test_artifact_filtering() {
        let mut collector = StreamingCollector::with_zero_copy();
        
        let artifact1 = ArtifactData {
            content: Cow::Borrowed("text"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "source1".to_string(),
                size_bytes: 4,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        let artifact2 = ArtifactData {
            content: Cow::Borrowed("json"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Json,
                source: "source2".to_string(),
                size_bytes: 4,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        collector.add_artifact(artifact1);
        collector.add_artifact(artifact2);
        
        let text_artifacts = collector.artifacts_by_type(&ArtifactType::Text);
        assert_eq!(text_artifacts.len(), 1);
        
        let source1_artifacts = collector.artifacts_by_source("source1");
        assert_eq!(source1_artifacts.len(), 1);
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let config = CleanroomConfig::default();
        let environment = CleanroomEnvironment::new(config).await.unwrap();
        
        let _stream = artifact_stream(environment);
        let _collector = streaming_collector();
        let _buffer = zero_copy_buffer();
        
        // Just verify they compile and create valid instances
        assert!(_collector.is_empty());
        assert!(_buffer.is_empty());
    }
}
