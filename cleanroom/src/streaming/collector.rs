//! Streaming collectors for artifacts
//!
//! This module provides specialized collectors for different types of artifacts,
//! including filtering, aggregation, and transformation capabilities.

use super::{ArtifactData, ArtifactMetadata, ArtifactType, StreamingCollector};
use crate::error::Result;
use std::collections::HashMap;
use std::time::Instant;

/// Filtering collector that only collects artifacts matching criteria
pub struct FilteringCollector {
    base_collector: StreamingCollector,
    filters: Vec<Box<dyn Fn(&ArtifactData<'_>) -> bool + Send + Sync>>,
}

impl FilteringCollector {
    /// Create a new filtering collector
    pub fn new(buffer_size: usize) -> Self {
        Self {
            base_collector: StreamingCollector::new(buffer_size),
            filters: Vec::new(),
        }
    }

    /// Add a filter function
    pub fn add_filter<F>(mut self, filter: F) -> Self
    where
        F: Fn(&ArtifactData<'_>) -> bool + Send + Sync + 'static,
    {
        self.filters.push(Box::new(filter));
        self
    }

    /// Filter by artifact type
    pub fn filter_by_type(mut self, artifact_type: ArtifactType) -> Self {
        self.filters.push(Box::new(move |artifact| {
            artifact.metadata.artifact_type == artifact_type
        }));
        self
    }

    /// Filter by source
    pub fn filter_by_source(mut self, source: &str) -> Self {
        let source = source.to_string();
        self.filters.push(Box::new(move |artifact| {
            artifact.metadata.source == source
        }));
        self
    }

    /// Filter by minimum size
    pub fn filter_by_min_size(mut self, min_size: usize) -> Self {
        self.filters.push(Box::new(move |artifact| {
            artifact.metadata.size_bytes >= min_size
        }));
        self
    }

    /// Filter by maximum size
    pub fn filter_by_max_size(mut self, max_size: usize) -> Self {
        self.filters.push(Box::new(move |artifact| {
            artifact.metadata.size_bytes <= max_size
        }));
        self
    }

    /// Filter by time range
    pub fn filter_by_time_range(mut self, start: Instant, end: Instant) -> Self {
        self.filters.push(Box::new(move |artifact| {
            artifact.timestamp >= start && artifact.timestamp <= end
        }));
        self
    }

    /// Add an artifact if it passes all filters
    pub fn add_artifact(&mut self, artifact: ArtifactData<'static>) {
        if self.filters.iter().all(|filter| filter(&artifact)) {
            self.base_collector.add_artifact(artifact);
        }
    }

    /// Get the base collector
    pub fn into_collector(self) -> StreamingCollector {
        self.base_collector
    }

    /// Get collected artifacts
    pub fn artifacts(&self) -> &[ArtifactData<'static>] {
        self.base_collector.artifacts()
    }

    /// Get artifact count
    pub fn count(&self) -> usize {
        self.base_collector.count()
    }

    /// Get total size
    pub fn total_size(&self) -> usize {
        self.base_collector.total_size()
    }

    /// Check if collector is empty
    pub fn is_empty(&self) -> bool {
        self.base_collector.is_empty()
    }
}

/// Aggregating collector that groups artifacts by criteria
pub struct AggregatingCollector {
    base_collector: StreamingCollector,
    aggregations: HashMap<String, Vec<ArtifactData<'static>>>,
    aggregation_key: Box<dyn Fn(&ArtifactData<'_>) -> String + Send + Sync>,
}

impl AggregatingCollector {
    /// Create a new aggregating collector
    pub fn new(buffer_size: usize) -> Self {
        Self {
            base_collector: StreamingCollector::new(buffer_size),
            aggregations: HashMap::new(),
            aggregation_key: Box::new(|artifact| artifact.metadata.source.clone()),
        }
    }

    /// Set aggregation key function
    pub fn with_aggregation_key<F>(mut self, key_fn: F) -> Self
    where
        F: Fn(&ArtifactData<'_>) -> String + Send + Sync + 'static,
    {
        self.aggregation_key = Box::new(key_fn);
        self
    }

    /// Aggregate by artifact type
    pub fn aggregate_by_type(mut self) -> Self {
        self.aggregation_key = Box::new(|artifact| {
            match &artifact.metadata.artifact_type {
                ArtifactType::Text => "text".to_string(),
                ArtifactType::Binary => "binary".to_string(),
                ArtifactType::Json => "json".to_string(),
                ArtifactType::Xml => "xml".to_string(),
                ArtifactType::Log => "log".to_string(),
                ArtifactType::Snapshot => "snapshot".to_string(),
                ArtifactType::Coverage => "coverage".to_string(),
                ArtifactType::Trace => "trace".to_string(),
                ArtifactType::Custom(name) => name.clone(),
            }
        });
        self
    }

    /// Aggregate by source
    pub fn aggregate_by_source(mut self) -> Self {
        self.aggregation_key = Box::new(|artifact| artifact.metadata.source.clone());
        self
    }

    /// Add an artifact and aggregate it
    pub fn add_artifact(&mut self, artifact: ArtifactData<'static>) {
        let key = (self.aggregation_key)(&artifact);
        self.aggregations.entry(key).or_insert_with(Vec::new).push(artifact.clone());
        self.base_collector.add_artifact(artifact);
    }

    /// Get aggregations
    pub fn aggregations(&self) -> &HashMap<String, Vec<ArtifactData<'static>>> {
        &self.aggregations
    }

    /// Get artifacts for a specific key
    pub fn get_artifacts_for_key(&self, key: &str) -> Option<&Vec<ArtifactData<'static>>> {
        self.aggregations.get(key)
    }

    /// Get aggregation keys
    pub fn aggregation_keys(&self) -> Vec<String> {
        self.aggregations.keys().cloned().collect()
    }

    /// Get the base collector
    pub fn into_collector(self) -> StreamingCollector {
        self.base_collector
    }

    /// Get collected artifacts
    pub fn artifacts(&self) -> &[ArtifactData<'static>] {
        self.base_collector.artifacts()
    }

    /// Get artifact count
    pub fn count(&self) -> usize {
        self.base_collector.count()
    }

    /// Get total size
    pub fn total_size(&self) -> usize {
        self.base_collector.total_size()
    }

    /// Check if collector is empty
    pub fn is_empty(&self) -> bool {
        self.base_collector.is_empty()
    }
}

/// Transforming collector that applies transformations to artifacts
pub struct TransformingCollector {
    base_collector: StreamingCollector,
    transformers: Vec<Box<dyn Fn(ArtifactData<'static>) -> ArtifactData<'static> + Send + Sync>>,
}

impl TransformingCollector {
    /// Create a new transforming collector
    pub fn new(buffer_size: usize) -> Self {
        Self {
            base_collector: StreamingCollector::new(buffer_size),
            transformers: Vec::new(),
        }
    }

    /// Add a transformer function
    pub fn add_transformer<F>(mut self, transformer: F) -> Self
    where
        F: Fn(ArtifactData<'static>) -> ArtifactData<'static> + Send + Sync + 'static,
    {
        self.transformers.push(Box::new(transformer));
        self
    }

    /// Transform artifact content
    pub fn transform_content<F>(mut self, transform_fn: F) -> Self
    where
        F: Fn(&str) -> String + Send + Sync + 'static,
    {
        self.transformers.push(Box::new(move |mut artifact| {
            let new_content = transform_fn(&artifact.content);
            artifact.content = std::borrow::Cow::Owned(new_content);
            artifact
        }));
        self
    }

    /// Add metadata property
    pub fn add_metadata_property(mut self, key: String, value: String) -> Self {
        self.transformers.push(Box::new(move |mut artifact| {
            artifact.metadata.properties.insert(key.clone(), value.clone());
            artifact
        }));
        self
    }

    /// Set artifact type
    pub fn set_artifact_type(mut self, artifact_type: ArtifactType) -> Self {
        self.transformers.push(Box::new(move |mut artifact| {
            artifact.metadata.artifact_type = artifact_type;
            artifact
        }));
        self
    }

    /// Add an artifact and apply transformations
    pub fn add_artifact(&mut self, mut artifact: ArtifactData<'static>) {
        for transformer in &self.transformers {
            artifact = transformer(artifact);
        }
        self.base_collector.add_artifact(artifact);
    }

    /// Get the base collector
    pub fn into_collector(self) -> StreamingCollector {
        self.base_collector
    }

    /// Get collected artifacts
    pub fn artifacts(&self) -> &[ArtifactData<'static>] {
        self.base_collector.artifacts()
    }

    /// Get artifact count
    pub fn count(&self) -> usize {
        self.base_collector.count()
    }

    /// Get total size
    pub fn total_size(&self) -> usize {
        self.base_collector.total_size()
    }

    /// Check if collector is empty
    pub fn is_empty(&self) -> bool {
        self.base_collector.is_empty()
    }
}

/// Batching collector that collects artifacts in batches
pub struct BatchingCollector {
    base_collector: StreamingCollector,
    batch_size: usize,
    current_batch: Vec<ArtifactData<'static>>,
    batch_processor: Option<Box<dyn Fn(Vec<ArtifactData<'static>>) -> Result<()> + Send + Sync>>,
}

impl BatchingCollector {
    /// Create a new batching collector
    pub fn new(buffer_size: usize, batch_size: usize) -> Self {
        Self {
            base_collector: StreamingCollector::new(buffer_size),
            batch_size,
            current_batch: Vec::new(),
            batch_processor: None,
        }
    }

    /// Set batch processor function
    pub fn with_batch_processor<F>(mut self, processor: F) -> Self
    where
        F: Fn(Vec<ArtifactData<'static>>) -> Result<()> + Send + Sync + 'static,
    {
        self.batch_processor = Some(Box::new(processor));
        self
    }

    /// Add an artifact to the current batch
    pub fn add_artifact(&mut self, artifact: ArtifactData<'static>) -> Result<()> {
        self.current_batch.push(artifact);
        
        if self.current_batch.len() >= self.batch_size {
            self.process_batch()?;
        }
        
        Ok(())
    }

    /// Process the current batch
    pub fn process_batch(&mut self) -> Result<()> {
        if !self.current_batch.is_empty() {
            if let Some(processor) = &self.batch_processor {
                processor(self.current_batch.clone())?;
            }
            
            // Move artifacts to base collector
            for artifact in self.current_batch.drain(..) {
                self.base_collector.add_artifact(artifact);
            }
        }
        
        Ok(())
    }

    /// Flush remaining artifacts
    pub fn flush(&mut self) -> Result<()> {
        self.process_batch()
    }

    /// Get the base collector
    pub fn into_collector(self) -> StreamingCollector {
        self.base_collector
    }

    /// Get collected artifacts
    pub fn artifacts(&self) -> &[ArtifactData<'static>] {
        self.base_collector.artifacts()
    }

    /// Get artifact count
    pub fn count(&self) -> usize {
        self.base_collector.count()
    }

    /// Get total size
    pub fn total_size(&self) -> usize {
        self.base_collector.total_size()
    }

    /// Check if collector is empty
    pub fn is_empty(&self) -> bool {
        self.base_collector.is_empty()
    }

    /// Get current batch size
    pub fn current_batch_size(&self) -> usize {
        self.current_batch.len()
    }

    /// Get batch size
    pub fn batch_size(&self) -> usize {
        self.batch_size
    }
}

/// Convenience function to create a filtering collector
pub fn filtering_collector(buffer_size: usize) -> FilteringCollector {
    FilteringCollector::new(buffer_size)
}

/// Convenience function to create an aggregating collector
pub fn aggregating_collector(buffer_size: usize) -> AggregatingCollector {
    AggregatingCollector::new(buffer_size)
}

/// Convenience function to create a transforming collector
pub fn transforming_collector(buffer_size: usize) -> TransformingCollector {
    TransformingCollector::new(buffer_size)
}

/// Convenience function to create a batching collector
pub fn batching_collector(buffer_size: usize, batch_size: usize) -> BatchingCollector {
    BatchingCollector::new(buffer_size, batch_size)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_filtering_collector() {
        let mut collector = FilteringCollector::new(100)
            .filter_by_type(ArtifactType::Text)
            .filter_by_min_size(5);
        
        let artifact1 = ArtifactData {
            content: std::borrow::Cow::Borrowed("hello"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "test".to_string(),
                size_bytes: 5,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        let artifact2 = ArtifactData {
            content: std::borrow::Cow::Borrowed("hi"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "test".to_string(),
                size_bytes: 2,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        collector.add_artifact(artifact1);
        collector.add_artifact(artifact2);
        
        assert_eq!(collector.count(), 1);
        assert_eq!(collector.artifacts()[0].content, "hello");
    }

    #[test]
    fn test_aggregating_collector() {
        let mut collector = AggregatingCollector::new(100)
            .aggregate_by_source();
        
        let artifact1 = ArtifactData {
            content: std::borrow::Cow::Borrowed("test1"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "source1".to_string(),
                size_bytes: 5,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        let artifact2 = ArtifactData {
            content: std::borrow::Cow::Borrowed("test2"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "source2".to_string(),
                size_bytes: 5,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        collector.add_artifact(artifact1);
        collector.add_artifact(artifact2);
        
        assert_eq!(collector.count(), 2);
        assert_eq!(collector.aggregation_keys().len(), 2);
        assert!(collector.get_artifacts_for_key("source1").is_some());
        assert!(collector.get_artifacts_for_key("source2").is_some());
    }

    #[test]
    fn test_transforming_collector() {
        let mut collector = TransformingCollector::new(100)
            .transform_content(|content| content.to_uppercase())
            .add_metadata_property("transformed".to_string(), "true".to_string());
        
        let artifact = ArtifactData {
            content: std::borrow::Cow::Borrowed("hello"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "test".to_string(),
                size_bytes: 5,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        collector.add_artifact(artifact);
        
        assert_eq!(collector.count(), 1);
        assert_eq!(collector.artifacts()[0].content, "HELLO");
        assert_eq!(collector.artifacts()[0].metadata.properties.get("transformed"), Some(&"true".to_string()));
    }

    #[test]
    fn test_batching_collector() {
        let mut collector = BatchingCollector::new(100, 2)
            .with_batch_processor(|batch| {
                assert_eq!(batch.len(), 2);
                Ok(())
            });
        
        let artifact1 = ArtifactData {
            content: std::borrow::Cow::Borrowed("test1"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "test".to_string(),
                size_bytes: 5,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        let artifact2 = ArtifactData {
            content: std::borrow::Cow::Borrowed("test2"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "test".to_string(),
                size_bytes: 5,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        collector.add_artifact(artifact1).unwrap();
        assert_eq!(collector.current_batch_size(), 1);
        
        collector.add_artifact(artifact2).unwrap();
        assert_eq!(collector.current_batch_size(), 0);
        assert_eq!(collector.count(), 2);
    }

    #[test]
    fn test_convenience_functions() {
        let _filtering = filtering_collector(100);
        let _aggregating = aggregating_collector(100);
        let _transforming = transforming_collector(100);
        let _batching = batching_collector(100, 10);
        
        // Just verify they compile and create valid instances
        assert!(_filtering.is_empty());
        assert!(_aggregating.is_empty());
        assert!(_transforming.is_empty());
        assert!(_batching.is_empty());
    }
}
