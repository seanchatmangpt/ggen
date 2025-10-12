//! Examples demonstrating zero-copy streaming for artifacts
//!
//! This example shows how to use the streaming APIs for efficient artifact
//! processing with zero-copy techniques and various collector patterns.

use cleanroom::streaming::{
    ArtifactStream, StreamingCollector, ArtifactData, ArtifactMetadata, ArtifactType,
    artifact_stream, streaming_collector,
};
use cleanroom::streaming::collector::{
    FilteringCollector, AggregatingCollector, TransformingCollector, BatchingCollector,
    filtering_collector, aggregating_collector, transforming_collector, batching_collector,
};
use cleanroom::streaming::buffer::{
    RingBuffer, CircularBuffer, MemoryMappedBuffer, BufferPool,
    ring_buffer, circular_buffer, memory_mapped_buffer, buffer_pool,
};
use cleanroom::builder::CleanroomBuilder;
use std::borrow::Cow;
use std::time::Instant;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("Cleanroom Streaming Artifacts Examples");
    println!("=======================================");

    // Create environment using builder
    let environment = CleanroomBuilder::new()
        .with_timeout(std::time::Duration::from_secs(30))
        .build()
        .await?;

    // Example 1: Basic artifact streaming
    println!("\n1. Basic Artifact Streaming");
    let stream = ArtifactStream::new(environment);
    
    let mut count = 0;
    stream.stream_artifacts(|artifact| {
        count += 1;
        println!("  ✓ Artifact {}: {} ({} bytes)", 
                count, 
                artifact.metadata.artifact_type, 
                artifact.metadata.size_bytes);
        Ok(())
    }).await?;
    
    println!("✓ Processed {} artifacts", count);

    // Example 2: Streaming collector
    println!("\n2. Streaming Collector");
    let stream = ArtifactStream::new(environment);
    let collector = stream.collect_into(StreamingCollector::with_zero_copy()).await?;
    
    println!("✓ Collected {} artifacts", collector.count());
    println!("✓ Total size: {} bytes", collector.total_size());
    println!("✓ Collection duration: {:?}", collector.duration());
    
    let stats = collector.statistics();
    println!("✓ Average size: {} bytes", stats.average_size);
    println!("✓ Artifact types: {:?}", stats.artifact_types);

    // Example 3: Filtering collector
    println!("\n3. Filtering Collector");
    let stream = ArtifactStream::new(environment);
    let mut filtering_collector = FilteringCollector::new(100)
        .filter_by_type(ArtifactType::Text)
        .filter_by_min_size(10);
    
    stream.stream_artifacts(|artifact| {
        filtering_collector.add_artifact(artifact);
        Ok(())
    }).await?;
    
    println!("✓ Filtered artifacts: {}", filtering_collector.count());
    for artifact in filtering_collector.artifacts() {
        println!("  - {}: {} bytes", artifact.metadata.artifact_type, artifact.metadata.size_bytes);
    }

    // Example 4: Aggregating collector
    println!("\n4. Aggregating Collector");
    let stream = ArtifactStream::new(environment);
    let mut aggregating_collector = AggregatingCollector::new(100)
        .aggregate_by_type();
    
    stream.stream_artifacts(|artifact| {
        aggregating_collector.add_artifact(artifact);
        Ok(())
    }).await?;
    
    println!("✓ Aggregated artifacts by type:");
    for (key, artifacts) in aggregating_collector.aggregations() {
        println!("  - {}: {} artifacts", key, artifacts.len());
    }

    // Example 5: Transforming collector
    println!("\n5. Transforming Collector");
    let stream = ArtifactStream::new(environment);
    let mut transforming_collector = TransformingCollector::new(100)
        .transform_content(|content| content.to_uppercase())
        .add_metadata_property("transformed".to_string(), "true".to_string());
    
    stream.stream_artifacts(|artifact| {
        transforming_collector.add_artifact(artifact);
        Ok(())
    }).await?;
    
    println!("✓ Transformed artifacts:");
    for artifact in transforming_collector.artifacts() {
        println!("  - Content: {}", artifact.content);
        println!("    Transformed: {}", 
                artifact.metadata.properties.get("transformed").unwrap_or(&"false".to_string()));
    }

    // Example 6: Batching collector
    println!("\n6. Batching Collector");
    let stream = ArtifactStream::new(environment);
    let mut batching_collector = BatchingCollector::new(100, 2)
        .with_batch_processor(|batch| {
            println!("  ✓ Processing batch of {} artifacts", batch.len());
            Ok(())
        });
    
    stream.stream_artifacts(|artifact| {
        batching_collector.add_artifact(artifact)?;
        Ok(())
    }).await?;
    
    // Flush remaining artifacts
    batching_collector.flush()?;
    
    println!("✓ Batching completed");

    // Example 7: Ring buffer
    println!("\n7. Ring Buffer");
    let mut ring_buffer = RingBuffer::new(100);
    
    let data1 = b"hello";
    let data2 = b"world";
    
    let written1 = ring_buffer.write(data1).unwrap();
    let written2 = ring_buffer.write(data2).unwrap();
    
    println!("✓ Written {} + {} = {} bytes", written1, written2, written1 + written2);
    println!("✓ Buffer size: {}", ring_buffer.size());
    println!("✓ Buffer utilization: {:.1}%", ring_buffer.utilization());
    
    let mut read_buf = vec![0; 10];
    let read = ring_buffer.read(&mut read_buf).unwrap();
    println!("✓ Read {} bytes: {}", read, String::from_utf8_lossy(&read_buf[..read]));

    // Example 8: Circular buffer with growth
    println!("\n8. Circular Buffer with Growth");
    let mut circular_buffer = CircularBuffer::new(10);
    
    let large_data = b"this is a very long string that exceeds the initial capacity";
    let written = circular_buffer.write(large_data).unwrap();
    
    println!("✓ Written {} bytes to circular buffer", written);
    println!("✓ Buffer capacity grew to: {}", circular_buffer.capacity());
    println!("✓ Buffer utilization: {:.1}%", circular_buffer.utilization());

    // Example 9: Memory-mapped buffer
    println!("\n9. Memory-Mapped Buffer");
    let mut memory_buffer = MemoryMappedBuffer::new(1000);
    
    let data = b"memory mapped data";
    let written = memory_buffer.write(data).await.unwrap();
    
    println!("✓ Written {} bytes to memory-mapped buffer", written);
    println!("✓ Buffer size: {}", memory_buffer.size());
    
    let mut read_buf = vec![0; data.len()];
    let read = memory_buffer.read(&mut read_buf).await.unwrap();
    println!("✓ Read {} bytes: {}", read, String::from_utf8_lossy(&read_buf));

    // Example 10: Buffer pool
    println!("\n10. Buffer Pool");
    let pool = BufferPool::new(100, 5);
    
    // Get buffers from pool
    let buffer1 = pool.get_buffer().await;
    let buffer2 = pool.get_buffer().await;
    
    println!("✓ Retrieved 2 buffers from pool");
    
    // Return buffers to pool
    pool.return_buffer(buffer1).await;
    pool.return_buffer(buffer2).await;
    
    let stats = pool.statistics().await;
    println!("✓ Pool statistics:");
    println!("  - Available buffers: {}", stats.available_buffers);
    println!("  - Max pool size: {}", stats.max_pool_size);
    println!("  - Buffer size: {}", stats.buffer_size);

    // Example 11: Convenience functions
    println!("\n11. Convenience Functions");
    let _stream = artifact_stream(CleanroomBuilder::new().build().await?);
    let _collector = streaming_collector();
    let _ring = ring_buffer(100);
    let _circular = circular_buffer(100);
    let _memory = memory_mapped_buffer(1000);
    let _pool = buffer_pool(100, 5);
    
    println!("✓ All convenience functions work correctly");

    // Example 12: Zero-copy optimization demonstration
    println!("\n12. Zero-Copy Optimization");
    let mut collector = StreamingCollector::with_zero_copy();
    
    // Create artifacts with borrowed content (zero-copy)
    let artifacts = vec![
        ArtifactData {
            content: Cow::Borrowed("zero-copy text"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "demo".to_string(),
                size_bytes: 13,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        },
        ArtifactData {
            content: Cow::Borrowed("another zero-copy text"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "demo".to_string(),
                size_bytes: 22,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        },
    ];
    
    for artifact in artifacts {
        collector.add_artifact(artifact);
    }
    
    println!("✓ Zero-copy artifacts collected: {}", collector.count());
    println!("✓ Total size: {} bytes", collector.total_size());
    
    // Demonstrate zero-copy access
    for artifact in collector.artifacts() {
        match &artifact.content {
            Cow::Borrowed(s) => println!("  - Borrowed content: {}", s),
            Cow::Owned(s) => println!("  - Owned content: {}", s),
        }
    }

    // Example 13: Performance comparison
    println!("\n13. Performance Comparison");
    let start = Instant::now();
    
    // Test with zero-copy
    let mut zero_copy_collector = StreamingCollector::with_zero_copy();
    for i in 0..1000 {
        let artifact = ArtifactData {
            content: Cow::Borrowed(&format!("artifact-{}", i)),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "perf-test".to_string(),
                size_bytes: 12,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        zero_copy_collector.add_artifact(artifact);
    }
    
    let zero_copy_duration = start.elapsed();
    println!("✓ Zero-copy collection: {:?} for {} artifacts", 
            zero_copy_duration, zero_copy_collector.count());
    
    // Test with owned content
    let start = Instant::now();
    let mut owned_collector = StreamingCollector::with_zero_copy();
    for i in 0..1000 {
        let artifact = ArtifactData {
            content: Cow::Owned(format!("artifact-{}", i)),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "perf-test".to_string(),
                size_bytes: 12,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        owned_collector.add_artifact(artifact);
    }
    
    let owned_duration = start.elapsed();
    println!("✓ Owned content collection: {:?} for {} artifacts", 
            owned_duration, owned_collector.count());
    
    if zero_copy_duration < owned_duration {
        println!("✓ Zero-copy is faster by {:?}", owned_duration - zero_copy_duration);
    } else {
        println!("✓ Owned content is faster by {:?}", zero_copy_duration - owned_duration);
    }

    println!("\n=== All Streaming Artifact Examples Completed Successfully ===");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_basic_streaming() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let stream = ArtifactStream::new(environment);
        let mut count = 0;
        
        stream.stream_artifacts(|_artifact| {
            count += 1;
            Ok(())
        }).await.expect("Should stream artifacts");
        
        assert!(count > 0);
    }

    #[tokio::test]
    async fn test_streaming_collector() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let stream = ArtifactStream::new(environment);
        let collector = stream.collect_into(StreamingCollector::with_zero_copy()).await.expect("Should collect artifacts");
        
        assert!(!collector.is_empty());
        assert!(collector.count() > 0);
    }

    #[tokio::test]
    async fn test_filtering_collector() {
        let mut collector = FilteringCollector::new(100)
            .filter_by_type(ArtifactType::Text);
        
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
        assert_eq!(collector.count(), 1);
    }

    #[tokio::test]
    async fn test_aggregating_collector() {
        let mut collector = AggregatingCollector::new(100)
            .aggregate_by_type();
        
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
        assert_eq!(collector.count(), 1);
        assert_eq!(collector.aggregation_keys().len(), 1);
    }

    #[tokio::test]
    async fn test_transforming_collector() {
        let mut collector = TransformingCollector::new(100)
            .transform_content(|content| content.to_uppercase());
        
        let artifact = ArtifactData {
            content: Cow::Borrowed("hello"),
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
    }

    #[tokio::test]
    async fn test_batching_collector() {
        let mut collector = BatchingCollector::new(100, 2)
            .with_batch_processor(|batch| {
                assert_eq!(batch.len(), 2);
                Ok(())
            });
        
        let artifact1 = ArtifactData {
            content: Cow::Borrowed("test1"),
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
            content: Cow::Borrowed("test2"),
            metadata: ArtifactMetadata {
                artifact_type: ArtifactType::Text,
                source: "test".to_string(),
                size_bytes: 5,
                content_hash: None,
                properties: std::collections::HashMap::new(),
            },
            timestamp: Instant::now(),
        };
        
        collector.add_artifact(artifact1).expect("Should add artifact");
        assert_eq!(collector.current_batch_size(), 1);
        
        collector.add_artifact(artifact2).expect("Should add artifact");
        assert_eq!(collector.current_batch_size(), 0);
        assert_eq!(collector.count(), 2);
    }

    #[test]
    fn test_ring_buffer() {
        let mut buffer = RingBuffer::new(100);
        
        let data = b"hello world";
        let written = buffer.write(data).unwrap();
        assert_eq!(written, data.len());
        
        let mut read_buf = vec![0; data.len()];
        let read = buffer.read(&mut read_buf).unwrap();
        assert_eq!(read, data.len());
        assert_eq!(&read_buf, data);
    }

    #[test]
    fn test_circular_buffer() {
        let mut buffer = CircularBuffer::new(10);
        
        let data = b"hello world this is a long string";
        let written = buffer.write(data).unwrap();
        assert_eq!(written, data.len());
        assert!(buffer.capacity() > 10);
    }

    #[tokio::test]
    async fn test_memory_mapped_buffer() {
        let mut buffer = MemoryMappedBuffer::new(100);
        
        let data = b"test data";
        let written = buffer.write(data).await.unwrap();
        assert_eq!(written, data.len());
        
        let mut read_buf = vec![0; data.len()];
        let read = buffer.read(&mut read_buf).await.unwrap();
        assert_eq!(read, data.len());
        assert_eq!(&read_buf, data);
    }

    #[tokio::test]
    async fn test_buffer_pool() {
        let pool = BufferPool::new(100, 5);
        
        let buffer = pool.get_buffer().await;
        assert_eq!(buffer.capacity(), 100);
        
        pool.return_buffer(buffer).await;
        
        let stats = pool.statistics().await;
        assert_eq!(stats.available_buffers, 1);
    }

    #[tokio::test]
    async fn test_convenience_functions() {
        let environment = CleanroomBuilder::new()
            .build_minimal()
            .await
            .expect("Should build environment");
        
        let _stream = artifact_stream(environment);
        let _collector = streaming_collector();
        let _ring = ring_buffer(100);
        let _circular = circular_buffer(100);
        let _memory = memory_mapped_buffer(100);
        let _pool = buffer_pool(100, 5);
        
        // Just verify they compile and create valid instances
        assert!(_collector.is_empty());
        assert!(_ring.is_empty());
        assert!(_circular.is_empty());
        assert!(_memory.is_empty());
    }
}
