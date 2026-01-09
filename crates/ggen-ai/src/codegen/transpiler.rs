//! TTL (Turtle) to Signature transpiler with caching and metrics
//!
//! Converts RDF ontology definitions (in TTL format) to DSPy Signature objects.
//! Includes LRU caching for property shapes and comprehensive metrics tracking.

use crate::error::{GgenAiError, Result};
use crate::dspy::{InputField, OutputField, Signature};
use lru::LruCache;
use std::num::NonZeroUsize;
use std::sync::{Arc, Mutex};
use super::metrics::{ProcessMetrics, Timer};

/// RDF property shape representation
/// Simplified model for shape constraints in RDF
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct PropertyShape {
    /// IRI/URI of the property
    pub property_iri: String,
    /// Name of the property (last part of IRI)
    pub property_name: String,
    /// Description or comment
    pub description: String,
    /// Expected datatype (e.g., "xsd:string", "xsd:integer")
    pub datatype: String,
    /// Minimum cardinality (0 for optional)
    pub min_count: usize,
    /// Maximum cardinality (0 = unbounded)
    pub max_count: usize,
}

impl PropertyShape {
    /// Create a new property shape
    pub fn new(
        property_iri: String,
        description: String,
        datatype: String,
    ) -> Self {
        let property_name = property_iri
            .split('/')
            .last()
            .or_else(|| property_iri.split('#').last())
            .unwrap_or("property")
            .to_string();

        Self {
            property_iri,
            property_name,
            description,
            datatype,
            min_count: 0,
            max_count: 0,
        }
    }

    /// Convert to InputField
    pub fn to_input_field(&self) -> InputField {
        InputField::new(
            &self.property_name,
            &self.description,
            &self.datatype,
        )
    }

    /// Convert to OutputField
    pub fn to_output_field(&self) -> OutputField {
        OutputField::new(
            &self.property_name,
            &self.description,
            &self.datatype,
        )
    }
}

/// TTL to Signature Transpiler with caching and metrics
///
/// Processes RDF ontology definitions and generates DSPy Signatures.
/// Implements LRU caching for property shapes to improve performance.
pub struct TTLToSignatureTranspiler {
    /// LRU cache for property shapes (key: IRI string)
    property_cache: Arc<Mutex<LruCache<String, Vec<PropertyShape>>>>,
    /// Process metrics tracking
    metrics: Arc<Mutex<ProcessMetrics>>,
}

impl TTLToSignatureTranspiler {
    /// Create a new transpiler with specified cache size
    ///
    /// # Arguments
    /// * `cache_size` - Maximum number of IRI entries to cache (e.g., 500)
    ///
    /// # Errors
    /// Returns error if cache_size is 0 (NonZeroUsize requirement)
    pub fn new(cache_size: usize) -> Result<Self> {
        let cache_capacity = NonZeroUsize::new(cache_size)
            .ok_or_else(|| GgenAiError::ConfigurationError(
                "Cache size must be greater than 0".to_string()
            ))?;

        Ok(Self {
            property_cache: Arc::new(Mutex::new(LruCache::new(cache_capacity))),
            metrics: Arc::new(Mutex::new(ProcessMetrics::new())),
        })
    }

    /// Build signatures from TTL ontology string
    ///
    /// Processes the TTL content and generates Signature objects for each class definition.
    ///
    /// # Arguments
    /// * `ttl_content` - The TTL ontology content as a string
    ///
    /// # Returns
    /// A vector of generated Signatures and populated metrics
    pub fn build_signatures(&self, ttl_content: &str) -> Result<Vec<Signature>> {
        let timer = Timer::start();
        let mut metrics = self.metrics.lock()
            .map_err(|_| GgenAiError::LockError("metrics".to_string()))?;

        let signatures = self.parse_signatures(ttl_content, &mut metrics)?;

        metrics.signatures_generated = signatures.len();
        metrics.processing_time_ms = timer.stop();

        Ok(signatures)
    }

    /// Parse signatures from TTL content with metric tracking
    fn parse_signatures(
        &self,
        ttl_content: &str,
        metrics: &mut ProcessMetrics,
    ) -> Result<Vec<Signature>> {
        let mut signatures = Vec::new();

        // Simple line-by-line parser for demo purposes
        // In production, would use proper Turtle/RDF parser
        for line in ttl_content.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Look for class definitions (simplified pattern)
            if line.contains("rdf:type") && line.contains("rdfs:Class") {
                match self.extract_signature_from_line(line, metrics) {
                    Ok(Some(sig)) => signatures.push(sig),
                    Ok(None) => {}
                    Err(_) => {
                        metrics.error_count += 1;
                    }
                }
            }
        }

        Ok(signatures)
    }

    /// Extract a signature from a TTL line
    fn extract_signature_from_line(
        &self,
        line: &str,
        metrics: &mut ProcessMetrics,
    ) -> Result<Option<Signature>> {
        // Simple extraction logic
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.is_empty() {
            return Ok(None);
        }

        let name = parts[0]
            .split('/')
            .last()
            .unwrap_or("Unknown")
            .to_string();

        let description = format!("Class definition for {}", name);

        let mut signature = Signature::new(name, description);

        // In a real implementation, would look up property shapes and add fields
        // For now, return basic signature
        Ok(Some(signature))
    }

    /// Find property shapes for a given IRI with caching
    ///
    /// # Arguments
    /// * `iri` - The IRI/URI to look up
    ///
    /// # Returns
    /// Vector of PropertyShape for the given IRI
    pub fn find_property_shapes(&self, iri: &str) -> Result<Vec<PropertyShape>> {
        // Try cache first
        {
            let mut cache = self.property_cache.lock()
                .map_err(|_| GgenAiError::LockError("property_cache".to_string()))?;

            if let Some(shapes) = cache.get(iri) {
                // Update metrics
                if let Ok(mut metrics) = self.metrics.lock() {
                    metrics.cache_hits += 1;
                }
                return Ok(shapes.clone());
            }
        }

        // Cache miss - compute property shapes
        let shapes = self.compute_property_shapes(iri)?;

        // Store in cache
        {
            let mut cache = self.property_cache.lock()
                .map_err(|_| GgenAiError::LockError("property_cache".to_string()))?;

            cache.put(iri.to_string(), shapes.clone());
        }

        // Update metrics
        if let Ok(mut metrics) = self.metrics.lock() {
            metrics.cache_misses += 1;
        }

        Ok(shapes)
    }

    /// Compute property shapes for an IRI (internal implementation)
    fn compute_property_shapes(&self, iri: &str) -> Result<Vec<PropertyShape>> {
        // Simplified implementation - in production would parse RDF graph
        let mut shapes = Vec::new();

        // Example: extract properties from IRI pattern
        if !iri.is_empty() {
            // Add a basic property shape
            let prop_name = format!("{}_property",
                iri.split('/').last().unwrap_or("unknown"));
            shapes.push(PropertyShape::new(
                format!("{}#{}", iri, prop_name),
                format!("Property of {}", iri),
                "xsd:string".to_string(),
            ));
        }

        Ok(shapes)
    }

    /// Extract datatype from property shape (with caching via shape lookup)
    ///
    /// # Arguments
    /// * `property_iri` - The property IRI to extract datatype from
    ///
    /// # Returns
    /// The datatype string (e.g., "xsd:string", "xsd:integer")
    pub fn extract_datatype(&self, property_iri: &str) -> Result<String> {
        // Use find_property_shapes which includes caching logic
        let shapes = self.find_property_shapes(property_iri)?;

        shapes
            .first()
            .map(|shape| shape.datatype.clone())
            .ok_or_else(|| GgenAiError::NotFound(
                format!("No datatype found for property: {}", property_iri)
            ))
    }

    /// Get current metrics
    pub fn metrics(&self) -> Result<ProcessMetrics> {
        self.metrics.lock()
            .map(|m| m.clone())
            .map_err(|_| GgenAiError::LockError("metrics".to_string()))
    }

    /// Clear the cache and reset metrics
    pub fn clear(&self) -> Result<()> {
        self.property_cache.lock()
            .map_err(|_| GgenAiError::LockError("property_cache".to_string()))?
            .clear();

        self.metrics.lock()
            .map_err(|_| GgenAiError::LockError("metrics".to_string()))?
            .reset();

        Ok(())
    }

    /// Get cache size
    pub fn cache_size(&self) -> Result<usize> {
        Ok(self.property_cache.lock()
            .map_err(|_| GgenAiError::LockError("property_cache".to_string()))?
            .len())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transpiler_creation() {
        let transpiler = TTLToSignatureTranspiler::new(500).unwrap();
        assert!(transpiler.cache_size().unwrap() == 0);
    }

    #[test]
    fn test_transpiler_creation_zero_cache() {
        let result = TTLToSignatureTranspiler::new(0);
        assert!(result.is_err());
    }

    #[test]
    fn test_find_property_shapes_cache_miss() {
        let transpiler = TTLToSignatureTranspiler::new(100).unwrap();
        let shapes = transpiler.find_property_shapes("http://example.com/Person").unwrap();
        assert!(!shapes.is_empty());

        let metrics = transpiler.metrics().unwrap();
        assert_eq!(metrics.cache_misses, 1);
        assert_eq!(metrics.cache_hits, 0);
    }

    #[test]
    fn test_find_property_shapes_cache_hit() {
        let transpiler = TTLToSignatureTranspiler::new(100).unwrap();

        // First call - cache miss
        let _shapes1 = transpiler.find_property_shapes("http://example.com/Person").unwrap();

        // Second call - cache hit
        let _shapes2 = transpiler.find_property_shapes("http://example.com/Person").unwrap();

        let metrics = transpiler.metrics().unwrap();
        assert_eq!(metrics.cache_misses, 1);
        assert_eq!(metrics.cache_hits, 1);
    }

    #[test]
    fn test_extract_datatype() {
        let transpiler = TTLToSignatureTranspiler::new(100).unwrap();
        let datatype = transpiler.extract_datatype("http://example.com/name").unwrap();
        assert_eq!(datatype, "xsd:string");
    }

    #[test]
    fn test_build_signatures_empty_ttl() {
        let transpiler = TTLToSignatureTranspiler::new(100).unwrap();
        let sigs = transpiler.build_signatures("").unwrap();
        assert_eq!(sigs.len(), 0);

        let metrics = transpiler.metrics().unwrap();
        assert_eq!(metrics.signatures_generated, 0);
    }

    #[test]
    fn test_clear_metrics() {
        let transpiler = TTLToSignatureTranspiler::new(100).unwrap();
        let _ = transpiler.find_property_shapes("http://example.com/Test").unwrap();

        let metrics_before = transpiler.metrics().unwrap();
        assert!(metrics_before.cache_misses > 0);

        transpiler.clear().unwrap();

        let metrics_after = transpiler.metrics().unwrap();
        assert_eq!(metrics_after.cache_misses, 0);
        assert_eq!(transpiler.cache_size().unwrap(), 0);
    }

    #[test]
    fn test_multiple_property_shapes_caching() {
        let transpiler = TTLToSignatureTranspiler::new(100).unwrap();

        // Access 3 different IRIs
        let _ = transpiler.find_property_shapes("http://example.com/Iri1").unwrap();
        let _ = transpiler.find_property_shapes("http://example.com/Iri2").unwrap();
        let _ = transpiler.find_property_shapes("http://example.com/Iri3").unwrap();

        let metrics = transpiler.metrics().unwrap();
        assert_eq!(metrics.cache_misses, 3);

        // Re-access first IRI - should hit cache
        let _ = transpiler.find_property_shapes("http://example.com/Iri1").unwrap();
        let metrics = transpiler.metrics().unwrap();
        assert_eq!(metrics.cache_hits, 1);
    }

    #[test]
    fn test_property_shape_conversion() {
        let shape = PropertyShape::new(
            "http://example.com/name".to_string(),
            "Person name".to_string(),
            "xsd:string".to_string(),
        );

        assert_eq!(shape.property_name, "name");
        let input = shape.to_input_field();
        assert_eq!(input.name(), "name");

        let output = shape.to_output_field();
        assert_eq!(output.name(), "name");
    }

    #[test]
    fn test_deterministic_caching() {
        let transpiler = TTLToSignatureTranspiler::new(100).unwrap();

        // Call same IRI multiple times
        for _ in 0..5 {
            let _ = transpiler.find_property_shapes("http://example.com/Test").unwrap();
        }

        let metrics = transpiler.metrics().unwrap();
        // Should be 1 miss and 4 hits
        assert_eq!(metrics.cache_misses, 1);
        assert_eq!(metrics.cache_hits, 4);
    }
}
