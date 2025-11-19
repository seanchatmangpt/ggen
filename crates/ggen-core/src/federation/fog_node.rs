//! Fog Computing Template Generation Nodes
//!
//! This module implements fog computing nodes that can generate templates
//! at the edge using synchronized ontologies from the federation network.
//! Features:
//! - Hierarchical fog node architecture (cloud → fog → edge)
//! - Adaptive template generation based on device capabilities
//! - Caching and prefetching strategies
//! - Load balancing across fog layers

use super::{NodeId, VectorClock, DeviceCapabilities, QosMetrics, NetworkTopology};
use crate::federation::crdt::DistributedKnowledgeGraph;
use crate::federation::gossip::GossipNode;
use crate::federation::rdf_delta::RdfTriple;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, SystemTime};

/// Fog node layer in the hierarchy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FogLayer {
    /// Cloud layer (full resources, complete ontology)
    Cloud,
    /// Fog layer (intermediate resources, partial ontology)
    Fog,
    /// Edge layer (limited resources, minimal ontology)
    Edge,
}

/// Edge device in the federation network
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EdgeDevice {
    pub node_id: NodeId,
    pub capabilities: DeviceCapabilities,
    pub qos_metrics: QosMetrics,
    pub parent_fog_node: Option<NodeId>,
    pub cached_ontology_size_mb: u64,
    pub last_sync: SystemTime,
}

impl EdgeDevice {
    pub fn new(
        node_id: NodeId,
        capabilities: DeviceCapabilities,
        qos_metrics: QosMetrics,
    ) -> Self {
        Self {
            node_id,
            capabilities,
            qos_metrics,
            parent_fog_node: None,
            cached_ontology_size_mb: 0,
            last_sync: SystemTime::now(),
        }
    }

    /// Check if device can generate templates locally
    pub fn can_generate_locally(&self) -> bool {
        self.capabilities.can_generate_templates
            && self.cached_ontology_size_mb <= self.capabilities.max_ontology_size_mb
            && !self.qos_metrics.is_constrained()
    }

    /// Estimate generation time based on device capabilities
    pub fn estimate_generation_time_ms(&self, template_complexity: u32) -> u64 {
        let base_time = template_complexity as u64 * 10; // 10ms per complexity unit
        let cpu_factor = 4.0 / self.capabilities.cpu_cores as f64;
        (base_time as f64 * cpu_factor) as u64
    }
}

/// Template generation request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateGenerationRequest {
    pub request_id: String,
    pub template_name: String,
    pub template_version: String,
    pub context: HashMap<String, String>,
    pub priority: RequestPriority,
    pub deadline_ms: Option<u64>,
}

/// Priority levels for generation requests
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum RequestPriority {
    Low = 0,
    Normal = 1,
    High = 2,
    Critical = 3,
}

/// Template generation response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateGenerationResponse {
    pub request_id: String,
    pub generated_content: String,
    pub generation_time_ms: u64,
    pub generator_node: NodeId,
    pub cache_hit: bool,
}

/// Template cache entry
#[derive(Debug, Clone, Serialize, Deserialize)]
struct CacheEntry {
    template_name: String,
    context_hash: u64,
    generated_content: String,
    created_at: SystemTime,
    access_count: u64,
    size_bytes: usize,
}

impl CacheEntry {
    fn is_expired(&self, ttl: Duration) -> bool {
        SystemTime::now()
            .duration_since(self.created_at)
            .map(|age| age > ttl)
            .unwrap_or(true)
    }
}

/// Template cache with LRU eviction
pub struct TemplateCache {
    entries: HashMap<String, CacheEntry>,
    max_size_mb: u64,
    current_size_bytes: usize,
    ttl: Duration,
}

impl TemplateCache {
    pub fn new(max_size_mb: u64, ttl: Duration) -> Self {
        Self {
            entries: HashMap::new(),
            max_size_mb,
            current_size_bytes: 0,
            ttl,
        }
    }

    fn compute_cache_key(template_name: &str, context: &HashMap<String, String>) -> String {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        template_name.hash(&mut hasher);

        let mut sorted_context: Vec<_> = context.iter().collect();
        sorted_context.sort_by_key(|(k, _)| *k);
        sorted_context.hash(&mut hasher);

        format!("{}:{:x}", template_name, hasher.finish())
    }

    pub fn get(&mut self, template_name: &str, context: &HashMap<String, String>) -> Option<String> {
        let key = Self::compute_cache_key(template_name, context);

        if let Some(entry) = self.entries.get_mut(&key) {
            if !entry.is_expired(self.ttl) {
                entry.access_count += 1;
                return Some(entry.generated_content.clone());
            } else {
                // Remove expired entry
                self.current_size_bytes -= entry.size_bytes;
                self.entries.remove(&key);
            }
        }

        None
    }

    pub fn put(
        &mut self,
        template_name: String,
        context: HashMap<String, String>,
        generated_content: String,
    ) {
        let key = Self::compute_cache_key(&template_name, &context);
        let size_bytes = generated_content.len();

        // Evict if necessary
        while self.current_size_bytes + size_bytes > (self.max_size_mb * 1024 * 1024) as usize
            && !self.entries.is_empty()
        {
            self.evict_lru();
        }

        let entry = CacheEntry {
            template_name,
            context_hash: 0,
            generated_content,
            created_at: SystemTime::now(),
            access_count: 1,
            size_bytes,
        };

        self.current_size_bytes += size_bytes;
        self.entries.insert(key, entry);
    }

    fn evict_lru(&mut self) {
        // Find entry with lowest access count and oldest timestamp
        let lru_key = self
            .entries
            .iter()
            .min_by_key(|(_, entry)| (entry.access_count, entry.created_at))
            .map(|(k, _)| k.clone());

        if let Some(key) = lru_key {
            if let Some(entry) = self.entries.remove(&key) {
                self.current_size_bytes -= entry.size_bytes;
            }
        }
    }

    pub fn clear(&mut self) {
        self.entries.clear();
        self.current_size_bytes = 0;
    }

    pub fn size_mb(&self) -> f64 {
        self.current_size_bytes as f64 / (1024.0 * 1024.0)
    }
}

/// Fog computing node for template generation
pub struct FogNode {
    pub node_id: NodeId,
    pub layer: FogLayer,
    pub capabilities: DeviceCapabilities,
    pub qos_metrics: QosMetrics,
    pub knowledge_graph: DistributedKnowledgeGraph,
    pub gossip_node: GossipNode,
    pub template_cache: TemplateCache,
    pub child_nodes: Vec<NodeId>,
    pub parent_node: Option<NodeId>,
    pub load_metrics: LoadMetrics,
}

/// Load balancing metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoadMetrics {
    pub active_requests: u32,
    pub avg_response_time_ms: u64,
    pub cpu_usage_percent: f32,
    pub memory_usage_percent: f32,
    pub cache_hit_rate: f32,
}

impl LoadMetrics {
    pub fn new() -> Self {
        Self {
            active_requests: 0,
            avg_response_time_ms: 0,
            cpu_usage_percent: 0.0,
            memory_usage_percent: 0.0,
            cache_hit_rate: 0.0,
        }
    }

    pub fn is_overloaded(&self) -> bool {
        self.active_requests > 100
            || self.cpu_usage_percent > 80.0
            || self.memory_usage_percent > 90.0
    }

    pub fn capacity_score(&self) -> f32 {
        // Higher score = more capacity
        let cpu_score = 1.0 - (self.cpu_usage_percent / 100.0);
        let mem_score = 1.0 - (self.memory_usage_percent / 100.0);
        let load_score = 1.0 - (self.active_requests as f32 / 100.0).min(1.0);

        (cpu_score + mem_score + load_score) / 3.0
    }
}

impl Default for LoadMetrics {
    fn default() -> Self {
        Self::new()
    }
}

impl FogNode {
    pub fn new(
        node_id: NodeId,
        layer: FogLayer,
        capabilities: DeviceCapabilities,
        qos_metrics: QosMetrics,
        knowledge_graph: DistributedKnowledgeGraph,
        gossip_node: GossipNode,
    ) -> Self {
        let cache_size_mb = match layer {
            FogLayer::Cloud => 1024, // 1GB
            FogLayer::Fog => 256,    // 256MB
            FogLayer::Edge => 64,    // 64MB
        };

        Self {
            node_id,
            layer,
            capabilities,
            qos_metrics,
            knowledge_graph,
            gossip_node,
            template_cache: TemplateCache::new(cache_size_mb, Duration::from_secs(3600)),
            child_nodes: Vec::new(),
            parent_node: None,
            load_metrics: LoadMetrics::new(),
        }
    }

    /// Add a child node to the hierarchy
    pub fn add_child_node(&mut self, child_id: NodeId) {
        if !self.child_nodes.contains(&child_id) {
            self.child_nodes.push(child_id);
        }
    }

    /// Generate template from request
    pub fn generate_template(
        &mut self,
        request: &TemplateGenerationRequest,
    ) -> Result<TemplateGenerationResponse, String> {
        let start_time = SystemTime::now();

        // Check cache first
        if let Some(cached_content) = self.template_cache.get(
            &request.template_name,
            &request.context,
        ) {
            let generation_time = start_time.elapsed().unwrap().as_millis() as u64;
            return Ok(TemplateGenerationResponse {
                request_id: request.request_id.clone(),
                generated_content: cached_content,
                generation_time_ms: generation_time,
                generator_node: self.node_id.clone(),
                cache_hit: true,
            });
        }

        // Generate template (placeholder - would use actual template engine)
        let generated_content = self.generate_from_ontology(request)?;

        // Cache the result
        self.template_cache.put(
            request.template_name.clone(),
            request.context.clone(),
            generated_content.clone(),
        );

        let generation_time = start_time.elapsed().unwrap().as_millis() as u64;

        Ok(TemplateGenerationResponse {
            request_id: request.request_id.clone(),
            generated_content,
            generation_time_ms: generation_time,
            generator_node: self.node_id.clone(),
            cache_hit: false,
        })
    }

    fn generate_from_ontology(
        &self,
        request: &TemplateGenerationRequest,
    ) -> Result<String, String> {
        // Placeholder for actual template generation using knowledge graph
        // Would query the CRDT-based distributed knowledge graph and
        // use Tera or similar template engine

        let triples = self.knowledge_graph.get_triples();

        Ok(format!(
            "// Generated template: {}\n// Version: {}\n// Triples in ontology: {}\n// Generated by: {}\n",
            request.template_name,
            request.template_version,
            triples.len(),
            self.node_id.0
        ))
    }

    /// Decide whether to handle request locally or delegate
    pub fn route_request(
        &self,
        request: &TemplateGenerationRequest,
    ) -> RoutingDecision {
        // If overloaded, delegate
        if self.load_metrics.is_overloaded() {
            return RoutingDecision::DelegateToParent;
        }

        // If we have the capability, handle locally
        if self.capabilities.can_generate_templates {
            return RoutingDecision::HandleLocally;
        }

        // Otherwise, delegate based on layer
        match self.layer {
            FogLayer::Cloud => RoutingDecision::HandleLocally,
            FogLayer::Fog => {
                if let Some(_parent) = &self.parent_node {
                    RoutingDecision::DelegateToParent
                } else {
                    RoutingDecision::HandleLocally
                }
            }
            FogLayer::Edge => {
                if let Some(_parent) = &self.parent_node {
                    RoutingDecision::DelegateToParent
                } else {
                    RoutingDecision::Reject("No parent available".to_string())
                }
            }
        }
    }

    /// Select best child node for delegation
    pub fn select_best_child(&self, child_metrics: &HashMap<NodeId, LoadMetrics>) -> Option<NodeId> {
        self.child_nodes
            .iter()
            .filter_map(|child_id| {
                child_metrics.get(child_id).map(|metrics| {
                    (child_id, metrics.capacity_score())
                })
            })
            .max_by(|(_, score_a), (_, score_b)| {
                score_a.partial_cmp(score_b).unwrap_or(std::cmp::Ordering::Equal)
            })
            .map(|(child_id, _)| child_id.clone())
    }

    /// Update load metrics
    pub fn update_load_metrics(&mut self, response_time_ms: u64) {
        const ALPHA: f32 = 0.1;

        self.load_metrics.avg_response_time_ms =
            ((1.0 - ALPHA) * self.load_metrics.avg_response_time_ms as f32
                + ALPHA * response_time_ms as f32) as u64;

        // Update cache hit rate
        let total_cache_accesses = self.template_cache.entries.len() as f32;
        if total_cache_accesses > 0.0 {
            let hits: u64 = self.template_cache.entries.values()
                .map(|e| e.access_count)
                .sum();
            self.load_metrics.cache_hit_rate = hits as f32 / total_cache_accesses;
        }
    }
}

/// Routing decision for template generation
#[derive(Debug, Clone)]
pub enum RoutingDecision {
    HandleLocally,
    DelegateToParent,
    DelegateToChild(NodeId),
    Reject(String),
}

/// Template generation node (convenience wrapper)
pub struct TemplateGenerationNode {
    pub fog_node: FogNode,
}

impl TemplateGenerationNode {
    pub fn new(fog_node: FogNode) -> Self {
        Self { fog_node }
    }

    pub fn process_request(
        &mut self,
        request: TemplateGenerationRequest,
    ) -> Result<TemplateGenerationResponse, String> {
        self.fog_node.load_metrics.active_requests += 1;

        let result = self.fog_node.generate_template(&request);

        self.fog_node.load_metrics.active_requests -= 1;

        if let Ok(ref response) = result {
            self.fog_node.update_load_metrics(response.generation_time_ms);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::federation::crdt::MergeStrategy;
    use crate::federation::gossip::GossipConfig;

    fn create_test_fog_node(id: &str, layer: FogLayer) -> FogNode {
        let capabilities = DeviceCapabilities {
            cpu_cores: 4,
            memory_mb: 2048,
            storage_mb: 10240,
            can_generate_templates: true,
            max_ontology_size_mb: 500,
        };

        let qos_metrics = QosMetrics {
            bandwidth_kbps: 1000,
            latency_ms: 20,
            packet_loss_rate: 0.01,
            jitter_ms: 5,
            battery_level: Some(0.8),
        };

        let node_id = NodeId::new(id);
        let kg = DistributedKnowledgeGraph::new(
            node_id.clone(),
            MergeStrategy::LastWriteWins,
        );

        let gossip_node = GossipNode::new(
            node_id.clone(),
            GossipConfig::default(),
            capabilities.clone(),
            qos_metrics.clone(),
        );

        FogNode::new(node_id, layer, capabilities, qos_metrics, kg, gossip_node)
    }

    #[test]
    fn test_template_cache() {
        let mut cache = TemplateCache::new(1, Duration::from_secs(3600));

        let mut context = HashMap::new();
        context.insert("key1".to_string(), "value1".to_string());

        assert!(cache.get("template1", &context).is_none());

        cache.put(
            "template1".to_string(),
            context.clone(),
            "generated content".to_string(),
        );

        assert_eq!(
            cache.get("template1", &context),
            Some("generated content".to_string())
        );
    }

    #[test]
    fn test_fog_node_generation() {
        let mut node = create_test_fog_node("fog1", FogLayer::Fog);

        let request = TemplateGenerationRequest {
            request_id: "req1".to_string(),
            template_name: "test-template".to_string(),
            template_version: "1.0.0".to_string(),
            context: HashMap::new(),
            priority: RequestPriority::Normal,
            deadline_ms: None,
        };

        let response = node.generate_template(&request).unwrap();
        assert_eq!(response.request_id, "req1");
        assert!(response.generation_time_ms >= 0);
    }

    #[test]
    fn test_fog_node_hierarchy() {
        let mut parent = create_test_fog_node("fog1", FogLayer::Fog);
        let child = create_test_fog_node("edge1", FogLayer::Edge);

        parent.add_child_node(child.node_id.clone());

        assert_eq!(parent.child_nodes.len(), 1);
        assert_eq!(parent.child_nodes[0], child.node_id);
    }

    #[test]
    fn test_load_metrics() {
        let mut metrics = LoadMetrics::new();

        assert!(!metrics.is_overloaded());

        metrics.cpu_usage_percent = 85.0;
        assert!(metrics.is_overloaded());
    }

    #[test]
    fn test_edge_device() {
        let capabilities = DeviceCapabilities {
            cpu_cores: 2,
            memory_mb: 512,
            storage_mb: 1024,
            can_generate_templates: true,
            max_ontology_size_mb: 100,
        };

        let qos_metrics = QosMetrics {
            bandwidth_kbps: 100,
            latency_ms: 50,
            packet_loss_rate: 0.05,
            jitter_ms: 10,
            battery_level: Some(0.5),
        };

        let device = EdgeDevice::new(
            NodeId::new("edge1"),
            capabilities,
            qos_metrics,
        );

        assert!(device.can_generate_locally());

        let generation_time = device.estimate_generation_time_ms(10);
        assert!(generation_time > 0);
    }
}
