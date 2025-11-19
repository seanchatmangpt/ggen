//! Integration tests for edge computing ontology federation
//!
//! This test suite demonstrates the complete functionality of the federation system,
//! including gossip-based synchronization, RDF delta encoding, CRDTs, and fog computing
//! template generation.

use ggen_core::federation::{
    // Core types
    NodeId, VectorClock, NetworkTopology, QosMetrics, DeviceCapabilities,
    // Gossip
    GossipNode, GossipConfig, GossipMessage, GossipMessageType, GossipPayload,
    // RDF Delta
    RdfDelta, DeltaEncoder, CompressionStrategy,
    rdf_delta::RdfTriple,
    // CRDT
    DistributedKnowledgeGraph, CrdtOperation, MergeStrategy,
    crdt::OperationTag,
    // Fog nodes
    FogNode, FogLayer, TemplateGenerationRequest, TemplateGenerationNode, EdgeDevice,
    fog_node::RequestPriority,
    // Protocol
    FederationProtocol, SyncRequest, SyncType, FederationConfig,
};
use std::collections::HashMap;

#[test]
fn test_vector_clock_operations() {
    let mut clock1 = VectorClock::new();
    let mut clock2 = VectorClock::new();

    let node1 = NodeId::new("node1");
    let node2 = NodeId::new("node2");

    clock1.increment(&node1);
    clock1.increment(&node1);

    clock2.increment(&node2);

    assert!(clock1.concurrent(&clock2));
    assert!(clock2.concurrent(&clock1));

    clock2.increment(&node1);
    clock2.increment(&node1);
    clock2.increment(&node1);

    assert!(clock1.happens_before(&clock2));
    assert!(!clock2.happens_before(&clock1));
}

#[test]
fn test_gossip_node_peer_selection() {
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

    let mut node = GossipNode::new(
        NodeId::new("central"),
        GossipConfig::default(),
        capabilities.clone(),
        qos_metrics.clone(),
    );

    // Add some peers
    for i in 1..=5 {
        let peer_info = ggen_core::federation::gossip::PeerInfo {
            node_id: NodeId::new(&format!("peer{}", i)),
            last_seen: std::time::SystemTime::now(),
            vector_clock: VectorClock::new(),
            qos_metrics: qos_metrics.clone(),
            reliability_score: 0.5 + (i as f32 * 0.1),
            round_trip_time_ms: 20 + (i * 5),
        };
        node.add_peer(peer_info);
    }

    let selected_peers = node.select_gossip_peers();
    assert!(!selected_peers.is_empty());
    assert!(selected_peers.len() <= node.config.fanout);
}

#[test]
fn test_rdf_delta_compression() {
    let mut delta = RdfDelta::new("v1".to_string(), "v2".to_string());

    // Add a large number of triples
    for i in 0..100 {
        delta.add_insert(RdfTriple {
            subject: format!("http://example.org/subject{}", i),
            predicate: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),
            object: "http://example.org/Class".to_string(),
            graph: Some("http://example.org/graph1".to_string()),
        });
    }

    assert_eq!(delta.size(), 100);

    // Test different compression strategies
    let mut encoder_none = DeltaEncoder::new(CompressionStrategy::None);
    let encoded_none = encoder_none.encode(&delta).unwrap();

    let mut encoder_gzip = DeltaEncoder::new(CompressionStrategy::Gzip);
    let encoded_gzip = encoder_gzip.encode(&delta).unwrap();

    let mut encoder_hybrid = DeltaEncoder::new(CompressionStrategy::Hybrid);
    let encoded_hybrid = encoder_hybrid.encode(&delta).unwrap();

    // Hybrid should provide better compression
    assert!(encoded_hybrid.len() < encoded_none.len());

    // Verify roundtrip
    let decoded = encoder_hybrid.decode(&encoded_hybrid).unwrap();
    assert_eq!(decoded.size(), delta.size());
}

#[test]
fn test_crdt_or_set_concurrent_operations() {
    use ggen_core::federation::crdt::OrSet;

    let mut set1 = OrSet::new();
    let mut set2 = OrSet::new();

    let triple = RdfTriple {
        subject: "ex:s1".to_string(),
        predicate: "ex:p1".to_string(),
        object: "ex:o1".to_string(),
        graph: None,
    };

    // Concurrent add operations
    let tag1 = OperationTag::new(NodeId::new("node1"), 1);
    let tag2 = OperationTag::new(NodeId::new("node2"), 1);

    set1.add(triple.clone(), tag1.clone());
    set2.add(triple.clone(), tag2.clone());

    // Both should contain the triple
    assert!(set1.contains(&triple));
    assert!(set2.contains(&triple));

    // Merge the sets
    set1.merge(&set2);

    // After merge, should still contain the triple
    assert!(set1.contains(&triple));

    // Remove with one tag
    set1.remove_tag(&triple, &tag1);

    // Should still be present due to tag2
    assert!(set1.contains(&triple));

    // Remove with second tag
    set1.remove_tag(&triple, &tag2);

    // Now should be removed
    assert!(!set1.contains(&triple));
}

#[test]
fn test_distributed_knowledge_graph() {
    let mut graph1 = DistributedKnowledgeGraph::new(
        NodeId::new("node1"),
        MergeStrategy::LastWriteWins,
    );

    let mut graph2 = DistributedKnowledgeGraph::new(
        NodeId::new("node2"),
        MergeStrategy::LastWriteWins,
    );

    // Add triples to both graphs
    let triple1 = RdfTriple {
        subject: "ex:alice".to_string(),
        predicate: "ex:name".to_string(),
        object: "Alice".to_string(),
        graph: None,
    };

    let triple2 = RdfTriple {
        subject: "ex:bob".to_string(),
        predicate: "ex:name".to_string(),
        object: "Bob".to_string(),
        graph: None,
    };

    graph1.add_triple(triple1.clone());
    graph2.add_triple(triple2.clone());

    assert_eq!(graph1.size(), 1);
    assert_eq!(graph2.size(), 1);

    // Merge graphs
    graph1.merge(&graph2);

    assert_eq!(graph1.size(), 2);
}

#[test]
fn test_fog_node_template_generation() {
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

    let node_id = NodeId::new("fog1");
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

    let mut fog_node = FogNode::new(
        node_id,
        FogLayer::Fog,
        capabilities,
        qos_metrics,
        kg,
        gossip_node,
    );

    // Create a template generation request
    let request = TemplateGenerationRequest {
        request_id: "req1".to_string(),
        template_name: "rust-service".to_string(),
        template_version: "1.0.0".to_string(),
        context: HashMap::new(),
        priority: RequestPriority::Normal,
        deadline_ms: None,
    };

    // Generate template
    let response = fog_node.generate_template(&request).unwrap();

    assert_eq!(response.request_id, "req1");
    assert!(!response.generated_content.is_empty());
    assert!(response.generation_time_ms >= 0);

    // Second request should be cached
    let response2 = fog_node.generate_template(&request).unwrap();
    assert!(response2.cache_hit);
}

#[test]
fn test_edge_device_capabilities() {
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
        capabilities.clone(),
        qos_metrics.clone(),
    );

    assert!(device.can_generate_locally());

    // Test with constrained network
    let constrained_qos = QosMetrics {
        bandwidth_kbps: 10,
        latency_ms: 200,
        packet_loss_rate: 0.2,
        jitter_ms: 50,
        battery_level: Some(0.1),
    };

    let constrained_device = EdgeDevice::new(
        NodeId::new("edge2"),
        capabilities,
        constrained_qos,
    );

    assert!(!constrained_device.can_generate_locally());
}

#[test]
fn test_federation_protocol_initialization() {
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

    let mut protocol = FederationProtocol::new(
        NodeId::new("node1"),
        FederationConfig::default(),
        capabilities,
        qos_metrics,
    );

    protocol.initialize().unwrap();

    let stats = protocol.get_statistics();
    assert_eq!(stats.node_id.0, "node1");
    assert_eq!(stats.total_triples, 0);
}

#[test]
fn test_federation_sync_request_response() {
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

    let mut protocol = FederationProtocol::new(
        NodeId::new("node1"),
        FederationConfig::default(),
        capabilities,
        qos_metrics,
    );

    protocol.initialize().unwrap();

    // Create a sync request
    let request = SyncRequest {
        request_id: "sync-1".to_string(),
        source_node: NodeId::new("node2"),
        target_node: protocol.node_id.clone(),
        vector_clock: VectorClock::new(),
        sync_type: SyncType::Delta,
        compression: CompressionStrategy::Hybrid,
    };

    // Handle the request
    let response = protocol.handle_sync_request(request).unwrap();

    assert!(response.success);
    assert_eq!(response.request_id, "sync-1");
}

#[test]
fn test_end_to_end_federation_scenario() {
    // Create three nodes: cloud, fog, and edge
    let capabilities_cloud = DeviceCapabilities {
        cpu_cores: 32,
        memory_mb: 32768,
        storage_mb: 1048576,
        can_generate_templates: true,
        max_ontology_size_mb: 10000,
    };

    let capabilities_fog = DeviceCapabilities {
        cpu_cores: 8,
        memory_mb: 8192,
        storage_mb: 102400,
        can_generate_templates: true,
        max_ontology_size_mb: 1000,
    };

    let capabilities_edge = DeviceCapabilities {
        cpu_cores: 2,
        memory_mb: 1024,
        storage_mb: 8192,
        can_generate_templates: true,
        max_ontology_size_mb: 100,
    };

    let qos_high = QosMetrics {
        bandwidth_kbps: 10000,
        latency_ms: 5,
        packet_loss_rate: 0.001,
        jitter_ms: 1,
        battery_level: None, // Powered device
    };

    let qos_medium = QosMetrics {
        bandwidth_kbps: 1000,
        latency_ms: 20,
        packet_loss_rate: 0.01,
        jitter_ms: 5,
        battery_level: Some(0.8),
    };

    let qos_low = QosMetrics {
        bandwidth_kbps: 100,
        latency_ms: 50,
        packet_loss_rate: 0.05,
        jitter_ms: 10,
        battery_level: Some(0.5),
    };

    // Create knowledge graphs
    let mut kg_cloud = DistributedKnowledgeGraph::new(
        NodeId::new("cloud1"),
        MergeStrategy::LastWriteWins,
    );

    let mut kg_fog = DistributedKnowledgeGraph::new(
        NodeId::new("fog1"),
        MergeStrategy::LastWriteWins,
    );

    let mut kg_edge = DistributedKnowledgeGraph::new(
        NodeId::new("edge1"),
        MergeStrategy::LastWriteWins,
    );

    // Add triples to cloud
    let triple1 = RdfTriple {
        subject: "ex:Service".to_string(),
        predicate: "rdf:type".to_string(),
        object: "ex:MicroService".to_string(),
        graph: None,
    };

    kg_cloud.add_triple(triple1.clone());

    // Synchronize cloud -> fog
    kg_fog.merge(&kg_cloud);
    assert_eq!(kg_fog.size(), 1);

    // Add additional triple at fog level
    let triple2 = RdfTriple {
        subject: "ex:EdgeDevice".to_string(),
        predicate: "rdf:type".to_string(),
        object: "ex:IoTDevice".to_string(),
        graph: None,
    };

    kg_fog.add_triple(triple2.clone());
    assert_eq!(kg_fog.size(), 2);

    // Synchronize fog -> edge
    kg_edge.merge(&kg_fog);
    assert_eq!(kg_edge.size(), 2);

    // Verify all nodes have consistent state
    assert_eq!(kg_cloud.size(), 1);
    assert_eq!(kg_fog.size(), 2);
    assert_eq!(kg_edge.size(), 2);
}
