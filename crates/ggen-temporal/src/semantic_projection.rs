//! Distributed semantic projections with causal consistency guarantees
//!
//! This module provides a distributed system for maintaining semantic projections
//! (materialized views) of temporal graphs across multiple nodes while guaranteeing
//! causal consistency using vector clocks.
//!
//! ## Key Concepts
//!
//! - **Semantic Projection**: A materialized view of the temporal graph optimized for specific queries
//! - **Causal Consistency**: Ensures causally related events are seen in the correct order
//! - **Distributed Coordination**: Multiple nodes can maintain projections cooperatively
//! - **Eventual Consistency**: All nodes converge to the same state given the same events
//!
//! ## Example
//!
//! ```rust
//! use ggen_temporal::semantic_projection::*;
//! use ggen_temporal::event_sourcing::EventStore;
//! use ggen_temporal::vector_clock::VectorClock;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Create a projection node
//! let mut node = ProjectionNode::new("node-1".to_string(), EventStore::new());
//!
//! // Create a semantic projection (e.g., for a specific entity type)
//! let projection = SemanticProjection::new(
//!     "user_projection".to_string(),
//!     "SELECT ?user ?name WHERE { ?user a :User ; :name ?name }".to_string()
//! );
//!
//! node.add_projection(projection);
//!
//! // Process events and maintain projections
//! node.process_events().await?;
//! # Ok(())
//! # }
//! ```

use crate::event_sourcing::{Event, EventId, EventStore, EventStream};
use crate::ontology_4d::TemporalGraph;
use crate::vector_clock::{NodeId, VectorClock, VectorTime};
use crate::{Result, TemporalError};
use async_trait::async_trait;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// A semantic projection (materialized view) of the temporal graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SemanticProjection {
    /// Unique projection ID
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// SPARQL query defining this projection
    pub query: String,
    /// Vector time of last update
    pub last_update: VectorTime,
    /// Events that have been applied to this projection
    pub applied_events: HashSet<EventId>,
}

impl SemanticProjection {
    #[must_use]
    pub fn new(name: String, query: String) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            name,
            query,
            last_update: VectorTime::new(),
            applied_events: HashSet::new(),
        }
    }

    /// Check if an event has been applied
    #[must_use]
    pub fn has_applied(&self, event_id: &EventId) -> bool {
        self.applied_events.contains(event_id)
    }

    /// Mark an event as applied
    pub fn mark_applied(&mut self, event_id: EventId, vector_time: VectorTime) {
        self.applied_events.insert(event_id);
        self.last_update.merge(&vector_time);
    }

    /// Check if this projection can apply an event (causal consistency check)
    #[must_use]
    pub fn can_apply(&self, event: &Event) -> bool {
        // Check if all causal dependencies have been applied
        event
            .causal_dependencies
            .iter()
            .all(|dep_id| self.applied_events.contains(dep_id))
    }
}

/// View of a semantic projection's data
#[derive(Debug, Clone)]
pub struct ProjectionView {
    /// Projection ID
    pub projection_id: String,
    /// Query results
    pub data: Vec<HashMap<String, String>>,
    /// Vector time of this view
    pub vector_time: VectorTime,
    /// Timestamp of view creation
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl ProjectionView {
    #[must_use]
    pub fn new(projection_id: String, data: Vec<HashMap<String, String>>, vector_time: VectorTime) -> Self {
        Self {
            projection_id,
            data,
            vector_time,
            timestamp: chrono::Utc::now(),
        }
    }

    /// Check if this view is causally consistent with a vector time
    #[must_use]
    pub fn is_consistent_with(&self, other_time: &VectorTime) -> bool {
        !self.vector_time.is_concurrent(other_time)
    }
}

/// Causal consistency checker for distributed projections
pub struct CausalConsistency {
    /// Node ID
    node_id: NodeId,
    /// Vector clock for this node
    clock: VectorClock,
    /// Known vector times from other nodes
    peer_clocks: Arc<DashMap<NodeId, VectorTime>>,
}

impl CausalConsistency {
    #[must_use]
    pub fn new(node_id: NodeId) -> Self {
        Self {
            clock: VectorClock::new(node_id.clone()),
            node_id,
            peer_clocks: Arc::new(DashMap::new()),
        }
    }

    /// Get current vector time
    #[must_use]
    pub fn current_time(&self) -> VectorTime {
        self.clock.timestamp()
    }

    /// Tick the local clock
    pub fn tick(&mut self) {
        self.clock.tick();
    }

    /// Update with a received vector time
    pub fn update(&mut self, received: &VectorTime) {
        self.clock.merge(received);
    }

    /// Update peer clock information
    pub fn update_peer(&self, peer_id: NodeId, peer_time: VectorTime) {
        self.peer_clocks.insert(peer_id, peer_time);
    }

    /// Check if an event can be safely applied (all dependencies satisfied)
    #[must_use]
    pub fn can_apply_event(&self, event: &Event) -> bool {
        // Check if event's vector time is compatible
        let current_time = self.current_time();

        // Event can be applied if it happened before or is concurrent
        !event.vector_time.happened_before(&current_time)
            || event.vector_time == current_time
    }

    /// Check if two nodes are causally consistent
    #[must_use]
    pub fn is_consistent_with_peer(&self, peer_id: &NodeId) -> bool {
        if let Some(peer_time) = self.peer_clocks.get(peer_id) {
            !self.current_time().is_concurrent(&peer_time)
        } else {
            false
        }
    }

    /// Get all known peer nodes
    #[must_use]
    pub fn known_peers(&self) -> Vec<NodeId> {
        self.peer_clocks
            .iter()
            .map(|entry| entry.key().clone())
            .collect()
    }
}

/// A node in the distributed projection system
pub struct ProjectionNode {
    /// Node ID
    node_id: NodeId,
    /// Event store
    event_store: EventStore,
    /// Temporal graph for this node
    temporal_graph: TemporalGraph,
    /// Active projections
    projections: Arc<DashMap<String, SemanticProjection>>,
    /// Causal consistency manager
    causal_consistency: CausalConsistency,
    /// Pending events (waiting for dependencies)
    pending_events: Arc<DashMap<EventId, Event>>,
}

impl ProjectionNode {
    /// Create a new projection node
    pub fn new(node_id: NodeId, event_store: EventStore) -> Result<Self> {
        let temporal_graph = TemporalGraph::new()?;

        Ok(Self {
            node_id: node_id.clone(),
            event_store,
            temporal_graph,
            projections: Arc::new(DashMap::new()),
            causal_consistency: CausalConsistency::new(node_id),
            pending_events: Arc::new(DashMap::new()),
        })
    }

    /// Get node ID
    #[must_use]
    pub fn node_id(&self) -> &NodeId {
        &self.node_id
    }

    /// Add a semantic projection
    pub fn add_projection(&self, projection: SemanticProjection) {
        self.projections
            .insert(projection.id.clone(), projection);
    }

    /// Remove a projection
    pub fn remove_projection(&self, projection_id: &str) -> Option<SemanticProjection> {
        self.projections.remove(projection_id).map(|(_, v)| v)
    }

    /// Get a projection view
    pub async fn get_projection_view(&self, projection_id: &str) -> Result<ProjectionView> {
        let projection = self
            .projections
            .get(projection_id)
            .ok_or_else(|| {
                TemporalError::InvalidQuery(format!("Projection not found: {projection_id}"))
            })?;

        // Execute the projection query on the temporal graph
        let data = self
            .temporal_graph
            .base_graph()
            .query(&projection.query)
            .map_err(|e| TemporalError::GraphError(e.to_string()))?;

        Ok(ProjectionView::new(
            projection_id.to_string(),
            data,
            projection.last_update.clone(),
        ))
    }

    /// Apply an event to all projections
    async fn apply_event_to_projections(&mut self, event: &Event) -> Result<()> {
        // Tick local clock
        self.causal_consistency.tick();

        // Update each projection that can accept this event
        for mut projection_entry in self.projections.iter_mut() {
            let projection = projection_entry.value_mut();

            if projection.can_apply(event) && !projection.has_applied(&event.id) {
                // Apply event to projection
                projection.mark_applied(event.id.clone(), event.vector_time.clone());

                // Update causal consistency tracker
                self.causal_consistency.update(&event.vector_time);
            }
        }

        Ok(())
    }

    /// Process all pending events that can now be applied
    async fn process_pending_events(&mut self) -> Result<()> {
        let mut applied_events = Vec::new();

        // Find events that can now be applied
        for entry in self.pending_events.iter() {
            let event = entry.value();

            // Check if all projections can apply this event
            let can_apply = self
                .projections
                .iter()
                .all(|proj| proj.value().can_apply(event));

            if can_apply {
                applied_events.push(entry.key().clone());
            }
        }

        // Apply and remove from pending
        for event_id in applied_events {
            if let Some((_, event)) = self.pending_events.remove(&event_id) {
                self.apply_event_to_projections(&event).await?;
            }
        }

        Ok(())
    }

    /// Receive and process an event
    pub async fn receive_event(&mut self, event: Event) -> Result<()> {
        // Check if event can be applied immediately
        let can_apply = self
            .projections
            .iter()
            .all(|proj| proj.value().can_apply(&event));

        if can_apply {
            self.apply_event_to_projections(&event).await?;
        } else {
            // Add to pending events
            self.pending_events.insert(event.id.clone(), event);
        }

        // Try to process any pending events
        self.process_pending_events().await?;

        Ok(())
    }

    /// Process all events from the event store
    pub async fn process_events(&mut self) -> Result<()> {
        let stream = self.event_store.get_all_events();

        for event in stream.events() {
            self.receive_event(event.clone()).await?;
        }

        Ok(())
    }

    /// Synchronize with a peer node
    pub async fn sync_with_peer(
        &mut self,
        peer_id: NodeId,
        peer_time: VectorTime,
    ) -> Result<()> {
        // Update peer clock information
        self.causal_consistency.update_peer(peer_id, peer_time);

        // Process any events that can now be applied
        self.process_pending_events().await?;

        Ok(())
    }

    /// Get current vector time
    #[must_use]
    pub fn current_time(&self) -> VectorTime {
        self.causal_consistency.current_time()
    }

    /// Get all projections
    #[must_use]
    pub fn get_projections(&self) -> Vec<String> {
        self.projections
            .iter()
            .map(|entry| entry.key().clone())
            .collect()
    }

    /// Check if node is causally consistent with a peer
    #[must_use]
    pub fn is_consistent_with_peer(&self, peer_id: &NodeId) -> bool {
        self.causal_consistency.is_consistent_with_peer(peer_id)
    }
}

/// Coordinator for managing multiple projection nodes
pub struct ProjectionCoordinator {
    /// All managed nodes
    nodes: HashMap<NodeId, ProjectionNode>,
}

impl ProjectionCoordinator {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            nodes: HashMap::new(),
        }
    }

    /// Add a node to the coordinator
    pub fn add_node(&mut self, node: ProjectionNode) {
        self.nodes.insert(node.node_id().clone(), node);
    }

    /// Broadcast an event to all nodes
    pub async fn broadcast_event(&mut self, event: Event) -> Result<()> {
        for node in self.nodes.values_mut() {
            node.receive_event(event.clone()).await?;
        }
        Ok(())
    }

    /// Synchronize all nodes
    pub async fn synchronize_all(&mut self) -> Result<()> {
        // Collect all node times
        let node_times: Vec<(NodeId, VectorTime)> = self
            .nodes
            .iter()
            .map(|(id, node)| (id.clone(), node.current_time()))
            .collect();

        // Sync each node with all peer times
        for (node_id, node) in &mut self.nodes {
            for (peer_id, peer_time) in &node_times {
                if peer_id != node_id {
                    node.sync_with_peer(peer_id.clone(), peer_time.clone())
                        .await?;
                }
            }
        }

        Ok(())
    }

    /// Get a node by ID
    #[must_use]
    pub fn get_node(&self, node_id: &NodeId) -> Option<&ProjectionNode> {
        self.nodes.get(node_id)
    }

    /// Get a mutable reference to a node
    pub fn get_node_mut(&mut self, node_id: &NodeId) -> Option<&mut ProjectionNode> {
        self.nodes.get_mut(node_id)
    }
}

impl Default for ProjectionCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::event_sourcing::{EventData, EventType};

    #[test]
    fn test_semantic_projection_creation() {
        let projection = SemanticProjection::new(
            "test_projection".to_string(),
            "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        );

        assert_eq!(projection.name, "test_projection");
        assert!(projection.applied_events.is_empty());
    }

    #[test]
    fn test_causal_consistency() {
        let mut consistency = CausalConsistency::new("node-1".to_string());

        consistency.tick();
        let time1 = consistency.current_time();
        assert_eq!(time1.get("node-1"), 1);

        consistency.tick();
        let time2 = consistency.current_time();
        assert_eq!(time2.get("node-1"), 2);
    }

    #[tokio::test]
    async fn test_projection_node() {
        let store = EventStore::new();
        let node = ProjectionNode::new("node-1".to_string(), store);

        assert!(node.is_ok());
        let node = node.unwrap();
        assert_eq!(node.node_id(), "node-1");
    }

    #[tokio::test]
    async fn test_projection_view() {
        let store = EventStore::new();
        let node = ProjectionNode::new("node-1".to_string(), store).unwrap();

        let projection = SemanticProjection::new(
            "test".to_string(),
            "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        );

        let projection_id = projection.id.clone();
        node.add_projection(projection);

        let view = node.get_projection_view(&projection_id).await;
        assert!(view.is_ok());
    }

    #[tokio::test]
    async fn test_coordinator() {
        let store1 = EventStore::new();
        let store2 = EventStore::new();

        let node1 = ProjectionNode::new("node-1".to_string(), store1).unwrap();
        let node2 = ProjectionNode::new("node-2".to_string(), store2).unwrap();

        let mut coordinator = ProjectionCoordinator::new();
        coordinator.add_node(node1);
        coordinator.add_node(node2);

        assert!(coordinator.get_node(&"node-1".to_string()).is_some());
        assert!(coordinator.get_node(&"node-2".to_string()).is_some());
    }

    #[test]
    fn test_projection_can_apply() {
        let mut projection = SemanticProjection::new(
            "test".to_string(),
            "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
        );

        let event1 = Event::new(
            "entity-1".to_string(),
            EventType::Created,
            EventData::Custom {
                data: serde_json::json!({}),
            },
            VectorTime::new(),
        );

        // Event with no dependencies can be applied
        assert!(projection.can_apply(&event1));

        // Mark event1 as applied
        projection.mark_applied(event1.id.clone(), event1.vector_time.clone());

        // Event with satisfied dependencies can be applied
        let event2 = Event::new(
            "entity-2".to_string(),
            EventType::Updated,
            EventData::Custom {
                data: serde_json::json!({}),
            },
            VectorTime::new(),
        )
        .with_dependencies(vec![event1.id.clone()]);

        assert!(projection.can_apply(&event2));

        // Event with unsatisfied dependencies cannot be applied
        let event3 = Event::new(
            "entity-3".to_string(),
            EventType::Updated,
            EventData::Custom {
                data: serde_json::json!({}),
            },
            VectorTime::new(),
        )
        .with_dependencies(vec!["missing_event".to_string()]);

        assert!(!projection.can_apply(&event3));
    }
}
