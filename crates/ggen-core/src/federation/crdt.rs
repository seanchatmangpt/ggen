//! Conflict-Free Replicated Data Type (CRDT) Extensions for Distributed Knowledge Graphs
//!
//! This module implements CRDT data structures specifically designed for RDF knowledge graphs:
//! - OR-Set (Observed-Remove Set) for RDF triples
//! - LWW-Element-Set (Last-Write-Wins Element Set)
//! - Multi-Value Register for conflicting values
//! - Causal consistency using vector clocks

use super::{NodeId, VectorClock};
use crate::federation::rdf_delta::{RdfTriple, RdfDelta, DeltaOperation};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::time::SystemTime;

/// CRDT operation for distributed knowledge graph
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CrdtOperation {
    /// Add a triple with unique tag
    AddTriple {
        triple: RdfTriple,
        tag: OperationTag,
    },
    /// Remove a triple with specific tag
    RemoveTriple {
        triple: RdfTriple,
        tag: OperationTag,
    },
    /// Update a triple (combines add and remove)
    UpdateTriple {
        old_triple: RdfTriple,
        new_triple: RdfTriple,
        tag: OperationTag,
    },
}

/// Unique tag for CRDT operations (node ID + timestamp)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct OperationTag {
    pub node_id: NodeId,
    pub timestamp: u64,
    pub sequence: u64,
}

impl OperationTag {
    pub fn new(node_id: NodeId, sequence: u64) -> Self {
        let timestamp = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_micros() as u64;

        Self {
            node_id,
            timestamp,
            sequence,
        }
    }

    /// Compare tags for Last-Write-Wins semantics
    pub fn wins_over(&self, other: &OperationTag) -> bool {
        if self.timestamp != other.timestamp {
            self.timestamp > other.timestamp
        } else if self.node_id != other.node_id {
            // Deterministic tie-breaking using node ID
            self.node_id.0 > other.node_id.0
        } else {
            self.sequence > other.sequence
        }
    }
}

/// Merge strategy for conflicting updates
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MergeStrategy {
    /// Last-Write-Wins: Keep the most recent update
    LastWriteWins,
    /// Keep all conflicting values (multi-value register)
    KeepAll,
    /// Custom merge function (application-specific)
    Custom,
}

/// OR-Set CRDT for RDF triples
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrSet {
    /// Map from triple to set of add tags
    elements: HashMap<RdfTriple, HashSet<OperationTag>>,
    /// Tombstones: removed tags
    tombstones: HashMap<RdfTriple, HashSet<OperationTag>>,
}

impl OrSet {
    pub fn new() -> Self {
        Self {
            elements: HashMap::new(),
            tombstones: HashMap::new(),
        }
    }

    /// Add a triple with a unique tag
    pub fn add(&mut self, triple: RdfTriple, tag: OperationTag) {
        self.elements
            .entry(triple)
            .or_insert_with(HashSet::new)
            .insert(tag);
    }

    /// Remove a triple (marks all current tags as tombstones)
    pub fn remove(&mut self, triple: &RdfTriple) {
        if let Some(tags) = self.elements.get(triple) {
            let tombstone_set = self.tombstones
                .entry(triple.clone())
                .or_insert_with(HashSet::new);

            for tag in tags {
                tombstone_set.insert(tag.clone());
            }
        }
    }

    /// Remove a specific tag
    pub fn remove_tag(&mut self, triple: &RdfTriple, tag: &OperationTag) {
        self.tombstones
            .entry(triple.clone())
            .or_insert_with(HashSet::new)
            .insert(tag.clone());
    }

    /// Check if triple exists (not all tags are tombstoned)
    pub fn contains(&self, triple: &RdfTriple) -> bool {
        if let Some(add_tags) = self.elements.get(triple) {
            let tombstone_tags = self.tombstones.get(triple);

            // At least one add tag that's not tombstoned
            add_tags.iter().any(|tag| {
                tombstone_tags.map_or(true, |ts| !ts.contains(tag))
            })
        } else {
            false
        }
    }

    /// Get all triples in the set
    pub fn triples(&self) -> Vec<RdfTriple> {
        self.elements
            .keys()
            .filter(|triple| self.contains(triple))
            .cloned()
            .collect()
    }

    /// Merge another OR-Set into this one
    pub fn merge(&mut self, other: &OrSet) {
        // Merge elements
        for (triple, tags) in &other.elements {
            let entry = self.elements.entry(triple.clone()).or_insert_with(HashSet::new);
            entry.extend(tags.iter().cloned());
        }

        // Merge tombstones
        for (triple, tags) in &other.tombstones {
            let entry = self.tombstones.entry(triple.clone()).or_insert_with(HashSet::new);
            entry.extend(tags.iter().cloned());
        }
    }

    pub fn size(&self) -> usize {
        self.triples().len()
    }
}

impl Default for OrSet {
    fn default() -> Self {
        Self::new()
    }
}

/// LWW-Element-Set CRDT for RDF triples
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LwwElementSet {
    /// Map from triple to its last operation tag
    elements: HashMap<RdfTriple, (OperationTag, bool)>, // (tag, is_present)
}

impl LwwElementSet {
    pub fn new() -> Self {
        Self {
            elements: HashMap::new(),
        }
    }

    /// Add a triple
    pub fn add(&mut self, triple: RdfTriple, tag: OperationTag) {
        let should_update = self.elements
            .get(&triple)
            .map_or(true, |(existing_tag, _)| tag.wins_over(existing_tag));

        if should_update {
            self.elements.insert(triple, (tag, true));
        }
    }

    /// Remove a triple
    pub fn remove(&mut self, triple: &RdfTriple, tag: OperationTag) {
        let should_update = self.elements
            .get(triple)
            .map_or(true, |(existing_tag, _)| tag.wins_over(existing_tag));

        if should_update {
            self.elements.insert(triple.clone(), (tag, false));
        }
    }

    /// Check if triple is present
    pub fn contains(&self, triple: &RdfTriple) -> bool {
        self.elements
            .get(triple)
            .map_or(false, |(_, is_present)| *is_present)
    }

    /// Get all present triples
    pub fn triples(&self) -> Vec<RdfTriple> {
        self.elements
            .iter()
            .filter(|(_, (_, is_present))| *is_present)
            .map(|(triple, _)| triple.clone())
            .collect()
    }

    /// Merge another LWW-Element-Set
    pub fn merge(&mut self, other: &LwwElementSet) {
        for (triple, (tag, is_present)) in &other.elements {
            let should_update = self.elements
                .get(triple)
                .map_or(true, |(existing_tag, _)| tag.wins_over(existing_tag));

            if should_update {
                self.elements.insert(triple.clone(), (tag.clone(), *is_present));
            }
        }
    }

    pub fn size(&self) -> usize {
        self.triples().len()
    }
}

impl Default for LwwElementSet {
    fn default() -> Self {
        Self::new()
    }
}

/// Multi-Value Register for handling conflicting values
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MultiValueRegister {
    values: HashMap<OperationTag, RdfTriple>,
    vector_clock: VectorClock,
}

impl MultiValueRegister {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            vector_clock: VectorClock::new(),
        }
    }

    /// Set value with tag and vector clock
    pub fn set(&mut self, triple: RdfTriple, tag: OperationTag, clock: VectorClock) {
        // Remove values that are causally dominated
        self.values.retain(|existing_tag, _| {
            // Keep if concurrent or newer
            !self.vector_clock.happens_before(&clock)
        });

        self.values.insert(tag, triple);
        self.vector_clock.merge(&clock);
    }

    /// Get all concurrent values
    pub fn get(&self) -> Vec<&RdfTriple> {
        self.values.values().collect()
    }

    /// Merge with another register
    pub fn merge(&mut self, other: &MultiValueRegister) {
        for (tag, triple) in &other.values {
            self.values.insert(tag.clone(), triple.clone());
        }
        self.vector_clock.merge(&other.vector_clock);

        // Remove dominated values
        let final_clock = self.vector_clock.clone();
        self.values.retain(|_, _| true); // Simplified - would need per-value clocks
    }

    pub fn has_conflicts(&self) -> bool {
        self.values.len() > 1
    }
}

impl Default for MultiValueRegister {
    fn default() -> Self {
        Self::new()
    }
}

/// Distributed Knowledge Graph using CRDTs
pub struct DistributedKnowledgeGraph {
    pub node_id: NodeId,
    pub triples: OrSet,
    pub lww_triples: LwwElementSet,
    pub merge_strategy: MergeStrategy,
    pub vector_clock: VectorClock,
    pub operation_sequence: u64,
}

impl DistributedKnowledgeGraph {
    pub fn new(node_id: NodeId, merge_strategy: MergeStrategy) -> Self {
        Self {
            node_id,
            triples: OrSet::new(),
            lww_triples: LwwElementSet::new(),
            merge_strategy,
            vector_clock: VectorClock::new(),
            operation_sequence: 0,
        }
    }

    /// Apply a CRDT operation
    pub fn apply_operation(&mut self, operation: CrdtOperation) -> Result<(), String> {
        match operation {
            CrdtOperation::AddTriple { triple, tag } => {
                match self.merge_strategy {
                    MergeStrategy::LastWriteWins => {
                        self.lww_triples.add(triple, tag);
                    }
                    _ => {
                        self.triples.add(triple, tag);
                    }
                }
            }
            CrdtOperation::RemoveTriple { triple, tag } => {
                match self.merge_strategy {
                    MergeStrategy::LastWriteWins => {
                        self.lww_triples.remove(&triple, tag);
                    }
                    _ => {
                        self.triples.remove_tag(&triple, &tag);
                    }
                }
            }
            CrdtOperation::UpdateTriple { old_triple, new_triple, tag } => {
                match self.merge_strategy {
                    MergeStrategy::LastWriteWins => {
                        self.lww_triples.remove(&old_triple, tag.clone());
                        self.lww_triples.add(new_triple, tag);
                    }
                    _ => {
                        self.triples.remove(&old_triple);
                        self.triples.add(new_triple, tag);
                    }
                }
            }
        }
        Ok(())
    }

    /// Add a new triple
    pub fn add_triple(&mut self, triple: RdfTriple) -> CrdtOperation {
        self.operation_sequence += 1;
        self.vector_clock.increment(&self.node_id);

        let tag = OperationTag::new(self.node_id.clone(), self.operation_sequence);
        let operation = CrdtOperation::AddTriple { triple, tag };

        self.apply_operation(operation.clone()).unwrap();
        operation
    }

    /// Remove a triple
    pub fn remove_triple(&mut self, triple: RdfTriple) -> CrdtOperation {
        self.operation_sequence += 1;
        self.vector_clock.increment(&self.node_id);

        let tag = OperationTag::new(self.node_id.clone(), self.operation_sequence);
        let operation = CrdtOperation::RemoveTriple { triple, tag };

        self.apply_operation(operation.clone()).unwrap();
        operation
    }

    /// Update a triple
    pub fn update_triple(&mut self, old_triple: RdfTriple, new_triple: RdfTriple) -> CrdtOperation {
        self.operation_sequence += 1;
        self.vector_clock.increment(&self.node_id);

        let tag = OperationTag::new(self.node_id.clone(), self.operation_sequence);
        let operation = CrdtOperation::UpdateTriple {
            old_triple,
            new_triple,
            tag,
        };

        self.apply_operation(operation.clone()).unwrap();
        operation
    }

    /// Merge another distributed knowledge graph
    pub fn merge(&mut self, other: &DistributedKnowledgeGraph) {
        match self.merge_strategy {
            MergeStrategy::LastWriteWins => {
                self.lww_triples.merge(&other.lww_triples);
            }
            _ => {
                self.triples.merge(&other.triples);
            }
        }
        self.vector_clock.merge(&other.vector_clock);
    }

    /// Get all triples
    pub fn get_triples(&self) -> Vec<RdfTriple> {
        match self.merge_strategy {
            MergeStrategy::LastWriteWins => self.lww_triples.triples(),
            _ => self.triples.triples(),
        }
    }

    /// Convert to RDF delta
    pub fn to_delta(&self, target_version: String) -> RdfDelta {
        let mut delta = RdfDelta::new(
            format!("{}-{}", self.node_id.0, self.operation_sequence),
            target_version,
        );

        for triple in self.get_triples() {
            delta.add_insert(triple);
        }

        delta
    }

    pub fn size(&self) -> usize {
        self.get_triples().len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_operation_tag_ordering() {
        let node1 = NodeId::new("node1");
        let node2 = NodeId::new("node2");

        let tag1 = OperationTag {
            node_id: node1.clone(),
            timestamp: 100,
            sequence: 1,
        };

        let tag2 = OperationTag {
            node_id: node1.clone(),
            timestamp: 200,
            sequence: 1,
        };

        assert!(tag2.wins_over(&tag1));
        assert!(!tag1.wins_over(&tag2));
    }

    #[test]
    fn test_or_set_add_remove() {
        let mut set = OrSet::new();
        let triple = RdfTriple {
            subject: "ex:s1".to_string(),
            predicate: "ex:p1".to_string(),
            object: "ex:o1".to_string(),
            graph: None,
        };

        let tag = OperationTag::new(NodeId::new("node1"), 1);
        set.add(triple.clone(), tag);

        assert!(set.contains(&triple));
        assert_eq!(set.size(), 1);

        set.remove(&triple);
        assert!(!set.contains(&triple));
        assert_eq!(set.size(), 0);
    }

    #[test]
    fn test_or_set_merge() {
        let mut set1 = OrSet::new();
        let mut set2 = OrSet::new();

        let triple1 = RdfTriple {
            subject: "ex:s1".to_string(),
            predicate: "ex:p1".to_string(),
            object: "ex:o1".to_string(),
            graph: None,
        };

        let triple2 = RdfTriple {
            subject: "ex:s2".to_string(),
            predicate: "ex:p2".to_string(),
            object: "ex:o2".to_string(),
            graph: None,
        };

        set1.add(triple1.clone(), OperationTag::new(NodeId::new("node1"), 1));
        set2.add(triple2.clone(), OperationTag::new(NodeId::new("node2"), 1));

        set1.merge(&set2);

        assert!(set1.contains(&triple1));
        assert!(set1.contains(&triple2));
        assert_eq!(set1.size(), 2);
    }

    #[test]
    fn test_lww_element_set() {
        let mut set = LwwElementSet::new();
        let triple = RdfTriple {
            subject: "ex:s1".to_string(),
            predicate: "ex:p1".to_string(),
            object: "ex:o1".to_string(),
            graph: None,
        };

        let tag1 = OperationTag {
            node_id: NodeId::new("node1"),
            timestamp: 100,
            sequence: 1,
        };

        let tag2 = OperationTag {
            node_id: NodeId::new("node1"),
            timestamp: 200,
            sequence: 2,
        };

        set.add(triple.clone(), tag1.clone());
        assert!(set.contains(&triple));

        set.remove(&triple, tag2);
        assert!(!set.contains(&triple));
    }

    #[test]
    fn test_distributed_knowledge_graph() {
        let mut graph = DistributedKnowledgeGraph::new(
            NodeId::new("node1"),
            MergeStrategy::LastWriteWins,
        );

        let triple = RdfTriple {
            subject: "ex:s1".to_string(),
            predicate: "ex:p1".to_string(),
            object: "ex:o1".to_string(),
            graph: None,
        };

        graph.add_triple(triple.clone());
        assert_eq!(graph.size(), 1);

        graph.remove_triple(triple);
        assert_eq!(graph.size(), 0);
    }

    #[test]
    fn test_distributed_knowledge_graph_merge() {
        let mut graph1 = DistributedKnowledgeGraph::new(
            NodeId::new("node1"),
            MergeStrategy::LastWriteWins,
        );

        let mut graph2 = DistributedKnowledgeGraph::new(
            NodeId::new("node2"),
            MergeStrategy::LastWriteWins,
        );

        let triple1 = RdfTriple {
            subject: "ex:s1".to_string(),
            predicate: "ex:p1".to_string(),
            object: "ex:o1".to_string(),
            graph: None,
        };

        let triple2 = RdfTriple {
            subject: "ex:s2".to_string(),
            predicate: "ex:p2".to_string(),
            object: "ex:o2".to_string(),
            graph: None,
        };

        graph1.add_triple(triple1);
        graph2.add_triple(triple2);

        graph1.merge(&graph2);
        assert_eq!(graph1.size(), 2);
    }
}
