//! Event-driven change detection and notification system

use crate::error::{GgenAiError, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{broadcast, RwLock};
use tracing::{debug, info, warn};

/// Types of graph changes that trigger regeneration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ChangeType {
    /// Node added to the graph
    NodeAdded,
    /// Node removed from the graph
    NodeRemoved,
    /// Node properties updated
    NodeUpdated,
    /// Edge added between nodes
    EdgeAdded,
    /// Edge removed between nodes
    EdgeRemoved,
    /// Edge properties updated
    EdgeUpdated,
    /// Ontology schema changed
    SchemaChanged,
    /// Template definition changed
    TemplateChanged,
}

/// Event representing a change in the knowledge graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChangeEvent {
    /// Unique event identifier
    pub id: String,
    /// Type of change
    pub change_type: ChangeType,
    /// Timestamp of the change
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Subject of the change (URI)
    pub subject: String,
    /// Optional predicate (for edge changes)
    pub predicate: Option<String>,
    /// Optional object (for edge changes)
    pub object: Option<String>,
    /// Changed properties
    pub properties: HashMap<String, serde_json::Value>,
    /// Source of the change (e.g., "api", "cli", "automation")
    pub source: String,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl ChangeEvent {
    /// Create a new change event
    pub fn new(
        change_type: ChangeType,
        subject: String,
        source: String,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            change_type,
            timestamp: chrono::Utc::now(),
            subject,
            predicate: None,
            object: None,
            properties: HashMap::new(),
            source,
            metadata: HashMap::new(),
        }
    }

    /// Create a node addition event
    pub fn node_added(uri: String, properties: HashMap<String, serde_json::Value>, source: String) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            change_type: ChangeType::NodeAdded,
            timestamp: chrono::Utc::now(),
            subject: uri,
            predicate: None,
            object: None,
            properties,
            source,
            metadata: HashMap::new(),
        }
    }

    /// Create an edge addition event
    pub fn edge_added(subject: String, predicate: String, object: String, source: String) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            change_type: ChangeType::EdgeAdded,
            timestamp: chrono::Utc::now(),
            subject,
            predicate: Some(predicate),
            object: Some(object),
            properties: HashMap::new(),
            source,
            metadata: HashMap::new(),
        }
    }

    /// Check if this event affects a specific subject
    pub fn affects(&self, uri: &str) -> bool {
        self.subject == uri
            || self.object.as_ref().map_or(false, |o| o == uri)
    }

    /// Get all affected URIs
    pub fn affected_uris(&self) -> Vec<&str> {
        let mut uris = vec![self.subject.as_str()];
        if let Some(obj) = &self.object {
            uris.push(obj.as_str());
        }
        uris
    }
}

/// Trait for receiving change event notifications
#[async_trait]
pub trait EventSubscriber: Send + Sync {
    /// Handle a change event
    async fn on_event(&self, event: &ChangeEvent) -> Result<()>;

    /// Optional filter - return true if subscriber wants this event
    fn filter(&self, _event: &ChangeEvent) -> bool {
        true
    }

    /// Get subscriber name for logging
    fn name(&self) -> &str;
}

/// Graph change notifier with pub/sub pattern
#[derive(Clone)]
pub struct GraphChangeNotifier {
    /// Broadcast channel for change events
    sender: Arc<broadcast::Sender<ChangeEvent>>,
    /// Registered subscribers
    subscribers: Arc<RwLock<Vec<Arc<dyn EventSubscriber>>>>,
    /// Event history (last N events)
    history: Arc<RwLock<Vec<ChangeEvent>>>,
    /// Maximum history size
    max_history: usize,
}

impl GraphChangeNotifier {
    /// Create a new change notifier
    pub fn new(channel_capacity: usize, max_history: usize) -> Self {
        let (sender, _) = broadcast::channel(channel_capacity);
        Self {
            sender: Arc::new(sender),
            subscribers: Arc::new(RwLock::new(Vec::new())),
            history: Arc::new(RwLock::new(Vec::new())),
            max_history,
        }
    }

    /// Publish a change event
    pub async fn publish(&self, event: ChangeEvent) -> Result<()> {
        info!(
            event_id = %event.id,
            change_type = ?event.change_type,
            subject = %event.subject,
            "Publishing change event"
        );

        // Add to history
        let mut history = self.history.write().await;
        history.push(event.clone());
        if history.len() > self.max_history {
            history.remove(0);
        }
        drop(history);

        // Send to broadcast channel
        if let Err(e) = self.sender.send(event.clone()) {
            warn!("No active receivers for event: {}", e);
        }

        // Notify subscribers
        let subscribers = self.subscribers.read().await;
        for subscriber in subscribers.iter() {
            if subscriber.filter(&event) {
                if let Err(e) = subscriber.on_event(&event).await {
                    warn!(
                        subscriber = subscriber.name(),
                        error = %e,
                        "Subscriber failed to process event"
                    );
                }
            }
        }

        Ok(())
    }

    /// Subscribe to change events
    pub fn subscribe(&self) -> broadcast::Receiver<ChangeEvent> {
        self.sender.subscribe()
    }

    /// Register a subscriber
    pub async fn register_subscriber(&self, subscriber: Arc<dyn EventSubscriber>) {
        info!(subscriber = subscriber.name(), "Registering event subscriber");
        let mut subscribers = self.subscribers.write().await;
        subscribers.push(subscriber);
    }

    /// Get event history
    pub async fn get_history(&self, limit: usize) -> Vec<ChangeEvent> {
        let history = self.history.read().await;
        let start = if history.len() > limit {
            history.len() - limit
        } else {
            0
        };
        history[start..].to_vec()
    }

    /// Get recent events affecting a specific subject
    pub async fn get_events_for(&self, uri: &str, limit: usize) -> Vec<ChangeEvent> {
        let history = self.history.read().await;
        history
            .iter()
            .filter(|e| e.affects(uri))
            .rev()
            .take(limit)
            .cloned()
            .collect()
    }

    /// Clear event history
    pub async fn clear_history(&self) {
        let mut history = self.history.write().await;
        history.clear();
        info!("Cleared event history");
    }
}

impl Default for GraphChangeNotifier {
    fn default() -> Self {
        Self::new(1000, 10000)
    }
}

/// Delta detector for identifying changes between graph states
pub struct DeltaDetector;

impl DeltaDetector {
    /// Detect changes between two graph states
    pub async fn detect_changes(
        old_state: &str,
        new_state: &str,
    ) -> Result<Vec<ChangeEvent>> {
        debug!("Detecting changes between graph states");
        let mut changes = Vec::new();

        // Parse both states into triples
        let old_triples = Self::parse_turtle_to_triples(old_state)?;
        let new_triples = Self::parse_turtle_to_triples(new_state)?;

        // Detect additions (triples in new but not in old)
        let additions = Self::detect_additions(&old_triples, &new_triples)?;
        changes.extend(additions);

        // Detect deletions (triples in old but not in new)
        let deletions = Self::detect_deletions(&old_triples, &new_triples)?;
        changes.extend(deletions);

        // Detect modifications (triples with same subject but different properties)
        let modifications = Self::detect_modifications(&old_triples, &new_triples)?;
        changes.extend(modifications);

        info!("Detected {} changes between graph states", changes.len());
        Ok(changes)
    }

    /// Check if a change affects specific template types
    pub fn affects_templates(event: &ChangeEvent, template_types: &[&str]) -> bool {
        // Check if change affects any of the specified template types
        for template_type in template_types {
            if event.subject.contains(template_type) {
                return true;
            }
        }
        false
    }

    /// Parse Turtle format to triples
    fn parse_turtle_to_triples(turtle: &str) -> Result<Vec<String>> {
        let mut triples = Vec::new();
        let lines: Vec<&str> = turtle.lines().collect();
        
        for line in lines {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') || line.starts_with('@') {
                continue;
            }
            
            if line.ends_with('.') {
                triples.push(line.to_string());
            }
        }
        
        Ok(triples)
    }

    /// Detect additions between old and new triples
    fn detect_additions(old_triples: &[String], new_triples: &[String]) -> Result<Vec<ChangeEvent>> {
        let mut changes = Vec::new();
        
        for triple in new_triples {
            if !old_triples.contains(triple) {
                changes.push(ChangeEvent {
                    id: uuid::Uuid::new_v4().to_string(),
                    change_type: ChangeType::NodeAdded,
                    subject: Self::extract_subject(triple),
                    predicate: Some(Self::extract_predicate(triple)),
                    object: Some(Self::extract_object(triple)),
                    timestamp: chrono::Utc::now(),
                    properties: std::collections::HashMap::new(),
                    source: "delta_detection".to_string(),
                });
            }
        }
        
        Ok(changes)
    }

    /// Detect deletions between old and new triples
    fn detect_deletions(old_triples: &[String], new_triples: &[String]) -> Result<Vec<ChangeEvent>> {
        let mut changes = Vec::new();
        
        for triple in old_triples {
            if !new_triples.contains(triple) {
                changes.push(ChangeEvent {
                    id: uuid::Uuid::new_v4().to_string(),
                    change_type: ChangeType::NodeRemoved,
                    subject: Self::extract_subject(triple),
                    predicate: Some(Self::extract_predicate(triple)),
                    object: Some(Self::extract_object(triple)),
                    timestamp: chrono::Utc::now(),
                    properties: std::collections::HashMap::new(),
                    source: "delta_detection".to_string(),
                });
            }
        }
        
        Ok(changes)
    }

    /// Detect modifications between old and new triples
    fn detect_modifications(old_triples: &[String], new_triples: &[String]) -> Result<Vec<ChangeEvent>> {
        let mut changes = Vec::new();
        
        // Group triples by subject
        let old_by_subject = Self::group_triples_by_subject(old_triples);
        let new_by_subject = Self::group_triples_by_subject(new_triples);
        
        for (subject, old_subject_triples) in &old_by_subject {
            if let Some(new_subject_triples) = new_by_subject.get(subject) {
                // Check for property changes
                for old_triple in old_subject_triples {
                    let predicate = Self::extract_predicate(old_triple);
                    let old_object = Self::extract_object(old_triple);
                    
                    // Find corresponding triple in new state
                    let new_triple = new_subject_triples.iter()
                        .find(|t| Self::extract_predicate(t) == predicate);
                    
                    if let Some(new_triple) = new_triple {
                        let new_object = Self::extract_object(new_triple);
                        if old_object != new_object {
                            changes.push(ChangeEvent {
                                id: uuid::Uuid::new_v4().to_string(),
                                change_type: ChangeType::NodeUpdated,
                                subject: subject.clone(),
                                predicate: Some(predicate.clone()),
                                object: Some(new_object.clone()),
                                timestamp: chrono::Utc::now(),
                                properties: {
                                    let mut meta = std::collections::HashMap::new();
                                    meta.insert("old_value".to_string(), serde_json::Value::String(old_object));
                                    meta
                                },
                                source: "delta_detection".to_string(),
                            });
                        }
                    }
                }
            }
        }
        
        Ok(changes)
    }

    /// Extract subject from triple
    fn extract_subject(triple: &str) -> String {
        triple.split_whitespace()
            .next()
            .unwrap_or("")
            .to_string()
    }

    /// Extract predicate from triple
    fn extract_predicate(triple: &str) -> String {
        triple.split_whitespace()
            .nth(1)
            .unwrap_or("")
            .to_string()
    }

    /// Extract object from triple
    fn extract_object(triple: &str) -> String {
        let parts: Vec<&str> = triple.split_whitespace().collect();
        if parts.len() >= 3 {
            parts[2..].join(" ").trim_end_matches('.').to_string()
        } else {
            String::new()
        }
    }

    /// Group triples by subject
    fn group_triples_by_subject(triples: &[String]) -> std::collections::HashMap<String, Vec<String>> {
        let mut grouped = std::collections::HashMap::new();
        
        for triple in triples {
            let subject = Self::extract_subject(triple);
            grouped.entry(subject).or_insert_with(Vec::new).push(triple.clone());
        }
        
        grouped
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_delta_detection() {
        let old_state = r#"
@prefix ex: <http://example.org/> .
ex:Person a owl:Class .
ex:Person ex:hasName "John" .
"#;

        let new_state = r#"
@prefix ex: <http://example.org/> .
ex:Person a owl:Class .
ex:Person ex:hasName "Jane" .
ex:Person ex:hasAge "25" .
"#;

        let changes = DeltaDetector::detect_changes(old_state, new_state).await.expect("Delta detection should succeed");
        
        // Should detect modification (name change) and addition (age)
        assert!(!changes.is_empty());
        assert!(changes.iter().any(|c| matches!(c.change_type, ChangeType::NodeUpdated)));
        assert!(changes.iter().any(|c| matches!(c.change_type, ChangeType::NodeAdded)));
    }

    #[tokio::test]
    async fn test_delta_detection_deletions() {
        let old_state = r#"
@prefix ex: <http://example.org/> .
ex:Person a owl:Class .
ex:Person ex:hasName "John" .
ex:Person ex:hasAge "25" .
"#;

        let new_state = r#"
@prefix ex: <http://example.org/> .
ex:Person a owl:Class .
ex:Person ex:hasName "John" .
"#;

        let changes = DeltaDetector::detect_changes(old_state, new_state).await.expect("Delta detection should succeed");
        
        // Should detect deletion (age property)
        assert!(!changes.is_empty());
        assert!(changes.iter().any(|c| matches!(c.change_type, ChangeType::NodeRemoved)));
    }

    #[test]
    fn test_affects_templates() {
        let event = ChangeEvent {
            id: "test".to_string(),
            change_type: ChangeType::NodeAdded,
            subject: "ex:UserTemplate".to_string(),
            predicate: Some("ex:hasProperty".to_string()),
            object: Some("ex:value".to_string()),
            timestamp: chrono::Utc::now(),
            properties: std::collections::HashMap::new(),
            source: "test".to_string(),
        };

        assert!(DeltaDetector::affects_templates(&event, &["UserTemplate"]));
        assert!(!DeltaDetector::affects_templates(&event, &["OtherTemplate"]));
    }

    #[test]
    fn test_triple_parsing() {
        let turtle = r#"
@prefix ex: <http://example.org/> .
ex:Person a owl:Class .
ex:Person ex:hasName "John" .
"#;

        let triples = DeltaDetector::parse_turtle_to_triples(turtle).expect("Parsing should succeed");
        assert_eq!(triples.len(), 2);
        assert!(triples.iter().any(|t| t.contains("ex:Person a owl:Class")));
        assert!(triples.iter().any(|t| t.contains("ex:Person ex:hasName \"John\"")));
    }

    struct TestSubscriber {
        name: String,
        events: Arc<RwLock<Vec<ChangeEvent>>>,
    }

    impl TestSubscriber {
        fn new(name: String) -> Self {
            Self {
                name,
                events: Arc::new(RwLock::new(Vec::new())),
            }
        }

        async fn get_events(&self) -> Vec<ChangeEvent> {
            self.events.read().await.clone()
        }
    }

    #[async_trait]
    impl EventSubscriber for TestSubscriber {
        async fn on_event(&self, event: &ChangeEvent) -> Result<()> {
            self.events.write().await.push(event.clone());
            Ok(())
        }

        fn name(&self) -> &str {
            &self.name
        }
    }

    #[tokio::test]
    async fn test_event_creation() {
        let event = ChangeEvent::node_added(
            "http://example.org/user/1".to_string(),
            HashMap::new(),
            "test".to_string(),
        );

        assert_eq!(event.change_type, ChangeType::NodeAdded);
        assert_eq!(event.subject, "http://example.org/user/1");
        assert_eq!(event.source, "test");
    }

    #[tokio::test]
    async fn test_notifier_publish_subscribe() {
        let notifier = GraphChangeNotifier::default();
        let mut receiver = notifier.subscribe();

        let event = ChangeEvent::node_added(
            "http://example.org/test".to_string(),
            HashMap::new(),
            "test".to_string(),
        );

        notifier.publish(event.clone()).await.unwrap();

        let received = receiver.recv().await.unwrap();
        assert_eq!(received.id, event.id);
        assert_eq!(received.subject, event.subject);
    }

    #[tokio::test]
    async fn test_subscriber_registration() {
        let notifier = GraphChangeNotifier::default();
        let subscriber = Arc::new(TestSubscriber::new("test-sub".to_string()));

        notifier.register_subscriber(subscriber.clone()).await;

        let event = ChangeEvent::node_added(
            "http://example.org/test".to_string(),
            HashMap::new(),
            "test".to_string(),
        );

        notifier.publish(event.clone()).await.unwrap();

        // Give subscriber time to process
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

        let events = subscriber.get_events().await;
        assert_eq!(events.len(), 1);
        assert_eq!(events[0].id, event.id);
    }

    #[tokio::test]
    async fn test_event_history() {
        let notifier = GraphChangeNotifier::new(100, 5);

        // Publish 10 events
        for i in 0..10 {
            let event = ChangeEvent::node_added(
                format!("http://example.org/test/{}", i),
                HashMap::new(),
                "test".to_string(),
            );
            notifier.publish(event).await.unwrap();
        }

        let history = notifier.get_history(100).await;
        // Should only keep last 5 due to max_history=5
        assert_eq!(history.len(), 5);
    }
}
