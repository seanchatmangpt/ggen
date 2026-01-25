# ADR-010: Event Sourcing for Audit Trail and Event Log

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Maintaining authoritative audit trail and enabling event-driven workflows
**Deciders:** Data Architecture Team

## Problem Statement

TAI system needs:
- Authoritative audit trail (who did what when)
- Compliance evidence (GDPR, SOC2)
- Event replay for debugging
- Historical analytics
- Event-driven workflows (policy enforcement, signal processing)

Relational storage alone insufficient (after-the-fact changes).

## Decision

**Event sourcing architecture:**
1. **Event log** (immutable): Cloud Pub/Sub with persistent topic storage
2. **State snapshots** (mutable): Firestore for current state
3. **Event replay** (deterministic): Reconstruct state from events

## Rationale

### Event Sourcing Benefits
1. **Immutability:**
   - Events never change (append-only)
   - Perfect audit trail
   - Tamper-evident log
   - Historical reconstruction

2. **Time Travel:**
   - Replay events to any point in time
   - Debug issues by examining state evolution
   - Understand system behavior

3. **Event-Driven Architecture:**
   - Services react to domain events
   - Decoupled communication
   - Natural scaling via pub/sub

4. **Compliance:**
   - Right to audit
   - Right to be forgotten (selective event deletion)
   - Data retention policies

## Implementation

### Event Definition

```rust
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DomainEvent {
    pub event_id: String,
    pub event_type: String,
    pub aggregate_type: String,
    pub aggregate_id: String,
    pub version: i64,
    pub timestamp: DateTime<Utc>,
    pub actor: String,  // User or service
    pub data: serde_json::Value,
    pub metadata: serde_json::Value,
}

// Policy events
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PolicyProposed {
    pub policy_id: String,
    pub policy_type: String,
    pub policy_name: String,
    pub rules: serde_json::Value,
    pub proposed_by: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PolicyEnforced {
    pub policy_id: String,
    pub enforced_at: DateTime<Utc>,
    pub enforced_by: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PolicyRevoked {
    pub policy_id: String,
    pub revoked_at: DateTime<Utc>,
    pub revoked_by: String,
    pub reason: String,
}

// Signal events
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SignalEmitted {
    pub signal_id: String,
    pub signal_type: String,
    pub timestamp_ns: i64,
    pub metadata: serde_json::Value,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SignalProcessed {
    pub signal_id: String,
    pub result: String,  // "success", "failure"
    pub processed_at: DateTime<Utc>,
}
```

### Event Store (Cloud Pub/Sub)

```rust
use google_cloud_pubsub::client::{Client, PublisherId};
use google_cloud_pubsub::publisher::Publisher;

pub struct EventStore {
    publisher: Publisher,
    topic_name: String,
}

impl EventStore {
    pub async fn new(project_id: &str, topic_name: &str) -> Result<Self> {
        let client = Client::new(Default::default()).await?;
        let topic = client.topic(topic_name);

        // Create topic if not exists
        if !topic.exists(None).await? {
            topic.create(None, None).await?;
        }

        let publisher = client.new_publisher(PublisherId::TopicName(
            format!("projects/{}/topics/{}", project_id, topic_name)
        )).await?;

        Ok(Self {
            publisher,
            topic_name: topic_name.to_string(),
        })
    }

    pub async fn append_event(&mut self, event: &DomainEvent) -> Result<String> {
        let json = serde_json::to_vec(event)?;

        let message_id = self.publisher
            .publish(
                google_cloud_pubsub::publisher::PubsubMessage {
                    data: json,
                    attributes: vec![
                        ("event_type".to_string(), event.event_type.clone()),
                        ("aggregate_type".to_string(), event.aggregate_type.clone()),
                        ("aggregate_id".to_string(), event.aggregate_id.clone()),
                        ("version".to_string(), event.version.to_string()),
                        ("timestamp".to_string(), event.timestamp.to_rfc3339()),
                        ("actor".to_string(), event.actor.clone()),
                    ],
                }
            )
            .await?;

        tracing::info!("Event appended: {}", message_id);
        Ok(message_id)
    }

    // Read events for aggregate
    pub async fn get_events(&self, aggregate_id: &str) -> Result<Vec<DomainEvent>> {
        // Use Firestore query (materialized view of events)
        let db = firestore::FirestoreClient::new().await?;

        let docs: Vec<DomainEvent> = db
            .fluent()
            .select()
            .from("events")
            .filter(|q| q.field_path("aggregate_id").eq(aggregate_id))
            .order_by([("version".to_string(), firestore::OrderDirection::Ascending)])
            .obj()
            .await?;

        Ok(docs)
    }

    // Replay events to reconstruct state
    pub async fn replay_events(
        &self,
        aggregate_id: &str,
        to_version: Option<i64>
    ) -> Result<Policy> {
        let events = self.get_events(aggregate_id).await?;

        let mut policy: Option<Policy> = None;

        for event in events {
            if let Some(version_limit) = to_version {
                if event.version > version_limit {
                    break;
                }
            }

            // Apply event to state
            policy = apply_event_to_policy(policy, event)?;
        }

        policy.ok_or_else(|| "No events found".into())
    }
}

fn apply_event_to_policy(
    mut policy: Option<Policy>,
    event: DomainEvent
) -> Result<Option<Policy>> {
    match event.event_type.as_str() {
        "PolicyProposed" => {
            let proposed: PolicyProposed = serde_json::from_value(event.data)?;
            policy = Some(Policy {
                id: proposed.policy_id.clone(),
                policy_type: proposed.policy_type,
                policy_name: proposed.policy_name,
                rules: proposed.rules,
                version: event.version,
                enabled: false,
                created_at_ns: event.timestamp.timestamp_nanos() as i64,
                updated_at_ns: event.timestamp.timestamp_nanos() as i64,
            });
        }
        "PolicyEnforced" => {
            if let Some(ref mut p) = policy {
                p.enabled = true;
                p.updated_at_ns = event.timestamp.timestamp_nanos() as i64;
            }
        }
        "PolicyRevoked" => {
            if let Some(ref mut p) = policy {
                p.enabled = false;
                p.updated_at_ns = event.timestamp.timestamp_nanos() as i64;
            }
        }
        _ => {}
    }

    Ok(policy)
}
```

### Snapshot Strategy

```rust
pub struct Snapshot {
    pub aggregate_id: String,
    pub aggregate_type: String,
    pub version: i64,
    pub state: serde_json::Value,
    pub timestamp: DateTime<Utc>,
}

pub struct EventStoreWithSnapshots {
    event_store: EventStore,
    snapshot_interval: i64,  // Create snapshot every N events
}

impl EventStoreWithSnapshots {
    pub async fn get_state(&self, aggregate_id: &str) -> Result<Policy> {
        // Try loading from snapshot
        if let Ok(snapshot) = self.load_snapshot(aggregate_id).await {
            // Load events after snapshot
            let events = self.event_store.get_events(aggregate_id)
                .await?
                .into_iter()
                .filter(|e| e.version > snapshot.version)
                .collect::<Vec<_>>();

            // Replay events from snapshot
            let mut state: Policy = serde_json::from_value(snapshot.state)?;
            for event in events {
                state = apply_event_to_policy(Some(state), event)?
                    .ok_or("Invalid state transition")?;
            }

            return Ok(state);
        }

        // Fall back to full replay
        self.event_store.replay_events(aggregate_id, None).await
    }

    pub async fn create_snapshot(&self, aggregate_id: &str, version: i64) -> Result<()> {
        let policy = self.event_store.replay_events(aggregate_id, Some(version)).await?;

        let snapshot = Snapshot {
            aggregate_id: aggregate_id.to_string(),
            aggregate_type: "Policy".to_string(),
            version,
            state: serde_json::to_value(&policy)?,
            timestamp: Utc::now(),
        };

        // Store snapshot in Firestore
        firestore::Client::new()
            .await?
            .fluent()
            .update()
            .in_col("snapshots")
            .document_id(aggregate_id)
            .set(serde_json::to_value(&snapshot)?)
            .execute()
            .await?;

        Ok(())
    }

    pub async fn load_snapshot(&self, aggregate_id: &str) -> Result<Snapshot> {
        firestore::Client::new()
            .await?
            .fluent()
            .select()
            .from("snapshots")
            .document_id(aggregate_id)
            .obj()
            .await
    }
}
```

### Event Sourcing in Business Logic

```rust
#[tonic::async_trait]
impl Governor for GovernorService {
    async fn propose_policy(
        &self,
        request: Request<Policy>,
    ) -> Result<Response<Receipt>, Status> {
        let policy = request.into_inner();
        let policy_id = policy.id.clone();

        // Create event
        let event = DomainEvent {
            event_id: Uuid::new_v4().to_string(),
            event_type: "PolicyProposed".to_string(),
            aggregate_type: "Policy".to_string(),
            aggregate_id: policy_id.clone(),
            version: 1,
            timestamp: Utc::now(),
            actor: extract_actor(&request)?,
            data: serde_json::to_value(&PolicyProposed {
                policy_id: policy.id.clone(),
                policy_type: policy.policy_type.clone(),
                policy_name: policy.policy_name.clone(),
                rules: policy.rules.clone(),
                proposed_by: extract_actor(&request)?,
            })?,
            metadata: serde_json::json!({}),
        };

        // Append to event log
        self.event_store.append_event(&event).await
            .map_err(|e| Status::internal(e.to_string()))?;

        // Create snapshot every 100 events
        let events = self.event_store.get_events(&policy_id).await?;
        if events.len() % 100 == 0 {
            self.event_store.create_snapshot(&policy_id, events.len() as i64).await?;
        }

        // Return receipt
        let receipt = Receipt {
            id: Uuid::new_v4().to_string(),
            request_id: policy_id,
            status: "success".to_string(),
            timestamp_ns: SystemTime::now()
                .duration_since(UNIX_EPOCH)?
                .as_nanos() as i64,
            result_data: Default::default(),
            error_message: String::new(),
            execution_time_us: 0,
        };

        Ok(Response::new(receipt))
    }
}
```

### Compliance and Retention

```rust
pub async fn delete_user_events(user_id: &str) -> Result<()> {
    // Right to be forgotten: selective event deletion
    let db = firestore::FirestoreClient::new().await?;

    // Find events involving this user
    let events: Vec<DomainEvent> = db
        .fluent()
        .select()
        .from("events")
        .filter(|q| q.field_path("actor").eq(user_id))
        .obj()
        .await?;

    // Delete events (with audit trail)
    for event in events {
        db.fluent()
            .delete()
            .from("events")
            .document_id(&event.event_id)
            .execute()
            .await?;
    }

    Ok(())
}

pub async fn enforce_retention_policy() -> Result<()> {
    // Delete events older than 7 years (GDPR requirement)
    let db = firestore::FirestoreClient::new().await?;
    let cutoff = Utc::now() - Duration::days(365 * 7);

    db.fluent()
        .delete()
        .from("events")
        .filter(|q| q.field_path("timestamp").lt(cutoff))
        .execute()
        .await?;

    Ok(())
}
```

## Monitoring

```prometheus
# Events appended per minute
tai_events_appended_total{event_type="PolicyProposed"} 142

# Event processing lag
tai_event_processing_lag_seconds 5.2

# Snapshot creation frequency
tai_snapshots_created_total 23
```

## Consequences

### Positive
- Perfect audit trail (immutable)
- Time travel debugging
- Event replay
- Compliance ready
- Scalable event processing

### Negative
- Storage overhead (all events kept)
- Complexity (eventual consistency)
- Event versioning challenges
- Debugging from logs harder
- Snapshot management overhead

## References
- [Event Sourcing Pattern](https://martinfowler.com/eaaDev/EventSourcing.html)
- [Cloud Pub/Sub Documentation](https://cloud.google.com/pubsub/docs)
- [CQRS Pattern](https://martinfowler.com/bliki/CQRS.html)
