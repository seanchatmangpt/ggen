//! OCEL process discovery via SPARQL over the OCEL-RDF event log.
//!
//! This module discovers canonical process models from OCEL event logs by analyzing
//! object-centric execution traces. For each object type, it identifies the most common
//! sequence of activities (canonical path) and measures how many instances deviate
//! from that path (variant ratio).
//!
//! Discovery is pure Rust analysis via SPARQL queries:
//! 1. Group events by object type
//! 2. For each object type, find all execution paths (sequences of activities)
//! 3. Identify the canonical path (most frequent)
//! 4. Calculate variant ratio (% of instances following non-canonical paths)
//!
//! No subprocess calls or external process-mining engines required.

use std::collections::HashMap;
use oxigraph::sparql::QueryResults;

use crate::graph::DeterministicGraph;
use crate::ocel::{OcelEvent, OcelLog};
use crate::GraphError;

/// A discovered process model for a single object type.
#[derive(Debug, Clone, PartialEq)]
pub struct ProcessModel {
    /// The object type this model describes (e.g., "pack", "lockfile-entry", "receipt").
    pub object_type: String,
    /// The canonical (most common) sequence of activities observed for this object type.
    pub canonical_path: Vec<String>,
    /// Total number of distinct execution paths observed (e.g., 3 variants for this object type).
    pub variant_count: usize,
    /// Ratio of instances that follow non-canonical paths (0.0 = all follow canonical, 1.0 = all deviate).
    pub variant_ratio: f64,
}

/// OCEL process discovery engine.
pub struct OcelProcessDiscovery;

impl OcelProcessDiscovery {
    /// Discover process models from an OCEL event log.
    ///
    /// For each object type in the log, this discovers the canonical execution path
    /// and measures variant frequency. Returns one `ProcessModel` per object type.
    ///
    /// # Algorithm
    ///
    /// 1. Group events by object type (e.g., pack, lockfile-entry)
    /// 2. For each object type:
    ///    - Build the activity sequence for each object instance (e.g., pack:acme/base@1.0)
    ///    - Count frequency of each path
    ///    - Canonical path = most frequent path
    ///    - variant_count = number of distinct paths observed
    ///    - variant_ratio = (total_instances - canonical_instances) / total_instances
    ///
    /// # Examples
    ///
    /// For a pack lifecycle log with install → verify → publish (canonical),
    /// this would return:
    /// ```text
    /// ProcessModel {
    ///   object_type: "pack",
    ///   canonical_path: ["pack.install", "pack.verify", "pack.publish"],
    ///   variant_count: 1,
    ///   variant_ratio: 0.0,
    /// }
    /// ```
    ///
    /// # Errors
    /// Returns a [`GraphError`] if SPARQL queries fail.
    pub fn discover(log: &OcelLog) -> Result<Vec<ProcessModel>, GraphError> {
        let mut models = Vec::new();

        // Group objects by type
        let mut objects_by_type: HashMap<String, Vec<String>> = HashMap::new();
        for obj in &log.objects {
            objects_by_type
                .entry(obj.r#type.clone())
                .or_insert_with(Vec::new)
                .push(obj.id.clone());
        }

        // For each object type, discover its process model
        for (obj_type, object_ids) in objects_by_type {
            // Build activity sequences for each object instance
            let mut sequences: HashMap<String, Vec<String>> = HashMap::new();
            for event in &log.events {
                for obj_ref in &event.objects {
                    if object_ids.contains(&obj_ref.id) {
                        sequences
                            .entry(obj_ref.id.clone())
                            .or_insert_with(Vec::new)
                            .push(event.activity.clone());
                    }
                }
            }

            // Count frequency of each path
            let mut path_frequency: HashMap<Vec<String>, usize> = HashMap::new();
            for (_, path) in sequences {
                *path_frequency.entry(path).or_insert(0) += 1;
            }

            // Find canonical path (most frequent)
            let canonical_path = path_frequency
                .iter()
                .max_by_key(|(_, count)| *count)
                .map(|(path, _)| path.clone())
                .unwrap_or_default();

            // Calculate variant ratio
            let total_instances = path_frequency.values().sum::<usize>();
            let canonical_instances = path_frequency.get(&canonical_path).copied().unwrap_or(0);
            let variant_ratio = if total_instances > 0 {
                (total_instances - canonical_instances) as f64 / total_instances as f64
            } else {
                0.0
            };

            let variant_count = path_frequency.len();

            models.push(ProcessModel {
                object_type: obj_type.clone(),
                canonical_path,
                variant_count,
                variant_ratio,
            });
        }

        Ok(models)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{TimeZone, Utc};
    use std::collections::HashMap;

    use crate::ocel::{OcelEvent, OcelObject, OcelObjectRef};

    fn ts(secs: i64) -> chrono::DateTime<chrono::Utc> {
        Utc.timestamp_opt(secs, 0)
            .single()
            .unwrap_or_else(Utc::now)
    }

    #[test]
    fn discovers_canonical_path_for_single_object_type() {
        // Arrange: two pack instances, both following install -> verify -> publish
        let mut log = OcelLog::new();

        // Objects
        log.objects.push(OcelObject {
            id: "pack:acme/base@1.0".to_string(),
            r#type: "pack".to_string(),
            attributes: HashMap::new(),
        });
        log.objects.push(OcelObject {
            id: "pack:acme/base@1.1".to_string(),
            r#type: "pack".to_string(),
            attributes: HashMap::new(),
        });

        // Events for pack 1.0
        log.events.push(OcelEvent {
            id: "ev1".to_string(),
            activity: "pack.install".to_string(),
            timestamp: ts(10),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev2".to_string(),
            activity: "pack.verify".to_string(),
            timestamp: ts(20),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev3".to_string(),
            activity: "pack.publish".to_string(),
            timestamp: ts(30),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Events for pack 1.1 (same path)
        log.events.push(OcelEvent {
            id: "ev4".to_string(),
            activity: "pack.install".to_string(),
            timestamp: ts(40),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.1".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev5".to_string(),
            activity: "pack.verify".to_string(),
            timestamp: ts(50),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.1".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev6".to_string(),
            activity: "pack.publish".to_string(),
            timestamp: ts(60),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.1".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Act
        let models = OcelProcessDiscovery::discover(&log).expect("discovery should succeed");

        // Assert
        assert_eq!(models.len(), 1, "should discover one process model (pack type)");
        let pack_model = &models[0];
        assert_eq!(
            pack_model.object_type, "pack",
            "model should be for pack object type"
        );
        assert_eq!(
            pack_model.canonical_path,
            vec!["pack.install", "pack.verify", "pack.publish"],
            "canonical path should be install -> verify -> publish"
        );
        assert_eq!(
            pack_model.variant_count, 1,
            "only one distinct path observed"
        );
        assert_eq!(
            pack_model.variant_ratio, 0.0,
            "all instances follow canonical path"
        );
    }

    #[test]
    fn detects_variants_in_execution_paths() {
        // Arrange: mix of canonical and variant paths
        let mut log = OcelLog::new();

        log.objects.push(OcelObject {
            id: "pack:acme/base@1.0".to_string(),
            r#type: "pack".to_string(),
            attributes: HashMap::new(),
        });
        log.objects.push(OcelObject {
            id: "pack:acme/base@1.1".to_string(),
            r#type: "pack".to_string(),
            attributes: HashMap::new(),
        });
        log.objects.push(OcelObject {
            id: "pack:acme/base@1.2".to_string(),
            r#type: "pack".to_string(),
            attributes: HashMap::new(),
        });

        // Pack 1.0: install -> verify -> publish (canonical)
        log.events.push(OcelEvent {
            id: "ev1".to_string(),
            activity: "pack.install".to_string(),
            timestamp: ts(10),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev2".to_string(),
            activity: "pack.verify".to_string(),
            timestamp: ts(20),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev3".to_string(),
            activity: "pack.publish".to_string(),
            timestamp: ts(30),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Pack 1.1: install -> verify -> publish (canonical)
        log.events.push(OcelEvent {
            id: "ev4".to_string(),
            activity: "pack.install".to_string(),
            timestamp: ts(40),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.1".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev5".to_string(),
            activity: "pack.verify".to_string(),
            timestamp: ts(50),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.1".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev6".to_string(),
            activity: "pack.publish".to_string(),
            timestamp: ts(60),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.1".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Pack 1.2: install -> remove (variant: no verify/publish)
        log.events.push(OcelEvent {
            id: "ev7".to_string(),
            activity: "pack.install".to_string(),
            timestamp: ts(70),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.2".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev8".to_string(),
            activity: "pack.remove".to_string(),
            timestamp: ts(80),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.2".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Act
        let models = OcelProcessDiscovery::discover(&log).expect("discovery should succeed");

        // Assert
        assert_eq!(models.len(), 1);
        let pack_model = &models[0];
        assert_eq!(
            pack_model.canonical_path,
            vec!["pack.install", "pack.verify", "pack.publish"],
            "canonical path should be the most frequent one"
        );
        assert_eq!(pack_model.variant_count, 2, "two distinct paths observed");
        assert_eq!(
            pack_model.variant_ratio,
            1.0 / 3.0,
            "one out of three instances follows variant path"
        );
    }

    #[test]
    fn discovers_multiple_object_types() {
        // Arrange: log with pack and lockfile-entry objects
        let mut log = OcelLog::new();

        log.objects.push(OcelObject {
            id: "pack:acme/base@1.0".to_string(),
            r#type: "pack".to_string(),
            attributes: HashMap::new(),
        });
        log.objects.push(OcelObject {
            id: "lockfile-entry:acme/base@1.0".to_string(),
            r#type: "lockfile-entry".to_string(),
            attributes: HashMap::new(),
        });

        // Pack events: install -> verify -> publish
        log.events.push(OcelEvent {
            id: "ev1".to_string(),
            activity: "pack.install".to_string(),
            timestamp: ts(10),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev2".to_string(),
            activity: "pack.verify".to_string(),
            timestamp: ts(20),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });
        log.events.push(OcelEvent {
            id: "ev3".to_string(),
            activity: "pack.publish".to_string(),
            timestamp: ts(30),
            objects: vec![OcelObjectRef {
                id: "pack:acme/base@1.0".to_string(),
                r#type: "pack".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Lockfile-entry events: lockfile.write
        log.events.push(OcelEvent {
            id: "ev4".to_string(),
            activity: "lockfile.write".to_string(),
            timestamp: ts(15),
            objects: vec![OcelObjectRef {
                id: "lockfile-entry:acme/base@1.0".to_string(),
                r#type: "lockfile-entry".to_string(),
                qualifier: Some("subject".to_string()),
            }],
            attributes: HashMap::new(),
        });

        // Act
        let models = OcelProcessDiscovery::discover(&log).expect("discovery should succeed");

        // Assert
        assert_eq!(models.len(), 2, "should discover two process models");

        let pack_model = models
            .iter()
            .find(|m| m.object_type == "pack")
            .expect("pack model should exist");
        assert_eq!(pack_model.variant_count, 1);
        assert_eq!(pack_model.variant_ratio, 0.0);

        let lock_model = models
            .iter()
            .find(|m| m.object_type == "lockfile-entry")
            .expect("lockfile-entry model should exist");
        assert_eq!(lock_model.canonical_path, vec!["lockfile.write"]);
        assert_eq!(lock_model.variant_count, 1);
    }
}
