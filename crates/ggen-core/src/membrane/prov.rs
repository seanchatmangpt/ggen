//! PROV (W3C Provenance Ontology) representation for ggen membrane events.
//!
//! Captures the execution lineage and dependencies between entities, activities,
//! and agents within the Genesis core and ggen outer membrane.

use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use crate::utils::error::Result;
use super::core::{GgenMembrane, BoundaryCrossing};

/// W3C PROV-JSON top-level structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvDocument {
    /// Entities: physical or digital objects
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub entity: HashMap<String, HashMap<String, String>>,
    /// Activities: processes that use or generate entities
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub activity: HashMap<String, HashMap<String, String>>,
    /// Agents: entities that bear responsibility for activities
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub agent: HashMap<String, HashMap<String, String>>,
    /// Generations: link entity to generating activity
    #[serde(default, rename = "wasGeneratedBy", skip_serializing_if = "HashMap::is_empty")]
    pub was_generated_by: HashMap<String, ProvRelation>,
    /// Usages: link activity to used entity
    #[serde(default, rename = "used", skip_serializing_if = "HashMap::is_empty")]
    pub used: HashMap<String, ProvRelation>,
    /// Associations: link activity to agent
    #[serde(default, rename = "wasAssociatedWith", skip_serializing_if = "HashMap::is_empty")]
    pub was_associated_with: HashMap<String, ProvRelation>,
    /// Attributions: link entity to agent
    #[serde(default, rename = "wasAttributedTo", skip_serializing_if = "HashMap::is_empty")]
    pub was_attributed_to: HashMap<String, ProvRelation>,
    /// Derivations: link entity to entity
    #[serde(default, rename = "wasDerivedFrom", skip_serializing_if = "HashMap::is_empty")]
    pub was_derived_from: HashMap<String, ProvRelation>,
}

/// Generic relation entry in PROV-JSON
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvRelation {
    /// Source of the relation (prov:entity / prov:activity)
    #[serde(rename = "prov:entity", skip_serializing_if = "Option::is_none")]
    pub entity: Option<String>,
    /// Target of the relation (prov:activity / prov:agent / prov:entity)
    #[serde(rename = "prov:activity", skip_serializing_if = "Option::is_none")]
    pub activity: Option<String>,
    /// Target agent of the relation (prov:agent)
    #[serde(rename = "prov:agent", skip_serializing_if = "Option::is_none")]
    pub agent: Option<String>,
    /// Generated entity in derivation
    #[serde(rename = "prov:generatedEntity", skip_serializing_if = "Option::is_none")]
    pub generated_entity: Option<String>,
    /// Used entity in derivation
    #[serde(rename = "prov:usedEntity", skip_serializing_if = "Option::is_none")]
    pub used_entity: Option<String>,
}

impl ProvDocument {
    /// Extract a PROV-JSON document from a membrane state
    pub fn from_membrane(membrane: &GgenMembrane) -> Self {
        let mut entity = HashMap::new();
        let mut activity = HashMap::new();
        let mut agent = HashMap::new();
        
        let mut was_generated_by = HashMap::new();
        let mut used = HashMap::new();
        let mut was_associated_with = HashMap::new();
        let mut was_attributed_to = HashMap::new();
        let mut was_derived_from = HashMap::new();

        // 1. Declare Agents
        // Outer membrane agent
        let mut ggen_attrs = HashMap::new();
        ggen_attrs.insert("prov:type".to_string(), "SoftwareAgent".to_string());
        ggen_attrs.insert("prov:label".to_string(), "ggen Membrane Adapter Layer".to_string());
        agent.insert("agent:ggen".to_string(), ggen_attrs);

        // Genesis core agent
        let mut genesis_attrs = HashMap::new();
        genesis_attrs.insert("prov:type".to_string(), "SoftwareAgent".to_string());
        genesis_attrs.insert("prov:label".to_string(), "Genesis Embedded Core".to_string());
        agent.insert("agent:genesis-core".to_string(), genesis_attrs);

        // 2. Declare Entities for parts
        for (part_id, part) in &membrane.core.parts {
            let mut part_attrs = HashMap::new();
            part_attrs.insert("prov:type".to_string(), "interchangeable-part".to_string());
            part_attrs.insert("ggen:part_type".to_string(), part.part_type.clone());
            part_attrs.insert("ggen:version".to_string(), part.version.clone());
            part_attrs.insert("ggen:payload_hash".to_string(), part.payload_hash.clone());
            entity.insert(format!("entity:part:{}", part_id), part_attrs);

            // Attribute part to genesis core agent
            let attr_id = format!("attr:part:{}", part_id);
            was_attributed_to.insert(
                attr_id,
                ProvRelation {
                    entity: Some(format!("entity:part:{}", part_id)),
                    activity: None,
                    agent: Some("agent:genesis-core".to_string()),
                    generated_entity: None,
                    used_entity: None,
                },
            );
        }

        // 3. Process each boundary crossing event
        for (idx, crossing) in membrane.event_log.iter().enumerate() {
            let act_id = format!("activity:crossing:{}", crossing.id);
            let input_ent_id = format!("entity:input:{}", crossing.id);
            let output_ent_id = format!("entity:output:{}", crossing.id);

            // Add Activity
            let mut act_attrs = HashMap::new();
            act_attrs.insert("prov:type".to_string(), "boundary-crossing".to_string());
            act_attrs.insert("ggen:interface_fn".to_string(), crossing.interface_fn.clone());
            act_attrs.insert("ggen:timestamp".to_string(), crossing.timestamp.to_rfc3339());
            act_attrs.insert("ggen:duration_us".to_string(), crossing.duration_us.to_string());
            activity.insert(act_id.clone(), act_attrs);

            // Add Input Entity
            let mut in_attrs = HashMap::new();
            in_attrs.insert("prov:type".to_string(), "input-payload".to_string());
            in_attrs.insert("ggen:hash".to_string(), crossing.input_hash.clone());
            entity.insert(input_ent_id.clone(), in_attrs);

            // Add Output Entity
            if let Some(ref oh) = crossing.output_hash {
                let mut out_attrs = HashMap::new();
                out_attrs.insert("prov:type".to_string(), "output-payload".to_string());
                out_attrs.insert("ggen:hash".to_string(), oh.clone());
                out_attrs.insert("ggen:status_code".to_string(), crossing.status_code.to_string());
                entity.insert(output_ent_id.clone(), out_attrs);

                // Output was generated by the activity
                let gen_id = format!("gen:output:{}", crossing.id);
                was_generated_by.insert(
                    gen_id,
                    ProvRelation {
                        entity: Some(output_ent_id.clone()),
                        activity: Some(act_id.clone()),
                        agent: None,
                        generated_entity: None,
                        used_entity: None,
                    },
                );

                // Output was derived from input
                let deriv_id = format!("deriv:out-from-in:{}", crossing.id);
                was_derived_from.insert(
                    deriv_id,
                    ProvRelation {
                        entity: None,
                        activity: None,
                        agent: None,
                        generated_entity: Some(output_ent_id.clone()),
                        used_entity: Some(input_ent_id.clone()),
                    },
                );
            }

            // Activity used input
            let use_id = format!("use:input:{}", crossing.id);
            used.insert(
                use_id,
                ProvRelation {
                    entity: Some(input_ent_id.clone()),
                    activity: Some(act_id.clone()),
                    agent: None,
                    generated_entity: None,
                    used_entity: None,
                },
            );

            // Activity was associated with ggen agent
            let assoc_id = format!("assoc:ggen:{}", crossing.id);
            was_associated_with.insert(
                assoc_id,
                ProvRelation {
                    entity: None,
                    activity: Some(act_id.clone()),
                    agent: Some("agent:ggen".to_string()),
                    generated_entity: None,
                    used_entity: None,
                },
            );

            // Activity used target interchangeable part
            let part_use_id = format!("use:part:{}:{}", crossing.callee_id, idx);
            used.insert(
                part_use_id,
                ProvRelation {
                    entity: Some(format!("entity:part:{}", crossing.callee_id)),
                    activity: Some(act_id.clone()),
                    agent: None,
                    generated_entity: None,
                    used_entity: None,
                },
            );
        }

        Self {
            entity,
            activity,
            agent,
            was_generated_by,
            used,
            was_associated_with,
            was_attributed_to,
            was_derived_from,
        }
    }

    /// Serialize to JSON string
    pub fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }
}
