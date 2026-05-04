//! YAWL and Petri net bidirectional bridge.
//!
//! This module provides conversion between YAWL workflows and Petri nets,
//! enabling process mining on YAWL-based workflow specifications.
//! Utilizes pictl-types for the underlying Petri net representation.

use crate::error::{Error, Result};
use crate::petri_net::{Arc, PetriNet, Place, Transition};

/// Bridge for converting between YAWL and Petri net representations.
#[derive(Debug, Clone)]
pub struct YawlBridge {
    /// Whether to preserve composite task structure during conversion.
    preserve_composites: bool,
}

impl Default for YawlBridge {
    fn default() -> Self {
        Self::new()
    }
}

impl YawlBridge {
    /// Create a new YAWL bridge with default settings.
    #[must_use]
    pub fn new() -> Self {
        Self {
            preserve_composites: true,
        }
    }

    /// Set whether to preserve composite task structure.
    #[must_use]
    pub fn with_preserve_composites(mut self, preserve: bool) -> Self {
        self.preserve_composites = preserve;
        self
    }

    /// Convert YAWL XML to a Petri net.
    pub fn yawl_to_petri_net(&self, yawl_xml: &str) -> Result<PetriNet> {
        let spec = self.parse_yawl_xml(yawl_xml)?;
        self.convert_yawl_spec_to_net(&spec)
    }

    fn parse_yawl_xml(&self, xml: &str) -> Result<YawlSpecification> {
        let document = roxmltree::Document::parse(xml)
            .map_err(|e| Error::YawlToPetriNetConversion(format!("XML parse error: {e}")))?;

        let root = document.root_element();
        let spec_elem = if root.tag_name().name() == "specification" {
            root
        } else {
            root.children()
                .find(|n| n.tag_name().name() == "specification")
                .ok_or_else(|| {
                    Error::YawlToPetriNetConversion("missing specification element".to_string())
                })?
        };

        let mut tasks = Vec::new();
        let mut conditions = Vec::new();
        let mut flows = Vec::new();

        for child in spec_elem.children() {
            match child.tag_name().name() {
                "task" => {
                    if let Some(task) = self.parse_task(&child)? {
                        tasks.push(task);
                    }
                }
                "condition" => {
                    if let Some(condition) = self.parse_condition(&child) {
                        conditions.push(condition);
                    }
                }
                "flow" => {
                    if let Some(flow) = self.parse_flow(&child) {
                        flows.push(flow);
                    }
                }
                _ => {}
            }
        }

        Ok(YawlSpecification {
            tasks,
            conditions,
            flows,
        })
    }

    fn parse_task(&self, elem: &roxmltree::Node) -> Result<Option<YawlTask>> {
        let id = elem
            .attribute("id")
            .ok_or_else(|| Error::YawlToPetriNetConversion("task missing id".to_string()))?;

        let name = elem
            .children()
            .find(|n| n.tag_name().name() == "name")
            .and_then(|n| n.text())
            .unwrap_or(id)
            .to_string();

        Ok(Some(YawlTask {
            id: id.to_string(),
            name,
        }))
    }

    fn parse_condition(&self, elem: &roxmltree::Node) -> Option<YawlCondition> {
        let id = elem.attribute("id")?;
        let name = elem
            .children()
            .find(|n| n.tag_name().name() == "name")
            .and_then(|n| n.text())
            .unwrap_or(id)
            .to_string();

        Some(YawlCondition {
            id: id.to_string(),
            name,
        })
    }

    fn parse_flow(&self, elem: &roxmltree::Node) -> Option<YawlFlow> {
        let from = elem.attribute("source")?.to_string();
        let to = elem.attribute("target")?.to_string();

        Some(YawlFlow { from, to })
    }

    fn convert_yawl_spec_to_net(&self, spec: &YawlSpecification) -> Result<PetriNet> {
        let mut net = PetriNet::new();

        for condition in &spec.conditions {
            net.places.push(Place::new(condition.id.clone(), condition.name.clone()));
        }

        for task in &spec.tasks {
            net.transitions.push(Transition::new(task.id.clone(), task.name.clone(), false));
        }

        for flow in &spec.flows {
            net.arcs.push(Arc::new(flow.from.clone(), flow.to.clone(), 1));
        }

        Ok(net)
    }
}

#[derive(Debug, Clone)]
struct YawlSpecification {
    tasks: Vec<YawlTask>,
    conditions: Vec<YawlCondition>,
    flows: Vec<YawlFlow>,
}

#[derive(Debug, Clone)]
struct YawlTask {
    id: String,
    name: String,
}

#[derive(Debug, Clone)]
struct YawlCondition {
    id: String,
    name: String,
}

#[derive(Debug, Clone)]
struct YawlFlow {
    from: String,
    to: String,
}
