//! YAWL and Petri net bidirectional bridge.
//!
//! This module provides conversion between YAWL workflows and Petri nets,
//! enabling process mining on YAWL-based workflow specifications.

use crate::error::{Error, Result};
use crate::petri_net::{Arc, Marking, PetriNet, Place, Transition};

/// Bridge for converting between YAWL and Petri net representations.
///
/// YAWL (Yet Another Workflow Language) is a workflow specification language
/// that extends Petri nets with advanced features like OR-joins, cancellation,
// and composite tasks.
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
    ///
    /// This method parses YAWL XML and converts it to an equivalent Petri net
    /// representation for process mining operations.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The YAWL XML is malformed
    /// - The YAWL structure cannot be represented as a Petri net
    /// - Required YAWL elements are missing
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::YawlBridge;
    ///
    /// let bridge = YawlBridge::new();
    /// let yawl_xml = std::fs::read_to_string("workflow.yawl")?;
    /// let petri_net = bridge.yawl_to_petri_net(&yawl_xml)?;
    /// ```
    pub fn yawl_to_petri_net(&self, yawl_xml: &str) -> Result<PetriNet> {
        // Parse YAWL XML structure
        let spec = self.parse_yawl_xml(yawl_xml)?;

        // Convert to Petri net
        self.convert_yawl_spec_to_net(&spec)
    }

    /// Convert a Petri net to YAWL XML.
    ///
    /// This method creates a YAWL workflow specification from a Petri net,
    /// enabling execution of discovered process models.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The Petri net structure is invalid
    /// - The structure cannot be represented in YAWL
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::{YawlBridge, PetriNet};
    ///
    /// let bridge = YawlBridge::new();
    /// let net = PetriNet::from_activities(&["A", "B", "C"]);
    /// let yawl_xml = bridge.petri_net_to_yawl(&net)?;
    /// ```
    pub fn petri_net_to_yawl(&self, net: &PetriNet) -> Result<String> {
        // Validate Petri net structure
        net.validate()?;

        // Convert to YAWL specification
        let spec = self.convert_net_to_yawl_spec(net)?;

        // Serialize to YAWL XML
        Ok(self.serialize_yawl_spec(&spec))
    }
}

/// Internal representation of a parsed YAWL specification.
#[derive(Debug, Clone)]
struct YawlSpecification {
    name: String,
    tasks: Vec<YawlTask>,
    conditions: Vec<YawlCondition>,
    flows: Vec<YawlFlow>,
}

/// A YAWL task (activity).
#[derive(Debug, Clone)]
struct YawlTask {
    id: String,
    name: String,
    split_type: YawlSplitType,
    join_type: YawlJoinType,
    is_composite: bool,
}

/// YAWL split behavior (AND, OR, XOR).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum YawlSplitType {
    And,
    Or,
    Xor,
}

/// YAWL join behavior (AND, OR, XOR).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum YawlJoinType {
    And,
    Or,
    Xor,
}

/// A YAWL condition (place/state).
#[derive(Debug, Clone)]
struct YawlCondition {
    id: String,
    name: String,
}

/// A flow connection in YAWL.
#[derive(Debug, Clone)]
struct YawlFlow {
    from: String,
    to: String,
}

impl YawlBridge {
    /// Parse YAWL XML into an internal specification structure.
    fn parse_yawl_xml(&self, xml: &str) -> Result<YawlSpecification> {
        // Use quick-xml for parsing
        let document = roxmltree::Document::parse(xml)
            .map_err(|e| Error::YawlToPetriNetConversion(format!("XML parse error: {e}")))?;

        let root = document.root_element();
        let spec_elem = root
            .children()
            .find(|n| n.tag_name().name() == "specification")
            .ok_or_else(|| {
                Error::YawlToPetriNetConversion("missing specification element".to_string())
            })?;

        let name = spec_elem
            .children()
            .find(|n| n.tag_name().name() == "name")
            .and_then(|n| n.text())
            .unwrap_or("Unnamed")
            .to_string();

        // Parse tasks and conditions
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
            name,
            tasks,
            conditions,
            flows,
        })
    }

    /// Parse a YAWL task element.
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

        // Parse split/join types (default to AND)
        let split_attr = elem.attribute("splitType").unwrap_or("and");
        let join_attr = elem.attribute("joinType").unwrap_or("and");

        let split_type = match split_attr.to_lowercase().as_str() {
            "and" => YawlSplitType::And,
            "or" => YawlSplitType::Or,
            "xor" => YawlSplitType::Xor,
            _ => {
                return Err(Error::YawlToPetriNetConversion(format!(
                    "invalid split type: {split_attr}"
                )))
            }
        };

        let join_type = match join_attr.to_lowercase().as_str() {
            "and" => YawlJoinType::And,
            "or" => YawlJoinType::Or,
            "xor" => YawlJoinType::Xor,
            _ => {
                return Err(Error::YawlToPetriNetConversion(format!(
                    "invalid join type: {join_attr}"
                )))
            }
        };

        // Check if composite (has decomposition)
        let is_composite = elem
            .children()
            .any(|n| n.tag_name().name() == "decomposition");

        Ok(Some(YawlTask {
            id: id.to_string(),
            name,
            split_type,
            join_type,
            is_composite,
        }))
    }

    /// Parse a YAWL condition element.
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

    /// Parse a YAWL flow element.
    fn parse_flow(&self, elem: &roxmltree::Node) -> Option<YawlFlow> {
        let from = elem.attribute("source")?.to_string();
        let to = elem.attribute("target")?.to_string();

        Some(YawlFlow { from, to })
    }

    /// Convert a parsed YAWL specification to a Petri net.
    fn convert_yawl_spec_to_net(&self, spec: &YawlSpecification) -> Result<PetriNet> {
        let mut net = PetriNet::new().with_name(&spec.name);

        // Convert conditions to places
        for condition in &spec.conditions {
            let place = Place::new(&condition.id).with_label(&condition.name);
            net.places.insert(place);
        }

        // Convert tasks to transitions with input/output places
        for task in &spec.tasks {
            if !self.preserve_composites && task.is_composite {
                // Flatten composite tasks: add internal places
                self.flatten_composite_task(&mut net, task)?;
            } else {
                // Simple task: add as transition
                let transition = Transition::new(&task.id).with_label(&task.name);
                net.transitions.insert(transition);
            }
        }

        // Convert flows to arcs
        for flow in &spec.flows {
            net.arcs.push(Arc::new(&flow.from, &flow.to));
        }

        // Set initial marking (find input condition)
        if let Some(input_cond) = spec.conditions.first() {
            net.initial_marking = Marking::new().with_token(&input_cond.id, 1);
        }

        // Set final marking (find output condition)
        if let Some(output_cond) = spec.conditions.last() {
            net.final_marking = Marking::new().with_token(&output_cond.id, 1);
        }

        net.validate()?;
        Ok(net)
    }

    /// Flatten a composite YAWL task into simpler Petri net structure.
    fn flatten_composite_task(&self, net: &mut PetriNet, task: &YawlTask) -> Result<()> {
        // Add input and output places for the composite task
        let input_place = Place::new(format!("{}_in", task.id));
        let output_place = Place::new(format!("{}_out", task.id));

        net.places.insert(input_place.clone());
        net.places.insert(output_place.clone());

        // Add transition for the composite task
        let transition = Transition::new(&task.id).with_label(&task.name);
        net.transitions.insert(transition.clone());

        // Connect input -> transition -> output
        net.arcs
            .push(Arc::new(input_place.id, transition.id.clone()));
        net.arcs.push(Arc::new(transition.id, output_place.id));

        Ok(())
    }

    /// Convert a Petri net to a YAWL specification.
    fn convert_net_to_yawl_spec(&self, net: &PetriNet) -> Result<YawlSpecification> {
        let mut tasks = Vec::new();
        let mut conditions = Vec::new();
        let mut flows = Vec::new();

        // Convert places to conditions
        for place in &net.places {
            conditions.push(YawlCondition {
                id: place.id.clone(),
                name: place.label.clone().unwrap_or_else(|| place.id.clone()),
            });
        }

        // Convert transitions to tasks
        for transition in &net.transitions {
            // Determine split/join types based on connectivity
            let (split_type, join_type) = self.determine_split_join_types(net, &transition.id)?;

            tasks.push(YawlTask {
                id: transition.id.clone(),
                name: transition
                    .label
                    .clone()
                    .unwrap_or_else(|| transition.id.clone()),
                split_type,
                join_type,
                is_composite: false, // Discovered models are typically flat
            });
        }

        // Convert arcs to flows
        for arc in &net.arcs {
            flows.push(YawlFlow {
                from: arc.source.clone(),
                to: arc.target.clone(),
            });
        }

        Ok(YawlSpecification {
            name: net
                .name
                .clone()
                .unwrap_or_else(|| "Discovered Process".to_string()),
            tasks,
            conditions,
            flows,
        })
    }

    /// Determine split and join types for a transition based on connectivity.
    fn determine_split_join_types(
        &self, net: &PetriNet, transition_id: &str,
    ) -> Result<(YawlSplitType, YawlJoinType)> {
        let input_places = net.input_places_for(transition_id);
        let output_places = net.output_places_for(transition_id);

        // For simple process discovery, default to AND splits/joins
        // More sophisticated analysis could detect XOR/OR patterns
        let split_type = if output_places.len() > 1 {
            YawlSplitType::And // Could be XOR/OR with more analysis
        } else {
            YawlSplitType::And
        };

        let join_type = if input_places.len() > 1 {
            YawlJoinType::And // Could be XOR/OR with more analysis
        } else {
            YawlJoinType::And
        };

        Ok((split_type, join_type))
    }

    /// Serialize a YAWL specification to XML.
    fn serialize_yawl_spec(&self, spec: &YawlSpecification) -> String {
        let mut xml = String::from("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        xml.push_str("<specification xmlns=\"http://www.yawlfoundation.org/yawlschema\">\n");
        xml.push_str(&format!("  <name>{}</name>\n", escape_xml(&spec.name)));

        // Add conditions
        for condition in &spec.conditions {
            xml.push_str("  <condition id=\"");
            xml.push_str(&condition.id);
            xml.push_str("\">\n");
            xml.push_str(&format!(
                "    <name>{}</name>\n",
                escape_xml(&condition.name)
            ));
            xml.push_str("  </condition>\n");
        }

        // Add tasks
        for task in &spec.tasks {
            xml.push_str("  <task id=\"");
            xml.push_str(&task.id);
            xml.push_str("\" splitType=\"");
            xml.push_str(split_type_to_str(task.split_type));
            xml.push_str("\" joinType=\"");
            xml.push_str(join_type_to_str(task.join_type));
            xml.push_str("\">\n");
            xml.push_str(&format!("    <name>{}</name>\n", escape_xml(&task.name)));
            xml.push_str("  </task>\n");
        }

        // Add flows
        for flow in &spec.flows {
            xml.push_str("  <flow source=\"");
            xml.push_str(&flow.from);
            xml.push_str("\" target=\"");
            xml.push_str(&flow.to);
            xml.push_str("\"/>\n");
        }

        xml.push_str("</specification>\n");
        xml
    }
}

/// Trait for YAWL to Petri net conversion.
pub trait YawlToPetriNet {
    /// Convert YAWL representation to Petri net.
    fn to_petri_net(&self) -> Result<PetriNet>;
}

/// Trait for Petri net to YAWL conversion.
pub trait PetriNetToYawl {
    /// Convert Petri net to YAWL XML representation.
    fn to_yawl(&self) -> Result<String>;
}

impl YawlToPetriNet for str {
    fn to_petri_net(&self) -> Result<PetriNet> {
        YawlBridge::new().yawl_to_petri_net(self)
    }
}

impl PetriNetToYawl for PetriNet {
    fn to_yawl(&self) -> Result<String> {
        YawlBridge::new().petri_net_to_yawl(self)
    }
}

/// Convert split type enum to string.
fn split_type_to_str(ty: YawlSplitType) -> &'static str {
    match ty {
        YawlSplitType::And => "and",
        YawlSplitType::Or => "or",
        YawlSplitType::Xor => "xor",
    }
}

/// Convert join type enum to string.
fn join_type_to_str(ty: YawlJoinType) -> &'static str {
    match ty {
        YawlJoinType::And => "and",
        YawlJoinType::Or => "or",
        YawlJoinType::Xor => "xor",
    }
}

/// Escape XML special characters.
fn escape_xml(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&apos;")
}

/// Convenience function for YAWL to Petri net conversion.
///
/// # Examples
///
/// ```rust,ignore
/// use ggen_process_mining::yawl_to_petri_net;
///
/// let yawl = std::fs::read_to_string("workflow.yawl")?;
/// let net = yawl_to_petri_net(&yawl)?;
/// ```
pub fn yawl_to_petri_net(yawl_xml: &str) -> Result<PetriNet> {
    YawlBridge::new().yawl_to_petri_net(yawl_xml)
}

/// Convenience function for Petri net to YAWL conversion.
///
/// # Examples
///
/// ```rust,ignore
/// use ggen_process_mining::{petri_net_to_yawl, PetriNet};
///
/// let net = PetriNet::from_activities(&["A", "B", "C"]);
/// let yawl = petri_net_to_yawl(&net)?;
/// ```
pub fn petri_net_to_yawl(net: &PetriNet) -> Result<String> {
    YawlBridge::new().petri_net_to_yawl(net)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yawl_bridge_creation() {
        let bridge = YawlBridge::new();
        assert!(bridge.preserve_composites);
    }

    #[test]
    fn test_yawl_bridge_with_config() {
        let bridge = YawlBridge::new().with_preserve_composites(false);
        assert!(!bridge.preserve_composites);
    }

    #[test]
    fn test_simple_yawl_to_petri_net() {
        let yawl_xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema">
  <name>Test Process</name>
  <condition id="c_input"><name>Input</name></condition>
  <condition id="c_output"><name>Output</name></condition>
  <task id="t1" splitType="and" joinType="and"><name>Task A</name></task>
  <flow source="c_input" target="t1"/>
  <flow source="t1" target="c_output"/>
</specification>"#;

        let result = YawlBridge::new().yawl_to_petri_net(yawl_xml);
        assert!(result.is_ok());

        let net = result.unwrap();
        assert_eq!(net.name, Some("Test Process".to_string()));
        assert!(net.validate().is_ok());
    }

    #[test]
    fn test_petri_net_to_yawl() {
        let net = PetriNet::from_activities(&["A", "B", "C"]);
        let result = YawlBridge::new().petri_net_to_yawl(&net);

        assert!(result.is_ok());
        let yawl = result.unwrap();
        assert!(yawl.contains("<specification"));
        assert!(yawl.contains("Task A"));
    }

    #[test]
    fn test_convenience_functions() {
        let net = PetriNet::from_activities(&["Approve", "Pay"]);

        let yawl = petri_net_to_yawl(&net).unwrap();
        assert!(yawl.contains("Approve"));
        assert!(yawl.contains("Pay"));
    }

    #[test]
    fn test_invalid_yawl_xml() {
        let invalid_xml = "not xml at all";
        let result = YawlBridge::new().yawl_to_petri_net(invalid_xml);

        assert!(result.is_err());
    }

    #[test]
    fn test_trait_yawl_to_petri_net() {
        let yawl_xml = r#"<?xml version="1.0" encoding="UTF-8"?>
<specification xmlns="http://www.yawlfoundation.org/yawlschema">
  <name>Test</name>
  <condition id="c1"><name>Start</name></condition>
  <condition id="c2"><name>End</name></condition>
  <task id="t1" splitType="and" joinType="and"><name>A</name></task>
  <flow source="c1" target="t1"/>
  <flow source="t1" target="c2"/>
</specification>"#;

        let result = yawl_xml.to_petri_net();
        assert!(result.is_ok());
    }

    #[test]
    fn test_trait_petri_net_to_yawl() {
        let net = PetriNet::from_activities(&["A", "B"]);
        let result = net.to_yawl();

        assert!(result.is_ok());
    }

    #[test]
    fn test_xml_escaping() {
        assert_eq!(escape_xml("hello & goodbye"), "hello &amp; goodbye");
        assert_eq!(escape_xml("<tag>"), "&lt;tag&gt;");
        assert_eq!(escape_xml("\"quoted\""), "&quot;quoted&quot;");
    }
}
