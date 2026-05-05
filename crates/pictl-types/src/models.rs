use serde::{Deserialize, Serialize};

/// A node in a Directly-Follows Graph
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct DFGNode {
    pub activity: String,
    pub frequency: usize,
}

impl DFGNode {
    pub fn new(activity: String, frequency: usize) -> Self {
        DFGNode {
            activity,
            frequency,
        }
    }
}

/// An edge in a Directly-Follows Graph
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct DFGEdge {
    pub source: String,
    pub target: String,
    pub frequency: usize,
}

impl DFGEdge {
    pub fn new(source: String, target: String, frequency: usize) -> Self {
        DFGEdge {
            source,
            target,
            frequency,
        }
    }
}

/// A Directly-Follows Graph model
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DFG {
    pub nodes: Vec<DFGNode>,
    pub edges: Vec<DFGEdge>,
    pub start_activities: Vec<String>,
    pub end_activities: Vec<String>,
}

impl DFG {
    pub fn new() -> Self {
        DFG {
            nodes: Vec::new(),
            edges: Vec::new(),
            start_activities: Vec::new(),
            end_activities: Vec::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }
}

impl Default for DFG {
    fn default() -> Self {
        Self::new()
    }
}

/// A place in a Petri net
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PetriNetPlace {
    pub id: String,
    pub label: String,
    pub initial_marking: usize,
}

impl PetriNetPlace {
    pub fn new(id: String, label: String) -> Self {
        PetriNetPlace {
            id,
            label,
            initial_marking: 0,
        }
    }
}

/// A transition in a Petri net
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PetriNetTransition {
    pub id: String,
    pub label: String,
    pub invisible: bool,
}

impl PetriNetTransition {
    pub fn new(id: String, label: String, invisible: bool) -> Self {
        PetriNetTransition {
            id,
            label,
            invisible,
        }
    }
}

/// An arc in a Petri net
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PetriNetArc {
    pub source: String,
    pub target: String,
    pub weight: usize,
}

impl PetriNetArc {
    pub fn new(source: String, target: String, weight: usize) -> Self {
        PetriNetArc {
            source,
            target,
            weight,
        }
    }
}

/// A Petri net model
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct PetriNet {
    pub places: Vec<PetriNetPlace>,
    pub transitions: Vec<PetriNetTransition>,
    pub arcs: Vec<PetriNetArc>,
}

impl PetriNet {
    pub fn new() -> Self {
        PetriNet {
            places: Vec::new(),
            transitions: Vec::new(),
            arcs: Vec::new(),
        }
    }

    pub fn len_places(&self) -> usize {
        self.places.len()
    }

    pub fn len_transitions(&self) -> usize {
        self.transitions.len()
    }

    pub fn is_empty(&self) -> bool {
        self.places.is_empty() && self.transitions.is_empty()
    }
}

impl Default for PetriNet {
    fn default() -> Self {
        Self::new()
    }
}

/// A Declare constraint
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DeclareConstraint {
    pub constraint_type: String,
    pub activities: Vec<String>,
    pub condition: String,
}

impl DeclareConstraint {
    pub fn new(constraint_type: String, activities: Vec<String>, condition: String) -> Self {
        DeclareConstraint {
            constraint_type,
            activities,
            condition,
        }
    }
}

/// A Declare process model
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct DeclareModel {
    pub constraints: Vec<DeclareConstraint>,
    pub activities: Vec<String>,
}

impl DeclareModel {
    pub fn new() -> Self {
        DeclareModel {
            constraints: Vec::new(),
            activities: Vec::new(),
        }
    }
}

impl Default for DeclareModel {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dfg_creation() {
        let dfg = DFG::new();
        assert!(dfg.is_empty());
    }

    #[test]
    fn test_petri_net_creation() {
        let pn = PetriNet::new();
        assert!(pn.is_empty());
    }
}
