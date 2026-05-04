//! Petri net data structures for process mining.
//!
//! Petri nets are a fundamental modeling formalism in process mining, used for
//! process discovery, conformance checking, and model transformation.
//! This implementation utilizes the high-performance types from pictl-types.

pub use pictl_types::models::{
    PetriNet, PetriNetArc as Arc, PetriNetPlace as Place, PetriNetTransition as Transition,
};

/// Marking represents the distribution of tokens in places.
/// Note: pictl-types doesn't expose a dedicated Marking struct yet,
/// we implement a simple wrapper here for compatibility.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Marking {
    /// Tokens in each place
    pub tokens: std::collections::HashMap<String, usize>,
}

impl Marking {
    /// Create a new marking
    pub fn new() -> Self {
        Self::default()
    }
}

/// Extension trait for PetriNet to maintain compatibility with ggen API.
pub trait PetriNetExt {
    /// Export this Petri Net as a Semantic OS Law (Turtle format)
    fn to_sos_law(&self, law_id: &str, name: &str) -> String;
}

impl PetriNetExt for PetriNet {
    fn to_sos_law(&self, law_id: &str, name: &str) -> String {
        let mut ttl = format!(
            r#"@prefix sos: <http://seanchatmangpt.com/sos#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<{law_id}> a sos:Law ;
    rdfs:label "{name}" ;
    sos:implementation "petri-net" .
"#
        );

        for place in &self.places {
            ttl.push_str(&format!(
                r#"
<{law_id}/place/{id}> a sos:Place ;
    rdfs:label "{label}" .
"#,
                id = place.id,
                label = place.label
            ));
        }

        for trans in &self.transitions {
            ttl.push_str(&format!(
                r#"
<{law_id}/transition/{id}> a sos:Transition ;
    rdfs:label "{label}" ;
    sos:isInvisible {invisible} .
"#,
                id = trans.id,
                label = trans.label,
                invisible = trans.invisible
            ));
        }

        for (i, arc) in self.arcs.iter().enumerate() {
            ttl.push_str(&format!(
                r#"
<{law_id}/arc/{i}> a sos:Arc ;
    sos:source <{law_id}/{src_type}/{source}> ;
    sos:target <{law_id}/{target_type}/{target}> ;
    sos:weight {weight} .
"#,
                i = i,
                src_type = if self.places.iter().any(|p| p.id == arc.source) { "place" } else { "transition" },
                source = arc.source,
                target_type = if self.places.iter().any(|p| p.id == arc.target) { "place" } else { "transition" },
                target = arc.target,
                weight = arc.weight
            ));
        }

        ttl
    }
}
