//! RDF Projection System for the ggen membrane.
//!
//! Projects membrane bindings, adapter layers, and boundary events into RDF Turtle representation,
//! integrated with W3C PROV-O ontology.

use crate::graph::Graph;
use crate::utils::error::Result;
use super::core::{GgenMembrane, BoundaryCrossing, InterchangeablePart};

/// RDF Projector for projecting membrane state into an RDF Graph
pub struct RdfMembraneProjector;

impl RdfMembraneProjector {
    /// Project the GgenMembrane state into an RDF Graph
    pub fn project(membrane: &GgenMembrane) -> Result<Graph> {
        let graph = Graph::new()?;

        // Prefix statements
        let base_ttl = r#"
            @prefix mem: <http://ggen.org/membrane#> .
            @prefix prov: <http://www.w3.org/ns/prov#> .
            @prefix ocel: <http://www.ocel-standard.org/ns#> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        "#;
        graph.insert_turtle(base_ttl)?;

        // 1. Project interchangeable parts as entities
        for (part_id, part) in &membrane.core.parts {
            let part_ttl = format!(
                r#"
                mem:part_{part_id} a mem:InterchangeablePart , prov:Entity ;
                    rdfs:label "Part {part_id}" ;
                    mem:partType "{part_type}" ;
                    mem:version "{version}" ;
                    mem:payloadHash "{hash}" ;
                    mem:payloadSize {size} ;
                    prov:wasAttributedTo mem:genesis_core .
                "#,
                part_id = part_id,
                part_type = part.part_type,
                version = part.version,
                hash = part.payload_hash,
                size = part.payload_size
            );
            graph.insert_turtle(&part_ttl)?;

            // Add interfaces
            for interface in &part.interfaces {
                let interface_ttl = format!(
                    r#"
                    mem:part_{part_id} mem:implementsInterface "{interface}" .
                    "#,
                    part_id = part_id,
                    interface = interface
                );
                graph.insert_turtle(&interface_ttl)?;
            }
        }

        // 2. Project core/agent entities
        let agents_ttl = r#"
            mem:genesis_core a prov:Agent , mem:GenesisCore ;
                rdfs:label "Genesis Embedded Core" .

            mem:ggen_membrane a prov:Agent , mem:GgenMembrane ;
                rdfs:label "ggen Membrane Adapter Layer" .
        "#;
        graph.insert_turtle(agents_ttl)?;

        // 3. Project adapter bindings
        for (outer_port, inner_interface) in &membrane.adapters {
            let adapter_ttl = format!(
                r#"
                mem:adapter_{outer_port} a mem:AdapterBinding , prov:Entity ;
                    rdfs:label "Adapter for Port {outer_port}" ;
                    mem:outerPort "{outer_port}" ;
                    mem:innerInterface "{inner_interface}" ;
                    prov:wasAttributedTo mem:ggen_membrane .
                "#,
                outer_port = outer_port,
                inner_interface = inner_interface
            );
            graph.insert_turtle(&adapter_ttl)?;
        }

        // 4. Project boundary crossing events
        for crossing in &membrane.event_log {
            // Check output hash presence safely
            let output_clause = if let Some(ref oh) = crossing.output_hash {
                format!(
                    r#"
                    mem:output_{id} a mem:OutputPayload , prov:Entity ;
                        mem:payloadHash "{oh}" ;
                        mem:statusCode {status} ;
                        prov:wasGeneratedBy mem:crossing_{id} .

                    mem:output_{id} prov:wasDerivedFrom mem:input_{id} .
                    "#,
                    id = crossing.id,
                    oh = oh,
                    status = crossing.status_code
                )
            } else {
                "".to_string()
            };

            let crossing_ttl = format!(
                r#"
                mem:crossing_{id} a mem:BoundaryCrossing , prov:Activity , ocel:Event ;
                    rdfs:label "Execution of {interface_fn}" ;
                    mem:crossingType "{crossing_type}" ;
                    mem:interfaceFn "{interface_fn}" ;
                    mem:durationUs {duration} ;
                    prov:startedAtTime "{timestamp}"^^xsd:dateTime ;
                    prov:wasAssociatedWith mem:ggen_membrane ;
                    prov:used mem:part_{callee} ;
                    prov:used mem:input_{id} .

                mem:input_{id} a mem:InputPayload , prov:Entity ;
                    mem:payloadHash "{input_hash}" .

                {output_clause}
                "#,
                id = crossing.id,
                interface_fn = crossing.interface_fn,
                crossing_type = crossing.crossing_type,
                duration = crossing.duration_us,
                timestamp = crossing.timestamp.to_rfc3339(),
                callee = crossing.callee_id,
                input_hash = crossing.input_hash,
                output_clause = output_clause
            );
            graph.insert_turtle(&crossing_ttl)?;

            // Vector clocks projection
            for (node, clock_val) in &crossing.vector_clock.clocks {
                let clock_ttl = format!(
                    r#"
                    mem:crossing_{id} mem:vectorClock [
                        mem:clockNode "{node}" ;
                        mem:clockVal {val}
                    ] .
                    "#,
                    id = crossing.id,
                    node = node,
                    val = clock_val
                );
                graph.insert_turtle(&clock_ttl)?;
            }
        }

        Ok(graph)
    }
}
