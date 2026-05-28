//! Directly-Follows Graph (DFG) vocabulary for SPARQL-mined process edges.
//!
//! These IRIs let a discovered DFG be materialized as RDF triples
//! (`?a1 dfg:directlyFollows ?a2`) so failure-edge mining is a SPARQL CONSTRUCT
//! over the OCEL-RDF event log rather than an external process-mining engine.

use oxigraph::model::NamedNodeRef;

/// DFG namespace URI.
pub const NAMESPACE: &str = "http://ggen.dev/dfg#";

/// `dfg:directlyFollows` — activity A is directly followed by activity B.
pub const DIRECTLY_FOLLOWS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/dfg#directlyFollows");

/// `dfg:frequency` — observed count of a directly-follows pair.
pub const FREQUENCY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/dfg#frequency");
