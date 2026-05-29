//! POWL (Partially Ordered Workflow Language) vocabulary for representing
//! repair routes as RDF.
//!
//! A repair route is a POWL model: a partial order of steps with choice, loop,
//! and silent operators. Representing it as triples makes routes themselves
//! SPARQL-queryable and conformance-checkable against the OCEL-RDF event log —
//! a route's `guard`/`loopCondition` are inline SPARQL ASK strings (OCPQ-style
//! constraints). See `crates/ggen-graph/src/ocel/conformance.rs`.

use oxigraph::model::NamedNodeRef;

/// POWL namespace URI.
pub const NAMESPACE: &str = "http://ggen.dev/powl#";

// ---- Classes ---------------------------------------------------------------

/// `powl:Route` — a complete repair route (partial order of steps).
pub const ROUTE: NamedNodeRef<'static> = NamedNodeRef::new_unchecked("http://ggen.dev/powl#Route");

/// `powl:Step` — an atomic repair action (maps to an OCEL activity).
pub const STEP: NamedNodeRef<'static> = NamedNodeRef::new_unchecked("http://ggen.dev/powl#Step");

/// `powl:Edge` — a partial-order / control-flow edge between steps.
pub const EDGE: NamedNodeRef<'static> = NamedNodeRef::new_unchecked("http://ggen.dev/powl#Edge");

/// `powl:XorChoice` — exclusive choice operator.
pub const XOR_CHOICE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#XorChoice");

/// `powl:Loop` — do/redo loop operator.
pub const LOOP: NamedNodeRef<'static> = NamedNodeRef::new_unchecked("http://ggen.dev/powl#Loop");

/// `powl:SilentStep` — invisible (tau) transition.
pub const SILENT_STEP: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#SilentStep");

// ---- Properties ------------------------------------------------------------

/// `powl:hasStep` — route → step.
pub const HAS_STEP: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#hasStep");

/// `powl:hasEdge` — route → edge.
pub const HAS_EDGE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#hasEdge");

/// `powl:source` — edge → source step (must precede).
pub const SOURCE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#source");

/// `powl:target` — edge → target step.
pub const TARGET: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#target");

/// `powl:edgeType` — "sequence" | "choice" | "loop" | "silent".
pub const EDGE_TYPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#edgeType");

/// `powl:activity` — step → OCEL activity name it corresponds to.
pub const ACTIVITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#activity");

/// `powl:family` — route → diagnostic family it repairs.
pub const FAMILY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#family");

/// `powl:guard` — step/edge → SPARQL ASK precondition (OCPQ constraint).
pub const GUARD: NamedNodeRef<'static> = NamedNodeRef::new_unchecked("http://ggen.dev/powl#guard");

/// `powl:loopCondition` — loop → SPARQL ASK continuation condition.
pub const LOOP_CONDITION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://ggen.dev/powl#loopCondition");
