//! OCEL 2.0 (Object-Centric Process Event Log) Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// OCEL Namespace URI.
pub const NAMESPACE: &str = "http://www.ocel-standard.org/ns#";

/// `ocel:Log`
pub const LOG: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#Log");

/// `ocel:Event`
pub const EVENT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#Event");

/// `ocel:Object`
pub const OBJECT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#Object");

/// `ocel:hasEvent`
pub const HAS_EVENT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#hasEvent");

/// `ocel:hasObject`
pub const HAS_OBJECT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#hasObject");

/// `ocel:eventType`
pub const EVENT_TYPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#eventType");

/// `ocel:objectType`
pub const OBJECT_TYPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#objectType");

/// `ocel:timestamp`
pub const TIMESTAMP: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#timestamp");

/// `ocel:activity`
pub const ACTIVITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.ocel-standard.org/ns#activity");
