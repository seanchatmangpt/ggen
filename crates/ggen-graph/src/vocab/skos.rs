//! W3C Simple Knowledge Organization System (SKOS) Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// SKOS Namespace URI.
pub const NAMESPACE: &str = "http://www.w3.org/2004/02/skos/core#";

/// `skos:Concept`
pub const CONCEPT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#Concept");

/// `skos:ConceptScheme`
pub const CONCEPT_SCHEME: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#ConceptScheme");

/// `skos:Collection`
pub const COLLECTION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#Collection");

/// `skos:OrderedCollection`
pub const ORDERED_COLLECTION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#OrderedCollection");

/// `skos:inScheme`
pub const IN_SCHEME: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#inScheme");

/// `skos:hasTopConcept`
pub const HAS_TOP_CONCEPT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#hasTopConcept");

/// `skos:topConceptOf`
pub const TOP_CONCEPT_OF: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#topConceptOf");

/// `skos:prefLabel`
pub const PREF_LABEL: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#prefLabel");

/// `skos:altLabel`
pub const ALT_LABEL: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#altLabel");

/// `skos:hiddenLabel`
pub const HIDDEN_LABEL: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#hiddenLabel");

/// `skos:notation`
pub const NOTATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#notation");

/// `skos:note`
pub const NOTE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#note");

/// `skos:changeNote`
pub const CHANGE_NOTE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#changeNote");

/// `skos:definition`
pub const DEFINITION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#definition");

/// `skos:editorialNote`
pub const EDITORIAL_NOTE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#editorialNote");

/// `skos:historyNote`
pub const HISTORY_NOTE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#historyNote");

/// `skos:scopeNote`
pub const SCOPE_NOTE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#scopeNote");

/// `skos:broader`
pub const BROADER: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#broader");

/// `skos:narrower`
pub const NARROWER: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#narrower");

/// `skos:related`
pub const RELATED: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#related");

/// `skos:broaderTransitive`
pub const BROADER_TRANSITIVE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#broaderTransitive");

/// `skos:narrowerTransitive`
pub const NARROWER_TRANSITIVE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#narrowerTransitive");

/// `skos:member`
pub const MEMBER: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#member");

/// `skos:memberList`
pub const MEMBER_LIST: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#memberList");

/// `skos:semanticRelation`
pub const SEMANTIC_RELATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#semanticRelation");

/// `skos:closeMatch`
pub const CLOSE_MATCH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#closeMatch");

/// `skos:exactMatch`
pub const EXACT_MATCH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#exactMatch");

/// `skos:broadMatch`
pub const BROAD_MATCH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#broadMatch");

/// `skos:narrowMatch`
pub const NARROW_MATCH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#narrowMatch");

/// `skos:relatedMatch`
pub const RELATED_MATCH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2004/02/skos/core#relatedMatch");
