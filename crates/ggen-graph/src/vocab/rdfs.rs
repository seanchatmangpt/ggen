//! RDF Schema (RDFS) Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// RDFS Namespace URI.
pub const NAMESPACE: &str = "http://www.w3.org/2000/01/rdf-schema#";

/// `rdfs:Class`
pub const CLASS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#Class");

/// `rdfs:subClassOf`
pub const SUB_CLASS_OF: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#subClassOf");

/// `rdfs:subPropertyOf`
pub const SUB_PROPERTY_OF: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#subPropertyOf");

/// `rdfs:domain`
pub const DOMAIN: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#domain");

/// `rdfs:range`
pub const RANGE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#range");

/// `rdfs:label`
pub const LABEL: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#label");

/// `rdfs:comment`
pub const COMMENT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#comment");

/// `rdfs:seeAlso`
pub const SEE_ALSO: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#seeAlso");

/// `rdfs:isDefinedBy`
pub const IS_DEFINED_BY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#isDefinedBy");

/// `rdfs:Resource`
pub const RESOURCE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#Resource");

/// `rdfs:Literal`
pub const LITERAL: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#Literal");

/// `rdfs:Datatype`
pub const DATATYPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2000/01/rdf-schema#Datatype");
