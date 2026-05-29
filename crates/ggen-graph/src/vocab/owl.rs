//! Web Ontology Language (OWL) Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// OWL Namespace URI.
pub const NAMESPACE: &str = "http://www.w3.org/2002/07/owl#";

/// `owl:Thing`
pub const THING: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#Thing");

/// `owl:Nothing`
pub const NOTHING: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#Nothing");

/// `owl:Class`
pub const CLASS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#Class");

/// `owl:ObjectProperty`
pub const OBJECT_PROPERTY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#ObjectProperty");

/// `owl:DatatypeProperty`
pub const DATATYPE_PROPERTY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#DatatypeProperty");

/// `owl:equivalentClass`
pub const EQUIVALENT_CLASS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#equivalentClass");

/// `owl:equivalentProperty`
pub const EQUIVALENT_PROPERTY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#equivalentProperty");

/// `owl:sameAs`
pub const SAME_AS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#sameAs");

/// `owl:differentFrom`
pub const DIFFERENT_FROM: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#differentFrom");

/// `owl:Restriction`
pub const RESTRICTION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#Restriction");

/// `owl:onProperty`
pub const ON_PROPERTY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#onProperty");

/// `owl:someValuesFrom`
pub const SOME_VALUES_FROM: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#someValuesFrom");

/// `owl:allValuesFrom`
pub const ALL_VALUES_FROM: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#allValuesFrom");

/// `owl:hasValue`
pub const HAS_VALUE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#hasValue");

/// `owl:minCardinality`
pub const MIN_CARDINALITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#minCardinality");

/// `owl:maxCardinality`
pub const MAX_CARDINALITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#maxCardinality");

/// `owl:cardinality`
pub const CARDINALITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#cardinality");

/// `owl:AnnotationProperty`
pub const ANNOTATION_PROPERTY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#AnnotationProperty");

/// `owl:Ontology`
pub const ONTOLOGY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2002/07/owl#Ontology");
