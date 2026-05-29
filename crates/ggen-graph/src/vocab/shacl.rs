//! W3C Shapes Constraint Language (SHACL) Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// SHACL Namespace URI.
pub const NAMESPACE: &str = "http://www.w3.org/ns/shacl#";

/// `shacl:Shape`
pub const SHAPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#Shape");

/// `shacl:NodeShape`
pub const NODE_SHAPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#NodeShape");

/// `shacl:PropertyShape`
pub const PROPERTY_SHAPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#PropertyShape");

/// `shacl:targetClass`
pub const TARGET_CLASS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#targetClass");

/// `shacl:targetNode`
pub const TARGET_NODE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#targetNode");

/// `shacl:targetSubjectsOf`
pub const TARGET_SUBJECTS_OF: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#targetSubjectsOf");

/// `shacl:targetObjectsOf`
pub const TARGET_OBJECTS_OF: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#targetObjectsOf");

/// `shacl:property`
pub const PROPERTY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#property");

/// `shacl:path`
pub const PATH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#path");

/// `shacl:class`
pub const CLASS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#class");

/// `shacl:datatype`
pub const DATATYPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#datatype");

/// `shacl:minCount`
pub const MIN_COUNT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#minCount");

/// `shacl:maxCount`
pub const MAX_COUNT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#maxCount");

/// `shacl:minExclusive`
pub const MIN_EXCLUSIVE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#minExclusive");

/// `shacl:minInclusive`
pub const MIN_INCLUSIVE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#minInclusive");

/// `shacl:maxExclusive`
pub const MAX_EXCLUSIVE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#maxExclusive");

/// `shacl:maxInclusive`
pub const MAX_INCLUSIVE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#maxInclusive");

/// `shacl:minLength`
pub const MIN_LENGTH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#minLength");

/// `shacl:maxLength`
pub const MAX_LENGTH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#maxLength");

/// `shacl:pattern`
pub const PATTERN: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#pattern");

/// `shacl:flags`
pub const FLAGS: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#flags");

/// `shacl:languageIn`
pub const LANGUAGE_IN: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#languageIn");

/// `shacl:uniqueLang`
pub const UNIQUE_LANG: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#uniqueLang");

/// `shacl:qualifiedValueShape`
pub const QUALIFIED_VALUE_SHAPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#qualifiedValueShape");

/// `shacl:qualifiedMinCount`
pub const QUALIFIED_MIN_COUNT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#qualifiedMinCount");

/// `shacl:qualifiedMaxCount`
pub const QUALIFIED_MAX_COUNT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#qualifiedMaxCount");

/// `shacl:closed`
pub const CLOSED: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#closed");

/// `shacl:ignoredProperties`
pub const IGNORED_PROPERTIES: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#ignoredProperties");

/// `shacl:hasValue`
pub const HAS_VALUE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#hasValue");

/// `shacl:in`
pub const IN: NamedNodeRef<'static> = NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#in");

/// `shacl:conform`
pub const CONFORM: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#conform");

/// `shacl:result`
pub const RESULT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#result");

/// `shacl:resultSeverity`
pub const RESULT_SEVERITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#resultSeverity");

/// `shacl:resultMessage`
pub const RESULT_MESSAGE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#resultMessage");

/// `shacl:sourceConstraintComponent`
pub const SOURCE_CONSTRAINT_COMPONENT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#sourceConstraintComponent");

/// `shacl:sourceShape`
pub const SOURCE_SHAPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#sourceShape");

/// `shacl:focusNode`
pub const FOCUS_NODE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#focusNode");

/// `shacl:value`
pub const VALUE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#value");

/// `shacl:severity`
pub const SEVERITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#severity");

/// `shacl:Violation`
pub const VIOLATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#Violation");

/// `shacl:Warning`
pub const WARNING: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#Warning");

/// `shacl:Info`
pub const INFO: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/shacl#Info");
