//! RDF Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// RDF Namespace URI.
pub const NAMESPACE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";

/// `rdf:type`
pub const TYPE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");

/// `rdf:Property`
pub const PROPERTY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#Property");

/// `rdf:Statement`
pub const STATEMENT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement");

/// `rdf:subject`
pub const SUBJECT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#subject");

/// `rdf:predicate`
pub const PREDICATE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate");

/// `rdf:object`
pub const OBJECT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#object");

/// `rdf:nil`
pub const NIL: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil");

/// `rdf:first`
pub const FIRST: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#first");

/// `rdf:rest`
pub const REST: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest");

/// `rdf:List`
pub const LIST: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#List");

/// `rdf:Bag`
pub const BAG: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#Bag");

/// `rdf:Alt`
pub const ALT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#Alt");

/// `rdf:Seq`
pub const SEQ: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq");

/// `rdf:value`
pub const VALUE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/1999/02/22-rdf-syntax-ns#value");
