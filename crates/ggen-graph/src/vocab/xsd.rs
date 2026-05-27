//! XML Schema Definition (XSD) Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// XSD Namespace URI.
pub const NAMESPACE: &str = "http://www.w3.org/2001/XMLSchema#";

/// `xsd:string`
pub const STRING: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#string");

/// `xsd:integer`
pub const INTEGER: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#integer");

/// `xsd:decimal`
pub const DECIMAL: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#decimal");

/// `xsd:double`
pub const DOUBLE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#double");

/// `xsd:float`
pub const FLOAT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#float");

/// `xsd:boolean`
pub const BOOLEAN: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#boolean");

/// `xsd:dateTime`
pub const DATE_TIME: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#dateTime");

/// `xsd:date`
pub const DATE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#date");

/// `xsd:time`
pub const TIME: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#time");

/// `xsd:duration`
pub const DURATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#duration");

/// `xsd:nonNegativeInteger`
pub const NON_NEGATIVE_INTEGER: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#nonNegativeInteger");

/// `xsd:positiveInteger`
pub const POSITIVE_INTEGER: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#positiveInteger");

/// `xsd:anyURI`
pub const ANY_URI: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#anyURI");

/// `xsd:base64Binary`
pub const BASE64_BINARY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#base64Binary");

/// `xsd:hexBinary`
pub const HEX_BINARY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/2001/XMLSchema#hexBinary");
