//! Emitted public vocabularies.

pub mod dfg;
pub mod ocel;
pub mod owl;
pub mod powl;
pub mod prov;
pub mod rdf;
pub mod rdfs;
pub mod shacl;
pub mod skos;
pub mod xsd;

/// RDF Namespace Constant
pub const RDF: &str = rdf::NAMESPACE;
/// RDF module alias (TitleCase)
pub use rdf as Rdf;
/// RDF module alias (UPPERCASE)
pub use rdf as RDF;

/// RDFS Namespace Constant
pub const RDFS: &str = rdfs::NAMESPACE;
/// RDFS module alias (TitleCase)
pub use rdfs as Rdfs;
/// RDFS module alias (UPPERCASE)
pub use rdfs as RDFS;

/// OWL Namespace Constant
pub const OWL: &str = owl::NAMESPACE;
/// OWL module alias (TitleCase)
pub use owl as Owl;
/// OWL module alias (UPPERCASE)
pub use owl as OWL;

/// XSD Namespace Constant
pub const XSD: &str = xsd::NAMESPACE;
/// XSD module alias (TitleCase)
pub use xsd as Xsd;
/// XSD module alias (UPPERCASE)
pub use xsd as XSD;

/// PROV Namespace Constant
pub const PROV: &str = prov::NAMESPACE;
/// PROV module alias (TitleCase)
pub use prov as Prov;
/// PROV module alias (UPPERCASE)
pub use prov as PROV;

/// SKOS Namespace Constant
pub const SKOS: &str = skos::NAMESPACE;
/// SKOS module alias (TitleCase)
pub use skos as Skos;
/// SKOS module alias (UPPERCASE)
pub use skos as SKOS;

/// SHACL Namespace Constant
pub const SHACL: &str = shacl::NAMESPACE;
/// SHACL module alias (TitleCase)
pub use shacl as Shacl;
/// SHACL module alias (UPPERCASE)
pub use shacl as SHACL;

/// OCEL Namespace Constant
pub const OCEL: &str = ocel::NAMESPACE;
/// OCEL module alias (TitleCase)
pub use ocel as Ocel;
/// OCEL module alias (UPPERCASE)
pub use ocel as OCEL;

/// POWL Namespace Constant
pub const POWL: &str = powl::NAMESPACE;
/// POWL module alias (TitleCase)
pub use powl as Powl;

/// DFG Namespace Constant
pub const DFG: &str = dfg::NAMESPACE;
/// DFG module alias (TitleCase)
pub use dfg as Dfg;
