//! W3C PROV-O Vocabulary constants.

use oxigraph::model::NamedNodeRef;

/// PROV Namespace URI.
pub const NAMESPACE: &str = "http://www.w3.org/ns/prov#";

/// `prov:Entity`
pub const ENTITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Entity");

/// `prov:Activity`
pub const ACTIVITY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Activity");

/// `prov:Agent`
pub const AGENT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Agent");

/// `prov:Person`
pub const PERSON: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Person");

/// `prov:Organization`
pub const ORGANIZATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Organization");

/// `prov:SoftwareAgent`
pub const SOFTWARE_AGENT: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#SoftwareAgent");

/// `prov:Location`
pub const LOCATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Location");

/// `prov:Bundle`
pub const BUNDLE: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Bundle");

/// `prov:Collection`
pub const COLLECTION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Collection");

/// `prov:EmptyCollection`
pub const EMPTY_COLLECTION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#EmptyCollection");

/// `prov:Plan`
pub const PLAN: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#Plan");

/// `prov:wasGeneratedBy`
pub const WAS_GENERATED_BY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasGeneratedBy");

/// `prov:wasDerivedFrom`
pub const WAS_DERIVED_FROM: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasDerivedFrom");

/// `prov:wasAttributedTo`
pub const WAS_ATTRIBUTED_TO: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasAttributedTo");

/// `prov:startedAtTime`
pub const STARTED_AT_TIME: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#startedAtTime");

/// `prov:endedAtTime`
pub const ENDED_AT_TIME: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#endedAtTime");

/// `prov:used`
pub const USED: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#used");

/// `prov:wasInformedBy`
pub const WAS_INFORMED_BY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasInformedBy");

/// `prov:wasStartedBy`
pub const WAS_STARTED_BY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasStartedBy");

/// `prov:wasEndedBy`
pub const WAS_ENDED_BY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasEndedBy");

/// `prov:wasInvalidatedBy`
pub const WAS_INVALIDATED_BY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasInvalidatedBy");

/// `prov:wasInfluencedBy`
pub const WAS_INFLUENCED_BY: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasInfluencedBy");

/// `prov:atLocation`
pub const AT_LOCATION: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#atLocation");

/// `prov:generated`
pub const GENERATED: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#generated");

/// `prov:wasAssociatedWith`
pub const WAS_ASSOCIATED_WITH: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#wasAssociatedWith");

/// `prov:actedOnBehalfOf`
pub const ACTED_ON_BEHALF_OF: NamedNodeRef<'static> =
    NamedNodeRef::new_unchecked("http://www.w3.org/ns/prov#actedOnBehalfOf");
