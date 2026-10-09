//! ggen semantic-kernel law layer (DoD#9, metamodel hygiene, authority pin,
//! namespace isolation) — the GraphLaw rule layer that every pack ontology and
//! every LawState ingestion passes through before admission.
//!
//! Design constraints:
//! * The law layer is pure string triples, IO-free and parser-free: every
//!   caller (the pest/rio parser, ggen-engine's N-Triples seam) can project
//!   terms to `&str` without crossing praxis-graphlaw's oxrdf version seam.
//! * Tests feed it real parsed triples from the repo's own pack ontologies
//!   (see `tests/ggen_law_pack_audit.rs`), never fixtures this module
//!   fabricates.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

/// The pinned authority root. Every core-term IRI in the ea/togaf family MUST
/// live under this root; anything in the family outside it is
/// [`GgenLawError::AuthorityRootUnpinned`] (fail-closed: no allow-list).
pub const AUTHORITY_ROOT: &str = "https://spec.chatmangpt.com/ea/v1#";

/// The namespace every pack's asserted facts project into.
pub const DOMAIN_NS: &str = "urn:domain#";

/// Prefix of every pack's isolated ingestion namespace:
/// `urn:ggen:pack:<name>#`.
pub const PACK_NS_PREFIX: &str = "urn:ggen:pack:";

/// The law classes DoD#9 keeps pairwise disjoint (all under the pinned root).
pub const LAW_CLASSES: [&str; 3] = [
    "https://spec.chatmangpt.com/ea/v1#Pack",
    "https://spec.chatmangpt.com/ea/v1#Abb",
    "https://spec.chatmangpt.com/ea/v1#Sbb",
];

/// Standard vocabularies a pack may use in a projection without leaking
/// pack-internal vocabulary.
const STANDARD_VOCABS: [&str; 5] = [
    "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    "http://www.w3.org/2000/01/rdf-schema#",
    "http://www.w3.org/2002/07/owl#",
    "http://www.w3.org/2004/02/skos/core#",
    "http://purl.org/dc/terms/",
];

/// IRIs in the ea/togaf family that MUST be pinned to [`AUTHORITY_ROOT`].
const FAMILY_PREFIXES: [&str; 3] = ["ea:", "togaf:", "http://www.opengroup.org/togaf"];

/// A `(subject, predicate, object)` fact as plain strings.
pub type LawTriple = (String, String, String);

/// Typed law errors. `Display` starts with the stable error code so receipts
/// and refusals can be matched on the code alone.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GgenLawError {
    /// DoD#9: one IRI claimed two of Pack/ABB/SBB.
    Dod9Collision { iri: String, roles: Vec<String> },
    /// A non-core source axiomatizes a core term, or a core term is redefined
    /// as an external class.
    MetamodelHygieneViolation {
        predicate: String,
        subject: String,
        object: String,
    },
    /// An ea/togaf-family IRI outside the pinned root.
    AuthorityRootUnpinned { iri: String },
    /// A pack-internal term escaped into the domain projection.
    NamespaceLeak { pack: String, term: String },
}

impl fmt::Display for GgenLawError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dod9Collision { iri, roles } => write!(
                f,
                "E_DOD9_COLLISION: {iri} is typed as {} (Pack, ABB, SBB are pairwise disjoint)",
                roles.join(" and ")
            ),
            Self::MetamodelHygieneViolation {
                predicate,
                subject,
                object,
            } => write!(
                f,
                "E_METAMODEL_HYGIENE_VIOLATION: {subject} {predicate} {object} -- packs are \
                 strict consumers; core terms under {AUTHORITY_ROOT} may be used, never \
                 defined or redefined from outside the root"
            ),
            Self::AuthorityRootUnpinned { iri } => write!(
                f,
                "E_AUTHORITY_ROOT_UNPINNED: {iri} is in the ea/togaf family but not under \
                 the pinned root {AUTHORITY_ROOT}"
            ),
            Self::NamespaceLeak { pack, term } => write!(
                f,
                "E_NAMESPACE_LEAK: pack-internal term {term} of pack {pack} escaped into \
                 the domain projection"
            ),
        }
    }
}

/// True if `iri` is in the ea/togaf family (the must-be-pinned set).
pub fn is_family_iri(iri: &str) -> bool {
    FAMILY_PREFIXES.iter().any(|p| iri.starts_with(p))
}

/// True if `iri` is under the pinned authority root.
pub fn is_pinned(iri: &str) -> bool {
    iri.starts_with(AUTHORITY_ROOT)
}

/// Strip angle brackets around a term string, if present.
fn bare(iri: &str) -> &str {
    iri.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(iri)
}

/// Fail-closed authority pin: any ea/togaf-family IRI must sit under
/// [`AUTHORITY_ROOT`].
pub fn check_authority_pin(iri: &str) -> Result<(), GgenLawError> {
    if is_family_iri(iri) && !is_pinned(iri) {
        return Err(GgenLawError::AuthorityRootUnpinned { iri: iri.into() });
    }
    Ok(())
}

/// DoD#9 direct form: Pack != ABB != SBB, pairwise, on three identities.
pub fn check_dod9_disjoint(pack: &str, abb: &str, sbb: &str) -> Result<(), GgenLawError> {
    let collide = |iri: &str, roles: &[&str]| GgenLawError::Dod9Collision {
        iri: format!("<{iri}>"),
        roles: roles.iter().map(|r| r.to_string()).collect(),
    };
    if pack == abb {
        return Err(collide(pack, &["Pack", "Abb"]));
    }
    if pack == sbb {
        return Err(collide(pack, &["Pack", "Sbb"]));
    }
    if abb == sbb {
        return Err(collide(abb, &["Abb", "Sbb"]));
    }
    Ok(())
}

/// The rdf:type IRI.
pub const RDF_TYPE: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type";

/// DoD#9 graph form: scan `(s, rdf:type, C)` facts; any subject typed as two
/// distinct law classes is a collision.
pub fn scan_dod9(triples: &[LawTriple]) -> Result<(), GgenLawError> {
    let mut roles: BTreeMap<&str, BTreeSet<&str>> = BTreeMap::new();
    for (s, p, o) in triples {
        let (s, p, o) = (bare(s), bare(p), bare(o));
        if p != RDF_TYPE {
            continue;
        }
        let Some(class) = LAW_CLASSES.iter().find(|c| **c == o) else {
            continue;
        };
        let entry = roles.entry(s).or_default();
        entry.insert(class_role(class));
        if entry.len() > 1 {
            return Err(GgenLawError::Dod9Collision {
                iri: format!("<{s}>"),
                roles: entry.iter().map(|r| r.to_string()).collect(),
            });
        }
    }
    Ok(())
}

fn class_role(class: &str) -> &'static str {
    if class == LAW_CLASSES[0] {
        "Pack"
    } else if class == LAW_CLASSES[1] {
        "Abb"
    } else {
        "Sbb"
    }
}

/// Metamodel hygiene over a parsed ontology document. For every
/// `owl:equivalentClass` / `rdfs:subClassOf` axiom:
///
/// * subject outside the pinned root and object under it -> violation
///   (external definition of a core term);
/// * subject under the root and object outside it -> violation
///   (a core term redefined as an external class);
/// * both outside -> allowed (pack-local hierarchies are pack-internal);
/// * both under -> allowed (the core's own axioms).
///
/// Every ea/togaf-family IRI appearing in such an axiom is additionally
/// authority-pin-checked (fail-closed on unpinned URIs).
pub fn check_hygiene(triples: &[LawTriple]) -> Result<(), GgenLawError> {
    const EQUIV: &str = "http://www.w3.org/2002/07/owl#equivalentClass";
    const SUBCLASS: &str = "http://www.w3.org/2000/01/rdf-schema#subClassOf";
    for (s, p, o) in triples {
        let (s, p, o) = (bare(s), bare(p), bare(o));
        if p != EQUIV && p != SUBCLASS {
            continue;
        }
        if o.starts_with('"') {
            continue; // literal object: not a class axiom
        }
        check_authority_pin(s)?;
        check_authority_pin(o)?;
        if is_pinned(s) != is_pinned(o) {
            return Err(GgenLawError::MetamodelHygieneViolation {
                predicate: p.to_string(),
                subject: s.to_string(),
                object: o.to_string(),
            });
        }
    }
    Ok(())
}

/// LawState ingestion: one pack's isolated namespace
/// (`urn:ggen:pack:<name>#`) and its projection into the domain graph.
pub struct PackIngest {
    pack: String,
    pack_ns: String,
}

impl PackIngest {
    /// New ingestion scope for `pack`. Refuses an empty name or a name
    /// containing `#` (it would forge the namespace boundary).
    pub fn new(pack: &str) -> Result<Self, GgenLawError> {
        if pack.is_empty() || pack.contains('#') {
            return Err(GgenLawError::NamespaceLeak {
                pack: pack.to_string(),
                term: format!("invalid pack name {pack:?} (empty or contains '#')"),
            });
        }
        Ok(Self {
            pack_ns: format!("{PACK_NS_PREFIX}{pack}#"),
            pack: pack.to_string(),
        })
    }

    pub fn pack(&self) -> &str {
        &self.pack
    }

    pub fn pack_ns(&self) -> &str {
        &self.pack_ns
    }

    /// Project pack facts into [`DOMAIN_NS`] without leaky assertions.
    ///
    /// Rules (fail-closed on every escape):
    /// * subject in pack ns -> rewritten to `urn:domain#<local>`;
    /// * subject already in domain ns -> kept (asserting the projection
    ///   target directly);
    /// * subject anywhere else -> [`GgenLawError::NamespaceLeak`];
    /// * predicate/object in pack ns -> leak (pack-internal vocabulary and
    ///   classes never appear in the domain projection);
    /// * predicate/object in domain ns, the authority root, a standard vocab,
    ///   or (object only) a literal -> kept;
    /// * any other IRI -> leak.
    pub fn project(&self, triples: &[LawTriple]) -> Result<Vec<LawTriple>, GgenLawError> {
        let ns = self.pack_ns.as_str();
        let mut out = Vec::with_capacity(triples.len());
        for (s, p, o) in triples {
            let (s, p, o) = (bare(s), bare(p), bare(o));
            let subject = if let Some(local) = s.strip_prefix(ns) {
                format!("{DOMAIN_NS}{local}")
            } else if s.starts_with(DOMAIN_NS) {
                s.to_string()
            } else {
                return Err(GgenLawError::NamespaceLeak {
                    pack: self.pack.clone(),
                    term: format!("<{s}>"),
                });
            };
            let predicate = if p.starts_with(ns) || allowed_projection_term(p).is_none() {
                return Err(GgenLawError::NamespaceLeak {
                    pack: self.pack.clone(),
                    term: format!("<{p}>"),
                });
            } else {
                p.to_string()
            };
            let object = if o.starts_with('"') {
                o.to_string()
            } else if o.starts_with(ns) {
                return Err(GgenLawError::NamespaceLeak {
                    pack: self.pack.clone(),
                    term: format!("<{o}>"),
                });
            } else if allowed_projection_term(o).is_some() || o.starts_with("urn:") {
                o.to_string()
            } else {
                return Err(GgenLawError::NamespaceLeak {
                    pack: self.pack.clone(),
                    term: format!("<{o}>"),
                });
            };
            out.push((subject, predicate, object));
        }
        // Postcondition: no pack-internal term survives projection.
        debug_assert!(out.iter().all(|(s, p, o)| {
            ![s.as_str(), p.as_str(), o.as_str()]
                .iter()
                .any(|t| t.starts_with(ns))
        }));
        Ok(out)
    }
}

/// Some(term itself) if `iri` may appear (as predicate or object) in a domain
/// projection: domain ns, pinned authority root, or a standard vocabulary.
fn allowed_projection_term(iri: &str) -> Option<()> {
    if iri.starts_with(DOMAIN_NS)
        || is_pinned(iri)
        || STANDARD_VOCABS.iter().any(|v| iri.starts_with(v))
    {
        Some(())
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dod9_direct_accepts_distinct_identities() {
        assert!(check_dod9_disjoint("affidavit-pack", "abb:report", "sbb:md-to-pdf").is_ok());
    }

    #[test]
    fn dod9_direct_refuses_each_pairwise_collision() {
        for (p, a, s) in [
            ("x", "x", "y"),
            ("x", "y", "x"),
            ("x", "y", "y"),
        ] {
            let err = check_dod9_disjoint(p, a, s).unwrap_err();
            assert!(
                err.to_string().starts_with("E_DOD9_COLLISION"),
                "got: {err}"
            );
        }
    }

    #[test]
    fn dod9_graph_scan_refuses_iri_typed_as_two_roles() {
        let pack = "<urn:ggen:pack:demo#it>";
        let root = AUTHORITY_ROOT;
        let triples = vec![
            (pack.into(), format!("<{RDF_TYPE}>"), format!("<{root}Pack>")),
            (pack.into(), format!("<{RDF_TYPE}>"), format!("<{root}Sbb>")),
        ];
        let err = scan_dod9(&triples).unwrap_err();
        assert!(err.to_string().contains("E_DOD9_COLLISION"), "got: {err}");
        assert!(err.to_string().contains("Pack and Sbb"), "got: {err}");
    }

    #[test]
    fn dod9_graph_scan_accepts_single_role() {
        let root = AUTHORITY_ROOT;
        let triples = vec![(
            "<urn:ggen:pack:demo#it>".into(),
            format!("<{RDF_TYPE}>"),
            format!("<{root}Abb>"),
        )];
        assert!(scan_dod9(&triples).is_ok());
    }

    #[test]
    fn hygiene_accepts_pack_local_hierarchy() {
        let triples = vec![(
            "<urn:ggen:pack:demo#Child>".into(),
            "<http://www.w3.org/2000/01/rdf-schema#subClassOf>".into(),
            "<urn:ggen:pack:demo#Parent>".into(),
        )];
        assert!(check_hygiene(&triples).is_ok());
    }

    #[test]
    fn hygiene_refuses_external_definition_of_core_term() {
        let triples = vec![(
            "<urn:ggen:pack:demo#Report>".into(),
            "<http://www.w3.org/2002/07/owl#equivalentClass>".into(),
            format!("<{}CoreArtifact>", AUTHORITY_ROOT),
        )];
        let err = check_hygiene(&triples).unwrap_err();
        assert!(
            err.to_string()
                .starts_with("E_METAMODEL_HYGIENE_VIOLATION"),
            "got: {err}"
        );
    }

    #[test]
    fn hygiene_refuses_core_term_redefined_as_external() {
        let triples = vec![(
            format!("<{}Pack>", AUTHORITY_ROOT),
            "<http://www.w3.org/2000/01/rdf-schema#subClassOf>".into(),
            "<urn:ggen:pack:demo#Thing>".into(),
        )];
        let err = check_hygiene(&triples).unwrap_err();
        assert!(err.to_string().contains("E_METAMODEL_HYGIENE_VIOLATION"));
    }

    #[test]
    fn authority_pin_is_fail_closed_on_family_iris() {
        assert!(check_authority_pin(&format!("{}Term", AUTHORITY_ROOT)).is_ok());
        assert!(check_authority_pin("http://www.opengroup.org/togaf/ContentMetamodel").is_err());
        let err = check_authority_pin("ea:Something").unwrap_err();
        assert!(err.to_string().starts_with("E_AUTHORITY_ROOT_UNPINNED"));
        // Non-family IRIs are outside the pin law entirely.
        assert!(check_authority_pin("http://example.org/anything").is_ok());
    }

    #[test]
    fn ingest_isolates_pack_namespace_and_projects_to_domain() {
        let ing = PackIngest::new("affidavit-pack").unwrap();
        assert_eq!(ing.pack_ns(), "urn:ggen:pack:affidavit-pack#");
        let triples = vec![(
            "<urn:ggen:pack:affidavit-pack#Receipt>".into(),
            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".into(),
            "<http://www.w3.org/2002/07/owl#Class>".into(),
        )];
        let projected = ing.project(&triples).unwrap();
        assert_eq!(projected[0].0, "urn:domain#Receipt");
        // Predicate (standard vocab) survives untouched.
        assert_eq!(
            projected[0].1,
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
        );
    }

    #[test]
    fn ingest_refuses_leaky_projection() {
        let ing = PackIngest::new("demo").unwrap();
        // Pack-internal predicate escapes.
        let leak_pred = vec![(
            "<urn:ggen:pack:demo#R>".into(),
            "<urn:ggen:pack:demo#relatesTo>".into(),
            "<urn:domain#S>".into(),
        )];
        assert!(ing.project(&leak_pred).is_err());
        // Pack-internal class as object escapes.
        let leak_obj = vec![(
            "<urn:ggen:pack:demo#R>".into(),
            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".into(),
            "<urn:ggen:pack:demo#Internal>".into(),
        )];
        assert!(ing.project(&leak_obj).is_err());
        // Subject outside the pack's own namespace is a leak too.
        let foreign = vec![(
            "<urn:ggen:pack:other#X>".into(),
            "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>".into(),
            "<urn:domain#Y>".into(),
        )];
        assert!(ing.project(&foreign).is_err());
        let err = ing.project(&foreign).unwrap_err();
        assert!(err.to_string().starts_with("E_NAMESPACE_LEAK"), "got: {err}");
    }

    #[test]
    fn ingest_refuses_invalid_pack_names() {
        assert!(PackIngest::new("").is_err());
        assert!(PackIngest::new("de#mo").is_err());
    }
}
