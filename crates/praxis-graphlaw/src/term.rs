// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(dead_code)]

use crate::registry::{FORMULA_REGISTRY, LIST_INTERN, LIST_REGISTRY, SYNTHETIC_COUNTER};
use crate::Encoder;
use std::sync::atomic::Ordering;

// ============================================================================
// N3 built-in support: RDF lists and quoted graphs ("formulas")
// ============================================================================
//
// Design decision (TICKET-005): rather than adding new `Term` variants for
// lists and quoted graphs, both are represented as ordinary `Term::BlankNode`
// values whose (synthetic, process-unique) label acts as a key into a
// process-wide side table:
//   - `LIST_REGISTRY`:    blank-node id -> ordered member ids (Vec<usize>)
//   - `FORMULA_REGISTRY`: blank-node id -> the quoted graph's own triples
// This keeps `Term` a closed 3-variant enum (Iri/Literal/BlankNode), so the
// many exhaustive `match term { Term::Iri | Term::Literal | Term::BlankNode }`
// sites elsewhere in the crate (sparql.rs, oxrdf_adapter.rs, shacl.rs) need no
// changes. List members may themselves be *variables* (e.g. `( ?p1 ?p2 )` used
// as the subject of math:sum in a rule body) -- those are resolved against the
// current row's `Binding` at builtin-evaluation time (see queryengine.rs).
// Stores the full `VarOrTerm` (not just its encoded id) per member,
// preserving whether each position is a genuine variable or a ground term
// -- `to_encoded()` alone can't be trusted to recover that distinction
// later (a variable name that had its leading '?' stripped before
// encoding, per `make_term`'s/`convert`'s shared "?"-stripping convention,
// is encoded exactly like a same-named IRI would be; the Var-vs-Term
// wrapper is the only reliable signal). Found via the real EYE
// `good_cobbler` corpus case: without this, `is_nonground_list_pattern`
// could never distinguish a genuine list-pattern variable from a
// same-spelled ground term.
//
// The registries themselves (`LIST_REGISTRY`, `LIST_INTERN`,
// `FORMULA_REGISTRY`, `SYNTHETIC_COUNTER`) live in `crate::registry`.

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum VarOrTerm {
    Var(Variable),
    Term(Term),
}

impl VarOrTerm {
    pub fn new_term(iri: String) -> VarOrTerm {
        let term = Term::parse(iri);
        VarOrTerm::Term(term)
    }

    pub fn new_var(name: String) -> VarOrTerm {
        let encoded = Encoder::add(name);
        VarOrTerm::Var(Variable { name: encoded })
    }

    pub fn new_encoded_term(id: usize) -> VarOrTerm {
        let term = Encoder::decode_to_term(id).unwrap_or({
            // Fallback for untracked IDs (e.g. in tests/mock setups)
            Term::Iri(TermImpl { iri: id })
        });
        VarOrTerm::Term(term)
    }

    pub fn new_encoded_var(name: usize) -> VarOrTerm {
        VarOrTerm::Var(Variable { name })
    }

    pub(crate) fn as_term(&self) -> &Term {
        if let VarOrTerm::Term(t) = self {
            t
        } else {
            panic!("Not a term")
        }
    }

    pub(crate) fn as_var(&self) -> &Variable {
        if let VarOrTerm::Var(v) = self {
            v
        } else {
            panic!("Not a Var")
        }
    }

    pub fn is_var(&self) -> bool {
        match self {
            Self::Var(_) => true,
            Self::Term(_) => false,
        }
    }

    pub fn is_term(&self) -> bool {
        !self.is_var()
    }

    pub fn to_encoded(&self) -> usize {
        match self {
            Self::Var(var) => var.name,
            Self::Term(term) => term.id(),
        }
    }

    fn rem_first_and_last(value: &str) -> &str {
        let mut chars = value.chars();
        chars.next();
        chars.next_back();
        chars.as_str()
    }

    pub fn convert(var_or_term: String) -> VarOrTerm {
        let var_or_term = var_or_term.trim().to_string();
        if let Some(var_name) = var_or_term.strip_prefix('?') {
            VarOrTerm::new_var(var_name.to_string())
        } else {
            let mut iri_prefix = var_or_term;
            if !iri_prefix.starts_with('<')
                && !iri_prefix.starts_with('"')
                && !iri_prefix.starts_with("_:")
            {
                iri_prefix = format!("<{}>", iri_prefix).to_string();
            }
            VarOrTerm::new_term(iri_prefix)
        }
    }

    pub fn new_literal(value: String, datatype: Option<String>, lang: Option<String>) -> VarOrTerm {
        let id = Encoder::add_literal(value, datatype, lang);
        let term = Encoder::decode_to_term(id).expect("Successfully decoded just-added literal");
        VarOrTerm::Term(term)
    }

    pub fn new_blank_node(label: String) -> VarOrTerm {
        let id = Encoder::add_blank_node(label);
        let term = Encoder::decode_to_term(id).expect("Successfully decoded just-added blank node");
        VarOrTerm::Term(term)
    }

    /// Build an RDF-list term from an ordered set of members (which may be
    /// variables or ground terms). See the module-level design note above.
    ///
    /// Poison-recovery note (matches `Encoder`'s own fix, see
    /// `encoding.rs::recover`'s doc comment for the full rationale): each
    /// `.lock()` here uses `unwrap_or_else(|poisoned| poisoned.into_inner())`
    /// rather than `.unwrap()`, so an unrelated panic elsewhere in the
    /// process no longer permanently wedges every future list/formula
    /// lookup. Honest nuance specific to this function: `LIST_INTERN` and
    /// `LIST_REGISTRY` are two separate locks updated in two separate
    /// statements (not one atomic critical section), so a panic landing
    /// between the two inserts below could in principle leave `LIST_REGISTRY`
    /// populated for an id that `LIST_INTERN` doesn't yet cache -- the next
    /// `new_list` call with the same members would then miss the cache and
    /// mint a second, redundant blank-node id for logically-identical
    /// members, rather than returning a wrong answer. That is a strictly
    /// smaller failure than today's alternative (the panic's poisoning wedges
    /// this function, and everything downstream of it, for the rest of the
    /// process).
    pub fn new_list(members: Vec<VarOrTerm>) -> VarOrTerm {
        if let Some(&existing_id) = LIST_INTERN
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .get(&members)
        {
            return VarOrTerm::new_encoded_term(existing_id);
        }
        let tag = SYNTHETIC_COUNTER.fetch_add(1, Ordering::SeqCst);
        let id = Encoder::add_blank_node(format!("__n3list_{}", tag));
        LIST_REGISTRY
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .insert(id, members.clone());
        LIST_INTERN
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .insert(members, id);
        VarOrTerm::new_encoded_term(id)
    }

    /// Build a quoted-graph ("formula") term from its constituent triples.
    /// See the module-level design note above.
    pub fn new_formula(triples: Vec<Triple>) -> VarOrTerm {
        let tag = SYNTHETIC_COUNTER.fetch_add(1, Ordering::SeqCst);
        let id = Encoder::add_blank_node(format!("__n3formula_{}", tag));
        FORMULA_REGISTRY
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .insert(id, triples);
        VarOrTerm::new_encoded_term(id)
    }

    /// If `id` names a synthetic list term, return its ordered member ids
    /// (encoded, losing the Var/Term distinction -- fine for consumers
    /// that only ever see fully-bound/ground lists, e.g. the `list:*`
    /// builtins in queryengine.rs). Use `list_members_typed` where telling
    /// a variable member apart from a same-spelled ground term matters.
    pub fn list_members(id: usize) -> Option<Vec<usize>> {
        Self::list_members_typed(id).map(|members| members.iter().map(|m| m.to_encoded()).collect())
    }

    /// Like `list_members`, but preserves each member's full `VarOrTerm`
    /// (Var vs Term), which `to_encoded()` alone discards.
    pub fn list_members_typed(id: usize) -> Option<Vec<VarOrTerm>> {
        LIST_REGISTRY
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .get(&id)
            .cloned()
    }

    /// True if `id` names a list term with at least one variable member,
    /// directly or nested arbitrarily deep -- i.e. a genuine *pattern*
    /// list rather than a fully ground one. Ground lists are looked up by
    /// ordinary exact-id equality (as before); only non-ground ones need
    /// the structural unification path below. Uses `list_members_typed`
    /// (not `list_members`/`to_encoded()`) specifically because a
    /// variable's encoded id is otherwise indistinguishable from a
    /// same-spelled ground term's -- `make_term`/`convert` both strip a
    /// variable's leading '?' before interning it, so e.g. `?Y` and a
    /// hypothetical bare IRI `Y` would encode identically; only the
    /// `VarOrTerm::Var` wrapper itself reliably says "this was a variable."
    pub fn is_nonground_list_pattern(id: usize) -> bool {
        match Self::list_members_typed(id) {
            Some(members) => members
                .iter()
                .any(|m| m.is_var() || Self::is_nonground_list_pattern(m.to_encoded())),
            None => false,
        }
    }

    /// Recursively unify a (possibly variable-containing) list-term
    /// pattern against a candidate list-term id, collecting any variable
    /// bindings discovered into `out_bindings` (as `(var_id, value_id)`
    /// pairs). Returns whether the two unify at all: same length, and
    /// every non-variable pattern member either equals the corresponding
    /// data member exactly or (if itself a nested list pattern) unifies
    /// recursively.
    ///
    /// This closes a real gap: `TripleIndex::query`'s ordinary lookups
    /// compare list-term ids for exact equality only (every list gets a
    /// fresh synthetic id at parse time -- see `LIST_REGISTRY`'s design
    /// note above), so a rule pattern like `(:good ?Y)` could never match
    /// a separately-parsed ground list `(:good :Cobbler)` even though
    /// they're structurally compatible member-for-member (found via the
    /// real EYE `good_cobbler` corpus case).
    pub fn unify_list_pattern(
        pattern_id: usize,
        data_id: usize,
        out_bindings: &mut Vec<(usize, usize)>,
    ) -> bool {
        let (Some(p_members), Some(d_members)) = (
            Self::list_members_typed(pattern_id),
            Self::list_members_typed(data_id),
        ) else {
            // Neither (or only one) side is a list at all -- fall back to
            // exact-id equality (covers e.g. a plain IRI/literal member).
            return pattern_id == data_id;
        };
        if p_members.len() != d_members.len() {
            return false;
        }
        for (p, d) in p_members.iter().zip(d_members.iter()) {
            let (p_id, d_id) = (p.to_encoded(), d.to_encoded());
            if p.is_var() {
                out_bindings.push((p_id, d_id));
            } else if Self::list_members_typed(p_id).is_some() {
                if !Self::unify_list_pattern(p_id, d_id, out_bindings) {
                    return false;
                }
            } else if p_id != d_id {
                return false;
            }
        }
        true
    }

    /// Recursively substitute variables in `term` using `resolve` (a
    /// var-id -> bound-value-id lookup for the current binding row),
    /// including INSIDE list-term structures -- rebuilding a fresh list
    /// term with substituted members if any member actually changed, since
    /// a list term itself is never `is_var()` and its internal variable
    /// members would otherwise be silently left unsubstituted. Found via
    /// the real EYE `good_cobbler` corpus case: without this, asserting a
    /// rule's list-valued head just copied the rule's own still-variable-
    /// containing pattern list verbatim, rather than the bound value.
    pub fn substitute_deep(
        term: &VarOrTerm,
        resolve: &impl Fn(usize) -> Option<usize>,
    ) -> VarOrTerm {
        if term.is_var() {
            if let Some(val) = resolve(term.to_encoded()) {
                return VarOrTerm::new_encoded_term(val);
            }
            return term.clone();
        }
        let id = term.to_encoded();
        if let Some(members) = Self::list_members_typed(id) {
            let mut changed = false;
            let new_members: Vec<VarOrTerm> = members
                .iter()
                .map(|m| {
                    let sub = Self::substitute_deep(m, resolve);
                    if sub.to_encoded() != m.to_encoded() {
                        changed = true;
                    }
                    sub
                })
                .collect();
            if changed {
                return Self::new_list(new_members);
            }
        }
        term.clone()
    }

    /// If `id` names a synthetic formula (quoted graph) term, return its triples.
    pub fn formula_triples(id: usize) -> Option<Vec<Triple>> {
        FORMULA_REGISTRY
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
            .get(&id)
            .cloned()
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Variable {
    pub(crate) name: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Term {
    Iri(TermImpl),
    Literal(LiteralImpl),
    BlankNode(BlankNodeImpl),
}

impl Term {
    pub fn id(&self) -> usize {
        match self {
            Self::Iri(iri) => iri.iri,
            Self::Literal(lit) => lit.id,
            Self::BlankNode(bnode) => bnode.id,
        }
    }

    pub fn parse(s: String) -> Self {
        let id = Encoder::add(s);
        Encoder::decode_to_term(id).expect("Successfully decoded just-added term")
    }
}

impl std::fmt::Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(s) = Encoder::decode(&self.id()) {
            write!(f, "{}", s)
        } else {
            write!(f, "Term({})", self.id())
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TermImpl {
    pub(crate) iri: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct LiteralImpl {
    pub(crate) id: usize,
    pub(crate) value: usize,
    pub(crate) datatype: Option<usize>,
    pub(crate) lang: Option<usize>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BlankNodeImpl {
    pub(crate) id: usize,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Triple {
    pub s: VarOrTerm,
    pub p: VarOrTerm,
    pub o: VarOrTerm,
    pub g: Option<VarOrTerm>,
}

impl Triple {
    pub fn from(subject: String, property: String, object: String) -> Triple {
        Triple {
            s: VarOrTerm::convert(subject),
            p: VarOrTerm::convert(property),
            o: VarOrTerm::convert(object),
            g: None,
        }
    }

    pub fn from_with_graph_name(
        subject: String,
        property: String,
        object: String,
        graph_name: String,
    ) -> Triple {
        let mut triple = Self::from(subject, property, object);
        triple.g = Some(VarOrTerm::convert(graph_name));
        triple
    }
}

#[cfg(test)]
#[path = "term_test.rs"]
mod term_test;
