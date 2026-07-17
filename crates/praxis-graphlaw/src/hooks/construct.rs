// SPARQL CONSTRUCT query evaluation and template instantiation

use crate::encoding::Encoder;
use crate::registry::{CONSTRUCT_BNODE_INTERN, SYNTHETIC_COUNTER};
use crate::sparql::Binding;
use crate::term::Triple;
use crate::tripleindex::TripleIndex;
use serde::{Deserialize, Serialize};
use spargebra::term::{NamedNodePattern, TermPattern};
use spargebra::SparqlParser;
use std::collections::BTreeMap;
use std::sync::atomic::Ordering;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HookReceipt {
    pub hook_name: String,
    pub delta_hash: String,
    pub idempotency_key: String,
    pub delta_quads: String,
}

fn collect_triple_patterns(
    gp: &spargebra::algebra::GraphPattern,
    out: &mut Vec<spargebra::term::TriplePattern>,
) {
    match gp {
        spargebra::algebra::GraphPattern::Bgp { patterns } => {
            out.extend(patterns.clone());
        }
        spargebra::algebra::GraphPattern::Join { left, right } => {
            collect_triple_patterns(left, out);
            collect_triple_patterns(right, out);
        }
        spargebra::algebra::GraphPattern::Distinct { inner }
        | spargebra::algebra::GraphPattern::Reduced { inner } => {
            collect_triple_patterns(inner, out);
        }
        spargebra::algebra::GraphPattern::Project { inner, .. } => {
            collect_triple_patterns(inner, out);
        }
        spargebra::algebra::GraphPattern::Filter { inner, .. } => {
            collect_triple_patterns(inner, out);
        }
        spargebra::algebra::GraphPattern::Group { inner, .. } => {
            collect_triple_patterns(inner, out);
        }
        spargebra::algebra::GraphPattern::Extend { inner, .. } => {
            collect_triple_patterns(inner, out);
        }
        spargebra::algebra::GraphPattern::LeftJoin { left, right, .. } => {
            collect_triple_patterns(left, out);
            collect_triple_patterns(right, out);
        }
        spargebra::algebra::GraphPattern::Union { left, right } => {
            collect_triple_patterns(left, out);
            collect_triple_patterns(right, out);
        }
        spargebra::algebra::GraphPattern::Minus { left, right } => {
            collect_triple_patterns(left, out);
            collect_triple_patterns(right, out);
        }
        _ => {}
    }
}

/// Per-row context threaded through template instantiation so CONSTRUCT
/// template blank nodes resolve per `instantiate_term_pattern`'s scoping
/// rule (see that function's doc comment for the full rationale).
struct RowBlankNodeContext<'a> {
    /// Exact CONSTRUCT query text -- half of the cross-round interning key
    /// (`mint_or_reuse_construct_blank_node`), so two different hooks that
    /// happen to use the same blank-node label and land on coincidentally
    /// identical bindings never alias onto each other.
    query_str: &'a str,
    /// Canonical, order-independent encoding of this row's own bindings --
    /// the other half of the interning key. See `canonical_row_key`.
    row_key: &'a str,
    /// Row-local memo: template label -> already-minted scoped label, so a
    /// label used more than once within THIS row's own template triples
    /// reuses one lookup instead of re-querying the global intern table
    /// for every occurrence.
    minted: BTreeMap<String, String>,
}

/// Canonical, order-independent encoding of one solution row's bindings,
/// used as half of `mint_or_reuse_construct_blank_node`'s interning key.
/// Sorted by variable name so row order -- an artifact of
/// `evaluate_plan_and_debug`'s own internal traversal, not a semantic
/// property of the solution -- never changes the key for the same content.
///
/// # Complexity
/// O(b log b) for b bindings in the row.
fn canonical_row_key(row: &[Binding]) -> String {
    let mut pairs: Vec<(&str, &str)> = row
        .iter()
        .map(|b| (b.var.as_str(), b.val.as_str()))
        .collect();
    pairs.sort_unstable();
    let mut key = String::new();
    for (var, val) in pairs {
        key.push_str(var);
        key.push('=');
        key.push_str(val);
        key.push('\u{1}');
    }
    key
}

/// Deterministically mint (or reuse) a fresh, process-unique blank-node
/// label for one (query, template blank-node label, solution row)
/// combination.
///
/// # Why content-addressed interning, not a bare counter
/// `TripleStore::materialize()`'s fixpoint loop (`reasoner/mod.rs`)
/// re-evaluates a firing hook's CONSTRUCT query on every round until no
/// hook produces a genuinely NEW triple (`apply_new_triple`'s
/// `triple_index.contains` dedup check). A bare per-call counter would
/// mint a DIFFERENT label for the IDENTICAL solution on every round, so
/// the round's output would never stop looking "new" and `materialize()`
/// would never converge -- the same failure mode `crate::term::
/// VarOrTerm::new_list`'s own `LIST_INTERN` table
/// (`crate::registry`) exists to avoid for synthetic list terms. Interning
/// by the full (query text, template label, canonical row content) key
/// gives idempotence across repeated evaluation of the SAME solution
/// (fixpoint convergence) while still minting a genuinely fresh label the
/// first time a given key is seen -- which is what keeps different
/// solution ROWS (the aliasing bug this function exists to fix) from
/// sharing an identity. Known, accepted narrowing versus strict SPARQL 1.1
/// per-execution semantics: two solution rows with byte-identical bindings
/// (a duplicate solution, not merely a different one) share one blank node
/// here rather than each getting its own -- an edge case no caller in this
/// codebase currently exercises, and required by materialize()'s own
/// convergence invariant above.
///
/// # Complexity
/// O(1) amortized (one hashmap lookup, and on a miss one insert, under a
/// single global lock).
fn mint_or_reuse_construct_blank_node(query_str: &str, label: &str, row_key: &str) -> String {
    let intern_key = format!("{query_str}\u{0}{label}\u{0}{row_key}");
    let mut intern = CONSTRUCT_BNODE_INTERN
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    if let Some(existing) = intern.get(&intern_key) {
        return existing.clone();
    }
    let tag = SYNTHETIC_COUNTER.fetch_add(1, Ordering::SeqCst);
    let scoped = format!("_:{}__construct_{}", label, tag);
    intern.insert(intern_key, scoped.clone());
    scoped
}

/// Instantiate a single CONSTRUCT-template term against one solution row.
///
/// # Blank-node scoping (SPARQL 1.1 sec 16.2, "Templates with Blank Nodes")
/// A CONSTRUCT template's blank-node labels are scoped PER SOLUTION, not
/// per query or per process: the same syntactic label used twice within
/// one solution's instantiated triples must resolve to the SAME node
/// (within-solution sharing), but the same label across DIFFERENT
/// solutions must resolve to DIFFERENT, fresh nodes. `ctx.minted` is the
/// row-local half of the mechanism that enforces this (the caller,
/// `evaluate_construct`, creates one fresh `RowBlankNodeContext` per
/// solution row and threads it through every template triple for that
/// row, then discards it before the next row); `mint_or_reuse_construct_
/// blank_node`'s process-wide, content-addressed intern table is the
/// other half, needed so the SAME row re-evaluated on a later
/// `materialize()` fixpoint round still resolves to the SAME label (see
/// that function's doc comment). This is deliberately NOT the previous
/// behavior of echoing the template's raw label string verbatim: that fed
/// the literal label into the process-wide `GLOBAL_ENCODER` blank-node
/// table (`crate::encoding`) directly, so the same label aliased onto one
/// node across every row of every `evaluate_construct` call for the
/// process's whole lifetime, regardless of which solution produced it.
fn instantiate_term_pattern(
    tp: &TermPattern,
    bindings: &[Binding],
    ctx: &mut RowBlankNodeContext,
) -> Option<String> {
    match tp {
        TermPattern::Variable(v) => {
            let var_name = v.as_str();
            bindings
                .iter()
                .find(|b| b.var == var_name)
                .map(|b| b.val.clone())
        }
        TermPattern::NamedNode(n) => Some(format!("<{}>", n.as_str())),
        TermPattern::BlankNode(b) => {
            let label = b.as_str();
            if let Some(scoped) = ctx.minted.get(label) {
                return Some(scoped.clone());
            }
            let scoped = mint_or_reuse_construct_blank_node(ctx.query_str, label, ctx.row_key);
            ctx.minted.insert(label.to_string(), scoped.clone());
            Some(scoped)
        }
        TermPattern::Literal(l) => {
            let mut s = format!("\"{}\"", l.value());
            if let Some(lang) = l.language() {
                s.push_str(&format!("@{}", lang));
            } else if l.datatype().as_str() != "http://www.w3.org/2001/XMLSchema#string" {
                s.push_str(&format!("^^<{}>", l.datatype().as_str()));
            }
            Some(s)
        }
        _ => None,
    }
}

fn instantiate_named_node_pattern(np: &NamedNodePattern, bindings: &[Binding]) -> Option<String> {
    match np {
        NamedNodePattern::Variable(v) => {
            let var_name = v.as_str();
            bindings
                .iter()
                .find(|b| b.var == var_name)
                .map(|b| b.val.clone())
        }
        NamedNodePattern::NamedNode(n) => Some(format!("<{}>", n.as_str())),
    }
}

fn instantiate_triple_pattern(
    tp: &spargebra::term::TriplePattern,
    row: &[Binding],
    ctx: &mut RowBlankNodeContext,
) -> Option<Triple> {
    let s_str = instantiate_term_pattern(&tp.subject, row, ctx)?;
    let p_str = instantiate_named_node_pattern(&tp.predicate, row)?;
    let o_str = instantiate_term_pattern(&tp.object, row, ctx)?;
    Some(Triple::from(s_str, p_str, o_str))
}

/// Evaluate a CONSTRUCT (or WHERE-pattern-as-deletion) query against
/// `triple_index`, instantiating the template once per solution row with
/// fresh, per-row blank-node scoping (see `instantiate_term_pattern`).
///
/// # Complexity
/// O(R * |T|) where R is the number of solution rows `evaluate_plan_and_debug`
/// yields and |T| is the template (or WHERE-pattern) triple count, plus one
/// O(1)-amortized `CONSTRUCT_BNODE_INTERN` lookup per template blank node.
///
/// # Determinism
/// `bindings` is collected eagerly from `crate::sparql::evaluate_plan_and_debug`,
/// a single-threaded, non-random iterator over `triple_index`; for a fixed
/// index and query its row order is a deterministic function of the index's
/// own (already order-independent-by-content) internal state, not of wall
/// clock or thread scheduling. Each row's blank nodes are then minted (or
/// reused) via `mint_or_reuse_construct_blank_node`, which is itself
/// content-addressed (query text + label + canonical row encoding) rather
/// than execution-order-addressed -- so re-running this same query against
/// this same index, or `materialize()` re-evaluating it on a later fixpoint
/// round, reproduces byte-identical output, independent of how many prior
/// calls have incremented `SYNTHETIC_COUNTER` for unrelated queries/rows.
pub fn evaluate_construct(
    query_str: &str,
    triple_index: &TripleIndex,
) -> Result<(Vec<Triple>, Vec<Triple>), String> {
    let query = SparqlParser::new()
        .parse_query(query_str)
        .map_err(|e| format!("SPARQL parse error: {}", e))?;

    if let spargebra::Query::Construct {
        ref template,
        ref pattern,
        ..
    } = query
    {
        let mut additions = Vec::new();
        let mut deletions = Vec::new();

        let plan = crate::plan_query_or_refuse(&query, triple_index)?;
        let bindings: Vec<Vec<Binding>> =
            crate::sparql::evaluate_plan_and_debug(&plan, triple_index).collect();

        if !template.is_empty() {
            // Fresh `RowBlankNodeContext` per row (not hoisted above the loop):
            // this is what gives each solution its own blank-node identities
            // while still sharing one identity across that solution's own
            // triples. See `instantiate_term_pattern`'s doc comment for the
            // full rule.
            for row in bindings {
                let row_key = canonical_row_key(&row);
                let mut ctx = RowBlankNodeContext {
                    query_str,
                    row_key: &row_key,
                    minted: BTreeMap::new(),
                };
                for tp in template {
                    if let Some(triple) = instantiate_triple_pattern(tp, &row, &mut ctx) {
                        additions.push(triple);
                    }
                }
            }
        } else {
            let mut patterns = Vec::new();
            collect_triple_patterns(&pattern, &mut patterns);
            for row in bindings {
                let row_key = canonical_row_key(&row);
                let mut ctx = RowBlankNodeContext {
                    query_str,
                    row_key: &row_key,
                    minted: BTreeMap::new(),
                };
                for tp in &patterns {
                    if let Some(triple) = instantiate_triple_pattern(tp, &row, &mut ctx) {
                        deletions.push(triple);
                    }
                }
            }
        }

        Ok((additions, deletions))
    } else {
        Err("Query is not a CONSTRUCT query".to_string())
    }
}

pub fn serialize_delta_quad(
    hook_iri: &str,
    triple: &Triple,
    is_addition: bool,
    lines: &mut Vec<String>,
) {
    let s = clean_decoded_term(&Encoder::decode(&triple.s.to_encoded()).unwrap_or_default());
    let p = clean_decoded_term(&Encoder::decode(&triple.p.to_encoded()).unwrap_or_default());
    let o = clean_decoded_term(&Encoder::decode(&triple.o.to_encoded()).unwrap_or_default());

    let triple_str = format!("{} {} {}", s, p, o);
    let hash = blake3::hash(triple_str.as_bytes()).to_hex().to_string();

    let bn_id = if is_addition {
        format!("_:add_{}", hash)
    } else {
        format!("_:del_{}", hash)
    };

    let pred = if is_addition {
        "<http://seanchatmangpt.github.io/praxis/kh#addQuad>"
    } else {
        "<http://seanchatmangpt.github.io/praxis/kh#deleteQuad>"
    };

    let wrap_iri = |x: &str| {
        if x.starts_with('<') || x.starts_with('"') || x.starts_with('_') {
            x.to_string()
        } else {
            format!("<{}>", x)
        }
    };

    let s_wrapped = wrap_iri(&s);
    let p_wrapped = wrap_iri(&p);
    let o_wrapped = if o.starts_with('"') {
        o.clone()
    } else {
        wrap_iri(&o)
    };
    let hook_wrapped = wrap_iri(hook_iri);

    lines.push(format!(
        "{} <http://seanchatmangpt.github.io/praxis/kh#subject> {} .",
        bn_id, s_wrapped
    ));
    lines.push(format!(
        "{} <http://seanchatmangpt.github.io/praxis/kh#predicate> {} .",
        bn_id, p_wrapped
    ));
    lines.push(format!(
        "{} <http://seanchatmangpt.github.io/praxis/kh#object> {} .",
        bn_id, o_wrapped
    ));
    lines.push(format!("{} {} {} .", hook_wrapped, pred, bn_id));
}

fn clean_decoded_term(s: &str) -> String {
    s.trim().to_string()
}
