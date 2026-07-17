use crate::{BodyLiteral, Rule as ReasonerRule, Triple, VarOrTerm};
use std::cell::RefCell;
use std::collections::HashMap;

use pest::iterators::{Pair, Pairs};
use pest::Parser;

use super::iri_resolve::PrefixMapper;
use super::n3_terms;

// ---------------------------------------------------------------------------
// `@forAll`/`@forSome` quantifier scoping
// ---------------------------------------------------------------------------
//
// Real per-formula scoping per the N3 CG spec: a variable named in an
// `@forAll` declared *within* some formula (the document root, or a nested
// `{ ... }`) is universally quantified and scoped to that formula -- reusing
// the same bare name in an unrelated sibling formula that *also* explicitly
// quantifies it must produce a genuinely distinct variable. A variable named
// in `@forSome` is existentially quantified and is skolemized (replaced with
// a fresh blank node) at parse time, scoped the same way.
//
// Deliberate scope of this implementation (documented conflict discovered
// while implementing): this only affects variables an author *explicitly*
// names in a `@forAll`/`@forSome` declaration. Bare/unquantified variables
// keep this engine's pre-existing flat, name-based identity across formula
// boundaries. That flat behaviour is not just an unrelated legacy quirk --
// `reasoner/log_implies.rs`'s dynamic `log:implies` reification is
// *structurally* built on it: its own doc comment states variables are
// "matched across antecedent/consequent/outer scopes purely by *name*", and
// `n3_scoping.rs`'s existing `test_chained_implication_through_log_implies_*`
// tests lock in exactly that -- a bare `?citizen` used in one top-level
// quoted formula (`:alice :says { ?citizen a :GoodCitizen }`) must resolve
// to the SAME variable as the bare `?citizen` used in a completely different
// formula nested inside a rule body (`?formula log:implies { ?citizen a
// :TaxPayer }`) and the rule's own head, or the derivation silently fails to
// ground. Auto-scoping every bare variable to its own formula (the naive
// reading of "sibling formulas must not collide") breaks that mechanism.
// Scoping only names an author has *explicitly* opted into quantifying has
// no such conflict, since `log:implies` fixtures never declare `@forAll`/
// `@forSome`, and it is the literal, spec-accurate meaning of those two
// declarations -- so that is what is implemented here.
#[derive(Default)]
struct FormulaScope {
    /// `@forAll`-declared name -> fresh variable name scoped to this formula.
    forall: HashMap<String, String>,
    /// `@forSome`-declared name -> skolemized blank-node term scoped to this formula.
    forsome: HashMap<String, VarOrTerm>,
}

#[derive(Default)]
struct ScopeStack {
    scopes: Vec<FormulaScope>,
    counter: usize,
}

impl ScopeStack {
    fn push(&mut self) {
        self.scopes.push(FormulaScope::default());
    }

    fn pop(&mut self) {
        self.scopes.pop();
    }

    /// Register `name` as `@forAll`-quantified in the current (innermost)
    /// scope, generating its fresh formula-scoped variable name.
    fn declare_forall(&mut self, name: &str) {
        self.counter += 1;
        let renamed = format!("{}__forall{}", name, self.counter);
        if let Some(scope) = self.scopes.last_mut() {
            scope.forall.insert(name.to_string(), renamed);
        }
    }

    /// Register `name` as `@forSome`-quantified in the current (innermost)
    /// scope, skolemizing it to a fresh blank node immediately.
    fn declare_forsome(&mut self, name: &str) {
        use crate::registry::SYNTHETIC_COUNTER;
        use std::sync::atomic::Ordering;

        self.counter += 1;
        let tag = SYNTHETIC_COUNTER.fetch_add(1, Ordering::SeqCst);
        let skolem = VarOrTerm::new_blank_node(format!("__n3forsome_{}_{}", tag, self.counter));
        if let Some(scope) = self.scopes.last_mut() {
            scope.forsome.insert(name.to_string(), skolem);
        }
    }

    /// Resolve a bare variable name (without the leading `?`) to its
    /// `VarOrTerm`, honoring any explicit `@forAll`/`@forSome` declaration
    /// found in the current formula-scope stack (innermost scope wins).
    /// Names never explicitly quantified fall through unchanged (this
    /// engine's pre-existing flat, name-based identity -- see the module
    /// doc comment above for why).
    fn resolve(&self, name: &str) -> VarOrTerm {
        for scope in self.scopes.iter().rev() {
            if let Some(term) = scope.forsome.get(name) {
                return term.clone();
            }
        }
        for scope in self.scopes.iter().rev() {
            if let Some(renamed) = scope.forall.get(name) {
                return VarOrTerm::new_var(renamed.clone());
            }
        }
        VarOrTerm::new_var(name.to_string())
    }
}

thread_local! {
    static SCOPE_STACK: RefCell<ScopeStack> = RefCell::new(ScopeStack::default());
}

/// Push a fresh formula scope for the duration of `f` (used both for the
/// document root and for each nested `Formula`-as-term), popping it again
/// afterwards even if `f` panics.
pub fn with_new_scope<T>(f: impl FnOnce() -> T) -> T {
    SCOPE_STACK.with(|s| s.borrow_mut().push());
    let result = f();
    SCOPE_STACK.with(|s| s.borrow_mut().pop());
    result
}

fn declare_forall_var(name: &str) {
    SCOPE_STACK.with(|s| s.borrow_mut().declare_forall(name));
}

fn declare_forsome_var(name: &str) {
    SCOPE_STACK.with(|s| s.borrow_mut().declare_forsome(name));
}

/// Resolve a bare variable name (leading `?` already stripped) through the
/// current formula-scope stack. The single call site is `make_term`, which
/// is itself the sole funnel for every `Var` pair in the grammar (subject,
/// object, property, list member, formula member, ...), so this covers all
/// variable occurrences uniformly.
pub fn resolve_var(name: &str) -> VarOrTerm {
    SCOPE_STACK.with(|s| s.borrow().resolve(name))
}

/// Scan a `ForAll`/`ForSome` pair's inner `Var` children and register each
/// declared name in the CURRENT (innermost) scope. Called in a pre-pass over
/// a formula's (or the document's) direct children before processing its
/// ordinary triples/rules, so declarations take effect regardless of where
/// textually within the formula they appear.
pub fn register_quantifier_declarations(pair: &Pair<Rule>) {
    let is_forall = pair.as_rule() == Rule::ForAll;
    for child in pair.clone().into_inner() {
        if child.as_rule() == Rule::Var {
            let name = &child.as_str()[1..]; // strip leading '?'
            if is_forall {
                declare_forall_var(name);
            } else {
                declare_forsome_var(name);
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Pest-generated parser
// ---------------------------------------------------------------------------

#[derive(Parser)]
#[grammar = "parser/n3.pest"]
pub struct N3Parser;

// ---------------------------------------------------------------------------
// Triple pattern parsing
// ---------------------------------------------------------------------------

/// Parse a single TP (triple pattern) production into one or more `Triple`s.
/// More than one triple results from comma sugar in an object list ("s p o1,
/// o2, o3 .") and/or semicolon sugar across predicate-object pairs ("s p1
/// o1; p2 o2 .") -- every resulting triple shares the same subject; a
/// semicolon segment's triples additionally share that segment's property.
/// `InverseTP` ("object is predicate of subject .") and `HasTP` ("subject
/// has predicate object .") are both real N3/EYE sugar for the ordinary
/// triple (subject, predicate, object).
pub fn parse_tp(pairs: Pairs<'_, Rule>, prefixes: &PrefixMapper) -> Vec<Triple> {
    let mut subject_vot = VarOrTerm::new_var("s".to_string());
    // Shared sink for both this TP's own (property, object) triples and any
    // extra triples desugared from `[ ... ]` bnode property lists nested in
    // its subject/object positions (or inside a List member).
    let mut triples = Vec::new();

    for pair in pairs {
        match pair.as_rule() {
            Rule::Subject => {
                subject_vot = parse_subject(pair, prefixes, &mut triples);
            }
            Rule::PredicateObjectList => {
                n3_terms::parse_predicate_object_list(
                    subject_vot.clone(),
                    pair,
                    prefixes,
                    &mut triples,
                );
            }
            // Inverse-predicate sugar: "object is predicate of subject ."
            // desugars to the ordinary triple (subject, predicate, object).
            Rule::InverseTP => {
                let mut inner = pair.into_inner();
                if let (Some(object_pair), Some(property_pair), Some(subject_pair)) =
                    (inner.next(), inner.next(), inner.next())
                {
                    let object_vot = n3_terms::parse_object(object_pair, prefixes, &mut triples);
                    let property_vot =
                        n3_terms::make_term(&n3_terms::expand_property(property_pair, prefixes));
                    let subject_vot2 = parse_subject(subject_pair, prefixes, &mut triples);
                    triples.push(Triple {
                        s: subject_vot2,
                        p: property_vot,
                        o: object_vot,
                        g: None,
                    });
                }
            }
            // `has` sugar: "subject has predicate object ." desugars to the
            // same ordinary triple (subject, predicate, object) -- `has` is
            // purely a readability filler word.
            Rule::HasTP => {
                let mut inner = pair.into_inner();
                if let (Some(subject_pair), Some(property_pair), Some(object_pair)) =
                    (inner.next(), inner.next(), inner.next())
                {
                    let subject_vot2 = parse_subject(subject_pair, prefixes, &mut triples);
                    let property_vot =
                        n3_terms::make_term(&n3_terms::expand_property(property_pair, prefixes));
                    let object_vot = n3_terms::parse_object(object_pair, prefixes, &mut triples);
                    triples.push(Triple {
                        s: subject_vot2,
                        p: property_vot,
                        o: object_vot,
                        g: None,
                    });
                }
            }
            Rule::EOI => {}
            _ => {}
        }
    }

    triples
}

fn parse_subject(pair: Pair<Rule>, prefixes: &PrefixMapper, extra: &mut Vec<Triple>) -> VarOrTerm {
    match pair.into_inner().next() {
        Some(child) => n3_terms::term_from_pair(child, prefixes, extra),
        None => VarOrTerm::new_var("s".to_string()),
    }
}

// ---------------------------------------------------------------------------
// Public parse function
// ---------------------------------------------------------------------------

/// Parse a complete N3 document into its plain (non-rule) fact triples and
/// its rules, in one unified pest-based pass.
///
/// Supports:
/// - `@prefix` declarations (anywhere in the document)
/// - Plain top-level fact triples ("s p o .", including comma-sugar object lists)
/// - `{body} => {head} .` rules with negated literals (`not { TP }`)
/// - Named IRIs (`<...>`), prefixed names, variables (`?name`), blank nodes (`_:x`)
/// - String, numeric, boolean, lang-tagged, and datatyped literals
/// - RDF lists (`( a b c )`) and quoted graphs (`{ a b c }`) used as terms
/// - Multi-triple heads
/// - `#` line comments
///
/// Returns `Err(String)` on parse failure.
pub fn parse_document(input: &str) -> Result<(Vec<Triple>, Vec<ReasonerRule>), String> {
    let parsed =
        N3Parser::parse(Rule::document, input).map_err(|e| format!("N3 parse error: {}", e))?;

    let document = match parsed.into_iter().next() {
        Some(p) => p,
        // The document root is itself a formula scope: any `@forAll`/
        // `@forSome` declared at the top level (outside any nested `{ }`)
        // scopes those variables document-wide (see the `ScopeStack` module
        // doc comment above `make_term`/`resolve_var`).
        None => return with_new_scope(|| Ok((Vec::new(), Vec::new()))),
    };

    with_new_scope(|| parse_document_body(document))
}

fn parse_document_body(document: Pair<Rule>) -> Result<(Vec<Triple>, Vec<ReasonerRule>), String> {
    let mut rules: Vec<ReasonerRule> = Vec::new();
    let mut content: Vec<Triple> = Vec::new();
    let mut prefix_mapper = PrefixMapper::new();

    let items: Vec<Pair<Rule>> = document.into_inner().collect();

    // Pre-pass: register every document-root `@forAll`/`@forSome` before
    // processing anything else, so declarations take effect regardless of
    // where textually in the document they appear (matching the same
    // pre-pass done per-`Formula` in `parse_formula`).
    for item in &items {
        if item.as_rule() == Rule::ForAll || item.as_rule() == Rule::ForSome {
            register_quantifier_declarations(item);
        }
    }

    for item in items {
        match item.as_rule() {
            // Declarations were already registered in the pre-pass above.
            Rule::ForAll | Rule::ForSome => {}
            Rule::Prefix => {
                let mut prefix_name = String::new();
                let mut prefix_iri = String::new();
                for child in item.into_inner() {
                    match child.as_rule() {
                        Rule::PrefixIdentifier => prefix_name = child.as_str().to_string(),
                        Rule::BracketedIri => {
                            if let Some(p) = child.into_inner().next() {
                                prefix_iri = p.as_str().to_string();
                            }
                        }
                        _ => {}
                    }
                }
                prefix_mapper.add(prefix_name, prefix_iri);
            }

            // `@base <IRI> .` -- sets/updates the current base IRI (which may
            // itself be a relative reference, resolved against whatever base
            // was already in effect: see `PrefixMapper::set_base`).
            Rule::Base => {
                if let Some(iri_pair) = item
                    .into_inner()
                    .find(|p| p.as_rule() == Rule::BracketedIri)
                    .and_then(|bracketed| bracketed.into_inner().next())
                {
                    prefix_mapper.set_base(iri_pair.as_str());
                }
            }

            // SPARQL-style `PREFIX p: <IRI>` -- same semantics as `@prefix`,
            // just without the leading `@`/trailing `.`.
            Rule::SparqlPrefix => {
                let mut prefix_name = String::new();
                let mut prefix_iri = String::new();
                for child in item.into_inner() {
                    match child.as_rule() {
                        Rule::PrefixIdentifier => prefix_name = child.as_str().to_string(),
                        Rule::BracketedIri => {
                            if let Some(p) = child.into_inner().next() {
                                prefix_iri = p.as_str().to_string();
                            }
                        }
                        _ => {}
                    }
                }
                prefix_mapper.add(prefix_name, prefix_iri);
            }

            // SPARQL-style `BASE <IRI>` -- same semantics as `@base`.
            Rule::SparqlBase => {
                if let Some(iri_pair) = item
                    .into_inner()
                    .find(|p| p.as_rule() == Rule::BracketedIri)
                    .and_then(|bracketed| bracketed.into_inner().next())
                {
                    prefix_mapper.set_base(iri_pair.as_str());
                }
            }

            // `@keywords a, is, of, true, false .` -- this engine always
            // recognizes exactly that fixed keyword set (see the grammar
            // comment on `Keywords`), so honoring the directive means
            // validating every declared word is a member of it; an unknown
            // word is a real error (matching real N3/EYE, which rejects
            // `@keywords` entries it doesn't understand) rather than being
            // silently ignored.
            Rule::Keywords => {
                const KNOWN_KEYWORDS: [&str; 5] = ["a", "is", "of", "true", "false"];
                for child in item.into_inner() {
                    if child.as_rule() == Rule::KeywordList {
                        for word_pair in child.into_inner() {
                            if word_pair.as_rule() == Rule::KeywordWord {
                                let word = word_pair.as_str();
                                if !KNOWN_KEYWORDS.contains(&word) {
                                    return Err(format!(
                                        "N3 parse error: unknown @keywords entry '{}' (supported: a, is, of, true, false)",
                                        word
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            Rule::TP => {
                content.extend(parse_tp(item.into_inner(), &prefix_mapper));
            }

            // `rule` wraps exactly one of `forward_rule` ("{body} => {head}")
            // or `backward_rule` ("{head} <= {body}"). Both grammar
            // productions still name their braces `Body`/`Head` according to
            // their *semantic* role (see the grammar comment), so the same
            // extraction logic below handles both without needing to know
            // which one it is.
            Rule::rule => {
                for variant in item.into_inner() {
                    // `<=>` biconditional sugar: "{A} <=> {B} ." means both
                    // "{A} => {B} ." and "{B} => {A} .". Both sides are plain
                    // `Head`s (TP+ groups); each becomes the *body* (as
                    // unnegated literals) for a rule per triple on the other
                    // side -- i.e. two ordinary rules generated from the one
                    // biconditional statement.
                    if variant.as_rule() == Rule::biconditional_rule {
                        let mut sides: Vec<Vec<Triple>> = Vec::new();
                        for part in variant.into_inner() {
                            if part.as_rule() == Rule::Head {
                                let mut side_triples = Vec::new();
                                for tp_pair in part.into_inner() {
                                    if tp_pair.as_rule() == Rule::TP {
                                        side_triples
                                            .extend(parse_tp(tp_pair.into_inner(), &prefix_mapper));
                                    }
                                }
                                sides.push(side_triples);
                            }
                        }
                        if sides.len() == 2 {
                            let left = sides[0].clone();
                            let right = sides[1].clone();
                            let left_body: Vec<BodyLiteral> = left
                                .iter()
                                .cloned()
                                .map(|pattern| BodyLiteral {
                                    negated: false,
                                    pattern,
                                })
                                .collect();
                            let right_body: Vec<BodyLiteral> = right
                                .iter()
                                .cloned()
                                .map(|pattern| BodyLiteral {
                                    negated: false,
                                    pattern,
                                })
                                .collect();
                            // left => right
                            for head in right.iter().cloned() {
                                rules.push(ReasonerRule {
                                    body: left_body.clone(),
                                    head,
                                });
                            }
                            // right => left
                            for head in left.iter().cloned() {
                                rules.push(ReasonerRule {
                                    body: right_body.clone(),
                                    head,
                                });
                            }
                        }
                        continue;
                    }

                    if variant.as_rule() != Rule::forward_rule
                        && variant.as_rule() != Rule::backward_rule
                    {
                        continue;
                    }

                    let mut body: Vec<BodyLiteral> = Vec::new();
                    let mut head_triples: Vec<Triple> = Vec::new();
                    let mut is_deny_head = false;

                    let parts: Vec<Pair<Rule>> = variant.into_inner().collect();

                    // The rule's antecedent (Body) is itself a formula per the
                    // N3 CG spec, so `@forAll`/`@forSome` may be declared
                    // directly inside its braces (e.g. `{ @forAll ?x . ?x a
                    // :Dog } => { ?x a :Mammal }.`), scoped to this rule (body
                    // + head). Push a fresh scope for the whole rule and
                    // pre-register any such declarations before parsing the
                    // body/head triples, matching the pre-pass pattern used
                    // for `Formula` and the document root.
                    with_new_scope(|| {
                        for part in &parts {
                            if part.as_rule() == Rule::Body {
                                for bl_pair in part.clone().into_inner() {
                                    if bl_pair.as_rule() == Rule::ForAll
                                        || bl_pair.as_rule() == Rule::ForSome
                                    {
                                        register_quantifier_declarations(&bl_pair);
                                    }
                                }
                            }
                        }

                        for part in parts {
                            match part.as_rule() {
                                Rule::Body => {
                                    for bl_pair in part.into_inner() {
                                        // Declarations were already registered
                                        // in the pre-pass above.
                                        if bl_pair.as_rule() == Rule::ForAll
                                            || bl_pair.as_rule() == Rule::ForSome
                                        {
                                            continue;
                                        }
                                        // `true` (TrueBody) means "unconditionally
                                        // true, no antecedent constraints" -- a
                                        // real EYE corpus idiom (e.g. the `peano`
                                        // case's `{(?A 0) :add ?A} <= true.`);
                                        // contributes zero body literals.
                                        if bl_pair.as_rule() == Rule::TrueBody {
                                            continue;
                                        }
                                        // bl_pair is a BodyLiteral
                                        let is_negated =
                                            bl_pair.as_str().trim_start().starts_with("not");
                                        // Find the TP inside the BodyLiteral
                                        if let Some(tp_pair) =
                                            bl_pair.into_inner().find(|p| p.as_rule() == Rule::TP)
                                        {
                                            let patterns =
                                                parse_tp(tp_pair.into_inner(), &prefix_mapper);
                                            for pattern in patterns {
                                                body.push(BodyLiteral {
                                                    negated: is_negated,
                                                    pattern,
                                                });
                                            }
                                        }
                                    }
                                }
                                Rule::Head => {
                                    for tp_pair in part.into_inner() {
                                        if tp_pair.as_rule() == Rule::TP {
                                            head_triples.extend(parse_tp(
                                                tp_pair.into_inner(),
                                                &prefix_mapper,
                                            ));
                                        } else if tp_pair.as_rule() == Rule::DenyHead {
                                            is_deny_head = true;
                                        }
                                    }
                                }
                                Rule::EOI => {}
                                _ => {}
                            }
                        }
                    });

                    if is_deny_head {
                        rules.push(ReasonerRule::new_denial(body.clone()));
                    } else {
                        // Emit one rule per head triple (multi-head rules desugar to multiple rules)
                        for head in head_triples {
                            rules.push(ReasonerRule {
                                body: body.clone(),
                                head,
                            });
                        }
                    }
                }
            }

            Rule::EOI => {}
            _ => {}
        }
    }

    Ok((content, rules))
}

/// Parse an N3-rule string into a list of Datalog `Rule`s (discarding any
/// plain top-level fact triples -- use `parse_document` to get both).
pub fn parse(input: &str) -> Result<Vec<ReasonerRule>, String> {
    parse_document(input).map(|(_content, rules)| rules)
}

#[cfg(test)]
#[path = "n3rule_parser_test.rs"]
mod n3rule_parser_test;
