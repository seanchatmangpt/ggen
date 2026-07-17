//! Tests probing several specific N3 characteristics flagged by a TBL-lens
//! re-audit: (1) whether nested quoted graphs (a formula containing another
//! formula) are handled without corrupting data or crashing, (2) whether
//! forward chaining correctly threads a derived fact from one rule into a
//! second rule's antecedent within the same `materialize()` fixpoint
//! (chained implication), and (3) `@forAll`/`@forSome` quantifier scoping
//! (see `lib/src/parser/n3rule_parser.rs`'s `ScopeStack`). (1) and (2) are
//! additive probes over the existing `log:implies`/quoted-graph
//! implementation in `lib/src/reasoner/log_implies.rs` and `lib/src/term.rs`
//! -- see those files' doc comments for the underlying mechanism
//! (`FORMULA_REGISTRY`, `process_log_implies_rule`).
//!
//! ## Documented scoping conflict (read before touching quantifier tests)
//!
//! Real N3 formula scoping would make EVERY bare (unquantified) variable
//! scoped to its smallest enclosing formula, so the same bare name reused in
//! two sibling quoted formulas would never collide even without an explicit
//! `@forAll`/`@forSome`. That reading is NOT what's implemented here, because
//! it directly conflicts with `log_implies.rs`'s dynamic `log:implies`
//! reification, which its own doc comment says is "matched across
//! antecedent/consequent/outer scopes purely by *name*" -- and the
//! `test_chained_implication_through_log_implies_*` tests below rely on
//! exactly that: `?citizen` bare in the top-level quoted formula
//! `:alice :says { ?citizen a :GoodCitizen }` must be the SAME variable as
//! the bare `?citizen` in the unrelated formula `{ ?citizen a :TaxPayer }`
//! nested inside a rule body, or the derivation never grounds. Auto-scoping
//! every bare variable per-formula would break that. So: this engine scopes
//! ONLY variables an author explicitly names in `@forAll`/`@forSome` (the
//! literal, spec-accurate meaning of those two declarations); bare variables
//! keep the pre-existing flat, name-based identity across formula
//! boundaries. The tests below lock in the explicit-quantifier behavior.

use praxis_graphlaw::parser::Parser;
use praxis_graphlaw::term::VarOrTerm;
use praxis_graphlaw::TripleStore;

fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples.iter().map(TripleStore::decode_triple).collect()
}

/// `materialize()` only returns newly-*derived* triples, not the base facts
/// already present in the store -- so checking whether a base fact was
/// preserved (as opposed to newly derived) requires reading the store's
/// index directly rather than relying on `materialize()`'s return value.
fn decode_all_stored_facts(store: &TripleStore) -> Vec<String> {
    (0..store.len())
        .filter_map(|i| store.triple_index.get(i))
        .map(TripleStore::decode_triple)
        .collect()
}

/// A quoted graph containing, as one of its own triples' objects, ANOTHER
/// quoted graph (two levels of nesting) must parse and materialize without
/// error, and the inner formula's triples must stay quoted -- i.e. they must
/// NOT leak into the plain asserted-fact set at any level. This guards
/// against a naive quoted-graph parser that only handles one level of `{ }`
/// nesting correctly and either flattens or drops the inner formula.
#[test]
fn test_nested_quoted_formula_does_not_leak_into_facts() {
    let data = "@prefix : <http://example.org/> .\n\
                \n\
                :carol a :Honest .\n\
                :alice :believes { :bob :says { :carol a :Dishonest } } .\n";

    let mut store = TripleStore::from(data);
    let _inferred = store.materialize().unwrap();
    let decoded = decode_all_stored_facts(&store);

    // The nested claim ":carol a :Dishonest" is buried two formula-levels
    // deep inside :alice's belief about what :bob says -- it must never
    // surface as a plain top-level fact (that would mean nesting leaked and
    // :carol is now contradictorily both Honest and Dishonest at top level).
    assert!(
        !decoded.iter().any(|d| d.contains("/carol") && d.contains("Dishonest")),
        "the nested quoted claim ':carol a :Dishonest' must stay quoted inside two levels of formula nesting, not leak into top-level facts. Stored facts: {:?}",
        decoded
    );

    // The genuinely top-level, unquoted fact must still be present and
    // unaffected by the nested formula's contradictory content.
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/carol") && d.contains("Honest") && !d.contains("Dishonest")),
        "the real top-level fact ':carol a :Honest' must be preserved. Stored facts: {:?}",
        decoded
    );
}

/// Two rules where the first rule's derived output is exactly the pattern
/// the second rule's antecedent requires, within a single `materialize()`
/// call (multi-step forward chaining, not manually re-invoking the
/// reasoner). This is distinct from the existing deep_taxonomy transitivity
/// tests (which chain the SAME rule against itself repeatedly); here two
/// DIFFERENT rules must correctly compose: rule 1 promotes a fact, rule 2
/// consumes exactly that promoted fact to derive a further one.
#[test]
fn test_chained_implication_across_two_distinct_rules() {
    let data = "@prefix : <http://example.org/> .\n\
                \n\
                :fido a :Dog .\n\
                \n\
                { ?x a :Dog } => { ?x a :Mammal }.\n\
                { ?x a :Mammal } => { ?x a :Animal }.\n";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/fido") && d.contains("Mammal")),
        "rule 1 (Dog => Mammal) must fire directly off the base fact. Derived: {:?}",
        decoded
    );
    assert!(
        decoded.iter().any(|d| d.contains("/fido") && d.contains("Animal")),
        "rule 2 (Mammal => Animal) must fire off rule 1's OUTPUT within the same materialize() pass, not just the base fact. Derived: {:?}",
        decoded
    );
}

/// Chained implication combined with `log:implies`: a dynamically-reified
/// rule's derived output must itself be able to feed a second, ordinary
/// rule's antecedent in the same pass -- confirming `log:implies`'s dynamic
/// reification composes with normal forward chaining rather than only
/// working in isolation.
#[test]
fn test_chained_implication_through_log_implies_then_ordinary_rule() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :bob a :GoodCitizen .\n\
                :alice :says { ?citizen a :GoodCitizen } .\n\
                { ?speaker :says ?formula . ?formula log:implies { ?citizen a :TaxPayer } } => { ?citizen a :TaxPayer }.\n\
                { ?p a :TaxPayer } => { ?p :owes :Taxes }.\n";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/bob") && d.contains("TaxPayer")),
        "expected :bob a :TaxPayer via log:implies. Derived: {:?}",
        decoded
    );
    assert!(
        decoded.iter().any(|d| d.contains("/bob") && d.contains("owes") && d.contains("Taxes")),
        "expected :bob :owes :Taxes to be derived from the log:implies-produced TaxPayer fact feeding the second ordinary rule. Derived: {:?}",
        decoded
    );
}

/// Chained `log:implies -> log:implies`: the fact dynamically derived by a
/// FIRST log:implies-reified rule must itself be able to satisfy a SECOND,
/// independent log:implies rule's quoted antecedent -- not just feed an
/// ordinary rule's plain body literal (already covered above). This
/// confirms dynamic formula reification composes with itself across
/// multiple fixpoint iterations, not only with static rules.
#[test]
fn test_chained_log_implies_through_two_independent_implies_rules() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :bob a :GoodCitizen .\n\
                :alice :says { ?citizen a :GoodCitizen } .\n\
                :carol :alsoSays { ?p a :TaxPayer } .\n\
                { ?speaker :says ?formula . ?formula log:implies { ?citizen a :TaxPayer } } => { ?citizen a :TaxPayer }.\n\
                { ?speaker2 :alsoSays ?formula2 . ?formula2 log:implies { ?p :owes :Taxes } } => { ?p :owes :Taxes }.\n";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/bob") && d.contains("TaxPayer")),
        "expected :bob a :TaxPayer via the FIRST log:implies rule. Derived: {:?}",
        decoded
    );
    assert!(
        decoded.iter().any(|d| d.contains("/bob") && d.contains("owes") && d.contains("Taxes")),
        "expected :bob :owes :Taxes via the SECOND log:implies rule, whose quoted antecedent ({{ ?p a :TaxPayer }}) matches the fact dynamically derived by the FIRST log:implies rule -- not a statically-asserted fact. Derived: {:?}",
        decoded
    );
}

/// `@forSome ?y` at the document root must skolemize `?y` to a fresh blank
/// node AT PARSE TIME, everywhere it's used in that scope -- so its
/// occurrence in a rule is no longer a variable at all, and every use of the
/// SAME declared name within that one scope resolves to the SAME skolem
/// (not a fresh one per occurrence).
#[test]
fn test_forsome_skolemizes_to_same_blank_node_within_one_scope() {
    let input = "@prefix : <http://example.org/> .\n\
                 @forSome ?y .\n\
                 { :alice :knows ?y } => { ?y a :Known }.";

    let rules = Parser::parse_rules(input).expect("forSome declaration should parse");
    assert_eq!(1, rules.len());
    assert_eq!(1, rules[0].body.len());

    let body_obj = &rules[0].body[0].pattern.o;
    let head_subj = &rules[0].head.s;
    assert!(
        !body_obj.is_var(),
        "@forSome ?y in the body must be skolemized, not a variable"
    );
    assert!(
        !head_subj.is_var(),
        "@forSome ?y in the head must be skolemized, not a variable"
    );
    assert_eq!(
        body_obj.to_encoded(),
        head_subj.to_encoded(),
        "both occurrences of the same @forSome-declared ?y within one scope must skolemize to the SAME blank node"
    );
}

/// Two INDEPENDENT documents that each explicitly `@forSome`-quantify a
/// variable of the same bare name must skolemize to genuinely DIFFERENT
/// blank nodes -- the skolem is fresh per declaration, not derived
/// deterministically from the bare name (which would make unrelated
/// existentials from separate documents collide).
#[test]
fn test_forsome_skolems_are_fresh_across_independent_documents() {
    let input = "@prefix : <http://example.org/> .\n\
                 @forSome ?y .\n\
                 { :alice :knows ?y } => { ?y a :Known }.";

    let rules_a = Parser::parse_rules(input).expect("first parse should succeed");
    let rules_b = Parser::parse_rules(input).expect("second parse should succeed");

    let skolem_a = &rules_a[0].head.s;
    let skolem_b = &rules_b[0].head.s;
    assert!(!skolem_a.is_var() && !skolem_b.is_var());
    assert_ne!(
        skolem_a.to_encoded(),
        skolem_b.to_encoded(),
        "@forSome skolems from two independently-parsed documents must not collide just because they share a bare variable name"
    );
}

/// `@forAll ?x` explicitly declared INSIDE a nested quoted formula (`{ }`
/// used as a term, not a rule's own Body/Head) scopes `?x` to that formula:
/// two SIBLING formulas that each independently declare `@forAll` for the
/// SAME bare name must end up with genuinely distinct underlying variables,
/// not the one shared/flat variable this engine uses for bare (unquantified)
/// names -- this is the real scoping behavior the plan calls for, applied to
/// the one case where it doesn't conflict with `log:implies`'s bare-name
/// threading (see the module doc comment above).
#[test]
fn test_forall_in_sibling_formulas_does_not_collide() {
    // `Parser::parse_rules` discards plain top-level facts, so the two
    // `@forAll`-quantified formulas are written as objects inside a rule
    // head instead (multi-head sugar), keeping both reachable from the
    // returned `Rule` structure via `VarOrTerm::formula_triples`.
    let input2 = "@prefix : <http://example.org/> .\n\
                  { :dummy :trigger :go } => {\n\
                    :alice :believesGeneral { @forAll ?x . ?x a :Person } .\n\
                    :bob :believesGeneral { @forAll ?x . ?x a :Robot } .\n\
                  }.";
    // Multi-head rules desugar to one Rule per head triple, so we get one
    // rule per believesGeneral triple, each with its own formula object.
    let rules2 = Parser::parse_rules(input2).expect("multi-head nested @forAll should parse");
    assert_eq!(2, rules2.len());

    let formula_a_id = rules2[0].head.o.to_encoded();
    let formula_b_id = rules2[1].head.o.to_encoded();
    let triples_a = VarOrTerm::formula_triples(formula_a_id).expect("formula A must be registered");
    let triples_b = VarOrTerm::formula_triples(formula_b_id).expect("formula B must be registered");
    assert_eq!(1, triples_a.len());
    assert_eq!(1, triples_b.len());

    let var_a = &triples_a[0].s;
    let var_b = &triples_b[0].s;
    assert!(
        var_a.is_var(),
        "@forAll ?x must remain a variable (universally quantified)"
    );
    assert!(
        var_b.is_var(),
        "@forAll ?x must remain a variable (universally quantified)"
    );
    assert_ne!(
        var_a.to_encoded(),
        var_b.to_encoded(),
        "the same bare name '?x', explicitly @forAll-quantified in two SIBLING formulas, must resolve to two distinct variables, not collide"
    );
}

/// `@forAll`/`@forSome` declared directly INSIDE a rule's own antecedent
/// braces (a `forward_rule`'s `Body`) must parse, not be rejected as a hard
/// syntax error. A rule's antecedent graph is itself a formula per the N3 CG
/// spec, so `{ @forAll ?x . ?x a :Dog } => { ?x a :Mammal }.` is a legitimate,
/// commonly-used idiom for self-contained portable N3 rules -- previously
/// `n3.pest`'s `Body` production only allowed `BodyLiteral`s inside the
/// antecedent braces (unlike the sibling `Formula` production, which already
/// allowed `ForAll`/`ForSome` alongside `TP`), so this failed with a hard
/// pest parse error ("expected Object") rather than either accepting the
/// quantifier or giving a semantic error.
#[test]
fn test_forall_declared_inside_rule_antecedent_braces_parses_and_scopes() {
    let input = "@prefix : <http://example.org/> .\n\
                 { @forAll ?x . ?x a :Dog } => { ?x a :Mammal }.";

    let rules = Parser::parse_rules(input)
        .expect("@forAll inside a rule's own antecedent braces must parse");
    assert_eq!(1, rules.len());
    assert_eq!(1, rules[0].body.len());

    let body_subj = &rules[0].body[0].pattern.s;
    let head_subj = &rules[0].head.s;
    assert!(
        body_subj.is_var(),
        "@forAll ?x must remain a variable (universally quantified)"
    );
    assert!(
        head_subj.is_var(),
        "@forAll ?x must remain a variable (universally quantified)"
    );
    assert_eq!(
        body_subj.to_encoded(),
        head_subj.to_encoded(),
        "the @forAll-declared ?x must be the SAME variable across this rule's body and head (scoped to the whole rule, not just the antecedent formula)"
    );

    // The declaration must actually take effect (real per-rule scoping), not
    // merely parse-and-ignore: an independently-parsed second rule that
    // declares @forAll for the same bare name in its own antecedent braces
    // must not collide with the first rule's variable.
    let input2 = "@prefix : <http://example.org/> .\n\
                  { @forAll ?x . ?x a :Cat } => { ?x a :Mammal }.";
    let rules2 = Parser::parse_rules(input2)
        .expect("second independent @forAll-in-antecedent rule should parse");
    assert_ne!(
        rules[0].body[0].pattern.s.to_encoded(),
        rules2[0].body[0].pattern.s.to_encoded(),
        "@forAll ?x declared in two independently-parsed rules' antecedents must not collide"
    );
}

/// Intra-rule multi-`log:implies`: a SINGLE rule body containing TWO
/// independent `log:implies` literals (each with its own quoted antecedent
/// formula, matched via a distinct `:says*` fact, and its own quoted
/// consequent) must derive the conclusions of BOTH, not just the first one
/// found by a naive `.position(...)`-based scan.
///
/// This locks in the chosen semantics (see `find_log_implies_literals`'s
/// doc comment in `lib/src/reasoner/log_implies.rs`): two `log:implies`
/// literals in one body are two ordinary, independently-evaluated body
/// literals that both happen to use the `log:implies` predicate, conjoined
/// with the shared "outer" bindings from the rest of the body -- not a
/// sequencing/consuming relationship, and not rejected as ambiguous.
///
/// Before the fix, `find_log_implies_literal` located only the first
/// `log:implies` literal and left the second in the "regular" (ordinary
/// query) body, where its pattern (`?f2 log:implies { ... }`) can never
/// match an actually-asserted triple -- so the outer-binding query failed
/// entirely and NEITHER conclusion (not even the first) was derived.
#[test]
fn test_two_independent_log_implies_literals_in_one_rule_body() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :bob a :GoodCitizen .\n\
                :carol a :Employed .\n\
                :alice :saysA { ?citizen a :GoodCitizen } .\n\
                :dave :saysB { ?worker a :Employed } .\n\
                { :alice :saysA ?f1 . ?f1 log:implies { ?citizen a :TaxPayer } .\n\
                  :dave :saysB ?f2 . ?f2 log:implies { ?worker a :Worker } } => { :multi :log_implies :triggered }.\n";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/bob") && d.contains("TaxPayer")),
        "expected :bob a :TaxPayer from the FIRST log:implies literal. Derived: {:?}",
        decoded
    );
    assert!(
        decoded.iter().any(|d| d.contains("/carol") && d.contains("Worker")),
        "expected :carol a :Worker from the SECOND, independent log:implies literal in the same rule body. Derived: {:?}",
        decoded
    );
    assert!(
        decoded.iter().any(|d| d.contains("multi") && d.contains("triggered")),
        "expected the rule's own head to fire once the shared outer bindings and both antecedents are satisfied. Derived: {:?}",
        decoded
    );
}
