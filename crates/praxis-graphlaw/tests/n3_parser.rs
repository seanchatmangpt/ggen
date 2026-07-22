use praxis_graphlaw::parser::Parser;

/// TICKET-005 (DoD): Test parsing N3 rules with multiple triples in the head.
/// Decomposes `{Body} => {Head1 . Head2}` into multiple rules sharing the same body.
#[test]
fn test_parse_multi_triple_head() {
    let input = "@prefix : <http://example.org/> .\n\
                 { ?x :parent ?y . ?y :sibling ?z } => { ?x :uncle ?z . ?z :nephew ?x }.";

    // Once implemented, parse_rules (or similar N3 parse function) should succeed and return decomposed rules.
    // E.g., we expect 2 rules to be generated from this single rule definition.
    let result = Parser::parse_rules(input);

    match result {
        Ok(rules) => {
            assert_eq!(
                rules.len(),
                2,
                "Expected N3 multi-triple head to decompose into 2 rules."
            );
            // Rule 1: Body => { ?x :uncle ?z }
            let r1 = &rules[0];
            assert_eq!(r1.body.len(), 2);
            assert_eq!(
                r1.head.p.to_encoded(),
                praxis_graphlaw::encoding::Encoder::add("<http://example.org/uncle>".to_string())
            );

            // Rule 2: Body => { ?z :nephew ?x }
            let r2 = &rules[1];
            assert_eq!(r2.body.len(), 2);
            assert_eq!(
                r2.head.p.to_encoded(),
                praxis_graphlaw::encoding::Encoder::add("<http://example.org/nephew>".to_string())
            );
        }
        Err(e) => {
            // Fail if not implemented or parsing fails
            panic!("test_parse_multi_triple_head failed: {}", e);
        }
    }
}

/// TICKET-005 (DoD): Test parsing N3 literals (strings, numbers, langtags, datatypes).
#[test]
fn test_parse_literal_terms() {
    let input = "@prefix : <http://example.org/> .\n\
                 @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\
                 { ?s :hasValue 42 } => { ?s :status \"active\" }.\n\
                 { ?s :greeting \"hello\"@en } => { ?s :greetingFrench \"bonjour\"^^xsd:string }.";

    let result = Parser::parse_rules(input).expect("literal grammar should parse");
    assert_eq!(2, result.len());

    // { ?s :hasValue 42 } => { ?s :status "active" } -- the head object is a
    // plain (simple) literal, no explicit datatype in the source, so it
    // decodes without a `^^` suffix -- matching every other literal-producing
    // path in this engine (see `parser/n3_terms.rs::parse_literal_pair`).
    let r0 = &result[0];
    assert!(r0.head.o.is_term());
    let decoded0 = praxis_graphlaw::encoding::Encoder::decode(&r0.head.o.to_encoded()).unwrap();
    assert_eq!(decoded0, "\"active\"");
    // And the body's numeric literal decodes as a proper xsd:integer, not an
    // opaque "<42>"-style IRI-shaped token.
    let body_obj = &r0.body[0].pattern.o;
    let decoded_num = praxis_graphlaw::encoding::Encoder::decode(&body_obj.to_encoded()).unwrap();
    assert_eq!(
        decoded_num,
        "\"42\"^^<http://www.w3.org/2001/XMLSchema#integer>"
    );

    // { ?s :greeting "hello"@en } => { ?s :greetingFrench "bonjour"^^xsd:string }
    let r1 = &result[1];
    let decoded1 = praxis_graphlaw::encoding::Encoder::decode(&r1.head.o.to_encoded()).unwrap();
    assert_eq!(
        decoded1,
        "\"bonjour\"^^<http://www.w3.org/2001/XMLSchema#string>"
    );
    let greeting = &r1.body[0].pattern.o;
    let decoded_greeting =
        praxis_graphlaw::encoding::Encoder::decode(&greeting.to_encoded()).unwrap();
    assert_eq!(decoded_greeting, "\"hello\"@en");
}

/// TICKET-005 (DoD): Test parsing N3 blank nodes (_:x).
///
/// (This grammar supports the explicit `_:label` blank node form used here in
/// both body and head position; the bracketed anonymous-blank-node-property-
/// list shorthand `[ :p :o ]` is a separate, purely syntactic sugar this
/// grammar does not target.)
#[test]
fn test_parse_blank_nodes() {
    let input = "@prefix : <http://example.org/> .\n\
                 { _:blank :property :value } => { :s :p _:blank2 }.";

    let rules = Parser::parse_rules(input).expect("blank node grammar should parse");
    assert_eq!(1, rules.len());
    let rule = &rules[0];

    let body_subject =
        praxis_graphlaw::encoding::Encoder::decode_to_term(rule.body[0].pattern.s.to_encoded());
    assert!(matches!(
        body_subject,
        Some(praxis_graphlaw::triples::Term::BlankNode(_))
    ));

    let head_object = praxis_graphlaw::encoding::Encoder::decode_to_term(rule.head.o.to_encoded());
    assert!(matches!(
        head_object,
        Some(praxis_graphlaw::triples::Term::BlankNode(_))
    ));
}

/// TICKET-005 (DoD): Test parsing RDF lists (e.g. (1 2 3)).
///
/// Lists are represented as a single term (a synthetic blank-node handle
/// indexing a process-wide side table of ordered members -- see the design
/// note on `VarOrTerm::new_list` in triples.rs), recovered here via
/// `VarOrTerm::list_members`.
#[test]
fn test_parse_lists() {
    let input = "@prefix : <http://example.org/> .\n\
                 { ?s :hasList ( 1 2 3 ) } => { ?s :status :valid }.";

    let rules = Parser::parse_rules(input).expect("list grammar should parse");
    assert_eq!(1, rules.len());
    let list_term = &rules[0].body[0].pattern.o;
    let members = praxis_graphlaw::triples::VarOrTerm::list_members(list_term.to_encoded())
        .expect("subject should be a list term");
    assert_eq!(3, members.len());
    let decoded: Vec<String> = members
        .iter()
        .map(|id| praxis_graphlaw::encoding::Encoder::decode(id).unwrap())
        .collect();
    assert_eq!(
        decoded,
        vec![
            "\"1\"^^<http://www.w3.org/2001/XMLSchema#integer>",
            "\"2\"^^<http://www.w3.org/2001/XMLSchema#integer>",
            "\"3\"^^<http://www.w3.org/2001/XMLSchema#integer>",
        ]
    );
}

/// TICKET-005 (DoD), updated for real quantifier scoping (see
/// `n3rule_parser.rs`'s `ScopeStack`): `@forAll ?x` at the document root
/// universally quantifies `?x` (it stays an ordinary pattern variable, just
/// renamed to a formula-scoped symbol), while `@forSome ?y` existentially
/// quantifies `?y`, which is skolemized to a fresh blank node AT PARSE TIME
/// -- so `?y`'s occurrence in the rule is no longer a variable at all.
#[test]
fn test_parse_quantifiers() {
    let input = "@prefix : <http://example.org/> .\n\
                 @forAll ?x .\n\
                 @forSome ?y .\n\
                 { ?x :knows ?y } => { ?x :hasConnection :yes }.";

    let rules = Parser::parse_rules(input).expect("quantifier declarations should parse");
    assert_eq!(1, rules.len());
    assert_eq!(1, rules[0].body.len());
    assert!(
        rules[0].body[0].pattern.s.is_var(),
        "@forAll ?x stays a (renamed) variable"
    );
    assert!(
        !rules[0].body[0].pattern.o.is_var(),
        "@forSome ?y must be skolemized to a fixed blank node, not remain a variable"
    );
}

/// TICKET-005 (DoD): Test parsing quoted graphs (e.g., {...} used as terms).
///
/// Quoted graphs are represented as a single term (a synthetic blank-node
/// handle indexing a process-wide side table of triples -- see the design
/// note on `VarOrTerm::new_formula` in triples.rs), recovered here via
/// `VarOrTerm::formula_triples`.
#[test]
fn test_parse_quoted_graphs() {
    let input = "@prefix : <http://example.org/> .\n\
                 { :alice :believes { :bob :status :happy } } => { :bob :isEnviedBy :alice }.";

    let rules = Parser::parse_rules(input).expect("quoted graph grammar should parse");
    assert_eq!(1, rules.len());
    let formula_term = &rules[0].body[0].pattern.o;
    let triples = praxis_graphlaw::triples::VarOrTerm::formula_triples(formula_term.to_encoded())
        .expect("object should be a quoted-graph (formula) term");
    assert_eq!(1, triples.len());
    assert_eq!(
        praxis_graphlaw::encoding::Encoder::decode(&triples[0].s.to_encoded()).unwrap(),
        "<http://example.org/bob>"
    );
    assert_eq!(
        praxis_graphlaw::encoding::Encoder::decode(&triples[0].o.to_encoded()).unwrap(),
        "<http://example.org/happy>"
    );
}

/// TICKET-005 (DoD): Test parsing backward implication (<=).
#[test]
fn test_parse_backward_implication() {
    let input = "@prefix : <http://example.org/> .\n\
                 { ?x :status :happy } <= { ?x :status :rich }.";

    let result = Parser::parse_rules(input);
    match result {
        Ok(rules) => {
            assert_eq!(rules.len(), 1);
            // Backward implication swaps body and head at parse time,
            // translating to: { ?x :status :rich } => { ?x :status :happy }
            let rule = &rules[0];
            assert_eq!(rule.body.len(), 1);
            assert_eq!(
                rule.body[0].pattern.p.to_encoded(),
                praxis_graphlaw::encoding::Encoder::add("<http://example.org/status>".to_string())
            );
            assert_eq!(
                rule.body[0].pattern.o.to_encoded(),
                praxis_graphlaw::encoding::Encoder::add("<http://example.org/rich>".to_string())
            );
            assert_eq!(
                rule.head.o.to_encoded(),
                praxis_graphlaw::encoding::Encoder::add("<http://example.org/happy>".to_string())
            );
        }
        Err(_) => {
            todo!("TICKET-005: Support backward implication <= in N3 grammar");
        }
    }
}

/// TICKET-005 (DoD): Test that N3 comments (#) are ignored during parsing.
#[test]
fn test_parse_comments_ignored() {
    let input = "# This is a prefix section comment\n\
                 @prefix : <http://example.org/> . # Line-end prefix comment\n\
                 # Rule starts here\n\
                 { ?x :status :rich } => { ?x :status :happy } . # Implication comment";

    let result = Parser::parse_rules(input);
    match result {
        Ok(rules) => {
            assert_eq!(rules.len(), 1);
            let rule = &rules[0];
            assert_eq!(rule.body.len(), 1);
        }
        Err(e) => {
            panic!("Failed to parse rules with comments: {}", e);
        }
    }
}

/// TICKET-005 (DoD): Test that malformed rules return a ParseError result instead of panicking.
#[test]
fn test_parse_error_returns_result_not_panic() {
    // Malformed input lacking closing brace
    let input = "@prefix : <http://example.org/> .\n\
                 { ?x :status :rich => { ?x :status :happy }.";

    let result = std::panic::catch_unwind(|| Parser::parse_rules(input));

    match result {
        Ok(parse_result) => {
            assert!(
                parse_result.is_err(),
                "Expected parsing a malformed rule to return an Err Result."
            );
        }
        Err(_) => {
            panic!("Parser panicked instead of returning Err Result!");
        }
    }
}
