use praxis_graphlaw::TripleStore;

/// Decode a set of derived triples into "s p o" strings for easy substring assertions.
fn decode_all(triples: &[praxis_graphlaw::triples::Triple]) -> Vec<String> {
    triples.iter().map(TripleStore::decode_triple).collect()
}

/// TICKET-005 (DoD): Test the log:equalTo built-in.
#[test]
fn test_log_builtin_equal_to() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :itemA :value 42 .\n\
                :itemB :value 42 .\n\
                \n\
                { ?x :value ?val1 . ?y :value ?val2 . ?val1 log:equalTo ?val2 } => { ?x :sameValueAs ?y }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    // log:equalTo holds for every (x, y) pair since both items share the value 42,
    // so all four combinations (including the two reflexive ones) should fire.
    assert_eq!(
        4,
        decoded.len(),
        "expected 4 derived sameValueAs triples, got: {:?}",
        decoded
    );
    for (a, b) in [
        ("itemA", "itemA"),
        ("itemA", "itemB"),
        ("itemB", "itemA"),
        ("itemB", "itemB"),
    ] {
        assert!(
            decoded.iter().any(|d| d.contains(&format!("/{}", a))
                && d.contains("sameValueAs")
                && d.contains(&format!("/{}>", b))),
            "{} sameValueAs {} should have been derived, got: {:?}",
            a,
            b,
            decoded
        );
    }
}

/// TICKET-005 (DoD): Test the log:implies built-in (dynamic rule implication).
///
/// Design note: the antecedent quoted graph is written with the *same*
/// variable (`?citizen`) that appears in the rule's own head. This engine
/// uses one flat, process-wide variable namespace (no @forSome/@forAll-style
/// per-formula scoping), so reusing the variable name is exactly how a
/// binding threads from the dynamically-matched antecedent through to the
/// consequent: `?formula log:implies {consequent}` is evaluated by matching
/// `?formula`'s own stored triples against the live data (here, `:bob a
/// :GoodCitizen`), and substituting the resulting bindings into both the
/// consequent template and the rule head.
#[test]
fn test_log_builtin_implies() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :bob a :GoodCitizen .\n\
                :alice :says { ?citizen a :GoodCitizen } .\n\
                { ?speaker :says ?formula . ?formula log:implies { ?citizen a :TaxPayer } } => { ?citizen a :TaxPayer }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/bob") && d.contains("TaxPayer")),
        "expected :bob a :TaxPayer to be derived via log:implies, got: {:?}",
        decoded
    );
}

/// TICKET-005 (DoD): Test the math:sum built-in.
#[test]
fn test_math_builtin_sum() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :order :itemPrice 10 .\n\
                :order :taxPrice 2 .\n\
                \n\
                { ?o :itemPrice ?p1 . ?o :taxPrice ?p2 . ( ?p1 ?p2 ) math:sum ?total } => { ?o :totalPrice ?total }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/order") && d.contains("totalPrice") && d.contains("\"12\"")),
        "expected :order :totalPrice 12 to be derived, got: {:?}",
        decoded
    );
}

/// TICKET-005 (DoD): Test the math:greaterThan built-in.
#[test]
fn test_math_builtin_greater_than() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :alice :age 25 .\n\
                :bob :age 18 .\n\
                \n\
                { ?person :age ?a . ?a math:greaterThan 21 } => { ?person a :Adult }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert_eq!(
        1,
        decoded.len(),
        "expected exactly 1 derived triple, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/alice") && d.contains("Adult")),
        "expected :alice a :Adult (25 > 21), got: {:?}",
        decoded
    );
    assert!(
        !decoded.iter().any(|d| d.contains("/bob")),
        ":bob (age 18) should not be classified as an Adult, got: {:?}",
        decoded
    );
}

/// TICKET-005 (DoD): Test the list:in built-in (membership test).
#[test]
fn test_list_builtin_in() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :myList :elements ( :apple :banana :cherry ) .\n\
                \n\
                { ?s :elements ?list . ?item list:in ?list } => { ?item a :Fruit }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert_eq!(
        3,
        decoded.len(),
        "expected 3 derived Fruit triples, got: {:?}",
        decoded
    );
    for fruit in ["/apple", "/banana", "/cherry"] {
        assert!(
            decoded
                .iter()
                .any(|d| d.contains(fruit) && d.contains("Fruit")),
            "expected {} a :Fruit to be derived, got: {:?}",
            fruit,
            decoded
        );
    }
}

/// TICKET-005 (DoD): Test the list:length built-in.
#[test]
fn test_list_builtin_length() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :shoppingCart :items ( :milk :bread :eggs :butter ) .\n\
                \n\
                { ?cart :items ?list . ?list list:length ?len } => { ?cart :itemCount ?len }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/shoppingCart") && d.contains("itemCount") && d.contains("\"4\"")),
        "expected :shoppingCart :itemCount 4 to be derived, got: {:?}",
        decoded
    );
}

/// TICKET-005 (DoD): Test the string:concat built-in.
#[test]
fn test_string_builtin_concat() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
                \n\
                :user :firstName \"John\" .\n\
                :user :lastName \"Doe\" .\n\
                \n\
                { ?u :firstName ?fn . ?u :lastName ?ln . ( ?fn \" \" ?ln ) string:concat ?fullName } => { ?u :name ?fullName }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/user") && d.contains("/name") && d.contains("John Doe")),
        "expected :user :name \"John Doe\" to be derived, got: {:?}",
        decoded
    );
}

/// TICKET-005 (DoD): Test the string:length built-in.
#[test]
fn test_string_builtin_length() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix string: <http://www.w3.org/2000/10/swap/string#> .\n\
                \n\
                :user :username \"johndoe123\" .\n\
                \n\
                { ?u :username ?name . ?name string:length ?len } => { ?u :usernameLength ?len }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/user") && d.contains("usernameLength") && d.contains("\"10\"")),
        "expected :user :usernameLength 10 to be derived, got: {:?}",
        decoded
    );
}

/// Test the math:difference, math:product, math:quotient, math:remainder
/// functional built-ins together (each mirrors math:sum's shape: a 2-element
/// list subject, functional result bound to the object).
#[test]
fn test_math_builtins_difference_product_quotient_remainder() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :calc :a 17 .\n\
                :calc :b 5 .\n\
                \n\
                { ?c :a ?x . ?c :b ?y . ( ?x ?y ) math:difference ?d } => { ?c :diff ?d }.\n\
                { ?c :a ?x . ?c :b ?y . ( ?x ?y ) math:product ?p } => { ?c :prod ?p }.\n\
                { ?c :a ?x . ?c :b ?y . ( ?x ?y ) math:quotient ?q } => { ?c :quot ?q }.\n\
                { ?c :a ?x . ?c :b ?y . ( ?x ?y ) math:remainder ?r } => { ?c :rem ?r }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/diff") && d.contains("\"12\"")),
        "expected 17-5=12, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/prod") && d.contains("\"85\"")),
        "expected 17*5=85, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/quot") && d.contains("\"3.4\"")),
        "expected 17/5=3.4, got: {:?}",
        decoded
    );
    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/rem") && d.contains("\"2\"")),
        "expected 17%5=2, got: {:?}",
        decoded
    );
}

/// Test the math:notLessThan, math:notGreaterThan, math:lessThan, and
/// math:equalTo row-filtering constraint built-ins (siblings of the
/// already-tested math:greaterThan) together.
#[test]
fn test_math_builtins_comparison_constraints() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix math: <http://www.w3.org/2000/10/swap/math#> .\n\
                \n\
                :ten :value 10 .\n\
                :twenty :value 20 .\n\
                \n\
                { ?x :value ?v . ?v math:notLessThan 10 } => { ?x :passesNotLessThan10 true }.\n\
                { ?x :value ?v . ?v math:notGreaterThan 10 } => { ?x :passesNotGreaterThan10 true }.\n\
                { ?x :value ?v . ?v math:lessThan 15 } => { ?x :passesLessThan15 true }.\n\
                { ?x :value ?v . ?v math:equalTo 10 } => { ?x :passesEqualTo10 true }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    // notLessThan 10: both 10 and 20 pass (10 >= 10, 20 >= 10).
    assert!(decoded
        .iter()
        .any(|d| d.contains("/ten") && d.contains("passesNotLessThan10")));
    assert!(decoded
        .iter()
        .any(|d| d.contains("/twenty") && d.contains("passesNotLessThan10")));
    // notGreaterThan 10: only 10 passes (10 <= 10), not 20.
    assert!(decoded
        .iter()
        .any(|d| d.contains("/ten") && d.contains("passesNotGreaterThan10")));
    assert!(!decoded
        .iter()
        .any(|d| d.contains("/twenty") && d.contains("passesNotGreaterThan10")));
    // lessThan 15: only 10 passes, not 20.
    assert!(decoded
        .iter()
        .any(|d| d.contains("/ten") && d.contains("passesLessThan15")));
    assert!(!decoded
        .iter()
        .any(|d| d.contains("/twenty") && d.contains("passesLessThan15")));
    // equalTo 10: only 10 passes.
    assert!(decoded
        .iter()
        .any(|d| d.contains("/ten") && d.contains("passesEqualTo10")));
    assert!(!decoded
        .iter()
        .any(|d| d.contains("/twenty") && d.contains("passesEqualTo10")));
}

/// Test the log:notEqualTo constraint built-in (negation of log:equalTo).
#[test]
fn test_log_builtin_not_equal_to() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix log: <http://www.w3.org/2000/10/swap/log#> .\n\
                \n\
                :itemA :value 42 .\n\
                :itemB :value 7 .\n\
                :itemC :value 42 .\n\
                \n\
                { ?x :value ?v1 . ?y :value ?v2 . ?v1 log:notEqualTo ?v2 } => { ?x :differsFrom ?y }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    // A (42) differs from B (7): yes. A (42) differs from C (42): no.
    assert!(decoded
        .iter()
        .any(|d| d.contains("/itemA") && d.contains("differsFrom") && d.contains("/itemB")));
    assert!(!decoded
        .iter()
        .any(|d| d.contains("/itemA") && d.contains("differsFrom") && d.contains("/itemC>")));
}

/// Test the list:append functional built-in (concatenates two list terms'
/// members into a new list term), verified via list:length and list:in on
/// the resulting merged list.
#[test]
fn test_list_builtin_append() {
    let data = "@prefix : <http://example.org/> .\n\
                @prefix list: <http://www.w3.org/2000/10/swap/list#> .\n\
                \n\
                :order :morningItems ( :eggs :bacon ) .\n\
                :order :eveningItems ( :soup :bread ) .\n\
                \n\
                { ?o :morningItems ?l1 . ?o :eveningItems ?l2 . ( ?l1 ?l2 ) list:append ?merged . ?merged list:length ?len } => { ?o :totalItemCount ?len }.\n\
                { ?o :morningItems ?l1 . ?o :eveningItems ?l2 . ( ?l1 ?l2 ) list:append ?merged . ?item list:in ?merged } => { ?item :inMergedList true }.";

    let mut store = TripleStore::from(data);
    let inferred = store.materialize().unwrap();
    let decoded = decode_all(&inferred);

    assert!(
        decoded
            .iter()
            .any(|d| d.contains("/order") && d.contains("totalItemCount") && d.contains("\"4\"")),
        "expected merged list length 4 (2+2), got: {:?}",
        decoded
    );
    for item in ["/eggs", "/bacon", "/soup", "/bread"] {
        assert!(
            decoded
                .iter()
                .any(|d| d.contains(item) && d.contains("inMergedList")),
            "expected {} to be a member of the appended list, got: {:?}",
            item,
            decoded
        );
    }
}
