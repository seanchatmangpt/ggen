//! Unit tests for every builtin in `crate::builtins`, exercised directly
//! against `classify`/`evaluate` (no full N3 parse/materialize pipeline).

use super::*;
use crate::{Triple, VarOrTerm};

fn num(n: i64) -> VarOrTerm {
    VarOrTerm::new_literal(
        n.to_string(),
        Some("<http://www.w3.org/2001/XMLSchema#integer>".into()),
        None,
    )
}

fn s(text: &str) -> VarOrTerm {
    VarOrTerm::new_literal(
        text.to_string(),
        Some("<http://www.w3.org/2001/XMLSchema#string>".into()),
        None,
    )
}

fn v(name: &str) -> VarOrTerm {
    VarOrTerm::new_var(name.to_string())
}

fn iri(x: &str) -> VarOrTerm {
    VarOrTerm::convert(x.to_string())
}

fn ground_triple(s_: VarOrTerm, p_: VarOrTerm, o_: VarOrTerm) -> Triple {
    Triple {
        s: s_,
        p: p_,
        o: o_,
        g: None,
    }
}

fn decoded_number(id: usize) -> f64 {
    numeric_value(id).expect("expected a numeric literal")
}

fn decoded_string(id: usize) -> String {
    lexical_value(id).expect("expected a literal")
}

// -- classify: sanity + reasoner-level registration --------------------

#[test]
fn classify_recognizes_all_procedural_builtins() {
    let cases = [
        (log::LOG_EQUAL_TO, BuiltinKind::EqualTo),
        (log::LOG_NOT_EQUAL_TO, BuiltinKind::NotEqualTo),
        (math::MATH_GREATER_THAN, BuiltinKind::GreaterThan),
        (math::MATH_SUM, BuiltinKind::Sum),
        (math::MATH_DIFFERENCE, BuiltinKind::Difference),
        (math::MATH_PRODUCT, BuiltinKind::Product),
        (math::MATH_QUOTIENT, BuiltinKind::Quotient),
        (math::MATH_REMAINDER, BuiltinKind::Remainder),
        (math::MATH_NOT_LESS_THAN, BuiltinKind::NotLessThan),
        (math::MATH_NOT_GREATER_THAN, BuiltinKind::NotGreaterThan),
        (math::MATH_LESS_THAN, BuiltinKind::LessThan),
        (math::MATH_EQUAL_TO, BuiltinKind::MathEqualTo),
        (string::STRING_LENGTH, BuiltinKind::StringLength),
        (string::STRING_CONCAT, BuiltinKind::StringConcat),
        (string::STRING_LESS_THAN, BuiltinKind::StringLessThan),
        (list::LIST_LENGTH, BuiltinKind::ListLength),
        (list::LIST_IN, BuiltinKind::ListIn),
        (list::LIST_APPEND, BuiltinKind::ListAppend),
        (
            func::FUNC_LANG_FROM_PLAIN_LITERAL,
            BuiltinKind::LangFromPlainLiteral,
        ),
        (list::LIST_FIRST, BuiltinKind::ListFirst),
        (list::LIST_REST, BuiltinKind::ListRest),
        (list::LIST_LAST, BuiltinKind::ListLast),
        (list::LIST_MEMBER, BuiltinKind::ListMember),
        (list::LIST_MEMBER_AT, BuiltinKind::ListMemberAt),
        (list::LIST_REMOVE, BuiltinKind::ListRemove),
        (list::LIST_SORT, BuiltinKind::ListSort),
        (list::LIST_UNIQUE, BuiltinKind::ListUnique),
        (list::LIST_REVERSE, BuiltinKind::ListReverse),
        (list::LIST_ITERATE, BuiltinKind::ListIterate),
    ];
    for (iri_str, expected) in cases {
        let p = iri(iri_str);
        assert_eq!(classify(&p), Some(expected), "misclassified {}", iri_str);
    }
}

#[test]
fn classify_registers_reasoner_level_builtins_as_known() {
    for iri_str in [LOG_IMPLIES, LOG_COLLECT_ALL_IN, LOG_NOT_INCLUDES] {
        let p = iri(iri_str);
        assert_eq!(
            classify(&p),
            Some(BuiltinKind::ReasonerLevel),
            "{} must classify as known (reasoner-level), not Unknown",
            iri_str
        );
    }
}

#[test]
fn classify_returns_none_for_unknown_predicate() {
    let p = iri("<http://example.org/notABuiltin>");
    assert_eq!(classify(&p), None);
}

#[test]
fn classify_returns_none_for_variable_predicate() {
    let p = v("p");
    assert_eq!(classify(&p), None);
}

#[test]
fn evaluate_returns_none_for_reasoner_level_kind() {
    // ReasonerLevel builtins are dispatched at the fixpoint level (reasoner/),
    // never through `builtins::evaluate` directly.
    let pattern = ground_triple(v("x"), v("y"), v("z"));
    let bindings = Binding::new();
    assert_eq!(
        evaluate(BuiltinKind::ReasonerLevel, &pattern, &bindings),
        None
    );
}

// -- log: -----------------------------------------------------------------

#[test]
fn log_equal_to_holds_for_equal_numbers() {
    let pattern = ground_triple(num(42), iri(log::LOG_EQUAL_TO), num(42));
    let out = log::eval_equal_to(&pattern, &Binding::new());
    assert!(out.is_some());
}

#[test]
fn log_equal_to_rejects_unequal_numbers() {
    let pattern = ground_triple(num(1), iri(log::LOG_EQUAL_TO), num(2));
    assert_eq!(log::eval_equal_to(&pattern, &Binding::new()), None);
}

#[test]
fn log_not_equal_to_holds_for_unequal_numbers() {
    let pattern = ground_triple(num(1), iri(log::LOG_NOT_EQUAL_TO), num(2));
    assert!(log::eval_not_equal_to(&pattern, &Binding::new()).is_some());
}

#[test]
fn log_not_equal_to_rejects_equal_numbers() {
    let pattern = ground_triple(num(5), iri(log::LOG_NOT_EQUAL_TO), num(5));
    assert_eq!(log::eval_not_equal_to(&pattern, &Binding::new()), None);
}

// -- math: ------------------------------------------------------------------

#[test]
fn math_greater_than_holds() {
    let pattern = ground_triple(num(5), iri(math::MATH_GREATER_THAN), num(3));
    assert!(math::eval_greater_than(&pattern, &Binding::new()).is_some());
}

#[test]
fn math_greater_than_rejects_wrong_direction() {
    let pattern = ground_triple(num(3), iri(math::MATH_GREATER_THAN), num(5));
    assert_eq!(math::eval_greater_than(&pattern, &Binding::new()), None);
}

#[test]
fn math_greater_than_rejects_non_numeric_operand() {
    let pattern = ground_triple(s("not-a-number"), iri(math::MATH_GREATER_THAN), num(3));
    assert_eq!(math::eval_greater_than(&pattern, &Binding::new()), None);
}

#[test]
fn math_less_than_holds() {
    let pattern = ground_triple(num(2), iri(math::MATH_LESS_THAN), num(9));
    assert!(math::eval_less_than(&pattern, &Binding::new()).is_some());
}

#[test]
fn math_not_less_than_holds_for_equal() {
    let pattern = ground_triple(num(4), iri(math::MATH_NOT_LESS_THAN), num(4));
    assert!(math::eval_not_less_than(&pattern, &Binding::new()).is_some());
}

#[test]
fn math_not_greater_than_holds_for_equal() {
    let pattern = ground_triple(num(4), iri(math::MATH_NOT_GREATER_THAN), num(4));
    assert!(math::eval_not_greater_than(&pattern, &Binding::new()).is_some());
}

#[test]
fn math_equal_to_holds() {
    let pattern = ground_triple(num(7), iri(math::MATH_EQUAL_TO), num(7));
    assert!(math::eval_math_equal_to(&pattern, &Binding::new()).is_some());
}

#[test]
fn math_equal_to_rejects_unequal() {
    let pattern = ground_triple(num(7), iri(math::MATH_EQUAL_TO), num(8));
    assert_eq!(math::eval_math_equal_to(&pattern, &Binding::new()), None);
}

#[test]
fn math_sum_computes_total() {
    let list = VarOrTerm::new_list(vec![num(2), num(3), num(4)]);
    let pattern = ground_triple(list, iri(math::MATH_SUM), v("out"));
    let out = math::eval_sum(&pattern, &Binding::new()).expect("sum should succeed");
    let out_var = pattern.o.to_encoded();
    let val = *out.get(&out_var).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 9.0);
}

#[test]
fn math_sum_rejects_non_list_subject() {
    // Wrong-type operand: subject is not a list term at all.
    let pattern = ground_triple(num(5), iri(math::MATH_SUM), v("out"));
    assert_eq!(math::eval_sum(&pattern, &Binding::new()), None);
}

#[test]
fn math_difference_computes_result() {
    let list = VarOrTerm::new_list(vec![num(10), num(4)]);
    let pattern = ground_triple(list, iri(math::MATH_DIFFERENCE), v("out"));
    let out = math::eval_difference(&pattern, &Binding::new()).expect("difference should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 6.0);
}

#[test]
fn math_difference_rejects_wrong_arity() {
    // math:difference is strictly binary -- a 3-element list must fail.
    let list = VarOrTerm::new_list(vec![num(10), num(4), num(1)]);
    let pattern = ground_triple(list, iri(math::MATH_DIFFERENCE), v("out"));
    assert_eq!(math::eval_difference(&pattern, &Binding::new()), None);
}

#[test]
fn math_product_computes_total() {
    let list = VarOrTerm::new_list(vec![num(2), num(3), num(5)]);
    let pattern = ground_triple(list, iri(math::MATH_PRODUCT), v("out"));
    let out = math::eval_product(&pattern, &Binding::new()).expect("product should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 30.0);
}

#[test]
fn math_quotient_computes_result() {
    let list = VarOrTerm::new_list(vec![num(20), num(4)]);
    let pattern = ground_triple(list, iri(math::MATH_QUOTIENT), v("out"));
    let out = math::eval_quotient(&pattern, &Binding::new()).expect("quotient should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 5.0);
}

#[test]
fn math_quotient_rejects_division_by_zero() {
    let list = VarOrTerm::new_list(vec![num(20), num(0)]);
    let pattern = ground_triple(list, iri(math::MATH_QUOTIENT), v("out"));
    assert_eq!(math::eval_quotient(&pattern, &Binding::new()), None);
}

#[test]
fn math_remainder_computes_result() {
    let list = VarOrTerm::new_list(vec![num(10), num(3)]);
    let pattern = ground_triple(list, iri(math::MATH_REMAINDER), v("out"));
    let out = math::eval_remainder(&pattern, &Binding::new()).expect("remainder should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 1.0);
}

#[test]
fn math_remainder_rejects_division_by_zero() {
    let list = VarOrTerm::new_list(vec![num(10), num(0)]);
    let pattern = ground_triple(list, iri(math::MATH_REMAINDER), v("out"));
    assert_eq!(math::eval_remainder(&pattern, &Binding::new()), None);
}

// -- string: ----------------------------------------------------------------

#[test]
fn string_length_counts_chars() {
    let pattern = ground_triple(s("hello"), iri(string::STRING_LENGTH), v("out"));
    let out = string::eval_string_length(&pattern, &Binding::new()).expect("length should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 5.0);
}

#[test]
fn string_length_rejects_non_var_object() {
    // Wrong-type/wrong-shape use: object must be a variable to receive the
    // computed length -- a ground object can never unify with a *computed*
    // result under this engine's one-shot functional-builtin contract.
    let pattern = ground_triple(s("hello"), iri(string::STRING_LENGTH), num(5));
    assert_eq!(string::eval_string_length(&pattern, &Binding::new()), None);
}

#[test]
fn string_concat_joins_members() {
    let list = VarOrTerm::new_list(vec![s("foo"), s("bar")]);
    let pattern = ground_triple(list, iri(string::STRING_CONCAT), v("out"));
    let out = string::eval_string_concat(&pattern, &Binding::new()).expect("concat should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_string(val), "foobar");
}

#[test]
fn string_concat_rejects_non_list_subject() {
    let pattern = ground_triple(s("not-a-list"), iri(string::STRING_CONCAT), v("out"));
    assert_eq!(string::eval_string_concat(&pattern, &Binding::new()), None);
}

#[test]
fn string_less_than_holds() {
    let pattern = ground_triple(s("abc"), iri(string::STRING_LESS_THAN), s("abd"));
    assert!(string::eval_string_less_than(&pattern, &Binding::new()).is_some());
}

#[test]
fn string_less_than_rejects_wrong_order() {
    let pattern = ground_triple(s("zzz"), iri(string::STRING_LESS_THAN), s("aaa"));
    assert_eq!(
        string::eval_string_less_than(&pattern, &Binding::new()),
        None
    );
}

// -- list: ------------------------------------------------------------------

#[test]
fn list_length_counts_members() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(3)]);
    let pattern = ground_triple(list, iri(list::LIST_LENGTH), v("out"));
    let out = list::eval_list_length(&pattern, &Binding::new()).expect("length should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 3.0);
}

#[test]
fn list_length_rejects_non_list_subject() {
    let pattern = ground_triple(num(1), iri(list::LIST_LENGTH), v("out"));
    assert_eq!(list::eval_list_length(&pattern, &Binding::new()), None);
}

#[test]
fn list_in_generates_each_member() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(3)]);
    let pattern = ground_triple(v("x"), iri(list::LIST_IN), list);
    let out = list::eval_list_in(&pattern, &Binding::new()).expect("list:in should succeed");
    let x_var = pattern.s.to_encoded();
    let vals: Vec<f64> = out
        .get(&x_var)
        .unwrap()
        .iter()
        .map(|&id| decoded_number(id))
        .collect();
    assert_eq!(vals.len(), 3);
    assert!(vals.contains(&1.0) && vals.contains(&2.0) && vals.contains(&3.0));
}

#[test]
fn list_in_rejects_non_var_subject() {
    // Wrong shape: list:in's subject position must be a variable (it's a
    // generator over the object list), a ground subject can't be generated into.
    let list = VarOrTerm::new_list(vec![num(1), num(2)]);
    let pattern = ground_triple(num(1), iri(list::LIST_IN), list);
    assert_eq!(list::eval_list_in(&pattern, &Binding::new()), None);
}

#[test]
fn list_append_concatenates_lists() {
    let l1 = VarOrTerm::new_list(vec![num(1), num(2)]);
    let l2 = VarOrTerm::new_list(vec![num(3), num(4)]);
    let members_list = VarOrTerm::new_list(vec![l1, l2]);
    let pattern = ground_triple(members_list, iri(list::LIST_APPEND), v("out"));
    let out = list::eval_list_append(&pattern, &Binding::new()).expect("append should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    let members = VarOrTerm::list_members(val).expect("result should be a list");
    let vals: Vec<f64> = members.iter().map(|&id| decoded_number(id)).collect();
    assert_eq!(vals, vec![1.0, 2.0, 3.0, 4.0]);
}

#[test]
fn list_append_rejects_wrong_arity() {
    // list:append takes exactly two lists to concatenate.
    let l1 = VarOrTerm::new_list(vec![num(1)]);
    let members_list = VarOrTerm::new_list(vec![l1]);
    let pattern = ground_triple(members_list, iri(list::LIST_APPEND), v("out"));
    assert_eq!(list::eval_list_append(&pattern, &Binding::new()), None);
}

#[test]
fn list_first_returns_first_member() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(3)]);
    let pattern = ground_triple(list, iri(list::LIST_FIRST), v("out"));
    let out = list::eval_list_first(&pattern, &Binding::new()).expect("first should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 1.0);
}

#[test]
fn list_first_rejects_empty_list() {
    let list = VarOrTerm::new_list(vec![]);
    let pattern = ground_triple(list, iri(list::LIST_FIRST), v("out"));
    assert_eq!(list::eval_list_first(&pattern, &Binding::new()), None);
}

#[test]
fn list_rest_drops_first_member() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(3)]);
    let pattern = ground_triple(list, iri(list::LIST_REST), v("out"));
    let out = list::eval_list_rest(&pattern, &Binding::new()).expect("rest should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    let members = VarOrTerm::list_members(val).expect("result should be a list");
    let vals: Vec<f64> = members.iter().map(|&id| decoded_number(id)).collect();
    assert_eq!(vals, vec![2.0, 3.0]);
}

#[test]
fn list_last_returns_last_member() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(3)]);
    let pattern = ground_triple(list, iri(list::LIST_LAST), v("out"));
    let out = list::eval_list_last(&pattern, &Binding::new()).expect("last should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_number(val), 3.0);
}

#[test]
fn list_member_generates_each_value() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(3)]);
    let pattern = ground_triple(list, iri(list::LIST_MEMBER), v("x"));
    let out = list::eval_list_member(&pattern, &Binding::new()).expect("member should succeed");
    let x_var = pattern.o.to_encoded();
    let vals: Vec<f64> = out
        .get(&x_var)
        .unwrap()
        .iter()
        .map(|&id| decoded_number(id))
        .collect();
    assert_eq!(vals.len(), 3);
    assert!(vals.contains(&1.0) && vals.contains(&2.0) && vals.contains(&3.0));
}

#[test]
fn list_member_rejects_non_var_object() {
    let list = VarOrTerm::new_list(vec![num(1), num(2)]);
    let pattern = ground_triple(list, iri(list::LIST_MEMBER), num(1));
    assert_eq!(list::eval_list_member(&pattern, &Binding::new()), None);
}

#[test]
fn list_member_at_returns_indexed_value() {
    let list = VarOrTerm::new_list(vec![s("a"), s("b"), s("c")]);
    let operands = VarOrTerm::new_list(vec![list, num(2)]);
    let pattern = ground_triple(operands, iri(list::LIST_MEMBER_AT), v("out"));
    let out =
        list::eval_list_member_at(&pattern, &Binding::new()).expect("memberAt should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_string(val), "c");
}

#[test]
fn list_member_at_rejects_out_of_range_index() {
    let list = VarOrTerm::new_list(vec![s("a"), s("b")]);
    let operands = VarOrTerm::new_list(vec![list, num(5)]);
    let pattern = ground_triple(operands, iri(list::LIST_MEMBER_AT), v("out"));
    assert_eq!(list::eval_list_member_at(&pattern, &Binding::new()), None);
}

#[test]
fn list_remove_drops_matching_items() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(1), num(3)]);
    let operands = VarOrTerm::new_list(vec![list, num(1)]);
    let pattern = ground_triple(operands, iri(list::LIST_REMOVE), v("out"));
    let out = list::eval_list_remove(&pattern, &Binding::new()).expect("remove should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    let members = VarOrTerm::list_members(val).expect("result should be a list");
    let vals: Vec<f64> = members.iter().map(|&id| decoded_number(id)).collect();
    assert_eq!(vals, vec![2.0, 3.0]);
}

#[test]
fn list_sort_orders_numerically() {
    let list = VarOrTerm::new_list(vec![num(3), num(1), num(2)]);
    let pattern = ground_triple(list, iri(list::LIST_SORT), v("out"));
    let out = list::eval_list_sort(&pattern, &Binding::new()).expect("sort should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    let members = VarOrTerm::list_members(val).expect("result should be a list");
    let vals: Vec<f64> = members.iter().map(|&id| decoded_number(id)).collect();
    assert_eq!(vals, vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_unique_dedupes_preserving_order() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(1), num(3), num(2)]);
    let pattern = ground_triple(list, iri(list::LIST_UNIQUE), v("out"));
    let out = list::eval_list_unique(&pattern, &Binding::new()).expect("unique should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    let members = VarOrTerm::list_members(val).expect("result should be a list");
    let vals: Vec<f64> = members.iter().map(|&id| decoded_number(id)).collect();
    assert_eq!(vals, vec![1.0, 2.0, 3.0]);
}

#[test]
fn list_reverse_reverses_members() {
    let list = VarOrTerm::new_list(vec![num(1), num(2), num(3)]);
    let pattern = ground_triple(list, iri(list::LIST_REVERSE), v("out"));
    let out = list::eval_list_reverse(&pattern, &Binding::new()).expect("reverse should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    let members = VarOrTerm::list_members(val).expect("result should be a list");
    let vals: Vec<f64> = members.iter().map(|&id| decoded_number(id)).collect();
    assert_eq!(vals, vec![3.0, 2.0, 1.0]);
}

#[test]
fn list_iterate_generates_index_item_pairs() {
    let list = VarOrTerm::new_list(vec![s("a"), s("b")]);
    let pattern = ground_triple(list, iri(list::LIST_ITERATE), v("pair"));
    let out = list::eval_list_iterate(&pattern, &Binding::new()).expect("iterate should succeed");
    let pair_var = pattern.o.to_encoded();
    let vals = out.get(&pair_var).unwrap();
    assert_eq!(vals.len(), 2);
    let pairs: Vec<(f64, String)> = vals
        .iter()
        .map(|&id| {
            let members = VarOrTerm::list_members(id).expect("pair should be a list");
            assert_eq!(members.len(), 2);
            (decoded_number(members[0]), decoded_string(members[1]))
        })
        .collect();
    assert!(pairs.contains(&(0.0, "a".to_string())));
    assert!(pairs.contains(&(1.0, "b".to_string())));
}

// -- func: --------------------------------------------------------------

#[test]
fn func_lang_from_plain_literal_extracts_tag() {
    let lit = VarOrTerm::new_literal("bonjour".to_string(), None, Some("fr".to_string()));
    let members_list = VarOrTerm::new_list(vec![lit]);
    let pattern = ground_triple(
        members_list,
        iri(func::FUNC_LANG_FROM_PLAIN_LITERAL),
        v("out"),
    );
    let out =
        func::eval_lang_from_plain_literal(&pattern, &Binding::new()).expect("should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_string(val), "fr");
}

#[test]
fn func_lang_from_plain_literal_defaults_to_empty_string() {
    let lit = s("no-lang-tag");
    let members_list = VarOrTerm::new_list(vec![lit]);
    let pattern = ground_triple(
        members_list,
        iri(func::FUNC_LANG_FROM_PLAIN_LITERAL),
        v("out"),
    );
    let out =
        func::eval_lang_from_plain_literal(&pattern, &Binding::new()).expect("should succeed");
    let val = *out.get(&pattern.o.to_encoded()).unwrap().first().unwrap();
    assert_eq!(decoded_string(val), "");
}

#[test]
fn func_lang_from_plain_literal_rejects_wrong_arity() {
    // The RIF function takes exactly one literal argument.
    let members_list = VarOrTerm::new_list(vec![s("a"), s("b")]);
    let pattern = ground_triple(
        members_list,
        iri(func::FUNC_LANG_FROM_PLAIN_LITERAL),
        v("out"),
    );
    assert_eq!(
        func::eval_lang_from_plain_literal(&pattern, &Binding::new()),
        None
    );
}

// -- dispatch: evaluate() routes to the same result as the direct call -----

#[test]
fn evaluate_dispatches_sum_same_as_direct_call() {
    let list = VarOrTerm::new_list(vec![num(1), num(1)]);
    let pattern = ground_triple(list, iri(math::MATH_SUM), v("out"));
    let via_dispatch =
        evaluate(BuiltinKind::Sum, &pattern, &Binding::new()).expect("dispatch should succeed");
    let val = *via_dispatch
        .get(&pattern.o.to_encoded())
        .unwrap()
        .first()
        .unwrap();
    assert_eq!(decoded_number(val), 2.0);
}
