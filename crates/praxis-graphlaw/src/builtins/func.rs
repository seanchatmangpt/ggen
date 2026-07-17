//! `func:` (RIF builtin-function) namespace builtins.
//!
//! All predicates here follow RIF's list-arg calling convention (like
//! `func:lang-from-PlainLiteral` below): the subject is a list holding the
//! function's argument(s), the object is the result. This mirrors how
//! EYE/cwm surface RIF builtin-function predicates in N3 rule bodies.

use super::{
    eval_functional, intern_number, intern_string, lang_value, lexical_value, numeric_value,
    subject_list_members,
};
use crate::{Binding, Triple, VarOrTerm};

// Real EYE `skos` corpus case idiom: `(?x) func:lang-from-PlainLiteral ?lang`,
// checking every resource has no more than one skos:prefLabel per language tag.
pub(crate) const FUNC_LANG_FROM_PLAIN_LITERAL: &str =
    "<http://www.w3.org/2007/rif-builtin-function#lang-from-PlainLiteral>";

pub(crate) const FUNC_NUMERIC_ADD: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-add>";
pub(crate) const FUNC_NUMERIC_SUBTRACT: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-subtract>";
pub(crate) const FUNC_NUMERIC_MULTIPLY: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-multiply>";
pub(crate) const FUNC_NUMERIC_DIVIDE: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-divide>";
pub(crate) const FUNC_NUMERIC_INTEGER_DIVIDE: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-integer-divide>";
pub(crate) const FUNC_NUMERIC_MOD: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-mod>";
pub(crate) const FUNC_NUMERIC_ABS: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-abs>";
pub(crate) const FUNC_NUMERIC_NEGATE: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-negate>";
pub(crate) const FUNC_NUMERIC_EQUAL: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-equal>";
pub(crate) const FUNC_NUMERIC_LESS_THAN: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-less-than>";
pub(crate) const FUNC_NUMERIC_GREATER_THAN: &str =
    "<http://www.w3.org/2007/rif-builtin-function#numeric-greater-than>";
pub(crate) const FUNC_STRING_LENGTH: &str =
    "<http://www.w3.org/2007/rif-builtin-function#string-length>";
pub(crate) const FUNC_SUBSTRING: &str = "<http://www.w3.org/2007/rif-builtin-function#substring>";

pub(crate) fn eval_lang_from_plain_literal(
    pattern: &Triple,
    bindings: &Binding,
) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        // RIF's `func:lang-from-PlainLiteral` extracts a literal's
        // language tag, or the empty string if it has none -- called
        // list-arg style like math:sum/string:concat (subject is a
        // one-element list holding the literal to inspect).
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 1 {
            return None;
        }
        Some(intern_string(lang_value(members[0])?))
    })
}

fn intern_boolean(value: bool) -> usize {
    VarOrTerm::new_literal(
        value.to_string(),
        Some("<http://www.w3.org/2001/XMLSchema#boolean>".to_string()),
        None,
    )
    .to_encoded()
}

fn eval_numeric_binary(
    pattern: &Triple,
    bindings: &Binding,
    op: impl Fn(f64, f64) -> Option<f64>,
) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let a = numeric_value(members[0])?;
        let b = numeric_value(members[1])?;
        Some(intern_number(op(a, b)?))
    })
}

fn eval_numeric_unary(
    pattern: &Triple,
    bindings: &Binding,
    op: impl Fn(f64) -> f64,
) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 1 {
            return None;
        }
        let a = numeric_value(members[0])?;
        Some(intern_number(op(a)))
    })
}

fn eval_numeric_predicate(
    pattern: &Triple,
    bindings: &Binding,
    op: impl Fn(f64, f64) -> bool,
) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let a = numeric_value(members[0])?;
        let b = numeric_value(members[1])?;
        Some(intern_boolean(op(a, b)))
    })
}

pub(crate) fn eval_numeric_add(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_binary(pattern, bindings, |a, b| Some(a + b))
}

pub(crate) fn eval_numeric_subtract(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_binary(pattern, bindings, |a, b| Some(a - b))
}

pub(crate) fn eval_numeric_multiply(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_binary(pattern, bindings, |a, b| Some(a * b))
}

pub(crate) fn eval_numeric_divide(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_binary(
        pattern,
        bindings,
        |a, b| if b == 0.0 { None } else { Some(a / b) },
    )
}

pub(crate) fn eval_numeric_integer_divide(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_binary(pattern, bindings, |a, b| {
        if b == 0.0 {
            None
        } else {
            Some((a / b).trunc())
        }
    })
}

pub(crate) fn eval_numeric_mod(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_binary(
        pattern,
        bindings,
        |a, b| if b == 0.0 { None } else { Some(a % b) },
    )
}

pub(crate) fn eval_numeric_abs(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_unary(pattern, bindings, f64::abs)
}

pub(crate) fn eval_numeric_negate(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_unary(pattern, bindings, |a| -a)
}

pub(crate) fn eval_numeric_equal(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_predicate(pattern, bindings, |a, b| a == b)
}

pub(crate) fn eval_numeric_less_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_predicate(pattern, bindings, |a, b| a < b)
}

pub(crate) fn eval_numeric_greater_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_numeric_predicate(pattern, bindings, |a, b| a > b)
}

pub(crate) fn eval_string_length(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 1 {
            return None;
        }
        let lex = lexical_value(members[0])?;
        Some(intern_number(lex.chars().count() as f64))
    })
}

pub(crate) fn eval_substring(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() < 2 || members.len() > 3 {
            return None;
        }
        let s = lexical_value(members[0])?;
        let chars: Vec<char> = s.chars().collect();
        // RIF `func:substring` uses 1-based start indices (xpath fn:substring
        // semantics): the character at `start` is the first one included.
        let start_1based = numeric_value(members[1])?;
        let start0 = (start_1based.round() as i64 - 1).max(0) as usize;
        if start0 >= chars.len() {
            return Some(intern_string(String::new()));
        }
        let end0 = if members.len() == 3 {
            let len = numeric_value(members[2])?;
            (start0 + len.round().max(0.0) as usize).min(chars.len())
        } else {
            chars.len()
        };
        if end0 <= start0 {
            return Some(intern_string(String::new()));
        }
        Some(intern_string(chars[start0..end0].iter().collect()))
    })
}
