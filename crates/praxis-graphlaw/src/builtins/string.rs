//! `string:` namespace builtins.

use super::{
    eval_functional, eval_row_constraint, intern_number, intern_string, lexical_value,
    resolve_operand, subject_list_members,
};
use crate::{Binding, Triple, VarOrTerm};
use regex::Regex;

pub(crate) const STRING_LENGTH: &str = "<http://www.w3.org/2000/10/swap/string#length>";
pub(crate) const STRING_CONCAT: &str = "<http://www.w3.org/2000/10/swap/string#concat>";
pub(crate) const STRING_LESS_THAN: &str = "<http://www.w3.org/2000/10/swap/string#lessThan>";
pub(crate) const STRING_CONTAINS: &str = "<http://www.w3.org/2000/10/swap/string#contains>";
pub(crate) const STRING_CONTAINS_IGNORING_CASE: &str =
    "<http://www.w3.org/2000/10/swap/string#containsIgnoringCase>";
pub(crate) const STRING_STARTS_WITH: &str = "<http://www.w3.org/2000/10/swap/string#startsWith>";
pub(crate) const STRING_ENDS_WITH: &str = "<http://www.w3.org/2000/10/swap/string#endsWith>";
pub(crate) const STRING_MATCHES: &str = "<http://www.w3.org/2000/10/swap/string#matches>";
pub(crate) const STRING_NOT_MATCHES: &str = "<http://www.w3.org/2000/10/swap/string#notMatches>";
pub(crate) const STRING_REPLACE: &str = "<http://www.w3.org/2000/10/swap/string#replace>";
pub(crate) const STRING_SUBSTRING: &str = "<http://www.w3.org/2000/10/swap/string#substring>";
pub(crate) const STRING_TO_UPPER_CASE: &str = "<http://www.w3.org/2000/10/swap/string#toUpperCase>";
pub(crate) const STRING_TO_LOWER_CASE: &str = "<http://www.w3.org/2000/10/swap/string#toLowerCase>";
pub(crate) const STRING_SPLIT: &str = "<http://www.w3.org/2000/10/swap/string#split>";
pub(crate) const STRING_EQUAL_IGNORING_CASE: &str =
    "<http://www.w3.org/2000/10/swap/string#equalIgnoringCase>";
pub(crate) const STRING_NOT_EQUAL_IGNORING_CASE: &str =
    "<http://www.w3.org/2000/10/swap/string#notEqualIgnoringCase>";
pub(crate) const STRING_GREATER_THAN: &str = "<http://www.w3.org/2000/10/swap/string#greaterThan>";
pub(crate) const STRING_FORMAT: &str = "<http://www.w3.org/2000/10/swap/string#format>";
pub(crate) const STRING_SCRAPE: &str = "<http://www.w3.org/2000/10/swap/string#scrape>";

pub(crate) fn eval_string_length(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let lex = lexical_value(s)?;
        Some(intern_number(lex.chars().count() as f64))
    })
}

pub(crate) fn eval_string_concat(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = super::subject_list_members(&pattern.s, bindings, row)?;
        let mut out = String::new();
        for m in members {
            out.push_str(&lexical_value(m)?);
        }
        Some(intern_string(out))
    })
}

pub(crate) fn eval_string_less_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a < b),
    )
}

pub(crate) fn eval_string_greater_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a > b),
    )
}

pub(crate) fn eval_string_contains(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a.contains(&b)),
    )
}

pub(crate) fn eval_string_contains_ignoring_case(
    pattern: &Triple, bindings: &Binding,
) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a.to_lowercase().contains(&b.to_lowercase())),
    )
}

pub(crate) fn eval_string_starts_with(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a.starts_with(&b)),
    )
}

pub(crate) fn eval_string_ends_with(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a.ends_with(&b)),
    )
}

pub(crate) fn eval_string_equal_ignoring_case(
    pattern: &Triple, bindings: &Binding,
) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a.to_lowercase() == b.to_lowercase()),
    )
}

pub(crate) fn eval_string_not_equal_ignoring_case(
    pattern: &Triple, bindings: &Binding,
) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b)) if a.to_lowercase() != b.to_lowercase()),
    )
}

pub(crate) fn eval_string_matches(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(pattern, bindings, |s, o| {
        matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b))
            if Regex::new(&b).map(|re| re.is_match(&a)).unwrap_or(false))
    })
}

pub(crate) fn eval_string_not_matches(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(pattern, bindings, |s, o| {
        matches!((lexical_value(s), lexical_value(o)), (Some(a), Some(b))
            if !Regex::new(&b).map(|re| re.is_match(&a)).unwrap_or(false))
    })
}

pub(crate) fn eval_string_to_upper_case(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        Some(intern_string(lexical_value(s)?.to_uppercase()))
    })
}

pub(crate) fn eval_string_to_lower_case(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        Some(intern_string(lexical_value(s)?.to_lowercase()))
    })
}

/// `(string pattern replacement) string:replace result` -- regex-based,
/// all non-overlapping matches replaced (mirrors EYE/cwm semantics).
pub(crate) fn eval_string_replace(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 3 {
            return None;
        }
        let s = lexical_value(members[0])?;
        let pat = lexical_value(members[1])?;
        let rep = lexical_value(members[2])?;
        let re = Regex::new(&pat).ok()?;
        Some(intern_string(re.replace_all(&s, rep.as_str()).into_owned()))
    })
}

/// `(string start end) string:substring result` -- 0-based, end-exclusive,
/// operating on Unicode scalar values (chars).
pub(crate) fn eval_string_substring(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 3 {
            return None;
        }
        let s = lexical_value(members[0])?;
        let start = super::numeric_value(members[1])? as usize;
        let end = super::numeric_value(members[2])? as usize;
        let chars: Vec<char> = s.chars().collect();
        if start > end || end > chars.len() {
            return None;
        }
        Some(intern_string(chars[start..end].iter().collect()))
    })
}

/// `(string separator) string:split result` -- result is a list of the
/// substrings between (literal, not regex) occurrences of `separator`.
pub(crate) fn eval_string_split(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let s = lexical_value(members[0])?;
        let sep = lexical_value(members[1])?;
        let parts: Vec<VarOrTerm> = if sep.is_empty() {
            s.chars()
                .map(|c| VarOrTerm::new_encoded_term(intern_string(c.to_string())))
                .collect()
        } else {
            s.split(sep.as_str())
                .map(|part| VarOrTerm::new_encoded_term(intern_string(part.to_string())))
                .collect()
        };
        Some(VarOrTerm::new_list(parts).to_encoded())
    })
}

/// `(format arg1 arg2 ...) string:format result` -- sequential `%s`
/// substitution (the common EYE/cwm idiom; other conversion specifiers are
/// not implemented since practice overwhelmingly uses `%s`).
pub(crate) fn eval_string_format(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.is_empty() {
            return None;
        }
        let fmt = lexical_value(members[0])?;
        let mut out = String::with_capacity(fmt.len());
        let mut args = members[1..].iter();
        let mut chars = fmt.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '%' {
                match chars.peek() {
                    Some('s') => {
                        chars.next();
                        let arg = args.next()?;
                        out.push_str(&lexical_value(*arg)?);
                    }
                    Some('%') => {
                        chars.next();
                        out.push('%');
                    }
                    _ => out.push('%'),
                }
            } else {
                out.push(c);
            }
        }
        Some(intern_string(out))
    })
}

/// `(string regex) string:scrape result` -- the first match of `regex`
/// against `string`; the first capture group if the pattern has one,
/// otherwise the whole match. `None` (builtin doesn't fire) if there is
/// no match, per EYE/cwm semantics.
pub(crate) fn eval_string_scrape(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let s = lexical_value(members[0])?;
        let pat = lexical_value(members[1])?;
        let re = Regex::new(&pat).ok()?;
        let caps = re.captures(&s)?;
        let matched = caps.get(1).or_else(|| caps.get(0))?;
        Some(intern_string(matched.as_str().to_string()))
    })
}
