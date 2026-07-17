//! `list:` namespace builtins.

use super::{
    copy_row, eval_functional, eval_generator, intern_number, resolve_operand, subject_list_members,
};
use crate::{Binding, Triple, VarOrTerm};

pub(crate) const LIST_LENGTH: &str = "<http://www.w3.org/2000/10/swap/list#length>";
pub(crate) const LIST_IN: &str = "<http://www.w3.org/2000/10/swap/list#in>";
pub(crate) const LIST_APPEND: &str = "<http://www.w3.org/2000/10/swap/list#append>";
pub(crate) const LIST_FIRST: &str = "<http://www.w3.org/2000/10/swap/list#first>";
pub(crate) const LIST_REST: &str = "<http://www.w3.org/2000/10/swap/list#rest>";
pub(crate) const LIST_LAST: &str = "<http://www.w3.org/2000/10/swap/list#last>";
pub(crate) const LIST_MEMBER: &str = "<http://www.w3.org/2000/10/swap/list#member>";
pub(crate) const LIST_MEMBER_AT: &str = "<http://www.w3.org/2000/10/swap/list#memberAt>";
pub(crate) const LIST_REMOVE: &str = "<http://www.w3.org/2000/10/swap/list#remove>";
pub(crate) const LIST_SORT: &str = "<http://www.w3.org/2000/10/swap/list#sort>";
pub(crate) const LIST_UNIQUE: &str = "<http://www.w3.org/2000/10/swap/list#unique>";
pub(crate) const LIST_REVERSE: &str = "<http://www.w3.org/2000/10/swap/list#reverse>";
pub(crate) const LIST_ITERATE: &str = "<http://www.w3.org/2000/10/swap/list#iterate>";
pub(crate) const LIST_NOT_MEMBER: &str = "<http://www.w3.org/2000/10/swap/list#notMember>";
pub(crate) const LIST_FIRST_REST: &str = "<http://www.w3.org/2000/10/swap/list#firstRest>";

pub(crate) fn eval_list_length(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        Some(intern_number(members.len() as f64))
    })
}

pub(crate) fn eval_list_in(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_generator(pattern, bindings)
}

pub(crate) fn eval_list_append(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let list1 = VarOrTerm::list_members(members[0])?;
        let list2 = VarOrTerm::list_members(members[1])?;
        let mut combined: Vec<VarOrTerm> = Vec::with_capacity(list1.len() + list2.len());
        for id in list1.into_iter().chain(list2) {
            combined.push(VarOrTerm::new_encoded_term(id));
        }
        Some(VarOrTerm::new_list(combined).to_encoded())
    })
}

fn ids_to_list(ids: &[usize]) -> usize {
    let members: Vec<VarOrTerm> = ids
        .iter()
        .map(|&id| VarOrTerm::new_encoded_term(id))
        .collect();
    VarOrTerm::new_list(members).to_encoded()
}

/// `list:first` -- subject is a list, object binds to its first member.
pub(crate) fn eval_list_first(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        members.first().copied()
    })
}

/// `list:rest` -- subject is a list, object binds to a new list containing
/// every member except the first.
pub(crate) fn eval_list_rest(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.is_empty() {
            return None;
        }
        Some(ids_to_list(&members[1..]))
    })
}

/// `list:last` -- subject is a list, object binds to its last member.
pub(crate) fn eval_list_last(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        members.last().copied()
    })
}

/// `list:member` -- subject is a (ground) list, object is a generated
/// member value. The mirror image of `list:in` (which generates the
/// *subject* from a ground object list).
pub(crate) fn eval_list_member(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    if !pattern.o.is_var() {
        return None;
    }
    let obj_var = pattern.o.to_encoded();
    if bindings.is_empty() {
        let members = subject_list_members(&pattern.s, bindings, 0)?;
        let mut result = Binding::new();
        for m in members {
            result.add(&obj_var, m);
        }
        return if !result.is_empty() {
            Some(result)
        } else {
            None
        };
    }
    let mut result = Binding::new();
    for row in 0..bindings.len() {
        if let Some(members) = subject_list_members(&pattern.s, bindings, row) {
            for m in members {
                copy_row(bindings, row, &mut result);
                result.add(&obj_var, m);
            }
        }
    }
    if !result.is_empty() {
        Some(result)
    } else {
        None
    }
}

/// `list:notMember` -- subject is a (ground) list, object is a value
/// already bound elsewhere; succeeds (row constraint) iff the value is NOT
/// among the list's members. The negated counterpart of `list:member`'s
/// generator, but used as a guard over an already-bound candidate rather
/// than to generate candidates.
pub(crate) fn eval_list_not_member(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    super::eval_row_constraint(pattern, bindings, |list_id, candidate_id| {
        match VarOrTerm::list_members(list_id) {
            Some(members) => !members.contains(&candidate_id),
            None => false,
        }
    })
}

/// `list:firstRest` -- object is a 2-element `(First Rest)` list; subject
/// binds to the list formed by prepending `First` onto the list `Rest`
/// (the "cons" direction real EYE rulesets use this builtin for, e.g.
/// prepending a newly-visited node onto a graph-search visited-list).
pub(crate) fn eval_list_first_rest(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    if !pattern.s.is_var() {
        return None;
    }
    let subj_var = pattern.s.to_encoded();
    let cons_for_row = |row: usize| -> Option<usize> {
        let pair = subject_list_members(&pattern.o, bindings, row)?;
        if pair.len() != 2 {
            return None;
        }
        let first = pair[0];
        let rest_members = VarOrTerm::list_members(pair[1])?;
        let mut combined = Vec::with_capacity(rest_members.len() + 1);
        combined.push(first);
        combined.extend(rest_members);
        Some(ids_to_list(&combined))
    };
    if bindings.is_empty() {
        let value = cons_for_row(0)?;
        let mut result = Binding::new();
        result.add(&subj_var, value);
        return Some(result);
    }
    let mut result = Binding::new();
    for row in 0..bindings.len() {
        if let Some(value) = cons_for_row(row) {
            copy_row(bindings, row, &mut result);
            result.add(&subj_var, value);
        }
    }
    if !result.is_empty() {
        Some(result)
    } else {
        None
    }
}

/// `list:memberAt` -- subject is `(list index)`, object binds to the
/// (0-based) member at `index`.
pub(crate) fn eval_list_member_at(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let operands = subject_list_members(&pattern.s, bindings, row)?;
        if operands.len() != 2 {
            return None;
        }
        let members = VarOrTerm::list_members(operands[0])?;
        let idx = super::numeric_value(operands[1])?;
        if idx < 0.0 || idx.fract() != 0.0 {
            return None;
        }
        members.get(idx as usize).copied()
    })
}

/// `list:remove` -- subject is `(list item)`, object binds to a new list
/// with every occurrence of `item` removed.
pub(crate) fn eval_list_remove(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let operands = subject_list_members(&pattern.s, bindings, row)?;
        if operands.len() != 2 {
            return None;
        }
        let members = VarOrTerm::list_members(operands[0])?;
        let target = operands[1];
        let filtered: Vec<usize> = members.into_iter().filter(|&m| m != target).collect();
        Some(ids_to_list(&filtered))
    })
}

/// `list:sort` -- subject is a list, object binds to a new list sorted
/// ascending (numerically where possible, falling back to lexical
/// comparison of the decoded literal text).
pub(crate) fn eval_list_sort(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let mut members = subject_list_members(&pattern.s, bindings, row)?;
        members.sort_by(
            |&a, &b| match (super::numeric_value(a), super::numeric_value(b)) {
                (Some(x), Some(y)) => x.partial_cmp(&y).unwrap_or(std::cmp::Ordering::Equal),
                _ => super::lexical_value(a)
                    .unwrap_or_default()
                    .cmp(&super::lexical_value(b).unwrap_or_default()),
            },
        );
        Some(ids_to_list(&members))
    })
}

/// `list:unique` -- subject is a list, object binds to a new list with
/// duplicate members removed, preserving first-occurrence order.
pub(crate) fn eval_list_unique(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        let mut seen = std::collections::HashSet::new();
        let deduped: Vec<usize> = members.into_iter().filter(|m| seen.insert(*m)).collect();
        Some(ids_to_list(&deduped))
    })
}

/// `list:reverse` -- subject is a list, object binds to a new list with
/// members in reverse order.
pub(crate) fn eval_list_reverse(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let mut members = subject_list_members(&pattern.s, bindings, row)?;
        members.reverse();
        Some(ids_to_list(&members))
    })
}

/// `list:iterate` -- subject is a (ground) list, object is generated as a
/// 2-element `(index item)` list for each member, 0-based.
pub(crate) fn eval_list_iterate(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    if !pattern.o.is_var() {
        return None;
    }
    let obj_var = pattern.o.to_encoded();
    let emit = |members: Vec<usize>, out: &mut Binding| {
        for (idx, m) in members.into_iter().enumerate() {
            let pair = ids_to_list(&[intern_number(idx as f64), m]);
            out.add(&obj_var, pair);
        }
    };
    if bindings.is_empty() {
        let list_id = resolve_operand(&pattern.s, bindings, 0)?;
        let members = VarOrTerm::list_members(list_id)?;
        let mut result = Binding::new();
        emit(members, &mut result);
        return if !result.is_empty() {
            Some(result)
        } else {
            None
        };
    }
    let mut result = Binding::new();
    for row in 0..bindings.len() {
        if let Some(list_id) = resolve_operand(&pattern.s, bindings, row) {
            if let Some(members) = VarOrTerm::list_members(list_id) {
                for (idx, m) in members.into_iter().enumerate() {
                    let pair = ids_to_list(&[intern_number(idx as f64), m]);
                    copy_row(bindings, row, &mut result);
                    result.add(&obj_var, pair);
                }
            }
        }
    }
    if !result.is_empty() {
        Some(result)
    } else {
        None
    }
}
