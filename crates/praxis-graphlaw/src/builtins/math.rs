//! `math:` namespace builtins.

use super::{
    eval_functional, eval_row_constraint, intern_number, numeric_value, resolve_operand,
    subject_list_members,
};
use crate::{Binding, Triple};

pub(crate) const MATH_GREATER_THAN: &str = "<http://www.w3.org/2000/10/swap/math#greaterThan>";
pub(crate) const MATH_SUM: &str = "<http://www.w3.org/2000/10/swap/math#sum>";
pub(crate) const MATH_DIFFERENCE: &str = "<http://www.w3.org/2000/10/swap/math#difference>";
pub(crate) const MATH_PRODUCT: &str = "<http://www.w3.org/2000/10/swap/math#product>";
pub(crate) const MATH_QUOTIENT: &str = "<http://www.w3.org/2000/10/swap/math#quotient>";
pub(crate) const MATH_REMAINDER: &str = "<http://www.w3.org/2000/10/swap/math#remainder>";
pub(crate) const MATH_NOT_LESS_THAN: &str = "<http://www.w3.org/2000/10/swap/math#notLessThan>";
pub(crate) const MATH_NOT_GREATER_THAN: &str =
    "<http://www.w3.org/2000/10/swap/math#notGreaterThan>";
pub(crate) const MATH_LESS_THAN: &str = "<http://www.w3.org/2000/10/swap/math#lessThan>";
pub(crate) const MATH_EQUAL_TO: &str = "<http://www.w3.org/2000/10/swap/math#equalTo>";
pub(crate) const MATH_NOT_EQUAL_TO: &str = "<http://www.w3.org/2000/10/swap/math#notEqualTo>";
pub(crate) const MATH_ABSOLUTE_VALUE: &str = "<http://www.w3.org/2000/10/swap/math#absoluteValue>";
pub(crate) const MATH_NEGATION: &str = "<http://www.w3.org/2000/10/swap/math#negation>";
pub(crate) const MATH_ROUNDED: &str = "<http://www.w3.org/2000/10/swap/math#rounded>";
pub(crate) const MATH_CEILING: &str = "<http://www.w3.org/2000/10/swap/math#ceiling>";
pub(crate) const MATH_FLOOR: &str = "<http://www.w3.org/2000/10/swap/math#floor>";
pub(crate) const MATH_EXPONENTIATION: &str = "<http://www.w3.org/2000/10/swap/math#exponentiation>";
pub(crate) const MATH_INTEGER_QUOTIENT: &str =
    "<http://www.w3.org/2000/10/swap/math#integerQuotient>";
pub(crate) const MATH_MIN: &str = "<http://www.w3.org/2000/10/swap/math#min>";
pub(crate) const MATH_MAX: &str = "<http://www.w3.org/2000/10/swap/math#max>";
pub(crate) const MATH_SIN: &str = "<http://www.w3.org/2000/10/swap/math#sin>";
pub(crate) const MATH_COS: &str = "<http://www.w3.org/2000/10/swap/math#cos>";
pub(crate) const MATH_TAN: &str = "<http://www.w3.org/2000/10/swap/math#tan>";
pub(crate) const MATH_ASIN: &str = "<http://www.w3.org/2000/10/swap/math#asin>";
pub(crate) const MATH_ACOS: &str = "<http://www.w3.org/2000/10/swap/math#acos>";
pub(crate) const MATH_ATAN: &str = "<http://www.w3.org/2000/10/swap/math#atan>";
pub(crate) const MATH_ATAN2: &str = "<http://www.w3.org/2000/10/swap/math#atan2>";
pub(crate) const MATH_LOGARITHM: &str = "<http://www.w3.org/2000/10/swap/math#logarithm>";
pub(crate) const MATH_MEMBER_COUNT: &str = "<http://www.w3.org/2000/10/swap/math#memberCount>";

pub(crate) fn eval_greater_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a > b),
    )
}

pub(crate) fn eval_not_less_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a >= b),
    )
}

pub(crate) fn eval_not_greater_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a <= b),
    )
}

pub(crate) fn eval_less_than(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a < b),
    )
}

pub(crate) fn eval_math_equal_to(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a == b),
    )
}

pub(crate) fn eval_math_not_equal_to(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_row_constraint(
        pattern,
        bindings,
        |s, o| matches!((numeric_value(s), numeric_value(o)), (Some(a), Some(b)) if a != b),
    )
}

pub(crate) fn eval_sum(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        // math:sum is n-ary per the N3 built-ins spec ("the sum of
        // the elements of the list"), not fixed at 2 operands -- e.g.
        // EYE's `dog` corpus case sums an arbitrarily-long collected
        // list. Fold over however many members the list has (an
        // empty list sums to 0, the additive identity).
        let members = subject_list_members(&pattern.s, bindings, row)?;
        let mut total = 0.0;
        for m in &members {
            total += numeric_value(*m)?;
        }
        Some(intern_number(total))
    })
}

pub(crate) fn eval_difference(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let a = numeric_value(members[0])?;
        let b = numeric_value(members[1])?;
        Some(intern_number(a - b))
    })
}

pub(crate) fn eval_product(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        // n-ary, like math:sum above.
        let members = subject_list_members(&pattern.s, bindings, row)?;
        let mut total = 1.0;
        for m in &members {
            total *= numeric_value(*m)?;
        }
        Some(intern_number(total))
    })
}

pub(crate) fn eval_quotient(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let a = numeric_value(members[0])?;
        let b = numeric_value(members[1])?;
        if b == 0.0 {
            return None;
        }
        Some(intern_number(a / b))
    })
}

pub(crate) fn eval_remainder(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let a = numeric_value(members[0])?;
        let b = numeric_value(members[1])?;
        if b == 0.0 {
            return None;
        }
        Some(intern_number(a % b))
    })
}

/// Resolve the (non-list) numeric subject of a unary `math:` function for
/// the given row.
fn subject_number(pattern: &Triple, bindings: &Binding, row: usize) -> Option<f64> {
    let id = resolve_operand(&pattern.s, bindings, row)?;
    numeric_value(id)
}

fn eval_unary(pattern: &Triple, bindings: &Binding, f: impl Fn(f64) -> f64) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let a = subject_number(pattern, bindings, row)?;
        Some(intern_number(f(a)))
    })
}

pub(crate) fn eval_absolute_value(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::abs)
}

pub(crate) fn eval_negation(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, |a| -a)
}

pub(crate) fn eval_rounded(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::round)
}

pub(crate) fn eval_ceiling(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::ceil)
}

pub(crate) fn eval_floor(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::floor)
}

pub(crate) fn eval_sin(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::sin)
}

pub(crate) fn eval_cos(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::cos)
}

pub(crate) fn eval_tan(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::tan)
}

pub(crate) fn eval_asin(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::asin)
}

pub(crate) fn eval_acos(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::acos)
}

pub(crate) fn eval_atan(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_unary(pattern, bindings, f64::atan)
}

pub(crate) fn eval_exponentiation(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let base = numeric_value(members[0])?;
        let exp = numeric_value(members[1])?;
        Some(intern_number(base.powf(exp)))
    })
}

pub(crate) fn eval_integer_quotient(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let a = numeric_value(members[0])?;
        let b = numeric_value(members[1])?;
        if b == 0.0 {
            return None;
        }
        Some(intern_number((a / b).trunc()))
    })
}

pub(crate) fn eval_min(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.is_empty() {
            return None;
        }
        let mut best: Option<f64> = None;
        for m in &members {
            let v = numeric_value(*m)?;
            best = Some(match best {
                Some(b) if b <= v => b,
                _ => v,
            });
        }
        Some(intern_number(best?))
    })
}

pub(crate) fn eval_max(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.is_empty() {
            return None;
        }
        let mut best: Option<f64> = None;
        for m in &members {
            let v = numeric_value(*m)?;
            best = Some(match best {
                Some(b) if b >= v => b,
                _ => v,
            });
        }
        Some(intern_number(best?))
    })
}

pub(crate) fn eval_atan2(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let y = numeric_value(members[0])?;
        let x = numeric_value(members[1])?;
        Some(intern_number(y.atan2(x)))
    })
}

pub(crate) fn eval_logarithm(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        if members.len() != 2 {
            return None;
        }
        let value = numeric_value(members[0])?;
        let base = numeric_value(members[1])?;
        Some(intern_number(value.log(base)))
    })
}

pub(crate) fn eval_member_count(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let members = subject_list_members(&pattern.s, bindings, row)?;
        Some(intern_number(members.len() as f64))
    })
}
