// Expression evaluation: term encoding, comparisons, boolean logic
use super::plan::PlanExpression;
use crate::tripleindex::EncodedBinding;
use crate::Encoder;
use std::cmp::Ordering;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum EncodedTerm {
    NamedNode { iri_id: usize },
    StringLiteral(String),
    IntegerLiteral(i64),
    BooleanLiteral(bool),
}

impl From<bool> for EncodedTerm {
    fn from(value: bool) -> Self {
        Self::BooleanLiteral(value)
    }
}

impl From<String> for EncodedTerm {
    fn from(value: String) -> Self {
        Self::StringLiteral(value)
    }
}

impl From<i64> for EncodedTerm {
    fn from(value: i64) -> Self {
        Self::IntegerLiteral(value)
    }
}

pub fn encode_term(term: EncodedTerm) -> usize {
    match term {
        EncodedTerm::BooleanLiteral(b) => Encoder::add_literal(
            b.to_string(),
            Some("<http://www.w3.org/2001/XMLSchema#boolean>".to_string()),
            None,
        ),
        EncodedTerm::IntegerLiteral(i) => Encoder::add_literal(
            i.to_string(),
            Some("<http://www.w3.org/2001/XMLSchema#integer>".to_string()),
            None,
        ),
        EncodedTerm::StringLiteral(s) => Encoder::add_literal(s, None, None),
        EncodedTerm::NamedNode { iri_id } => iri_id,
    }
}

pub fn eval_expression<'a>(
    expression: &'a PlanExpression,
) -> Box<dyn Fn(&Vec<EncodedBinding>) -> Option<EncodedTerm> + 'a> {
    match expression {
        PlanExpression::Greater(a, b) => partial_compare_helper(a, b, Ordering::Greater, None),
        PlanExpression::Less(a, b) => partial_compare_helper(a, b, Ordering::Less, None),
        PlanExpression::GreaterOrEqual(a, b) => {
            partial_compare_helper(a, b, Ordering::Greater, Some(Ordering::Equal))
        }
        PlanExpression::LessOrEqual(a, b) => {
            partial_compare_helper(a, b, Ordering::Less, Some(Ordering::Equal))
        }
        PlanExpression::Equal(a, b) => {
            let eval_a = eval_expression(a);
            let eval_b = eval_expression(b);
            Box::new(move |bindings| {
                let val_a = eval_a(bindings)?;
                let val_b = eval_b(bindings)?;
                Some(EncodedTerm::BooleanLiteral(val_a == val_b))
            })
        }
        PlanExpression::NotEqual(a, b) => {
            let eval_a = eval_expression(a);
            let eval_b = eval_expression(b);
            Box::new(move |bindings| {
                let val_a = eval_a(bindings)?;
                let val_b = eval_b(bindings)?;
                Some(EncodedTerm::BooleanLiteral(val_a != val_b))
            })
        }
        PlanExpression::And(a, b) => {
            let eval_a = eval_expression(a);
            let eval_b = eval_expression(b);
            Box::new(move |bindings| {
                let term_a = eval_a(bindings)?;
                let term_b = eval_b(bindings)?;
                let bool_a = to_bool(&term_a)?;
                let bool_b = to_bool(&term_b)?;
                Some(EncodedTerm::BooleanLiteral(bool_a && bool_b))
            })
        }
        PlanExpression::Or(a, b) => {
            let eval_a = eval_expression(a);
            let eval_b = eval_expression(b);
            Box::new(move |bindings| {
                let term_a = eval_a(bindings);
                let term_b = eval_b(bindings);
                let bool_a = term_a.and_then(|t| to_bool(&t));
                let bool_b = term_b.and_then(|t| to_bool(&t));
                match (bool_a, bool_b) {
                    (Some(true), _) | (_, Some(true)) => Some(EncodedTerm::BooleanLiteral(true)),
                    (Some(false), Some(false)) => Some(EncodedTerm::BooleanLiteral(false)),
                    _ => None,
                }
            })
        }
        PlanExpression::Not(a) => {
            let eval_a = eval_expression(a);
            Box::new(move |bindings| {
                let term_a = eval_a(bindings)?;
                let bool_a = to_bool(&term_a)?;
                Some(EncodedTerm::BooleanLiteral(!bool_a))
            })
        }
        PlanExpression::Variable(v) => Box::new(move |bindings| {
            let var_value: Vec<&EncodedBinding> = bindings.iter().filter(|b| b.var == *v).collect();
            var_value.first().and_then(|&binding| {
                if let Some(term) = Encoder::decode_to_term(binding.val) {
                    match term {
                        crate::Term::Iri(iri) => Some(EncodedTerm::NamedNode { iri_id: iri.iri }),
                        crate::Term::Literal(lit) => {
                            let dt = lit.datatype.and_then(|dt_id| Encoder::decode(&dt_id));
                            let val_str = Encoder::decode(&lit.value).unwrap_or_default();
                            if let Some(dt_str) = dt {
                                if dt_str == "<http://www.w3.org/2001/XMLSchema#integer>" {
                                    if let Ok(i) = val_str.parse::<i64>() {
                                        return Some(EncodedTerm::IntegerLiteral(i));
                                    }
                                } else if dt_str == "<http://www.w3.org/2001/XMLSchema#boolean>" {
                                    return Some(EncodedTerm::BooleanLiteral(
                                        val_str == "true" || val_str == "1",
                                    ));
                                }
                                // Disclosed, separate gap (not fixed here): xsd:decimal and
                                // other numeric xsd types fall through to StringLiteral below,
                                // same as any other untyped literal -- EncodedTerm (this file)
                                // has no decimal/float variant at all, only IntegerLiteral. A
                                // value the accumulators (sparql/accumulators.rs) now correctly
                                // tag xsd:decimal (SUM/MIN/MAX/AVG) still can't be numerically
                                // compared once it reaches here, so HAVING/ORDER BY/comparisons
                                // on those aggregates' results still silently fail past this
                                // point -- only xsd:integer (COUNT) round-trips correctly.
                            }
                            Some(EncodedTerm::StringLiteral(val_str))
                        }
                        crate::Term::BlankNode(bnode) => {
                            Some(EncodedTerm::StringLiteral(format!("_:{}", bnode.id)))
                        }
                    }
                } else {
                    Encoder::decode(&binding.val).map(EncodedTerm::StringLiteral)
                }
            })
        }),
        PlanExpression::Constant(t) => {
            let t = t.clone();
            let encoded_term = match &t {
                crate::Term::Literal(lit) => {
                    let dt = lit.datatype.and_then(|dt_id| Encoder::decode(&dt_id));
                    let val_str = Encoder::decode(&lit.value).unwrap_or_default();
                    if let Some(dt_str) = dt {
                        if dt_str == "<http://www.w3.org/2001/XMLSchema#integer>" {
                            if let Ok(i) = val_str.parse::<i64>() {
                                EncodedTerm::IntegerLiteral(i)
                            } else {
                                EncodedTerm::StringLiteral(val_str)
                            }
                        } else if dt_str == "<http://www.w3.org/2001/XMLSchema#boolean>" {
                            EncodedTerm::BooleanLiteral(val_str == "true" || val_str == "1")
                        } else {
                            EncodedTerm::StringLiteral(val_str)
                        }
                    } else {
                        EncodedTerm::StringLiteral(val_str)
                    }
                }
                crate::Term::Iri(iri) => EncodedTerm::NamedNode { iri_id: iri.iri },
                crate::Term::BlankNode(bnode) => {
                    EncodedTerm::StringLiteral(format!("_:{}", bnode.id))
                }
            };
            Box::new(move |_| Some(encoded_term.clone()))
        }
        _ => Box::new(|_| Some(EncodedTerm::BooleanLiteral(false))),
    }
}

fn partial_compare_helper<'a>(
    a: &'a Box<PlanExpression>,
    b: &'a Box<PlanExpression>,
    ordering: Ordering,
    second_order: Option<Ordering>,
) -> Box<dyn Fn(&Vec<EncodedBinding>) -> Option<EncodedTerm> + 'a> {
    let a = eval_expression(a);
    let b = eval_expression(b);

    Box::new(move |bindings| {
        let b_res = b(bindings);

        let r: Option<Ordering> = match a(bindings) {
            Some(EncodedTerm::IntegerLiteral(int_val_a)) => match b_res {
                Some(EncodedTerm::IntegerLiteral(int_val_b)) => int_val_a.partial_cmp(&int_val_b),
                _ => None,
            },
            Some(EncodedTerm::StringLiteral(str_val_a)) => match b(bindings) {
                Some(EncodedTerm::StringLiteral(str_val_b)) => str_val_a.partial_cmp(&str_val_b),
                _ => None,
            },
            _ => None,
        };
        if let Some(r) = r {
            if let Some(second_ordering) = second_order {
                if r == ordering || r == second_ordering {
                    Some(true.into())
                } else {
                    Some(false.into())
                }
            } else {
                Some((r == ordering).into())
            }
        } else {
            Some(false.into())
        }
    })
}

pub fn to_bool(term: &EncodedTerm) -> Option<bool> {
    match term {
        EncodedTerm::BooleanLiteral(value) => Some(*value),
        EncodedTerm::StringLiteral(value) => Some(!value.is_empty()),
        EncodedTerm::IntegerLiteral(value) => Some(*value != 0),
        _ => None,
    }
}
