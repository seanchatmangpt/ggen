// Plan extraction: query pattern analysis, expression building, aggregation setup
use crate::{Encoder, Term, Triple};
use spargebra::algebra::*;
use spargebra::term::Variable;
use std::fmt::Error;
use std::rc::Rc;

fn extract_triples(
    triple_patterns: &Vec<spargebra::term::TriplePattern>,
    _encoder: &mut Encoder,
) -> Vec<Triple> {
    let mut triples = Vec::new();
    for spargebra::term::TriplePattern {
        subject: s,
        predicate: p,
        object: o,
    } in triple_patterns
    {
        triples.push(Triple::from(s.to_string(), p.to_string(), o.to_string()));
    }
    triples
}

pub fn strip_variable_prefix(var: &str) -> &str {
    var.strip_prefix('?').unwrap_or(var)
}

#[derive(Debug)]
pub enum PlanExpression {
    Constant(Term),
    Variable(usize),
    Greater(Box<Self>, Box<Self>),
    GreaterOrEqual(Box<Self>, Box<Self>),
    Less(Box<Self>, Box<Self>),
    LessOrEqual(Box<Self>, Box<Self>),
    Equal(Box<Self>, Box<Self>),
    NotEqual(Box<Self>, Box<Self>),
    And(Box<Self>, Box<Self>),
    Or(Box<Self>, Box<Self>),
    Not(Box<Self>),
    Done,
}

#[derive(Debug)]
pub enum PlanNode {
    Join {
        left: Box<Self>,
        right: Box<Self>,
    },
    QuadPattern {
        pattern: Triple,
    },
    Project {
        child: Box<Self>,
        mapping: Vec<usize>,
    },
    Filter {
        child: Box<Self>,
        expression: Box<PlanExpression>,
    },
    Aggregate {
        child: Box<Self>,
        keys: Vec<Variable>,
        aggregates: Rc<Vec<(PlanAggregation, Variable)>>,
    },
    Extend {
        child: Box<Self>,
        expression: PlanExpression,
        to: Variable,
    },
    LeftJoin {
        left: Box<Self>,
        right: Box<Self>,
        expression: Option<PlanExpression>,
    },
    Union {
        left: Box<Self>,
        right: Box<Self>,
    },
    Minus {
        left: Box<Self>,
        right: Box<Self>,
    },
    Done,
    Unit,
}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct PlanAggregation {
    pub function: PlanAggregationFunction,
    pub distinct: bool,
    pub variable: Option<Variable>,
}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum PlanAggregationFunction {
    Count,
    Sum,
    Min,
    Max,
    Avg,
}

fn new_join(left: PlanNode, right: PlanNode) -> PlanNode {
    PlanNode::Join {
        left: Box::new(left),
        right: Box::new(right),
    }
}

pub fn extract_query_plan(graph_pattern: &GraphPattern) -> PlanNode {
    match graph_pattern {
        GraphPattern::Bgp { patterns } => patterns
            .iter()
            .map(|t| PlanNode::QuadPattern {
                pattern: Triple::from(
                    t.subject.to_string(),
                    t.predicate.to_string(),
                    t.object.to_string(),
                ),
            })
            .reduce(new_join)
            .unwrap_or(PlanNode::Unit),
        GraphPattern::Join { left, right } => PlanNode::Join {
            left: Box::new(extract_query_plan(left)),
            right: Box::new(extract_query_plan(right)),
        },
        GraphPattern::Distinct { inner } | GraphPattern::Reduced { inner } => {
            extract_query_plan(inner)
        }
        GraphPattern::Project { inner, variables } => {
            let new_vars = variables
                .iter()
                .map(|v| {
                    let var_str = v.as_str().to_string();
                    let var_str = strip_variable_prefix(&var_str).to_string();
                    Encoder::add(var_str)
                })
                .collect();
            PlanNode::Project {
                child: Box::new(extract_query_plan(inner)),
                mapping: new_vars,
            }
        }
        GraphPattern::Filter { expr, inner } => PlanNode::Filter {
            child: Box::new(extract_query_plan(inner)),
            expression: Box::new(extract_expression(expr)),
        },
        GraphPattern::Group {
            inner,
            variables: by,
            aggregates,
        } => {
            let inner_variables = by.clone();

            PlanNode::Aggregate {
                child: Box::new(extract_query_plan(inner)),
                keys: inner_variables.clone(),
                aggregates: Rc::new(
                    aggregates
                        .iter()
                        .map(|(v, a)| {
                            Ok((
                                build_for_aggregate(a, &mut inner_variables.clone()).unwrap(),
                                v.clone(),
                            ))
                        })
                        .collect::<Result<Vec<_>, Error>>()
                        .unwrap(),
                ),
            }
        }
        GraphPattern::Extend {
            inner,
            expression,
            variable,
        } => {
            let to_str = variable.clone().into_string();
            let to_str = strip_variable_prefix(&to_str).to_string();
            Encoder::add(to_str);
            PlanNode::Extend {
                child: Box::new(extract_query_plan(inner)),
                expression: extract_expression(expression),
                to: variable.clone(),
            }
        }
        GraphPattern::LeftJoin {
            left,
            right,
            expression,
        } => PlanNode::LeftJoin {
            left: Box::new(extract_query_plan(left)),
            right: Box::new(extract_query_plan(right)),
            expression: expression.as_ref().map(extract_expression),
        },
        GraphPattern::Union { left, right } => PlanNode::Union {
            left: Box::new(extract_query_plan(left)),
            right: Box::new(extract_query_plan(right)),
        },
        GraphPattern::Minus { left, right } => PlanNode::Minus {
            left: Box::new(extract_query_plan(left)),
            right: Box::new(extract_query_plan(right)),
        },
        _ => PlanNode::Done,
    }
}

pub fn build_for_aggregate(
    aggregate: &AggregateExpression,
    _variables: &mut Vec<Variable>,
) -> Result<PlanAggregation, String> {
    match aggregate {
        AggregateExpression::CountSolutions { distinct } => Ok(PlanAggregation {
            function: PlanAggregationFunction::Count,
            distinct: *distinct,
            variable: None,
        }),
        AggregateExpression::FunctionCall {
            name,
            expr,
            distinct,
        } => {
            let function = match name {
                AggregateFunction::Count => PlanAggregationFunction::Count,
                AggregateFunction::Sum => PlanAggregationFunction::Sum,
                AggregateFunction::Min => PlanAggregationFunction::Min,
                AggregateFunction::Max => PlanAggregationFunction::Max,
                AggregateFunction::Avg => PlanAggregationFunction::Avg,
                _ => return Err("Failed".to_string()),
            };
            let var = match expr {
                Expression::Variable(v) => Some(v.clone()),
                _ => None,
            };
            Ok(PlanAggregation {
                function,
                distinct: *distinct,
                variable: var,
            })
        }
    }
}

pub fn extract_expression(expression: &Expression) -> PlanExpression {
    match expression {
        Expression::Greater(a, b) => PlanExpression::Greater(
            Box::new(extract_expression(a)),
            Box::new(extract_expression(b)),
        ),
        Expression::GreaterOrEqual(a, b) => PlanExpression::GreaterOrEqual(
            Box::new(extract_expression(a)),
            Box::new(extract_expression(b)),
        ),
        Expression::Less(a, b) => PlanExpression::Less(
            Box::new(extract_expression(a)),
            Box::new(extract_expression(b)),
        ),
        Expression::LessOrEqual(a, b) => PlanExpression::LessOrEqual(
            Box::new(extract_expression(a)),
            Box::new(extract_expression(b)),
        ),
        Expression::Equal(a, b) => PlanExpression::Equal(
            Box::new(extract_expression(a)),
            Box::new(extract_expression(b)),
        ),

        Expression::And(a, b) => PlanExpression::And(
            Box::new(extract_expression(a)),
            Box::new(extract_expression(b)),
        ),
        Expression::Or(a, b) => PlanExpression::Or(
            Box::new(extract_expression(a)),
            Box::new(extract_expression(b)),
        ),
        Expression::Not(a) => PlanExpression::Not(Box::new(extract_expression(a))),
        Expression::Variable(var) => {
            let var_str = var.as_str().to_string();
            let var_str = strip_variable_prefix(&var_str).to_string();
            PlanExpression::Variable(Encoder::add(var_str))
        }
        Expression::Literal(value) => {
            let val = value.value().to_string();
            let datatype = format!("<{}>", value.datatype().as_str());
            let lang = value.language().map(|l| l.to_string());
            let id = Encoder::add_literal(val, Some(datatype), lang);
            let term = Encoder::decode_to_term(id).unwrap();
            PlanExpression::Constant(term)
        }
        Expression::NamedNode(iri) => {
            let id = Encoder::add(format!("<{}>", iri.as_str()));
            let term = Encoder::decode_to_term(id).unwrap();
            PlanExpression::Constant(term)
        }
        _ => PlanExpression::Done,
    }
}
