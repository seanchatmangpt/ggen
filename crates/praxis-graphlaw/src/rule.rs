use crate::fastmap::FxHashSet;
use crate::term::{Triple, VarOrTerm};
use crate::Encoder;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct BodyLiteral {
    pub negated: bool,
    pub pattern: Triple,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum AggregateFunction {
    Count,
    Sum,
    Min,
    Max,
    Avg,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Aggregate {
    pub function: AggregateFunction,
    pub source_var: String,
    pub target_var: String,
    pub group_vars: Vec<String>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Rule {
    pub body: Vec<BodyLiteral>,
    pub head: Triple,
}

/// Sentinel head predicate marking a denial/consistency-check rule (`{ body
/// } => false.`, e.g. SKOS's disjointness constraints) -- a real N3/RIF
/// idiom for integrity constraints rather than fact derivation, distinct
/// from ordinary rule assertion. Reusing `Rule.head: Triple` with a
/// dedicated marker predicate (rather than adding a new field/enum to
/// `Rule` itself) avoids changing every one of the ~18 existing call sites
/// across the workspace that construct a `Rule` literal for the ordinary
/// assertion case -- the same predicate-string-dispatch technique this
/// engine already uses for `log:implies`/`log:notIncludes`/
/// `log:collectAllIn` (see `reasoner.rs`'s `find_log_*_literal` helpers).
pub const DENIAL_HEAD_MARKER: &str = "<http://www.w3.org/2000/10/swap/log#__denial__>";

impl Rule {
    /// Construct a denial rule's `Rule` value for the parser: `body` with a
    /// head that's always the reserved `DENIAL_HEAD_MARKER` sentinel triple
    /// (its s/o carry no meaning and are never inspected).
    pub fn new_denial(body: Vec<BodyLiteral>) -> Rule {
        let marker = VarOrTerm::new_term(DENIAL_HEAD_MARKER.to_string());
        Rule {
            body,
            head: Triple {
                s: marker.clone(),
                p: marker.clone(),
                o: marker,
                g: None,
            },
        }
    }

    /// Whether this rule is a denial/consistency-check rule (`=> false.`)
    /// rather than an ordinary fact-asserting rule.
    pub fn is_denial(&self) -> bool {
        self.head.p.is_term()
            && Encoder::decode(&self.head.p.to_encoded()).as_deref() == Some(DENIAL_HEAD_MARKER)
    }
}

/// Selectivity classification for a rule body pattern.
/// Patterns are ordered from most selective (exact fact lookup) to least selective
/// (full Cartesian scan).
///
/// # Complexity
/// Classification is O(1) per pattern; ordering is O(n log n) where n is the number of body patterns.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Selectivity {
    /// All variables in pattern are bound (exact fact lookup).
    Exact { bound_vars: usize },
    /// Predicate and object are bound, subject unbound (1 degree of freedom).
    PredicateObject { s_unbound: bool },
    /// Subject and predicate are bound, object unbound (1 degree of freedom).
    SubjectPredicate { o_unbound: bool },
    /// Predicate only is bound (2 degrees of freedom).
    PredicateOnly { so_unbound: bool },
    /// One variable is bound somewhere in the pattern (2+ degrees of freedom).
    OneUnbound,
    /// Zero variables bound; full Cartesian scan (3 degrees of freedom).
    FullScan,
}

impl Selectivity {
    /// Returns a numeric score for sorting (lower = more selective = should evaluate first).
    /// Used as a tie-breaker.
    pub fn score(&self) -> u8 {
        match self {
            Selectivity::Exact { .. } => 0,
            Selectivity::PredicateObject { .. } => 1,
            Selectivity::SubjectPredicate { .. } => 2,
            Selectivity::PredicateOnly { .. } => 3,
            Selectivity::OneUnbound => 4,
            Selectivity::FullScan => 5,
        }
    }
}

/// A single step in an ordered rule evaluation.
/// Represents a body pattern reordered by selectivity heuristic,
/// with variables bound by this pattern tracked for subsequent steps.
#[derive(Debug, Clone)]
pub struct PatternStep {
    /// The body pattern (triple template).
    pub pattern: Triple,
    /// Whether this literal is negated.
    pub negated: bool,
    /// Selectivity classification of this pattern given already-bound variables.
    pub selectivity: Selectivity,
    /// Variables newly bound by this pattern.
    pub new_vars: FxHashSet<usize>,
}

/// A rule compiled into an ordered representation.
/// Rules are compiled at load time: body patterns are reordered by selectivity
/// heuristic (exact lookups first, full scans last) to reduce join cardinality.
///
/// # Complexity
/// Compilation is O(n^2) where n is the number of body patterns (classify selectivity
/// of each pattern for each binding state). This is done once at load time.
#[derive(Debug, Clone)]
pub struct CompiledRule {
    /// Reference to the original rule (used for error messages and debugging).
    pub original_rule: Rc<Rule>,
    /// Head triple of the rule.
    pub head: Triple,
    /// Ordered body patterns (by selectivity heuristic).
    pub body: Vec<PatternStep>,
    /// Index of the first body pattern to evaluate (driving atom).
    pub driving_atom: usize,
}

/// Extracts variables from a triple pattern.
/// Returns a set of encoded variable IDs.
fn extract_pattern_vars(pattern: &Triple) -> FxHashSet<usize> {
    let mut vars = FxHashSet::default();
    if pattern.s.is_var() {
        vars.insert(pattern.s.to_encoded());
    }
    if pattern.p.is_var() {
        vars.insert(pattern.p.to_encoded());
    }
    if pattern.o.is_var() {
        vars.insert(pattern.o.to_encoded());
    }
    if let Some(ref g) = pattern.g {
        if g.is_var() {
            vars.insert(g.to_encoded());
        }
    }
    vars
}

/// Classify the selectivity of a body pattern given already-bound variables.
///
/// # Complexity
/// O(1) — checks if three positions (s, p, o) are bound or unbound.
pub fn classify_pattern_selectivity(
    pattern: &Triple, bound_vars: &FxHashSet<usize>,
) -> Selectivity {
    let s_bound =
        pattern.s.is_term() || (pattern.s.is_var() && bound_vars.contains(&pattern.s.to_encoded()));
    let p_bound =
        pattern.p.is_term() || (pattern.p.is_var() && bound_vars.contains(&pattern.p.to_encoded()));
    let o_bound =
        pattern.o.is_term() || (pattern.o.is_var() && bound_vars.contains(&pattern.o.to_encoded()));

    let bound_count = (s_bound as usize) + (p_bound as usize) + (o_bound as usize);

    match bound_count {
        3 => Selectivity::Exact { bound_vars: 3 },
        2 => {
            if p_bound && o_bound {
                Selectivity::PredicateObject {
                    s_unbound: !s_bound,
                }
            } else if s_bound && p_bound {
                Selectivity::SubjectPredicate {
                    o_unbound: !o_bound,
                }
            } else {
                // s_bound && o_bound, p_unbound
                Selectivity::PredicateOnly { so_unbound: true }
            }
        }
        1 => Selectivity::OneUnbound,
        _ => Selectivity::FullScan,
    }
}

/// Reorder rule body patterns by selectivity heuristic.
///
/// Evaluates patterns from most selective (exact fact lookups) to least selective
/// (full Cartesian scans), updating bound variables after each pattern.
/// This reduces join cardinality and improves performance.
///
/// # Complexity
/// O(n^2) where n is the number of body patterns.
/// - For each pattern, classify its selectivity: O(1)
/// - Sort by selectivity: O(n log n)
/// - Recompute selectivity with updated bindings: O(n)
/// Overall: O(n^2) due to nested iteration over patterns as bindings grow.
///
/// # Algorithm
/// 1. Start with variables bound in the rule head (if any reappear in the body).
/// 2. For each remaining unordered pattern:
///    a. Classify its selectivity given currently-bound variables.
///    b. Select the pattern with the highest selectivity (most selective first).
///    c. Add that pattern to the ordered result.
///    d. Update bound_vars with variables newly bound by this pattern.
/// 3. Repeat until all patterns are ordered.
pub fn order_body_patterns(rule: &Rule) -> Result<Vec<PatternStep>, String> {
    if rule.body.is_empty() {
        return Ok(Vec::new());
    }

    // Extract variables bound in the head (for head-to-body unification).
    let mut bound_vars: FxHashSet<usize> = FxHashSet::default();
    bound_vars.extend(extract_pattern_vars(&rule.head));

    let mut ordered = Vec::new();
    let mut remaining: Vec<(usize, &BodyLiteral)> = rule.body.iter().enumerate().collect();

    while !remaining.is_empty() {
        // Find the pattern with the best (lowest score = most selective) selectivity.
        let mut best_idx = 0;
        let mut best_selectivity =
            classify_pattern_selectivity(&remaining[0].1.pattern, &bound_vars);

        for (i, (_, body_lit)) in remaining.iter().enumerate().skip(1) {
            let selectivity = classify_pattern_selectivity(&body_lit.pattern, &bound_vars);
            // Selectivity uses Ord (score-based), so we select the minimum.
            if selectivity < best_selectivity {
                best_idx = i;
                best_selectivity = selectivity;
            }
        }

        let (_original_idx, body_lit) = remaining.remove(best_idx);
        let new_vars: FxHashSet<usize> = extract_pattern_vars(&body_lit.pattern)
            .into_iter()
            .filter(|v| !bound_vars.contains(v))
            .collect();

        bound_vars.extend(new_vars.iter().cloned());

        ordered.push(PatternStep {
            pattern: body_lit.pattern.clone(),
            negated: body_lit.negated,
            selectivity: best_selectivity,
            new_vars,
        });
    }

    Ok(ordered)
}

impl CompiledRule {
    /// Compile a rule: reorder body patterns by selectivity heuristic.
    pub fn compile(rule: Rule) -> Result<CompiledRule, String> {
        let head = rule.head.clone();
        let body = order_body_patterns(&rule)?;
        let driving_atom = 0; // First pattern in ordered sequence.
        Ok(CompiledRule {
            original_rule: Rc::new(rule),
            head,
            body,
            driving_atom,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_selectivity_classification_exact() {
        // All positions bound: exact lookup
        let pattern = Triple {
            s: VarOrTerm::new_term("http://example.org/subj".to_string()),
            p: VarOrTerm::new_term("http://example.org/pred".to_string()),
            o: VarOrTerm::new_term("http://example.org/obj".to_string()),
            g: None,
        };
        let bound_vars = FxHashSet::default();
        let selectivity = classify_pattern_selectivity(&pattern, &bound_vars);
        assert!(matches!(selectivity, Selectivity::Exact { .. }));
    }

    #[test]
    fn test_selectivity_classification_pred_obj() {
        // Predicate and object bound, subject unbound
        let pattern = Triple {
            s: VarOrTerm::new_var("?s".to_string()),
            p: VarOrTerm::new_term("http://example.org/pred".to_string()),
            o: VarOrTerm::new_term("http://example.org/obj".to_string()),
            g: None,
        };
        let bound_vars = FxHashSet::default();
        let selectivity = classify_pattern_selectivity(&pattern, &bound_vars);
        assert!(matches!(selectivity, Selectivity::PredicateObject { .. }));
    }

    #[test]
    fn test_selectivity_classification_full_scan() {
        // No positions bound: full Cartesian scan
        let pattern = Triple {
            s: VarOrTerm::new_var("?s".to_string()),
            p: VarOrTerm::new_var("?p".to_string()),
            o: VarOrTerm::new_var("?o".to_string()),
            g: None,
        };
        let bound_vars = FxHashSet::default();
        let selectivity = classify_pattern_selectivity(&pattern, &bound_vars);
        assert_eq!(selectivity, Selectivity::FullScan);
    }

    #[test]
    fn test_selectivity_ordering() {
        // Test that Exact < PredicateObject < FullScan
        let exact = Selectivity::Exact { bound_vars: 3 };
        let pred_obj = Selectivity::PredicateObject { s_unbound: true };
        let full_scan = Selectivity::FullScan;

        assert!(exact < pred_obj);
        assert!(pred_obj < full_scan);
    }

    #[test]
    fn test_extract_pattern_vars_simple() {
        let pattern = Triple {
            s: VarOrTerm::new_var("?s".to_string()),
            p: VarOrTerm::new_term("http://example.org/pred".to_string()),
            o: VarOrTerm::new_var("?o".to_string()),
            g: None,
        };
        let vars = extract_pattern_vars(&pattern);
        assert_eq!(vars.len(), 2); // Should find ?s and ?o
    }

    #[test]
    fn test_order_body_patterns_simple() {
        // Simple rule: (?s ?p ?o) with exact match first, then partial
        let rule = Rule {
            head: Triple {
                s: VarOrTerm::new_var("?s".to_string()),
                p: VarOrTerm::new_term("http://example.org/type".to_string()),
                o: VarOrTerm::new_var("?type".to_string()),
                g: None,
            },
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple {
                        s: VarOrTerm::new_var("?s".to_string()),
                        p: VarOrTerm::new_var("?p".to_string()),
                        o: VarOrTerm::new_var("?o".to_string()),
                        g: None,
                    },
                },
                BodyLiteral {
                    negated: false,
                    pattern: Triple {
                        s: VarOrTerm::new_var("?o".to_string()),
                        p: VarOrTerm::new_term("http://example.org/type".to_string()),
                        o: VarOrTerm::new_var("?type".to_string()),
                        g: None,
                    },
                },
            ],
        };

        let result = order_body_patterns(&rule);
        assert!(result.is_ok());
        let ordered = result.unwrap();
        assert_eq!(ordered.len(), 2);

        // The second pattern (bound o to ?o) should be more selective initially,
        // but we start from scratch so both should be evaluated
        // After first pattern, second pattern becomes more selective
    }

    #[test]
    fn test_compiled_rule_compile() {
        let rule = Rule {
            head: Triple {
                s: VarOrTerm::new_var("?x".to_string()),
                p: VarOrTerm::new_term("http://example.org/prop".to_string()),
                o: VarOrTerm::new_var("?y".to_string()),
                g: None,
            },
            body: vec![BodyLiteral {
                negated: false,
                pattern: Triple {
                    s: VarOrTerm::new_var("?x".to_string()),
                    p: VarOrTerm::new_var("?p".to_string()),
                    o: VarOrTerm::new_var("?y".to_string()),
                    g: None,
                },
            }],
        };

        let result = CompiledRule::compile(rule);
        assert!(result.is_ok());
        let compiled = result.unwrap();
        assert_eq!(compiled.body.len(), 1);
        assert_eq!(compiled.driving_atom, 0);
    }
}
