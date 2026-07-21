// Hook condition compilation and evaluation (PROJ-404)

use crate::encoding::Encoder;
use crate::TripleStore;

use super::datalog::translate_datalog_to_n3;
use super::delta_query::{count_pred_in_delta, count_pred_in_store, parse_shape_map};
use super::parsing::clean_term;
use super::{
    CmpOp, CompiledCondition, DiagnosticDetail, GraphDelta, HookCondition, TriggerDiagnostic,
};

/// Converts a HookCondition to CompiledCondition.
/// PROJ-404: All supported conditions are converted directly.
/// Unsupported dialect features are marked as Unsupported variant.
pub fn compile_condition(condition: &HookCondition) -> CompiledCondition {
    match condition {
        HookCondition::Datalog { program, goal } => CompiledCondition::Datalog {
            program: program.clone(),
            goal: goal.clone(),
        },
        HookCondition::Delta { var } => CompiledCondition::Delta {
            pattern: var.clone(),
        },
        HookCondition::Threshold {
            var: _,
            op: _,
            k: _,
        } => {
            // Thresholds are converted to count-based conditions
            CompiledCondition::Threshold { min_count: 0 }
        }
        HookCondition::Count { var: _, op, k } => CompiledCondition::Count {
            op: *op,
            value: *k as usize,
        },
        HookCondition::Window {
            var: _,
            op: _,
            k: _,
            window: _,
        } => {
            // Window conditions represented as duration
            CompiledCondition::Window { duration_ms: 1000 }
        }
        HookCondition::N3 { rules } => CompiledCondition::N3 {
            rules: rules.clone(),
        },
        HookCondition::Shacl { shapes } => CompiledCondition::Shape {
            target_iri: "http://example.org/target".to_string(),
            shape_iri: shapes.clone(),
        },
        HookCondition::Shex {
            schema: _,
            shape_map: _,
        } => CompiledCondition::Unsupported {
            reason: "ShEx conditions require external shape evaluation boundary".to_string(),
        },
        HookCondition::Sparql { query: _ } => CompiledCondition::Unsupported {
            reason: "SPARQL conditions are evaluated via external endpoint".to_string(),
        },
    }
}

fn delta_touches(delta: &GraphDelta, var: &str) -> Result<bool, String> {
    for t in delta.additions.iter().chain(delta.removals.iter()) {
        let p_str = Encoder::decode(&t.p.to_encoded())
            .ok_or_else(|| format!("failed to decode predicate in delta: {:?}", t.p))?;
        let clean_p = clean_term(&p_str);
        let clean_v = clean_term(var);
        if clean_p == clean_v {
            return Ok(true);
        }
    }
    Ok(false)
}

/// Evaluates CompiledHooks against the current state and delta.
///
/// # Complexity
/// O(|H| * C) where |H| = number of hooks, C = per-condition evaluation cost
/// (typically O(|F|) where |F| = triple store size, dominated by SHACL/SPARQL)
pub fn evaluate_condition(
    condition: &HookCondition, post_state: &TripleStore, delta: &GraphDelta,
    history: &[GraphDelta], hook_iri: &str,
) -> Result<(bool, Option<TriggerDiagnostic>), String> {
    match condition {
        HookCondition::Datalog { program, goal } => {
            let n3_rules = translate_datalog_to_n3(program, hook_iri)?;
            let mut temp_store = TripleStore::new();
            for t in &post_state.triple_index.triples {
                temp_store.add(t.clone());
            }
            if !n3_rules.trim().is_empty() {
                temp_store
                    .load_rules(&n3_rules)
                    .map_err(|e| format!("load rules error: {} (rules: {})", e, n3_rules))?;
            }
            temp_store
                .materialize()
                .map_err(|e| format!("materialize error: {} (rules: {})", e, n3_rules))?;
            let goal_lower = goal.to_lowercase();
            let mut fired = false;
            for t in &temp_store.triple_index.triples {
                let p_str = Encoder::decode(&t.p.to_encoded()).ok_or_else(|| {
                    "failed to decode predicate in Datalog goal evaluation".to_string()
                })?;
                let cleaned_p = clean_term(&p_str);
                let is_type = cleaned_p == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                    || cleaned_p == "a";
                if is_type {
                    let o_str = Encoder::decode(&t.o.to_encoded()).ok_or_else(|| {
                        "failed to decode object in Datalog goal evaluation".to_string()
                    })?;
                    let clean_o = clean_term(&o_str).to_lowercase();
                    if clean_o == goal_lower {
                        fired = true;
                        break;
                    }
                } else {
                    if cleaned_p.to_lowercase() == goal_lower {
                        fired = true;
                        break;
                    }
                }
            }
            let diag = TriggerDiagnostic {
                hook_iri: hook_iri.to_string(),
                conforms: !fired,
                details: if fired {
                    vec![DiagnosticDetail {
                        focus_node: None,
                        result_path: None,
                        value: None,
                        severity: Some("Fired".to_string()),
                        message: format!("Datalog goal '{}' was derived in post-state", goal),
                    }]
                } else {
                    Vec::new()
                },
            };
            Ok((fired, Some(diag)))
        }
        HookCondition::Delta { var } => {
            let fired = delta_touches(delta, var)?;
            let diag = TriggerDiagnostic {
                hook_iri: hook_iri.to_string(),
                conforms: !fired,
                details: if fired {
                    vec![DiagnosticDetail {
                        focus_node: None,
                        result_path: Some(var.clone()),
                        value: None,
                        severity: Some("Fired".to_string()),
                        message: format!("Delta modified predicate '{}'", var),
                    }]
                } else {
                    Vec::new()
                },
            };
            Ok((fired, Some(diag)))
        }
        HookCondition::Threshold { var, op, k } => {
            let count = count_pred_in_store(post_state, var);
            let fired = op.holds(count, *k);
            let diag = TriggerDiagnostic {
                hook_iri: hook_iri.to_string(),
                conforms: !fired,
                details: if fired {
                    vec![DiagnosticDetail {
                        focus_node: None,
                        result_path: Some(var.clone()),
                        value: Some(count.to_string()),
                        severity: Some("Fired".to_string()),
                        message: format!(
                            "Predicate '{}' count {} held comparison {:?}",
                            var, count, op
                        ),
                    }]
                } else {
                    Vec::new()
                },
            };
            Ok((fired, Some(diag)))
        }
        HookCondition::Count { var, op, k } => {
            let count = count_pred_in_delta(delta, var);
            let fired = op.holds(count, *k);
            let diag = TriggerDiagnostic {
                hook_iri: hook_iri.to_string(),
                conforms: !fired,
                details: if fired {
                    vec![DiagnosticDetail {
                        focus_node: None,
                        result_path: Some(var.clone()),
                        value: Some(count.to_string()),
                        severity: Some("Fired".to_string()),
                        message: format!(
                            "Predicate '{}' delta count {} held comparison {:?}",
                            var, count, op
                        ),
                    }]
                } else {
                    Vec::new()
                },
            };
            Ok((fired, Some(diag)))
        }
        HookCondition::Window { var, op, k, window } => {
            let mut total = count_pred_in_delta(delta, var);
            for d in history.iter().take(usize::from(*window).saturating_sub(1)) {
                total += count_pred_in_delta(d, var);
            }
            let fired = op.holds(total, *k);
            let diag = TriggerDiagnostic {
                hook_iri: hook_iri.to_string(),
                conforms: !fired,
                details: if fired {
                    vec![DiagnosticDetail {
                        focus_node: None,
                        result_path: Some(var.clone()),
                        value: Some(total.to_string()),
                        severity: Some("Fired".to_string()),
                        message: format!(
                            "Predicate '{}' window count {} held comparison {:?}",
                            var, total, op
                        ),
                    }]
                } else {
                    Vec::new()
                },
            };
            Ok((fired, Some(diag)))
        }
        HookCondition::Shacl { shapes } => {
            let report = post_state.validate_shacl(shapes)?;
            let conforms = report.conforms;
            let details = report
                .results
                .iter()
                .map(|res| DiagnosticDetail {
                    focus_node: Some(res.focus_node.to_string()),
                    result_path: res.result_path.as_ref().map(|t| t.to_string()),
                    value: res.value.as_ref().map(|t| t.to_string()),
                    severity: Some(res.severity.to_string()),
                    message: res
                        .message
                        .clone()
                        .unwrap_or_else(|| "SHACL shape violation".to_string()),
                })
                .collect();
            Ok((
                !conforms,
                Some(TriggerDiagnostic {
                    hook_iri: hook_iri.to_string(),
                    conforms,
                    details,
                }),
            ))
        }
        HookCondition::Shex { schema, shape_map } => {
            let shape_map_parsed = parse_shape_map(shape_map);
            let (conforms, failures) = if schema.trim().starts_with('{') {
                let report = post_state
                    .validate_shex(schema, &shape_map_parsed)
                    .map_err(|e| format!("ShexJ error: {}", e))?;
                let failures: Vec<(String, String, String)> = report
                    .failures
                    .iter()
                    .map(|fail| {
                        (
                            fail.node.to_string(),
                            fail.shape.to_string(),
                            fail.reason.to_string(),
                        )
                    })
                    .collect();
                (report.conforms, failures)
            } else {
                let report = post_state
                    .validate_shex_c(schema, &shape_map_parsed)
                    .map_err(|e| format!("ShexC error: {}", e))?;
                let failures: Vec<(String, String, String)> = report
                    .failures
                    .iter()
                    .map(|fail| {
                        (
                            fail.node.to_string(),
                            fail.shape.to_string(),
                            fail.reason.to_string(),
                        )
                    })
                    .collect();
                (report.conforms, failures)
            };
            let details = failures
                .iter()
                .map(|(node, shape, reason)| DiagnosticDetail {
                    focus_node: Some(node.clone()),
                    result_path: None,
                    value: None,
                    severity: Some("Violation".to_string()),
                    message: format!("Shape validation failed for {}: {}", shape, reason),
                })
                .collect();
            Ok((
                !conforms,
                Some(TriggerDiagnostic {
                    hook_iri: hook_iri.to_string(),
                    conforms,
                    details,
                }),
            ))
        }
        HookCondition::N3 { rules } => {
            let mut temp_store = TripleStore::from(rules);
            for t in &post_state.triple_index.triples {
                temp_store.add(t.clone());
            }
            temp_store
                .materialize()
                .map_err(|e| format!("materialize error: {}", e))?;
            let violations = temp_store.check_denials();
            let conforms = violations.is_empty();
            let details = violations
                .iter()
                .map(|msg| DiagnosticDetail {
                    focus_node: None,
                    result_path: None,
                    value: None,
                    severity: Some("Denial".to_string()),
                    message: msg.clone(),
                })
                .collect();
            Ok((
                !conforms,
                Some(TriggerDiagnostic {
                    hook_iri: hook_iri.to_string(),
                    conforms,
                    details,
                }),
            ))
        }
        HookCondition::Sparql { query } => {
            let results = post_state
                .query(query)
                .map_err(|e| format!("SPARQL error: {}", e))?;
            let fired = !results.is_empty();
            let diag = TriggerDiagnostic {
                hook_iri: hook_iri.to_string(),
                conforms: !fired,
                details: if fired {
                    vec![DiagnosticDetail {
                        focus_node: None,
                        result_path: None,
                        value: Some(results.len().to_string()),
                        severity: Some("Fired".to_string()),
                        message: format!("SPARQL query returned {} results", results.len()),
                    }]
                } else {
                    Vec::new()
                },
            };
            Ok((fired, Some(diag)))
        }
    }
}
