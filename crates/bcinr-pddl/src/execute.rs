//! BRCE execution loop: Prolog8 admission per tape op → OCEL → BLAKE3 receipt.

use crate::error::Pddl8Error;
use std::collections::{BTreeSet, HashMap};
use wasm4pm_compat::ocel::{
    OCELAttributeValue, OCELEvent, OCELEventAttribute, OCELObject, OCELRelationship, OCELType,
    OCELTypeAttribute, OCEL,
};
use wasm4pm_compat::pddl::{
    Pddl8Domain, Pddl8ExecutionLog, Pddl8ExecutionReceipt, Pddl8GroundAtom, Pddl8Problem,
    Pddl8StepResult, Pddl8Tape, PddlEffect, TemporalExecutionReceipt, TemporalPlan,
};

use prolog8::{
    Atom8 as P8Atom, Catalog, CatalogId, EpochId, FactBlock8, FactRow8, FeatureBit, Kernel,
    PredicateId, PredicateMeta, PredicateProofPolicy, ProofMode, QueryAtom8, QueryResult, Rule8,
    RuleId, SourceId, TermId,
};

const SRC: SourceId = SourceId(0);

struct Ctx {
    kernel: Kernel,
    pred_ids: HashMap<String, PredicateId>,
    term_ids: HashMap<String, TermId>,
    next_pred: u32,
    next_rule: u32,
    epoch: u64,
}

impl Ctx {
    fn new() -> Self {
        Self {
            kernel: Kernel::new(Catalog::new(CatalogId(1))),
            pred_ids: HashMap::new(),
            term_ids: HashMap::new(),
            next_pred: 1,
            next_rule: 1,
            epoch: 0,
        }
    }

    fn pred(&mut self, name: &str, arity: u8) -> PredicateId {
        let key = format!("{name}/{arity}");
        if let Some(&id) = self.pred_ids.get(&key) {
            return id;
        }
        let id = PredicateId(self.next_pred);
        self.next_pred += 1;
        self.kernel.catalog.add_predicate(PredicateMeta {
            pred_id: id,
            label: name.to_owned(),
            arity,
            access_orders: vec![],
            proof_policy: PredicateProofPolicy::OnRequest,
            materialized: false,
        });
        self.pred_ids.insert(key, id);
        id
    }

    fn term(&mut self, s: &str) -> TermId {
        if let Some(&t) = self.term_ids.get(s) {
            return t;
        }
        let t = self.kernel.catalog.intern_term(s);
        self.term_ids.insert(s.to_owned(), t);
        t
    }

    fn load_ground_atom(&mut self, atom: &Pddl8GroundAtom) {
        let arity = atom.args.len() as u8;
        let pred = self.pred(&atom.pred, arity);
        let args: Vec<TermId> = atom.args.iter().map(|a| self.term(a)).collect();
        let row = FactRow8::new(pred, arity, &args, SRC);
        let _ = self
            .kernel
            .load_facts(FactBlock8::new(pred, arity, vec![row]));
    }

    fn load_may_fire(&mut self, label: &str) {
        let pred = self.pred("may_fire", 1);
        let term = self.term(label);
        let row = FactRow8::new(pred, 1, &[term], SRC);
        let _ = self.kernel.load_facts(FactBlock8::new(pred, 1, vec![row]));
    }

    fn query_may_fire(&mut self, label: &str) -> bool {
        let pred = self.pred("may_fire", 1);
        let term = self.term(label);
        let mut atom = P8Atom::new(pred, 1, &[term]);
        atom.binding_mask = 0b1;
        let q = QueryAtom8 {
            atom,
            output_mask: 0,
            proof_mode: ProofMode::PositiveOnly,
            epoch: EpochId(self.epoch),
        };
        matches!(self.kernel.query(&q), QueryResult::Answered(_))
    }

    fn query_goal_reached(&mut self) -> bool {
        let pred = self.pred("goal_reached", 1);
        let term = self.term("__goal__");
        let mut atom = P8Atom::new(pred, 1, &[term]);
        atom.binding_mask = 0b1;
        let q = QueryAtom8 {
            atom,
            output_mask: 0,
            proof_mode: ProofMode::PositiveOnly,
            epoch: EpochId(self.epoch),
        };
        matches!(self.kernel.query(&q), QueryResult::Answered(_))
    }

    fn tick_epoch(&mut self) {
        self.epoch += 1;
    }
}

/// Execute a `Pddl8Tape` through the Prolog8 admission gate.
///
/// Returns `(Pddl8ExecutionLog, Pddl8ExecutionReceipt, OCEL)`.
///
/// `case_id` identifies this execution in the OCEL log.
/// `policy_rules` are `(head_label, [body_labels])` May-fire Horn rules.
/// If empty, every scheduled op is pre-admitted via a fact.
pub fn execute_tape(
    tape: &Pddl8Tape, initial_state: &BTreeSet<Pddl8GroundAtom>, goal: &[Pddl8GroundAtom],
    case_id: &str, policy_rules: &[(&str, Vec<&str>)],
) -> Result<(Pddl8ExecutionLog, Pddl8ExecutionReceipt, OCEL), Pddl8Error> {
    validate_case_id(case_id)?;
    let mut ctx = Ctx::new();

    // Load initial world state
    for atom in initial_state {
        ctx.load_ground_atom(atom);
    }

    // Load policy rules or pre-admit all ops. `may_fire(label)` is a set
    // membership fact keyed on label alone (queried the same way below) —
    // loading the same label more than once creates duplicate FactBlock8s
    // that prolog8's scan_facts then scans and assembles proof/receipt
    // answers for redundantly, so labels are deduplicated before loading.
    if policy_rules.is_empty() {
        let mut loaded_labels = BTreeSet::new();
        for op in &tape.ops {
            if loaded_labels.insert(op.label.as_str()) {
                ctx.load_may_fire(&op.label);
            }
        }
    } else {
        for (i, (head, body)) in policy_rules.iter().enumerate() {
            load_may_fire_rule(&mut ctx, i as u32 + 1, head, body)?;
        }
    }

    let mut chain: [u8; 32] = [0; 32];
    let mut steps = Vec::new();
    let mut state = initial_state.clone();
    let mut ocel_events = Vec::new();

    for op in &tape.ops {
        if !ctx.query_may_fire(&op.label) {
            return Err(Pddl8Error::StepDenied {
                op_index: op.index,
                reason: format!("Prolog8 denied may_fire({})", op.label),
            });
        }

        // Apply effects
        for add in &op.action.add_effects {
            ctx.load_ground_atom(add);
            state.insert(add.clone());
        }
        for del in &op.action.del_effects {
            state.remove(del);
        }
        ctx.tick_epoch();

        // Receipt chain step
        let mut h = blake3::Hasher::new();
        h.update(&chain);
        h.update(op.label.as_bytes());
        h.update(&[op.index]);
        h.update(&ctx.epoch.to_le_bytes());
        chain = *h.finalize().as_bytes();
        let hash_hex = hex(&chain);

        steps.push(Pddl8StepResult {
            op_index: op.index,
            label: op.label.clone(),
            admitted: true,
            epoch_after: ctx.epoch,
            receipt_hash: hash_hex.clone(),
        });

        // OCEL event
        let adds_str = op
            .action
            .add_effects
            .iter()
            .map(|a| a.label())
            .collect::<Vec<_>>()
            .join(";");
        let dels_str = op
            .action
            .del_effects
            .iter()
            .map(|a| a.label())
            .collect::<Vec<_>>()
            .join(";");
        ocel_events.push(OCELEvent {
            id: format!("{case_id}-op{}", op.index),
            event_type: "pddl8-step".to_string(),
            time: chrono::Utc::now().fixed_offset(),
            attributes: vec![
                OCELEventAttribute {
                    name: "activity".to_string(),
                    value: OCELAttributeValue::String(op.label.clone()),
                },
                OCELEventAttribute {
                    name: "epoch".to_string(),
                    value: OCELAttributeValue::Integer(ctx.epoch as i64),
                },
                OCELEventAttribute {
                    name: "adds".to_string(),
                    value: OCELAttributeValue::String(adds_str),
                },
                OCELEventAttribute {
                    name: "dels".to_string(),
                    value: OCELAttributeValue::String(dels_str),
                },
                OCELEventAttribute {
                    name: "receipt".to_string(),
                    value: OCELAttributeValue::String(hash_hex),
                },
            ],
            relationships: vec![OCELRelationship {
                object_id: case_id.to_string(),
                qualifier: "case".to_string(),
            }],
        });
    }

    // Goal gate
    let goal_set: BTreeSet<Pddl8GroundAtom> = goal.iter().cloned().collect();
    let goal_atoms_met = goal_set.iter().all(|g| state.contains(g));
    if goal_atoms_met {
        let pred = ctx.pred("goal_reached", 1);
        let term = ctx.term("__goal__");
        let row = FactRow8::new(pred, 1, &[term], SRC);
        let _ = ctx.kernel.load_facts(FactBlock8::new(pred, 1, vec![row]));
    }
    let goal_reached = goal_atoms_met && ctx.query_goal_reached();

    let mut h = blake3::Hasher::new();
    h.update(&chain);
    h.update(if goal_reached {
        b"GOAL_MET"
    } else {
        b"GOAL_MISS"
    });
    chain = *h.finalize().as_bytes();
    let final_chain = hex(&chain);

    // Build receipt
    let op_labels: Vec<String> = tape.ops.iter().map(|o| o.label.clone()).collect();
    let init_labels: Vec<String> = initial_state.iter().map(|a| a.label()).collect();
    let goal_labels: Vec<String> = goal.iter().map(|a| a.label()).collect();

    let receipt = Pddl8ExecutionReceipt {
        plan_root: hash_strings(&op_labels),
        state_root: hash_strings(&init_labels),
        goal_root: hash_strings(&goal_labels),
        chain_hash: final_chain.clone(),
        goal_reached,
        step_count: steps.len(),
    };

    let log = Pddl8ExecutionLog {
        steps,
        goal_reached,
        chain_hash: final_chain,
    };

    // OCEL wrapper
    let ocel = OCEL {
        event_types: vec![OCELType {
            name: "pddl8-step".to_string(),
            attributes: vec![
                OCELTypeAttribute {
                    name: "activity".to_string(),
                    value_type: "string".to_string(),
                },
                OCELTypeAttribute {
                    name: "epoch".to_string(),
                    value_type: "integer".to_string(),
                },
                OCELTypeAttribute {
                    name: "adds".to_string(),
                    value_type: "string".to_string(),
                },
                OCELTypeAttribute {
                    name: "dels".to_string(),
                    value_type: "string".to_string(),
                },
                OCELTypeAttribute {
                    name: "receipt".to_string(),
                    value_type: "string".to_string(),
                },
            ],
        }],
        object_types: vec![OCELType {
            name: "plan-case".to_string(),
            attributes: vec![],
        }],
        events: ocel_events,
        objects: vec![OCELObject {
            id: case_id.to_string(),
            object_type: "plan-case".to_string(),
            attributes: vec![],
            relationships: vec![],
        }],
    };

    Ok((log, receipt, ocel))
}

fn load_may_fire_rule(
    ctx: &mut Ctx, _id: u32, head: &str, body: &[&str],
) -> Result<(), Pddl8Error> {
    if body.len() > 8 {
        return Err(Pddl8Error::BoundExceeded {
            what: "rule body atoms",
            limit: 8,
            got: body.len(),
        });
    }
    let head_pred = ctx.pred("may_fire", 1);
    let head_term = ctx.term(head);
    let mut head_atom = P8Atom::new(head_pred, 1, &[head_term]);
    head_atom.binding_mask = 0b1;

    let mut body_arr = [P8Atom::new(PredicateId(0), 0, &[]); 8];
    for (i, bl) in body.iter().enumerate() {
        let bp = ctx.pred("may_fire", 1);
        let bt = ctx.term(bl);
        let mut ba = P8Atom::new(bp, 1, &[bt]);
        ba.binding_mask = 0b1;
        body_arr[i] = ba;
    }
    let body_len = body.len() as u8;
    let rule = Rule8 {
        rule_id: RuleId(ctx.next_rule),
        head: head_atom,
        body: body_arr,
        body_len,
        body_mask: if body_len == 0 {
            0
        } else {
            (1u8 << body_len) - 1
        },
        negation_mask: 0,
        builtin_mask: 0,
        var_count: 0,
        var_live_mask: 0,
        feature_mask: FeatureBit::Facts.mask() | FeatureBit::HornRules.mask(),
        proof_mask: 0,
        plan_id: prolog8::types::PlanId::default(),
    };
    ctx.next_rule += 1;
    ctx.kernel
        .load_rule(rule)
        .map_err(|e| Pddl8Error::AdmissionLoadError(format!("{e:?}")))
}

fn hex(b: &[u8; 32]) -> String {
    b.iter().map(|x| format!("{x:02x}")).collect()
}

fn hash_strings(items: &[String]) -> String {
    let mut h = blake3::Hasher::new();
    for s in items {
        h.update(s.as_bytes());
        h.update(b"\x00");
    }
    h.finalize().to_hex().to_string()
}

fn validate_case_id(case_id: &str) -> Result<(), Pddl8Error> {
    if case_id.is_empty() || case_id.len() > 64 {
        return Err(Pddl8Error::InvalidCaseId(format!(
            "must be 1-64 chars, got {}",
            case_id.len()
        )));
    }
    for c in case_id.chars() {
        if !matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '-' | '_') {
            return Err(Pddl8Error::InvalidCaseId(format!(
                "invalid char '{c}'; only [a-zA-Z0-9_-] allowed"
            )));
        }
    }
    Ok(())
}

fn atom_key(pred: &str, args: &[String]) -> String {
    if args.is_empty() {
        pred.to_string()
    } else {
        format!("({} {})", pred, args.join(" "))
    }
}

/// Apply the propositional (add/del) parts of a `PddlEffect` to `state`,
/// substituting schema variables via `subst` and recursing through
/// `Timed`/`Forall`/`When` wrappers. Numeric effects are skipped — this is
/// only used for the post-hoc goal check in `execute_temporal_plan`, which
/// (like its classical-action counterpart above) only tracks propositional
/// atoms, not numeric fluents.
fn apply_pddl_effect_propositional(
    eff: &PddlEffect, subst: &HashMap<&str, &str>, state: &mut BTreeSet<String>,
) {
    use wasm4pm_compat::pddl::Pddl8Atom;
    let ground_args = |args: &[String]| -> Vec<String> {
        args.iter()
            .map(|a| {
                if Pddl8Atom::is_variable(a) {
                    subst
                        .get(a.as_str())
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| a.clone())
                } else {
                    a.clone()
                }
            })
            .collect()
    };
    match eff {
        PddlEffect::Add(a) => {
            state.insert(atom_key(&a.pred, &ground_args(&a.args)));
        }
        PddlEffect::Del(a) => {
            state.remove(&atom_key(&a.pred, &ground_args(&a.args)));
        }
        PddlEffect::Numeric(_) => {}
        PddlEffect::Timed(_, inner) => apply_pddl_effect_propositional(inner, subst, state),
        PddlEffect::Forall { effects, .. } => {
            for e in effects {
                apply_pddl_effect_propositional(e, subst, state);
            }
        }
        PddlEffect::When { effects, .. } => {
            for e in effects {
                apply_pddl_effect_propositional(e, subst, state);
            }
        }
    }
}

fn parse_action_label(label: &str) -> (&str, Vec<&str>) {
    if let Some(i) = label.find('(') {
        let name = &label[..i];
        let rest = label[i + 1..].trim_end_matches(')');
        let args = if rest.is_empty() {
            vec![]
        } else {
            rest.split(',').collect()
        };
        (name, args)
    } else {
        (label, vec![])
    }
}

/// Recompute the rolling BLAKE3 chain over a sequence of temporal plan steps.
///
/// This is the same algorithm used inside `execute_temporal_plan` — call it with
/// the plan steps from a `WorldManufactureReceipt` to independently verify the
/// `plan_chain_hash` field.
pub fn compute_plan_chain(steps: &[wasm4pm_compat::pddl::TemporalPlanStep]) -> String {
    let mut chain = [0u8; 32];
    for step in steps {
        let mut h = blake3::Hasher::new();
        h.update(&chain);
        h.update(step.action_name.as_bytes());
        h.update(&step.start_time.to_bits().to_le_bytes());
        h.update(&step.duration.to_bits().to_le_bytes());
        chain = *h.finalize().as_bytes();
    }
    hex(&chain)
}

/// L3 substage timing for one `execute_temporal_plan_instrumented` call —
/// see `docs/DFCM_BENCHMARK_ANALYSIS.md`'s resolution-ladder section.
///
/// `proof_receipt_build_ns` is a proxy, not a true isolation of
/// `prolog8::Kernel::query`'s internal proof/receipt assembly: that code is
/// private to the `prolog8` crate and not independently instrumentable from
/// outside it. This field times the externally-visible per-step BLAKE3
/// chain-extension block that consumes each query's result instead — the
/// closest observable stand-in for "receipt-building work per step."
#[derive(Debug, Clone, Copy, Default)]
pub struct SubstageNs {
    pub fact_load_ns: u128,
    pub query_ns: u128,
    pub effects_apply_ns: u128,
    pub proof_receipt_build_ns: u128,
    pub trace_build_ns: u128,
}

/// Bench-only instrumented variant of `execute_temporal_plan`, duplicating
/// its logic with `Instant::now()` checkpoints around each L3 substage.
/// Exists *only* so DfCM crown-suite benchmarking can attribute
/// admission/replay cost by substage without adding timing overhead to
/// `execute_temporal_plan` itself, which every production caller uses.
/// Keep this in sync with `execute_temporal_plan` if that function's
/// structure changes.
pub fn execute_temporal_plan_instrumented(
    plan: &TemporalPlan, domain: &Pddl8Domain, problem: &Pddl8Problem, case_id: &str,
    policy_rules: &[(&str, Vec<&str>)],
) -> Result<(TemporalExecutionReceipt, OCEL, SubstageNs), Pddl8Error> {
    use blake3::Hasher;
    use std::time::Instant;

    validate_case_id(case_id)?;

    let mut steps = plan.steps.clone();
    steps.sort_by(|a, b| a.start_time.partial_cmp(&b.start_time).unwrap());

    let mut substage = SubstageNs::default();

    let t_fact_load = Instant::now();
    let mut ctx = Ctx::new();
    if policy_rules.is_empty() {
        // Deduplicate labels before loading — see the dedup comment on the
        // production execute_temporal_plan below for the full rationale.
        let mut loaded_labels = BTreeSet::new();
        for step in &steps {
            if loaded_labels.insert(step.action_name.as_str()) {
                ctx.load_may_fire(&step.action_name);
            }
        }
    } else {
        for (i, (head, body)) in policy_rules.iter().enumerate() {
            load_may_fire_rule(&mut ctx, i as u32 + 1, head, body)?;
        }
    }
    substage.fact_load_ns += t_fact_load.elapsed().as_nanos();

    let mut state: BTreeSet<String> = problem
        .init
        .iter()
        .map(|a| atom_key(&a.pred, &a.args))
        .collect();
    let goal_keys: BTreeSet<String> = problem
        .goal
        .iter()
        .map(|a| atom_key(&a.pred, &a.args))
        .collect();

    let mut plan_hasher = Hasher::new();
    for step in &steps {
        plan_hasher.update(step.start_time.to_bits().to_le_bytes().as_ref());
        plan_hasher.update(step.action_name.as_bytes());
        for arg in &step.args {
            plan_hasher.update(arg.as_bytes());
        }
    }
    let plan_root = hex(plan_hasher.finalize().as_bytes());
    let state_root = hash_strings(
        &problem
            .init
            .iter()
            .map(|a| format!("({} {})", a.pred, a.args.join(" ")))
            .collect::<Vec<_>>(),
    );
    let goal_root = hash_strings(
        &problem
            .goal
            .iter()
            .map(|a| format!("({} {})", a.pred, a.args.join(" ")))
            .collect::<Vec<_>>(),
    );

    let mut chain = [0u8; 32];
    let mut ocel_events = Vec::new();

    for (i, step) in steps.iter().enumerate() {
        let t_query = Instant::now();
        let admitted = ctx.query_may_fire(&step.action_name);
        substage.query_ns += t_query.elapsed().as_nanos();
        if !admitted {
            return Err(Pddl8Error::StepDenied {
                op_index: i as u8,
                reason: format!("Prolog8 denied may_fire({})", step.action_name),
            });
        }

        let t_effects = Instant::now();
        let (base_name, parsed_args) = parse_action_label(&step.action_name);
        if let Some(schema) = domain.actions.iter().find(|a| a.name == base_name) {
            let subst: HashMap<&str, &str> = schema
                .params
                .iter()
                .zip(parsed_args.iter())
                .map(|(p, a)| (p.as_str(), *a))
                .collect();
            for eff in &schema.add_effects {
                let grounded: Vec<String> = eff
                    .args
                    .iter()
                    .map(|a| {
                        subst
                            .get(a.as_str())
                            .map(|&s| s.to_string())
                            .unwrap_or_else(|| a.clone())
                    })
                    .collect();
                state.insert(atom_key(&eff.pred, &grounded));
            }
            for eff in &schema.del_effects {
                let grounded: Vec<String> = eff
                    .args
                    .iter()
                    .map(|a| {
                        subst
                            .get(a.as_str())
                            .map(|&s| s.to_string())
                            .unwrap_or_else(|| a.clone())
                    })
                    .collect();
                state.remove(&atom_key(&eff.pred, &grounded));
            }
        } else if let Some(da) = domain
            .durative_actions
            .iter()
            .find(|d| d.name == step.action_name)
        {
            let subst: HashMap<&str, &str> = da
                .params
                .iter()
                .map(|(p, _)| p.as_str())
                .zip(step.args.iter().map(|s| s.as_str()))
                .collect();
            for eff in &da.effects {
                apply_pddl_effect_propositional(eff, &subst, &mut state);
            }
        }
        substage.effects_apply_ns += t_effects.elapsed().as_nanos();

        let t_chain = Instant::now();
        let mut h = Hasher::new();
        h.update(&chain);
        h.update(step.action_name.as_bytes());
        h.update(&step.start_time.to_bits().to_le_bytes());
        h.update(&step.duration.to_bits().to_le_bytes());
        chain = *h.finalize().as_bytes();
        substage.proof_receipt_build_ns += t_chain.elapsed().as_nanos();

        let t_trace = Instant::now();
        let relationships = step
            .args
            .iter()
            .enumerate()
            .map(|(j, arg)| OCELRelationship {
                object_id: format!("obj:{case_id}:{j}:{arg}"),
                qualifier: "arg".to_string(),
            })
            .collect();
        ocel_events.push(OCELEvent {
            id: format!("{case_id}:temporal:{i}"),
            event_type: "temporal-step".to_string(),
            time: chrono::Utc::now().fixed_offset(),
            attributes: vec![
                OCELEventAttribute {
                    name: "activity".to_string(),
                    value: OCELAttributeValue::String(step.action_name.clone()),
                },
                OCELEventAttribute {
                    name: "duration".to_string(),
                    value: OCELAttributeValue::Float(step.duration),
                },
                OCELEventAttribute {
                    name: "start_time".to_string(),
                    value: OCELAttributeValue::Float(step.start_time),
                },
                OCELEventAttribute {
                    name: "step_index".to_string(),
                    value: OCELAttributeValue::Integer(i as i64),
                },
                OCELEventAttribute {
                    name: "args".to_string(),
                    value: OCELAttributeValue::String(step.args.join(",")),
                },
            ],
            relationships,
        });
        substage.trace_build_ns += t_trace.elapsed().as_nanos();
    }

    let goal_reached = goal_keys.iter().all(|g| state.contains(g));

    let t_final_chain = Instant::now();
    let mut h = Hasher::new();
    h.update(&chain);
    h.update(if goal_reached {
        b"GOAL_MET"
    } else {
        b"GOAL_MISS"
    });
    chain = *h.finalize().as_bytes();
    let chain_hash = hex(&chain);
    substage.proof_receipt_build_ns += t_final_chain.elapsed().as_nanos();

    let makespan = steps
        .iter()
        .map(|s| s.start_time + s.duration)
        .fold(0.0_f64, f64::max);
    let _ = domain;
    let requirements: Vec<String> = vec![];

    let receipt = TemporalExecutionReceipt {
        plan_root,
        state_root,
        goal_root,
        makespan,
        step_count: steps.len(),
        requirements,
        goal_reached,
        chain_hash,
    };

    let t_ocel_objects = Instant::now();
    let mut seen_objects = std::collections::BTreeSet::new();
    let mut objects = Vec::new();
    for step in &steps {
        for (j, arg) in step.args.iter().enumerate() {
            let obj_id = format!("obj:{case_id}:{j}:{arg}");
            if seen_objects.insert(obj_id.clone()) {
                objects.push(OCELObject {
                    id: obj_id,
                    object_type: "pddl-object".to_string(),
                    attributes: vec![],
                    relationships: vec![],
                });
            }
        }
    }
    objects.push(OCELObject {
        id: case_id.to_string(),
        object_type: "plan-case".to_string(),
        attributes: vec![],
        relationships: vec![],
    });

    let ocel = OCEL {
        event_types: vec![OCELType {
            name: "temporal-step".to_string(),
            attributes: vec![
                OCELTypeAttribute {
                    name: "activity".to_string(),
                    value_type: "string".to_string(),
                },
                OCELTypeAttribute {
                    name: "duration".to_string(),
                    value_type: "float".to_string(),
                },
                OCELTypeAttribute {
                    name: "start_time".to_string(),
                    value_type: "float".to_string(),
                },
                OCELTypeAttribute {
                    name: "step_index".to_string(),
                    value_type: "integer".to_string(),
                },
                OCELTypeAttribute {
                    name: "args".to_string(),
                    value_type: "string".to_string(),
                },
            ],
        }],
        object_types: vec![
            OCELType {
                name: "plan-case".to_string(),
                attributes: vec![],
            },
            OCELType {
                name: "pddl-object".to_string(),
                attributes: vec![],
            },
        ],
        events: ocel_events,
        objects,
    };
    substage.trace_build_ns += t_ocel_objects.elapsed().as_nanos();

    Ok((receipt, ocel, substage))
}

/// Execute a temporal plan through the Prolog8 admission gate + BLAKE3 receipt + OCEL output.
///
/// Mirrors `execute_tape` — every step is checked via `query_may_fire` before it fires.
/// `policy_rules` are `(head_label, [body_labels])` Horn rules. Empty slice = permissive
/// (every unique action name is pre-admitted as a ground fact, identical to today's behavior).
pub fn execute_temporal_plan(
    plan: &TemporalPlan, domain: &Pddl8Domain, problem: &Pddl8Problem, case_id: &str,
    policy_rules: &[(&str, Vec<&str>)],
) -> Result<(TemporalExecutionReceipt, OCEL), Pddl8Error> {
    use blake3::Hasher;

    validate_case_id(case_id)?;

    // Sort steps by start_time
    let mut steps = plan.steps.clone();
    steps.sort_by(|a, b| a.start_time.partial_cmp(&b.start_time).unwrap());

    // Initialize Prolog8 admission gate — mirrors execute_tape lines 121-133.
    let mut ctx = Ctx::new();
    if policy_rules.is_empty() {
        // `may_fire(label)` is a set-membership fact keyed on the bare
        // action label alone — queried the same way in the loop below
        // (`ctx.query_may_fire(&step.action_name)`), never by step index
        // or instance identity. Under this existing schema-level admission
        // model, loading the same label once vs. N times cannot change
        // what's provable, so labels are deduplicated before loading: with
        // durative-action plans, every step of the same schema shares the
        // same bare action_name, and loading it once per step created N
        // byte-identical FactBlock8s that prolog8's scan_facts then scanned
        // and assembled a redundant proof/receipt answer for on every
        // query (see docs/DFCM_BENCHMARK_ANALYSIS.md's L3 substage finding
        // — this was ~98% of admission_ns/replay_ns).
        let mut loaded_labels = BTreeSet::new();
        for step in &steps {
            if loaded_labels.insert(step.action_name.as_str()) {
                ctx.load_may_fire(&step.action_name);
            }
        }
    } else {
        for (i, (head, body)) in policy_rules.iter().enumerate() {
            load_may_fire_rule(&mut ctx, i as u32 + 1, head, body)?;
        }
    }

    // Build initial state for goal tracking (problem.init atoms are ground — no vars)
    let mut state: BTreeSet<String> = problem
        .init
        .iter()
        .map(|a| atom_key(&a.pred, &a.args))
        .collect();

    // Goal atoms for final check
    let goal_keys: BTreeSet<String> = problem
        .goal
        .iter()
        .map(|a| atom_key(&a.pred, &a.args))
        .collect();

    // Compute plan_root: BLAKE3 over all (start_time, action_name, args)
    let mut plan_hasher = Hasher::new();
    for step in &steps {
        plan_hasher.update(step.start_time.to_bits().to_le_bytes().as_ref());
        plan_hasher.update(step.action_name.as_bytes());
        for arg in &step.args {
            plan_hasher.update(arg.as_bytes());
        }
    }
    let plan_root = hex(plan_hasher.finalize().as_bytes());

    // Compute state_root: BLAKE3 over initial state atoms
    let state_root = hash_strings(
        &problem
            .init
            .iter()
            .map(|a| format!("({} {})", a.pred, a.args.join(" ")))
            .collect::<Vec<_>>(),
    );

    // Compute goal_root: BLAKE3 over goal atoms
    let goal_root = hash_strings(
        &problem
            .goal
            .iter()
            .map(|a| format!("({} {})", a.pred, a.args.join(" ")))
            .collect::<Vec<_>>(),
    );

    // Rolling BLAKE3 chain over plan steps
    let mut chain = [0u8; 32];
    let mut ocel_events = Vec::new();

    for (i, step) in steps.iter().enumerate() {
        // Prolog8 admission gate — same contract as execute_tape.
        if !ctx.query_may_fire(&step.action_name) {
            return Err(Pddl8Error::StepDenied {
                op_index: i as u8,
                reason: format!("Prolog8 denied may_fire({})", step.action_name),
            });
        }

        // Apply action effects to track state for goal verification.
        // Parse the action label ("pick-up(a)" → name "pick-up", args ["a"]) and
        // look up the schema in the domain to ground effects.
        let (base_name, parsed_args) = parse_action_label(&step.action_name);
        if let Some(schema) = domain.actions.iter().find(|a| a.name == base_name) {
            let subst: HashMap<&str, &str> = schema
                .params
                .iter()
                .zip(parsed_args.iter())
                .map(|(p, a)| (p.as_str(), *a))
                .collect();
            for eff in &schema.add_effects {
                let grounded: Vec<String> = eff
                    .args
                    .iter()
                    .map(|a| {
                        subst
                            .get(a.as_str())
                            .map(|&s| s.to_string())
                            .unwrap_or_else(|| a.clone())
                    })
                    .collect();
                state.insert(atom_key(&eff.pred, &grounded));
            }
            for eff in &schema.del_effects {
                let grounded: Vec<String> = eff
                    .args
                    .iter()
                    .map(|a| {
                        subst
                            .get(a.as_str())
                            .map(|&s| s.to_string())
                            .unwrap_or_else(|| a.clone())
                    })
                    .collect();
                state.remove(&atom_key(&eff.pred, &grounded));
            }
        } else if let Some(da) = domain
            .durative_actions
            .iter()
            .find(|d| d.name == step.action_name)
        {
            // Durative-action counterpart of the classical-action branch above.
            // Unlike classical action labels ("pick-up(a)"), `step.action_name`
            // for a durative step is the bare schema name (e.g. "assign-worker")
            // with the bound args in the separate `step.args` field — see
            // `GroundTemporalProblem::find_temporal_plan` / `ground_durative_schema`.
            // Apply all propositional add/del effects (both at-start and
            // at-end, ignoring intra-step timing) so goal checks against
            // durative-action plans see the same final state the planner
            // already verified in `find_temporal_plan`.
            let subst: HashMap<&str, &str> = da
                .params
                .iter()
                .map(|(p, _)| p.as_str())
                .zip(step.args.iter().map(|s| s.as_str()))
                .collect();
            for eff in &da.effects {
                apply_pddl_effect_propositional(eff, &subst, &mut state);
            }
        }

        // Extend chain: BLAKE3(chain || step.action_name || start_time_bits || duration_bits)
        let mut h = Hasher::new();
        h.update(&chain);
        h.update(step.action_name.as_bytes());
        h.update(&step.start_time.to_bits().to_le_bytes());
        h.update(&step.duration.to_bits().to_le_bytes());
        chain = *h.finalize().as_bytes();

        // Emit OCEL event for this step
        let relationships = step
            .args
            .iter()
            .enumerate()
            .map(|(j, arg)| OCELRelationship {
                object_id: format!("obj:{case_id}:{j}:{arg}"),
                qualifier: "arg".to_string(),
            })
            .collect();

        ocel_events.push(OCELEvent {
            id: format!("{case_id}:temporal:{i}"),
            event_type: "temporal-step".to_string(),
            time: chrono::Utc::now().fixed_offset(),
            attributes: vec![
                OCELEventAttribute {
                    name: "activity".to_string(),
                    value: OCELAttributeValue::String(step.action_name.clone()),
                },
                OCELEventAttribute {
                    name: "duration".to_string(),
                    value: OCELAttributeValue::Float(step.duration),
                },
                OCELEventAttribute {
                    name: "start_time".to_string(),
                    value: OCELAttributeValue::Float(step.start_time),
                },
                OCELEventAttribute {
                    name: "step_index".to_string(),
                    value: OCELAttributeValue::Integer(i as i64),
                },
                OCELEventAttribute {
                    name: "args".to_string(),
                    value: OCELAttributeValue::String(step.args.join(",")),
                },
            ],
            relationships,
        });
    }

    // Check goal against final state
    let goal_reached = goal_keys.iter().all(|g| state.contains(g));

    // Final chain: include goal verdict (matches execute_tape contract)
    let mut h = Hasher::new();
    h.update(&chain);
    h.update(if goal_reached {
        b"GOAL_MET"
    } else {
        b"GOAL_MISS"
    });
    chain = *h.finalize().as_bytes();
    let chain_hash = hex(&chain);

    let makespan = steps
        .iter()
        .map(|s| s.start_time + s.duration)
        .fold(0.0_f64, f64::max);

    // Pddl8Domain has no requirements field — use empty vec
    let _ = domain;
    let requirements: Vec<String> = vec![];

    let receipt = TemporalExecutionReceipt {
        plan_root,
        state_root,
        goal_root,
        makespan,
        step_count: steps.len(),
        requirements,
        goal_reached,
        chain_hash,
    };

    // Collect unique arg objects
    let mut seen_objects = std::collections::BTreeSet::new();
    let mut objects = Vec::new();
    for step in &steps {
        for (j, arg) in step.args.iter().enumerate() {
            let obj_id = format!("obj:{case_id}:{j}:{arg}");
            if seen_objects.insert(obj_id.clone()) {
                objects.push(OCELObject {
                    id: obj_id,
                    object_type: "pddl-object".to_string(),
                    attributes: vec![],
                    relationships: vec![],
                });
            }
        }
    }
    objects.push(OCELObject {
        id: case_id.to_string(),
        object_type: "plan-case".to_string(),
        attributes: vec![],
        relationships: vec![],
    });

    let ocel = OCEL {
        event_types: vec![OCELType {
            name: "temporal-step".to_string(),
            attributes: vec![
                OCELTypeAttribute {
                    name: "activity".to_string(),
                    value_type: "string".to_string(),
                },
                OCELTypeAttribute {
                    name: "duration".to_string(),
                    value_type: "float".to_string(),
                },
                OCELTypeAttribute {
                    name: "start_time".to_string(),
                    value_type: "float".to_string(),
                },
                OCELTypeAttribute {
                    name: "step_index".to_string(),
                    value_type: "integer".to_string(),
                },
                OCELTypeAttribute {
                    name: "args".to_string(),
                    value_type: "string".to_string(),
                },
            ],
        }],
        object_types: vec![
            OCELType {
                name: "plan-case".to_string(),
                attributes: vec![],
            },
            OCELType {
                name: "pddl-object".to_string(),
                attributes: vec![],
            },
        ],
        events: ocel_events,
        objects,
    };

    Ok((receipt, ocel))
}
