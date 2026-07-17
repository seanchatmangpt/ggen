// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::type_complexity, clippy::ptr_arg, dead_code)]

use crate::fastmap::FxHashSet;
use crate::{
    BackwardChainer, Binding, Encoder, QueryEngine, Reasoner, Rule, RuleIndex, SimpleQueryEngine,
    Triple, TripleIndex, TripleStore, VarOrTerm,
};
use log::trace; // Use log crate when building application
use std::collections::HashMap;
use std::fmt::Write;
use std::rc::Rc;
use std::sync::Arc;

use crate::imars_window::ImarsWindow;

pub struct CSprite {
    pub rules: Vec<Rule>,
    pub rules_index: RuleIndex,
    pub triple_index: TripleIndex,
    window_reasoner: CSpriteReasoner,
    reasoner: Reasoner,
    imars: ImarsWindow<Triple>,
}

impl Default for CSprite {
    fn default() -> Self {
        Self::new()
    }
}

impl CSprite {
    pub fn new() -> CSprite {
        CSprite {
            rules: Vec::new(),
            rules_index: RuleIndex::new(),
            triple_index: TripleIndex::new(),
            window_reasoner: CSpriteReasoner {},
            reasoner: Reasoner {},
            imars: ImarsWindow::new_no_window(),
        }
    }
    pub fn from(data: &str) -> CSprite {
        let triple_store = TripleStore::from(data);
        CSprite {
            rules: triple_store.rules,
            rules_index: triple_store.rules_index,
            triple_index: triple_store.triple_index,
            window_reasoner: CSpriteReasoner {},
            reasoner: Reasoner {},
            imars: ImarsWindow::new_no_window(),
        }
    }
    pub fn window_update(
        &mut self,
        new_data: Vec<(i32, Rc<Triple>)>,
        _old_data: Vec<(i32, Rc<Triple>)>,
        last_ts: &i32,
    ) {
        //remove expired data
        let old_items = self.imars.remove_old_elements(last_ts);

        old_items
            .into_iter()
            .for_each(|(_ts, item)| self.triple_index.remove_ref(&item));

        //add new data

        new_data.iter().for_each(|(ts, triple)| {
            self.imars.add_without_update(triple.clone(), *ts);
            self.add_ref(triple.clone());
        });
        let _materialization = self.window_reasoner.materialize(
            &new_data,
            &mut self.triple_index,
            &self.rules_index,
            &mut self.imars,
        );

        //add materialization to maintenance program
        //materialization.into_iter().for_each(|(ts,t)|self.imars.add_without_update(t,ts));
    }
    fn decode_triples(triples: &Vec<(i32, Rc<Triple>)>) -> String {
        let mut res = String::new();
        for (ts, triple) in triples {
            let decoded_s = Encoder::decode(&triple.s.to_encoded()).unwrap();
            let decoded_p = Encoder::decode(&triple.p.to_encoded()).unwrap();
            let decoded_o = Encoder::decode(&triple.o.to_encoded()).unwrap();

            writeln!(
                &mut res,
                "{} {} {} @ {}.",
                decoded_s, decoded_p, decoded_o, ts
            )
            .unwrap();
        }
        res
    }

    pub fn add(&mut self, triple: Triple) {
        trace! {"Adding triple: {:?}", self.decode_triple(&triple) }
        self.triple_index.add(triple);
    }
    pub fn add_ref(&mut self, triple: Rc<Triple>) {
        trace! {"Adding triple: {:?}", self.decode_triple(triple.as_ref()) }
        self.triple_index.add_ref(triple);
    }
    pub fn remove_ref(&mut self, triple: Rc<Triple>) {
        trace! {"Removing triple: {:?}", self.decode_triple(triple.as_ref()) }
        self.triple_index.remove_ref(&triple);
    }
    pub fn add_rules(&mut self, rules: Vec<Rule>) {
        rules
            .into_iter()
            .for_each(|rule| self.rules_index.add(rule));
    }
    pub fn len(&self) -> usize {
        self.triple_index.len()
    }
    pub fn is_empty(&self) -> bool {
        self.triple_index.is_empty()
    }
    fn decode_triple(&self, triple: &Triple) -> String {
        let s = Encoder::decode(&triple.s.to_encoded()).unwrap();
        let p = Encoder::decode(&triple.p.to_encoded()).unwrap();
        let o = Encoder::decode(&triple.o.to_encoded()).unwrap();
        format!("{} {} {}", s, p, o)
    }
    pub fn materialize_window(
        &mut self,
        window_items: Vec<(i32, Rc<Triple>)>,
    ) -> Vec<(i32, Triple)> {
        self.window_reasoner
            .materialize(
                &window_items,
                &mut self.triple_index,
                &self.rules_index,
                &mut self.imars,
            )
            .into_iter()
            .map(|(ts, t)| (ts, Rc::try_unwrap(t).unwrap_or_else(|t| (*t).clone())))
            .collect()
    }
    pub fn materialize(&mut self) -> Result<Vec<Triple>, String> {
        let aggregates = HashMap::new();
        let strata = crate::datalog::validate_rules(&self.rules, &aggregates)
            .unwrap_or_else(|_| vec![0; self.rules.len()]);
        self.reasoner.materialize(
            &mut self.triple_index,
            &self.rules,
            &strata,
            &aggregates,
            &Vec::new(),
            &mut Vec::new(),
            &mut Vec::new(),
            &mut Vec::new(),
        )
    }
    pub fn clear(&mut self) {
        self.triple_index.clear();
    }
    pub(crate) fn compute_sprite(&mut self, query: &Triple) {
        let (backward_rules, hierarcies) = self.eval_backward_csprite(query);

        // new rules
        let mut new_rules: Vec<Arc<Rule>> = backward_rules
            .into_iter()
            .filter(|r| r.body.len() > 1)
            .collect();
        for hierarchy in hierarcies {
            let rewritten_hierarchy = Self::rewrite_hierarchy(&hierarchy);
            rewritten_hierarchy
                .into_iter()
                .for_each(|r| new_rules.push(Arc::new(r)));
        }

        // new rule index
        let mut parsed_rules_index = RuleIndex::new();
        for rule in new_rules.iter() {
            parsed_rules_index.add_ref(rule);
        }
        self.rules_index = parsed_rules_index;
        self.rules = new_rules
            .into_iter()
            .map(|r| Arc::try_unwrap(r).unwrap_or_else(|r| (*r).clone()))
            .collect();
    }
    fn eval_backward_csprite(
        &self,
        rule_head: &Triple,
    ) -> (FxHashSet<Arc<Rule>>, Vec<Vec<Arc<Rule>>>) {
        let mut matched_rules = FxHashSet::default();
        let mut hierarchies = Vec::new();
        let mut history = FxHashSet::default();
        self.eval_backward_csprite_helper(
            rule_head,
            &mut matched_rules,
            false,
            &mut hierarchies,
            &mut history,
        );
        (matched_rules, hierarchies)
        //self.eval_backward_csprite_helper_with_stack(rule_head)
    }
    fn eval_backward_csprite_helper(
        &self,
        rule_head: &Triple,
        matched_rules: &mut FxHashSet<Arc<Rule>>,
        hierarchy: bool,
        hierarchies: &mut Vec<Vec<Arc<Rule>>>,
        history: &mut FxHashSet<Triple>,
    ) {
        if !history.insert(rule_head.clone()) {
            return;
        }
        let sub_rules: Vec<(Arc<Rule>, Vec<(usize, usize)>)> =
            BackwardChainer::find_subrules(&self.rules_index, rule_head);
        let mut current_hierarchy = false;
        for (sub_rule, _var_subs) in sub_rules.into_iter() {
            if matched_rules.insert(sub_rule.clone()) {
                if sub_rule.body.len() == 1 {
                    //hierarchy candidate
                    if hierarchy {
                        if let Some(current_hierarchy) = hierarchies.last_mut() {
                            current_hierarchy.push(sub_rule.clone());
                        }
                    } else {
                        hierarchies.push(Vec::from([sub_rule.clone()]));
                    }
                    current_hierarchy = true;
                }
                for body_lit in &sub_rule.body {
                    let rule_atom = &body_lit.pattern;
                    //recursive call
                    self.eval_backward_csprite_helper(
                        rule_atom,
                        matched_rules,
                        current_hierarchy,
                        hierarchies,
                        history,
                    );
                }
            }
        }
        history.remove(rule_head);
    }
    fn eval_backward_csprite_helper_with_stack(
        &self,
        rule_head: &Triple,
    ) -> (FxHashSet<Arc<Rule>>, Vec<Vec<Arc<Rule>>>) {
        enum StackFrame {
            Enter { rule_head: Triple, hierarchy: bool },
            Exit { rule_head: Triple },
        }
        let mut stack = vec![StackFrame::Enter {
            rule_head: rule_head.clone(),
            hierarchy: false,
        }];
        let mut matched_rules = FxHashSet::default();
        let mut hierarchies: Vec<Vec<Arc<Rule>>> = Vec::new();
        let mut history = FxHashSet::default();
        while let Some(frame) = stack.pop() {
            match frame {
                StackFrame::Exit { rule_head } => {
                    history.remove(&rule_head);
                }
                StackFrame::Enter {
                    rule_head,
                    hierarchy,
                } => {
                    if !history.insert(rule_head.clone()) {
                        continue;
                    }
                    stack.push(StackFrame::Exit {
                        rule_head: rule_head.clone(),
                    });
                    let sub_rules: Vec<(Arc<Rule>, Vec<(usize, usize)>)> =
                        BackwardChainer::find_subrules(&self.rules_index, &rule_head);
                    for (sub_rule, _var_subs) in sub_rules.into_iter() {
                        if matched_rules.insert(sub_rule.clone()) {
                            let mut current_hierarchy = false;
                            if sub_rule.body.len() == 1 {
                                //hierarchy candidate
                                if hierarchy {
                                    if let Some(h) = hierarchies.last_mut() {
                                        h.push(sub_rule.clone());
                                    }
                                } else {
                                    hierarchies.push(Vec::from([sub_rule.clone()]));
                                }
                                current_hierarchy = true;
                            }
                            for body_lit in sub_rule.body.iter().rev() {
                                let rule_atom = &body_lit.pattern;
                                stack.push(StackFrame::Enter {
                                    rule_head: rule_atom.clone(),
                                    hierarchy: current_hierarchy,
                                });
                            }
                        }
                    }
                }
            }
        }
        (matched_rules, hierarchies)
    }
    fn rewrite_hierarchy(rules: &Vec<Arc<Rule>>) -> Vec<Rule> {
        let mut new_rules = Vec::new();
        if !rules.is_empty() {
            let new_head = &rules.first().unwrap().head;
            for rule in rules.iter() {
                new_rules.push(Rule {
                    body: rule.body.clone(),
                    head: new_head.clone(),
                })
            }
        }

        new_rules
    }
}
pub struct CSpriteReasoner;

impl CSpriteReasoner {
    pub fn materialize(
        &mut self,
        new_data: &Vec<(i32, Rc<Triple>)>,
        triple_index: &mut TripleIndex,
        rules_index: &RuleIndex,
        window: &mut ImarsWindow<Triple>,
    ) -> Vec<(i32, Rc<Triple>)> {
        let mut inferred = Vec::new();
        let mut counter = 0;
        let mut pending_changes = Vec::new();
        new_data
            .iter()
            .for_each(|i| pending_changes.push(i.clone()));
        while counter < pending_changes.len() {
            let (_ts, process_quad) = pending_changes.get(counter).unwrap();
            //trace!("Processing: {:?}",decode_triple(process_quad));
            //let matching_rules = self.find_matching_rules(process_quad);
            let matching_rules = rules_index.find_match(process_quad);
            trace!("Found Rules: {:?}", matching_rules);
            let mut new_triples = Vec::new();

            for rule in matching_rules {
                if let Some(mut temp_bindings) =
                    SimpleQueryEngine::query(triple_index, &rule.body, None)
                {
                    let new_heads =
                        Reasoner::substitute_head_with_bindings(&rule.head, &temp_bindings);
                    let reconstructed = CSpriteReasoner::reconstruct_triples_from_bindings(
                        &mut temp_bindings,
                        rule,
                    );
                    for i in 0..new_heads.len() {
                        let new_head = new_heads.get(i).unwrap().clone();
                        //println!("Inferred head: {:?}", Self::decode_triple(&new_head,encoder));
                        //compute time stamp
                        let triples = reconstructed.get(i).unwrap();
                        //println!("Triples: {:?}", triples);
                        // let min_ts: Vec<Option<i32>> =triples.iter().map(|t|window.get_time_stamp(Rc::new(t.clone()))).collect();
                        // let min_ts =triples.iter().map(|t|window.get_time_stamp(Rc::new(t.clone()))).filter(|t|t.is_some()).min().unwrap().unwrap();//todo update to reference only
                        let items: Vec<(i32, &Triple)> = triples
                            .iter()
                            .map(|t| (window.get_time_stamp(Rc::new(t.clone())), t))
                            .filter(|(ts, _t)| ts.is_some())
                            .map(|(ts, t)| (ts.unwrap(), t))
                            .collect();
                        if items.is_empty() {
                            continue; // skip this head — no timestamps available
                        }
                        let (min_ts, min_triple) =
                            items.iter().fold(
                                items[0],
                                |acc, &item| {
                                    if acc.0 <= item.0 {
                                        acc
                                    } else {
                                        item
                                    }
                                },
                            );
                        new_triples.push((min_ts, new_head.clone(), min_triple.clone()));
                    }
                }
            }
            for (ts, new_triple, min_triple) in new_triples {
                if !triple_index.contains(&new_triple) {
                    //trace!("Inferred: {:?}",self.decode_triple(&triple));
                    let triple_ref = Rc::new(new_triple);
                    inferred.push((ts, triple_ref.clone()));
                    //add to maintanance program

                    // window.add_without_update(triple_ref.clone(),ts);
                    window.add_after(triple_ref.clone(), Rc::new(min_triple.clone()), ts);
                    pending_changes.push((ts, triple_ref.clone()));

                    triple_index.add_ref(triple_ref);
                }
            }
            counter += 1;
        }

        inferred
    }
    #[allow(dead_code)]
    fn decode_triple(triple: &Triple) -> String {
        let mut res = String::new();

        let decoded_s = Encoder::decode(&triple.s.to_encoded()).unwrap();
        let decoded_p = Encoder::decode(&triple.p.to_encoded()).unwrap();
        let decoded_o = Encoder::decode(&triple.o.to_encoded()).unwrap();

        writeln!(&mut res, "{} {} {}.", decoded_s, decoded_p, decoded_o).unwrap();

        res
    }
    pub(crate) fn reconstruct_triples_from_bindings(
        result_bindings: &mut Binding,
        rule: &Rule,
    ) -> Vec<Vec<Triple>> {
        let mut counter = 0;
        let mut all_triples = Vec::new();
        while counter < result_bindings.len() {
            let mut triples = Vec::new();
            // Skip negated literals: `SimpleQueryEngine::query` (queryengine/mod.rs)
            // only ever populates `result_bindings` from the rule's non-negated body
            // literals -- a negated literal succeeds precisely because no matching fact
            // exists, so it has no witnessing triple to reconstruct. A variable that
            // appears solely inside a negated literal (e.g. `not {?x <urn:excluded>
            // ?reason}`) is therefore never a key in `result_bindings`; iterating
            // negated literals here previously panicked on that lookup.
            for body_lit in rule.body.iter().filter(|bl| !bl.negated) {
                let triple = &body_lit.pattern;
                let s = if triple.s.is_var() {
                    VarOrTerm::new_encoded_term(
                        *result_bindings
                            .get(&triple.s.as_var().name)
                            .unwrap()
                            .get(counter)
                            .unwrap(),
                    )
                } else {
                    triple.s.clone()
                };
                let p = if triple.p.is_var() {
                    VarOrTerm::new_encoded_term(
                        *result_bindings
                            .get(&triple.p.as_var().name)
                            .unwrap()
                            .get(counter)
                            .unwrap(),
                    )
                } else {
                    triple.p.clone()
                };
                let o = if triple.o.is_var() {
                    VarOrTerm::new_encoded_term(
                        *result_bindings
                            .get(&triple.o.as_var().name)
                            .unwrap()
                            .get(counter)
                            .unwrap(),
                    )
                } else {
                    triple.o.clone()
                };
                triples.push(Triple { s, p, o, g: None });
            }
            counter += 1;
            all_triples.push(triples);
        }
        all_triples
    }
}

#[cfg(test)]
#[path = "csprite_test.rs"]
mod csprite_test;
