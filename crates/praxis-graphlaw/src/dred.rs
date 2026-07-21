// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::needless_range_loop)]

use crate::{
    Binding, QueryEngine, Reasoner, Rule, RuleIndex, SimpleQueryEngine, Triple, TripleIndex,
    TripleStore, VarOrTerm,
};
use log::trace; // Use log crate when building application

use crate::utils::Utils;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

pub struct DRed {
    pub rules: Vec<Rule>,
    pub rules_index: RuleIndex,
    pub triple_index: TripleIndex,
    reasoner: Reasoner,
}

impl Default for DRed {
    fn default() -> Self {
        Self::new()
    }
}

impl DRed {
    pub fn new() -> Self {
        Self {
            rules: Vec::new(),
            rules_index: RuleIndex::new(),
            triple_index: TripleIndex::new(),
            reasoner: Reasoner {},
        }
    }
    pub fn from(data: &str) -> Self {
        let triple_store = TripleStore::from(data);
        Self {
            rules: triple_store.rules,
            rules_index: triple_store.rules_index,
            triple_index: triple_store.triple_index,
            reasoner: Reasoner {},
        }
    }
    pub fn add(&mut self, triple: Triple) {
        trace! {"Adding triple: {:?}", Utils::decode_triple(&triple) }
        self.triple_index.add(triple);
    }
    pub fn add_ref(&mut self, triple: Rc<Triple>) {
        trace! {"Adding triple: {:?}", Utils::decode_triple(triple.as_ref()) }
        self.triple_index.add_ref(triple);
    }
    pub fn remove_ref(&mut self, triple: Rc<Triple>) {
        trace! {"Removing triple: {:?}", Utils::decode_triple(triple.as_ref()) }
        // over delete
        let mut over_deletion = Vec::new();
        let mut stack = Vec::from([triple.as_ref().clone()]);

        while let Some(current_triple) = stack.pop() {
            let matching_rules = self.rules_index.find_match(&current_triple);
            let matching_rules: Vec<Rule> = matching_rules
                .clone()
                .into_iter()
                .flat_map(|r| Reasoner::substitute_rule(&current_triple, r))
                .collect();

            let delete_triples =
                Reasoner::infer_rule_heads(&self.triple_index, None, matching_rules);
            delete_triples.into_iter().for_each(|t| {
                if !over_deletion.contains(&t) {
                    stack.push(t.clone());
                    over_deletion.push(t);
                }
            });
        }

        // delete overdeletion
        over_deletion
            .iter()
            .for_each(|t| self.triple_index.remove_ref(t));
        // delete E-
        self.triple_index.remove_ref(&triple);

        let mut delete_list: Vec<(Triple, bool)> =
            over_deletion.into_iter().map(|t| (t, false)).collect();
        // Rederivation step
        let mut delete_num = delete_list.len();
        let mut prev_delete_num = delete_num + 1;
        while delete_num < prev_delete_num {
            prev_delete_num = delete_num;
            for i in 0..delete_list.len() {
                let (delete_triple, delete_status) = &delete_list[i].clone();
                if !delete_status {
                    let matched_rules = Self::find_rules_by_head(&self.rules_index, delete_triple);
                    for (matched_rule, bindings) in matched_rules {
                        let substitute_rule =
                            Reasoner::substitute_rule_body_with_binding(&matched_rule, &bindings);
                        if SimpleQueryEngine::query(&self.triple_index, &substitute_rule, None)
                            .is_some()
                            && !self.triple_index.contains(delete_triple)
                        {
                            self.triple_index.add(delete_triple.clone());
                            delete_num -= 1;
                            delete_list[i].1 = true;
                            break;
                        }
                    }
                }
            }
        }
    }
    /// Find all rules whose head matches `head_triple`, returning each rule with bound variable mappings.
    /// Uses the head-predicate index for efficient O(1) lookup.
    pub(crate) fn find_rules_by_head(
        rules_index: &RuleIndex, head_triple: &Triple,
    ) -> Vec<(Arc<Rule>, Binding)> {
        let candidates: &[Arc<Rule>] = if head_triple.p.is_term() {
            // Fast path: look up only rules whose head predicate matches
            rules_index
                .head_by_pred
                .get(&head_triple.p.to_encoded())
                .map(|v| v.as_slice())
                .unwrap_or(&[])
        } else {
            // Variable predicate: must check all rules
            rules_index.rules.as_slice()
        };
        let mut rule_matches = Vec::new();
        for rule in candidates.iter() {
            let head: &Triple = &rule.head;
            let mut binding = Binding::new();
            if Self::eval_triple_element(&head.s, &head_triple.s, &mut binding)
                && Self::eval_triple_element(&head.p, &head_triple.p, &mut binding)
                && Self::eval_triple_element(&head.o, &head_triple.o, &mut binding)
            {
                rule_matches.push((rule.clone(), binding));
            }
        }
        rule_matches
    }
    /// Unify a rule head element with a query element, adding variable bindings.
    fn eval_triple_element(left: &VarOrTerm, right: &VarOrTerm, bindings: &mut Binding) -> bool {
        if let (VarOrTerm::Var(left_name), VarOrTerm::Term(right_name)) = (left, right) {
            bindings.add(&left_name.name, right_name.id());
            true
        } else {
            left.eq(right)
        }
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
}
#[cfg(test)]
mod test {
    use crate::dred::DRed;
    use crate::utils::Utils;
    use crate::{Triple, VarOrTerm};
    use std::rc::Rc;

    #[test]
    fn test() {
        let data = ":john :teaches :math.\n\
                :peter :teaches :math.\n\
                :john :teaches :physics.\n\
            {?s :teaches ?y.}=>{?s a :Person.}\n\
            {?s :teaches ?y.}=>{?y a :Course.}\n\
            {?s a :TA.}=>{?s a :Person.}\n\
            {?s a :Person.?s :teaches ?y.?y a :Course.}=>{?s a :TA.}";
        let mut dred = DRed::from(data);
        let inferred = dred.materialize();
        if let Ok(ref triples) = inferred {
            triples
                .iter()
                .for_each(|t| println!("{:?}", Utils::decode_triple(t)));
        }
        println!("{:?}", inferred);
        assert_eq!(9, dred.triple_index.len());

        let remove_triple = Triple {
            s: VarOrTerm::new_term(":john".to_string()),
            p: VarOrTerm::new_term(":teaches".to_string()),
            o: VarOrTerm::new_term(":math".to_string()),
            g: None,
        };

        dred.remove_ref(Rc::new(remove_triple));
        assert_eq!(8, dred.triple_index.len());
    }

    /// w3wenskxg dogfood audit (cycle 7): `remove_ref`'s rederivation pass
    /// (`find_rules_by_head` -> `substitute_rule_body_with_binding`) previously panicked when the
    /// matched rule's head was fully ground (no variables anywhere). `find_rules_by_head` only
    /// calls `Binding::add` for a `(Var, Term)` pair (`eval_triple_element`); a ground head
    /// matched via literal equality against a ground `head_triple` adds no bindings at all, so
    /// the resulting `Binding` has zero columns, not just zero rows.
    /// `substitute_triple_with_bindings`'s `for _ in 0..binding.len()` loop never executed for
    /// that empty binding, producing an empty `Vec`, and `substitute_rule_body_with_binding`'s
    /// `.first().unwrap()` panicked on it -- `dred.rs`'s own call site has no external
    /// `is_empty()` guard (unlike `substitute_rule`'s separate internal call a few lines above).
    #[test]
    fn remove_ref_does_not_panic_when_a_ground_head_rule_is_rederived() {
        let data = ":a :p :b.\n\
                {:a :p :b.}=>{:trigger a :Fired.}";
        let mut dred = DRed::from(data);
        dred.materialize()
            .expect("a fully ground rule must materialize cleanly");
        assert!(
            dred.triple_index.contains(&Triple {
                s: VarOrTerm::new_term(":trigger".to_string()),
                p: VarOrTerm::new_term("a".to_string()),
                o: VarOrTerm::new_term(":Fired".to_string()),
                g: None,
            }),
            "the ground rule must have derived :trigger a :Fired"
        );

        let remove_triple = Triple {
            s: VarOrTerm::new_term(":a".to_string()),
            p: VarOrTerm::new_term(":p".to_string()),
            o: VarOrTerm::new_term(":b".to_string()),
            g: None,
        };

        // Must not panic: rederiving :trigger a :Fired against the fully ground rule head
        // reaches `find_rules_by_head` -> `substitute_rule_body_with_binding` with an empty
        // (zero-column) `Binding`.
        dred.remove_ref(Rc::new(remove_triple));
    }
}
