// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::type_complexity)]

use crate::fastmap::FxHashMap;
use crate::{Rule, Triple};
// Arc, not Rc: RuleIndex is a field of TripleStore, and TripleStore genuinely
// crosses a real thread boundary (rsp.rs::RSPEngine::register_r2r spawns a
// thread holding a SimpleR2R { item: TripleStore } via Arc<Mutex<Box<dyn
// R2ROperator<..>: Send>>>). Rc's non-atomic refcount made the old
// `unsafe impl Send for TripleStore` unsound; Arc makes Send hold honestly.
use std::sync::Arc;

pub struct RuleIndex {
    pub rules: Vec<Arc<Rule>>,
    spo: Vec<Arc<Rule>>,
    s: FxHashMap<usize, Vec<Arc<Rule>>>,
    p: FxHashMap<usize, Vec<Arc<Rule>>>,
    o: FxHashMap<usize, Vec<Arc<Rule>>>,
    sp: FxHashMap<usize, FxHashMap<usize, Vec<Arc<Rule>>>>,
    po: FxHashMap<usize, FxHashMap<usize, Vec<Arc<Rule>>>>,
    so: FxHashMap<usize, FxHashMap<usize, Vec<Arc<Rule>>>>,
    spo_all: FxHashMap<usize, FxHashMap<usize, FxHashMap<usize, Vec<Arc<Rule>>>>>,
    /// Index from head predicate encoding -> rules whose head has that predicate.
    /// Used for fast backward-chaining rule lookup.
    pub head_by_pred: FxHashMap<usize, Vec<Arc<Rule>>>,
}

impl Default for RuleIndex {
    fn default() -> Self {
        Self::new()
    }
}

impl RuleIndex {
    pub fn len(&self) -> usize {
        self.spo.len()
            + self.s.len()
            + self.o.len()
            + self.p.len()
            + self.sp.len()
            + self.po.len()
            + self.so.len()
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn new() -> RuleIndex {
        RuleIndex {
            rules: Vec::new(),
            s: FxHashMap::default(),
            p: FxHashMap::default(),
            o: FxHashMap::default(),
            so: FxHashMap::default(),
            po: FxHashMap::default(),
            sp: FxHashMap::default(),
            spo: Vec::new(),
            spo_all: FxHashMap::default(),
            head_by_pred: FxHashMap::default(),
        }
    }
    fn add_rc(&mut self, rule: Arc<Rule>) {
        self.rules.push(rule.clone());
        // Index by head predicate for fast backward-chaining lookup
        self.head_by_pred
            .entry(rule.head.p.to_encoded())
            .or_default()
            .push(rule.clone());
        for body_lit in rule.body.iter() {
            if body_lit.negated {
                continue;
            }
            let Triple { s, p, o, .. } = &body_lit.pattern;
            //s match
            if s.is_term() && p.is_var() && o.is_var() {
                self.s.entry(s.to_encoded()).or_default();
                if let Some(rules) = self.s.get_mut(&s.to_encoded()) {
                    if !rules.contains(&rule) {
                        rules.push(rule.clone())
                    };
                }
                // self.s.get(&s.to_string()).unwrap().push(rule.clone());
            }
            //p match
            if s.is_var() && p.is_term() && o.is_var() {
                self.p.entry(p.to_encoded()).or_default();
                //self.p.get_mut(&p.to_string()).unwrap().push(rule.clone());
                if let Some(rules) = self.p.get_mut(&p.to_encoded()) {
                    if !rules.contains(&rule) {
                        rules.push(rule.clone())
                    };
                }
            }
            //o match
            if s.is_var() && p.is_var() && o.is_term() {
                self.o.entry(o.to_encoded()).or_default();
                //self.o.get_mut(&o.to_string()).unwrap().push(rule.clone());
                if let Some(rules) = self.o.get_mut(&o.to_encoded()) {
                    if !rules.contains(&rule) {
                        rules.push(rule.clone())
                    };
                }
            }
            //sp
            if s.is_term() && p.is_term() && o.is_var() {
                self.sp.entry(s.to_encoded()).or_default();
                if !self
                    .sp
                    .get(&s.to_encoded())
                    .unwrap()
                    .contains_key(&p.to_encoded())
                {
                    self.sp
                        .get_mut(&s.to_encoded())
                        .unwrap()
                        .insert(p.to_encoded(), Vec::new());
                }
                //self.sp.get_mut(&sp_str).unwrap().push(rule.clone());
                if let Some(rules) = self
                    .sp
                    .get_mut(&s.to_encoded())
                    .unwrap()
                    .get_mut(&p.to_encoded())
                {
                    if !rules.contains(&rule) {
                        rules.push(rule.clone())
                    };
                }
            }
            //so
            if s.is_term() && p.is_var() && o.is_term() {
                self.so.entry(s.to_encoded()).or_default();
                if !self
                    .so
                    .get(&s.to_encoded())
                    .unwrap()
                    .contains_key(&o.to_encoded())
                {
                    self.so
                        .get_mut(&s.to_encoded())
                        .unwrap()
                        .insert(o.to_encoded(), Vec::new());
                }
                //self.sp.get_mut(&sp_str).unwrap().push(rule.clone());
                if let Some(rules) = self
                    .so
                    .get_mut(&s.to_encoded())
                    .unwrap()
                    .get_mut(&o.to_encoded())
                {
                    if !rules.contains(&rule) {
                        rules.push(rule.clone())
                    };
                }
            }
            //po
            if s.is_var() && p.is_term() && o.is_term() {
                self.po.entry(p.to_encoded()).or_default();
                if !self
                    .po
                    .get(&p.to_encoded())
                    .unwrap()
                    .contains_key(&o.to_encoded())
                {
                    self.po
                        .get_mut(&p.to_encoded())
                        .unwrap()
                        .insert(o.to_encoded(), Vec::new());
                }
                //self.sp.get_mut(&sp_str).unwrap().push(rule.clone());
                if let Some(rules) = self
                    .po
                    .get_mut(&p.to_encoded())
                    .unwrap()
                    .get_mut(&o.to_encoded())
                {
                    if !rules.contains(&rule) {
                        rules.push(rule.clone())
                    };
                }
            }
            //spo
            if s.is_term() && p.is_term() && o.is_term() {
                self.spo_all.entry(s.to_encoded()).or_default();
                if !self
                    .spo_all
                    .get(&s.to_encoded())
                    .unwrap()
                    .contains_key(&p.to_encoded())
                {
                    self.spo_all
                        .get_mut(&s.to_encoded())
                        .unwrap()
                        .insert(p.to_encoded(), FxHashMap::default());
                }
                if !self
                    .spo_all
                    .get(&s.to_encoded())
                    .unwrap()
                    .get(&p.to_encoded())
                    .unwrap()
                    .contains_key(&o.to_encoded())
                {
                    self.spo_all
                        .get_mut(&s.to_encoded())
                        .unwrap()
                        .get_mut(&p.to_encoded())
                        .unwrap()
                        .insert(o.to_encoded(), Vec::new());
                }
                //self.sp.get_mut(&sp_str).unwrap().push(rule.clone());
                if let Some(rules) = self
                    .spo_all
                    .get_mut(&s.to_encoded())
                    .unwrap()
                    .get_mut(&p.to_encoded())
                    .unwrap()
                    .get_mut(&o.to_encoded())
                {
                    if !rules.contains(&rule) {
                        rules.push(rule.clone())
                    };
                }
            }
            //?s?p?o
            if s.is_var() && p.is_var() && o.is_var() {
                //self.spo.push(rule.clone());
                if !self.spo.contains(&rule) {
                    self.spo.push(rule.clone())
                };
            }
        }
    }
    pub fn add(&mut self, rule: Rule) {
        let clone_rule = Arc::new(rule);
        self.add_rc(clone_rule);
    }
    pub fn add_ref(&mut self, rule: &Rule) {
        let clone_rule = Arc::new(rule.clone());
        self.add_rc(clone_rule);
    }

    /// Fast lookup of rules by head predicate. Returns an empty slice if none found.
    pub fn find_by_head_pred(&self, pred_encoded: usize) -> &[Arc<Rule>] {
        self.head_by_pred
            .get(&pred_encoded)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    pub fn find_match(&self, triple: &Triple) -> Vec<&Rule> {
        let mut matched_triples: Vec<&Rule> = Vec::new();
        //check s
        if let Some(rule) = self.s.get(&triple.s.to_encoded()) {
            rule.iter().for_each(|r| matched_triples.push(r));
        }
        //check p
        if let Some(rule) = self.p.get(&triple.p.to_encoded()) {
            rule.iter().for_each(|r| matched_triples.push(r));
        }
        //check o
        if let Some(rule) = self.o.get(&triple.o.to_encoded()) {
            rule.iter().for_each(|r| matched_triples.push(r));
        }
        //check so
        if let Some(s_rules) = self.so.get(&triple.s.to_encoded()) {
            if let Some(rules) = s_rules.get(&triple.o.to_encoded()) {
                rules.iter().for_each(|r| matched_triples.push(r));
            }
        }
        //check po
        if let Some(p_rules) = self.po.get(&triple.p.to_encoded()) {
            if let Some(rules) = p_rules.get(&triple.o.to_encoded()) {
                rules.iter().for_each(|r| matched_triples.push(r));
            }
        }
        //check sp
        if let Some(s_rules) = self.sp.get(&triple.s.to_encoded()) {
            if let Some(rules) = s_rules.get(&triple.p.to_encoded()) {
                rules.iter().for_each(|r| matched_triples.push(r));
            }
        }
        //check spo
        if let Some(s_rules) = self.spo_all.get(&triple.s.to_encoded()) {
            if let Some(p_rules) = s_rules.get(&triple.p.to_encoded()) {
                if let Some(rules) = p_rules.get(&triple.o.to_encoded()) {
                    rules.iter().for_each(|r| matched_triples.push(r));
                }
            }
        }
        self.spo.iter().for_each(|r| matched_triples.push(r));

        matched_triples
    }
}
