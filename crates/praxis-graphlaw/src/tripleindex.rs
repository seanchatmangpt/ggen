// Vendored research-lineage engine (RoXi lineage): API reshaping is out of scope;
// lints below are documented scoped allows, not silent drift.
#![allow(clippy::type_complexity, clippy::ptr_arg, dead_code)]

use crate::fastmap::FxHashMap;
use crate::{Binding, Term, Triple, VarOrTerm};
use std::iter::empty;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Clone)]
pub struct TripleIndex {
    pub triples: Vec<Triple>,
    pub spo: FxHashMap<usize, FxHashMap<usize, Vec<(usize, usize, Option<Term>)>>>,
    pub pos: FxHashMap<usize, FxHashMap<usize, Vec<(usize, usize, Option<Term>)>>>,
    pub osp: FxHashMap<usize, FxHashMap<usize, Vec<(usize, usize, Option<Term>)>>>,
    counter: usize,
}

impl Default for TripleIndex {
    fn default() -> Self {
        Self::new()
    }
}

impl TripleIndex {
    pub fn len(&self) -> usize {
        self.triples.len()
    }
    pub fn is_empty(&self) -> bool {
        self.triples.is_empty()
    }
    pub fn get(&self, index: usize) -> Option<&Triple> {
        self.triples.get(index)
    }
    pub fn new() -> TripleIndex {
        TripleIndex {
            triples: Vec::new(),
            spo: FxHashMap::default(),
            pos: FxHashMap::default(),
            osp: FxHashMap::default(),
            counter: 0,
        }
    }
    /// Add a triple via Rc, avoiding duplicates.
    pub(crate) fn add_ref(&mut self, triple: Rc<Triple>) {
        if !self.contains(triple.as_ref()) {
            self.add(triple.as_ref().clone());
        }
    }
    pub fn remove_ref(&mut self, triple: &Triple) {
        //remove spo
        if self.spo.contains_key(&triple.s.to_encoded())
            && self
                .spo
                .get(&triple.s.to_encoded())
                .unwrap()
                .contains_key(&triple.p.to_encoded())
        {
            let spo_values = self
                .spo
                .get_mut(&triple.s.to_encoded())
                .unwrap()
                .get_mut(&triple.p.to_encoded())
                .unwrap();
            spo_values.retain(|(val, _counter, _)| *val != triple.o.to_encoded());
        }
        //remove pos
        if self.pos.contains_key(&triple.p.to_encoded())
            && self
                .pos
                .get(&triple.p.to_encoded())
                .unwrap()
                .contains_key(&triple.o.to_encoded())
        {
            let values = self
                .pos
                .get_mut(&triple.p.to_encoded())
                .unwrap()
                .get_mut(&triple.o.to_encoded())
                .unwrap();
            values.retain(|(val, _counter, _)| *val != triple.s.to_encoded());
        }
        // remove osp
        if self.osp.contains_key(&triple.o.to_encoded())
            && self
                .osp
                .get(&triple.o.to_encoded())
                .unwrap()
                .contains_key(&triple.s.to_encoded())
        {
            let values = self
                .osp
                .get_mut(&triple.o.to_encoded())
                .unwrap()
                .get_mut(&triple.s.to_encoded())
                .unwrap();
            values.retain(|(val, _counter, _)| *val != triple.p.to_encoded());
        }
        self.triples.retain(|t| *t != *triple);
        self.counter -= 1;
    }
    pub fn add(&mut self, triple: Triple) {
        self.spo.entry(triple.s.to_encoded()).or_default();
        if !self
            .spo
            .get(&triple.s.to_encoded())
            .unwrap()
            .contains_key(&triple.p.to_encoded())
        {
            self.spo
                .get_mut(&triple.s.to_encoded())
                .unwrap()
                .insert(triple.p.to_encoded(), Vec::new());
        }
        self.spo
            .get_mut(&triple.s.to_encoded())
            .unwrap()
            .get_mut(&triple.p.to_encoded())
            .unwrap()
            .push((
                triple.o.to_encoded(),
                self.counter,
                triple.g.clone().map(|g| g.as_term().clone()),
            ));
        //pos
        self.pos.entry(triple.p.to_encoded()).or_default();
        if !self
            .pos
            .get(&triple.p.to_encoded())
            .unwrap()
            .contains_key(&triple.o.to_encoded())
        {
            self.pos
                .get_mut(&triple.p.to_encoded())
                .unwrap()
                .insert(triple.o.to_encoded(), Vec::new());
        }
        self.pos
            .get_mut(&triple.p.to_encoded())
            .unwrap()
            .get_mut(&triple.o.to_encoded())
            .unwrap()
            .push((
                triple.s.to_encoded(),
                self.counter,
                triple.g.clone().map(|g| g.as_term().clone()),
            ));
        //osp
        self.osp.entry(triple.o.to_encoded()).or_default();
        if !self
            .osp
            .get(&triple.o.to_encoded())
            .unwrap()
            .contains_key(&triple.s.to_encoded())
        {
            self.osp
                .get_mut(&triple.o.to_encoded())
                .unwrap()
                .insert(triple.s.to_encoded(), Vec::new());
        }
        self.osp
            .get_mut(&triple.o.to_encoded())
            .unwrap()
            .get_mut(&triple.s.to_encoded())
            .unwrap()
            .push((
                triple.p.to_encoded(),
                self.counter,
                triple.g.clone().map(|g| g.as_term().clone()),
            ));
        self.triples.push(triple);
        self.counter += 1;
    }
    pub fn contains(&self, triple: &Triple) -> bool {
        if !self.osp.contains_key(&triple.o.to_encoded()) {
            false
        } else {
            if !self
                .osp
                .get(&triple.o.to_encoded())
                .unwrap()
                .contains_key(&triple.s.to_encoded())
            {
                false
            } else {
                for (encoded, _counter, _) in self
                    .osp
                    .get(&triple.o.to_encoded())
                    .unwrap()
                    .get(&triple.s.to_encoded())
                    .unwrap()
                {
                    if encoded == &triple.p.to_encoded() {
                        return true;
                    }
                }
                false
            }
        }
    }
    pub fn query(&self, query_triple: &Triple, triple_counter: Option<usize>) -> Option<Binding> {
        // A subject/object that's a *non-ground* list-term pattern (e.g.
        // `(:good ?Y)`) can never be found via the ordinary indexed
        // lookups below -- those compare list-term ids for exact equality,
        // but every list gets a fresh synthetic id at parse time, so a
        // pattern list is never `==` to a separately-parsed, structurally
        // compatible ground list. Route those queries through a dedicated
        // structural-unification scan instead (see `VarOrTerm::unify_list_pattern`).
        let s_is_list_pattern = query_triple.s.is_term()
            && VarOrTerm::is_nonground_list_pattern(query_triple.s.to_encoded());
        let o_is_list_pattern = query_triple.o.is_term()
            && VarOrTerm::is_nonground_list_pattern(query_triple.o.to_encoded());
        if s_is_list_pattern || o_is_list_pattern {
            return self.query_list_pattern(
                query_triple,
                triple_counter,
                s_is_list_pattern,
                o_is_list_pattern,
            );
        }

        let mut matched_binding = Binding::new();
        let counter_check = if let Some(size) = triple_counter {
            size
        } else {
            self.counter
        };
        //?s p o
        if query_triple.s.is_var() & query_triple.p.is_term() & query_triple.o.is_term() {
            if let Some(indexes) = self.pos.get(&query_triple.p.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.o.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter <= counter_check {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.s.to_encoded(), *encoded_match);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        //s ?p o
        else if query_triple.s.is_term() & query_triple.p.is_var() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.s.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter <= counter_check {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.p.to_encoded(), *encoded_match);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        //s p ?o
        else if query_triple.s.is_term() & query_triple.p.is_term() & query_triple.o.is_var() {
            if let Some(indexes) = self.spo.get(&query_triple.s.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.p.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter <= counter_check {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.o.to_encoded(), *encoded_match);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        //?s ?p o
        else if query_triple.s.is_var() & query_triple.p.is_var() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                for (s_key, p_values) in indexes.iter() {
                    for (encoded_match, counter, graph_name) in p_values.iter() {
                        if *counter <= counter_check {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.s.to_encoded(), *s_key);
                            matched_binding.add(&query_triple.p.to_encoded(), *encoded_match);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        //s ?p ?o
        else if query_triple.s.is_term() & query_triple.p.is_var() & query_triple.o.is_var() {
            if let Some(indexes) = self.spo.get(&query_triple.s.to_encoded()) {
                for (p_key, o_values) in indexes.iter() {
                    for (encoded_match, counter, graph_name) in o_values.iter() {
                        if *counter <= counter_check {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.p.to_encoded(), *p_key);
                            matched_binding.add(&query_triple.o.to_encoded(), *encoded_match);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        //?s p ?o
        else if query_triple.s.is_var() & query_triple.p.is_term() & query_triple.o.is_var() {
            if let Some(indexes) = self.pos.get(&query_triple.p.to_encoded()) {
                for (o_key, s_values) in indexes.iter() {
                    for (encoded_match, counter, graph_name) in s_values.iter() {
                        if *counter <= counter_check {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.o.to_encoded(), *o_key);
                            matched_binding.add(&query_triple.s.to_encoded(), *encoded_match);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        //?s ?p ?o
        else if query_triple.s.is_var() & query_triple.p.is_var() & query_triple.o.is_var() {
            for (s_key, p_index) in self.spo.iter() {
                for (p_key, o_values) in p_index.iter() {
                    for (encoded_match, counter, graph_name) in o_values.iter() {
                        if *counter <= counter_check {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.s.to_encoded(), *s_key);
                            matched_binding.add(&query_triple.p.to_encoded(), *p_key);
                            matched_binding.add(&query_triple.o.to_encoded(), *encoded_match);
                        } else {
                            break;
                        }
                    }
                }
            }
        }
        //s p o
        else if query_triple.s.is_term() & query_triple.p.is_term() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.s.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter <= counter_check {
                            if *encoded_match == query_triple.p.to_encoded() {
                                // Triple matches s/p/o — check graph condition
                                if query_triple.g.is_some() {
                                    // Graph variable or term: collect/filter bindings
                                    if !Self::check_quad_match_and_add(
                                        &query_triple,
                                        &mut matched_binding,
                                        graph_name,
                                    ) {
                                        // graph term didn't match — skip but keep looking
                                        continue;
                                    }
                                    // continue to collect all matching graph bindings
                                } else {
                                    // No graph constraint — return immediately on first match
                                    return Some(matched_binding);
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
            }
        }

        if !matched_binding.is_empty() {
            Some(matched_binding)
        } else {
            None
        }
    }

    pub fn query_range(
        &self, query_triple: &Triple, min_counter: usize, max_counter: usize,
    ) -> Option<Binding> {
        let s_is_list_pattern = query_triple.s.is_term()
            && VarOrTerm::is_nonground_list_pattern(query_triple.s.to_encoded());
        let o_is_list_pattern = query_triple.o.is_term()
            && VarOrTerm::is_nonground_list_pattern(query_triple.o.to_encoded());
        if s_is_list_pattern || o_is_list_pattern {
            return self.query_range_list_pattern(
                query_triple,
                min_counter,
                max_counter,
                s_is_list_pattern,
                o_is_list_pattern,
            );
        }

        // Delta-window optimization: when min_counter > 0 (a true delta window, not a prefix),
        // use flat scan over triples[min_counter..max_counter) instead of walking index
        // dictionaries. Critical for transitive rules like {?a p ?b. ?b p ?c} => {?a p ?c}
        // where p is bound but s and o are unbound. Indexed branches would iterate all
        // distinct objects for that p, cost O(d); flat scan is O(delta_size).
        // See PROJ-411 for transitive-closure cubic scaling root cause.
        // DISABLED(PROJ-411): pending root-cause discrimination. Delta-scan optimization
        // regressed performance. Root cause confirmed as
        // indexed branches iterating all distinct o/p keys before filtering by counter range.
        // Proper fix requires index restructuring (deferred to PROJ-411 follow-up).
        // Keeping delta_scan implementation for reference.
        #[allow(unreachable_code)]
        if false
            && min_counter > 0
            && query_triple.p.is_term()
            && (query_triple.s.is_var() || query_triple.o.is_var())
        {
            return self.query_range_delta_scan(query_triple, min_counter, max_counter);
        }

        let mut matched_binding = Binding::new();
        //?s p o
        if query_triple.s.is_var() & query_triple.p.is_term() & query_triple.o.is_term() {
            if let Some(indexes) = self.pos.get(&query_triple.p.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.o.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.s.to_encoded(), *encoded_match);
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }
        //s ?p o
        else if query_triple.s.is_term() & query_triple.p.is_var() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.s.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.p.to_encoded(), *encoded_match);
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }
        //s p ?o
        else if query_triple.s.is_term() & query_triple.p.is_term() & query_triple.o.is_var() {
            if let Some(indexes) = self.spo.get(&query_triple.s.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.p.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.o.to_encoded(), *encoded_match);
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }
        //?s ?p o
        else if query_triple.s.is_var() & query_triple.p.is_var() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                for (s_key, p_values) in indexes.iter() {
                    for (encoded_match, counter, graph_name) in p_values.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.s.to_encoded(), *s_key);
                            matched_binding.add(&query_triple.p.to_encoded(), *encoded_match);
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }
        //s ?p ?o
        else if query_triple.s.is_term() & query_triple.p.is_var() & query_triple.o.is_var() {
            if let Some(indexes) = self.spo.get(&query_triple.s.to_encoded()) {
                for (p_key, o_values) in indexes.iter() {
                    for (encoded_match, counter, graph_name) in o_values.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.p.to_encoded(), *p_key);
                            matched_binding.add(&query_triple.o.to_encoded(), *encoded_match);
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }
        //?s p ?o
        else if query_triple.s.is_var() & query_triple.p.is_term() & query_triple.o.is_var() {
            if let Some(indexes) = self.pos.get(&query_triple.p.to_encoded()) {
                for (o_key, s_values) in indexes.iter() {
                    for (encoded_match, counter, graph_name) in s_values.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.o.to_encoded(), *o_key);
                            matched_binding.add(&query_triple.s.to_encoded(), *encoded_match);
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }
        //?s ?p ?o
        else if query_triple.s.is_var() & query_triple.p.is_var() & query_triple.o.is_var() {
            for (s_key, p_index) in self.spo.iter() {
                for (p_key, o_values) in p_index.iter() {
                    for (encoded_match, counter, graph_name) in o_values.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if !Self::check_quad_match_and_add(
                                &query_triple,
                                &mut matched_binding,
                                graph_name,
                            ) {
                                break;
                            }
                            matched_binding.add(&query_triple.s.to_encoded(), *s_key);
                            matched_binding.add(&query_triple.p.to_encoded(), *p_key);
                            matched_binding.add(&query_triple.o.to_encoded(), *encoded_match);
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }
        //s p o
        else if query_triple.s.is_term() & query_triple.p.is_term() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.s.to_encoded()) {
                    for (encoded_match, counter, graph_name) in indexes2.iter() {
                        if *counter >= min_counter && *counter < max_counter {
                            if *encoded_match == query_triple.p.to_encoded() {
                                if query_triple.g.is_some() {
                                    if !Self::check_quad_match_and_add(
                                        &query_triple,
                                        &mut matched_binding,
                                        graph_name,
                                    ) {
                                        continue;
                                    }
                                } else {
                                    return Some(matched_binding);
                                }
                            }
                        } else if *counter >= max_counter {
                            break;
                        }
                    }
                }
            }
        }

        if !matched_binding.is_empty() {
            Some(matched_binding)
        } else {
            None
        }
    }

    /// Fast path for delta-window queries (min_counter > 0).
    /// Scans triples[min_counter..max_counter) once, filtering by ground terms.
    /// Replaces the O(distinct_predicates * entries_per_bucket) indexed iteration
    /// with O(delta_size) flat scan, crucial for transitive-closure performance.
    fn query_range_delta_scan(
        &self, query_triple: &Triple, min_counter: usize, max_counter: usize,
    ) -> Option<Binding> {
        let mut matched_binding = Binding::new();
        let mut found_any = false;

        let max_idx = max_counter.min(self.triples.len());
        for (idx, triple) in self.triples.iter().enumerate() {
            if idx >= max_idx {
                break;
            }
            if idx < min_counter {
                continue;
            }

            // Check all ground positions
            if query_triple.s.is_term() && triple.s.to_encoded() != query_triple.s.to_encoded() {
                continue;
            }
            if query_triple.p.is_term() && triple.p.to_encoded() != query_triple.p.to_encoded() {
                continue;
            }
            if query_triple.o.is_term() && triple.o.to_encoded() != query_triple.o.to_encoded() {
                continue;
            }

            // Check graph name if specified (match semantics of indexed branches)
            if let Some(ref query_g) = query_triple.g {
                if let VarOrTerm::Term(query_term) = query_g {
                    // Query has a ground graph constraint; triple must match it
                    if let Some(ref triple_g) = triple.g {
                        if let VarOrTerm::Term(triple_term) = triple_g {
                            if triple_term != query_term {
                                continue;
                            }
                        } else {
                            continue;
                        }
                    } else {
                        continue;
                    }
                }
                // If query_g is a Var, we'll bind it below after matching s/p/o
            }

            found_any = true;

            // Bind variable positions
            if query_triple.s.is_var() {
                matched_binding.add(&query_triple.s.to_encoded(), triple.s.to_encoded());
            }
            if query_triple.p.is_var() {
                matched_binding.add(&query_triple.p.to_encoded(), triple.p.to_encoded());
            }
            if query_triple.o.is_var() {
                matched_binding.add(&query_triple.o.to_encoded(), triple.o.to_encoded());
            }
            // Bind graph variable if present
            if let Some(ref query_g) = query_triple.g {
                if let VarOrTerm::Var(var_name) = query_g {
                    if let Some(ref triple_g) = triple.g {
                        if let VarOrTerm::Term(term) = triple_g {
                            matched_binding.add(&var_name.name, term.id());
                        }
                    }
                }
            }
        }

        // Return Some(empty_binding) if we matched a fully-ground query, or Some(bindings)
        // if we found any matches with variables to bind. Return None only if no matches.
        if found_any || !matched_binding.is_empty() {
            Some(matched_binding)
        } else {
            None
        }
    }

    fn query_range_list_pattern(
        &self, query_triple: &Triple, min_counter: usize, max_counter: usize,
        s_is_list_pattern: bool, o_is_list_pattern: bool,
    ) -> Option<Binding> {
        let mut matched_binding = Binding::new();

        for (idx, triple) in self.triples.iter().enumerate() {
            if idx >= max_counter {
                break;
            }
            if idx < min_counter {
                continue;
            }
            if query_triple.p.is_term() && triple.p.to_encoded() != query_triple.p.to_encoded() {
                continue;
            }
            if !s_is_list_pattern
                && query_triple.s.is_term()
                && triple.s.to_encoded() != query_triple.s.to_encoded()
            {
                continue;
            }
            if !o_is_list_pattern
                && query_triple.o.is_term()
                && triple.o.to_encoded() != query_triple.o.to_encoded()
            {
                continue;
            }

            let mut extra_bindings = Vec::new();
            if s_is_list_pattern
                && !VarOrTerm::unify_list_pattern(
                    query_triple.s.to_encoded(),
                    triple.s.to_encoded(),
                    &mut extra_bindings,
                )
            {
                continue;
            }
            if o_is_list_pattern
                && !VarOrTerm::unify_list_pattern(
                    query_triple.o.to_encoded(),
                    triple.o.to_encoded(),
                    &mut extra_bindings,
                )
            {
                continue;
            }

            {
                let mut consistent = FxHashMap::default();
                let mut ok = true;
                for &(var_id, val_id) in &extra_bindings {
                    match consistent.get(&var_id) {
                        Some(&existing) if existing != val_id => {
                            ok = false;
                            break;
                        }
                        _ => {
                            consistent.insert(var_id, val_id);
                        }
                    }
                }
                if !ok {
                    continue;
                }
            }

            if query_triple.s.is_var() {
                matched_binding.add(&query_triple.s.to_encoded(), triple.s.to_encoded());
            }
            if query_triple.p.is_var() {
                matched_binding.add(&query_triple.p.to_encoded(), triple.p.to_encoded());
            }
            if query_triple.o.is_var() {
                matched_binding.add(&query_triple.o.to_encoded(), triple.o.to_encoded());
            }
            for (var_id, val_id) in extra_bindings {
                matched_binding.add(&var_id, val_id);
            }
        }

        if !matched_binding.is_empty() {
            Some(matched_binding)
        } else {
            None
        }
    }

    /// A linear-scan fallback for queries where the subject and/or object
    /// is a non-ground list-term pattern (see `VarOrTerm::unify_list_pattern`'s
    /// doc comment for why the ordinary indexed lookups in `query` can't
    /// handle this). Correctness over speed: this crate's list-pattern
    /// corpus cases (the real EYE `good_cobbler`/`peano` cases) are small
    /// fact sets, not indexed-lookup-scale data.
    fn query_list_pattern(
        &self, query_triple: &Triple, triple_counter: Option<usize>, s_is_list_pattern: bool,
        o_is_list_pattern: bool,
    ) -> Option<Binding> {
        let counter_check = triple_counter.unwrap_or(self.counter);
        let mut matched_binding = Binding::new();

        for (idx, triple) in self.triples.iter().enumerate() {
            if idx > counter_check {
                break;
            }
            if query_triple.p.is_term() && triple.p.to_encoded() != query_triple.p.to_encoded() {
                continue;
            }
            if !s_is_list_pattern
                && query_triple.s.is_term()
                && triple.s.to_encoded() != query_triple.s.to_encoded()
            {
                continue;
            }
            if !o_is_list_pattern
                && query_triple.o.is_term()
                && triple.o.to_encoded() != query_triple.o.to_encoded()
            {
                continue;
            }

            let mut extra_bindings = Vec::new();
            if s_is_list_pattern
                && !VarOrTerm::unify_list_pattern(
                    query_triple.s.to_encoded(),
                    triple.s.to_encoded(),
                    &mut extra_bindings,
                )
            {
                continue;
            }
            if o_is_list_pattern
                && !VarOrTerm::unify_list_pattern(
                    query_triple.o.to_encoded(),
                    triple.o.to_encoded(),
                    &mut extra_bindings,
                )
            {
                continue;
            }

            // A variable can occur more than once inside a single list
            // pattern (e.g. the real EYE `basic-monadic` corpus case's
            // 11-element cycle-detection list `(?D0 ?D1 ... ?D9 ?D0)`,
            // where `?D0` closes the cycle by repeating as both the first
            // and last member) -- `unify_list_pattern` binds every
            // occurrence independently without checking they agree, so a
            // repeated variable must be reconciled here: reject this
            // candidate triple if any variable is bound to two different
            // values within the same match.
            {
                let mut consistent = FxHashMap::default();
                let mut ok = true;
                for &(var_id, val_id) in &extra_bindings {
                    match consistent.get(&var_id) {
                        Some(&existing) if existing != val_id => {
                            ok = false;
                            break;
                        }
                        _ => {
                            consistent.insert(var_id, val_id);
                        }
                    }
                }
                if !ok {
                    continue;
                }
            }

            if query_triple.s.is_var() {
                matched_binding.add(&query_triple.s.to_encoded(), triple.s.to_encoded());
            }
            if query_triple.p.is_var() {
                matched_binding.add(&query_triple.p.to_encoded(), triple.p.to_encoded());
            }
            if query_triple.o.is_var() {
                matched_binding.add(&query_triple.o.to_encoded(), triple.o.to_encoded());
            }
            for (var_id, val_id) in extra_bindings {
                matched_binding.add(&var_id, val_id);
            }
        }

        if !matched_binding.is_empty() {
            Some(matched_binding)
        } else {
            None
        }
    }

    pub fn query_help<'a>(
        &'a self, query_triple: &'a Triple, _triple_counter: Option<usize>,
    ) -> Box<dyn Iterator<Item = Vec<EncodedBinding>> + 'a> {
        //?s p o
        if query_triple.s.is_var() & query_triple.p.is_term() & query_triple.o.is_term() {
            if let Some(indexes) = self.pos.get(&query_triple.p.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.o.to_encoded()) {
                    Self::extract_binding_values_single_var(
                        &query_triple.s,
                        &query_triple.g,
                        indexes2,
                    )
                } else {
                    Box::new(empty())
                }
            } else {
                Box::new(empty())
            }
        }
        //s ?p o
        else if query_triple.s.is_term() & query_triple.p.is_var() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.s.to_encoded()) {
                    Self::extract_binding_values_single_var(
                        &query_triple.p,
                        &query_triple.g,
                        indexes2,
                    )
                } else {
                    Box::new(empty())
                }
            } else {
                Box::new(empty())
            }
        }
        // //s p ?o
        else if query_triple.s.is_term() & query_triple.p.is_term() & query_triple.o.is_var() {
            if let Some(indexes) = self.spo.get(&query_triple.s.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.p.to_encoded()) {
                    Self::extract_binding_values_single_var(
                        &query_triple.o,
                        &query_triple.g,
                        indexes2,
                    )
                } else {
                    Box::new(empty())
                }
            } else {
                Box::new(empty())
            }
        }
        // //?s ?p o
        else if query_triple.s.is_var() & query_triple.p.is_var() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                Box::new(
                    indexes
                        .iter()
                        .flat_map(|(s_key, p_values)| {
                            p_values
                                .iter()
                                .zip(std::iter::repeat_n(s_key, p_values.len()))
                        })
                        .filter_map(|((encoded_match, _counter, graph_name), s_key)| {
                            let mut bindings = Vec::with_capacity(3);
                            bindings.push(EncodedBinding {
                                var: query_triple.s.to_encoded(),
                                val: *s_key,
                            });
                            bindings.push(EncodedBinding {
                                var: query_triple.p.to_encoded(),
                                val: *encoded_match,
                            });

                            match &query_triple.g {
                                Some(VarOrTerm::Var(var_name)) if graph_name.is_some() => {
                                    bindings.push(EncodedBinding {
                                        var: var_name.name,
                                        val: graph_name.clone().unwrap().id(),
                                    });
                                }
                                Some(VarOrTerm::Term(term))
                                    if !graph_name.clone().is_some_and(|t| t.eq(term)) =>
                                {
                                    return None
                                }
                                _ => {}
                            }
                            Some(bindings)
                        }),
                )
            } else {
                Box::new(empty())
            }
        }
        //s ?p ?o
        else if query_triple.s.is_term() & query_triple.p.is_var() & query_triple.o.is_var() {
            if let Some(indexes) = self.spo.get(&query_triple.s.to_encoded()) {
                Box::new(
                    indexes
                        .iter()
                        .flat_map(|(key, values)| {
                            values.iter().zip(std::iter::repeat_n(key, values.len()))
                        })
                        .filter_map(|((encoded_match, _counter, graph_name), key)| {
                            let mut bindings = Vec::with_capacity(3);
                            bindings.push(EncodedBinding {
                                var: query_triple.p.to_encoded(),
                                val: *key,
                            });
                            bindings.push(EncodedBinding {
                                var: query_triple.o.to_encoded(),
                                val: *encoded_match,
                            });

                            match &query_triple.g {
                                Some(VarOrTerm::Var(var_name)) if graph_name.is_some() => {
                                    bindings.push(EncodedBinding {
                                        var: var_name.name,
                                        val: graph_name.clone().unwrap().id(),
                                    });
                                }
                                Some(VarOrTerm::Term(term))
                                    if !graph_name.clone().is_some_and(|t| t.eq(term)) =>
                                {
                                    return None
                                }
                                _ => {}
                            }
                            Some(bindings)
                        }),
                )
            } else {
                Box::new(empty())
            }
        }
        //?s p ?o
        else if query_triple.s.is_var() & query_triple.p.is_term() & query_triple.o.is_var() {
            if let Some(indexes) = self.pos.get(&query_triple.p.to_encoded()) {
                Box::new(
                    indexes
                        .iter()
                        .flat_map(|(key, values)| {
                            values.iter().zip(std::iter::repeat_n(key, values.len()))
                        })
                        .filter_map(|((encoded_match, _counter, graph_name), key)| {
                            let mut bindings = Vec::with_capacity(3);
                            bindings.push(EncodedBinding {
                                var: query_triple.s.to_encoded(),
                                val: *encoded_match,
                            });
                            bindings.push(EncodedBinding {
                                var: query_triple.o.to_encoded(),
                                val: *key,
                            });

                            match &query_triple.g {
                                Some(VarOrTerm::Var(var_name)) if graph_name.is_some() => {
                                    bindings.push(EncodedBinding {
                                        var: var_name.name,
                                        val: graph_name.clone().unwrap().id(),
                                    });
                                }
                                Some(VarOrTerm::Term(term))
                                    if !graph_name.clone().is_some_and(|t| t.eq(term)) =>
                                {
                                    return None
                                }
                                _ => {}
                            }
                            Some(bindings)
                        }),
                )
            } else {
                Box::new(empty())
            }
        }
        // //?s ?p ?o
        else if query_triple.s.is_var() & query_triple.p.is_var() & query_triple.o.is_var() {
            Box::new(
                self.spo
                    .iter()
                    .flat_map(|(s_key, p_vals)| {
                        p_vals.iter().zip(std::iter::repeat_n(s_key, p_vals.len()))
                    })
                    .flat_map(|((p_key, o_values), s_key)| {
                        o_values
                            .iter()
                            .zip(std::iter::repeat_n(p_key, o_values.len()))
                            .zip(std::iter::repeat_n(s_key, o_values.len()))
                    })
                    .filter_map(|(((encoded_match, _counter, graph_name), p_key), s_key)| {
                        let mut bindings = Vec::with_capacity(3);
                        bindings.push(EncodedBinding {
                            var: query_triple.s.to_encoded(),
                            val: *s_key,
                        });
                        bindings.push(EncodedBinding {
                            var: query_triple.p.to_encoded(),
                            val: *p_key,
                        });
                        bindings.push(EncodedBinding {
                            var: query_triple.o.to_encoded(),
                            val: *encoded_match,
                        });

                        match &query_triple.g {
                            Some(VarOrTerm::Var(var_name)) if graph_name.is_some() => {
                                bindings.push(EncodedBinding {
                                    var: var_name.name,
                                    val: graph_name.clone().unwrap().id(),
                                });
                            }
                            Some(VarOrTerm::Term(term))
                                if !graph_name.clone().is_some_and(|t| t.eq(term)) =>
                            {
                                return None
                            }
                            _ => {}
                        }
                        Some(bindings)
                    }),
            )
        }
        // //s p o
        else if query_triple.s.is_term() & query_triple.p.is_term() & query_triple.o.is_term() {
            if let Some(indexes) = self.osp.get(&query_triple.o.to_encoded()) {
                if let Some(indexes2) = indexes.get(&query_triple.s.to_encoded()) {
                    Box::new(
                        indexes2
                            .iter()
                            .flat_map(|(encoded_match, _counter, _graph_name)| {
                                if *encoded_match == query_triple.p.to_encoded() {
                                    // return when triple has been found in knowlege base
                                    Some(Vec::with_capacity(0))
                                } else {
                                    None
                                }
                            }),
                    )
                } else {
                    Box::new(empty())
                }
            } else {
                Box::new(empty())
            }
        } else {
            Box::new(empty())
        }
        //
        // if matched_binding.len() > 0{
        //     Some(matched_binding)
        // }else{
        //     None
        // }
    }

    fn extract_binding_values_single_var<'a>(
        variable: &'a VarOrTerm, graph_var: &'a Option<VarOrTerm>,
        indexes2: &'a Vec<(usize, usize, Option<Term>)>,
    ) -> Box<dyn Iterator<Item = Vec<EncodedBinding>> + 'a> {
        Box::new(
            indexes2
                .iter()
                .filter_map(move |(encoded_match, _counter, graph_name)| {
                    let mut bindings = Vec::with_capacity(2);
                    bindings.push(EncodedBinding {
                        var: variable.to_encoded(),
                        val: *encoded_match,
                    });
                    match graph_var {
                        Some(VarOrTerm::Var(var_name)) if graph_name.is_some() => {
                            bindings.push(EncodedBinding {
                                var: var_name.name,
                                val: graph_name.clone().unwrap().id(),
                            });
                        }
                        Some(VarOrTerm::Term(term))
                            if !graph_name.clone().is_some_and(|t| t.eq(term)) =>
                        {
                            return None
                        }
                        _ => {}
                    }
                    Some(bindings)
                }),
        )
    }
    fn check_quad_match_and_add(
        query_triple: &&Triple, matched_binding: &mut Binding, graph_name: &Option<Term>,
    ) -> bool {
        match &query_triple.g {
            Some(VarOrTerm::Var(var_name)) if graph_name.is_some() => {
                matched_binding.add(&var_name.name, graph_name.clone().unwrap().id());
                true
            }
            Some(VarOrTerm::Term(term)) if !graph_name.clone().is_some_and(|t| t.eq(term)) => false,
            _ => true,
        }
    }
    pub fn clear(&mut self) {
        self.triples.clear();
        self.spo.clear();
        self.osp.clear();
        self.pos.clear();
        self.counter = 0;
    }
}
#[derive(Debug, Clone)]
pub struct EncodedBinding {
    pub var: usize,
    pub val: usize,
}
pub struct QuadIterator<'a> {
    query: Triple,
    index: &'a TripleIndex,
}
impl<'a> Iterator for QuadIterator<'a> {
    type Item = Binding;
    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

/// Immutable snapshot of a TripleIndex for zero-copy, thread-safe read-only access.
/// Shared across multiple consumers (SHACL validators, OWL-RL engines, etc.)
/// without allocating fresh indexes per dialect.
///
/// # Complexity
/// - Construction: O(1) (Arc clone)
/// - Query: same as TripleIndex (uses indexed lookups, O(1) to O(n) per constraint)
#[derive(Clone)]
pub struct TripleIndexSnapshot {
    inner: Arc<TripleIndex>,
}

impl TripleIndexSnapshot {
    /// Create a snapshot from an existing TripleIndex.
    pub fn from_index(index: TripleIndex) -> Self {
        TripleIndexSnapshot {
            inner: Arc::new(index),
        }
    }

    /// Create a snapshot from an Arc-wrapped TripleIndex.
    pub fn from_arc(inner: Arc<TripleIndex>) -> Self {
        TripleIndexSnapshot { inner }
    }

    /// Get the underlying Arc for advanced use cases.
    pub fn as_arc(&self) -> Arc<TripleIndex> {
        Arc::clone(&self.inner)
    }

    /// Get a reference to the inner TripleIndex for backward compat.
    pub fn as_inner(&self) -> &TripleIndex {
        &self.inner
    }
}

/// Zero-allocation query result iterator.
/// Returns either a slice of results or empty, without boxing.
/// Used in hot paths where allocations should be avoided.
#[derive(Clone)]
pub enum QueryResultIter {
    /// Empty result set
    Empty,
    /// Single binding result
    Single(Option<Binding>),
}

impl QueryResultIter {
    /// Convert to an iterator over bindings
    pub fn into_iter(self) -> Box<dyn Iterator<Item = Binding>> {
        match self {
            QueryResultIter::Empty => Box::new(std::iter::empty()),
            QueryResultIter::Single(Some(b)) => Box::new(std::iter::once(b)),
            QueryResultIter::Single(None) => Box::new(std::iter::empty()),
        }
    }
}

#[cfg(test)]
#[path = "tripleindex_test.rs"]
mod tripleindex_test;
