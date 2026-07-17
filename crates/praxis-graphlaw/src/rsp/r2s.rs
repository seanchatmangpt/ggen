use std::collections::HashSet;
use std::hash::Hash;
use std::mem;

#[derive(Default)]
pub enum StreamOperator {
    #[default]
    RSTREAM,
    ISTREAM,
    DSTREAM,
}
pub struct Relation2StreamOperator<O> {
    stream_operator: StreamOperator,
    old_result: HashSet<O>,
    new_result: HashSet<O>,
    ts: usize,
}

impl<O> Relation2StreamOperator<O>
where
    O: Clone + Hash + Eq + Ord,
{
    pub fn new(stream_operator: StreamOperator, start_time: usize) -> Relation2StreamOperator<O> {
        match stream_operator {
            StreamOperator::RSTREAM => Relation2StreamOperator {
                stream_operator,
                old_result: HashSet::with_capacity(0),
                new_result: HashSet::with_capacity(0),
                ts: start_time,
            },
            _ => Relation2StreamOperator {
                stream_operator,
                old_result: HashSet::new(),
                new_result: HashSet::new(),
                ts: start_time,
            },
        }
    }
    pub fn eval(&mut self, new_response: Vec<O>, ts: usize) -> Vec<O> {
        match self.stream_operator {
            // RSTREAM returns `new_response` verbatim -- its order is whatever the caller's
            // own `Vec<O>` already had, not a `HashSet` iteration, so it's not this bug class.
            StreamOperator::RSTREAM => new_response,
            // ISTREAM filters the ORIGINAL `new_response` Vec (order-preserving from the
            // caller), only checking `self.old_result` via `.contains()` (a membership test,
            // order-insensitive) -- also not this bug class; `to_compare`'s iteration order is
            // never a HashSet's.
            StreamOperator::ISTREAM => {
                let to_compare = new_response.clone();
                self.prepare_compare(new_response, ts);
                to_compare
                    .into_iter()
                    .filter(|b| !self.old_result.contains(b))
                    .collect()
            }
            // DSTREAM (deletions since the last tick) is the one real instance of swarm finding
            // #22's bug class in this file: `to_compare` is a clone of `self.old_result`, a
            // `std::collections::HashSet<O>` (the same `RandomState`-hashed, per-process-
            // reseeded map type as #22, not even the narrower `FxHashMap` case of #23/#24), and
            // was iterated directly with no sort -- so the returned deletion order differed
            // across separate process runs of byte-identical input. `O` now requires `Ord`
            // (added to this impl block's bounds; the crate's only real instantiation, `Vec<
            // Binding>`, satisfies it now that `Binding` derives `Ord` -- see that derive's own
            // doc comment), so sorting here is unambiguous: a `Vec<Binding>` total order with
            // no ties to break arbitrarily.
            StreamOperator::DSTREAM => {
                self.prepare_compare(new_response, ts);
                let mut to_compare: Vec<O> = self
                    .old_result
                    .iter()
                    .filter(|b| !self.new_result.contains(*b))
                    .cloned()
                    .collect();
                to_compare.sort_unstable();
                to_compare
            }
        }
    }

    fn prepare_compare(&mut self, new_repsonse: Vec<O>, ts: usize) {
        if self.ts < ts {
            mem::swap(&mut self.new_result, &mut self.old_result);
            self.new_result.clear();
            self.ts = ts;
        }
        new_repsonse.into_iter().for_each(|v| {
            self.new_result.insert(v);
        });
    }
}
#[cfg(test)]
#[path = "r2s_test.rs"]
mod r2s_test;
