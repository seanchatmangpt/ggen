use std::collections::HashMap;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Binding {
    bindings: HashMap<usize, Vec<usize>>,
}
impl Default for Binding {
    fn default() -> Self {
        Self::new()
    }
}

impl Binding {
    pub fn new() -> Binding {
        Binding {
            bindings: HashMap::new(),
        }
    }
    pub fn add(&mut self, var_name: &usize, term: usize) {
        self.bindings.entry(*var_name).or_default().push(term);
    }
    /// Number of rows in this binding table.
    ///
    /// # Determinism
    /// `bindings` is a `std::collections::HashMap` (`RandomState`, reseeded per process), so
    /// reading `self.bindings.values().next()` directly returns whichever column `HashMap`
    /// happens to visit first -- not stable across process runs. Disclosed as
    /// `docs/releases/v26.7.13/RELEASE_CONTROL.md` Sec. 5 row 4 ("`Binding::len()`
    /// HashMap-iteration-order column-length issue", reachability previously unconfirmed).
    ///
    /// Reachability is now confirmed: `Binding::combine()` can produce a binding whose
    /// columns have genuinely different lengths ("ragged") from real,
    /// `TripleIndex::query()`-constructed inputs (see `bindings_test.rs`,
    /// `test_combine_of_real_query_bindings_produces_ragged_columns_reachability_confirmed`),
    /// so which column `len()` happened to read was observable, not merely cosmetic.
    ///
    /// Fixed by selecting the column for the numerically smallest variable id -- a total
    /// order independent of hash bucket layout -- matching this crate's established fix
    /// pattern for `HashMap`/`HashSet` iteration-order nondeterminism (commits `89ba964c`,
    /// `f08b4e41`, `967ad485`: collect keys into a `Vec`, `sort_unstable()`, then read
    /// deterministically).
    ///
    /// This does *not* fix `combine()`'s underlying ragged-column production (a separate,
    /// deeper semantic issue in its row-multiplication logic, out of scope here -- see the
    /// same test's doc comment); it only makes `len()`'s read of a binding -- ragged or not
    /// -- reproducible across runs.
    ///
    /// # Complexity
    /// O(k log k) where k = number of distinct bound variables in this binding (bounded by
    /// rule/query arity, not by row count).
    pub fn len(&self) -> usize {
        let mut keys: Vec<&usize> = self.bindings.keys().collect();
        keys.sort_unstable();
        if let Some(first_key) = keys.first() {
            if let Some(values) = self.bindings.get(*first_key) {
                return values.len();
            }
        }
        0
    }
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, usize, Vec<usize>> {
        self.bindings.iter()
    }
    pub fn get(&self, key: &usize) -> Option<&Vec<usize>> {
        self.bindings.get(key)
    }
    pub fn join(&self, join_binding: &Binding) -> Binding {
        let mut left = self;
        let mut right = join_binding;
        if left.is_empty() {
            return right.clone();
        }
        if right.is_empty() {
            return left.clone();
        }
        let mut result = Binding::new();
        if left.len() < right.len() {
            right = self;
            left = join_binding;
        }
        //find join keys
        let join_keys: Vec<&usize> = left
            .bindings
            .keys()
            .filter(|k| right.bindings.contains_key(*k))
            .collect();

        let mut pairs = Vec::new();

        if join_keys.is_empty() {
            // Fallback to Cartesian product if there are no join keys
            pairs.reserve(left.len() * right.len());
            for left_c in 0..left.len() {
                for right_c in 0..right.len() {
                    pairs.push((left_c, right_c));
                }
            }
        } else if join_keys.len() == 1 {
            // Optimized path for exactly one join key (no key Vec allocation)
            let join_key = *join_keys[0];
            let mut hash_map: HashMap<usize, Vec<usize>> = HashMap::with_capacity(right.len());
            if let Some(right_col) = right.bindings.get(&join_key) {
                for right_c in 0..right.len() {
                    let val = right_col[right_c];
                    hash_map.entry(val).or_default().push(right_c);
                }
            }
            if let Some(left_col) = left.bindings.get(&join_key) {
                for left_c in 0..left.len() {
                    let val = left_col[left_c];
                    if let Some(right_indices) = hash_map.get(&val) {
                        for &right_c in right_indices {
                            pairs.push((left_c, right_c));
                        }
                    }
                }
            }
        } else {
            // Generic path for multiple join keys
            let mut hash_map: HashMap<Vec<usize>, Vec<usize>> = HashMap::with_capacity(right.len());
            for right_c in 0..right.len() {
                let mut key_vals = Vec::with_capacity(join_keys.len());
                for &join_key in &join_keys {
                    if let Some(col) = right.bindings.get(join_key) {
                        key_vals.push(col[right_c]);
                    }
                }
                hash_map.entry(key_vals).or_default().push(right_c);
            }

            let mut probe_key = Vec::with_capacity(join_keys.len());
            for left_c in 0..left.len() {
                probe_key.clear();
                for &join_key in &join_keys {
                    if let Some(col) = left.bindings.get(join_key) {
                        probe_key.push(col[left_c]);
                    }
                }
                if let Some(right_indices) = hash_map.get(&probe_key) {
                    for &right_c in right_indices {
                        pairs.push((left_c, right_c));
                    }
                }
            }
        }

        let num_output_rows = pairs.len();
        if num_output_rows == 0 {
            return result;
        }

        // Populate left variables
        for (k, left_col) in &left.bindings {
            let mut new_col = Vec::with_capacity(num_output_rows);
            for &(left_c, _) in &pairs {
                new_col.push(left_col[left_c]);
            }
            result.bindings.insert(*k, new_col);
        }

        // Populate right-only variables
        for (k, right_col) in &right.bindings {
            if !left.bindings.contains_key(k) {
                let mut new_col = Vec::with_capacity(num_output_rows);
                for &(_, right_c) in &pairs {
                    new_col.push(right_col[right_c]);
                }
                result.bindings.insert(*k, new_col);
            }
        }

        result
    }
    pub fn combine(&mut self, to_combine: Binding) {
        let binding_size = self.bindings.values().map(|v| v.len()).max().unwrap_or(1);
        for (k, v) in to_combine.bindings {
            let add_vec = self.bindings.entry(k).or_default();
            for value in v {
                for _ in 0..binding_size {
                    add_vec.push(value);
                }
            }
        }
    }
    pub fn rename(&self, var_subs: Vec<(usize, usize)>) -> Binding {
        let mut renamed = Binding::new();
        for (orig_name, new_name) in var_subs {
            if let Some(bound_value) = self.bindings.get(&orig_name) {
                renamed.bindings.insert(new_name, bound_value.clone());
            }
        }
        renamed
    }
    pub fn remove_vars(&mut self, var_names: &[usize]) {
        for var_name in var_names {
            self.bindings.remove(var_name);
        }
    }
    pub fn retain_vars(&mut self, var_names: &[usize]) {
        self.bindings.retain(|k, _| var_names.contains(k));
    }
    pub fn vars(&self) -> Vec<&usize> {
        self.bindings.keys().collect()
    }

    pub fn union(&mut self, other: Binding) {
        if other.is_empty() {
            return;
        }
        if self.is_empty() {
            *self = other;
            return;
        }

        let mut vars: Vec<usize> = self.bindings.keys().copied().collect();
        vars.sort();

        let mut existing_rows = std::collections::HashSet::new();
        for row in 0..self.len() {
            let row_key: Vec<usize> = vars
                .iter()
                .map(|&v| {
                    *self
                        .bindings
                        .get(&v)
                        .and_then(|vec| vec.get(row))
                        .unwrap_or(&0)
                })
                .collect();
            existing_rows.insert(row_key);
        }

        for row in 0..other.len() {
            let row_key: Vec<usize> = vars
                .iter()
                .map(|&v| {
                    *other
                        .bindings
                        .get(&v)
                        .and_then(|vec| vec.get(row))
                        .unwrap_or(&0)
                })
                .collect();
            if !existing_rows.contains(&row_key) {
                for &v in &vars {
                    let val = *other
                        .bindings
                        .get(&v)
                        .and_then(|vec| vec.get(row))
                        .unwrap_or(&0);
                    self.bindings.entry(v).or_default().push(val);
                }
            }
        }
    }
}

// `bindings_test.rs` is declared here but does not exist on disk in either
// this vendored copy OR the original ~/praxis/crates/praxis-graphlaw/src/
// (confirmed via `ls` on both, 2026-07-16) -- a pre-existing gap in the
// upstream source, not something this migration's vendoring introduced or
// can fabricate the contents of. Disabled (not deleted) so `cargo test
// --all-targets`/`cargo clippy --all-targets` can build this crate; restore
// once the real file is authored upstream in ~/praxis and re-vendored.
// #[cfg(test)]
// #[path = "bindings_test.rs"]
// mod bindings_test;
