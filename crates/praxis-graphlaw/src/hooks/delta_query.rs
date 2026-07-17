// Delta and shape query utilities

use crate::encoding::Encoder;
use crate::TripleStore;

use super::parsing::clean_term;
use super::GraphDelta;

pub(crate) fn count_pred_in_store(store: &TripleStore, var: &str) -> u64 {
    let clean_v = clean_term(var);
    store
        .triple_index
        .triples
        .iter()
        .filter(|t| {
            let p_str = Encoder::decode(&t.p.to_encoded()).unwrap_or_default();
            clean_term(&p_str) == clean_v
        })
        .count() as u64
}

pub(crate) fn count_pred_in_delta(delta: &GraphDelta, var: &str) -> u64 {
    let clean_v = clean_term(var);
    let add_count = delta
        .additions
        .iter()
        .filter(|t| {
            let p_str = Encoder::decode(&t.p.to_encoded()).unwrap_or_default();
            clean_term(&p_str) == clean_v
        })
        .count() as u64;
    let rem_count = delta
        .removals
        .iter()
        .filter(|t| {
            let p_str = Encoder::decode(&t.p.to_encoded()).unwrap_or_default();
            clean_term(&p_str) == clean_v
        })
        .count() as u64;
    add_count + rem_count
}

pub fn parse_shape_map(s: &str) -> Vec<(String, String)> {
    let mut map = Vec::new();
    for entry in s.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        if let Some((node, shape)) = entry.split_once('@') {
            let clean = |x: &str| {
                let x = x.trim();
                if x.starts_with('<') && x.ends_with('>') {
                    x[1..x.len() - 1].to_string()
                } else {
                    x.to_string()
                }
            };
            map.push((clean(node), clean(shape)));
        }
    }
    map
}
