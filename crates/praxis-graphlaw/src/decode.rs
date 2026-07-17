//! Decode/serialization helpers for `TripleStore`, extracted out of
//! `lib.rs` into their own module. Kept as an `impl TripleStore` block
//! (rather than free functions) so existing call sites (`TripleStore::decode_rule(...)`,
//! `self.decode_triples(...)`, etc.) keep compiling unchanged.

use crate::bindings::Binding;
use crate::encoding::Encoder;
use crate::rule::Rule;
use crate::term::Triple;
use crate::TripleStore;
use std::fmt::Write;

impl TripleStore {
    pub fn decode_rule(rule: &Rule) -> String {
        let mut res = String::new();
        let decoded_head = Self::decode_triples(std::slice::from_ref(&rule.head));
        let decoded_body: String = rule
            .body
            .iter()
            .map(|lit| {
                let s = Self::decode_triples(std::slice::from_ref(&lit.pattern));
                if lit.negated {
                    format!("not {{{}}}", s.trim())
                } else {
                    s
                }
            })
            .collect();
        writeln!(&mut res, "{{{}}}=>{{{}}}.", decoded_body, decoded_head).unwrap();
        res
    }
    pub fn decode_rules(rules: &[Rule]) -> String {
        let mut res = String::new();
        for rule in rules {
            let decoded_head = Self::decode_triples(std::slice::from_ref(&rule.head));
            let decoded_body: String = rule
                .body
                .iter()
                .map(|lit| {
                    let s = Self::decode_triples(std::slice::from_ref(&lit.pattern));
                    if lit.negated {
                        format!("not {{{}}}", s.trim())
                    } else {
                        s
                    }
                })
                .collect();
            writeln!(&mut res, "{{{}}}=>{{{}}}.", decoded_body, decoded_head).unwrap();
        }
        res
    }
    pub fn decode_triples(triples: &[Triple]) -> String {
        let mut res = String::new();
        for triple in triples {
            let decoded_s = Encoder::decode(&triple.s.to_encoded()).unwrap();
            let decoded_p = Encoder::decode(&triple.p.to_encoded()).unwrap();
            let decoded_o = Encoder::decode(&triple.o.to_encoded()).unwrap();

            writeln!(&mut res, "{} {} {}.", decoded_s, decoded_p, decoded_o).unwrap();
        }
        res
    }
    pub fn decode_bindings(bindings: &Binding) -> String {
        let mut res = String::new();
        for (key, val) in bindings.iter() {
            let decoded_values: String = val.iter().map(|t| Encoder::decode(t).unwrap()).collect();

            writeln!(
                &mut res,
                " {}: [{}] .",
                Encoder::decode(key).unwrap(),
                decoded_values
            )
            .unwrap();
        }
        res
    }
    pub fn decode_triple(triple: &Triple) -> String {
        let s = Encoder::decode(&triple.s.to_encoded()).unwrap();
        let p = Encoder::decode(&triple.p.to_encoded()).unwrap();
        let o = Encoder::decode(&triple.o.to_encoded()).unwrap();
        format!("{} {} {}", s, p, o)
    }
}
