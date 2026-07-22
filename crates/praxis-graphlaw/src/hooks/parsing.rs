// Hook parsing: validation, extraction, and alias rewriting (PROJ-401, PROJ-402)
// FIX #9: rewrite_hook_alias handles rdf:type → kh:Hook conversion

use crate::encoding::Encoder;
use crate::fastmap::FxHashMap;
use crate::term::{Triple, VarOrTerm};
use crate::TripleStore;
use serde::{Deserialize, Serialize};

use super::quads::parse_construct;
use super::{
    CmpOp, EffectKind, HookCondition, KnowledgeHook, ALLOWED_KH_PREDICATES, HOOK_ALIAS_MAP,
    HOOK_ALIAS_NS, KH_NS, SHACL_LAW_PACK,
};

pub fn clean_term(s: &str) -> &str {
    let mut s = s.trim();
    if s.starts_with('<') && s.ends_with('>') {
        s = &s[1..s.len() - 1];
    } else if s.starts_with('"') && s.ends_with('"') {
        s = &s[1..s.len() - 1];
    }
    s
}

pub fn parse_rdf_integer<T: std::str::FromStr>(s: &str) -> Result<T, String>
where
    T::Err: std::fmt::Display,
{
    let mut s = s.trim();
    if let Some((val, _dt)) = s.split_once("^^") {
        s = val.trim();
    }
    if s.starts_with('"') && s.ends_with('"') {
        s = &s[1..s.len() - 1];
    }
    s.parse::<T>()
        .map_err(|e| format!("failed to parse integer '{}': {}", s, e))
}

pub fn contains_forbidden_keyword(text: &str) -> bool {
    let text = text.to_lowercase();
    let suspicious = ["shell", "exec", "curl", "socket", "fetch"];
    suspicious.iter().any(|&keyword| {
        text.contains(keyword)
            && !text.starts_with("<http://seanchatmangpt.github.io/praxis/")
            && !text.starts_with("<http://www.w3.org/")
            && !text.starts_with("http://seanchatmangpt.github.io/praxis/")
            && !text.starts_with("http://www.w3.org/")
    })
}

fn is_rdf_type(s: &str) -> bool {
    clean_term(s) == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
}

fn is_kh_hook(s: &str) -> bool {
    clean_term(s) == "http://seanchatmangpt.github.io/praxis/kh#Hook"
}

struct HookProps {
    map: FxHashMap<String, Vec<String>>,
}

impl HookProps {
    fn new(triples: &[Triple], subject: &str) -> Result<Self, String> {
        let mut map = FxHashMap::default();
        for t in triples {
            let s_str = Encoder::decode(&t.s.to_encoded())
                .ok_or_else(|| format!("failed to decode subject: {:?}", t.s))?;
            if clean_term(&s_str) == subject {
                let p_str = Encoder::decode(&t.p.to_encoded())
                    .ok_or_else(|| format!("failed to decode predicate: {:?}", t.p))?;
                let cleaned_p = clean_term(&p_str);
                if let Some(local) = cleaned_p.strip_prefix(KH_NS) {
                    let o_str = Encoder::decode(&t.o.to_encoded())
                        .ok_or_else(|| format!("failed to decode object: {:?}", t.o))?;
                    let cleaned_o = clean_term(&o_str).to_string();
                    map.entry(local.to_string())
                        .or_insert_with(Vec::new)
                        .push(cleaned_o);
                }
            }
        }
        Ok(HookProps { map })
    }

    fn one_str(&self, local: &str) -> Result<String, String> {
        match self.map.get(local).map(|v| v.as_slice()) {
            Some([val]) => Ok(val.clone()),
            None => Err(format!("missing kh:{}", local)),
            _ => Err(format!("multiple values for kh:{}", local)),
        }
    }

    fn opt_str(&self, local: &str) -> Result<Option<String>, String> {
        match self.map.get(local).map(|v| v.as_slice()) {
            Some([val]) => Ok(Some(val.clone())),
            None => Ok(None),
            _ => Err(format!("multiple values for kh:{}", local)),
        }
    }

    fn all_str(&self, local: &str) -> Vec<String> {
        self.map.get(local).cloned().unwrap_or_default()
    }
}

/// Rewrites hook:* triples to kh:* equivalents (alias → canonical conversion).
///
/// # Algorithm
/// For each triple: if the predicate URI is in the hook: namespace, lookup the
/// local part in HOOK_ALIAS_MAP. If found, construct a new kh: URI with the mapped local part;
/// otherwise refuse -- an unrecognized `hook:` alias predicate is refused here, not left
/// unchanged, because a `hook:`-namespaced predicate that fails this lookup is NOT in the
/// `kh:` namespace afterward, so it would otherwise silently evade every downstream check
/// that only inspects `kh:`-prefixed predicates (the SHACL closed-shape validation and the
/// `ALLOWED_KH_PREDICATES` scan both key off `KH_NS`): a fail-open gap, not a deferred check.
/// Subject and object are left unchanged.
///
/// # Determinism
/// The mapping is immutable and position-independent. Output order matches input order.
/// No sorting or HashMap iteration; all operations are O(1) per triple.
///
/// # Complexity
/// O(|T|) where |T| is the number of input triples. Each triple is examined once.
///
/// # Errors
/// Returns `Err` for any `hook:`-namespaced predicate not present in `HOOK_ALIAS_MAP`
/// (found 2026-07-21: `test_hook_alias_unknown_predicate_refused` proved
/// `hook:unknown_field` previously round-tripped through `load_hook_pack` unrejected --
/// this closes that gap at its source instead of downstream). Other validation failures
/// (unknown `kh:` predicates already rewritten, closed-shape violations, ...) still occur
/// downstream in `validate_and_extract_hooks`, unchanged.
///
/// # FIX #9
/// Handles rdf:type → kh:Hook conversion from hook:Hook alias to kh:Hook canonical form.
fn rewrite_hook_alias(triples: &[Triple]) -> Result<Vec<Triple>, String> {
    triples
        .iter()
        .map(|t| {
            let p_str = Encoder::decode(&t.p.to_encoded())
                .ok_or_else(|| format!("failed to decode predicate: {:?}", t.p))?;
            let cleaned_p = clean_term(&p_str);
            let o_str = Encoder::decode(&t.o.to_encoded())
                .ok_or_else(|| format!("failed to decode object: {:?}", t.o))?;
            let cleaned_o = clean_term(&o_str);

            let mut new_t = t.clone();

            // Rewrite predicates in hook: namespace
            if let Some(local) = cleaned_p.strip_prefix(HOOK_ALIAS_NS) {
                match HOOK_ALIAS_MAP.iter().find(|(alias, _)| *alias == local) {
                    Some((_, mapped_local)) => {
                        let new_p_uri = format!("{}{}", KH_NS, mapped_local);
                        new_t.p = VarOrTerm::new_term(new_p_uri);
                    }
                    None => {
                        return Err(format!(
                            "unknown hook: alias predicate: hook:{} (not in HOOK_ALIAS_MAP)",
                            local
                        ));
                    }
                }
            }

            // Rewrite object in rdf:type triples for hook:Hook (kh:Hook)
            if cleaned_p == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                && cleaned_o == format!("{}Hook", HOOK_ALIAS_NS)
            {
                new_t.o = VarOrTerm::new_term(format!("{}Hook", KH_NS));
            }

            Ok(new_t)
        })
        .collect()
}

pub fn validate_and_extract_hooks(triples: &[Triple]) -> Result<Vec<KnowledgeHook>, String> {
    // Pre-pass: rewrite hook: aliases to kh: canonical form
    let rewritten_triples = rewrite_hook_alias(triples)?;
    let mut temp_store = TripleStore::new();
    for t in &rewritten_triples {
        temp_store.add(t.clone());
    }
    let report = temp_store.validate_shacl(SHACL_LAW_PACK)?;
    if !report.conforms {
        let mut err_msg = "SHACL validation failed:".to_string();
        for res in &report.results {
            err_msg.push_str(&format!(
                "\n  - Focus node: {}, path: {:?}, constraint: {}",
                res.focus_node, res.result_path, res.source_constraint_component
            ));
        }
        return Err(err_msg);
    }

    for t in &rewritten_triples {
        let decoded_s = Encoder::decode(&t.s.to_encoded())
            .ok_or_else(|| format!("failed to decode subject: {:?}", t.s))?;
        let decoded_p = Encoder::decode(&t.p.to_encoded())
            .ok_or_else(|| format!("failed to decode predicate: {:?}", t.p))?;
        let decoded_o = Encoder::decode(&t.o.to_encoded())
            .ok_or_else(|| format!("failed to decode object: {:?}", t.o))?;

        if contains_forbidden_keyword(&decoded_s) {
            return Err(format!("forbidden keyword in subject: {}", decoded_s));
        }
        if contains_forbidden_keyword(&decoded_p) {
            return Err(format!("forbidden keyword in predicate: {}", decoded_p));
        }
        if contains_forbidden_keyword(&decoded_o) {
            return Err(format!("forbidden keyword in object: {}", decoded_o));
        }
        if let Some(ref g) = t.g {
            let decoded_g = Encoder::decode(&g.to_encoded())
                .ok_or_else(|| format!("failed to decode graph: {:?}", g))?;
            if contains_forbidden_keyword(&decoded_g) {
                return Err(format!("forbidden keyword in graph: {}", decoded_g));
            }
        }

        let cleaned_p = clean_term(&decoded_p);
        let cleaned_o = clean_term(&decoded_o);
        if cleaned_p == "http://seanchatmangpt.github.io/praxis/kh#handler" {
            if cleaned_o != "http://seanchatmangpt.github.io/praxis/handler#sparql-construct" {
                return Err(format!("forbidden or unrecognized handler: {}", cleaned_o));
            }
        }
        if cleaned_p == "http://seanchatmangpt.github.io/praxis/kh#query" {
            if let Err(e) = spargebra::SparqlParser::new().parse_query(&cleaned_o) {
                return Err(format!("SPARQL parse error: {}", e));
            }
            if cleaned_o.to_uppercase().contains("CONSTRUCT") {
                if let Ok(c_query) = parse_construct(&cleaned_o) {
                    for (s, p, o) in &c_query.template_triples {
                        if s.contains("http://seanchatmangpt.github.io/praxis/kh#")
                            || s.contains("kh:")
                            || p.contains("http://seanchatmangpt.github.io/praxis/kh#")
                            || p.contains("kh:")
                            || o.contains("http://seanchatmangpt.github.io/praxis/kh#")
                            || o.contains("kh:")
                        {
                            return Err(
                                "CONSTRUCT template attempts to modify hook registry namespace"
                                    .to_string(),
                            );
                        }
                    }
                } else {
                    return Err("invalid construct query".to_string());
                }
            }
        }

        if let Some(local) = cleaned_p.strip_prefix(KH_NS) {
            if !ALLOWED_KH_PREDICATES.contains(&local) {
                return Err(format!(
                    "forbidden predicate in kh: namespace: {}",
                    cleaned_p
                ));
            }
        }
    }

    let mut hook_subjects = Vec::new();
    for t in &rewritten_triples {
        let s_str = Encoder::decode(&t.s.to_encoded())
            .ok_or_else(|| format!("failed to decode subject: {:?}", t.s))?;
        let p_str = Encoder::decode(&t.p.to_encoded())
            .ok_or_else(|| format!("failed to decode predicate: {:?}", t.p))?;
        let o_str = Encoder::decode(&t.o.to_encoded())
            .ok_or_else(|| format!("failed to decode object: {:?}", t.o))?;
        if is_rdf_type(&p_str) && is_kh_hook(&o_str) {
            let clean_s = clean_term(&s_str).to_string();
            if !hook_subjects.contains(&clean_s) {
                hook_subjects.push(clean_s);
            }
        }
    }

    if hook_subjects.len() > 12 {
        return Err(format!(
            "too many hooks declared: {}; max 12",
            hook_subjects.len()
        ));
    }

    let mut hooks = Vec::new();
    for subj in hook_subjects {
        let props = HookProps::new(&rewritten_triples, &subj)?;
        let name = props.one_str("name")?;
        let on = props.opt_str("on")?.unwrap_or_else(|| "any".to_string());
        if !matches!(on.as_str(), "assert" | "retract" | "any") {
            return Err(format!(
                "hook:on must be assert, retract, or any, got: {}",
                on
            ));
        }
        let kind = props.one_str("kind")?;
        let condition = match kind.as_str() {
            "datalog" => {
                let program = props.one_str("program")?;
                let goal = props.one_str("goal")?;
                HookCondition::Datalog { program, goal }
            }
            "delta" => {
                let var = props.one_str("var")?;
                HookCondition::Delta { var }
            }
            "threshold" => {
                let var = props.one_str("var")?;
                let op_str = props.one_str("op")?;
                let op = CmpOp::parse(&op_str)?;
                let k = parse_rdf_integer::<u64>(&props.one_str("k")?)?;
                HookCondition::Threshold { var, op, k }
            }
            "count" => {
                let var = props.one_str("var")?;
                let op_str = props.one_str("op")?;
                let op = CmpOp::parse(&op_str)?;
                let k = parse_rdf_integer::<u64>(&props.one_str("k")?)?;
                HookCondition::Count { var, op, k }
            }
            "window" => {
                let var = props.one_str("var")?;
                let op_str = props.one_str("op")?;
                let op = CmpOp::parse(&op_str)?;
                let k = parse_rdf_integer::<u64>(&props.one_str("k")?)?;
                let window = parse_rdf_integer::<u8>(&props.one_str("window")?)?;
                // Refuse kh:window 0 at parse time rather than let it reach
                // `Reasoner::materialize`'s round loop, which computes `window - 1` as a bound
                // for how many PAST rounds (beyond the current one, already counted
                // unconditionally before this subtraction) to look back over -- a genuine,
                // reproduced bug found by an adversarial dogfood audit this session: `usize::
                // from(0u8) - 1` underflows and panics under this workspace's default
                // overflow-checked build profile, reachable in production via `ggen law derive/
                // explain/export/validate` (ggen/src/graph.rs's build_law_store has no
                // catch_unwind around TripleStore::materialize). `kh:window 0` is not a
                // reinterpretable edge case with an obvious intended meaning (a "look back zero
                // rounds" window is degenerate, not just a boundary value) -- refusing it here,
                // at admission, is the honest fix; silently reinterpreting it as window=1 would
                // be guessing at semantics this project's own hook vocabulary never specifies.
                if window == 0 {
                    return Err(
                        "hook:window must be >= 1 (0 is not a valid look-back window; it would \
                         underflow the round-history bound in Reasoner::materialize)"
                            .to_string(),
                    );
                }
                HookCondition::Window { var, op, k, window }
            }
            "shacl" => {
                let shapes = props.one_str("program")?;
                HookCondition::Shacl { shapes }
            }
            "shex" => {
                let schema = props.one_str("program")?;
                let shape_map = props.one_str("goal")?;
                HookCondition::Shex { schema, shape_map }
            }
            "n3" => {
                let rules = props.one_str("program")?;
                HookCondition::N3 { rules }
            }
            "sparql" => {
                let query = props.one_str("query")?;
                if spargebra::Query::parse(&query, None).is_err() {
                    return Err("invalid sparql query".to_string());
                }
                HookCondition::Sparql { query }
            }
            other => {
                return Err(format!("unsupported condition kind '{}'", other));
            }
        };

        let effect_str = props.one_str("effect")?;
        let effect = match effect_str.as_str() {
            "emit-delta" => EffectKind::EmitDelta,
            "ground-action" => EffectKind::GroundAction,
            "refuse" => EffectKind::Refuse,
            other => return Err(format!("unknown effect: {}", other)),
        };

        let action = props.opt_str("action")?;
        let reason = props.opt_str("reason")?;
        match effect {
            EffectKind::GroundAction if action.is_none() => {
                return Err("effect 'ground-action' requires kh:action".to_string());
            }
            EffectKind::Refuse if reason.is_none() => {
                return Err("effect 'refuse' requires kh:reason".to_string());
            }
            _ => {}
        }

        let priority = props
            .opt_str("priority")?
            .map(|s| parse_rdf_integer::<u8>(&s))
            .transpose()?
            .unwrap_or(0);

        let after = props.all_str("after");

        hooks.push(KnowledgeHook {
            iri: subj,
            name,
            on,
            condition,
            effect,
            action,
            reason,
            priority,
            after,
        });
    }

    Ok(hooks)
}
