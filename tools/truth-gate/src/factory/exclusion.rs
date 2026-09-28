//! ExclusionPolicy: executable admission rules for truth-gate.
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Severity { Warn, Block }

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Rule { pub id: &'static str, pub statement: &'static str, pub severity: Severity }

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Observation { pub subject: String, pub facts: Vec<String>, pub authority: Option<String>, pub epoch: u64 }

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Finding { pub rule_id: &'static str, pub subject: String, pub reason: String, pub blocking: bool }

pub trait AdmissionPolicy { fn rules(&self) -> Vec<Rule>; fn evaluate(&self, observation: &Observation) -> Vec<Finding>; }

#[derive(Debug, Default, Clone, Copy)]
pub struct ExclusionPolicy;

impl ExclusionPolicy {
    pub fn ruleset() -> Vec<Rule> {
        vec![
        Rule { id: "exclusion.1", statement: "typed exclusions carry subject and reason", severity: Severity::Block },
        Rule { id: "exclusion.2", statement: "excluded evidence cannot silently reenter", severity: Severity::Warn },
        Rule { id: "exclusion.3", statement: "reentry requires new admission", severity: Severity::Warn },
        ]
    }
    fn fact(observation: &Observation, needle: &str) -> bool { observation.facts.iter().any(|f| f == needle) }
    fn finding(rule: &Rule, observation: &Observation, reason: impl Into<String>) -> Finding {
        Finding { rule_id: rule.id, subject: observation.subject.clone(), reason: reason.into(), blocking: rule.severity == Severity::Block }
    }
}

impl AdmissionPolicy for ExclusionPolicy {
    fn rules(&self) -> Vec<Rule> { Self::ruleset() }
    fn evaluate(&self, observation: &Observation) -> Vec<Finding> {
        let mut out = Vec::new();
        for rule in Self::ruleset() {
            let admitted = Self::fact(observation, rule.id) || Self::fact(observation, "admitted");
            if !admitted { out.push(Self::finding(&rule, observation, format!("missing admission fact for {}", rule.id))); }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test] fn missing_facts_are_findings() { let o=Observation{subject:"s".into(),facts:vec![],authority:None,epoch:1}; assert_eq!(ExclusionPolicy.evaluate(&o).len(),3); }
    #[test] fn admitted_fact_closes_rules() { let o=Observation{subject:"s".into(),facts:vec!["admitted".into()],authority:Some("a".into()),epoch:1}; assert!(ExclusionPolicy.evaluate(&o).is_empty()); }
    #[test] fn rules_are_stable() { let ids:Vec<_>=ExclusionPolicy::ruleset().into_iter().map(|r|r.id).collect(); assert_eq!(ids.len(),3); assert!(ids.iter().all(|id|id.starts_with("exclusion."))); }
}
