//! ConsumerPolicy: bounded admission contract.
use serde::{Deserialize,Serialize};
#[derive(Debug,Clone,Serialize,Deserialize)]pub struct State{pub subject:String,pub facts:Vec<String>,pub epoch:u64}
#[derive(Debug,Clone,Serialize,Deserialize)]pub struct Result{pub subject:String,pub accepted:bool,pub missing:Vec<&'static str>,pub epoch:u64}
#[derive(Default)]pub struct ConsumerPolicy;
impl ConsumerPolicy{pub const REQUIRED:[&'static str;3]=["consumer:identity","consumer:authority","consumer:receipt"];
pub fn evaluate(&self,s:&State)->Result{let missing=Self::REQUIRED.iter().copied().filter(|r|!s.facts.iter().any(|f|f=="*"||f==r)).collect::<Vec<_>>();Result{subject:s.subject.clone(),accepted:missing.is_empty(),missing,epoch:s.epoch}}
pub fn explain(&self,s:&State)->Vec<String>{self.evaluate(s).missing.iter().map(|r|format!("{} lacks {}",s.subject,r)).collect()}}
#[cfg(test)]mod tests{use super::*;fn s(f:&[&str])->State{State{subject:"exact".into(),facts:f.iter().map(|x|x.to_string()).collect(),epoch:1}}
#[test]fn refuses_empty(){assert!(!ConsumerPolicy.evaluate(&s(&[])).accepted);}
#[test]fn admits_wildcard(){assert!(ConsumerPolicy.evaluate(&s(&["*"])).accepted);}
#[test]fn bounded_missing(){assert_eq!(ConsumerPolicy.evaluate(&s(&[])).missing.len(),3);}
#[test]fn explains_subject(){assert!(ConsumerPolicy.explain(&s(&[]))[0].contains("exact"));}}
