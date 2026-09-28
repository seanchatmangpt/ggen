//! PlannerPolicy: typed policy facet used by the truth-gate admission engine.
use serde::{Deserialize,Serialize};
#[derive(Debug,Clone,Serialize,Deserialize,PartialEq,Eq)] pub struct Input{pub subject:String,pub admitted:Vec<String>,pub epoch:u64}
#[derive(Debug,Clone,Serialize,Deserialize,PartialEq,Eq)] pub struct Decision{pub subject:String,pub policy:&'static str,pub admitted:bool,pub missing:Vec<&'static str>,pub epoch:u64}
#[derive(Debug,Default,Clone,Copy)] pub struct PlannerPolicy;
impl PlannerPolicy{
 pub const RULES:[&'static str;3]=["planner.identity","planner.boundary","planner.receipt"];
 pub fn evaluate(&self,input:&Input)->Decision{
  let missing=Self::RULES.iter().copied().filter(|r|!input.admitted.iter().any(|a|a=="*"||a==r)).collect::<Vec<_>>();
  Decision{subject:input.subject.clone(),policy:"planner",admitted:missing.is_empty(),missing,epoch:input.epoch}
 }
 pub fn explain(&self,input:&Input)->String{let d=self.evaluate(input);if d.admitted{format!("{} admitted by {}",d.subject,d.policy)}else{format!("{} refused by {}: {}",d.subject,d.policy,d.missing.join(","))}}
 pub fn required(&self)->&'static [&'static str]{&Self::RULES}
}
#[cfg(test)]mod tests{use super::*;
 fn input(xs:&[&str])->Input{Input{subject:"subject@sha".into(),admitted:xs.iter().map(|s|s.to_string()).collect(),epoch:42}}
 #[test]fn empty_refuses(){let d=PlannerPolicy.evaluate(&input(&[]));assert!(!d.admitted);assert_eq!(d.missing.len(),3);}
 #[test]fn wildcard_admits(){let d=PlannerPolicy.evaluate(&input(&["*"]));assert!(d.admitted);assert!(d.missing.is_empty());}
 #[test]fn partial_is_explicit(){let d=PlannerPolicy.evaluate(&input(&["planner.identity"]));assert_eq!(d.missing.len(),2);}
 #[test]fn explanation_names_policy(){assert!(PlannerPolicy.explain(&input(&[])).contains("planner"));}
 #[test]fn requirements_stable(){assert_eq!(PlannerPolicy.required(),&Self::RULES);}
}
