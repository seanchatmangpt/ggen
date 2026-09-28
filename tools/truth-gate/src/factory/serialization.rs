//! SerializationPolicy: exact-subject contract admission.
use serde::{Deserialize,Serialize};
#[derive(Debug,Clone,Serialize,Deserialize)]pub struct Contract{pub subject:String,pub version:String,pub facts:Vec<String>,pub provenance:String}
#[derive(Debug,Clone,Serialize,Deserialize)]pub struct Verdict{pub policy:&'static str,pub subject:String,pub accepted:bool,pub reasons:Vec<String>}
#[derive(Default)]pub struct SerializationPolicy;
impl SerializationPolicy{pub const REQUIRED:[&'static str;4]=["serialization:subject","serialization:version","serialization:provenance","serialization:boundary"];
pub fn check(&self,c:&Contract)->Verdict{let reasons=Self::REQUIRED.iter().filter(|r|!c.facts.iter().any(|f|f=="*"||f==**r)).map(|r|format!("missing {r}")).collect::<Vec<_>>();Verdict{policy:"serialization",subject:c.subject.clone(),accepted:reasons.is_empty(),reasons}}
pub fn identity(&self,c:&Contract)->String{format!("{}:{}:{}:{}",c.subject,c.version,c.provenance,Self::REQUIRED.join("|"))}
pub fn requirements(&self)->impl Iterator<Item=&&'static str>{Self::REQUIRED.iter()}}
#[cfg(test)]mod tests{use super::*;fn c(f:&[&str])->Contract{Contract{subject:"repo-at-sha".into(),version:"1".into(),facts:f.iter().map(|x|x.to_string()).collect(),provenance:"generator".into()}}
#[test]fn closed_by_default(){assert!(!SerializationPolicy.check(&c(&[])).accepted);}
#[test]fn wildcard_admits(){assert!(SerializationPolicy.check(&c(&["*"])).accepted);}
#[test]fn reports_missing(){assert_eq!(SerializationPolicy.check(&c(&[])).reasons.len(),4);}
#[test]fn identity_binds_subject(){assert!(SerializationPolicy.identity(&c(&[])).contains("repo-at-sha"));}
#[test]fn exposes_requirements(){assert_eq!(SerializationPolicy.requirements().count(),4);}}
