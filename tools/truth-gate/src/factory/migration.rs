//! MigrationPolicy: exact-subject contract admission.
use serde::{Deserialize,Serialize};
#[derive(Debug,Clone,Serialize,Deserialize)]pub struct Contract{pub subject:String,pub version:String,pub facts:Vec<String>,pub provenance:String}
#[derive(Debug,Clone,Serialize,Deserialize)]pub struct Verdict{pub policy:&'static str,pub subject:String,pub accepted:bool,pub reasons:Vec<String>}
#[derive(Default)]pub struct MigrationPolicy;
impl MigrationPolicy{pub const REQUIRED:[&'static str;4]=["migration:subject","migration:version","migration:provenance","migration:boundary"];
pub fn check(&self,c:&Contract)->Verdict{let reasons=Self::REQUIRED.iter().filter(|r|!c.facts.iter().any(|f|f=="*"||f==**r)).map(|r|format!("missing {r}")).collect::<Vec<_>>();Verdict{policy:"migration",subject:c.subject.clone(),accepted:reasons.is_empty(),reasons}}
pub fn identity(&self,c:&Contract)->String{format!("{}:{}:{}:{}",c.subject,c.version,c.provenance,Self::REQUIRED.join("|"))}
pub fn requirements(&self)->impl Iterator<Item=&&'static str>{Self::REQUIRED.iter()}}
#[cfg(test)]mod tests{use super::*;fn c(f:&[&str])->Contract{Contract{subject:"repo-at-sha".into(),version:"1".into(),facts:f.iter().map(|x|x.to_string()).collect(),provenance:"generator".into()}}
#[test]fn closed_by_default(){assert!(!MigrationPolicy.check(&c(&[])).accepted);}
#[test]fn wildcard_admits(){assert!(MigrationPolicy.check(&c(&["*"])).accepted);}
#[test]fn reports_missing(){assert_eq!(MigrationPolicy.check(&c(&[])).reasons.len(),4);}
#[test]fn identity_binds_subject(){assert!(MigrationPolicy.identity(&c(&[])).contains("repo-at-sha"));}
#[test]fn exposes_requirements(){assert_eq!(MigrationPolicy.requirements().count(),4);}}
