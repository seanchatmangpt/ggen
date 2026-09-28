//! Typed exclusion with explicit re-entry policy.
use std::collections::BTreeMap;
#[derive(Debug,Clone,PartialEq,Eq)] pub enum ExclusionKind{IdentityMismatch,MissingAuthority,MissingEvidence,BoundaryViolation,CostExceeded}
#[derive(Debug,Clone)] pub struct Exclusion{pub subject:String,pub kind:ExclusionKind,pub evidence:String,pub reversible:bool}
#[derive(Default)] pub struct Exclusions{items:BTreeMap<String,Vec<Exclusion>>}
impl Exclusions{
 pub fn add(&mut self,x:Exclusion){self.items.entry(x.subject.clone()).or_default().push(x);}
 pub fn blocked(&self,subject:&str)->bool{self.items.get(subject).map(|xs|xs.iter().any(|x|!x.reversible)).unwrap_or(false)}
 pub fn can_reenter(&self,subject:&str)->bool{self.items.get(subject).map(|xs|xs.iter().all(|x|x.reversible)).unwrap_or(true)}
 pub fn reasons(&self,subject:&str)->&[Exclusion]{self.items.get(subject).map(Vec::as_slice).unwrap_or(&[])}
}
#[cfg(test)] mod tests{use super::*;#[test] fn irreversible_blocks(){let mut e=Exclusions::default();e.add(Exclusion{subject:"s".into(),kind:ExclusionKind::BoundaryViolation,evidence:"r".into(),reversible:false});assert!(e.blocked("s"));}}
