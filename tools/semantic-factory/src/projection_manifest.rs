//! Content-addressed semantic projection manifest.
use std::collections::BTreeMap;
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Projection{pub subject:String,pub source:String,pub generator:String,pub artifact:String,pub epoch:u64}
#[derive(Default)] pub struct Manifest{by_subject:BTreeMap<String,Vec<Projection>>}
impl Manifest{
 pub fn admit(&mut self,p:Projection)->Result<(),String>{
  if p.subject.is_empty()||p.source.is_empty()||p.generator.is_empty()||p.artifact.is_empty(){return Err("incomplete-projection".into());}
  let xs=self.by_subject.entry(p.subject.clone()).or_default();
  if xs.iter().any(|x|x.epoch==p.epoch){return Err("epoch-collision".into());}
  xs.push(p);xs.sort_by_key(|x|x.epoch);Ok(())
 }
 pub fn latest(&self,subject:&str)->Option<&Projection>{self.by_subject.get(subject)?.last()}
 pub fn history(&self,subject:&str)->&[Projection]{self.by_subject.get(subject).map(Vec::as_slice).unwrap_or(&[])}
}
#[cfg(test)] mod tests{use super::*;#[test] fn latest_wins(){let mut m=Manifest::default();for e in 1..=2{m.admit(Projection{subject:"s".into(),source:"o".into(),generator:"g".into(),artifact:e.to_string(),epoch:e}).unwrap();}assert_eq!(m.latest("s").unwrap().artifact,"2");}}
