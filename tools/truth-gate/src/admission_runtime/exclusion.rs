use serde::{Deserialize,Serialize};
#[derive(Clone,Debug,Serialize,Deserialize,PartialEq,Eq)] pub enum ExclusionReason{WrongSubject,MissingAuthority,FailedFalsifier,GeneratedEdit,BoundaryViolation,InsufficientEvidence}
#[derive(Clone,Debug,Serialize,Deserialize)] pub struct Exclusion{pub edge:String,pub reason:ExclusionReason,pub evidence:String,pub permanent:bool}
#[derive(Clone,Debug,Default,Serialize,Deserialize)] pub struct ExclusionSet{pub items:Vec<Exclusion>}
impl ExclusionSet{pub fn add(&mut self,e:Exclusion){if !self.items.iter().any(|x|x.edge==e.edge&&x.reason==e.reason){self.items.push(e)}}pub fn blocked(&self,e:&str)->bool{self.items.iter().any(|x|x.edge==e&&x.permanent)}}