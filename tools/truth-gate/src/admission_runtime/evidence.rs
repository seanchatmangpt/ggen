use serde::{Deserialize,Serialize}; use super::subject::SubjectId;
#[derive(Clone,Debug,Serialize,Deserialize,PartialEq,Eq)] pub enum EvidenceKind{Observation,Test,Falsifier,Receipt,Replay}
#[derive(Clone,Debug,Serialize,Deserialize)] pub struct Evidence{pub id:String,pub subject:SubjectId,pub kind:EvidenceKind,pub claim:String,pub digest:String}
impl Evidence{pub fn validate(&self)->Result<(),String>{self.subject.validate()?;if self.id.is_empty()||self.claim.is_empty()||self.digest.len()<8{Err("invalid evidence".into())}else{Ok(())}}}
pub fn exact_subject<'a>(s:&SubjectId,x:&'a[Evidence])->Vec<&'a Evidence>{x.iter().filter(|e|&e.subject==s).collect()}