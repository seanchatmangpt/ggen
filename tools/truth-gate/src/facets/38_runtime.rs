//! runtime semantic facet.
pub struct RuntimeFacet{pub subject:String,pub value:String,pub source:String}
impl RuntimeFacet{pub fn valid(&self,expected:&str)->bool{self.subject==expected&&!self.value.is_empty()&&!self.source.is_empty()} pub fn receipt(&self)->String{format!("runtime|{}|{}",self.subject,self.source)}}
#[cfg(test)] mod tests{use super::*;#[test] fn validates(){let x=RuntimeFacet{subject:"s:a".into(),value:"ok".into(),source:"git:abc".into()};assert!(x.valid("s:a"));assert!(!x.valid("s:b"));}}
