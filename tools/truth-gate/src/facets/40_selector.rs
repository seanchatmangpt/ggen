//! selector semantic facet.
pub struct SelectorFacet{pub subject:String,pub value:String,pub source:String}
impl SelectorFacet{pub fn valid(&self,expected:&str)->bool{self.subject==expected&&!self.value.is_empty()&&!self.source.is_empty()} pub fn receipt(&self)->String{format!("selector|{}|{}",self.subject,self.source)}}
#[cfg(test)] mod tests{use super::*;#[test] fn validates(){let x=SelectorFacet{subject:"s:a".into(),value:"ok".into(),source:"git:abc".into()};assert!(x.valid("s:a"));assert!(!x.valid("s:b"));}}
