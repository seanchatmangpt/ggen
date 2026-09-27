//! actuator semantic facet.
pub struct ActuatorFacet{pub subject:String,pub value:String,pub source:String}
impl ActuatorFacet{pub fn valid(&self,expected:&str)->bool{self.subject==expected&&!self.value.is_empty()&&!self.source.is_empty()} pub fn receipt(&self)->String{format!("actuator|{}|{}",self.subject,self.source)}}
#[cfg(test)] mod tests{use super::*;#[test] fn validates(){let x=ActuatorFacet{subject:"s:a".into(),value:"ok".into(),source:"git:abc".into()};assert!(x.valid("s:a"));assert!(!x.valid("s:b"));}}
