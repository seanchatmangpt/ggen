//! End-to-end SELECT -> CONSTRUCT -> DO boundary pipeline.
#[derive(Debug,Clone,PartialEq,Eq)] pub enum Phase{Select,Construct,Do}
#[derive(Debug,Clone)] pub struct Request{pub subject:String,pub provenance:String,pub authorized:bool,pub consequence:String}
#[derive(Debug,Clone)] pub struct Decision{pub phase:Phase,pub admitted:bool,pub reasons:Vec<String>}
pub fn select(r:&Request)->Decision{
 let mut reasons=Vec::new();
 if r.subject.is_empty(){reasons.push("missing-subject".into());}
 if r.provenance.is_empty(){reasons.push("missing-provenance".into());}
 Decision{phase:Phase::Select,admitted:reasons.is_empty(),reasons}
}
pub fn construct(r:&Request,d:&Decision)->Decision{
 let mut reasons=d.reasons.clone();
 if !d.admitted{reasons.push("select-refused".into());}
 if r.consequence.is_empty(){reasons.push("missing-consequence".into());}
 Decision{phase:Phase::Construct,admitted:reasons.is_empty(),reasons}
}
pub fn authorize_do(r:&Request,d:&Decision)->Decision{
 let mut reasons=d.reasons.clone();
 if !d.admitted{reasons.push("construct-refused".into());}
 if !r.authorized{reasons.push("authority-missing".into());}
 Decision{phase:Phase::Do,admitted:reasons.is_empty(),reasons}
}
#[cfg(test)] mod tests{use super::*;#[test] fn do_needs_authority(){let r=Request{subject:"s".into(),provenance:"git:x".into(),authorized:false,consequence:"write".into()};let s=select(&r);let c=construct(&r,&s);assert!(!authorize_do(&r,&c).admitted);}}
