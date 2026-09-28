//! Minimal falsifier candidates for admitted claims.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Claim{pub subject:String,pub predicate:String,pub expected:String}
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Observation{pub subject:String,pub predicate:String,pub value:String,pub provenance:String}
pub fn falsifies(c:&Claim,o:&Observation)->bool{
 c.subject==o.subject && c.predicate==o.predicate && !o.provenance.is_empty() && c.expected!=o.value
}
pub fn minimal<'a>(c:&Claim,obs:&'a[Observation])->Option<&'a Observation>{obs.iter().find(|o|falsifies(c,o))}
pub fn same_subject<'a>(c:&Claim,obs:&'a[Observation])->impl Iterator<Item=&'a Observation>{let s=c.subject.clone();obs.iter().filter(move|o|o.subject==s)}
#[cfg(test)] mod tests{use super::*;#[test] fn adjacency_is_not_refutation(){let c=Claim{subject:"a".into(),predicate:"p".into(),expected:"1".into()};let o=Observation{subject:"b".into(),predicate:"p".into(),value:"0".into(),provenance:"x".into()};assert!(!falsifies(&c,&o));}}
