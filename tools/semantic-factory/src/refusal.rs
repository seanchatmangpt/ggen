//! Typed refusal admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Refusal{pub reason:String}
impl Refusal{
 pub fn admits(&self)->bool{!self.reason.is_empty()}
 pub const fn facet()->&'static str{"refusal"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Refusal::facet(),"refusal");}}
