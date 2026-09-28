//! Typed consumer admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Consumer{pub consumer_id:String,pub expected:String}
impl Consumer{
 pub fn admits(&self)->bool{self.consumer_id == self.expected}
 pub const fn facet()->&'static str{"consumer"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Consumer::facet(),"consumer");}}
