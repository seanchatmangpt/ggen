//! Typed replay admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Replay{pub receipt_id:String,pub expected:String}
impl Replay{
 pub fn admits(&self)->bool{self.receipt_id == self.expected}
 pub const fn facet()->&'static str{"replay"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Replay::facet(),"replay");}}
