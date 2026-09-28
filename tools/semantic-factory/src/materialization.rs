//! Typed materialization admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Materialization{pub artifact_hash:String,pub expected:String}
impl Materialization{
 pub fn admits(&self)->bool{self.artifact_hash == self.expected}
 pub const fn facet()->&'static str{"materialization"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Materialization::facet(),"materialization");}}
