//! Typed contract admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Contract{pub version:String,pub expected:String}
impl Contract{
 pub fn admits(&self)->bool{self.version == self.expected}
 pub const fn facet()->&'static str{"contract"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Contract::facet(),"contract");}}
