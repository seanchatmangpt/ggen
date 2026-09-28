//! Typed projection admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Projection{pub source_hash:String,pub expected:String}
impl Projection{
 pub fn admits(&self)->bool{self.source_hash == self.expected}
 pub const fn facet()->&'static str{"projection"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Projection::facet(),"projection");}}
