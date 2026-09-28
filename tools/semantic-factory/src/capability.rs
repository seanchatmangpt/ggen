//! Typed capability admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Capability{pub available:bool}
impl Capability{
 pub fn admits(&self)->bool{self.available}
 pub const fn facet()->&'static str{"capability"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Capability::facet(),"capability");}}
