//! Typed boundary admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Boundary{pub scope:String,pub expected:String}
impl Boundary{
 pub fn admits(&self)->bool{self.scope == self.expected}
 pub const fn facet()->&'static str{"boundary"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Boundary::facet(),"boundary");}}
