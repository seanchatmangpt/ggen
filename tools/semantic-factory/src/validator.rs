//! Typed validator admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Validator{pub valid:bool}
impl Validator{
 pub fn admits(&self)->bool{self.valid}
 pub const fn facet()->&'static str{"validator"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Validator::facet(),"validator");}}
