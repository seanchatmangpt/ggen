//! Typed admission admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Admission{pub admitted:bool}
impl Admission{
 pub fn admits(&self)->bool{self.admitted}
 pub const fn facet()->&'static str{"admission"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Admission::facet(),"admission");}}
