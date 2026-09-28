//! Typed observation admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Observation{pub observed:bool}
impl Observation{
 pub fn admits(&self)->bool{self.observed}
 pub const fn facet()->&'static str{"observation"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Observation::facet(),"observation");}}
