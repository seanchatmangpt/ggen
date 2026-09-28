//! Typed falsifier admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Falsifier{pub counterexample:bool}
impl Falsifier{
 pub fn admits(&self)->bool{self.counterexample}
 pub const fn facet()->&'static str{"falsifier"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Falsifier::facet(),"falsifier");}}
