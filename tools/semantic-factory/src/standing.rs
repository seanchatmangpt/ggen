//! Typed standing admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Standing{pub recognized:bool}
impl Standing{
 pub fn admits(&self)->bool{self.recognized}
 pub const fn facet()->&'static str{"standing"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Standing::facet(),"standing");}}
