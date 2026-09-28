//! Typed guard admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Guard{pub allowed:bool}
impl Guard{
 pub fn admits(&self)->bool{self.allowed}
 pub const fn facet()->&'static str{"guard"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Guard::facet(),"guard");}}
