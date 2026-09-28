//! registry contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Registry{pub registered:bool}
impl Registry{pub fn admits(&self)->bool{self.registered} pub const fn facet()->&'static str{"registry"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Registry::facet(),"registry");}}
