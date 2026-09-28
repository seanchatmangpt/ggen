//! policy contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Policy{pub permitted:bool}
impl Policy{pub fn admits(&self)->bool{self.permitted} pub const fn facet()->&'static str{"policy"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Policy::facet(),"policy");}}
