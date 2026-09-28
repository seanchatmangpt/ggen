//! rule contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Rule{pub fired:bool}
impl Rule{pub fn admits(&self)->bool{self.fired} pub const fn facet()->&'static str{"rule"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Rule::facet(),"rule");}}
