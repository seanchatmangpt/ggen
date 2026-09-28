//! runtime contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Runtime{pub alive:bool}
impl Runtime{pub fn admits(&self)->bool{self.alive} pub const fn facet()->&'static str{"runtime"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Runtime::facet(),"runtime");}}
