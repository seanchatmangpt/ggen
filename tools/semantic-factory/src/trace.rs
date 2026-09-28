//! trace contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Trace{pub parent_ok:bool}
impl Trace{pub fn admits(&self)->bool{self.parent_ok} pub const fn facet()->&'static str{"trace"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Trace::facet(),"trace");}}
