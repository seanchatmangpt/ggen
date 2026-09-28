//! compatibility contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Compatibility{pub compatible:bool}
impl Compatibility{pub fn admits(&self)->bool{self.compatible} pub const fn facet()->&'static str{"compatibility"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Compatibility::facet(),"compatibility");}}
