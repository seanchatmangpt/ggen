//! positive_fixture contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct PositiveFixture{pub accepted:bool}
impl PositiveFixture{pub fn admits(&self)->bool{self.accepted} pub const fn facet()->&'static str{"positive_fixture"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(PositiveFixture::facet(),"positive_fixture");}}
