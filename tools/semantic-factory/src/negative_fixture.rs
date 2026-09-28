//! negative_fixture contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct NegativeFixture{pub rejected:bool}
impl NegativeFixture{pub fn admits(&self)->bool{self.rejected} pub const fn facet()->&'static str{"negative_fixture"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(NegativeFixture::facet(),"negative_fixture");}}
