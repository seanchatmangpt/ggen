//! property_harness contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct PropertyHarness{pub cases:u64,pub failures:u64}
impl PropertyHarness{pub fn admits(&self)->bool{self.cases > 0 && self.failures == 0} pub const fn facet()->&'static str{"property_harness"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(PropertyHarness::facet(),"property_harness");}}
