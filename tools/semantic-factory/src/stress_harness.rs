//! stress_harness contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct StressHarness{pub operations:u64}
impl StressHarness{pub fn admits(&self)->bool{self.operations > 0} pub const fn facet()->&'static str{"stress_harness"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(StressHarness::facet(),"stress_harness");}}
