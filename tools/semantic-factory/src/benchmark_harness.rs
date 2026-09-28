//! benchmark_harness contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct BenchmarkHarness{pub samples:u64}
impl BenchmarkHarness{pub fn admits(&self)->bool{self.samples > 0} pub const fn facet()->&'static str{"benchmark_harness"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(BenchmarkHarness::facet(),"benchmark_harness");}}
