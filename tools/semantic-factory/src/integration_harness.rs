//! integration_harness contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct IntegrationHarness{pub producer:bool,pub consumer:bool}
impl IntegrationHarness{pub fn admits(&self)->bool{self.producer && self.consumer} pub const fn facet()->&'static str{"integration_harness"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(IntegrationHarness::facet(),"integration_harness");}}
