//! Typed cost admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Cost{pub units:u64,pub limit:u64}
impl Cost{
 pub fn admits(&self)->bool{self.units <= self.limit}
 pub const fn facet()->&'static str{"cost"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Cost::facet(),"cost");}}
