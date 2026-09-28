//! query contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Query{pub matched:bool}
impl Query{pub fn admits(&self)->bool{self.matched} pub const fn facet()->&'static str{"query"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Query::facet(),"query");}}
