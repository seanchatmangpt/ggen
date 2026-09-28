//! migration contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Migration{pub forward:bool,pub reversible:bool}
impl Migration{pub fn admits(&self)->bool{self.forward && self.reversible} pub const fn facet()->&'static str{"migration"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Migration::facet(),"migration");}}
