//! store contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Store{pub persisted:bool}
impl Store{pub fn admits(&self)->bool{self.persisted} pub const fn facet()->&'static str{"store"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Store::facet(),"store");}}
