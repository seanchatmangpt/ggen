//! checkpoint contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Checkpoint{pub durable:bool}
impl Checkpoint{pub fn admits(&self)->bool{self.durable} pub const fn facet()->&'static str{"checkpoint"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Checkpoint::facet(),"checkpoint");}}
