//! Typed receipt admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Receipt{pub id:String,pub replayable:bool}
impl Receipt{
 pub fn admits(&self)->bool{!self.id.is_empty() && self.replayable}
 pub const fn facet()->&'static str{"receipt"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Receipt::facet(),"receipt");}}
